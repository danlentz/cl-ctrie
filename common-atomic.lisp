;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :atom
  (:use :cl :sb-ext :sb-vm :sb-thread :sb-int :sb-sys)
  (:import-from :sb-ext
    :get-cas-expansion :define-cas-expander :cas
    :compare-and-swap :atomic-incf :atomic-decf :defcas :defglobal)
  (:export
    :get-cas-expansion :define-cas-expander :cas :compare-and-swap :defcas
    :atomic-incf :atomic-decf :define-atomic-incf :atomic-incf-index-of
    :defglobal :compare-and-set! :atomic-updatef :reference :box :deref
    ;;
    :make-frlock :frlock :frlock-name :frlock-write :frlock-read
    :frlock-read-begin :frlock-read-end :grab-frlock-write-lock
    :release-frlock-write-lock
    ;;
    :make-brlock :brlock :brlock-p :brlock-name :brlock-write-lock
    :brlock-write-unlock :brlock-read-lock :brlock-read-unlock))
    
(in-package :atom)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized atomic place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Atomic Update (sbcl src copied over until i update to a more recent release)
;; TODO: unused?

(defmacro atomic-updatef (place update-fn &rest arguments &environment env) 
  "Updates PLACE atomically to the value returned by calling function
  designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.
  PLACE may be read and UPDATE-FN evaluated and called multiple times before the
  update succeeds: atomicity in this context means that value of place did not
  change between the time it was read, and the time it was replaced with the
  computed value. PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.
  EXAMPLE: Conses T to the head of FOO-LIST:
  ;;;   (defstruct foo list)
  ;;;   (defvar *foo* (make-foo))
  ;;;   (atomic-update (foo-list *foo*) #'cons t)"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))


(defmacro define-atomic-incf (name accessor &rest args)
  `(defun ,name (x)
     (let* ((old (,accessor x ,@args))
             (new (1+ old)))
       (loop until (eq old (sb-ext:cas (,accessor x ,@args) old new)) do
         (setf old (,accessor x ,@args)
           new (1+ old)))
       new)))

;;; 
;;; Form: (DEFINE-ATOMIC-INCF ATOMIC-INCF-INDEX-OF INDEX-OF)
;;; First step of expansion:
;;
;; (DEFUN ATOMIC-INCF-INDEX-OF (X)
;;   (LET* ((OLD (INDEX-OF X)) (NEW (1+ OLD)))
;;     (LOOP UNTIL (EQ OLD (CAS (INDEX-OF X) OLD NEW))
;;           DO (SETF OLD (INDEX-OF X)
;;                    NEW (1+ OLD)))
;;     NEW))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FRLOCK and BRLOCKS for SBCL 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; FRLOCK is a "fast read lock", which allows readers to gain unlocked access
;;;; to values, and provides post-read verification. Readers which intersected
;;;; with writers need to retry. frlock is very efficient when there are many
;;;; readers and writes are both fast and relatively scarce. It is, however,
;;;; unsuitable when readers and writers need exclusion, such as with SBCL's
;;;; current hash-table implementation.
;;;;
;;;; brlock is a "big read lock", which provides for mutual exlusion between
;;;; readers and writers. It is not as fast for readers as frlock, but more
;;;; appropriate if writes are not as fast.
;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (frlock (:constructor make-frlock (&key name))
                   (:predicate nil)
                   (:copier nil))
  "FRlock, aka Fast Read Lock.

Fast Read Locks allow multiple readers one potential writer to operate in
parallel while providing for consistency for readers and mutual exclusion
for writers.

Readers gain entry to protected regions without waiting, but need to retry if
a writer operated inside the region while they were reading. This makes frlocks
very efficient when readers are much more common than writers.

FRlocks are NOT suitable when it is not safe at all for readers and writers to
operate on the same data in parallel: they provide consistency, not exclusion
between readers and writers. Hence using an frlock to eg. protect an SBCL
hash-table is unsafe. If multiple readers operating in parallel with a writer
would be safe but inconsistent without a lock, frlocks are suitable.

The recommended interface to use is FRLOCK-READ and FRLOCK-WRITE, but those
needing it can also use a lower-level interface.

Example:

  ;; Values returned by FOO are always consistent so that
  ;; the third value is the sum of the two first ones.
  (let ((a 0)
        (b 0)
        (c 0)
        (lk (make-frlock)))
    (defun foo ()
       (frlock-read (lk) a b c))
    (defun bar (x y)
       (frlock-write (lk)
         (setf a x
               b y
               c (+ x y)))))"
  (mutex (make-mutex :name "FRLock mutex") :type mutex :read-only t)
  ;; Using FIXNUM counters makes sure we don't need to cons a bignum
  ;; for the return value, ever.
  (pre-counter 0 :type (and unsigned-byte fixnum))
  (post-counter 0 :type (and unsigned-byte fixnum))
  ;; On 32bit platforms a fixnum can roll over pretty easily, so we also use
  ;; an epoch marker to keep track of that.
  (epoch (list t) :type cons)
  (name nil))

(declaim (inline frlock-read-begin))
(defun frlock-read-begin (frlock)
  "Start a read sequence on FRLOCK. Returns a read-token and an epoch to be
validated later. Using FRLOCK-READ instead is recommended."
  (barrier (:read))
  (values (frlock-post-counter frlock)
          (frlock-epoch frlock)))

(declaim (inline frlock-read-end))
(defun frlock-read-end (frlock)
  "Ends a read sequence on FRLOCK. Returns a token and an epoch. If the token
and epoch are EQL to the read-token and epoch returned by FRLOCK-READ-BEGIN,
the values read under the FRLOCK are consistent and can be used: if the values
differ, the values are inconsistent and the read must be restated.

Using FRLOCK-READ instead is recommended.

Example:

  (multiple-value-bind (t0 e0) (frlock-read-begin *fr*)
    (let ((a (get-a))
          (b (get-b)))
      (multiple-value-bind (t1 e1) (frlock-read-end *fr*)
        (if (and (eql t0 t1) (eql e0 e1))
            (list :a a :b b)
            :aborted))))"
  (barrier (:read))
  (values (frlock-pre-counter frlock)
          (frlock-epoch frlock)))

(defmacro frlock-read ((frlock) &body value-forms)
  "Evaluates VALUE-FORMS under FRLOCK till it obtains a consistent
set, and returns that as multiple values."
  (once-only ((frlock frlock))
    (with-unique-names (t0 t1 e0 e1)
      (let ((syms (make-gensym-list (length value-forms))))
        `(loop
           (multiple-value-bind (,t0 ,e0) (frlock-read-begin ,frlock)
             (let ,(mapcar 'list syms value-forms)
               (barrier (:compiler))
               (multiple-value-bind (,t1 ,e1) (frlock-read-end ,frlock)
                (when (and (eql ,t1 ,t0) (eql ,e1 ,e0))
                  (return (values ,@syms)))))))))))

;;; Actual implementation.
(defun %%grab-frlock-write-lock (frlock wait-p timeout)
  (when (grab-mutex (frlock-mutex frlock) :waitp wait-p :timeout timeout)
    (let ((new (logand most-positive-fixnum (1+ (frlock-pre-counter frlock)))))
      ;; Here's our roll-over protection: if a reader has been unlucky enough
      ;; to stand inside the lock long enough for the counter to go from 0 to
      ;; 0, they will still be holding on to the old epoch. While it is
      ;; extremely unlikely, it isn't quite "not before heath death of the
      ;; universe" stuff: a 30 bit counter can roll over in a couple of
      ;; seconds -- and a thread can easily be interrupted by eg. a timer for
      ;; that long, so a pathological system could be have a thread in a
      ;; danger-zone every second. Run that system for a year, and it would
      ;; have a 1 in 3 chance of hitting the incipient bug. Adding an epoch
      ;; makes sure that isn't going to happen.
      (when (zerop new)
        (setf (frlock-epoch frlock) (list t)))
      (setf (frlock-pre-counter frlock) new))
    (barrier (:write))
    t))

;;; Interrupt-mangling free entry point for FRLOCK-WRITE.
(declaim (inline %grab-frlock-write-lock))
(defun %grab-frlock-write-lock (frlock &key (wait-p t) timeout)
  (%%grab-frlock-write-lock frlock wait-p timeout))

;;; Normal entry-point.
(declaim (inline grab-frlock-write-lock))
(defun grab-frlock-write-lock (frlock &key (wait-p t) timeout)
  "Acquires FRLOCK for writing, invalidating existing and future read-tokens
for the duration. Returns T on success, and NIL if the lock wasn't acquired
due to eg. a timeout. Using FRLOCK-WRITE instead is recommended."
  (without-interrupts
    (allow-with-interrupts (%%grab-frlock-write-lock frlock wait-p timeout))))

(declaim (inline release-frlock-write-lock))
(defun release-frlock-write-lock (frlock)
  "Releases FRLOCK after writing, allowing valid read-tokens to be acquired again.
Signals an error if the current thread doesn't hold FRLOCK for writing. Using FRLOCK-WRITE
instead is recommended."
  (setf (frlock-post-counter frlock)
        (logand most-positive-fixnum (1+ (frlock-post-counter frlock))))
  (release-mutex (frlock-mutex frlock) :if-not-owner :error)
  (barrier (:write)))

(defmacro frlock-write ((frlock &key (wait-p t) timeout) &body body)
  "Executes BODY while holding FRLOCK for writing."
  (once-only ((frlock frlock))
    (with-unique-names (got-it)
      `(without-interrupts
         (let (,got-it)
           (unwind-protect
                (when (setf ,got-it (allow-with-interrupts
                                      (%grab-frlock-write-lock ,frlock :timeout ,timeout
                                                                      :wait-p ,wait-p)))
                  (with-local-interrupts ,@body))
             (when ,got-it
               (release-frlock-write-lock ,frlock))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (brlock (:constructor make-brlock (&key name))
                   (:copier nil))
  (mutex (make-mutex :name "BRLock mutex") :type mutex :read-only t)
  (reader-count 0 :type word)
  (name nil))

(defun brlock-write-lock (brlock &key (wait-p t) timeout)
  "Acquires BRLOCK for writing. Waits until all readers have released the lock.
Returns T if the lock was acquired succesfully, NIL otherwise. Using
WITH-BRLOCK-WRITE instead is recommended. WARNING: There is no sanity checking to
verify that the current thread doesn't already hold the BRLOCK for reading:
recursive read -> write lock acquisition will deadlock."
  (when (grab-mutex (brlock-mutex brlock) :waitp wait-p :timeout timeout)
    (or (zerop (brlock-reader-count brlock))
        (unless (and wait-p
                     (wait-for (zerop (brlock-reader-count brlock)) :timeout timeout))
          (release-mutex (brlock-mutex brlock))
          nil)
        t)))

(defun brlock-write-unlock (brlock)
  "Releases BRLOCK from writing. Using WITH-BRLCK-WRITE instead is recommended."
  (setf (brlock-name brlock) nil)
  (release-mutex (brlock-mutex brlock)))

(defmacro with-brlock-write ((brlock &key (wait-p t) timeout) &body body)
  "Acquires BRLOCK for writing for the duration of BODY."
  (once-only ((brlock brlock))
    (with-unique-names (got-it)
      `(without-interrupts
         (let (,got-it)
           (unwind-protect
                (when (setf ,got-it (allow-with-interrupts (brlock-write-lock ,brlock
                                                                              :timeout ,timeout
                                                                              :wait-p ,wait-p)))
                  (with-local-interrupts ,@body))
             (when ,got-it
               (brlock-write-unlock ,brlock))))))))

(defun brlock-read-lock (brlock &key (wait-p t) timeout)
  "Acquires BRLOCK for reading. WARNING: There is no sanity checking to
verify that the current thread doesn't already hold the BRLOCK for reading:
recursive read lock acquisition will deadlock if there are writers waiting to
gain the lock."
  (loop with mutex = (brlock-mutex brlock)
        do (atomic-incf (brlock-reader-count brlock))
           (cond ((mutex-owner mutex)
                  (atomic-decf (brlock-reader-count brlock))
                  (unless wait-p
                    (return nil)))
                 (t
                  (return t)))
           (or (wait-for (not (mutex-owner mutex)) :timeout timeout)
               (return nil))))

(defun brlock-read-unlock (brlock)
  "Releases BRLOCK from reading. WARNING: There is no sanity checking to
verify that the current thread holds the BRLOCK for reading in the first place."
  (atomic-decf (brlock-reader-count brlock)))

(defmacro with-brlock-read ((brlock &key (wait-p t) timeout) &body body)
  "Acquires BRLOCK for reading for the duration of BODY."
  (once-only ((brlock brlock))
    (with-unique-names (got-it)
      `(without-interrupts
         (let (,got-it)
           (unwind-protect
             (when (setf ,got-it (allow-with-interrupts (brlock-read-lock ,brlock
                                                          :timeout ,timeout
                                                          :wait-p ,wait-p)))
               (with-local-interrupts ,@body))
             (when ,got-it
               (brlock-read-unlock ,brlock))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-brlocks ()
  (let ((rw (make-brlock)) (a 0) (b 0) (c 0)
         (n-readers 100) (reader-i 10000)
         (n-writers 10)  (writer-i 1000)
         (run! nil) (w-e! nil) (r-e! nil))
    (mapc #'join-thread
      (nconc
        (loop repeat n-readers
          collect (make-thread
                    (lambda ()
                      (loop until run! do (thread-yield))
                      (handler-case
                        (loop repeat reader-i
                          do (with-brlock-read (rw)
                               (assert (eql c (+ a b)))
                               (sleep 0.0001)))
                        (error (e)
                          (setf r-e! e))))))
        (loop repeat n-writers
          collect (make-thread
                    (lambda ()
                      (loop until run! do (thread-yield))
                      (handler-case
                        (loop repeat writer-i
                          do (assert
                               (with-brlock-write (rw)
                                 (setf a (random 10000)
                                   b (random 10000)
                                   c (+ a b))))
                          (sleep (random 0.001)))
                        (error (e)
                          (setf w-e! e))))))
        (progn
          (setf run! t)
          nil)))
    (list w-e! r-e!)))

(defun test-frlocks ()
  (let ((rw (make-frlock)) (a 0) (b 0) (c 0)
         (n-readers 100)   (reader-i 1000000)
         (n-writers 10)    (writer-i 10000)
         (run! nil) (w-e! nil) (r-e! nil))
    (mapc #'join-thread
      (nconc
        (loop repeat n-readers
          collect (make-thread
                    (lambda ()
                      (loop until run! do (thread-yield))
                      (handler-case
                        (loop repeat reader-i
                          do (multiple-value-bind (a b c)
                               (frlock-read (rw) a b c)
                               (assert (eql c (+ a b)))))
                        (error (e)
                          (setf r-e! e))))))
        (loop repeat n-writers
          collect (make-thread
                    (lambda ()
                      (loop until run! do (thread-yield))
                      (handler-case
                        (loop repeat writer-i
                          do (frlock-write (rw)
                               (setf a (random 10000)
                                 b (random 10000)
                                 c (+ a b)))
                          (sleep (random 0.0001)))
                        (error (e)
                          (setf w-e! e))))))
        (progn
          (setf run! t)
          nil)))
    (list w-e! r-e!)))

(defun test-all-rwlocks () 
  (loop repeat 1
    for f = (print (cons :frlock (time (test-frlocks))))
    for b = (print (cons :brlock (time (test-brlocks))))
    while (and (every #'null (cdr f)) (every #'null (cdr b)))))

;;;; Portions Copyright (c) 2012 Nikodemus Siivola <nikodemus@sb-studio.net>
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions: The above copyright notice and this permission
;;;; notice shall be included in all copies or substantial portions of the 
;;;; Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instrumented boxed reference 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reference ()
  ((value
    :reader deref
    :initarg :value)
   (validator
    :reader get-validator
    :initarg :validator)))

(defun validp (ref newval)
  (let ((validator (get-validator ref)))
    (%validp validator newval)))

(defun %validp (validator value)
  (or (not validator) (funcall validator value)))

(defmethod initialize-instance :after ((ref reference) &key value validator &allow-other-keys)
  (assert (%validp validator value)))


(defun set-validator (ref validator)
  "Attempt to set a new VALIDATOR for an AGENT, ATOM, or REF."
  (assert (%validp validator (deref ref)))
  (setf (slot-value ref 'validator) validator))


(defclass box (reference)
  ())


(defmethod pointer:deref ((box box) &optional (k #'identity) &rest args)
  (apply k (deref box) args))

(defmethod (setf pointer:deref) (value (box box) &optional (k #'identity) &rest args)
  (apply k (atomic-setf box value) args)) 

(defun make-box (value &optional validator)
  (make-instance 'box :value value :validator validator))


(defun compare-and-set! (atom oldval newval)
  "Atomically set a new value for an atom."
  (assert (validp atom newval))
  #+sbcl
  (eq (sb-ext:compare-and-swap (slot-value atom 'value) oldval newval) oldval))


(defun atomic-update! (atom f &rest args)
  "Set the value of ATOM to the result of applying F."
  (loop
     for oldval = (deref atom)
     for newval = (apply f oldval args)
     until (compare-and-set! atom oldval newval)
     finally (return newval)))

(defun atomic-setf (atom newval)
  "Set ATOM no NEWVAL, without regard to the previous value of ATOM."
  (atomic-update! atom (constantly newval)))


(defun flip (fn)
  "Return a function that swaps the order of the first two arguments to FN."
  (lambda (x y &rest args)
    (apply fn y x args)))

(defun atomic-adjoinf (atom &rest args)
  "ADJOIN an item to the list held by ATOM.  Accepts :KEY, :TEST, and :TEST-NOT arguments."
  (apply #'atomic-update! atom (flip #'adjoin) args))

(defun atomic-removef (atom &rest args)
  "REMOVE an item from the sequence held by ATOM.
Accepts :FROM-END, :TEST, :TEST-NOT, :START, :END, :COUNT, and :KEY."
  (apply #'atomic-update! atom (flip #'remove) args))

(defun atomic-unionf (atom &rest args)
  "Atomically sets the value of ATOM to the UNION of the previous
value and the provided list.  Accepts :KEY, :TEST, and :TEST-NOT."
  (apply #'atomic-update! atom #'union args))



#||
;; reasonably portable compare-and-swap
;; MIT Licensed: http://opensource.org/licenses/MIT
;; Copyright (c) 2012 Josh Marchán
;; https://github.com/sykopomp/memento-mori/blob/develop/src/utils.lisp
;;
;; (defmacro portable-compare-and-swap (place old-value new-value)
;;   #+sbcl      (let ((old-val-var (gensym “OLD-VALUE-“)))
;;                 `(let ((,old-val-var ,old-value))
;;                    (eq ,old-val-var (sb-ext:compare-and-swap ,place
;;                                       ,old-val-var ,new-value))))
;;   #+ccl       `(ccl::conditional-store ,place ,old-value ,new-value)
;;   #+lispworks `(system:compare-and-swap ,place ,old-value ,new-value)
;;   #+allegro   `(excl:atomic-conditional-setf ,place ,new-value ,old-value)
;;   #-(or allegro lispworks ccl sbcl) `(error “Not supported.”))
;;
;; (defmacro memo-thunk (&body body)
;;   (with-gensyms (storage thunk)
;;     `(let (,thunk ,storage)
;;        (setf ,thunk (lambda ()
;;                       (portable-compare-and-swap (svref ,storage 0) ,thunk
;;                         (let ((value (progn ,@body)))
;;                           (lambda () value)))
;;                       (funcall (svref ,storage 0))))
;;        (setf ,storage (make-array ‘(1) :element-type ‘function :initial-element ,thunk))
;;        ,thunk)))
;;
;; (defmacro lcons (s1 s2)
;;   `(cons ,s1 (memo-thunk ,s2)))
;;
;; (defun lcar (s)
;;   (car s))
;;
;; (defun lcdr (s)
;;   (when (cdr s) (funcall (cdr s))))
;;
;; ;; functional i/o streams using memoized SICP streams
;;
;; (defun istream (stream)
;;   (when-let (char (read-char stream nil nil))
;;     (lcons char (istream stream))))
;;
;; (defun i-unread-char (char istream)
;;   (cons char istream))
;;
;; (defun i-read-char (istream)
;;   (values (lcar istream) (lcdr istream)))
||#
