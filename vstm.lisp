;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; original implementation by: Dr. David McClain
;;;;; updates, tests, and sbcl compatibility by: Dan Lentz
;;;;;
;;;;; portions Copyright (C) 2008-2010 by SpectroDynamics, LLC
;;;;;


#|
LispWorks Discussion
David McClain | 21 Jan 09:58
Re: STM…David McClain <dbm <at> refined-audiometrics.com>
2010-01-21 08:58:15 GMT


Here's a version that offers several compromises:

1. Uses Time-Order (TO) protocol, so that each var has both read &  
write timestamps. You know immediately when you need to rollback. And  
you get the advantage of preserved invariants inside the body of the  
transaction. I.e., if the transaction makes it through the invariant  
checking without being rolled back, then the invariant will be  
satisfied (assuming it is correct to begin with).

Rules of TO:
        a) Given TS the timestamp for a transaction, RTS the read timestamp  
on the variable, and WTS the write timestamp on a variable,
        b) For reads:
                If TS < WTS then rollback -- someone from future has already updated  
the var
                otherwise, set RTS to Max(RTS, TS)

        c) For writes:
                If TS < RTS then rollback -- someone from future has already read  
the var
                If TS < WTS then skip the write -- it has already been overwritten  
by someone from future
                otherwise set WTS = TS and write the new value into the var

2. Uses shared locks with multiple readers, single writer. In order to  
update the read time-stamps properly, a CAS operation is used in the  
read-var routine.

3. Runs about 3/4 of the speed of the lock-free DSTM (still pretty  
good!) I'm seeing about 600 KTrans/sec, with rollback percentages well  
below 1% (typ 0.15%).

4. Uses a hybrid architecture so that commit and rollback remain  
trivial, a la DSTM. And no need (thanks to TO) for commit time  
checking that vars haven't changed beneath you. So no consing of vars  
visited for reading. Just flip the transaction state from :ACTIVE  
to :ABORTED or :COMMITTED.

5. Since read / write can rollback at any time, it is no longer  
permitted to have naked read / write. We automatically wrap them in an  
ATOMIC if they are presented as naked. Otherwise, you could be getting  
rollback-exception errors in your code.

---------------------------------------------------------------------------------------

;; vstm-locking.lisp -- Software Transactional Memory using Versions
;; -- Runs about 1/3 speed of dstm-no-locking.lisp --
;; See paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;
;; Copyright (C) 2008-2010 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/RAL  01/10
;;  
-------------------------------------------------------------------------
|#

(defpackage :vstm
   (:use #:common-lisp)
   (:export
    #:var
    #:create-var
    #:read-var
    #:write-var
    #:write-vars
    #:atomic
    #:orelse
    #:rollback
    #:rmw
    #:check
    ))

;; ---------------------------------------------------------------------
(in-package :vstm)
;; ---------------------------------------------------------------------

(tls:define .transaction. nil)

(defun current-transaction ()
  (unless (bt:current-thread) 
    (warn "Starting Multiprocessing")
    (bt:start-multiprocessing))
  (tls:symbol-value '.transaction.))

  ;; #+lispworks (mp:process-private-property 'dstm:*transaction*)
  ;; #+sbcl (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil))


(defun (setf current-transaction) (trans)
  (unless (bt:current-thread) 
    (warn "Starting Multiprocessing")
    (bt:start-multiprocessing))
  (setf  (tls:symbol-value '.transaction.) trans))

    ;; #+lispworks (mp:process-private-property 'dstm:*transaction*) 
    ;; #+sbcl (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil)
    ;; trans))

;; ----------------------------------------------------

(defclass vstm::transaction ()
  ((state  :accessor transaction-state  :initform :active  :initarg :state)
    (vers   :reader   transaction-vers)
    (subs   :accessor transaction-subs   :initform nil)
    (parent :accessor transaction-parent)
    ))

;; ----------------------------------------------------


(defparameter *version* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *nfails*  (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *nrolls*  (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *ntrans*  (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))

(defmethod initialize-instance :after ((trans transaction)
                                        &key &allow-other-keys)
   (setf (slot-value trans 'vers)
         (let ((curtrans (current-transaction)))
           (setf (slot-value trans 'parent) curtrans)
           (if curtrans
               (transaction-vers curtrans)
             (sb-ext:atomic-incf (aref *version* 0))))))

(defun equivalentp (trans1 trans2)
   (= (transaction-vers trans1) (transaction-vers trans2)))

(defun do-orelse (&rest fns)
   ;; Perform one of the functions in the list fns.  The list is
   ;; examined in order, front to back.  The first one to succeed is
   ;; the sub-transaction accepted.  If none succeed, or the overall
   ;; transaction fails, the the whole thing is restarted.
   (let ((ct-save (current-transaction)))
     (loop
      (dolist (fn fns)
        (setf (current-transaction)
         (make-instance 'transaction))
        (sb-ext:atomic-incf (aref *ntrans* 0))
        (unwind-protect
            (handler-case
                (return-from do-orelse
                  (prog1
                      (funcall fn)
                    (commit)))
              (rollback-exn (exn)
                (declare (ignore exn)))
              (error (exn)
                (rollback-trans-and-subs (current-transaction))
                (error exn)) )
          ;; unwind
          (setf (current-transaction) ct-save)) )
      ;; end of list
      (when ct-save
        (rollback)) )))

(defmacro orelse (&rest clauses)
   `(apply #'do-orelse
     (list ,@(mapcar (lambda (clause)
                       `(lambda ()
                          ,clause))
                     clauses))))

(defmacro atomic (&body body)
   `(orelse
     (progn
       ,@body)))

(defmacro check (&body body)
   ;; Transaction checking
   `(unless (atomic ,@body)
      (rollback)))

;; common idiom... RMW = read / modify / write
(defun do-rmw (place fn)
   (atomic
     (write-var place (funcall fn (read-var place)))))

(defmacro rmw ((var-name place) &body body)
   `(do-rmw ,place (lambda (,var-name)
                     ,@body)))

;; ----------------------------------------------------

(defclass var () ())

(defvar *permanently-committed* (make-instance 'transaction
                                  :state :committed))


(defclass dstm-var (var)
  ((new   :accessor dstm-var-new   :initform nil  :initarg :new)
    (old   :accessor dstm-var-old   :initform nil)
    (oldv  :accessor dstm-var-oldv  :initform 0)
    (trans :accessor dstm-var-trans :initform *permanently-committed*)
    (rdv   :accessor dstm-var-rdv   :initform 0)
    (lock  :accessor dstm-var-lock  :initform (atom:make-frlock))))

(defun create-var (&optional val)
   (make-instance 'dstm-var :new val))


(define-condition rollback-exn ()
   ())

(defun reclaim-lists (trans)
   (setf (transaction-subs trans) nil))


(defun rollback-trans-and-subs (trans)
   (dolist (sub (transaction-subs trans))
     (rollback-trans-and-subs sub))
   (setf (transaction-state trans) :ABORTED)
   (reclaim-lists trans))

(defun rollback ()
   (let ((trans (current-transaction)))
     (when trans
       ;; rollbacks outside of transactions are permitted but
       ;; meaningless
       (sb-ext:atomic-incf (aref *nrolls* 0))
       (rollback-trans-and-subs trans)
       (error (load-time-value
               (make-condition 'rollback-exn)
               t)))))

(defun commit ()
   (let ((trans (current-transaction)))
     (commit-with-transaction trans
                              (null (transaction-parent trans))) ))

(defun commit-with-transaction (trans final)
   (dolist (sub (transaction-subs trans))
     (commit-with-transaction sub final))
   (unless final
     (let ((parent (transaction-parent trans)))
       (when parent
         ;; trans succeeded for now, add to parents subs-list, but
         ;; leave in :ACTIVE state
         ;; -- happens only on the first commit attempt
         (push trans (transaction-subs parent)))))
   (when final
     (setf (transaction-state trans) :COMMITTED)
     (reclaim-lists trans)))

;; ----------------------------------------------------

(defun current-val (var vtrans vstate)
   (if (eq :ABORTED vstate)
       (values (dstm-var-old var)
               (dstm-var-oldv var))
     (values (dstm-var-new var)
             (transaction-vers vtrans)) ))

(defun read-var (var)
   (let ((trans (current-transaction)))
     (if trans
         (read-var-with-transaction trans var)
       ;; else
       (atomic
         (read-var var))) ))

(defun read-var-with-transaction (trans var)
  (let ((tvers (transaction-vers trans))
         (rw (dstm-var-lock var)))
    (loop
      (atom:frlock-read (rw)
        (let* ((vtrans (dstm-var-trans var))
                (vstate (transaction-state vtrans)))
          (labels ((update-rdv ()
                     (loop for rdv = (dstm-var-rdv var)
                       until (sb-ext:compare-and-swap (slot-value var 'rdv) rdv
                               (max rdv tvers))) ))
            (cond
              ((not (eq :ACTIVE vstate))
                (multiple-value-bind (cval cvers)
                  (current-val var vtrans vstate)
                  (if (>= tvers cvers)
                    (progn
                      (update-rdv)
                      (return-from read-var-with-transaction cval))
                    ;; else
                    (rollback)) ))
              ((equivalentp trans vtrans)
                (update-rdv)
                (return-from read-var-with-transaction (dstm-var-new  
                                                         var)))
              ))))
      ;; (rollback) ;; un-comment to get pessimistic resolution
      )))

;; ----------------------------------------------------------

(defun write-var (var val)
   (let ((trans   (current-transaction)))
     (if trans
         (write-var-with-transaction trans var val)
       ;; else
       (atomic
         (write-var var val)))))

(defun write-vars (&rest pairs)
   (do ((pairs pairs (rest (rest pairs))))
       ((null pairs))
     (write-var (first pairs) (second pairs))))

(defun write-var-with-transaction (trans var val)
  ;; trans is the current transaction for a thread
  (let ((tvers (transaction-vers trans))
         (rw (dstm-var-lock var)))
    (loop
      ;;(mp:with-exclusive-lock ((dstm-var-lock var))
      (atom:frlock-write (rw)
        (let* ((vtrans (dstm-var-trans var))
                (vstate (transaction-state vtrans))
                (rdv    (dstm-var-rdv var)))
          (cond
            ((not (eq :ACTIVE vstate))
              (multiple-value-bind (cval cvers)
                (current-val var vtrans vstate)
                (if (>= tvers cvers)
                  (if (>= tvers rdv)
                    (progn
                      (when (eq :COMMITTED vstate)
                        (setf (dstm-var-old  var) cval
                          (dstm-var-oldv var) cvers))
                      (return-from write-var-with-transaction
                        (setf (dstm-var-trans var) trans
                          (dstm-var-new   var) val)) )
                    ;; else
                    (rollback))
                  ;; else
                  (return-from write-var-with-transaction cval))))
            ((equivalentp trans vtrans)
              (if (>= tvers rdv)
                (return-from write-var-with-transaction
                  (setf (dstm-var-new var) val))
                (rollback)))
            )))
      ;; (rollback) ;; uncomment to get pessimistic resolution
      )))


;; Test it out... hopefully lots of contention... yep!
#|

(defun show-rolls (&optional (duration 1))
   (let ((pcnt (/ (aref *nrolls* 0) (aref *ntrans* 0) 0.01))
         (rate (/ (aref *ntrans* 0) duration)))
     (list :rollbacks (aref *nrolls* 0)
           :transactions (aref *ntrans* 0)
           :percent-rollbacks pcnt
           :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
           :duration duration
           :trans-per-sec  rate)))

(defun reset ()
   (setf (current-transaction) nil)
   (setf (aref *nrolls* 0) 0)
   (setf (aref *ntrans* 0) 0))

(defvar *a* (create-var 0))
(defvar *b* (create-var 0))

(defun check-invariant (&aux a b)
   (atomic
     (setf a (read-var *a*)
           b (read-var *b*)
           ))
     (unless (= b (* 2 a))
       (error "Invariant broken: A = ~A, B = ~A" a  b)))

(defun common-code (delta)
   (atomic
     (let ((a (+ delta (read-var *a*))))
       (write-var *a* a)
       (write-var *b* (* 2 a))
       ;; (check (= (read-var *b*) (* 2 (read-var *a*))))
       )))

(defun count-up ()
   (loop repeat 500000 do (common-code 1)))

(defun count-down ()
   (loop repeat 500000 do (common-code -1)))

(defun checker (&rest procs)
  (let ((start (get-internal-real-time)))
    (loop while (some #'sb-thread::thread-alive-p procs)
           do (check-invariant))
    (let ((stop (get-internal-real-time)))
      (print (show-rolls (/ (- stop start) internal-time-units-per-second 1.0 )))) ))

(progn
  (print "Start VSTM Test...")
  (setf
    *a* (create-var 0)
    *b* (create-var 0))
  (reset)
  (sb-thread:make-thread (lambda ()
                           (checker
                             (sb-thread:make-thread #'count-up :name "up-counter")
                             (sb-thread:make-thread #'count-down :name "down-counter")))))
  

(progn
   (setf *a* (create-var 0)
         *b* (create-var 0))
   (reset)
   (let ((start (usec:get-time-usec))
         (ct 0)
         (down (bfly:spawn-link #'count-down
                                :name :down-counter))
         (up   (bfly:spawn-link #'count-up
                                :name :up-counter)))
     (loop until (= 2 ct)
           do
           (bfly:recv ()
             ( #T(bfly:exit-message :from-pid pid)
                 :when (or (eq pid down)
                           (eq pid up))
                 (incf ct))
             ( _ )))

     (let ((stop (usec:get-time-usec)))
       (show-rolls (* 1e-6 (- stop start))))
     ))

|#

;; Dr. David McClain
;; dbm <at> refined-audiometrics.com

