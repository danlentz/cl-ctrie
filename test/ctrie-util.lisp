;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/ctrie/util :in cl-ctrie/ctrie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CATCH-CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-catch-case/many-iterations :auto-call nil) (&optional (iterations 10))
  (let ((a 0) (b 0) (c 0) (d 0) (count 0)
         (start (get-internal-real-time)))
    (flet ((report (ts case-name case-count)
             (incf count)
             (when *debug*
               (sleep .01)
               (format t "Test ~5,'0D [tick ~7,'0D] Tag ~S now ~D~%" count
                 (- ts start) case-name case-count))))
      (loop repeat iterations do
        (catch-case (let ((key (alexandria:whichever :a :b :c :d)))
                      (when key (throw key (get-internal-real-time))))
          (:a (report it :A (incf a)))
          (:b (report it :B (incf b)))
          (:c (report it :C (incf c)))
          (:d (report it :D (incf d)))
          (t  "T")))
      (is (= iterations (+ a b c d))))))


(deftest check-catch-case/many-iterations-repeatedly (&key (rounds 128) (iterations 256))
  (is (notany #'null
        (loop repeat rounds
          collect (check-catch-case/many-iterations iterations)))))

;; (check-catch-case/many-iterations-repeatedly)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATOMIC-UPDATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unless CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package ;; package error handled by find-package* already
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil))))

(deftest check-atomic-update/parallel (&key (threads 512) (iterations 512))
  ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
  ;; atomic update with (INCF (CDR X)), the result becomes
  ;; unpredictable.
  (let ((x (cons :count 0)))
    (mapc #'sb-thread:join-thread
      (loop repeat threads
        collect (sb-thread:make-thread
                  (lambda ()
                    (loop repeat iterations
                      do (atomic-update (cdr x) #'1+)
                      (sleep 0.00001))))))
    (is (= (* threads iterations) (cdr x)))))

;; (CHECK-ATOMIC-UPDATE/PARALLEL)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOL-HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (fully-qualified-symbol-name :xyz)   ":XYZ"
;; (fully-qualified-symbol-name 'xyz)   "CL-CTRIE::XYZ"
;; (fully-qualified-symbol-name 'list)  "COMMON-LISP:LIST"
;; (fully-qualified-symbol-name 'ctrie) "CL-CTRIE:CTRIE"



(define-test check-byte-vector-hex-string-roundrip
  (loop repeat 10 do
    (let* ((bv0 (create-unique-id-byte-vector))
            (bv1 (hex-string-to-byte-vector (byte-vector-to-hex-string bv0))))
      (assert-equalp bv0 bv1))))

;; (test-byte-vector-hex-string-roundrip)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;
;; (test-byte-vector-hex-string-roundrip)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185))




#|
(defparameter *x* '(1 2 3))
(defparameter *write-x* (get-place (car *x*)))
(funcall *write-x* 4)
(print *x*)
(defun no-really (set-place)
  (let ((*x* 42))
   (funcall set-place 7)))
(no-really *write-x*)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARCS, FLAGS, HASHING and BITMAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test check-flag-computation
  (with-ctrie (make-ctrie)
    (assert-eql 2048     (flag nil 0))
    (assert-eql 1        (flag nil 5))
    (assert-eql 1        (flag nil 10))
    (assert-eql 65536    (flag nil 15))
    (assert-eql 1        (flag nil 20))
    (assert-eql 256      (flag nil 25))
    (assert-eql 4        (flag t 0))
    (assert-eql 1024     (flag 0 0))
    (assert-eql 67108864 (flag 1 0))
    (assert-eql 262144   (flag 1 5))
    (assert-eql 2048     (flag 1 10))
    (assert-eql 8388608  (flag 1 15))))


(define-test check-flag-arc-position
  (loop for power from 0 to 32 do
    (assert-eql 0 (flag-arc-position (expt 2 power) 0))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 1)) #x10000000) 1))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 2))
    (assert-eql 2 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 3))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 3)) #x10000000) 4))))

