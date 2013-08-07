;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; Lock Free DTSM -- Software Transactional Memory after Herlihy, et. al.
;;;;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787
;;;;;
;;;;; implementation based on hdstm-lockfree STM by Dr. David McClain
;;;;; portions Copyright (C) 2008-2010 by SpectroDynamics, LLC
;;;;;
;;;;; Thanks also to pkhuong for assistance debugging :)
;;;;;

(defpackage :dstm
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
    #:check))

(in-package :dstm)

(defparameter *transactions* (make-hash-table :test #'eq :synchronized t))


(defvar *transaction* nil)


(defclass transaction ()
  ((state   :accessor transaction-state  :initform :active  :initarg :state)
    (root   :reader   transaction-root   :initform nil)
    (reads  :accessor transaction-reads  :initform nil)
    (subs   :accessor transaction-subs   :initform nil)
    (parent :accessor transaction-parent :initform (current-transaction))))


;; (defun current-transaction ()
;;   (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil))


;; (defun (setf current-transaction) (trans)
;;   (setf
;;     #+sbcl (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil)
;;     trans))

(defun current-transaction ()
  (gethash sb-thread:*current-thread* *transactions*))

(defun (setf current-transaction) (trans)
  (setf (gethash sb-thread:*current-thread* *transactions*) trans))


(defmethod initialize-instance :after ((this-transaction transaction) &key &allow-other-keys)
  (setf (slot-value this-transaction 'root)
    (let ((current-transaction (current-transaction)))
      (if current-transaction
        (transaction-root current-transaction)
        this-transaction))))

(defun equivalentp (trans1 trans2)
   (eq (transaction-root trans1) (transaction-root trans2)))


(defclass var ()
   ())


(defclass dstm-var (var)
  ((new
     :accessor dstm-var-new
     :initform nil
     :initarg :new)
    (old
      :accessor dstm-var-old
      :initform nil)
    (trans
      :accessor dstm-var-trans
      :initform (load-time-value (make-instance 'transaction :state :committed)))))


(defun create-var (&optional val)
   (make-instance 'dstm-var :new val))


(define-condition rollback-exn ()
   ())

(defun reclaim-lists (trans)
  (setf (transaction-reads  trans) nil
    (transaction-subs   trans) nil))

(defparameter *nrolls* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *ntrans* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))


(defun rollback-trans-and-subs (trans)
  (dolist (sub (transaction-subs trans))
    (rollback-trans-and-subs sub))
  (setf (transaction-state trans) :ABORTED)
  (reclaim-lists trans))


(defun rollback ()
  (let ((trans (current-transaction)))
    (when trans      
      (sb-ext:atomic-incf (aref *nrolls* 0))
      (rollback-trans-and-subs trans)
      (error (load-time-value (make-condition 'rollback-exn) t)))))


(defun check-reads (trans)
  (dolist (pair (transaction-reads trans))
    (destructuring-bind (var . vtrans) pair
      (let ((vnow (dstm-var-trans var)))
        (unless (or (eq vnow vtrans)          ;; unchanged?
                  (equivalentp trans vnow))   ;; ...or changed by us...
          (rollback))))))


(defun commit (final)
   (commit-with-transaction (current-transaction) final))


(defun commit-with-transaction (trans final)
   (dolist (sub (transaction-subs trans))
     (commit-with-transaction sub final))
   (check-reads trans)
   (let ((parent (shiftf (transaction-parent trans) nil)))
     (when parent
       ;; trans succeeded for now, add to parents subs-list, but leave
       ;; in :ACTIVE state -- happens only on the first commit attempt
       (push trans (transaction-subs parent))))
   (when final
     (setf (transaction-state trans) :COMMITTED)
     (reclaim-lists trans)))


(defun read-var (var)
  (let ((trans (current-transaction)))
    (loop
      (let* ((vtrans (dstm-var-trans var))
              (vstate (transaction-state vtrans)))
        (when (or
                (not (eq :ACTIVE vstate))
                (and trans (equivalentp trans vtrans)))
          (when trans (push (cons var vtrans) (transaction-reads trans)))
          (return (if (eq :ABORTED vstate)
                    (dstm-var-old var)
                    (dstm-var-new var))))))))


(defun write-var (var val)
   (let* ((trans   (current-transaction))
          (wtrans  (or trans (make-instance 'transaction))))
     (prog1 (write-var-with-transaction wtrans var val)
       (unless trans (setf (transaction-state wtrans) :COMMITTED)))))


(defun write-vars (&rest pairs)
   (do ((pairs pairs (cddr pairs)))
       ((null pairs))
     (write-var (car pairs) (cdar pairs))))


(defun write-var-with-transaction (trans var val)
  "trans is the current transaction for a thread"
  (loop
    (let* ((vtrans (dstm-var-trans var))
            (vstate (transaction-state vtrans)))
      (cond
        ((not (eq :ACTIVE vstate))  (when (eq (sb-ext:cas (slot-value var 'trans) vtrans trans)
                                            vtrans)
                                      (when (eq :COMMITTED vstate)
                                        (setf (dstm-var-old var) (dstm-var-new var)))
                                      (return (setf (dstm-var-new var) val))))
        ((equivalentp trans vtrans) (return (setf (dstm-var-new var) val)))))))


(defun do-orelse (&rest fns)
  "Perform one of the functions in the list fns.  The list is
   examined in order, front to back.  The first one to succeed is
   the sub-transaction accepted.  If none succeed, or the overall
   transaction fails, the the whole thing is restarted."
  (let ((ct-save (current-transaction)))
    (loop
      (dolist (fn fns)
        (setf (current-transaction)
          (make-instance 'transaction))
        (sb-ext:atomic-incf (aref *ntrans* 0))
        (unwind-protect (handler-case (return-from do-orelse
                                        (prog1 (funcall fn)
                                          (commit (null ct-save))))
                          (rollback-exn (exn)
                            (declare (ignore exn)))
                          (error (exn)
                            (rollback-trans-and-subs (current-transaction))
                            (error exn)))
          (setf (current-transaction) ct-save)))
      ;; end of list
      (when ct-save
        (rollback)))))


(defmacro orelse (&rest clauses)
  `(apply #'do-orelse (list ,@(mapcar (lambda (clause) `(lambda () ,clause)) clauses))))


(defmacro atomic (&body body)
  `(orelse (progn
             ,@body)))


(defmacro check (&body body)
  `(unless (atomic ,@body)
     (rollback)))


(defun do-rmw (place fn)
  "RMW = read / modify / write"
  (atomic (write-var place (funcall fn (read-var place)))))


(defmacro rmw ((var-name place) &body body)
   `(do-rmw ,place (lambda (,var-name) ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STM Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-rolls (&optional (duration 1))
  (let ((pcnt (/ (aref *nrolls* 0) (aref *ntrans* 0) 0.01))
         (rate (* 1.0 (/ (aref *ntrans* 0) duration))))
    (list
      :rollbacks (aref *nrolls* 0)
      :transactions (aref *ntrans* 0)
      :percent-rollbacks pcnt
      :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
      :duration (* 1.0 duration)
      :trans-per-sec  rate)))

(defun reset ()
  (setf (aref  *nrolls* 0) 0)
  (setf (aref  *ntrans* 0) 0))

(defvar *a* (create-var 0))
(defvar *b* (create-var 0))

(defvar *up-remaining*   0)
(defvar *down-remaining* 0)

(defun check-invariant (&aux x y)
  (atomic
    (setf
      x (read-var *a*)
      y (read-var *b*)))
  (unless (= y (* 2 x))    
    (error "~&Invariant broken: A = ~A, B = ~A~%" x y)))

(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read-var *a*))))
      (write-var *a* a)
      (write-var *b* (* 2 a)))))

(defun test-dstm (&optional (iterations 1000000))
  (flet ((count-up ()
           (loop for i from iterations downto 0
             do (setf *up-remaining* i)
             do (common-code 1)))
          (count-down ()
            (loop for i from iterations downto 0
              do (setf *down-remaining* i)
              do (common-code -1)))
          (checker ()
            (loop until (and (eql *up-remaining* 0) (eql *down-remaining* 0))
              do (check-invariant))))
    (format *trace-output* "~&Start DSTM Test...~%")
    (setf *a* (create-var 0))
    (setf *b* (create-var 0))
    (setf *up-remaining* iterations)
    (setf *down-remaining* iterations)
    (reset)
    (let ((start (get-internal-real-time))
           (procs (list
                    (sb-thread:make-thread #'count-down :name "down")
                    (sb-thread:make-thread #'count-up   :name "up")
                    (sb-thread:make-thread #'checker    :name "check"))))
      (loop while (some #'sb-thread:thread-alive-p procs))
      (let ((stop (get-internal-real-time)))
        (princ (show-rolls  (/ (- stop start) internal-time-units-per-second))
          *trace-output*)))))


;; (test-dstm 50000)
;; (:ROLLBACKS 30657 :TRANSACTIONS 153278 :PERCENT-ROLLBACKS 20.000914
;;   :TRANS-PER-ROLL 4.9997716 :DURATION 0.943 :TRANS-PER-SEC 162542.95)

;; (test-dstm 100000)
;; (:ROLLBACKS 63844 :TRANSACTIONS 308001 :PERCENT-ROLLBACKS 20.728504
;;   :TRANS-PER-ROLL 4.824275 :DURATION 2.1 :TRANS-PER-SEC 146667.14)

;; (test-dstm 500000)
;; (:ROLLBACKS 316705 :TRANSACTIONS 1545188 :PERCENT-ROLLBACKS 20.496212
;;   :TRANS-PER-ROLL 4.8789506 :DURATION 10.662 :TRANS-PER-SEC 144924.78)

;; (test-dstm 1000000)
;; (:ROLLBACKS 624829 :TRANSACTIONS 3069373 :PERCENT-ROLLBACKS 20.356894
;;   :TRANS-PER-ROLL 4.912341 :DURATION 21.683 :TRANS-PER-SEC 141556.66)
