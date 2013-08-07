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

;;(delete-package :dstm)

(defpackage :dstm
  (:shadow :read :write :variable) 
  (:use :common-lisp)
  (:export
    #:variable
    #:make-value
 ;   #:read-var
 ;   #:write-var
 ;   #:write-vars
    #:atomic
    #:orelse
    #:check
    #:rollback
    #:read
    #:write  
    #:rmw
    
    ))

(in-package :dstm)

;; (defparameter *transactions* (make-hash-table :test #'eq :synchronized t))
;; (defvar *transaction* nil)
; (defclass transaction ()
;;   ((state   :accessor transaction-state  :initform :active  :initarg :state)
;;     (root   :reader   transaction-root   :initform nil)
;;     (reads  :accessor transaction-reads  :initform nil)
;;     (subs   :accessor transaction-subs   :initform nil)
;;     (parent :accessor transaction-parent :initform (current-transaction))))
 ;; (defun current-transaction ()
 ;;   (sb-thread:symbol-value-in-thread 'dstm::*transaction* sb-thread:*current-thread* nil))
 ;; (defun (setf current-transaction) (trans)
 ;;   (setf
 ;;     #+sbcl (sb-thread:symbol-value-in-thread 'dstm::*transaction* sb-thread:*current-thread* nil)
 ;;     trans))
 ;; (defun current-transaction ()
 ;;   (gethash sb-thread:*current-thread* *transactions*))
 ;; (defun (setf current-transaction) (trans)
 ;;   (setf (gethash sb-thread:*current-thread* *transactions*) trans))
;; (defmethod initialize-instance :after ((this-transaction transaction) &key &allow-other-keys)
;;   (setf (slot-value this-transaction 'root)
;;     (let ((current-transaction (current-transaction)))
;;       (if current-transaction
;;         (transaction-root current-transaction)
;;         this-transaction))))


(defparameter *nrolls* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *ntrans* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))

(tls:define .transaction. nil)

(defun current-transaction ()
  (tls:symbol-value '.transaction.))

(defun (setf current-transaction) (trans)
  (setf (tls:symbol-value '.transaction.) trans))
  
(defstruct (transaction (:constructor %make-transaction))
  (state  :active)
  (parent (current-transaction))
  (root   nil)
  (reads  nil)
  (subs   nil))

(defmethod print-object ((self transaction) stream)
  (print-unreadable-object (self stream :type nil :identity t)
    (format stream "~A ~S (~D reads, ~D subs)"
      (if (eq self (transaction-root self))
        :root-transaction
        :transaction)
      (transaction-state self)
      (length (transaction-reads self))
      (length (transaction-subs self))
      )))

(defun make-transaction (&key (state :active))
  (let ((this-transaction (%make-transaction :state state)))
    (setf (transaction-root this-transaction)
      (let ((current-transaction (current-transaction)))
        (if current-transaction
          (transaction-root current-transaction)
          this-transaction)))
    this-transaction))
                          
(defun equivalentp (trans1 trans2)
   (eq (transaction-root trans1) (transaction-root trans2)))

(defclass variable ()
   ())

(defclass transactional-variable (variable)
  ((new
     :accessor new-value-of
     :initform nil
     :initarg :new)
    (old
      :accessor old-value-of
      :initform nil)
    (trans
      :accessor transaction-of
      :initform (load-time-value (make-transaction :state :committed)))))

(defun make-value (&optional value)
  (make-instance 'transactional-variable :new value))

(defmethod print-object ((self transactional-variable) stream)
  (print-unreadable-object (self stream :type  nil :identity nil)
    (if (eq :committed (transaction-state (transaction-of self)))
      (format stream "TRANSACTIONAL-VALUE: ~S" (new-value-of self))
      (format stream "TRANSACTIONAL-VALUE: ~S" (old-value-of self)))
    (format stream " [~A]" (transaction-state (transaction-of self)))))
      
(define-condition rollback-execution ()
   ())

(defun reclaim-lists (trans)
  (setf
    (transaction-reads trans) nil
    (transaction-subs  trans) nil))


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
      (error (load-time-value
               (make-condition 'rollback-execution) t)))))


(defun check-reads (trans)
  (dolist (pair (transaction-reads trans))
    (destructuring-bind (var . vtrans) pair
      (let ((vnow (transaction-of var)))
        (unless (or (eq vnow vtrans)          ;; unchanged?
                  (equivalentp trans vnow))   ;; ...or changed by us...
          (rollback))))))

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

(defun commit (final)
  (commit-with-transaction (current-transaction) final))

(defun read (var)  
  (let ((trans (current-transaction)))
    (loop
      (let* ((vtrans (transaction-of var))
              (vstate (transaction-state vtrans)))
        (when (or
                (not (eq :ACTIVE vstate))
                (and trans (equivalentp trans vtrans)))
          (when trans (push (cons var vtrans) (transaction-reads trans)))
          (return (if (eq :ABORTED vstate)
                    (old-value-of var)
                    (new-value-of var))))))))

(defun read-var (var)
  (read var))

(defun write-var-with-transaction (trans var val)
  "trans is the current transaction for a thread"
  (loop
    (let* ((vtrans (transaction-of var))
            (vstate (transaction-state vtrans)))
      (cond
        ((not (eq :ACTIVE vstate))
          (when (eq (sb-ext:compare-and-swap (slot-value var 'trans) vtrans trans) vtrans)
            (when (eq :COMMITTED vstate)
              (setf (old-value-of var) (new-value-of var)))
            (return (setf (new-value-of var) val))))
        ((equivalentp trans vtrans)
          (return (setf (new-value-of var) val)))))))

(defun write (var val)
  (let* ((trans   (current-transaction))
          (wtrans  (or trans (make-transaction))))
    (prog1 (write-var-with-transaction wtrans var val)
      (unless trans (setf (transaction-state wtrans) :COMMITTED)))))

(defun write-var (var val)
  (write var val))

(defun write-vars (&rest pairs)
  (do ((pairs pairs (cddr pairs)))
    ((null pairs))
    (write (car pairs) (cdar pairs))))

(defun do-orelse (&rest fns)
  "Perform one of the functions in the list fns.  The list is
   examined in order, front to back.  The first one to succeed is
   the sub-transaction accepted.  If none succeed, or the overall
   transaction fails, the the whole thing is restarted."
  (let ((ct-save (current-transaction)))
    (loop
      (dolist (fn fns)
        (setf (current-transaction) (make-transaction))
        (sb-ext:atomic-incf (aref *ntrans* 0))
        (unwind-protect (handler-case (return-from do-orelse
                                        (prog1 (funcall fn)
                                          (commit (null ct-save))))
                          (rollback-execution (exn)
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
  (atomic (write place (funcall fn (read place)))))


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

(defvar *a* (make-value 0))
(defvar *b* (make-value 0))

(defvar *up-remaining*   0)
(defvar *down-remaining* 0)

(defun check-invariant (&aux x y)
  (atomic
    (setf
      x (read *a*)
      y (read *b*)))
  (unless (= y (* 2 x))    
    (error "~&Invariant broken: A = ~A, B = ~A~%" x y)))

(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read *a*))))
      (write *a* a)
      (write *b* (* 2 a)))))

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
    (setf *a* (make-value 0))
    (setf *b* (make-value 0))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (test-dstm 50000)
;; (:ROLLBACKS 41543 :TRANSACTIONS 166679 :PERCENT-ROLLBACKS 24.923956
;;   :TRANS-PER-ROLL 4.012204 :DURATION 0.186 :TRANS-PER-SEC 896123.6)

;; (test-dstm 100000)
;; (:ROLLBACKS 100717 :TRANSACTIONS 341761 :PERCENT-ROLLBACKS 29.470009
;;   :TRANS-PER-ROLL 3.39328 :DURATION 0.31 :TRANS-PER-SEC 1102454.9)

;; (test-dstm 500000)
;; (:ROLLBACKS 397331 :TRANSACTIONS 1650343 :PERCENT-ROLLBACKS 24.075663
;;   :TRANS-PER-ROLL 4.153572 :DURATION 1.658 :TRANS-PER-SEC 995381.8)

;; (test-dstm 1000000)
;; (:ROLLBACKS 826858 :TRANSACTIONS 3295458 :PERCENT-ROLLBACKS 25.090837
;;   :TRANS-PER-ROLL 3.9855187 :DURATION 3.263 :TRANS-PER-SEC 1009947.3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prior results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


