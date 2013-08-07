;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; Lock Free DTSM -- Software Transactional Memory after Herlihy, et. al.
;;;;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787
;;;;;
;;;;; implementation based on hdstm-lockfree STM by Dr. David McClain
;;;;; portions Copyright (C) 2008-2010 by SpectroDynamics, LLC
;;;;;

(defpackage :hdstm
  (:use :common-lisp))


(in-package :hdstm)


(defclass dstm-ref ()
  ((cell  :accessor dstm-ref-cell  :initform nil  :initarg :cell)))

(defclass dstm-val ()
  ((old   :reader   dstm-val-old   :initform nil  :initarg :old)
   (new   :accessor dstm-val-new   :initform nil  :initarg :new)
   (trans :reader   dstm-val-trans :initform nil  :initarg :trans)))

(defclass transaction ()
  ((state  :accessor transaction-state :initarg :state :initform :active)
   (reads  :accessor transaction-reads :initform nil)))

(defun create-var (&optional val)
  (make-instance 'dstm-ref :cell (make-instance 'dstm-val :new val)))

;;:trans (load-time-value (make-instance 'transaction :state :committed)))))

(defmacro volatile (&body body)
  `(progn
     (sb-thread:barrier (:data-dependency)
       ,@body)))
  
(defmacro flush-volatile (&body body)
  `(prog1 (progn ,@body)
     (sb-thread:barrier (:data-dependency))))

(defun current-transaction ()
  (tls:symbol-value 'transaction))

(defun set-current-transaction (trans)
  (setf (tls:symbol-value 'transaction) trans))
  
(define-condition rollback-exn ()
  ())

(defun set-state (trans new-state)
  (tagbody :iter
    (let ((old-state (volatile (transaction-state trans))))
      (unless (or (eq old-state new-state)
                (flush-volatile (eq (sb-ext:compare-and-swap (slot-value trans 'state)
                                  old-state new-state) old-state)))
        (go :iter)))))

(defun abort-transaction (trans)
  (set-state trans :ABORTED)
  (setf (transaction-reads trans) nil))

(defparameter *nrolls* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *ntrans* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *nfails* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))

(defun rollback (trans)
  (when trans
    (sb-ext:atomic-incf (aref *nrolls* 0))
    (abort-transaction trans)
    (when (eq trans (current-transaction))
      (error (make-condition 'rollback-exn)))))

(defun check-reads (trans)
  (dolist (pair (shiftf (transaction-reads trans) nil))
    (destructuring-bind (var . vref) pair
      (let ((vnow (volatile (dstm-ref-cell var))))
        (unless (or (eq vref vnow) (eq trans (dstm-val-trans vnow)))
          (rollback trans))))))

(defun commit ()
  (let ((trans (current-transaction)))
    (unless (eq :ACTIVE (volatile (transaction-state trans)))
      (rollback trans))
    (check-reads trans)
    (set-state trans :COMMITTED)))

(defun current-value (vref)
  (anaphora:aif (dstm-val-trans vref)
    (if (eq :aborted (volatile (transaction-state anaphora:it)))
      (dstm-val-old vref)
      (dstm-val-new vref))   
    (dstm-val-new vref)))

(defun conflict-manager-for-read (trans1 trans2)
  (declare (ignore trans1 trans2))
  nil)

;; either rollback trans1 - the reader and don't return
;; or else rollback trans2 - the writer and do return
;; or... just wait and return
;; (sleep 0.01))

(defun conflict-manager-for-write (trans1 trans2)
  (declare (ignore trans2))
  (rollback trans1))

;; either rollback trans1 - the new writer and don't return
;; or else rollback trans2 - the old writer and do return
;; or... just wait and return

(defun read-var (var)
  (let (result (trans (current-transaction)))
    (tagbody :iter
      (let* ((vref (volatile (dstm-ref-cell var)))
              (vtrans (dstm-val-trans vref)))         
        (when (null trans)
          (setf result (dstm-val-new vref))
          (return-from read-var result))
        (when (eq :ABORTED (volatile (transaction-state trans)))
          (rollback trans))
        (when (eq trans vtrans)
          (push (cons var vref) (transaction-reads trans))
          (setf result (dstm-val-new vref))
          (return-from read-var result))
        (when (and vtrans (eq :ACTIVE (volatile (transaction-state vtrans))))
          (conflict-manager-for-read trans vtrans)
          (go :iter))
        (let ((val (current-value vref)))
          (push (cons var vref) (transaction-reads trans))
          (setf result val)
          (return-from read-var result))))))

(defun write-var (var val)
  (let ((trans  (current-transaction)))
    (tagbody :iter
      (let* ((vref (volatile (dstm-ref-cell var)))
              (vtrans (dstm-val-trans vref)))
        (when (or (null trans) (eq trans vtrans))
          (return-from write-var
            (flush-volatile (setf (dstm-val-new vref) val))))        
        (when (eq :ABORTED (volatile (transaction-state trans)))
          (rollback trans))          
        (when (and vtrans (eq :active (volatile (transaction-state vtrans))))
          (conflict-manager-for-write trans vtrans)
          (go :iter))          
        (let ((oldv (current-value vref)))
          (unless (flush-volatile
                    (volatile (eq (sb-ext:compare-and-swap (slot-value var 'cell) vref
                                (make-instance 'dstm-val :old oldv :new val :trans trans)) vref)))
            (go :iter)))))))


(defun do-atomic (fn)
  ;; Perform the function fn under a transaction, allowing restart
  ;; when needed
  (if (current-transaction)
    (funcall fn)    
    (loop (set-current-transaction (progn
                                     (sb-ext:atomic-incf (aref *ntrans* 0))
                                     (make-instance 'transaction)))
      (handler-case (return-from do-atomic
                      (prog1 (funcall fn)
                        (commit)
                        (set-current-transaction nil)))        
        (rollback-exn (exn)
          (declare (ignore exn)))
        (error (exn)
          (abort-transaction (current-transaction))
          (set-current-transaction nil)
          (error exn))))))
    
(defmacro atomic (&body body)
  `(do-atomic (lambda () ,@body)))

(defun show-rolls ()
  (list *nrolls* *ntrans* (/ *nrolls* *ntrans* 0.01)))

;; ---------------------------------------------------------------

(defun do-orelse (&rest fns)
  ;; Perform one of the functions in the list fns.  The list is
  ;; examined in order, front to back.  The first one to succeed is
  ;; the sub-transaction accepted.  If none succeed, or the overall
  ;; transaction fails, the the whole thing is restarted.
  ;;
  ;; Each level of ORELSE nesting pushes a new hashtable onto the
  ;; write cache list of hashtables. The last one is the one belonging
  ;; to the outermost ATOMICALLY transaction.
  (if (current-transaction)
    (progn
      (dolist (fn fns)
        (handler-case (return-from do-orelse (funcall fn))            
          (rollback-exn (exn)
            (declare (ignore exn)))
          (error (exn)
            (abort-transaction (current-transaction))
            (set-current-transaction nil)
            (error exn))))        
      (rollback (current-transaction)))    
    (loop
      (set-current-transaction (make-instance 'transaction))
      (dolist (fn fns)
        (handler-case  (return-from do-orelse
                         (prog1 (funcall fn)
                           (commit)
                           (set-current-transaction nil)))
          (rollback-exn (exn)
            (declare (ignore exn)))
          (error (exn)
            (abort-transaction (current-transaction))
            (set-current-transaction nil)
            (error exn)))))))
     
(defmacro orelse (&rest clauses)
  `(apply #'do-orelse ,(mapcar (lambda (clause)
                                 `(lambda () ,clause))
                         clauses)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STM Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-rolls (&optional (duration 1))
  (let ((pcnt (/ (aref *nrolls* 0) (aref *ntrans* 0) 0.01))
         (rate (* 1.0 (/ (aref *ntrans* 0) duration))))
    (list
      :failures     (aref *nfails* 0)
      :rollbacks    (aref *nrolls* 0)
      :transactions (aref *ntrans* 0)
      :percent-rollbacks pcnt
      :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
      :duration (* 1.0 duration)
      :trans-per-sec  rate)))

(defun reset ()
  (setf (aref  *nrolls* 0) 0)
  (setf (aref  *nfails* 0) 0)
  (setf (aref  *ntrans* 0) 0))

(defvar *a*  (create-var 0))
(defvar *b*  (create-var 0))
(defvar *up-remaining*   0)
(defvar *down-remaining* 0)

(defun check-invariant (&aux x y)
  (atomic
    (setf
      x (:printv (read-var *a*))
      y (:printv (read-var *b*))))
  (unless (= y (* 2 x))
    (sb-ext:atomic-incf (aref *nfails* 0))
    ;; (describe *a*)
    ;; (describe (dstm-ref-cell *a*))
    ;; (describe (dstm-val-trans (dstm-ref-cell *a*)))
    ;; (describe *b*)
    ;; (describe (dstm-ref-cell *b*))
    ;; (describe (dstm-val-trans (dstm-ref-cell *b*)))
    (format *trace-output* "~&Invariant broken: A = ~A, B = ~A~%" x y)))

(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read-var *a*))))
      (write-var *a* a)
      (write-var *b* (* 2 a))
    ;; (:printv (read-var *a*) a)
    ;; (:printv (read-var *b*))
    )))

(defun test-dstm (&optional (iterations 1000000))
  (flet ((count-up ()
           (loop for k from iterations downto 0
             do (setf *up-remaining* k)
             (common-code 1)))
          (count-down ()
            (loop for j from iterations downto 0
              do (setf *down-remaining* j)
              (common-code -1)))
          (checker ()
            (sleep 0.001)
            (loop until (and (eql *up-remaining* 0) (eql *down-remaining* 0))
              do (check-invariant))))
    (format *trace-output* "~&Start DSTM Test...~%")
    (setf *a* (create-var 0))
    (setf *b* (create-var 0))
    (reset)
    (setf *up-remaining* iterations)
    (setf *down-remaining* iterations)
    (let ((start (get-internal-real-time))
           (procs (list
                    (sb-thread:make-thread (lambda () (count-down)) :name "down")
                    (sb-thread:make-thread (lambda () (count-up))   :name "up")
                    (sb-thread:make-thread (lambda () (checker))    :name "check"))))
      (loop while (some #'sb-thread:thread-alive-p procs))
      (let ((stop (get-internal-real-time)))
        (princ (show-rolls  (/ (- stop start) internal-time-units-per-second))
          *trace-output*)))))


;; (test-dstm 50000)
;; (:FAILURES 0 :ROLLBACKS 66170 :TRANSACTIONS 166173 :PERCENT-ROLLBACKS 39.819946
;;   :TRANS-PER-ROLL 2.5113041 :DURATION 0.262 :TRANS-PER-SEC 634248.06)


;; (test-dstm 100000)
;; (:FAILURES 0 :ROLLBACKS 129323 :TRANSACTIONS 329326 :PERCENT-ROLLBACKS
;;   39.268993 :TRANS-PER-ROLL 2.5465384 :DURATION 0.521 :TRANS-PER-SEC 632103.6)


#|

;; (defun check-invariant (&aux a b)
;;   (atomic
;;     (setf a (read-var *a*)
;;           b (read-var *b*)))
;;   (unless (= b (* 2 a))
;;     (sb-ext:atomic-incf (aref *nfails* 0))
;;     (format *trace-output* "Invariant broken: A = ~A, B = ~A" a b)))

;; (defun common-code (delta)
;;   (atomic
;;     (let ((a (+ delta (read-var *a*))))
;;       (write-var *a* a)
;;       (write-var *b* (* 2 a))))
;;   (check-invariant))

;; (defun count-up ()
;;   (loop repeat 5000000 do (common-code 1)))

;; (defun count-down ()
;;   (loop repeat 5000000 do (common-code -1)))

;; (progn
;;   (setf *a* (create-var 0)
;;         *b* (create-var 0))
;;   (reset)
;;   (set-current-transaction nil)
;;   ;; (common-code 1)
;;   ;; (count-up)
;;   (sb-thread:make-thread #'count-up)
;;   ;; (common-code 1)
;;   (sb-thread:make-thread #'count-down)
;;   )


|#
