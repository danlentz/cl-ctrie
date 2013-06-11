;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar       *ctrie*         nil
  "Within the dynamic extent of a CTRIE operation this variable will
  be bound to the root-container CTRIE operand.  It is an error if an
  operation is defined that attempts access to a CTRIE without this
  binding, which is properly established by wrapping the operation in
  an appropriate WITH-CTRIE form.")

(defvar       *ctrie-registry*   nil)
(defvar       *ctrie-seqs*       nil)


(defvar       *hash-code*     nil
  "Special variable used to store the hash-code that corresponds to the
  current operation.  Used as a means of improving efficiency by eliminating
  needless recomputation of the hash function, which is the most expensive
  part of most user-level ctrie operations.  If this value is not set, then
  the hash will simply be computed on demand and processing will continue
  unaffected.  Use of this variable is simply an optional performace
  optimization techniqie. For additional detail, refer to the documentation
  for functions `FLAG` and `CTHASH`")

(defparameter *retries*        16
  "Establishes the number of restarts permitted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-RETRIES-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.")


(defparameter *timeout*         2
  "Establishes the duration (in seconds) allotted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-TIMEOUT-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.")


(defparameter *context*       nil
  "Diagnostic value, not used in production, used to maintain
  additional information tracking the current dynamic state.")


(defparameter *debug*         nil
  "Debugging flag, not used in production, to enable generation of
  additional diagnostic and reporting information.")

(defvar *delta* 0)

(defun stamp-adjust-delta (&optional from)
  (if (zerop *delta*)
    from
    (+ *delta* (or from 0))))

(defun funcall-with-delta (d thunk)
  (let ((*delta* (+ *delta* (or d 0))))
    (funcall thunk)))

(defmacro with-delta (&optional (d 0) &body body)
  `(funcall-with-delta ,d (lambda () ,@body)))
  
(defparameter *timestamps-enabled* t)

(defparameter *timestamp-factory* 'stamp-adjust-delta)

;;'osicat:get-monotonic-time

(defparameter *pool-enabled*    t)

(defparameter *pool-high-water* (expt 2 10))

(defvar       *pool-list*        nil)

(defvar       *pool-worker*      nil)

(defvar       *pool-queue*       nil)

(defparameter *default-mmap-dir* nil)


(defparameter *root-uri*
  (if (short-site-name)
    (puri:uri (short-site-name))
    (make-instance 'puri:uri :scheme :http :host (machine-instance))))

