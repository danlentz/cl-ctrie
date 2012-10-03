;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-condition ctrie-error (error) 
  ((ctrie :initform *ctrie* :initarg :ctrie :reader ctrie-of))
  (:documentation "Abstract superclass of CTRIE related conditions."))


(define-condition ctrie-structural-error (ctrie-error)
  ((node :initarg :node :reader node-of))
  (:documentation "Condition designating that the CTRIE data structure
   has been determined to be invalid."))


(define-condition ctrie-generational-mismatch (ctrie-structural-error)
  ((expected :initarg :expected :reader expected)
    (found   :initarg :found    :reader found))
  (:documentation "Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal"))


(define-condition ctrie-operational-error (ctrie-error)
  ((op :initarg :op :reader op-of))
  (:documentation "Condition for when an operational failure or
  inconsistency has occurred."))


(define-condition ctrie-modification-failed  (ctrie-operational-error)
  ((place   :initarg :place  :reader place-of)
    (reason :initarg :reason :reader reason-of))
  (:report
    (lambda (condition stream)
      (with-slots (ctrie op place reason) condition
        (format stream "CTRIE MODIFICATION FAILED~%")
        (format stream "-------------------------~%~%")
        (format stream "FAILURE: Operation ~S failed to modify ~S in CTRIE at #x~X~%~%"
          op place (if ctrie (sb-kernel:get-lisp-obj-address ctrie) 0))
        (format stream "CAUSE:   ~A~%~%" reason)
        (format stream "ABOUT:   ~A~%~%"
          (documentation (type-of condition) 'type)))))
  (:documentation
        "This condition indicates an unhandled failure of an attempt to
         perform stateful modification to CTRIE.  The most common case in
         which this might occur is when such an attempt is mode on a CTRIE
         designated as READONLY-P.  In any case, this condition represents an
         exception from which processing cannot continue and requires
         interactive user intervention in order to recover."))


(define-condition ctrie-invalid-dynamic-context (ctrie-operational-error)
  ((ctrie-context :initform *ctrie* :initarg :ctrie-context :reader ctrie-context)
    (gen-context  :initarg :gen-context   :reader gen-context))
  (:documentation "Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form"))


(define-condition ctrie-operation-timeout-exceeded (ctrie-operational-error)
  ((seconds :initform *timeout* :initarg :seconds :reader seconds-of))
  (:documentation "Condition indicating an operation has failed the
   maximum number of times specified by the s-variable *retries*"))


(define-condition ctrie-operation-retries-exceeded (ctrie-operational-error)
  ((retries :initform *retries* :initarg :retries :reader retries-of))
  (:documentation "Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*"))


(define-condition ctrie-not-implemented (ctrie-error) ()
  (:documentation "Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded."))


(define-condition ctrie-not-supported (ctrie-error) ()
  (:documentation "Condition designating functionality that is
  deliberately not supported."))


(defmacro ctrie-error (condition &rest args)
  "Signal a CTRIE related condition."
  `(error ',condition :ctrie *ctrie* ,@args))


(defun ctrie-modification-failed (reason &key op place)
  "Signal a modification failure with the appropriate attendant metadata."
  (ctrie-error ctrie-modification-failed :op op :place place :reason reason))
