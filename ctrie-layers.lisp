;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layered Context Groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass grouped-layer-class (standard-layer-class) ())

(defgeneric group-root    (layer))
(defgeneric default-layer (layer))

(define-layered-method adjoin-layer-using-class ((to-add grouped-layer-class)
                                                  active-layers)

  (call-next-layered-method  to-add
    (remove-layer (group-root (find-layer to-add)) active-layers)))

(define-layered-method remove-layer-using-class ((to-remove grouped-layer-class)
                                                  active-layers)
  (declare (ignore active-layers))
  (multiple-value-bind (new-layers cacheable-p) (call-next-method)
    (values (adjoin-layer (default-layer (find-layer to-remove)) new-layers)
      cacheable-p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Allocation-Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer allocation-layer ()
  ((group-root
     :initform 'allocation-layer
     :reader group-root)
    (default-layer
      :initform 'fundamental
      :reader default-layer)))


(deflayer fundamental (allocation-layer) ()
  (:metaclass grouped-layer-class))

(deflayer transient  (allocation-layer) ()
  (:metaclass grouped-layer-class))

(deflayer cvm  (allocation-layer)
  ((base :special t :initform 0 :initarg :base :accessor base)) 
  (:metaclass grouped-layer-class))

(deflayer persistent (allocation-layer)
  ((storage-directory-pathname
     :accessor storage-directory-pathname
     :initform (apply 'open-store (ensure-list *default-mmap-dir*))))
  (:metaclass grouped-layer-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Balance-Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer balance-layer ()
  ((group-root
     :initform 'balance-layer
     :reader group-root)
    (default-layer
      :initform 'weight-balanced
      :reader default-layer)))

(deflayer unbalanced (balance-layer) ()
  (:metaclass grouped-layer-class))

(deflayer randomly-balanced (balance-layer) ()
  (:metaclass grouped-layer-class))

(deflayer height-balanced (balance-layer) ()
  (:metaclass grouped-layer-class))

(deflayer weight-balanced (balance-layer) ()
  (:metaclass grouped-layer-class))

(deflayer randomly-balanced (balance-layer) ()
  (:metaclass grouped-layer-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection-layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer functor-layer ()
  ((group-root
     :initform 'functor-layer
     :reader group-root)
    (default-layer
      :initform 'map
      :reader default-layer)))

(deflayer set (functor-layer) ()
  (:metaclass grouped-layer-class))

(deflayer bag (functor-layer) ()
  (:metaclass grouped-layer-class))

(deflayer map (functor-layer) ()
  (:metaclass grouped-layer-class))

(deflayer seq (functor-layer) ()
  (:metaclass grouped-layer-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transaction-layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer transaction-layer ()
  ((group-root
     :initform 'transaction-layer
     :reader group-root)
    (default-layer
      :initform 'dstm
      :reader default-layer)))

(deflayer isolated (transaction-layer) ()
  (:metaclass grouped-layer-class))

(deflayer dstm (transaction-layer) ()
  (:metaclass grouped-layer-class))

(deflayer unprotected (transaction-layer) ()
  (:metaclass grouped-layer-class))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reconstruct a Context from List of Names 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combined-layer-context (base &rest layers
                                &aux (result (contextl::copy-layer-context base)))
  (flet ((augment-with (layer)
           (setf result (adjoin-layer layer result))))
    (mapcar #'augment-with layers) result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reified Context 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *live-contexts* (make-hash-table))

(defclass context-layer-class (grouped-layer-class)
  ((abstract-namespace
     :accessor abstract-namespace-of
     :initform uuid:+NAMESPACE-URL+)
    (null-namespace
      :accessor null-namespace-of
      :initform (uuid:make-null-uuid))
    (anonymous-namespace
      :accessor anonymous-namespace-of
      :initform (uuid:make-v4-uuid))))

(defclass special-direct-context-layer-class (context-layer-class) ())

(deflayer special-direct-context (context-layer)
  ((id
     :special t
     :initarg :id
     :accessor id-of
     :accessor context-id
     :initform nil))
  (:metaclass special-direct-context-layer-class))



(defun compute-default-namespace ()
  (uuid:make-v5-uuid (abstract-namespace-of (class-of (find-layer 'context)))
    (princ-to-string  *root-uri*)))



(deflayer context-layer ()
  ((group-root
     :initform 'context-layer
     :reader root-contxt
     :reader group-root)
    (default-layer
      :initform 'context
      :accessor default-layer)

    (dynamic-state
      :special t
      :initform nil
      :accessor dynamic-state-of
      :accessor context-dynamic-state)))

(deflayer context (context-layer)
  ((committed-data
     :special t
     :initform nil
     :accessor committed-data-of
     :accessor context-committed-data)    
    (direct-parents
     :special t
     :initarg :parents
     :accessor parents-of
     :initform nil))
  (:metaclass context-layer-class))

(define-symbol-macro -context- (find-layer 'context))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (or (find-package :_) (make-package :_)))

(defmethod print-object ((self  =LAYER-CLASS-DEFINER-FOR-CONTEXT=) stream)
  (format stream "~S"
    (if (and (slot-exists-p self 'id) (slot-boundp self 'id))
      (intern (princ-to-string  (id-of self)) :_)
      (intern (princ-to-string (uuid:make-null-uuid)) :_))))


(let* ((null-namespace (null-namespace-of (make-instance 'context-layer-class)))
        (id-symbol (intern (princ-to-string null-namespace) :_))
        (nil-symbol (intern "NIL" :_)))
  (with-active-layers ((context-layer :id null-namespace))
    (defclass _::nil (context-layer)
      ()
      (:default-initargs :id null-namespace 'contextl::original-name id-symbol)
      (:metaclass context-layer-class))
;    (ensure-layer id-symbol
;      :metaclass 'grouped-layer-class)
;    (setf (id-of (find-layer null-symbol)) null-namespace)
    (setf (symbol-value id-symbol) nil-symbol)
    (export id-symbol :_)
    (setf (symbol-value nil-symbol) nil-symbol)
    (export nil-symbol :_)))

;; (defmethod initialize-instance :after ((self context-layer-class) &key)
;;   (when (and
;;           (slot-exists-p self 'direct-parents)
;;           (null (direct-parents-of self)))
;;     (setf (id-of self)
;;       (uuid:make-v5-uuid (abstract-namespace-of (class-of self))
;;         (princ-to-string *root-uri*)))))

          
;;   (unless (slot-boundp self 'id)
;;     (setf (id-of self) (uuid:make-uuid-from-string
;;                          (princ-to-string (layer-name self))))))
;
;(deflayer fundamental (allocation-layer) ()
;  (:metaclass grouped-layer-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection-layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro dlambda ((&rest args) &body body)
  (let ((env (gensym)))
    `(let ((,env (capture-dynamic-environment)))
       (lambda (,@args)
         (with-dynamic-environment (,env)
           ,@body)))))

(define-symbol-macro $layers (active-layers))


