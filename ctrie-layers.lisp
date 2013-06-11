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
  (call-next-layered-method to-add
    (remove-layer (group-root (find-layer to-add)) active-layers)))

(define-layered-method remove-layer-using-class ((to-remove grouped-layer-class)
                                                  active-layers)
  (declare (ignore active-layers))
  (multiple-value-bind (new-layers cacheable-p) (call-next-method)
    (values (adjoin-layer (default-layer (find-layer to-remove)) new-layers)
      cacheable-p)))

;; Allocation Layer Group 

(deflayer allocation-group ()
  ((group-root
     :initform 'allocation-group
     :reader group-root)
    (default-layer
      :initform 'fundamental
      :reader default-layer)))

(deflayer fundamental (allocation-group)
  ()
  (:metaclass grouped-layer-class))

(deflayer transient  (allocation-group)
  ()
  (:metaclass grouped-layer-class))

(deflayer cvm  (allocation-group)
  ((base :special t :initform 0 :initarg :base :accessor base)) 
  (:metaclass grouped-layer-class))

(deflayer persistent (allocation-group)
  ((storage-directory-pathname
     :accessor storage-directory-pathname
     :initform (apply 'open-store (ensure-list *default-mmap-dir*))))
  (:metaclass grouped-layer-class))

(deflayer persistent/cache (persistent)
  ()
  (:metaclass grouped-layer-class))

(deftype allocation-group-layer ()
  `(member cvm fundamental transient persistent persistent/cache))

(defun valid-allocation-group-layer-p (layer)
  (check-type layer symbol) 
  (typep layer 'allocation-group-layer))

;; Dicipline Layer Group

(deflayer dicipline-group ()
  ((group-root
     :initform 'dicipline-group
     :reader group-root)
    (default-layer
      :initform 'unordered
      :reader default-layer)))

(deflayer unordered (dicipline-group) ()
  (:metaclass grouped-layer-class))

(deflayer ordered   (dicipline-group) ()
  (:metaclass grouped-layer-class))

(deflayer height-balanced (ordered) ()
  (:metaclass grouped-layer-class))

(deflayer weight-balanced (ordered) ()
  (:metaclass grouped-layer-class))

(deftype dicipline-group-layer ()
  `(member unordered height-balanced weight-balanced))

(defun valid-dicipline-group-layer-p (layer)
  (check-type layer symbol) 
  (typep layer 'dicipline-group-layer))
  
;; Interface Layer Group


(deflayer interface-group ()
  ((group-root
     :initform 'interface-group
     :reader group-root)
    (default-layer
      :initform 'map
      :reader default-layer)))


(deflayer set (interface-group) ()
  (:metaclass grouped-layer-class))

(deflayer bag (interface-group) ()
  (:metaclass grouped-layer-class))

(deflayer map (interface-group) ()
  (:metaclass grouped-layer-class))

(deflayer seq (interface-group) ()
  (:metaclass grouped-layer-class))

(deftype interface-group-layer ()
  `(member map set bag seq))

(defun valid-interface-group-layer-p (layer)
  (check-type layer symbol) 
  (typep layer 'interface-group-layer))

(deftype valid-configuration-layer ()
  `(or allocation-group-layer dicipline-group-layer interface-group-layer))

(defun valid-configuration-layer-p (layer)
  (check-type layer symbol) 
  (typep layer 'valid-configuration-layer))

(defun valid-configuration-context-p (layers)
  (check-type layers list)
  (and
    (every #'valid-configuration-layer-p    layers)
    (some  #'valid-allocation-group-layer-p layers)
    (some  #'valid-dicipline-group-layer-p  layers)
    (some  #'valid-interface-group-layer-p  layers)))
  
(deftype valid-configuration-context ()
  `(satisfies valid-configuration-context-p))


(defun combined-layer-context (base &rest layers
                                &aux (result (contextl::copy-layer-context base)))
  (flet ((augment-with (layer)
           (setf result (adjoin-layer layer result))))
    (mapcar #'augment-with layers)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensuring Root and Findiomg Default Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass base-layer ()
  ((group-root
     :initform 'interface
     :reader group-root)
    (default-layer
      :initform 'map
      :reader default-layer)))

;;(deflayer default-context ()
  
