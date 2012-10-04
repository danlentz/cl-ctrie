;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-mmap)


(defclass mm-metaclass (standard-class)
  ((mtagmap
     :accessor mm-metaclass-mtagmap
     :initform nil)
    (tag
      :reader mm-metaclass-tag
      :initform nil)
    (len
      :initform 0
      :accessor mm-metaclass-len)
    (default-instantiator
      :initform nil)
    (default-walker
      :initform nil)
    (walker
      :initarg walker
      :initform nil)
    (instantiator
      :accessor mm-instantiator
      :initarg instantiator
      :initform nil)
    (allocator
      :initform nil))
  (:documentation "Metaclass for memory mapped objects."))


(defclass mm-object ()
  ((%ptr :type mptr :accessor %ptr :initarg %ptr))
  (:documentation "Base class for all memory mapped objects."))


(declaim (ftype (function (mm-object) (mptr)) ptr))

(defun ptr (object)
  (declare (type mm-object object))
  (%ptr object))

;;  (the mptr (%ptr object)))

#+(or)
(define-compiler-macro ptr (object)
  `(the mptr (%ptr (the mm-object ,object))))


(defun mm-object-pointer (mm-object)
  (mptr-pointer (ptr mm-object)))


(defmethod initialize-instance :around ((class mm-metaclass) &rest all-keys)
  (ensure-inherits-from-mm-object class #'call-next-method all-keys))


(defmethod reinitialize-instance :around ((class mm-metaclass) &rest all-keys)
  (ensure-inherits-from-mm-object class #'call-next-method all-keys))


(defun ensure-inherits-from-mm-object (class next-method all-keys)
  (let ((parent (find-class 'mm-object)))
    (labels ((inherits-from (classes)
               (loop
                 for class in classes
                 thereis (or (subtypep class parent)
                           (inherits-from (class-direct-subclasses class))))))
      (let ((all-keys (copy-list all-keys)))
        (symbol-macrolet ((direct-superclasses (getf all-keys :direct-superclasses)))
          (setf direct-superclasses (if (inherits-from direct-superclasses)
                                      direct-superclasses
                                      (cons parent direct-superclasses))))
        (apply next-method class all-keys)))))


(deftype mm-slot-definition-reader ()
  `(function (mm-object) t))

(deftype mm-slot-definition-writer ()
  `(function (t mm-object) t))


(defgeneric slot-definition-memory-mapped (slotd)
  (:method (slotd) (declare (ignorable slotd))))


(defgeneric slot-definition-mmap-pointer-p (slotd)
  (:method (slotd) (declare (ignorable slotd))))


(defclass mm-slot-definition (slot-definition)   
  ((persistent
     :initarg :persistent     
     :reader slot-definition-memory-mapped-p
     :reader slot-definition-memory-mapped
     :initform t)))


(defclass mm-effective-slot-definition (mm-slot-definition standard-effective-slot-definition)
  ((offset
     :initarg :offset
     :reader mm-slot-definition-offset)
    (mmap-pointer-p
      :initform nil
      :initarg :mmap-pointer-p
      :accessor slot-definition-mmap-pointer-p)
    (writer-function
      :accessor slot-definition-writer-function)
    (reader-function
      :accessor slot-definition-reader-function)))


(defclass mm-direct-slot-definition (standard-direct-slot-definition mm-slot-definition)
  ())


(defmethod validate-superclass ((class mm-metaclass) (super standard-class))
  "Memory mapped classes may inherit from ordinary classes."
  t)

(defmethod validate-superclass ((class standard-class) (super mm-metaclass))
  "Ordinary classes may NOT inherit from memory mapped classes."
  nil)

(defmethod slot-definition-allocation ((slotd mm-slot-definition))
  (if (slot-definition-memory-mapped slotd)
    :memory
    (call-next-method)))


(defmethod direct-slot-definition-class ((class mm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'mm-direct-slot-definition))

(defvar *mop-hack-effective-slot-definition-class* nil)


(defmethod effective-slot-definition-class ((class mm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (or *mop-hack-effective-slot-definition-class* (call-next-method)))


(defmethod compute-slots :before ((class mm-metaclass))
  (with-slots (len) class
    (setf len 0)))


(defmethod compute-effective-slot-definition :around ((class mm-metaclass) name dslotds)
  (declare (ignorable name))
  (let ((dslotds (remove nil dslotds))) 
    (let ((last-dslot (first (last dslotds)))) 
      (let ((*mop-hack-effective-slot-definition-class*
              (when (slot-definition-memory-mapped last-dslot) 
                (find-class 'mm-effective-slot-definition))))
        (let ((eslot (call-next-method))) 
          (when (slot-definition-memory-mapped eslot)
            (setf (slot-definition-mmap-pointer-p eslot) 
              (loop
                for dslot in dslotds
                always (or (eq 'mptr (slot-definition-type dslot))
                         (eq 'mm-box (slot-definition-mm-type dslot)))))
            (let ((type (slot-definition-type eslot)))
              (with-slots (len) class
                (setf (slot-value eslot 'offset) len)
                (incf len (stored-type-size type)))))
          eslot)))))


(defmethod slot-value-using-class ((class mm-metaclass) object
                                    (slotd mm-effective-slot-definition))
  (declare (ignorable class))
  (funcall (the mm-slot-definition-reader (slot-definition-reader-function slotd))
    object))


(defmethod (setf slot-value-using-class) (new-value (class mm-metaclass) object
                                           (slotd mm-effective-slot-definition))
  (declare (ignorable class))
  (funcall (the mm-slot-definition-writer (slot-definition-writer-function slotd))
    new-value object))


(defmethod slot-boundp-using-class ((class mm-metaclass) object
                                     (slotd mm-effective-slot-definition))
  (declare (ignorable class object slotd))
  t)


(defmethod slot-makunbound-using-class ((class mm-metaclass) object
                                         (slotd mm-effective-slot-definition))
  (declare (ignorable class object slotd))
  (error "Memory mapped slots cannot be unbound."))


