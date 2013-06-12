;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

(defvar *persistent-class-index* nil)

(defun persistent-class-index ()
  (or *persistent-class-index*
    (setf *persistent-class-index* (find "persistent-class-index"
                                     (mm:retrieve-all-instances 'special-persistent-ctrie)
                          :key #'ctrie-name))
    (setf *persistent-class-index* (make-instance 'special-persistent-ctrie
                                     :name  "persistent-class-index" :test 'eq))))

(defclass ctrie-class (standard-class)
  ((instances :accessor instances-of)))

(defclass transient-instance-index (transient-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(transient)))

(defmethod slot-unbound (class (object ctrie-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (make-instance 'transient-instance-index)))

(defmethod ctrie-next-id ((thing ctrie-class))
  (or (ctrie-put-update-if (instances-of thing) 0 1 '+ 'numberp)
      (ctrie-put-ensure (instances-of thing) 0 1)))

(defclass persistent-class (standard-class)
  ((instances :accessor instances-of)))


(defmethod ctrie-next-id ((thing persistent-class))
  (or (ctrie-put-update-if (instances-of thing) 0 1 '+ 'numberp)
    (ctrie-put-ensure (instances-of thing) 0 1)))

(deflayer instance-index)

(mm:defmmclass persistent-instance-index (special-persistent-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(persistent)))


;;(define-layered-method compute-ctrie-classname :in-layer instance-index ()
;;  'persistent-instance-index)

(defmethod slot-unbound (class (object persistent-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (or (ctrie-get (persistent-class-index) (class-name object)) ;(class-of object)))
      (setf (instances-of object)
        (setf (ctrie-get (persistent-class-index) (class-name object)) ;(class-of object)))
          (with-active-layers (instance-index)
            (make-instance 'persistent-instance-index
              :name (fully-qualified-symbol-name (class-name object)) ; (class-of object)))
              :hash 'identity :test 'eql
              :context '(persistent))))))))

;;          (make-instance 'persistent-instance-index :name (class-name (class-of object))))))))

(defclass ctrie-object (standard-object)
  ((slots :reader slots-of)  
    (id :reader id-of)))
 
(defmethod validate-superclass ((class ctrie-class) (superclass standard-class))
  t)

(defclass persistent-object (standard-object)
  ((slots :reader slots-of)  
    (id :reader id-of)))

(defmethod validate-superclass ((class persistent-class) (superclass standard-class))
  t)

(defmethod initialize-instance :around ((class ctrie-class) &rest initargs
                                         &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
        thereis (subtypep class (find-class 'ctrie-object)))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'ctrie-object)))
      initargs)))

(defmethod initialize-instance :around ((class persistent-class) &rest initargs
                                         &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
        thereis (subtypep class (find-class 'persistent-object)))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'persistent-object)))
      initargs)))

(defmethod reinitialize-instance :around ((class ctrie-class) &rest initargs
                                           &key (direct-superclasses '()
                                                  direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
        (loop for class in direct-superclasses
          thereis (subtypep class (find-class 'ctrie-object))))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'ctrie-object)))
      initargs)))

(defmethod reinitialize-instance :around ((class persistent-class) &rest initargs
                                           &key (direct-superclasses '()
                                                  direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
        (loop for class in direct-superclasses
          thereis (subtypep class (find-class 'persistent-object))))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'persistent-object)))
      initargs)))


(defclass ctrie-direct-slot-definition (standard-direct-slot-definition)
  ()
  (:default-initargs :allocation :ctrie))
 
(defmethod direct-slot-definition-class ((class ctrie-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'ctrie-direct-slot-definition))


(defclass persistent-direct-slot-definition (standard-direct-slot-definition)
  ()
  (:default-initargs :allocation :persistent))
 
(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-direct-slot-definition))

(defclass ctrie-effective-slot-definition (standard-effective-slot-definition)
  ())

(defclass persistent-effective-slot-definition (standard-effective-slot-definition)
  ())

(defvar *effective-slot-definition-class*)
 
(defmethod compute-effective-slot-definition ((class ctrie-class) (name t)
                                               direct-slot-definitions)
  (let ((*effective-slot-definition-class*
          (if (eq (slot-definition-allocation (first direct-slot-definitions)) :ctrie)
            (find-class 'ctrie-effective-slot-definition)
            (find-class 'standard-effective-slot-definition))))
    (call-next-method)))
 
(defmethod effective-slot-definition-class ((class ctrie-class) &rest initargs)
  (declare (ignore initargs))
  *effective-slot-definition-class*)

(defmethod compute-effective-slot-definition ((class persistent-class) (name t)
                                               direct-slot-definitions)
  (let ((*effective-slot-definition-class*
          (if (eq (slot-definition-allocation (first direct-slot-definitions)) :persistent)
            (find-class 'persistent-effective-slot-definition)
            (find-class 'standard-effective-slot-definition))))
    (call-next-method)))
 
(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  *effective-slot-definition-class*)


(defmethod shared-initialize :before ((object ctrie-object) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp object 'id)
    (setf (slot-value object 'id) (ctrie-next-id (class-of object))))
  (unless (slot-boundp object 'slots)
    (setf (slot-value object 'slots)
      (or (ctrie-get (instances-of (class-of object)) (id-of object))
        (with-active-layers (transient)
          (ctrie-put (instances-of (class-of object)) (id-of object)
            (make-instance 'transient-ctrie :test 'eq :context '(transient))))))))

(defmethod shared-initialize :before ((object persistent-object) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp object 'id)
    (setf (slot-value object 'id) (ctrie-next-id (class-of object))))
  (unless (slot-boundp object 'slots)
    (setf (slot-value object 'slots)
      (or (ctrie-get (instances-of (class-of object)) (id-of object))
        (ctrie-put (instances-of (class-of object)) (id-of object)
          (with-active-layers (persistent)
            (make-instance 'persistent-ctrie :name
               (format nil "<~A/~D>" (class-name (class-of object))
              (slot-value object 'id))
            :test 'eq :context '(persistent))))))))
            
(defmethod slot-value-using-class ((class ctrie-class) object
                                    (slot ctrie-effective-slot-definition))
  (multiple-value-bind (value present-p)
      (ctrie-get (slots-of object) (slot-definition-name slot))
    (if present-p value
      (slot-unbound class object (slot-definition-name slot)))))
 
(defmethod (setf slot-value-using-class) (value (class ctrie-class) object
                                           (slot ctrie-effective-slot-definition))
  (setf (ctrie-get (slots-of object) (slot-definition-name slot)) value))

            
(defmethod slot-value-using-class ((class persistent-class) object
                                    (slot persistent-effective-slot-definition))
  (multiple-value-bind (value present-p)
      (ctrie-get (slots-of object) (slot-definition-name slot))
    (if present-p value
        (slot-unbound class object (slot-definition-name slot)))))
 
(defmethod (setf slot-value-using-class) (value (class persistent-class) object
                                           (slot persistent-effective-slot-definition))
  (setf (ctrie-get (slots-of object) (slot-definition-name slot)) value))


(defmethod slot-boundp-using-class ((class ctrie-class) object
                                     (slot ctrie-effective-slot-definition))
  (nth-value 1 (ctrie-get (slots-of object) (slot-definition-name slot))))

(defmethod slot-makunbound-using-class ((class ctrie-class) object
                                         (slot ctrie-effective-slot-definition))
  (ctrie-drop (slots-of object) (slot-definition-name slot)))


(defmethod slot-boundp-using-class ((class persistent-class) object
                                     (slot persistent-effective-slot-definition))
  (nth-value 1 (ctrie-get (slots-of object) (slot-definition-name slot))))


(defmethod slot-makunbound-using-class ((class persistent-class) object
                                         (slot persistent-effective-slot-definition))
  (ctrie-drop (slots-of object) (slot-definition-name slot)))


(defmethod print-object ((self persistent-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (persistent)" (id-of self))))


(defmethod print-object ((self ctrie-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D" (id-of self))))


(defun all-persistent-classes ()
  (ctrie-keys (persistent-class-index)))

;; TODO: ok the following obeys the normal semantics of eq being more
;; stringent than equal, but might be downright confusing to users.
;; check how other libs handle equality test in presence of transient
;; slots

(defgeneric persistent-object-eq (object1 object2))

(defmethod persistent-object-eq ((object1 t) (object2 t))
  (eq object1 object2))

(defmethod persistent-object-eq ((object1 persistent-object) (object2 persistent-object))
  (and
    (eq (class-of object1) (class-of object2))
    (eql (id-of object1) (id-of object2))
    (loop for slot in (class-slots (class-of object1))
      when (eq (slot-definition-allocation slot) :instance)
      unless (equal
               (slot-value object1 (slot-definition-name slot))
               (slot-value object2 (slot-definition-name slot)))
      return nil
      finally (return t))))
               
(defgeneric persistent-object-equal (object1 object2))

(defmethod persistent-object-equal ((object1 t) (object2 t))
  (equal object1 object2))

(defmethod persistent-object-equal ((object1 persistent-object) (object2 persistent-object))
  (and
    (eq (class-of object1) (class-of object2))
    (eql (id-of object1) (id-of object2))))

