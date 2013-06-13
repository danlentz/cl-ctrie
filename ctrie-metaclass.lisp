;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


(defvar *effective-slot-definition-class*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computed/Cacheable Metaobjects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cacheable-slot-definition (standard-slot-definition)
  ((cache-p :accessor slot-definition-cache-p :initarg :cache :initform nil)
    (cached-value :accessor slot-definition-cached-value :initarg :cached-value)))

(defclass computed-slot-definition (cacheable-slot-definition)
  ((slot-value-function
     :initarg :slot-value-function
     :accessor slot-definition-slot-value-function
     :initform (constantly nil))
    (setf-slot-value-function
      :initarg :setf-slot-value-function
      :accessor slot-definition-setf-slot-value-function
      :initform (constantly nil)))
  (:default-initargs :allocation :computed))

(defclass computed-direct-slot-definition (standard-direct-slot-definition
                                            computed-slot-definition)
  ())

(defclass computed-effective-slot-definition (standard-effective-slot-definition
                                               computed-slot-definition)
  ())

(defun slot-definition-computed-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :computed))


(defmethod shared-initialize :before ((object transactional-object) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp object 'id)
    (setf (slot-value object 'id) (ctrie-next-id (class-of object))))
  (unless (slot-boundp object 'slots)
    (setf (slot-value object 'slots)
      (or (ctrie-get (instances-of (class-of object)) (id-of object))
        (with-active-layers (transient)
          (ctrie-put (instances-of (class-of object)) (id-of object)
            (make-instance 'transient-ctrie :test 'eq :context '(transient))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactional-Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transactional-class (standard-class)
  ((instances :accessor instances-of)))

(defclass transient-instance-index (transient-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(transient)))

(defmethod slot-unbound (class (object transactional-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (make-instance 'transient-instance-index)))

(defmethod ctrie-next-id ((thing transactional-class))
  (or (ctrie-put-update-if (instances-of thing) 0 1 '+ 'numberp)
      (ctrie-put-ensure (instances-of thing) 0 1)))

(defclass transactional-object (standard-object)
  ((slots :reader slots-of :initarg :slots)  
    (id :reader id-of :initarg :id)))
 
(defmethod validate-superclass ((class transactional-class) (superclass standard-class))
  t)

(defmethod initialize-instance :around ((class transactional-class) &rest initargs
                                         &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
        thereis (subtypep class (find-class 'transactional-object)))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'transactional-object)))
      initargs)))

(defmethod reinitialize-instance :around ((class transactional-class) &rest initargs
                                           &key (direct-superclasses '()
                                                  direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
        (loop for class in direct-superclasses
          thereis (subtypep class (find-class 'transactional-object))))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'transactional-object)))
      initargs)))

(defclass transactional-direct-slot-definition (standard-direct-slot-definition)
  ()
  (:default-initargs :allocation :transactional))
 
(defmethod direct-slot-definition-class ((class transactional-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'transactional-direct-slot-definition))

(defclass transactional-effective-slot-definition (standard-effective-slot-definition)
  ())

 
(defmethod compute-effective-slot-definition ((class transactional-class) (name t)
                                               direct-slot-definitions)
  (let ((*effective-slot-definition-class*
          (if (eq (slot-definition-allocation (first direct-slot-definitions)) :ctrie)
            (find-class 'transactional-effective-slot-definition)
            (find-class 'standard-effective-slot-definition))))
    (call-next-method)))
 
(defmethod effective-slot-definition-class ((class transactional-class) &rest initargs)
  (declare (ignore initargs))
  *effective-slot-definition-class*)

            
(defmethod slot-value-using-class ((class transactional-class) object
                                    (slot transactional-effective-slot-definition))
  (multiple-value-bind (value present-p)
      (ctrie-get (slots-of object) (slot-definition-name slot))
    (if present-p value
      (slot-unbound class object (slot-definition-name slot)))))
 
(defmethod (setf slot-value-using-class) (value (class transactional-class) object
                                           (slot transactional-effective-slot-definition))
  (setf (ctrie-get (slots-of object) (slot-definition-name slot)) value))


(defmethod slot-boundp-using-class ((class transactional-class) object
                                     (slot transactional-effective-slot-definition))
  (nth-value 1 (ctrie-get (slots-of object) (slot-definition-name slot))))

(defmethod slot-makunbound-using-class ((class transactional-class) object
                                         (slot transactional-effective-slot-definition))
  (ctrie-drop (slots-of object) (slot-definition-name slot)))

(defmethod print-object ((self transactional-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (transactional)" (id-of self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent-Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *persistent-class-index* nil)

(defun persistent-class-index ()
  (or *persistent-class-index*
    (setf *persistent-class-index* (find "persistent-class-index"
                                     (mm:retrieve-all-instances 'special-persistent-ctrie)
                          :key #'ctrie-name))
    (setf *persistent-class-index* (make-instance 'special-persistent-ctrie
                                     :name  "persistent-class-index" :test 'eq))))

(defclass persistent-class (standard-class)
  ((instances :accessor instances-of)))

(defmethod ctrie-next-id ((thing persistent-class))
  (or (ctrie-put-update-if (instances-of thing) 0 1 '+ 'numberp)
    (ctrie-put-ensure (instances-of thing) 0 1)))

(deflayer instance-index)

(mm:defmmclass persistent-instance-index (special-persistent-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(persistent)))

(defmethod slot-unbound (class (object persistent-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (or (ctrie-get (persistent-class-index) (class-name object))
      (setf (instances-of object)
        (setf (ctrie-get (persistent-class-index) (class-name object)) 
          (with-active-layers (instance-index)
            (make-instance 'persistent-instance-index
              :name (fully-qualified-symbol-name (class-name object)) 
              :hash 'identity :test 'eql
              :context '(persistent))))))))

(defclass persistent-object (standard-object)
  ((slots :reader slots-of :initarg :slots)  
    (id :reader id-of :initarg :id)))

(defmethod validate-superclass ((class persistent-class) (superclass standard-class))
  t)

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

(defclass persistent-direct-slot-definition (standard-direct-slot-definition)
  ()
  (:default-initargs :allocation :persistent))
 
(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:persistent  (find-class 'persistent-direct-slot-definition))
    (:computed    (find-class 'computed-direct-slot-definition))
    (t  (call-next-method))))

(defun slot-definition-persistent-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :persistent))


(defclass persistent-effective-slot-definition (standard-effective-slot-definition)
  ())

(defmethod compute-effective-slot-definition ((class persistent-class) (name t)
                                               direct-slot-definitions)
  (aprog1 (call-next-method)
    (dolist (slot direct-slot-definitions)
      (typecase slot
        (computed-slot-definition (setf
                                    (slot-definition-slot-value-function it)
                                    (slot-definition-slot-value-function slot)
                                    (slot-definition-setf-slot-value-function it)
                                    (slot-definition-setf-slot-value-function slot)
                                    (slot-definition-cache-p it)
                                    (slot-definition-cache-p slot))
          (return))))))
          
(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:persistent  (find-class 'persistent-effective-slot-definition))
    (:computed    (find-class 'computed-effective-slot-definition))
    (t   (call-next-method))))

(mm:defmmclass persistent-transactional-object (persistent-ctrie)
  ())

(defvar *persistent-object-cells* (make-instance 'transient-ctrie))

(defgeneric get-cell (object))

(defmethod get-cell ((object persistent-object))
  (ctrie-put-ensure (ctrie-put-ensure *persistent-object-cells*
                      (class-name (class-of object)) (make-instance 'transient-ctrie))
    (id-of object) (dstm:create-var
                     (or (ctrie-get (instances-of (class-of object)) (id-of object))
                       (ctrie-put (instances-of (class-of object)) (id-of object)
                         (with-active-layers (persistent)
                           (make-instance 'persistent-transactional-object :name
                             (format nil "<~A/~D>" (class-name (class-of object))
                               (slot-value object 'id)) :test 'eq :context '(persistent))))))))

(defmethod get-cell ((object dstm:var))
  object)

(defmethod shared-initialize :before ((object persistent-object) slot-names &rest initargs)
  (declare (ignore slot-names))
  (if (getf initargs :id)
    (setf (slot-value object 'id) (getf initargs :id))
    (unless (slot-boundp object 'id)
      (setf (slot-value object 'id) (ctrie-next-id (class-of object)))))
  (if (getf initargs :slots)
    (setf (slot-value object 'slots) (getf initargs :slots))
    (unless (slot-boundp object 'slots)
      (setf (slot-value object 'slots) (get-cell object)))))

(defmethod slot-value-using-class ((class persistent-class) object
                                    (slot computed-effective-slot-definition))
  (cond
    ((and (slot-definition-cache-p slot) (slot-boundp slot 'cached-value))
      (slot-definition-cached-value slot))
    ((slot-definition-cache-p slot)
      (setf (slot-definition-cached-value slot)
        (funcall (slot-definition-slot-value-function slot) object)))
    (t
      (funcall (slot-definition-slot-value-function slot) object))))

(defmethod (setf slot-value-using-class) (value (class persistent-class) object
                                           (slot computed-effective-slot-definition))
  (cond
    ((slot-definition-cache-p slot)
      (setf (slot-definition-cached-value slot)
        (funcall (slot-definition-setf-slot-value-function slot) value object)))
    (t
      (funcall (slot-definition-setf-slot-value-function slot) value object))))

(defun recompute-slot (object slot-name)
  (assert (typep (find slot-name
                   (class-slots (class-of object)) :key #'slot-definition-name)
            'computed-effective-slot-definition))
  (prog1 object
    (slot-makunbound (find slot-name
                       (class-slots (class-of object)) :key #'slot-definition-name)
      'cached-value)))
    
(defmethod slot-value-using-class ((class persistent-class) object
                                    (slot persistent-effective-slot-definition))
  (dstm:atomic 
    (multiple-value-bind (value present-p)
      (ctrie-get (dstm:read-var (slots-of object)) (slot-definition-name slot))
      (if present-p value
        (slot-unbound class object (slot-definition-name slot))))))
 
(defmethod (setf slot-value-using-class) (value (class persistent-class) object
                                           (slot persistent-effective-slot-definition))
  (prog1 value
    (dstm:atomic
      (dstm:rmw (slots (slots-of object))
        (aprog1 (ctrie-fork slots)
          (ctrie-put it (slot-definition-name slot) value))))))
 
(defmethod slot-boundp-using-class ((class persistent-class) object
                                     (slot persistent-effective-slot-definition))
  (nth-value 1 (ctrie-get (dstm:read-var (slots-of object)) (slot-definition-name slot))))


(defmethod slot-boundp-using-class ((class persistent-class) object
                                     (slot computed-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class persistent-class) object
                                         (slot persistent-effective-slot-definition))
  (prog1 nil
    (dstm:atomic
      (dstm:rmw (slots (slots-of object))
        (aprog1 (ctrie-fork slots)
          (ctrie-drop it (slot-definition-name slot)))))))

(defmethod slot-makunbound-using-class ((class persistent-class) object
                                         (slot computed-effective-slot-definition))
  t)

(defmethod print-object ((self persistent-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (persistent)" (id-of self))))

(defun all-persistent-classes ()
  (ctrie-keys (persistent-class-index)))

(defgeneric persistent-object-eq (object1 object2))

(defmethod persistent-object-eq ((object1 t) (object2 t))
  (eq object1 object2))

(defmethod persistent-object-eq ((object1 persistent-object) (object2 persistent-object))
  (and
    (eq (class-of object1) (class-of object2))
    (eql (id-of object1) (id-of object2))))

(mm:defmmclass persistent-proxy ()
  ((class-name :initarg :class-name :accessor class-name-of :type symbol)
    (instance-id :initarg :instance-id :accessor instance-id-of :type integer)))

(defun find-persistent-object (class-name id)
  (let1 instances (ctrie-get (persistent-class-index) class-name)
    (when instances
      (let1 object (ctrie-get instances id)
        (when object
          (funcall #'make-instance class-name :id id ))))))
            
(define-layered-method maybe-box :in persistent ((object persistent-object))
  (make-instance 'persistent-proxy
    :class-name (class-name (class-of object))
    :instance-id (id-of object)))

(define-layered-method maybe-unbox :in persistent ((object persistent-proxy))
  (funcall #'make-instance (class-name-of object) :id (instance-id-of object))) 

(defun persistent-objects-of-class (class-name &optional include-subclasses)
  (flet ((get-instances (class-name)
           (let1 instances (ctrie-get (persistent-class-index) class-name)
             (when instances
               (mapcar (lambda (id) (unless (zerop id) (funcall #'make-instance class-name :id id)))
                 (reverse (ctrie-keys instances)))))))
    (let1 classes (if include-subclasses
                    (mapcar #'class-name (object:class-subclasses class-name))
                    (list class-name))
      (remove-if #'null (loop for class in classes appending (get-instances class))))))

(defmacro define-persistent-class (name supers slots &rest options)
  (let* ((new-slots nil)
          (options (remove :metaclass options :key #'first))
          (options (push '(:metaclass persistent-class) options)))
    (loop
      for raw-slot in slots
      for slot = (ensure-list raw-slot)
      for class =  (eq (getf (rest slot) :allocation) :class)
      for computed =  (or (eq (getf (rest slot) :allocation) :computed)
                        (getf (rest slot) :slot-value-function)
                        (getf (rest slot) :compute-as))
      for transient = (or (getf (rest slot) :transient)
                        (eq (getf (rest slot) :allocation) :instance))
      do (cond
           (class (push (list* (car slot)
                          (append (remove-from-plist (rest slot)
                                    :compute-as :allocation :transient :persistent)
                            '(:allocation :class)))
                    new-slots))
           (computed (let* ((svf (or (getf (rest slot) :slot-value-function)
                                   (awhen (getf (rest slot) :compute-as)
                                     (eval `(lambda (-self-) (declare (ignorable -self-)) ,it)))
                                   (constantly nil)))
                             (ssvf (or (getf (rest slot) :setf-slot-value-function)
                                     (awhen (getf (rest slot) :update-as)
                                       (eval `(lambda (-value- -self-)
                                                (declare (ignorable -value- -self-)) ,it)))
                                     (constantly nil))))
                       (push (list* (car slot)
                               (append (remove-from-plist (rest slot) :compute-as :update-as
                                         :slot-value-function :setf-slot-value-function
                                         :allocation :transient :persistent)
                                 (list :allocation :computed :slot-value-function (compile nil svf)
                                   :setf-slot-value-function (compile nil ssvf))))
                         new-slots)))
           ((not transient)
             (push (list* (car slot)
                     (append (remove-from-plist (rest slot)
                               :allocation :transient :persistent)
                       '(:allocation :persistent)))
               new-slots))
           (t (push  (list* (car slot)
                       (append (remove-from-plist (rest slot)
                                 :allocation :transient :persistent)
                         '(:allocation :instance)))
                new-slots)))
      finally (setf new-slots (nreverse new-slots)))
    `(defclass ,name ,supers ,new-slots ,@options)))


;;; Form: (DEFINE-PERSISTENT-CLASS P6 (P5)
;;                                    ((D :COMPUTE-AS (LENGTH *FEATURES*))
;;                                     (E :SLOT-VALUE-FUNCTION
;;                                      (LAMBDA (X)
;;                                        (DECLARE (IGNORE X))
;;                                        (GET-INTERNAL-REAL-TIME)))
;;                                     (F :SLOT-VALUE-FUNCTION
;;                                      (LAMBDA (X)
;;                                        (DECLARE (IGNORE X))
;;                                        (GET-INTERNAL-REAL-TIME))
;;                                      :CACHE T)
;;                                     (G :ALLOCATION :COMPUTED)
;;                                     (W :INITARG :W :INITFORM
;;                                      (GET-UNIVERSAL-TIME) :TRANSIENT NIL
;;                                      :ACCESSOR W-OF)
;;                                     V (U :TRANSIENT T :INITFORM 0)
;;                                     (C :ALLOCATION :CLASS :INITFORM 44)
;;                                     (S :INITARG :S :INITFORM 5)
;;                                     (M :ALLOCATION :INSTANCE)
;;                                     (N :ALLOCATION :PERSISTENT)))
;;; First step of expansion:
;;
;; (DEFCLASS P6 (P5)
;;           ((D :ALLOCATION :COMPUTED :SLOT-VALUE-FUNCTION #
;;             :SETF-SLOT-VALUE-FUNCTION #)
;;            (E :ALLOCATION :COMPUTED :SLOT-VALUE-FUNCTION
;;             (LAMBDA (X) (DECLARE (IGNORE X)) (GET-INTERNAL-REAL-TIME))
;;             :SETF-SLOT-VALUE-FUNCTION #)
;;            (F :CACHE T :ALLOCATION :COMPUTED :SLOT-VALUE-FUNCTION
;;             (LAMBDA (X) (DECLARE (IGNORE X)) (GET-INTERNAL-REAL-TIME))
;;             :SETF-SLOT-VALUE-FUNCTION #)
;;            (G :ALLOCATION :COMPUTED :SLOT-VALUE-FUNCTION #
;;             :SETF-SLOT-VALUE-FUNCTION #)
;;            (W :INITARG :W :INITFORM (GET-UNIVERSAL-TIME) :ACCESSOR W-OF
;;             :ALLOCATION :PERSISTENT)
;;            (V :ALLOCATION :PERSISTENT) (U :INITFORM 0 :ALLOCATION :INSTANCE)
;;            (C :INITFORM 44 :ALLOCATION :CLASS)
;;            (S :INITARG :S :INITFORM 5 :ALLOCATION :PERSISTENT)
;;            (M :ALLOCATION :INSTANCE) (N :ALLOCATION :PERSISTENT))
;;   (:METACLASS PERSISTENT-CLASS))

