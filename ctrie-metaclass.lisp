;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


(defvar *effective-slot-definition-class*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computed/Cacheable Metaobjects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        
(defclass cacheable-slot-definition (standard-slot-definition)
  ((cache-p
     :accessor slot-definition-cache-p
     :initarg :cache
     :initform nil)
    (cached-value
      :accessor slot-definition-cached-value
      :initarg :cached-value)))

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


(defclass derived-slot-definition (computed-slot-definition)
  ((derived-variable
     :initarg :variable
     :accessor slot-definition-derived-variable
     :initform (screamer:make-variable)))
  (:default-initargs :allocation :derived
    :cache t
    :slot-value-function '?:slot-valuev
    :setf-slot-value-function '?:make-equal))

(defclass derived-direct-slot-definition (standard-direct-slot-definition
                                            derived-slot-definition)
  ())

(defclass derived-effective-slot-definition (standard-effective-slot-definition
                                               derived-slot-definition)
  ())

(defun slot-definition-derived-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :derived))


(defun recompute-slot (object slot-name)
  (assert (typep (find slot-name (class-slots (class-of object))
                   :key #'slot-definition-name)
            'computed-effective-slot-definition))
  (prog1 object
    (slot-makunbound (find slot-name (class-slots (class-of object))
                       :key #'slot-definition-name)
      'cached-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental-Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass fundamental-class (standard-class) ; (index:indexed-class)
  ((instances :accessor instances-of)))

(defmethod initialize-instance :after ((class fundamental-class) &key)
  (setf (instances-of class)
    (or (ctrie-get (fundamental-class-index) (class-name class))
      (setf (ctrie-get (fundamental-class-index) (class-name class)) 
        (make-ctrie nil 
          :name (fully-qualified-symbol-name (class-name class)) 
          :hash 'identity :test 'eql
          :context '(fundamental))))))

  
(defclass fundamental-object (standard-object)
  ((slots :reader slots-of :initarg :slots :initform nil)  
    (id :reader id-of :initarg :id)))

(defvar *fundamental-class-index* nil)

(defun fundamental-class-index ()
  (or *fundamental-class-index*
    (setf *fundamental-class-index*
      (make-instance 'fundamental-ctrie))))

(defmethod ctrie-next-id ((thing fundamental-class))
  (index-incf (instances-of thing)))

(defun %fetch-slot-ctrie (object)
  (ctrie-get (instances-of (class-of object))  (id-of object)))

(defclass fundamental-instance-index (fundamental-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(fundamental))
  (:metaclass structure-class))

(deflayer instance-index)

(defmethod slot-unbound (class (object fundamental-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (or (ctrie-get (fundamental-class-index) (class-name object))
      (setf (instances-of object)
        (setf (ctrie-get (fundamental-class-index) (class-name object)) 
          (make-ctrie nil 
            :name (fully-qualified-symbol-name (class-name object)) 
            :hash 'identity :test 'eql
            :context '(fundamental)))))))


(defmethod validate-superclass ((class fundamental-class) (superclass standard-class))
  t)

(defmethod initialize-instance :around ((class fundamental-class) &rest initargs
                                         &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
        thereis (subtypep class (find-class 'fundamental-object)))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'fundamental-object)))
      initargs)))

(defmethod reinitialize-instance :around ((class fundamental-class) &rest initargs
                                           &key (direct-superclasses '()
                                                  direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
        (loop for class in direct-superclasses
          thereis (subtypep class (find-class 'fundamental-object))))
    (call-next-method)
    (apply #'call-next-method class
      :direct-superclasses (append direct-superclasses
                             (list (find-class 'fundamental-object)))
      initargs)))

(defclass fundamental-direct-slot-definition (standard-direct-slot-definition)
                                               ;index:index-direct-slot-definition)
  ()
  (:default-initargs :allocation :fundamental))
 
(defmethod direct-slot-definition-class ((class fundamental-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:fundamental    (find-class 'fundamental-direct-slot-definition))
    (:derived        (find-class 'derived-direct-slot-definition))
    (:computed       (find-class 'computed-direct-slot-definition))
    (t               (call-next-method))))

(defun slot-definition-fundamental-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :fundamental))

(defclass fundamental-effective-slot-definition (standard-effective-slot-definition)
                                                  ;index:index-effective-slot-definition)
  ())
 
(defmethod compute-effective-slot-definition ((class fundamental-class) (name t)
                                               direct-slot-definitions)
  (aprog1 (call-next-method)
    (dolist (slot direct-slot-definitions)
      (typecase slot
        (derived-slot-definition (setf
                                   (slot-definition-derived-variable it)
                                   (slot-definition-derived-variable slot)
                                    (slot-definition-slot-value-function it)
                                    (slot-definition-slot-value-function slot)
                                    (slot-definition-setf-slot-value-function it)
                                    (slot-definition-setf-slot-value-function slot)
                                    (slot-definition-cache-p it)
                                    (slot-definition-cache-p slot))
          (return))
        (computed-slot-definition (setf
                                    (slot-definition-slot-value-function it)
                                    (slot-definition-slot-value-function slot)
                                    (slot-definition-setf-slot-value-function it)
                                    (slot-definition-setf-slot-value-function slot)
                                    (slot-definition-cache-p it)
                                    (slot-definition-cache-p slot))
          (return))))))

(defmethod effective-slot-definition-class ((class fundamental-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:fundamental    (find-class 'fundamental-effective-slot-definition))
    (:derived        (find-class 'derived-effective-slot-definition))
    (:computed       (find-class 'computed-effective-slot-definition))
    (t               (call-next-method))))

(defclass fundamental-ctrie-object (fundamental-ctrie)
  ()
  (:metaclass structure-class))

(defvar *fundamental-object-cells* (make-instance 'fundamental-ctrie))

;; (defgeneric get-cell (object))

;; (defmethod get-cell ((object fundamental-object))
;;   ;; (ctrie-put-ensure (ctrie-put-ensure *fundamental-object-cells*
;;   ;;                     (class-name (class-of object)) (make-instance 'fundamental-ctrie))
;;   ;;   (id-of object)
;;     (or (ctrie-get (instances-of (class-of object)) (id-of object))
;;                      (ctrie-put (instances-of (class-of object)) (id-of object)
;;                        (with-active-layers (fundamental)
;;                          (make-instance 'fundamental-ctrie-object
;;                            :name (format nil "<~A/~D>" (class-name (class-of object))
;;                                      (slot-value object 'id))
;;                            :test 'eq
;;                            :context '(fundamental))))))

(defmethod shared-initialize :before ((object fundamental-object) slot-names &rest initargs)
  (declare (ignore slot-names))
  (if (getf initargs :id)
    (setf (slot-value object 'id) (getf initargs :id))
    (unless (slot-boundp object 'id)
      (setf (slot-value object 'id) (ctrie-next-id (class-of object)))))
  (if (getf initargs :slots)
    (setf (slot-value object 'slots) (getf initargs :slots))
    (setf (slot-value object 'slots)
      (or (ctrie-get (instances-of (class-of object)) (id-of object))
        (aprog1 (with-active-layers (fundamental)
                  (make-fundamental-ctrie
                    :name (format nil "<~A/~D>" (class-name (class-of object))
                            (slot-value object 'id))
                    :test 'eq
                    :context '(fundamental)))
          (setf (slot-value object 'slots) it)
          (ctrie-put (instances-of (class-of object)) (id-of object) it))))))
    



;;           (get-cell object))))))

;; (defmethod slot-value-using-class ((class fundamental-class) object
;;                                     (slot computed-effective-slot-definition))
;;   (cond
;;     ((and (slot-definition-cache-p slot) (slot-boundp slot 'cached-value))
;;       (slot-definition-cached-value slot))
;;     ((slot-definition-cache-p slot)
;;       (setf (slot-definition-cached-value slot)
;;         (funcall (slot-definition-slot-value-function slot) object)))
;;     (t
;;       (funcall (slot-definition-slot-value-function slot) object))))

#+()
(defmethod slot-value-using-class ((class fundamental-class) object
                                    (slot derived-effective-slot-definition))
  (cond
    ((and (slot-definition-cache-p slot) (slot-boundp slot 'cached-value))
      (slot-definition-cached-value slot))
    ((slot-definition-cache-p slot)
      (setf (slot-definition-cached-value slot)
        (funcall (slot-definition-slot-value-function slot) object)))
    (t
      (funcall (slot-definition-slot-value-function slot) object))))

;; (defmethod (setf slot-value-using-class) (value (class fundamental-class) object
;;                                            (slot computed-effective-slot-definition))
;;   (cond
;;     ((slot-definition-cache-p slot)
;;       (setf (slot-definition-cached-value slot)
;;         (funcall (slot-definition-setf-slot-value-function slot) value object)))
;;     (t
;;       (funcall (slot-definition-setf-slot-value-function slot) value object))))

(defmethod slot-value-using-class ((class fundamental-class) object
                                    (slot fundamental-effective-slot-definition))
  (multiple-value-bind (value present-p)
    (ctrie-get (slots-of object) (slot-definition-name slot))
    (if present-p value
      (slot-unbound class object (slot-definition-name slot)))))
 
(defmethod (setf slot-value-using-class) (value (class fundamental-class) object
                                           (slot fundamental-effective-slot-definition))
  (prog1 value
    (ctrie-put (slots-of object) (slot-definition-name slot) value)))

(defmethod slot-boundp-using-class ((class fundamental-class) object
                                     (slot fundamental-effective-slot-definition))
  (nth-value 1 (ctrie-get (slots-of object) (slot-definition-name slot))))


(defmethod slot-makunbound-using-class ((class fundamental-class) object
                                         (slot fundamental-effective-slot-definition))
  (prog1 (call-next-method)
    (ctrie-drop (slots-of object) (slot-definition-name slot))))

(defmethod slot-boundp-using-class ((class fundamental-class) object
                                     (slot computed-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class fundamental-class) object
                                         (slot computed-effective-slot-definition))
  t)
#+()
(defmethod index::destroy-object-with-class :around ((class fundamental-class) object)
  (call-next-method)
  (ctrie-drop (instances-of (class-of object)) (id-of object))
  (slot-makunbound object 'id)
  (slot-makunbound object 'slots))
    
    
(defmethod print-object ((self fundamental-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (fundamental)" 0 #+()(id-of self))))

(defun all-fundamental-classes ()
  (ctrie-keys (fundamental-class-index)))

(defgeneric fundamental-object-eq (object1 object2))

(defmethod fundamental-object-eq ((object1 t) (object2 t))
  (eq object1 object2))

(defmethod fundamental-object-eq ((object1 fundamental-object)
                                     (object2 fundamental-object))
  (and
    (eq (class-of object1) (class-of object2))
    (eql (id-of object1) (id-of object2))))

(defclass fundamental-proxy ()
  ((class-name :initarg :class-name :accessor class-name-of :type symbol)
    (slots :initarg :slots :accessor slots-of)
    (instance-id :initarg :instance-id :accessor instance-id-of :type integer)))

(defun find-fundamental-object (class-name id)
  (let1 instances (ctrie-get (fundamental-class-index) class-name)
    (when instances
      (let1 object (ctrie-get instances id)
        (when object
          (funcall #'make-instance class-name :id id))))))
            
(define-layered-method maybe-box :in persistent ((object fundamental-object))
  (let ((class-name (class-of object))
         (id (id-of object)))
    (maybe-box
      (make-instance 'fundamental-proxy :class-name class-name :instance-id id
        :slots (ctrie-get (ctrie-get (fundamental-class-index) class-name) id)))))

(define-layered-method maybe-unbox :in persistent ((object fundamental-proxy))
  (funcall #'make-instance (class-name-of object)
    :id (instance-id-of object)
    :slots (slots-of object)))

(defun fundamental-objects-of-class (class-name &optional include-subclasses)
  (flet ((get-instances (class-name)
           (let1 instances (ctrie-get (fundamental-class-index) class-name)
             (when instances
               (mapcar (lambda (id) (unless (zerop id) (funcall #'make-instance class-name :id id)))
                 (reverse (ctrie-keys instances)))))))
    (let1 classes (if include-subclasses
                    (mapcar #'class-name (object:class-subclasses class-name))
                    (list class-name))
      (remove-if #'null (loop for class in classes appending (get-instances class))))))



(defmacro define-fundamental-class (name supers slots &rest options)
  (let* ((new-slots nil)
          (options (remove :metaclass options :key #'first))
          (options (push '(:metaclass fundamental-class) options)))
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
                                    :compute-as :allocation :transient)
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
                                         :allocation :transient)
                                 (list :allocation :computed
                                   :slot-value-function (compile nil svf)
                                   :setf-slot-value-function (compile nil ssvf))))
                         new-slots)))
           ((not transient)
             (push (list* (car slot)
                     (append (remove-from-plist (rest slot)
                               :allocation :transient)
                       '(:allocation :fundamental)))
               new-slots))
           (t (push  (list* (car slot)
                       (append (remove-from-plist (rest slot)
                                 :allocation :transient)
                         '(:allocation :instance)))
                new-slots)))
      finally (setf new-slots (nreverse new-slots)))
    `(defclass ,name ,supers ,new-slots ,@options)))


(define-fundamental-class  fu ()
  ((name    
     :initarg :name :initform (princ-to-string (random-elt *features*))
     :accessor thing-name :type string
     :documentation "Fullname of comment, e.g. t1_c3v7f8u")
   (kind    
    :initarg :kind :initform "xyz" :accessor thing-kind :type string
     :documentation "String identifier that denotes the object's type")
   (data    
     :initarg :data :initform (get-universal-time) :accessor thing-data
     :documentation "Custom data structure")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactional-Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transactional-class (index:indexed-class) ;;(standard-class)
  ((instances :accessor instances-of)))

(defclass transactional-object (standard-object)
  ((slots :reader slots-of :initarg :slots)  
    (id :reader id-of :initarg :id)))

(defvar *transactional-class-index* nil)

(defun transactional-class-index ()
  (or *transactional-class-index*
    (setf *transactional-class-index*
      (make-instance 'transient-ctrie :test #'eq))))

(defmethod ctrie-next-id ((thing transactional-class))
  (index-incf (instances-of thing)))


(defun %fetch-slot-ctrie (object)
  (ctrie-get (instances-of (class-of object))  (id-of object)))

;;  (or (ctrie-put-update-if (instances-of thing) 0 1 '+ 'numberp)
;;      (ctrie-put-ensure (instances-of thing) 0 1)))

(defclass transactional-instance-index (transient-ctrie)
  ()
  (:default-initargs :hash 'identity :test 'eql :context '(transient)))


(defmethod slot-unbound (class (object transactional-class) (slot-name (eql 'instances)))
  (setf (instances-of object)
    (or (ctrie-get (transactional-class-index) (class-name object))
      (setf (instances-of object)
        (setf (ctrie-get (transactional-class-index) (class-name object)) 
          (with-active-layers (instance-index)
            (make-instance 'transactional-instance-index
              :name (fully-qualified-symbol-name (class-name object)) 
              :hash 'identity :test 'eql
              :context '(transient))))))))


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

(defclass transactional-direct-slot-definition (index:index-direct-slot-definition)
  ()
  (:default-initargs :allocation :transactional))
 
(defmethod direct-slot-definition-class ((class transactional-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:transactional  (find-class 'transactional-direct-slot-definition))
    (:computed       (find-class 'computed-direct-slot-definition))
    (t               (call-next-method))))

(defun slot-definition-transactional-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :transactional))

(defclass transactional-effective-slot-definition (index:index-effective-slot-definition)
  ())
 
(defmethod compute-effective-slot-definition ((class transactional-class) (name t)
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

(defmethod effective-slot-definition-class ((class transactional-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:transactional  (find-class 'transactional-effective-slot-definition))
    (:computed       (find-class 'computed-effective-slot-definition))
    (t               (call-next-method))))

(defclass transient-transactional-object (transient-ctrie)
  ())

(defvar *transactional-object-cells* (make-instance 'transient-ctrie))

(defgeneric get-cell (object &optional slots))

(defmethod get-cell ((object vstm:var) &optional slots)
  (declare (ignore slots))
  object)

(defmethod get-cell ((object transactional-object)  &optional slots)
  (declare (ignore slots))
  (ctrie-put-ensure (ctrie-put-ensure *transactional-object-cells*
                      (class-name (class-of object)) (make-instance 'transient-ctrie))
    (id-of object) (vstm:create-var
                     (or (ctrie-get (instances-of (class-of object)) (id-of object))
                       (ctrie-put (instances-of (class-of object)) (id-of object)
                         (with-active-layers (transient)
                           (make-instance 'transient-transactional-object
                             :name (format nil "<~A/~D>" (class-name (class-of object))
                                     (slot-value object 'id)) :test 'eq
                             :context '(transient))))))))


(defmethod shared-initialize :before ((object transactional-object) slot-names &rest initargs)
  (declare (ignore slot-names))
  (if (getf initargs :id)
    (setf (slot-value object 'id) (getf initargs :id))
    (unless (slot-boundp object 'id)
      (setf (slot-value object 'id) (ctrie-next-id (class-of object)))))
  (if (getf initargs :slots)
    (setf (slot-value object 'slots) ;; note this is not currently used and may go away 
      (ctrie-put (ctrie-get *transactional-object-cells* (class-name (class-of object)))
        (id-of object) (vstm:create-var (ctrie-put (instances-of (class-of object))
                                          (id-of object)
                                          (vstm:read-var (getf initargs :slots))))))
    (unless (slot-boundp object 'slots)
      (setf (slot-value object 'slots) (get-cell object)))))

(defmethod slot-value-using-class ((class transactional-class) object
                                    (slot computed-effective-slot-definition))
  (cond
    ((and (slot-definition-cache-p slot) (slot-boundp slot 'cached-value))
      (slot-definition-cached-value slot))
    ((slot-definition-cache-p slot)
      (setf (slot-definition-cached-value slot)
        (funcall (slot-definition-slot-value-function slot) object)))
    (t
      (funcall (slot-definition-slot-value-function slot) object))))

(defmethod (setf slot-value-using-class) (value (class transactional-class) object
                                           (slot computed-effective-slot-definition))
  (cond
    ((slot-definition-cache-p slot)
      (setf (slot-definition-cached-value slot)
        (funcall (slot-definition-setf-slot-value-function slot) value object)))
    (t
      (funcall (slot-definition-setf-slot-value-function slot) value object))))

(defun recompute-slot (object slot-name)
  (assert (typep (find slot-name (class-slots (class-of object))
                   :key #'slot-definition-name)
            'computed-effective-slot-definition))
  (prog1 object
    (slot-makunbound (find slot-name (class-slots (class-of object))
                       :key #'slot-definition-name)
      'cached-value)))

(defmethod slot-value-using-class ((class transactional-class) object
                                    (slot transactional-effective-slot-definition))
  (vstm:atomic 
    (multiple-value-bind (value present-p)
      (ctrie-get (vstm:read-var (slots-of object)) (slot-definition-name slot))
      (if present-p value
        (slot-unbound class object (slot-definition-name slot))))))
 
(defmethod (setf slot-value-using-class) (value (class transactional-class) object
                                           (slot transactional-effective-slot-definition))
  (prog1 value
    (vstm:atomic
      (vstm:rmw (slots (slots-of object))
        (aprog1 (ctrie-fork slots)
          (ctrie-put it (slot-definition-name slot) value))))))

(defmethod slot-boundp-using-class ((class transactional-class) object
                                     (slot transactional-effective-slot-definition))
  (nth-value 1 (ctrie-get (vstm:read-var (slots-of object)) (slot-definition-name slot))))


(defmethod slot-makunbound-using-class ((class transactional-class) object
                                         (slot transactional-effective-slot-definition))
  (prog1 (call-next-method)
    (vstm:atomic
      (vstm:rmw (slots (slots-of object))
        (aprog1 (ctrie-fork slots)
          (ctrie-drop it (slot-definition-name slot)))))))

(defmethod slot-boundp-using-class ((class transactional-class) object
                                     (slot computed-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class transactional-class) object
                                         (slot computed-effective-slot-definition))
  t)

(defmethod index::destroy-object-with-class :around ((class transactional-class) object)
  (vstm:atomic
    (call-next-method)
    (vstm:rollback))
  (ctrie-drop (instances-of (class-of object)) (id-of object))
  (slot-makunbound object 'id)
  (slot-makunbound object 'slots))
    
    
(defmethod print-object ((self transactional-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (transactional)" (id-of self))))

(defun all-transactional-classes ()
  (ctrie-keys (transactional-class-index)))

(defgeneric transactional-object-eq (object1 object2))

(defmethod transactional-object-eq ((object1 t) (object2 t))
  (eq object1 object2))

(defmethod transactional-object-eq ((object1 transactional-object)
                                     (object2 transactional-object))
  (and
    (eq (class-of object1) (class-of object2))
    (eql (id-of object1) (id-of object2))))

(defclass transactional-proxy ()
  ((class-name :initarg :class-name :accessor class-name-of :type symbol)
    (slots :initarg :slots :accessor slots-of)
    (instance-id :initarg :instance-id :accessor instance-id-of :type integer)))

(defun find-transactional-object (class-name id)
  (let1 instances (ctrie-get (transactional-class-index) class-name)
    (when instances
      (let1 object (ctrie-get instances id)
        (when object
          (funcall #'make-instance class-name :id id))))))
            
(define-layered-method maybe-box :in persistent ((object transactional-object))
  (let ((class-name (class-of object))
         (id (id-of object)))
    (maybe-box
      (make-instance 'transactional-proxy :class-name class-name :instance-id id
        :slots (ctrie-get (ctrie-get (transactional-class-index) class-name) id)))))

(define-layered-method maybe-unbox :in persistent ((object transactional-proxy))
  (funcall #'make-instance (class-name-of object)
    :id (instance-id-of object)
    :slots (slots-of object)))
   


(defun transactional-objects-of-class (class-name &optional include-subclasses)
  (flet ((get-instances (class-name)
           (let1 instances (ctrie-get (transactional-class-index) class-name)
             (when instances
               (mapcar (lambda (id) (unless (zerop id) (funcall #'make-instance class-name :id id)))
                 (reverse (ctrie-keys instances)))))))
    (let1 classes (if include-subclasses
                    (mapcar #'class-name (object:class-subclasses class-name))
                    (list class-name))
      (remove-if #'null (loop for class in classes appending (get-instances class))))))



(defmacro define-transactional-class (name supers slots &rest options)
  (let* ((new-slots nil)
          (options (remove :metaclass options :key #'first))
          (options (push '(:metaclass transactional-class) options)))
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
                                    :compute-as :allocation :transient)
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
                                         :allocation :transient)
                                 (list :allocation :computed :slot-value-function svf ;(compile nil svf)
                                   :setf-slot-value-function ssvf #+()(compile nil ssvf))))
                         new-slots)))
           ((not transient)
             (push (list* (car slot)
                     (append (remove-from-plist (rest slot)
                               :allocation :transient)
                       '(:allocation :transactional)))
               new-slots))
           (t (push  (list* (car slot)
                       (append (remove-from-plist (rest slot)
                                 :allocation :transient)
                         '(:allocation :instance)))
                new-slots)))
      finally (setf new-slots (nreverse new-slots)))
    `(defclass ,name ,supers ,new-slots ,@options)))



(define-transactional-class  unfunny ()
  ((name    
    :initarg :name :initform "" :accessor thing-name :type string
     :documentation "Fullname of comment, e.g. t1_c3v7f8u")
   (kind    
    :initarg :kind :initform "" :accessor thing-kind :type string
     :documentation "String identifier that denotes the object's type")
   (data    
     :initarg :data :initform nil :accessor thing-data
     :documentation "Custom data structure")))


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

(defclass persistent-class (transactional-class)
  ((instances :accessor instances-of)))

(defclass persistent-object (transactional-object)
  ())

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

(defmethod validate-superclass ((class persistent-class) (superclass transactional-class))
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

(defclass persistent-direct-slot-definition (transactional-direct-slot-definition)
  ()
  (:default-initargs :allocation :persistent))
 
(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:persistent  (find-class 'persistent-direct-slot-definition))
    (t  (call-next-method))))

(defun slot-definition-persistent-p (slot-definition)
  (eq (slot-definition-allocation slot-definition) :persistent))


(defclass persistent-effective-slot-definition (transactional-effective-slot-definition)
  ())

(defmethod compute-effective-slot-definition ((class persistent-class) (name t)
                                               direct-slot-definitions)
  (call-next-method))

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignorable initargs))
  (case (getf initargs :allocation)
    (:persistent  (find-class 'persistent-effective-slot-definition))
    (t   (call-next-method))))

(mm:defmmclass persistent-transactional-object (persistent-ctrie)
  ())

(defvar *persistent-object-cells* (make-instance 'transient-ctrie))


(defmethod get-cell ((object persistent-object))
  (ctrie-put-ensure (ctrie-put-ensure *persistent-object-cells*
                      (class-name (class-of object)) (make-instance 'transient-ctrie))
    (id-of object) (vstm:create-var
                     (or (ctrie-get (instances-of (class-of object)) (id-of object))
                       (ctrie-put (instances-of (class-of object)) (id-of object)
                         (with-active-layers (persistent)
                           (make-instance 'persistent-transactional-object :name
                             (format nil "<~A/~D>" (class-name (class-of object))
                               (slot-value object 'id)) :test 'eq :context '(persistent))))))))


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
  (call-next-method))

(defmethod (setf slot-value-using-class) (value (class persistent-class) object
                                           (slot computed-effective-slot-definition))
  (call-next-method))

(defmethod slot-value-using-class ((class persistent-class) object
                                    (slot persistent-effective-slot-definition))
;;  (call-next-method))

  (vstm:atomic 
    (multiple-value-bind (value present-p)
      (ctrie-get (vstm:read-var (slots-of object)) (slot-definition-name slot))
      (if present-p (maybe-unbox value)
        (slot-unbound class object (slot-definition-name slot))))))



(defmethod (setf slot-value-using-class) (value (class persistent-class) object
                                           (slot persistent-effective-slot-definition))
  (call-next-method))

(defmethod slot-boundp-using-class ((class persistent-class) object
                                     (slot persistent-effective-slot-definition))
  (call-next-method))

(defmethod slot-makunbound-using-class ((class persistent-class) object
                                         (slot persistent-effective-slot-definition))
  (call-next-method))

(defmethod slot-boundp-using-class ((class persistent-class) object
                                     (slot computed-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class persistent-class) object
                                         (slot computed-effective-slot-definition))
  t)

(defmethod print-object ((self persistent-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "id: ~D (persistent)" (id-of self))))

(defun all-persistent-classes ()
  (ctrie-keys (persistent-class-index)))

(defun all-persistent-objects ()
  (apply #'append
    (mapcar #'persistent-objects-of-class
      (all-persistent-classes))))

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
      for slot      = (ensure-list raw-slot)
      for class     = (eq (getf (rest slot) :allocation) :class)
      for derived   = (or (eq (getf (rest slot) :allocation) :derived)
                        (getf (rest slot) :derive-as))
      for computed  = (or (eq (getf (rest slot) :allocation) :computed)
                        (getf (rest slot) :slot-value-function)
                        (getf (rest slot) :compute-as))
      for transient = (or (getf (rest slot) :transient)
                        (eq (getf (rest slot) :allocation) :instance)
                        (and (find :persistent (rest slot))
                          (not (getf (rest slot) :persistent))))
      do (cond
           (class (push (list* (car slot)
                          (append (remove-from-plist (rest slot)
                                    :update-by :derive-as :constrain-by :compute-as :cache
                                    :slot-value-function :setf-slot-value-function
                                    :allocation :transient :persistent)
                            '(:allocation :class)))
                    new-slots))
           (computed (let* ((svf (or (getf (rest slot) :slot-value-function)
                                   (awhen (getf (rest slot) :compute-as)
                                     (eval `(lambda (-self-) (declare (ignorable -self-)) ,it)))
                                   (constantly nil)))
                             (ssvf (or (getf (rest slot) :setf-slot-value-function)
                                     (awhen (getf (rest slot) :update-by)
                                       (eval `(lambda (-value- -self-)
                                                (declare (ignorable -value- -self-)) ,it)))
                                     (constantly nil))))
                       (push (list* (car slot)
                               (append (remove-from-plist (rest slot)
                                         :compute-as :update-by :derive-as :constrain-by
                                         :slot-value-function :setf-slot-value-function
                                         :allocation :transient :persistent)
                                 (list :allocation :computed :slot-value-function (compile nil svf)
                                   :setf-slot-value-function (compile nil ssvf))))
                         new-slots)))
           ((not transient)
             (push (list* (car slot)
                     (append (remove-from-plist (rest slot)
                               :update-by :derive-as :constrain-by :compute-as :cache
                               :slot-value-function :setf-slot-value-function
                               :allocation :transient :persistent)
                       '(:allocation :persistent)))
               new-slots))
           (t (push  (list* (car slot)
                       (append (remove-from-plist (rest slot)
                                 :update-by :derive-as :constrain-by :compute-as :cache
                                 :slot-value-function :setf-slot-value-function
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


;; (defmethod update-instance-for-different-class :before ((previous fundamental-object) 
;;                                                          (current transactional-object)
;;                                                          &rest initargs)
;;   ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
