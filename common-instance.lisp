;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :object
  (:nicknames :obj)
  (:use :closer-common-lisp :c2mop :alexandria)
  (:export :proto
    :find-class*
    :finalized-class
    :new
    :slot-value*
    :slot-definition-sexp
    :object-layout
    :layout-info
    :dd-name
    :*sbcl-struct-inherits*
    :defstruct-descriptor-name
    :layout-info-or-die
    :save-able-supers
    :struct-supers
    :struct-defs
    :create-make-foo
    :sbcl-define-structure
    :super-layout
    :super-layouts
    :serializable-slots
    :serializable-slots-using-class
    :find-direct-slot-definition-by-initarg
    :class-subclasses
    :class-superclasses
    :class-slot-names
    :clone-instance
    :cloned-object-as
    :cloned-object
    :copy-instance
    :make-uninitialized-instance
    :unparse-direct-slot-definition
    :class-add-slot))

(in-package :object)

(unless (find-package :mop)
  (rename-package (package-name :c2mop) (package-name :c2mop) '(:mop :c2mop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *nuke-existing-packages*   nil)
(defvar *nuke-existing-classes*    nil)
(defvar *store-class-superclasses* t)

(defun find-class* (class-designator)
  (typecase class-designator
    (class    class-designator)  
    (keyword (find-class* (string class-designator)))
    (string  (find-class* (read-from-string class-designator)))
    (symbol  (find-class class-designator))
    (t       (find-class class-designator))))

(defun proto (thing)
  (flet ((get-proto (c)
           (let ((cc (find-class c)))
             (c2mop:finalize-inheritance cc)
             (c2mop:class-prototype cc))))
    (etypecase thing
      (class  (get-proto thing))
      (standard-object (get-proto (class-of thing)))
      (symbol (get-proto  thing)))))

(defun finalized-class (class-designator)
  (finalize-inheritance (find-class* class-designator))
  (find-class* class-designator))

(defun new (&rest args)
  (apply #'make-instance args))

(defun slot-value* (obj slot &optional (unbound-return :unbound))
  (handler-case (values (slot-value obj slot) t)
    (unbound-slot (c) (values unbound-return c))))

(defun required-arg (name)
  (error "~S is a required argument" name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unparse-direct-slot-definition (slotd)
  "Return a list of slot details which can be used as an argument to ensure-class"
  (list
    :name (sb-mop:slot-definition-name slotd)
    :type (sb-mop:slot-definition-type slotd)
    :initfunction (sb-mop:slot-definition-initfunction slotd)
    :allocation (sb-mop:slot-definition-allocation slotd)
    :initargs (sb-mop:slot-definition-initargs slotd)
    :readers (sb-mop:slot-definition-readers slotd)
    :writers (sb-mop:slot-definition-writers slotd)))


(defun class-add-slot (class-name slot-name)
  (sb-mop:ensure-class-using-class
    (find-class class-name) class-name
    :direct-slots (cons `(:name ,slot-name)
                    (mapcar #'unparse-direct-slot-definition
                      (sb-mop:class-direct-slots (find-class class-name))))))

(defun find-direct-slot-definition-by-initarg (class initarg)
  (loop named outer
        for super-class in (cdr (c2mop:class-precedence-list class))
        do
        (unless (c2mop:class-finalized-p super-class)
          (c2mop:finalize-inheritance super-class))
        (loop for slot in (c2mop:class-direct-slots super-class)
              when (member initarg (c2mop:slot-definition-initargs slot))
          do (return-from outer slot))))

(defun mapappend (fun &rest args)
   (if (some 'null args)
       '()
       (append (apply fun (mapcar 'car args))
         (mapappend fun (mapcar 'cdr args)))))


;;   (let ((result (mapappend #'(lambda (c) (when c (class-direct-subclasses c))) (list (find-class* class))))) result))
;;     (unless proper? (push (find-class* class) result))
;;     (remove-duplicates result)))

;; (subclasses 'standard-object)

;; (defun subclasses (class &key (proper? t))
;;   "Returns all of the subclasses of the class including the class itself."
;;   (let ((result nil))
;;     (map-subclasses class (lambda (class)
;;                             (push class result))
;;                     :proper? proper?)
;;     (nreverse result)))

(defun class-subclasses (thing &key proper)
  (labels ((all-subclasses (class)
             (cons class
                   (mapcan #'all-subclasses
                           (c2mop:class-direct-subclasses class)))))
    (let ((result (all-subclasses (find-class* thing))))
      (if proper (rest result) result))))

(defun class-superclasses (thing &key (proper t))
  "Returns a list of superclasses of thing. Thing can be a class,
   object or symbol naming a class. The list of classes returned is
   'proper'; it does not include the class itself."
  (let ((result (class-precedence-list (find-class* thing))))
    (if proper (rest result) result)))


(defun class-slot-names (thing)
  (let ((class (find-class* thing)))
    (if class
      (mapcar #'c2mop:slot-definition-name
              (c2mop:class-slots (finalized-class class)))
      (progn
        (warn "class for ~a not found)" thing)
        nil))))

(defgeneric class-copyable-slots (class)
  (:documentation "Returns the set of slots of CLASS which are
considered for copying by COPY-INSTANCE.
If CLASS is of type STANDARD-CLASS, all slots \(as returned by
CLASS-SLOTS) are considered.")
  (:method ((class standard-class))
    (c2mop:class-slots class)))

(defgeneric make-uninitialized-instance (class)
  (:documentation "Allocates a fresh uninitialized instance of the
given class CLASS.
If CLASS is of type CLASS, ALLOCATE-INSTANCE is used.")
  (:method ((class class))
           (allocate-instance class)))

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a \(shallow) copy of OBJECT.
An uninitialized object of the same class as OBJECT is allocated by
calling MAKE-UNINITIALIZED-INSTANCE.  For all slots returned by
\(CLASS-COPYABLE-SLOTS \(CLASS-OF OBJECT)), the returned object has the
same slot values and slot-unbound status as OBJECT.
REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
           (let* ((class (class-of object))
                  (copy (make-uninitialized-instance class)))
             (dolist (slot (class-copyable-slots class))
               (let ((slot-name (c2mop:slot-definition-name slot)))
                 (when (slot-boundp object slot-name)
                   (setf (slot-value copy slot-name)
                         (slot-value object slot-name)))))
             (apply #'reinitialize-instance copy initargs))))

#+()
(defun cloned-object (object  &rest slots)
  "Copy on write."
  (let* ((class (class-of object))
          (clone (allocate-instance class)))
    (dolist (slot (c2mop:class-slots class))
      (let ((name (c2mop:slot-definition-name slot)))
        (when (slot-boundp object name)
          (setf (slot-value clone name) (slot-value object name)))))
    (do ((name  (car  slots) (car  slots))
         (value (cadr slots) (cadr slots))
         (slots (cddr slots) (cddr slots)))
        ((null name)
         clone)
      (setf (slot-value clone name) value))))

;; TODO: rename
(defun cloned-object-as (class object)
  (assert (subclassp class (class-of object)))
  (let* ((class (find-class class))
          (clone (allocate-instance class)))
    (prog1 clone
      (dolist (slot (c2mop:class-slots class))
        (let ((name (c2mop:slot-definition-name slot)))
          (when (slot-boundp object name)
            (setf (slot-value clone name) (slot-value object name))))))))
    
;; (cloned-object (uuid:make-v4-uuid))
;; C3C72EC2-A553-4436-9B61-589516002CD4

;; (cloned-object-as 'cl-ctrie::context (uuid:make-v4-uuid))
;; 4DCBAA4C-8461-46E5-AA54-BFFIND-CLASS*C047D4F8

;; (class-of (cloned-object-as 'cl-ctrie::context (uuid:make-v4-uuid)))
;; #<STANDARD-CLASS CL-CTRIE::CONTEXT>



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marshal Object as S-Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *object->sexp-visited-objects* nil)

(defun marshal-object (obj &key suppress-types suppress-properties)
  "Converts arbitrary CLOS objects into s-expressions that can easily be used in tests."
 (if (and (subtypep (type-of obj) 'standard-object)
        (find obj *object->sexp-visited-objects*))
    :recursive-reference
    (let ((*object->sexp-visited-objects* (cons obj *object->sexp-visited-objects*)))
      (cond ((find (type-of obj) suppress-types) :suppressed)
        ((subtypep (type-of obj) 'standard-object)
          (multiple-value-bind (instance slots)
            (make-load-form-saving-slots obj)
            (let ((class (cadr (cadadr instance)))
                   (bound-slots (mapcar #'(lambda (s)
                                            (list (second (first (last (second s))))
                                              (marshal-object (second (third s))
                                                :suppress-types suppress-types
                                                :suppress-properties suppress-properties)))
                                  (remove 'slot-makunbound (rest slots) :key #'first))))
              (list* class
                (sort (remove-if #'(lambda (slot)
                                     (find (first slot) suppress-properties))
                        bound-slots)
                  #'string< :key (compose #'symbol-name #'first))))))
        ((null obj) nil)
        ((listp obj)
          (mapcar #'(lambda (obj) (marshal-object obj
                                    :suppress-types suppress-types
                                    :suppress-properties suppress-properties)) obj))
        (t obj)))))


;; (mapcar #'get-slot-details (sb-mop:class-slots (find-class* 'cl-user::test-file)))
;;
;; ((:NAME ASDF::NAME :ALLOCATION :INSTANCE :INITARGS (:NAME) :READERS NIL
;;    :TYPE  STRING :WRITERS NIL)
;;   (:NAME ASDF:VERSION :ALLOCATION :INSTANCE :INITARGS (:VERSION) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::DESCRIPTION :ALLOCATION :INSTANCE :INITARGS (:DESCRIPTION)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::LONG-DESCRIPTION :ALLOCATION :INSTANCE :INITARGS
;;     (:LONG-DESCRIPTION) :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::LOAD-DEPENDENCIES :ALLOCATION :INSTANCE :INITARGS NIL :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::IN-ORDER-TO :ALLOCATION :INSTANCE :INITARGS (:IN-ORDER-TO)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::DO-FIRST :ALLOCATION :INSTANCE :INITARGS (:DO-FIRST) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::INLINE-METHODS :ALLOCATION :INSTANCE :INITARGS NIL :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::PARENT :ALLOCATION :INSTANCE :INITARGS (:PARENT) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::RELATIVE-PATHNAME :ALLOCATION :INSTANCE :INITARGS (:PATHNAME)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::ABSOLUTE-PATHNAME :ALLOCATION :INSTANCE :INITARGS NIL :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::OPERATION-TIMES :ALLOCATION :INSTANCE :INITARGS NIL :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::AROUND-COMPILE :ALLOCATION :INSTANCE :INITARGS (:AROUND-COMPILE)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::%ENCODING :ALLOCATION :INSTANCE :INITARGS (:ENCODING) :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::PROPERTIES :ALLOCATION :INSTANCE :INITARGS (:PROPERTIES) :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME TYPE :ALLOCATION :INSTANCE :INITARGS (:TYPE) :READERS NIL :TYPE T
;;     :WRITERS NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support Jand-wrolling Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serializable-slots (object)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot-definitions to serialize. The default
    is to call serializable-slots-using-class with the object 
    and the objects class")
  (:method ((object standard-object))
   (serializable-slots-using-class object (class-of object)))
#+(or sbcl cmu openmcl allegro)
  (:method ((object structure-object))
   (serializable-slots-using-class object (class-of object)))
  (:method ((object condition))
   (serializable-slots-using-class object (class-of object))))

; unfortunately the metaclass of conditions in sbcl and cmu 
; are not standard-class

(defgeneric serializable-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class standard-class))
   (class-slots class))
#+(or sbcl cmu openmcl allegro) 
  (:method ((object t) (class structure-class))
   (class-slots class))
#+sbcl
  (:method ((object t) (class sb-pcl::condition-class))
   (class-slots class))
#+cmu
  (:method ((object t) (class pcl::condition-class))
   (class-slots class)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deep SBCL Magic on Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structure definition storing
(defun object-layout (obj)
  (slot-value obj 'sb-pcl::wrapper))

(defun layout-info (obj)
  (declare (type sb-kernel:layout obj))
  (slot-value obj 'sb-int:info))

(defun dd-name (dd)
  "defstruct-descriptor name"
  (slot-value dd 'sb-kernel::name))

(defun defstruct-descriptor-name (dd)
  "defstruct-descriptor name"
  (slot-value dd 'sb-kernel::name))

(defvar *sbcl-struct-inherits*
  `(,(object-layout (find-class t))
    ,@(when-let (class (find-class 'sb-kernel:instance nil))
        (list (object-layout class)))
    ,(object-layout (find-class 'cl:structure-object))))

(defstruct (struct-def (:conc-name sdef-))
  (supers (required-arg :supers) :type list)
  (info (required-arg :info) :type sb-kernel:defstruct-description))

(defun layout-info-or-die (obj)
  (let ((wrapper (object-layout obj)))
    (if wrapper
        (or (layout-info wrapper) 
            (error "No defstruct-definition for ~A." obj))
        (error "No wrapper for ~A." obj))))

(defun save-able-supers (obj)
  (set-difference (coerce (slot-value (object-layout obj) 'sb-kernel::inherits)
                          'list)
                  *sbcl-struct-inherits*))

(defun struct-supers (obj)
  (loop for x in (save-able-supers obj) 
     collect (let ((name (dd-name (layout-info x))))
               (if *store-class-superclasses* 
                   (find-class name)
                   name))))


;; Restoring 
(defun struct-defs (info)
  (append (sb-kernel::constructor-definitions info)
          (sb-kernel::class-method-definitions info)))

(defun create-make-foo (dd)
  (declare (optimize speed))
  (funcall (compile nil `(lambda () ,@(struct-defs dd))))
  (find-class (dd-name dd)))


(defun sb-kernel-defstruct (dd supers source)
  (declare (ignorable source))
  (sb-kernel::%defstruct dd supers source))


(defun sbcl-define-structure (dd supers)
  (cond ((or *nuke-existing-classes*  
             (not (find-class (dd-name dd) nil)))
         ;; create-struct
         (sb-kernel-defstruct dd supers nil)
         ;; compiler stuff
         (sb-kernel::%compiler-defstruct dd supers)
         ;; create make-?
         (create-make-foo dd))
    (t (warn "~A will not be overwritten.  See ~S" (find-class (dd-name dd))
         *nuke-existing-classes*)   (find-class (dd-name dd)))))
         
(defun super-layout (super)
  (etypecase super
    (symbol (object-layout (find-class super)))
    (structure-class 
     (super-layout (dd-name (layout-info-or-die super))))))

(defun super-layouts (supers)
  (loop for super in supers 
    collect (super-layout super)))

