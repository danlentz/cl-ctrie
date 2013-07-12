;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactional Collection  Object 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass collection-object ()
  ()
  (:documentation "collection-object protocol class"))

(define-transactional-class transactional-collection-object (collection-object)
  ((root-node
     :initform nil
     :initarg :root-node
     :accessor root-node-of)
    (context
      :initarg :context
      :accessor context-of)
    (env
      :compute-as (apply #'combined-layer-context contextl::*root-context* (context-of -self-))
      :cache t
      :reader env-of)
    (type-name
      :initform nil
      :initarg :type-name
      :accessor type-name-of
      :index-type index:hash-index
      :index-reader collections-of-type
      :index-values all-collection-types)
    (category
      :initform nil
      :initarg :category
      :accessor category-of
      :index-type index:category-index
      :index-var *categorized-collection-index*
      :index-reader collections-with-category
      :index-values all-collection-categories)
    (flags
      :initform nil
      :initarg :flags
      :accessor :flags-of
      :index-type index:hash-list-index
      :index-reader collections-with-flag
      :index-values all-collection-flags)
    (creation-timestamp
      :initform (get-universal-time)
      :accessor creation-timestamp-of
      :index-type index:hash-index
      :index-reader collections-with-creation-timestamp
      :index-values all-collection-creation-timestamps)
    (package-name
      :initform nil
      :initarg :package-name
      :accessor package-name-of
      :index-type index:hash-index
      :index-initargs (:test #'equal)
      :index-reader collections-in-package-name)
    (symbol-name
      :initform (byte-vector-to-hex-string (create-unique-id-byte-vector))
      :initarg :symbol-name
      :accessor symbol-name-of
      :index-type index:hash-index
      :index-initargs (:test #'equal)
      :index-reader collections-with-symbol-name)
    (symbol-for-binding
      :compute-as (let* ((package-name (package-name-of -self-))
                          (package (when package-name
                                     (or (find-package package-name)
                                       (make-package package-name)))))
                    (if package
                      (intern (symbol-name-of -self-) package)
                      (make-symbol (symbol-name-of -self-))))
      :reader symbol-for-binding-of
      :cache t)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Collection Object 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-persistent-class persistent-collection-object (transactional-collection-object)
  ((root-node
     :initform nil
     :initarg :root-node
     :accessor root-node-of)
    (context
      :initarg :context
      :accessor context-of)
    (type-name
      :initform nil
      :initarg :type-name
      :accessor type-name-of
      :index-type index:hash-index
      :index-reader persistent-collections-of-type
      :index-values all-persistent--collection-types)
    (category
      :initform nil
      :initarg :category
      :accessor category-of
      :index-type index:category-index
      :index-var *categorized-persistent-collection-index*
      :index-reader persistent-collections-with-category
      :index-values all-persistent-collection-categories)
    (flags
      :initform nil
      :initarg :flags
      :accessor :flags-of
      :index-type index:hash-list-index
      :index-reader persistent-collections-with-flag
      :index-values all-persistent-collection-flags)
    (creation-timestamp
      :initform (get-universal-time)
      :reader creation-timestamp-of
      :index-type index:hash-index
      :index-reader persistent-collections-with-creation-timestamp
      :index-values all-persistent-collection-creation-timestamps)
    (package-name
      :initform nil
      :initarg :package-name
      :accessor package-name-of
      :index-type index:hash-index
      :index-initargs (:test #'equal)
      :index-reader persistent-collections-in-package-name)
    (symbol-name
      :initform (byte-vector-to-hex-string (create-unique-id-byte-vector))
      :initarg :symbol-name
      :accessor symbol-name-of
      :index-type index:hash-index
      :index-initargs (:test #'equal)
      :index-reader persistent-collections-with-symbol-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Class "Collection" 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import '(order:order-layer order:default-order))

(defclass collection ()
  ((object
     :initarg :object
     :accessor collection-object-of)
    (context
      :initarg :context
      :initform '() 
      :accessor collection-context-of)))

(defclass set (collection)
  ())

(defclass bag (collection)
  ())

(defclass map (collection)
  ())

(defclass seq (collection)
  ())

(defun make-collection (type &key object name package (persistent nil) (flags nil) (category '(t))
                         (order 'order:default-order) (balance 'weight-balanced) (transactions 'dstm))
  ;; (if-exists :error) (if-does-not-exist :create)
  (when object (check-type object collection-object)
    (unless (eq type (type-name-of object))
      (warn "TYPE arg (~S) disagrees with type of OBJECT arg (~S)" type object))
    (return-from make-collection
      (make-instance (type-name-of object) :object object :context (context-of object))))
  (let* ((allocation    (if persistent 'persistent 'transient))
          (object-class (if persistent 'persistent-collection-object 'transactional-collection-object))
          (context      (list allocation type transactions order balance))
          (object       (apply #'make-instance object-class
                          (append `(:context ,context :type-name ,type)
                            (when name     `(:symbol-name ,name))
                            (when package  `(:package-name ,package))
                            (when flags    `(:flags ,flags))
                            (when category `(:category ,category))))))
    (make-instance type :object object :context context)))

(defmacro with-collection ((collection) &body body)
  `(funcall-with-layer-context (env-of (collection-object-of ,collection))
     (lambda () ,@body)))

(defmethod root-node-of ((c null))
  nil)

(defmethod root-node-of ((c collection))
  (with-collection (c)
    (root-node-of (collection-object-of c))))

(defmethod (setf root-node-of) (value (c collection))
  (prog1 value
    (with-collection (c)
      (setf (root-node-of (collection-object-of c)) value))))

(defmacro/once with-update-to-collection ((root-node &once collection) &body body)
  `(prog1 ,collection
     (when ,collection
       (with-collection (,collection)
         (let1 ,root-node (root-node-of ,collection)
           (declare (ignorable ,root-node))
           (setf (root-node-of ,collection)
             ,@body))))))





