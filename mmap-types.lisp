;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)


(defmmclass mm-symbol ()
  ((package-name :initarg :package)
   (symbol-name :initarg :symbol))
  (instantiator unbox-symbol))


(defmmclass mm-box ()
  ((ptr))
  (instantiator unbox-box))


(defmmclass marray () ;; special arrays
  ((length :type mindex :initarg :length :reader marray-length)
   (base :type mptr :initarg :base :reader marray-base))
  (:documentation "The base representation of a memory-mapped vector.")
  (walker walk-array))


(defmmclass mm-array (marray) ;; stored lisp arrays
  ()
  (instantiator unbox-array)
  (walker walk-array))


(defmmclass mm-string (mm-array)
  ()
  (instantiator unbox-string)
  (walker walk-array))


(defmmclass mm-cons ()
  ((a :initarg :car)
   (b :initarg :cdr))
  (instantiator unbox-cons))


(defmmclass mm-array-as-list (mm-array)
  ()
  (walker walk-array)
  (instantiator unbox-array-as-list))


(eval-when (:compile-toplevel :load-toplevel)
  (defun specialized-class-array-boxer-name (classname)
    (alexandria:symbolicate classname '-array-boxer))

  (defun generate-boxed-numeric-type (name &key type)
    (let ((unboxer (alexandria:symbolicate 'unbox- name)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel)
	   (defmmclass ,name ()
	       ((value :type ,type))
	     (instantiator ,unboxer)))
	 (with-constant-tag-for-class (tag ,name)
	   (defun ,unboxer (index)
	     (d (mpointer tag index) 0 ,type)))
	 (define-box-array ,(specialized-class-array-boxer-name name) ,name ,type)))))


(defmacro define-boxed-numeric-types (&rest typespecs)
  (let (types)
    `(progn 
       ,@(loop for typespec in typespecs
	       collect 
	       (destructuring-bind (name &optional (type name))
		   (alexandria:ensure-list typespec)
		 (let ((name (alexandria:symbolicate 'boxed- name)))
		   (push `(,name . ,type) types)
		   (generate-boxed-numeric-type name :type type))))

       (macrolet ((later ()
		    (generate-boxer ',(reverse types))))
	 (later)))))


(defun unbox-array-internal-general (elem-tag elem-index len)
  (declare (type mtag elem-tag) (type mindex elem-index) (type mindex len))
  (let* ((mtagmap (mtagmap elem-tag))
	 (ilen (mtagmap-elem-len mtagmap))
	 (instantiator (mtagmap-instantiator mtagmap))
	 (array (make-array len)))
    (declare (type mm-instantiator instantiator))
    (loop for i below len
	  for index from elem-index by ilen
	  do (setf (aref array i) (funcall (the mm-instantiator instantiator) index)))
    array))


(defgeneric lisp-object-to-mptr-impl (object)
  (:documentation "Override this generic function to give an
  user-defined class an alternative serialisation in the memory mapped
  datastore. Return the mptr pointing to this serialisation. Note that
  the serialisation for builtin types are inlined and cannot be
  affected."))


(eval-when (:compile-toplevel :load-toplevel)
  (defun generate-boxer (types)
    `(progn
       (defun box-object (object)
	 (typecase object
	   ,@(loop for (class . type) in types
               collect
               `(,type
                  ,(let* ((class (find-class class)) 
                           (tag (mm-metaclass-tag class)))
                     `(let ((index (mtagmap-alloc (mtagmap ,tag) ,(mm-metaclass-len class))))
                        (setf (d (mpointer ,tag index) 0 ,type) object)
                        (make-mptr ,tag index)))))
	   (symbol (box-symbol object))
	   (string (box-string object))
	   (array  (locally (declare (notinline box-array))
		     (box-array object)))
	   (cons   (box-cons object))
	   (t      (lisp-object-to-mptr-impl object))))

       (defun unbox-array-internal (elem-tag elem-index len)
	 (declare (type mtag elem-tag) (type mindex elem-index) (type mindex len))
	 (case elem-tag
	   ,@(loop for (classname . type) in types
               for class = (find-class classname)
               for tag = (mm-metaclass-tag class)
               collect `(,tag 
                          (let ((array (make-array len :element-type ',type))
                                 (pointer (mpointer ,tag elem-index)))
                            (declare (type (simple-array ,type) array))
                            (loop for i below len do (setf (aref array i) (d pointer i ,type)))
                            array)))
	   (t (unbox-array-internal-general elem-tag elem-index len))))
       
       (defun box-array (object)
	 (assert (not (cdr (array-dimensions object))))
	 (declaim (notinline general-box-array))
	 (etypecase object
	   (simple-array  (typecase object
                            ,@(loop
                                for (class . type) in types
                                collect `((array ,type)
                                           (,(specialized-class-array-boxer-name class) object)))
                            (t (general-box-array object))))
	   (array         (general-box-array object)))))))


(defmacro define-box-array (array-boxer-name box-class lisp-type &key convertor
                             (array-class 'mm-array))
  (let ((stored-type (if (stored-cffi-type lisp-type) lisp-type 'mptr)))
   `(with-constant-tag-for-class (element-tag ,box-class) 
      (with-constant-tag-for-class (array-tag ,array-class)
	(defun ,array-boxer-name (array)
	  (declare (type (simple-array ,lisp-type (*)) array))
	  (let* ((len (length array))
                  (index (mtagmap-alloc (mtagmap element-tag)
                           (* ,(mm-metaclass-len (find-class box-class)) len)))
		 (pointer (mpointer element-tag index))
		 ,@(when convertor ;; conversion first! allocating can invalidate our pointers
		       `((array (map '(vector ,stored-type) #',convertor array)))))
	    ,@(when convertor `((declare (type (simple-array ,stored-type (*)) array))))
	    (loop for i below len do (setf (d pointer i ,stored-type) (aref array i)))
	    (let ((barray (mtagmap-alloc (mtagmap array-tag)
                            ,(mm-metaclass-len (find-class array-class)))))
	      (with-pointer-slots (base length) ((mpointer array-tag barray) ,array-class)
		(setf base (make-mptr element-tag index)
		      length len)
		(make-mptr array-tag barray)))))))))


(define-box-array general-box-array mm-box t :convertor lisp-object-to-mptr)

(define-boxed-numeric-types
  (byte (unsigned-byte 8))
  double-float
  single-float
  (unsigned (unsigned-byte 64))
  (signed (signed-byte 64)))


(defmmclass mm-fixed-string (mm-string)
  ((cropped-length :type mindex :initform 0))
  (walker walk-array))


;;
;;


(defun marray-ref (marray i)
  "Like aref, but for memory mapped arrays. note: doesn't work on specialised arrays"
  (declare (type mindex i))
  (mptr-to-lisp-object (dw (mptr-pointer (marray-base marray)) i)))


(defun (setf marray-ref) (new marray i) 
  (declare (type mindex i))
  (let ((new (lisp-object-to-mptr new)))
    (setf (dw (mptr-pointer (marray-base marray)) i) new))
  new)


(defclause-sequence in-marray index-of-marray
  :access-fn     'marray-ref
  :size-fn       'marray-length
  :sequence-type 'marray
  :element-type  t
  :element-doc-string "Elements of an marray"
  :index-doc-string   "Indices of marray")


(defun marray-to-list (marray)
  "Converts a memory mapped array to a Lisp list; nil is converted to nil"
  (when marray (iter (for c in-marray marray) (collect c))))


(defun list-to-marray (list)
  "Converts a Lisp list to a memory-mapped array object; nil is converted to nil"
  (when list (make-marray (length list) :initial-contents list)))
