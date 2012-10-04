;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)

(defmacro define-lisp-object-to-mptr ()
  `(defun lisp-object-to-mptr (obj)
     "Stores the object OBJ in the memory mapped datastore and returns the mptr referring to it"
     (typecase obj
       (mm-object (ptr obj))
       (t (box-object obj)))))


(define-lisp-object-to-mptr) ;; should be redefined after box-object is
			     ;; defined, which needs many types to be
			     ;; defined, in a circular fashion


(defmacro with-constant-tag-for-class ((tagsym classname) &body body)
  (check-type tagsym symbol)
  (check-type classname symbol)
  (let ((class (find-class classname)))
    (ensure-finalize-inheritance class)
    (let ((tag (mm-metaclass-tag class)))
      (check-type tag mtag)   
      `(progn
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (assert (= ,tag ,(mm-metaclass-tag (find-class classname))) ()
             "The tag for classname ~A has changed; compiled code may be invalid" ',classname))
	 (symbol-macrolet ((,tagsym ,tag))
	   ,@body)))))


(defun force-mptr (obj)
  (etypecase obj
    (mptr obj)
    (mm-object (ptr obj))))


(defun mptr (obj)
  "If OBJ is already an integer, return it. If it is a memory mapped
  object, return the MPTR corresponding to it. Otherwise, raise an error."
  (force-mptr obj))


(defun force-tag (obj)
  (etypecase obj
    (mtag obj)
    (mtagmap (mm-metaclass-tag (mtagmap-class obj)))
    (symbol (mm-metaclass-tag (find-class obj)))
    (mm-metaclass (mm-metaclass-tag obj))
    (mm-object (mptr-tag (ptr obj)))
    (mptr (mptr-tag obj))))


(defmethod finalize-inheritance :after ((class mm-metaclass))
  (setup-mtagmap-for-metaclass class)
  (setup-default-metaclass-functions class)
  class)


(defun metaclass-default-walker-form (class)
  (let ((offsets (loop
                   for slot in (class-slots class)
                   when (slot-definition-mmap-pointer-p slot)
                   collect (slot-value slot 'offset))))
    (when offsets
      `(lambda (mptr walker-func)
	 (declare (type mm-walk-func walker-func))
	 ,@(loop
             for offset in offsets
             collect `(let ((p (+ mptr ,(ash offset +mtag-bits+))))
                        (funcall walker-func (dw (mptr-pointer p)) p 1)))))))


(defun metaclass-default-instantiator-form (class)
  `(lambda (index)
     (declare  (type mindex index))
     (let ((instance (allocate-instance ,class)))
       (setf (%ptr instance) (make-mptr ,(mm-metaclass-tag class) index))
       ,@(loop
           for s in (class-slots class)
           unless (slot-definition-memory-mapped s)
           when (slot-definition-initfunction s)
           collect `(setf
                      (slot-value instance ',(slot-definition-name s))
                      (funcall ,(slot-definition-initfunction s))))
       instance)))


(defun slot-definition-initform-mm-zerop (slotd)
  (cond
    ((not (slot-definition-initfunction slotd)))
    ((constantp (slot-definition-initform slotd))
	 (multiple-value-bind (val failed) (ignore-errors (eval (slot-definition-initform slotd)))
	   (unless failed
             (cond
               ((slot-definition-mm-boxing slotd)            (eq nil val))
               ((slot-definition-mm-write-convertor slotd)   nil)
               ((numberp val)                                (= val 0))))))))


(defun metaclass-allocator-form (class)
  "Returns a lambda-form that allocates a new object, and sets all
  memory mapped slots to their default values unless they are going to
  be overridden by the initargs"
  `(lambda (instance initargs)
     (declare (ignorable initargs))
     (setf (%ptr instance)
       (make-mptr ,(mm-metaclass-tag class)
         (mtagmap-alloc (mtagmap ,(mm-metaclass-tag class)) ,(mm-metaclass-len class))))
     ,@(let* ((slots    (loop for s in (class-slots class)
                          when (and (slot-definition-memory-mapped s) 
                                 (not (slot-definition-initform-mm-zerop s))
                                 (slot-definition-initargs s)) collect s))
               (gensyms (loop for s in slots
                          collect (gensym (princ-to-string (slot-definition-name s)))))
               (params  (remove-duplicates (loop for s in slots
                                             appending (slot-definition-initargs s))))
               (cases   (loop for p in params
                          collect `(,p ,@(loop
                                           for s in slots 
                                           for g in gensyms
                                           when (member p (slot-definition-initargs s))
                                           collect `(setf ,g t))))))
         (when slots
           `((let ,gensyms (loop for arg in initargs by #'cddr do (case arg ,@cases))
               ,@(loop for s in slots for g in gensyms
                   collect `(unless ,g (funcall (the mm-slot-definition-writer
                                                  ,(slot-definition-writer-function s)) 
                                         (funcall ,(slot-definition-initfunction s))
                                         instance)))))))
     instance))


(defun setup-default-metaclass-functions (class)
  (loop for slot in (class-slots class)
    do (when (slot-definition-memory-mapped slot) (mm-effective-slot-definition-setup slot)))  
  (flet ((maybe-compile (form)
	   (when form (compile nil form))))
    (with-slots (default-walker default-instantiator allocator) class
      (setf default-walker        (maybe-compile (metaclass-default-walker-form class)))
      (setf default-instantiator  (compile nil (metaclass-default-instantiator-form class)))
      (setf allocator             (compile nil (metaclass-allocator-form class))))))


(defun mm-metaclass-filename (class)
  (assert (class-name class) (class) "Cannot mmap anonymous classes.") ; possible but sensible??
  (check-type (class-name class) symbol) 
  (make-pathname :name (flet ((clean (str)
                                (remove-if-not #'alphanumericp str)))
                         (let ((name (class-name class)))
                           (concatenate 'string
                             (clean (package-name (symbol-package name))) "-"
                             (clean (symbol-name name)))))))


(defun mm-metaclass-pathname (class)
  (merge-pathnames (mm-metaclass-filename class) *mmap-pathname-defaults*))


(declaim (ftype (function (mm-metaclass &optional mindex) mptr) mm-metaclass-alloc))

(defun mm-metaclass-alloc (class &optional (amount 1))
  (declare (type mindex amount))
  (make-mptr (mm-metaclass-tag class) (mtagmap-alloc (mm-metaclass-mtagmap class) 
                                        (* amount (mm-metaclass-len class)))))


(defun mm-metaclass-custom-function (class slot &optional (default-slot
                                                            (let ((*package* #.*package*))
                                                              (alexandria:symbolicate
                                                                'default- slot))))
  (typecase (slot-value class slot)
    (null     (slot-value class default-slot))
    (list     (let ((f (first (slot-value class slot)))) 
                (or (ignore-errors (alexandria:ensure-function f)) f)))))


(defun setup-mtagmap-for-metaclass (class)
  (when (zerop (mm-metaclass-len class))
    (warn "Pointlessly memory mapping a class with zero length objects: ~A" class))
  (with-slots (tag mtagmap) class
    (unless tag
      (let ((existing (loop for m across *mtagmaps*
                        for a from 0 thereis (when (and m (equalp (class-name class)
                                                            (class-name (mtagmap-class m))))
                                               a))))
	(setf tag (or existing (next-available-tag)))
	(assert tag (*mtagmaps*) "No more tags available. Too many types defined in datastore")))
    (unless (mtagmap tag)
      (setf (mtagmap tag) (make-mtagmap))
      (setf (mtagmap-layout (mtagmap tag)) (mm-metaclass-slot-layout class)))
    (assert-class-slot-layout class (mtagmap-layout (mtagmap tag)) :finalize nil)
    (setf mtagmap (mtagmap tag)) 
    (setf (mtagmap-class mtagmap) class))  
  class)


(defun mm-metaclass-initialize-alloc (class instance initargs)
  (declare  (type mm-metaclass class))
  (funcall (the function (slot-value class 'allocator)) instance initargs))


(defmethod initialize-instance :before ((instance mm-object) &rest initargs)
  (let ((class (class-of instance)))
    ;;(log:info "~S ~S ~S" class instance initargs)
    ;;(log:info "this is allocate-instance: ~S"
    (mm-metaclass-initialize-alloc class instance initargs)))


  ;; (allocate-instance class initargs))
  ;;   (let ((it 
  ;;     (ignore-errors 
  ;;     (describe it)
  ;;     (log:sexp it))
  ;;     it)))
;; 

(defun always-true (&rest args)
  (declare (ignore args))
  t)


(defun slot-definition-mm-type (slotd)
  (if (stored-cffi-type (slot-definition-type slotd))
    (slot-definition-type slotd)
    'mm-box))


(defun slot-definition-mm-boxing (slotd)
  (eq (slot-definition-mm-type slotd) 'mm-box))


(defun slot-definition-mm-read-convertor (slotd)
  (cond
    ((slot-definition-mm-boxing slotd)    'mptr-to-lisp-object)))


(defun slot-definition-mm-write-convertor (slotd)
  (cond
    ((slot-definition-mm-boxing slotd)	 'lisp-object-to-mptr)))


(defun slot-definition-mm-read-form (slotd raw-access-form)
  (let ((c (slot-definition-mm-read-convertor slotd)))
    (if c
      `(,c ,raw-access-form)
      raw-access-form)))

(defun slot-definition-mm-write-form (slotd raw-write-form new-val-sym)
  (let ((c (slot-definition-mm-write-convertor slotd)))
   (cond (c  `(let ((,new-val-sym (,c ,new-val-sym))) 
                ,raw-write-form))
	 (t  raw-write-form))))

;; note above that (lisp-object-to-mptr new-val) can invalidate the current pointer


(defun mm-effective-slot-definition-lambda-forms (slotd)
  (let* ((offset (slot-value slotd 'offset))
          (type (slot-definition-mm-type slotd))
          (raw-access-form `(d ,(if (zerop offset)
                                  `(mm-object-pointer object) 
                                  `(cffi:inc-pointer (mm-object-pointer object) ,offset))
                              0 ,(if (eq type 'mm-box) 'mptr type)))
          (read-form  (slot-definition-mm-read-form slotd raw-access-form)))
    (values
      `(lambda (object)
         ,read-form)
      `(lambda (new-val object)
         ,(slot-definition-mm-write-form slotd `(setf ,raw-access-form new-val) 'new-val)
         new-val))))


(defun mm-effective-slot-definition-setup (slotd)
  (with-slots (offset) slotd
    (check-type offset mindex)
    (multiple-value-bind (reader writer) (mm-effective-slot-definition-lambda-forms slotd)
      (setf (slot-definition-reader-function slotd)  (compile nil reader)
	    (slot-definition-writer-function slotd)  (compile nil writer)))
    (values)))


(defun mm-slot-offset (class slotname)
  (let* ((class (force-class class))
          (slotd (find slotname (class-slots class) :key #'slot-definition-name)))
    (assert slotd)
    (assert (slot-definition-memory-mapped slotd))
    (slot-value slotd 'offset)))


(defmacro with-raw-slot ((slotname classname &key (accessor-name slotname)) 
			 object-pointer &body body &environment env)
  (let ((class (find-class classname t env)))
    (ensure-finalize-inheritance class)
    (let* ((slotd (or (find slotname (class-slots class) :key #'slot-definition-name) 
		      (error "Class ~A has no slot ~A" classname slotname)))
	   (offset (slot-value slotd 'offset))
	   (slot-type (slot-definition-type slotd))
	   (d-slot-type (if (stored-cffi-type slot-type) slot-type 'mptr)))
      (alexandria:with-gensyms (apointer)
	`(let ((,apointer (cffi:inc-pointer ,object-pointer ,offset)))
	   (declare (type machine-pointer ,apointer))
	   (symbol-macrolet ((,accessor-name
			      (d ,apointer 0 ,d-slot-type)))
	     ,@body))))))


(defmacro with-pointer-slots (slotnames (object-pointer classname) &body body)
  (alexandria:once-only (object-pointer)
    (labels ((r (slotnames)
	       (if slotnames
                 `(with-raw-slot (,(first slotnames) ,classname) ,object-pointer
		      ,(r (rest slotnames)))
                 `(locally ,@body))))
      (r slotnames))))


(defun mm-metaclass-slot-layout (class)
  (ensure-finalize-inheritance class)
  (let ((slots (class-slots class)))
    (loop
      for s in slots 
      when (slot-definition-memory-mapped s)
      collect `(,(slot-definition-name s)
                 ,(slot-value s 'offset)
                 ,(stored-type-size (slot-definition-type s))
                 ,@(when (slot-definition-mmap-pointer-p s)
                     `(:mmap-pointer t))))))


(defun layout-compatible-p (a b)
  (flet ((sort-layout (layout)
	   (sort (copy-list layout) #'> :key #'second)))
    (equalp 
     (mapcar #'rest (sort-layout a)) 
     (mapcar #'rest (sort-layout b)))))


(defun ensure-finalize-inheritance (class)
  (let ((class (force-class class)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))))


(defun assert-class-slot-layout (class layout &key (finalize t))
  (when finalize  (ensure-finalize-inheritance class))
  (cassert (layout-compatible-p layout (mm-metaclass-slot-layout class)) ()
    "Layout for class ~A has changed from ~A" class layout))


(defmacro check-class-slot-layout (classname &optional (layout (mm-metaclass-slot-layout
                                                                 (find-class classname))))
  `(assert-class-slot-layout (find-class ',classname) ',layout))


(defmacro defmmclass (name direct-supers direct-slots &rest options)
  "Define a memory mapped class, like defclass. Automatically adds :metaclass mm-metaclass
  to options, if it is not present, finalizes the class immediately, and puts in an assertion
  that the class layout in the loaded datastore is compatible."
  `(progn
     (eval-when (:load-toplevel :execute :compile-toplevel) 
       (defclass ,name ,direct-supers ,direct-slots 
	 ,@(if (assoc :metaclass options) 
	       options
	       `((:metaclass mm-metaclass) ,@options)))
       (ensure-finalize-inheritance ',name))
     (eval-when (:execute) (check-class-slot-layout ,name))
     (find-class ',name)))


(defun tree-to-atoms-or-strings (tree)
  (typecase tree
    (integer tree)
    (null    tree)
    (list    (loop for i in tree collect (tree-to-atoms-or-strings i)))
    (t       (princ-to-string tree))))


(defun mm-metaclass-schema (class)
  (with-standard-io-syntax
    (tree-to-atoms-or-strings
     (list
      (mm-metaclass-filename class)
      (mm-metaclass-tag class)
      (mm-metaclass-slot-layout class)))))


(defmacro with-cached-slots (slots instance &body body)
  "Like with-slots, but each slot is only read from the datastore once.
  It is written to the datastore immediately after every write, and the
  cached version becomes the value written (not the value as serialised
  and deserialised). This is an optimization to stop repeatedly instantiating 
  slots into Lisp memory. Note that it also useful because it preserves
  non-persistent slots of objects stored in SLOTS of INSTANCE over their
  lexical scope."
  (alexandria:with-unique-names (new-val)
    (let* ((tmps (loop for s in slots do (check-type s symbol) collect (gensym (symbol-name s))))
            (funcs (loop for tmp in tmps collect tmp collect `(setf ,tmp)))
            (ffuncs (loop for f in funcs collect `(function ,f))))
      (alexandria:once-only (instance)
        `(let ,(loop for tmp in tmps for s in slots
                 collect `(,tmp (slot-value ,instance ',s)))
           (flet ,(loop
                    for tmp in tmps
                    for s in slots
                    collect `(,tmp () ,tmp)
                    collect `((setf ,tmp) (,new-val) (setf ,tmp (setf (slot-value ,instance ',s)
                                                                  ,new-val))))
             (declare (ignorable ,@ffuncs))
             (symbol-macrolet ,(loop for s in slots for tmp in tmps
                                 collect `(,s (,tmp)))
               ,@body)))))))


;; NOTE eliminating  errors whwn printing class prototype on sbcl
;; by conditionalizing output on boundness check of slot

(defmethod print-object ((object mm-object) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object '%ptr) 
      (let ((ptr (ptr object)))
        (format stream " M@~D(~D:~D)" ptr (mptr-tag ptr) (mptr-index ptr)))
      (format stream " %unbound% "))))
