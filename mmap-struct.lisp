;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)

(defun compute-default-location-for-store ()
  (ensure-directories-exist
    (merge-pathnames #p".mm/" (user-homedir-pathname)) :verbose t))


(defvar *mmap-pathname-defaults*)                         
(defvar *mmap-base-pathname*)
(defvar *mmap-sharing*         osicat-posix:MAP-SHARED)
(defvar *mmap-protection*      (logior osicat-posix:PROT-READ osicat-posix:PROT-WRITE))

(defvar *mmap-may-allocate* t
  "If this is not true, and an attempt is made to extend a memory
  mapped region, an error will be signalled.")

(setf (documentation '*mmap-base-pathname* 'variable)
  "The base path in which the datastore files are to be found.")


(deftype mptr ()
  "A representation of a location in the memory mapped datastore.
  Remains constant across remappings of the memory mapped regions to
  different offsets in physical memory."
  `(unsigned-byte ,+mptr-bits+))


(deftype mtag ()
  `(unsigned-byte ,+mtag-bits+))


(deftype mindex ()
  `(unsigned-byte ,+mindex-bits+))


(deftype machine-pointer ()
  (type-of (cffi:null-pointer))
  #+allegro (progn `(unsigned-byte 32) #+64bit `(unsigned-byte 64)))


(defun stored-cffi-type (type)
  (let ((cffi-type 
	 (alexandria:switch (type :test 'subtypep)
	   ('(unsigned-byte 8) :unsigned-char)
	   ('(unsigned-byte 64) :unsigned-long-long)
	   ('(signed-byte 64) :long-long)
	   ('single-float :float)
	   ('double-float :double))))
    cffi-type))


(defun stored-type-size (type)
  (cffi:foreign-type-size (or (stored-cffi-type type) (stored-cffi-type 'mptr))))


(defmacro d (machine-pointer &optional (index 0) (type '(unsigned-byte 8)))
  `(cffi:mem-aref ,machine-pointer ,(stored-cffi-type type) ,index))


(defmacro dw (machine-pointer &optional (index 0))
  `(d ,machine-pointer ,index mptr))


(defun mptr-tag (mptr)
  (declare (type mptr mptr))
  (the mtag (logand mptr (1- (ash 1 +mtag-bits+))))) ; Allegro  8.1 is too stupid to optimize ldb


(declaim (ftype (function (mptr) (mindex)) mptr-index))
(defun  mptr-index (mptr)
  (declare (type mptr mptr))
  (the mindex (ash mptr (- +mtag-bits+))))


(declaim (ftype (function (mtag mindex) mptr) make-mptr))
(defun make-mptr (tag index)
  (declare (type mtag tag) (type mindex index))
  (the mptr (logior (ash index +mtag-bits+) tag)))

(deftype mm-instantiator ()
  `(function (mindex) t) 
  #+allegro `function)   ;; Allegro 8.1  bug with function type specifiers and `the'


(deftype mm-walk-func ()
  `(function (mptr mptr mindex) t)
  #+allegro `function)   ;; Allegro 8.1  bug with function type specifiers and `the'
  

(declaim (inline mtagmap-ptr mtagmap-len mtagmap-elem-len))


(defstruct mtagmap  
  (fd -1 :type fixnum)
  (ptr (cffi:null-pointer) :type machine-pointer)
  (len 0 :type mindex)
  class
  layout
  instantiator
  walker
  (elem-len 0 :type mindex))


(deftype mtagmaps-array ()
  `(simple-array (or mtagmap null) (,+mtags+)))

(defvar *mtagmaps* (the mtagmaps-array (make-array +mtags+ :initial-element nil :element-type 
                                         '(or mtagmap null))))

(declaim (type mtagmaps-array *mtagmaps*))


(defun mtagmap (mtag)
  (declare (type mtag mtag))
  (aref (the mtagmaps-array *mtagmaps*) mtag))


(defun (setf mtagmap) (val mtag)
  (check-type mtag mtag)
  (check-type val (or null mtagmap))
  (setf (aref (the mtagmaps-array *mtagmaps*) mtag) val))


(defmacro mm-instantiator-for-tag (mtag)
  `(the mm-instantiator (mtagmap-instantiator (the mtagmap (mtagmap ,mtag)))))


(defun next-available-tag ()
  (loop for i from 0 thereis (unless (mtagmap i) i)))


(defun mpointer (mtag mindex)
  (declare (type mtag mtag) (type mindex mindex))
  (cffi:inc-pointer (mtagmap-ptr (the mtagmap (mtagmap mtag))) mindex))


(defun mptr-pointer (mptr)
  (mpointer (mptr-tag mptr) (mptr-index mptr)))


(defun mptr-to-lisp-object (mptr)
  "Deference the object at location MPTR in the memory mapped
  datastore and create a Lisp representation of it."
  (funcall (the mm-instantiator (mm-instantiator-for-tag (mptr-tag mptr))) 
	   (mptr-index mptr)))

