;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-mmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MM-BOX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-constant-tag-for-class (tag mm-box)
  (defun unbox-box (index)
    (with-pointer-slots (ptr) ((mpointer tag index) mm-box)
      (mptr-to-lisp-object ptr))))


(with-constant-tag-for-class (element-tag mm-box) 
  (defun make-marray (length &key (initial-element nil initial-element-p) (marray-class 'marray) 
                       (initial-contents nil initial-contents-p)) 
    "Create a new marray (memory-mapped array) structure in the
    datastore, similarly to make-array."
    (let ((marray (make-instance marray-class :length length 
                    :base (make-mptr element-tag (mtagmap-alloc (mtagmap element-tag) 
                                                   (* length #.(stored-type-size 'mptr)))))))
      (symbol-macrolet ()
	(cond
          (initial-contents-p (let ((initial-contents (mapcar #'lisp-object-to-mptr
                                                        initial-contents))
                                     (ptr (mptr-pointer (marray-base marray))))
                                (loop
                                  for i below length 
                                  for n in initial-contents
                                  do (setf (dw ptr i) n))))
          (initial-element-p  (let ((initial-element (lisp-object-to-mptr initial-element))
                                     (ptr (mptr-pointer (marray-base marray))))
                                (loop for i below length
                                  do (setf (dw ptr i) initial-element))))))
      marray)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MM-CONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box-cons (cons)
  (cond
    ((consp (cdr cons))	 (let* ((new (cons (car cons) nil))
                                 (new-tail new)
                                 (len 2))
                           (declare (type fixnum len))
                           (loop
                             for x = (cdr cons) then (cdr x)
                             while (consp x)
                             do (setf new-tail (setf (cdr new-tail) (cons (car x) nil))) (incf len)
                             finally (setf (cdr new-tail) (cons x nil)))
                           (ptr (make-marray len
                                  :initial-contents new
                                  :marray-class 'mm-array-as-list))))
    (t                     (ptr (make-instance 'mm-cons
                                  :car (car cons)
                                  :cdr (cdr cons))))))


(with-constant-tag-for-class (tag mm-cons)
  (check-class-slot-layout mm-cons)
  (defun unbox-cons (index)
    (with-pointer-slots (a b) ((mpointer tag index) mm-cons)
      (cons (mptr-to-lisp-object a) (mptr-to-lisp-object b)))))


(with-constant-tag-for-class (tag mm-array-as-list)
  (defun unbox-array-as-list (index)
    (with-pointer-slots (base length) ((mpointer tag index) mm-array-as-list)
      (let ((base base) 
             (length length))
        (flet ((elem (n)
                 (mptr-to-lisp-object (dw (mptr-pointer base) n))))
          (let* ((cons (cons (elem 0) nil))
                  (tail cons))
            (loop
              for i from 1 below (1- length)
              do (setf tail (setf (cdr tail) (cons (elem i) nil)))
              finally (setf (cdr tail) (elem (1- length))))
            cons))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MM-SYMBOL
;;
;; Symbol handling has been revised so as to handle uninterned symbols, which
;; previously caused painful bugs and led to eventual crash. Specifically,
;; uninterned symbols can be stored persistently, will retain identity through
;; a round-trip to/from persistent storage, and will be comsidered to be uniquely
;; represented based on #'symbol-name.  That is, gensyms with the same name will
;; always resolve to the same mm-symbol.  This prevents the undesirable alternative
;; problems caused by the default gensym semantics which results in the same gensym
;; persisted again and again each time it is encountered when identity had not
;; been preserved.  For all intents and purposes, this rendered uninterned symbols
;; entirely unusable and quite toxic to the operation of the system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :|| (:use))

(defvar *stored-symbols* nil)

(defmacro prop-for-mm-symbol (sym)
  `(get ,sym 'mm-symbol))


(with-constant-tag-for-class (tag mm-symbol)
  (check-class-slot-layout mm-symbol)
  (declaim (ftype (function (symbol) (mptr)) uncached-box-symbol box-symbol))

  (defun box-symbol-miss (object)
    (declare (type symbol object))
    (let* ((pkg (symbol-package object)) 
            (mptr (ptr (make-instance 'mm-symbol
                         :package (if pkg (package-name pkg) "")
                         :symbol  (symbol-name object)))))
      (assert (not (zerop mptr)))
      (prog1 mptr
        (if pkg
          (progn
            (push object *stored-symbols*)
            (setf (prop-for-mm-symbol object) mptr))
          (let ((altsym (intern (symbol-name object) :||)))
            (setf (prop-for-mm-symbol altsym) mptr)
            (setf (get altsym :uninterned) object)
            (pushnew altsym *stored-symbols* :test #'eq))))))
   
  (defun box-symbol (object)
    (declare (type symbol object))
    (cond
      ((not object)                  (make-mptr tag 0))
      ((not (symbol-package object)) (or
                                       (prop-for-mm-symbol (intern (symbol-name object) :||))
                                       (box-symbol-miss object)))
      (t                             (or
                                       (prop-for-mm-symbol object)
                                       (box-symbol-miss object)))))
  
  (defun unbox-symbol (index)
    (unless (zerop index)
      (with-pointer-slots (package-name symbol-name) ((mpointer tag index) mm-symbol)
        (let ((package-name (mptr-to-lisp-object package-name))
               (symbol-name (mptr-to-lisp-object symbol-name)))
          (let ((sym (intern symbol-name (find-package package-name))))
            (pushnew sym *stored-symbols* :test #'eq)
            (setf (prop-for-mm-symbol sym) (make-mptr tag index))
            (if (equal package-name "")
              (or (get sym :uninterned)
                (setf (get sym :uninterned) (make-symbol symbol-name)))
              sym)))))))
  
           
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MM-ARRAY & MM-STRING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tag-general-unbox-array (tag index)
  (with-pointer-slots (length base) ((mpointer tag index) mm-array)
    (unbox-array-internal (mptr-tag base) (mptr-index base) length)))

(with-constant-tag-for-class (tag mm-array)
  (defun unbox-array (index)
    (tag-general-unbox-array tag index)))

(with-constant-tag-for-class (tag mm-string)
  (defun unbox-string (index)
    (cl-irregsexp.bytestrings:force-string  
     (cl-irregsexp.bytestrings:force-byte-vector 
      (tag-general-unbox-array tag index)))))

(define-box-array internal-box-string boxed-byte (unsigned-byte 8) :array-class mm-string)

(defun box-string (string)
  (internal-box-string (cl-irregsexp.bytestrings:force-simple-byte-vector string)))

(defun walk-array (mptr func)
  (macrolet ((base-offset ()
	       (ash (mm-slot-offset 'mm-array 'base) +mtag-bits+)))
    (with-pointer-slots (length base) ((mptr-pointer mptr) mm-array)
      (let ((length length))
        (unless (zerop length)
          (funcall func base (+ mptr (base-offset)) length))))))



;;; XXXX these things are really awful and should be redone much more nicely
(defmacro direct-slot-mptr (class object slot)
  `(with-pointer-slots (,slot) ((mm-object-pointer ,object) ,class)
     ,slot))

(defmacro set-direct-slot-mptr (class object slot new-value)
  `(with-pointer-slots (,slot) ((mm-object-pointer ,object) ,class)
     (setf ,slot ,new-value)))

(defsetf direct-slot-mptr set-direct-slot-mptr)

(defmacro direct-slot-numeric-maref (class object slot element-type index)
  "Access element INDEX of an array of ELEMENT-TYPE that is stored in
  slot SLOT of OBJECT, which is an instance of class CLASS, without
  instantiating the array into the memory of the host Lisp
  implementation."
  `(with-pointer-slots (base) ((mptr-pointer (direct-slot-mptr ,class ,object ,slot)) marray)
     (d (mptr-pointer base) ,index ,element-type)))

(defmacro set-direct-slot-numeric-maref (class object slot element-type index new-value )
  `(with-pointer-slots (base) ((mptr-pointer (direct-slot-mptr ,class ,object ,slot)) marray)
     (setf (d (mptr-pointer base) ,index ,element-type) ,new-value)))

(defsetf direct-slot-numeric-maref set-direct-slot-numeric-maref)


(defun meq (a b)
  "True iff either (eq a b) or A and B are both datastore objects
  representing the same object in the datastore."
  (or (eq a b) 
    (and
      (typep a 'mm-object)
      (typep b 'mm-object)
      (= (ptr a) (ptr b)))))
