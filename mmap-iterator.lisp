;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)


(defmacro doclass ((var class-specifier &key fresh-instances reverse) &body body)
  "For each object in the memory-mapped datastore of class denoted by
  CLASS-SPECIFIER (evaluated), lexically bind VAR to a Lisp object
  representing that object around BODY and execute it.

  FRESH-INSTANCES (generalized boolean, not evaluated), if true means
  means that a fresh Lisp object will be created for each datastore
  object -- by default a single Lisp object is instantiated and it is
  modified destructively to point to each object in the class.

  REVERSE (generalized boolean, not evaluated), if true means that
  objects will be iterated in order from newest to oldest. If false (default),
  they are iterated from oldest to newest."
  (alexandria:with-unique-names (tag class mtagmap last-index first-index
                                  instantiator len index)
    `(let* ((,class (force-class ,class-specifier))
             (,tag (mm-metaclass-tag ,class))
             (,len (mm-metaclass-len ,class)))
       (declare (type mindex ,len))
       (when ,tag   ; if finalize-inheritance has not yet been called
	 (let ((,mtagmap (mtagmap ,tag)))
	   (unless (mtagmap-closed-p ,mtagmap)
             (let* ((,instantiator (mtagmap-instantiator ,mtagmap))
		     (,last-index (mtagmap-last-index ,mtagmap))
		     (,first-index (mtagmap-first-index ,mtagmap)))
               (declare (type mindex ,last-index ,first-index))
               (when (> ,last-index ,first-index)
                 (decf ,last-index ,len)
                 (let ((,index ,(if reverse `,last-index `,first-index)))
                   (loop ,(if fresh-instances `for `with) ,var = (funcall ,instantiator ,index) 
                     do (let ,(when fresh-instances `((,var ,var))) ,@body)
                     (when (= ,index ,(if reverse `,first-index `,last-index))
                       (return))
                     (,(if reverse `decf `incf) ,index ,len)
                     ,@(unless fresh-instances
                         `((setf (%ptr ,var) (make-mptr ,tag ,index))))))))))))))
  

(defun mm-subclasses (class)
  (remove-duplicates (list* class (loop
                                    for c in (class-direct-subclasses class)
                                    when (typep class 'mm-metaclass)
                                    appending (mm-subclasses c)))))


(defmacro dosubclasses ((var class-specifier &rest options) &body body)
  "For the class itself and each subclass of the class denoted by
 CLASS-SPECIFIER (evaluated) run doclass."
  (alexandria:with-unique-names (one-class class)
    `(flet ((,one-class (,class)
              (doclass (,var ,class ,@options)
                ,@body)))
       (loop for ,class in (mm-subclasses (force-class ,class-specifier))
         do (,one-class ,class)))))


(defun retrieve-all-instances (class &aux ret)
  "Returns a list of all instances of CLASS."
  (dosubclasses (p class :fresh-instances t)
    (push p ret))
  ret)


(defun count-all-instances (class)
  "Return a count of the number of instances of the class denoted by
  CLASS and any subclasses of it."
  (loop
    for     c in (mm-subclasses (force-class class))
    for     m = (mm-metaclass-mtagmap c)
    summing (if (mtagmap-closed-p m) 0 (mtagmap-count m))))

