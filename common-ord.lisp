;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :ord
  (:documentation "")
  (:use :common-lisp :closer-mop)
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:export
    :compare|>|
    :compare|<|
    :compare|<=|
    :compare|=|
    :compare|>=|
    :compare
    :make-ci-char
    :make-ci-string
    :slots-to-compare
    :writing-readably
    :of-type
    :proper-list
    :proper-list-p
    :association-list
    :association-list-p))

(in-package :ord)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard formatting for lexical comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro writing-readably (&rest forms)
  "Macro to wrap around some forms, causing their writing to be more suitable for
   lexical comparison."
  `(let ((*print-escape*  t)
          (*print-level*  nil)
          (*print-length* nil)
          (*print-array*  t)
          (*package*     (find-package :keyword)))     
     ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boxed comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ci-char   c)
(defstruct ci-string s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric compare (a b)
  (:documentation ""))


(defmethod compare ((a null) (b null))
  "ordinal comparison of null values is always equal"
  0)


(defmethod  compare ((a t) (b t))
  (if (eq (type-of a) (type-of b))
    (ord:compare (sxhash a) (sxhash b))
    (writing-readably 
      (ord:compare
        (format nil "~S" (type-of a))
        (format nil "~S" (type-of b))))))


(defmethod  compare ((a number) (b number))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))


(defmethod  compare ((a real) (b real))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))


(defmethod  compare ((a character) (b character))
  (cond
    ((char= a b)  0)
    ((char< a b) -1)
    (t 1)))


(defmethod  compare ((a string) (b string))
  (cond
    ((string= a b)  0)
    ((string< a b) -1)
    (t 1)))


(defmethod  compare ((a hash-table) (b hash-table))
  (cond
    ((string=  (princ-to-string a) (princ-to-string b))  0)
    ((string<  (princ-to-string a) (princ-to-string b)) -1)
    (t 1)))


(defmethod  compare ((a symbol) (b symbol))
  (let ((pkgcmp (compare (symbol-package a) (symbol-package b))))
    (if (zerop pkgcmp)
      (compare (symbol-name a) (symbol-name b))
      pkgcmp)))


(defmethod  compare ((a pathname) (b pathname))
  (compare (namestring a) (namestring b)))


(defmethod  compare ((a package) (b package))
  (compare (package-name a) (package-name b)))


(defmethod  compare ((a ci-char) (b ci-char))
  (cond
    ((char-equal (ci-char-c a) (ci-char-c b))  0)
    ((char-lessp (ci-char-c a) (ci-char-c b)) -1)
    (t 1)))


(defmethod  compare ((a ci-string) (b ci-string))
  (cond
    ((string-equal (ci-string-s a) (ci-string-s b))  0)
    ((string-lessp (ci-string-s a) (ci-string-s b)) -1)
    (t 1)))


(defmethod  compare ((a local-time:timestamp) (b local-time:timestamp))
  (cond
    ((local-time:timestamp= a b)  0)
    ((local-time:timestamp< a b) -1)
    (t 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric slots-to-compare-using-class (class object)
  (:documentation ""))


(defmethod  slots-to-compare-using-class ((class standard-class) object)
  "comparible slots may be cutomized by class, with the default being all slots"
  (mapcar #'c2mop:slot-definition-name 
    (c2mop:class-slots object)))


(defgeneric slots-to-compare (object)
  (:documentation ""))


(defmethod  slots-to-compare ((object standard-object))
  "comparible slots of a standard-object are defined by specialization on it class"
  (slots-to-compare-using-class (class-of object) object))


(defmethod ord:compare ((a standard-object) (b standard-object))
  "ordinal comparison of arbitrary standard-objects performed as follows:
   -- objects of different classes ordered by lexical comparison of class name
   -- objects of a class for which slots-to-compare returns null are ordered by lexical
       comparison of printed representation.  For standard print-unreadable-object output,
       this achieves equality on the objects being #'eq, otherwise returns a consistent
       but arbitrary ordinal comparison value for the lifetime of these specific instances.
       Customized print-unreadable-object representations also provides a simple means
       of adjustment to the resulting comparison.
   -- objects of identical class are compared based on the boundness and slot-value of
       the slots-names in list returned by slots-to-compare.  Slots unbound in both
       obects are considered equal.  Unbound slots are considered greater than bound slots of the
       same slot-name. Two bound slots-values with the same slot-name are compared recursively
       with ord:compare.
   -- when all preceding steps complete without ordinal determination, the objects are
       considered equal"
  (if (not (eq (class-of a) (class-of b)))
    (writing-readably 
      (ord:compare
        (format nil "~S" (class-name (class-of a)))
        (format nil "~S" (class-name (class-of b)))))
    (let ((slots (slots-to-compare a)))
      (when (null slots)
        (return-from compare
          (writing-readably 
            (ord:compare (format nil "~S" a) (format nil "~S" b)))))
      (loop
        :for x :in slots
        :do (cond
              ((and (not (slot-boundp a x)) (not (slot-boundp b x)))  nil)            
              ((not (slot-boundp a x)) (return-from compare   1))
              ((not (slot-boundp b x)) (return-from compare  -1))
              (t
                (let ((c (ord:compare (slot-value a x) (slot-value b x))))
                  (unless (zerop c)
                    (return-from compare c))))))
      0)))
    

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare|<| (a b)
   (minusp (compare a b)))

(defun compare|<=| (a b)
   (not (plusp (compare a b))))

(defun compare|=| (a b)
   (zerop (compare a b)))

(defun compare|>=| (a b)
   (not (minusp (compare a b))))

(defun compare|>| (a b)
   (plusp (compare a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are adopted from Alexandria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype proper-list ()
  "Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expected type in a TYPE-ERROR."
  `(and list (satisfies proper-list-p)))

;; (proper-list-p (cons t t)) => nil


(defun alist-elt-p (elt)
  (and (consp elt)
       (atom (car elt))
       (atom (cdr elt))))


(defun alistp (list)
  (when (listp list)
    (dolist (elt list)
      (unless (alist-elt-p elt)
        (return-from alistp nil)))
    t))


(defun association-list-p (var)
  "Returns true if OBJECT is an association list. Examples:
    (association-list-p 1) => NIL
    (association-list-p '(1 2 3)) => NIL
    (association-list-p nil) => T
    (association-list-p '((foo))) => T
    (association-list-p '((:a . 1) (:b . 2))) => T"
  (alistp var))


(deftype association-list ()
  `(satisfies alistp))


(defun of-type (type)
  "Returns a function of one argument, which returns true when its argument is
   of TYPE."
  (lambda (thing) (typep thing type)))
