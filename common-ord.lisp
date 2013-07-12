;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :order
  (:nicknames :ord)
  (:documentation "")
  (:use :closer-common-lisp :closer-mop :contextl :alexandria)
  (:export
    :compare
    :compare|>|
    :compare|<|
    :compare|<=|
    :compare|=|
    :compare|>=|
    :slots-to-compare
    :writing-readably
    :<=>
    :hash
    :order-layer
    :default-order))

(in-package :order)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Universal Ordinality / Equivalence Predicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-function hash (thing)
  (:documentation "Universal hash operator"))

(define-layered-function compare (a b)
  (:documentation "0 => equivalence; 1 => (a > b); -1 => (a < b)"))

(defun <=> (a b)
  "apply order:compare"
  (compare a b))

(defun :<=> (a b)
  "apply order:compare"
  (compare a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Standard Context: Layers Representing Unspecialized Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer order-layer)

(deflayer default-order (order-layer) ()
  (:documentation "Everything here and below is effectively in layer '(T)"))

(ensure-active-layer 'default-order)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Common-lisp Hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-method hash :in t ((thing t))
  (sxhash thing))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-layered-method compare :in t ((a null) (b null))
  "ordinal comparison of null values is always equal"
  0)

(define-layered-method compare :in t ((a number) (b number))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))

(define-layered-method compare :in t ((a real) (b real))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))

(define-layered-method compare :in t ((a character) (b character))
  (cond
    ((char= a b)  0)
    ((char< a b) -1)
    (t            1)))

(define-layered-method compare :in t ((a string) (b string))
  (cond
    ((string= a b)  0)
    ((string< a b) -1)
    (t              1)))

(define-layered-method compare :in t ((a hash-table) (b hash-table))
  (cond
    ((string=  (princ-to-string a) (princ-to-string b))  0)
    ((string<  (princ-to-string a) (princ-to-string b)) -1)
    (t                                                   1)))

(define-layered-method compare :in t ((a symbol) (b symbol))
  (let ((pkgcmp (compare (symbol-package a) (symbol-package b))))
    (if (zerop pkgcmp)
      (compare (symbol-name a) (symbol-name b))
      pkgcmp)))

(define-layered-method compare :in t ((a pathname) (b pathname))
  (compare (namestring a) (namestring b)))

(define-layered-method compare :in t ((a package) (b package))
  (compare (package-name a) (package-name b)))

(define-layered-method compare :in t ((a local-time:timestamp) (b local-time:timestamp))
  (cond
    ((local-time:timestamp= a b)  0)
    ((local-time:timestamp< a b) -1)
    (t                            1)))

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

(define-layered-method compare :in t ((a t) (b t))
  (if (and
        (subtypep (type-of a) (type-of b))
        (subtypep (type-of b) (type-of a)))
    (ord:compare (sxhash a) (sxhash b))
    (writing-readably 
      (ord:compare
        (format nil "~S" (type-of a))
        (format nil "~S" (type-of b))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric slots-to-compare-using-class (class object)
  (:documentation ""))

(defmethod  slots-to-compare-using-class ((class standard-class) object)
  "comparible slots may be cutomized by class, with the default being all slots"
  (mapcar #'c2mop:slot-definition-name 
    (c2mop:class-slots (class-of object))))

(defgeneric slots-to-compare (object)
  (:documentation ""))

(defmethod  slots-to-compare ((object standard-object))
  "comparible slots of a standard-object are defined by specialization on it class"
  (slots-to-compare-using-class (class-of (class-of object)) object))

(define-layered-method compare :in t ((a standard-object) (b standard-object))
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
        (compare 
          (format nil "~S" (class-name (class-of a)))
          (format nil "~S" (class-name (class-of b)))))
      (let ((slots (slots-to-compare a)))
        (when (null slots)
          (return-from compare
            (writing-readably 
              (compare (format nil "~S" a) (format nil "~S" b)))))
        (loop :for x :in slots
          :do (cond
                ((and (not (slot-boundp a x)) (not (slot-boundp b x)))  nil)            
                ((not (slot-boundp a x)) (return-from compare   1))
                ((not (slot-boundp b x)) (return-from compare  -1))
                (t
                  (let ((c (compare (slot-value a x) (slot-value b x))))
                    (unless (zerop c)
                      (return-from compare c))))))
        0)))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare< (a b)
   (minusp (compare a b)))

(defun compare<= (a b)
   (not (plusp (compare a b))))

(defun compare= (a b)
   (zerop (compare a b)))

(defun compare>= (a b)
   (not (minusp (compare a b))))

(defun compare> (a b)
   (plusp (compare a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-Specific Specializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; REVERSED-ORDER
;;

(deflayer reversed-order (order-layer) ()
  (:documentation "A simple specialization for contexts in which you
  want the exact opposite of the default order, such as from gratest
  to least.  The following example demonstrates how to acheive this:
  ;;;
  ;;;   (WITH-ACTIVE-LAYERS (DEFAULT-ORDER)
  ;;;     (COMPARE :A :Z)) => -1
  ;;;
  ;;;   (WITH-ACTIVE-LAYERS (REVERSED-ORDER)
  ;;;     (COMPARE :A :Z)) =>  1
  ;;;"))

(define-layered-method compare :in reversed-order :around ((a t) (b t))
  (- (call-next-layered-method)))

;;
;; CASE-INSENSITIVE
;;

(deflayer case-insensitive (order-layer))

(define-layered-method hash :in case-insensitive ((thing t))
  "It is usually a good idea to coordinate hash with compare so that they
   agree as closely as possible.  PSXHASH is a case-insensitive hash function."
  (sb-ext::psxhash thing))
  
(define-layered-method compare :in case-insensitive ((a string) (b string))
  (call-next-layered-method (string-upcase a) (string-upcase b)))

(define-layered-method compare :in case-insensitive ((a character) (b character))
  (call-next-layered-method (char-upcase a) (char-upcase b)))

;;
;; TRUENAME-ORDER
;;

(deflayer truename-order (order-layer))

(define-layered-method hash :in truename-order ((thing pathname))
  (call-next-layered-method (truename thing)))

(define-layered-method compare :in truename-order ((a pathname) (b pathname))
  (call-next-layered-method (truename a) (truename b)))




