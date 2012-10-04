;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)


(defmacro defun-speedy (name lambda-list &body body &environment env)
  (declare (ignorable env))
  `(progn
     #+(or) (declaim (inline ,name)) 
     #+lispworks ,@(when env `((declaim (notinline ,name)))) 
     (defun ,name ,lambda-list
       #+(or) (declare (optimize speed))
       ,@body)))


(defun fc (class-designator)
  (typecase class-designator
    (class    class-designator)  
    (keyword (fc (string class-designator)))
    (string  (fc (read-from-string class-designator)))
    (symbol  (find-class class-designator))
    (t       (find-class class-designator))))

;; (fc 'standard-class)
;; (fc :standard-class)
;; (fc 'cl:standard-class)
;; (fc "standard-class")
;; (fc ":standard-class")
;; (fc "cl:standard-class")
;; (fc (fc 'standard-class))


(defun force-class (class-specifier)
  (fc class-specifier))


(defmacro cassert (test-form &optional places string &rest args)
  (declare (ignore places))
  `(unless ,test-form
     (cerror "Ignore the assertion"
       ,(or string (format nil "Assertion ~S failed" test-form)) ,@args)))


(define-symbol-macro ? (describe *))


#+swank 
(defun ^ (thing &optional wait)
  (swank:inspect-in-emacs thing :wait wait))

#+swank
(define-symbol-macro ^* (^ *))

#+swank
(define-symbol-macro ^** (^ **))

#+swank
(define-symbol-macro ^*** (^ ***))


(defun finalize (class-designator)
  (finalize-inheritance (fc class-designator))
  (fc class-designator))

(defun new (&rest args)
  (apply #'make-instance args))
