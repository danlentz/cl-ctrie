;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :macro
  (:use :cl)
  (:export :let1
    :defun/inline
    :once-only
    :defmacro/once
    :building-list
    :building-vector
    :with-thread
    :anaphoric
    :aprog1
    :awhen
    :it
    :atypecase
    :define-lazy-singleton
    :defun-dynamic
    :flet-dynamic
    :map-cut
    :cut
    :define-synonym))

(in-package :macro)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macrology originating from LMJ's excellent LPARALLEL http://www.lparallel.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro let1 (var value &body body)
    "Make a single `let' binding, heroically saving three columns."
    `(let ((,var ,value))
       ,@body))

  (defmacro defun/inline (name args &body body)
    "define a function automatically declared to be INLINE"
    `(progn
       (declaim (inline ,name))
       (defun ,name ,args ,@body)))

  (defmacro once-only-1 (var &body body)
    (let ((tmp (gensym (symbol-name var))))
      ``(let ((,',tmp ,,var))
          ,(let ((,var ',tmp))
             ,@body))))

  (defmacro once-only (vars &body body)
    (if vars
      `(once-only-1 ,(car vars)
         (once-only ,(cdr vars)
           ,@body))
      `(progn ,@body)))

  (defun unsplice (form)
    (if form (list form) nil))

  (defun has-docstring-p (body)
    (and (stringp (car body)) (cdr body)))

  (defun has-declare-p (body)
    (and (consp (car body)) (eq (caar body) 'declare)))

  (defmacro with-preamble ((preamble body-var) &body body)
    "Pop docstring and declarations off `body-var' and assign them to `preamble'."
    `(let ((,preamble (loop
                        :while (or (has-docstring-p ,body-var)
                                 (has-declare-p ,body-var))
                        :collect (pop ,body-var))))
       ,@body))

  (defmacro defmacro/once (name params &body body)
    "Like `defmacro' except that params which are immediately preceded
   by `&once' are passed to a `once-only' call which surrounds `body'."
    (labels ((once-keyword-p (obj)
               (and (symbolp obj) (equalp (symbol-name obj) "&once")))
              (remove-once-keywords (params)
                (mapcar (lambda (x) (if (consp x) (remove-once-keywords x) x))
                  (remove-if #'once-keyword-p params)))
              (find-once-params (params)
                (mapcon (lambda (x)
                          (cond ((consp (first x))
                                  (find-once-params (first x)))
                            ((once-keyword-p (first x))
                              (unless (and (cdr x) (atom (cadr x)))
                                (error "`&once' without parameter in ~a" name))
                              (list (second x)))
                            (t
                              nil)))
                  params)))
      (with-preamble (preamble body)
        `(defmacro ,name ,(remove-once-keywords params)
           ,@preamble
           (once-only ,(find-once-params params)
             ,@body)))))
  )


(defmacro define-lazy-singleton (name init &optional docstring)
  "Define a function NAME, that will return a singleton object,
   initialized lazily with INIT on first call.
   Also define a symbol macro <NAME> that will expand to (NAME)."
  (alexandria:with-unique-names (singleton)
    `(let (,singleton)
       (defun ,name ()
         ,docstring
         (or ,singleton
           (setf ,singleton ,init)))
       (define-symbol-macro ,(intern (format nil "<~A>" name)) (,name)))))
  

(defmacro define-synonym (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:if (macro-function ',orig)
            (setf (macro-function ',alias) (macro-function ',orig))
            (setf (symbol-function ',alias) (symbol-function ',orig)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro/once building-list (&once n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop :repeat ,n :collect (progn ,@body)))


(defmacro/once building-vector (&once n &body body)
  "Execute `body' `n' times, collecting the results into a vector."
  (alexandria:with-gensyms (result index)
    `(let1 ,result (make-array ,n)
       (dotimes (,index ,n ,result)
         (setf (aref ,result ,index) (progn ,@body))))))


(defmacro with-thread ((&key bindings name) &body body)
  `(let1 bt:*default-special-bindings* ,bindings
     (bt:make-thread (lambda () ,@body)
       :name ,name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Cut"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-cut (fn &rest args &aux arg-list)
  (let* ((body (mapcar (lambda (arg) (case arg
                                  (<> (let ((sym (gensym)))
                                        (push sym arg-list)
                                        sym))
                                  (otherwise arg))) 
                 args))
          (fn-form (nconc (etypecase fn 
                            (symbol (list fn))
                            (function `(funcall ,fn)))
                     body)))
    `(lambda ,(nreverse arg-list) ,fn-form)))
         
(defmacro cut (function-name &rest args-or-<>)
  `(apply #'map-cut ',function-name (quote ,args-or-<>)))
  
  
;; CL-USER> (#.(cut list 1 2 <> 4 <>) 3 "t")
;; (1 2 3 4 "t")
;; CL-USER> (apply #'map-cut '(list 1 2 <> 4 <>))
;; (LAMBDA (#:G925 #:G926) (LIST 1 2 #:G925 4 #:G926))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anaphora
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro anaphoric (op test &body body)
  "higher-order anaphoric operator creation macro."
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro aprog1 (first &body rest)
  "Binds IT to the first form so that it can be used in the rest of the
  forms. The whole thing returns IT."
  `(anaphoric prog1 ,first ,@rest))

(defmacro awhen (test &body body)
  "Like WHEN, except binds the result of the test to IT (via LET) for the scope
  of the body."
  `(anaphoric when ,test ,@body))

(defmacro atypecase (keyform &body cases)
  "Like TYPECASE, except binds the result of the keyform to IT (via LET) for
  the scope of the cases."
  `(anaphoric typecase ,keyform ,@cases))


;;; Dynamically bound functions.

;;; DEFUN-DYNAMIC defines a function which may be dynamically bound
;;; with FLET-DYNAMIC.

(defconstant +dynamic-fun-tag+ 'dynamic-fun-tag)


(defmacro defun-dynamic (name params &body body)
  (let ((args (gensym))
	(hidden (or (get name +dynamic-fun-tag+) (gensym))))
    `(progn
       (defparameter ,hidden
	 (lambda ,params ,@body))
       (defun ,name (&rest ,args)
	 (declare (special ,hidden))
	 (apply (symbol-value ',hidden) ,args))
       (setf (get ',name  +dynamic-fun-tag+) ',hidden))))



(defmacro flet-dynamic (clauses &body body)
  (let ((names (list)))
    `(let ,(loop
	      :for (name params . fn-body) :in clauses
	      :for hidden = (or (get name +dynamic-fun-tag+)
				(error "~a in FLET-DYNAMIC ~
                                    was not defined by DEFUN-DYNAMIC."
				       name))
	      :do (push hidden names)
	      :collect `(,hidden
			 (lambda ,params ,@fn-body)))
       (declare (special ,@names))
       ,@body)))




;;;; test

;; (defun-dynamic foo (x)
;;   (* 2 x))

;; (defun bar (x)
;;   (foo x))

;; (defun test ()
;;   (assert (boundp (get 'foo +dynamic-fun-tag+)))
;;   (assert (= 6 (foo 3)))
;;   (assert (= 30 (flet-dynamic ((foo (x) (* 10 x)))
;;                   (foo 3))))
;;   (assert (= 30 (flet-dynamic ((foo (x) (* 10 x)))
;;                   (bar 3))))
;;   (assert (= 30 (funcall (compile nil '(lambda ()
;;                                         (flet-dynamic ((foo (x) (* 10 x)))
;;                                           (bar 3)))))))
;;   :ok)


