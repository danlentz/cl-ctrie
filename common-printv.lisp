;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :printv
  (:use :cl)
  (:export
    #:ppmx
    #:printv
    #:ppmx-reader
    #:enable-ppmx-reader
    #:vlet*
    #:vlet
    #:vcond
    #:printv-reader
    #:enable-printv-reader
    #:*figlet-font*
    #:*printv-output*
    #:enable-printv-output
    #:disable-printv-output
    #:with-printv-output-enabled
    #:with-printv-output-disabled))

(in-package :printv)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (or (find-package :||) (make-package :||)))

(defvar *figlet-font*   "standard")
(defvar *printv-output* *trace-output*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPMX - Macroexpansion Pretty-Printer (from CCL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
           (exp (macroexpand exp1))
           (*print-circle* nil))
     (format *printv-output* "~%;; Form: ~W"  (quote ,form))
     (cond ((equal exp exp1)
             (format *printv-output* "~%;;~%;; Macro expansion:~%")
             (pprint exp *printv-output*))
       (t (format *printv-output* "~&;; First step of expansion:~%")
         (pprint exp1 *printv-output*)
         (format *printv-output* "~%;;~%;; Final expansion:~%")
         (pprint exp *printv-output*)))
     (format *printv-output* "~%;;~%;; ")
     (values)))

(setf (macro-function :ppmx) (macro-function 'ppmx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTV - Extended Edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;; Licensed under Apache License 2.0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-printv-output (&optional (stream *trace-output*))
  (setf *printv-output* stream))

(defun disable-printv-output ()
  (setf *printv-output* (make-broadcast-stream)))

(defmacro with-printv-output-enabled ((&optional (stream *trace-output*))
                                       &body body)
  `(let ((,*printv-output* ,stream))
     ,@body))

(defmacro with-printv-output-disabled ((&optional (stream (make-broadcast-stream)))
                                        &body body)
  `(let ((,*printv-output* ,stream))
     ,@body))

(defun minor-separator ()
  (format *printv-output* "~&;;; ~72,,,'-<-~> ;;;~%")
  (force-output *printv-output*))

(defun major-separator ()
  (format *printv-output* "~&;;;~77,,,';<;~>~%")
  (format *printv-output* "~&;;; ~72,,,'=<=~> ;;;~%")
  (format *printv-output* "~&;;;~77,,,';<;~>~%")
  (force-output *printv-output*))

(defun form-printer (form)
  (typecase form
    ;; String (label):
    (string (if (equal (subseq form 0 2) "#|")
              (format *printv-output* "~&~a~%" form)
              (format *printv-output* "~&;;; ~a~%" form)))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *printv-output* "~&;;;   ~w =>" form))
    (vector (format *printv-output* "~&;;   ~s~%" form)) 
    ;; Self-evaluating form:
    (t (format *printv-output* "~&;;;   ~s~%" form)))
  (force-output *printv-output*))

(defun values-printer (values-list)
  (format *printv-output* "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%"  values-list)
  (force-output *printv-output*))

(defmacro vlet* (bind-forms &body body)
  `(progn  (format *printv-output* "~&           [")
     (let* ,(mapcar #'(lambda (form)
                        (if (listp form)
                          `(,(car form) (let ((v ,(cadr form)))
                                          (format *printv-output*
                                            " [~S=~S] " ',(car form) v)
                                          v))
                          form))
              bind-forms)
       (format *printv-output* "]~&;;;   =>")
       ,@body)))

(defmacro vlet (bind-forms &body body)
  `(progn  (format *printv-output* "~&           [")
     (let ,(mapcar #'(lambda (form)
                       (if (listp form)
                         `(,(car form) (let ((v ,(cadr form)))
                                         (format *printv-output*
                                           " [~S=~S] " ',(car form) v)
                                         v))
                         form))
             bind-forms)
       (format *printv-output* "]~&;;;   =>")
       ,@body)))

(defmacro vcond (&body clauses)
  `(progn  (format *printv-output* "~&~%          ")
     (cond ,@(mapcar #'(lambda (clause)
                       `((let ((x ,(car clause)))
                           (format *printv-output*
                             " [~S -> ~S]~%          " ',(car clause) x)
			  x)
			,@(cdr clause)))
               clauses))
      (format *printv-output* "~&;;;   =>")))

(defun expander (forms &optional values-trans-fn) ;; Allow for customized printv'ers:
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil) ,result-sym)
       ,@(loop for form in forms nconcing
           (cond
             ;; Markup form:
             ((eq form ':ff) (list '(major-separator)))
             ((eq form ':hr) (list '(minor-separator)))            
             ;; Binding form:
             ((and (consp form) (or (eq (car form) 'let) (eq (car form) 'let*)))
               `((form-printer ',form)
                  (values-printer
                    (setf ,result-sym
                      ,(if values-trans-fn
                         `(funcall ,values-trans-fn
                            (multiple-value-list
                              ,(case (car form)
                                 (let `(vlet ,@(rest form)))
                                 (let* `(vlet* ,@(rest form))))))
                         `(multiple-value-list
                            ,(case (car form)
                               (let `(vlet ,@(rest form)))
                               (let* `(vlet* ,@(rest form))))))))))            
             ;; COND form:
             ((and (consp form) (eq (car form) 'cond)) 
               `((form-printer ',form)
                  (values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list (vcond ,@(rest form))))
                                         `(multiple-value-list (vcond ,@(rest form))))))))
             ;; FIGLET banner:             
             ((and (symbolp form) (eq (symbol-package form) (find-package :||)))
               `((form-printer 
                   ,(with-output-to-string (s)                      
                      (princ "#|" s)
                      (terpri s)
                      (ignore-errors
                        (asdf/run-program:run-program
                          (format nil "figlet -f ~A -w ~D ~A"
                            *figlet-font* *print-right-margin* (symbol-name form))
                          :output s))
                      (princ "|#" s)))))
             ;; Evaluated form:
             ((or (consp form) (and (symbolp form) (not (keywordp form))))
               `((form-printer ',form)
                  (values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list ,form))
                                         `(multiple-value-list ,form))))))             
             ;; Self-evaluating form:
             (t `((form-printer 
                    (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))

(defmacro printv (&rest forms)
  (expander forms))

(setf (macro-function :printv) (macro-function 'printv))

;; (printv  ||::|PRINTv| )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTV/PPMX Readtable Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+swank
(defun register-readtable (&optional (package *package*))
  (let ((package-name (typecase package
                        (string                package)
                        (symbol   (symbol-name package))
                        (package (package-name package)))))
    (push (cons package-name *readtable*) swank:*readtable-alist*)))

#-swank
(defun register-readtable (&optional (package *package*))
  nil)

(defun ppmx-reader (stream char)
  (declare (ignore char))
    (let ((body (read stream t nil t)))
      `(ppmx ,body)))

(defun enable-ppmx-reader (&optional (char #\$))
  (prog1 char
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character char 'ppmx-reader)
    (register-readtable)))

(defun printv-reader (stream char)
  (declare (ignore char))
    (let ((body (read stream t nil t)))
      `(printv ,body)))

(defun enable-printv-reader (&optional (char #\^))
  (prog1 char
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character char 'printv-reader)
    (register-readtable)))

(when (find-package :readtable)
  (let ((pubsyms '(ppmx-reader enable-ppmx-reader
                    printv-reader enable-printv-reader)))
  (import pubsyms :readtable)
  (export pubsyms :readtable)))
#|
#+named-readtables
(named-readtables:defreadtable :printv
  (:merge :standard)
  (:macro-char #\$ #'ppmx-reader   t)
  (:macro-char #\^ #'printv-reader t))

#+named-readtables
(named-readtables:in-readtable :printv)
|#  
;;#-named-readtables
(enable-ppmx-reader)

;;#-named-readtables
(enable-printv-reader)

(register-readtable)
(pushnew :printv *features*)
