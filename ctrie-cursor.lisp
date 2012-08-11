;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #` Reader Macro 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg collect (symb 'a i))
       ,(funcall (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character  #\# #\` #'|#`-reader|))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialized Binding Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro fbind ((name form) &body body)
  (let ((gname (gensym (string name))))
    `(let ((,gname ,form))
       (declare (function ,gname))
       (flet ((,name (&rest args) (apply ,gname args)))
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pandoric Object Protocol 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


(defmacro dlambda (&rest dispatch-table)
  (let* ((arglist (gensym "ARGS")))
    `(lambda (&rest ,arglist)
       (case (car ,arglist)
         ,@(mapcar (lambda (d)
                     `(,(if (eq t (car d))
                          t
                          (list (car d)))
                        (apply (lambda ,@(cdr d))
                          ,(if (eq t (car d))
                             arglist
                             `(cdr ,arglist)))))
             dispatch-table)))))


(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))


(defmacro alet-hotpatch (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (dlambda
       (:hotpatch (closure)
         (setq this closure))
       (t (&rest args)
         (apply this args)))))


(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s) `(setq this #',s)))
     (labels (,@states) #',(caar states))))


(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
        ((consp (car bs))
          (car bs))
        (t (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))


(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
         letargs)
     (t (error "Unknown pandoric get: ~a" sym))))


(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
         letargs)
     (t (error "Unknown pandoric set: ~a" sym))))


(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this) (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda 
         (:pandoric-get (sym)        ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)    ,(pandoriclet-set letargs))
         (t             (&rest args)  (apply this args))))))


(defun/inline pandoric-slot-value (box sym)
  (funcall box :pandoric-get sym))


(defsetf pandoric-slot-value (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))


(defmacro with-pandoric-slots (syms box &rest body)
  (let ((g!box (gensym "box")))
    `(let ((,g!box ,box))
       (declare (ignorable ,g!box))
       (symbol-macrolet (,@(mapcar #`(,a1 (pandoric-slot-value ,g!box ',a1))
                             syms))
         ,@body))))


(defun pandoric-hotpatch (box new)
  (with-pandoric-slots (this) box
    (setq this new)))


(defmacro pandoric-recode (vars box new)
  `(with-pandoric-slots (this ,@vars) ,box
     (setq this ,new)))


(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (declare (ignorable this self))
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)        ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)    ,(pandoriclet-set pargs))
                (t             (&rest args)  (apply this args)))))))


(defvar *pandoric-eval-tunnel*)


(defmacro pandoric-eval (vars expr)
  `(let ((*pandoric-eval-tunnel* (plambda () ,vars t)))
     (eval `(with-pandoric-slots ,',vars *pandoric-eval-tunnel* ,,expr))))


(defmacro define-pandoric-function (name args &body body)
  `(defun ,name (self)
     ,(if args
        `(with-pandoric-slots ,args self ,@body)
        `(progn ,@body))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; PRINCIPAL SYMBOLS DEFINING THE 'PANDORIC API' ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    FBIND
;;    THIS
;;    SELF
;;    ALET
;;    ALET-FSM
;;    ALET-HOTPATCH
;;    ALAMBDA
;;    DLAMBDA
;;    PLAMBDA
;;    DEFINE-PANDORIC-FUNCTION 
;;    WITH-PANDORIC-SLOTS
;;    PANDORIC-SLOT-VALUE
;;    PANDORIC-RECODE
;;    PANDORIC-EVAL
;;    PANDORICLET-GET
;;    PANDORICLET-SET
;;    PANDORICLET
;;    PANDORIC-HOTPATCH
;;    *PANDORIC-EVAL-TUNNEL*
;;    SHARP-BACKTICK
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; EXAMPLE USE OF PANDORIC OBJECT PROTOCOL ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                   Implementing a Simple Application
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (define-pandoric-function stats-counter-mean (sum count)
;;   (/ sum count))
;;
;;
;; (define-pandoric-function stats-counter-variance (sum-of-squares sum count)
;;   (if (< count 2)
;;     0
;;     (/ (- sum-of-squares (* sum (stats-counter-mean self))) (- count 1))))
;;
;;
;; (define-pandoric-function stats-counter-stddev ()
;;   (sqrt (stats-counter-variance self)))
;;
;;
;; (defun make-stats-counter (&key (count 0) (sum 0) (sum-of-squares 0))
;;   (plambda (n) (sum count sum-of-squares)
;;     (incf sum-of-squares (expt n 2))
;;     (incf sum n)
;;     (incf count)
;;     (format t "~&mean=~A~%var=~A~%stdev=~A~%~%"
;;       (stats-counter-mean self)
;;       (stats-counter-variance self)
;;       (stats-counter-stddev self))))
;;
;;
;; (defmacro define-stats-counter (name &rest args)
;;   (let ((fn (apply #'make-stats-counter args)))
;;     `(prog1 (quote ,name)
;;        (defparameter ,name ,fn)
;;        (setf (symbol-function (quote ,name)) ,fn))))
;;
;;
;; (defmacro with-stats-counter ((name &rest args) &body body)
;;   (let ((fn (apply #'make-stats-counter args)))
;;     `(let ((,name ,fn))
;;        (fbind (,name ,fn)
;;          ,@body))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'special' pandoric object with global binding and dynamic scope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (define-stats-counter mysc) ==>  MYSC
;;
;; FN:
;;
;; (describe #'mysc)
;;
;; #<CLOSURE (LAMBDA (&REST #:ARGS6) :IN MAKE-STATS-COUNTER) {100C10975B}>
;;   [compiled closure]
;;
;; Lambda-list: (&REST ARGS6)
;; Derived type: (FUNCTION (&REST T) *)
;; Source form:
;;   (SB-INT:NAMED-LAMBDA MAKE-STATS-COUNTER
;;       (&KEY (COUNT 0) (SUM 0) (SUM-OF-SQUARES 0))
;;     (BLOCK MAKE-STATS-COUNTER
;;       (PLAMBDA (N) (SUM COUNT SUM-OF-SQUARES) (INCF SUM-OF-SQUARES (EXPT N 2))
;;                (INCF SUM N) (INCF COUNT)
;;                (FORMAT T "~&mean=~A~%var=~A~%stdev=~A~%~%"
;;                        (STATS-COUNTER-MEAN SELF) (STATS-COUNTER-VARIANCE SELF)
;;                        (STATS-COUNTER-STDDEV SELF)))))
;;
;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;
;;
;; VAR:
;;
;; (describe 'mysc)
;;
;; PANDORA::MYSC
;;   [symbol]
;;
;; MYSC names a special variable:
;;   Value: #<CLOSURE (LAMBDA # :IN MAKE-STATS-COUNTER) {100EED964B}>
;;
;; MYSC names a compiled function:
;;   Lambda-list: (&REST ARGS6)
;;   Derived type: FUNCTION
;;   Source form:
;;     (SB-INT:NAMED-LAMBDA MAKE-STATS-COUNTER
;;         (&KEY (COUNT 0) (SUM 0) (SUM-OF-SQUARES 0))
;;       (BLOCK MAKE-STATS-COUNTER
;;         (PLAMBDA (N) (SUM COUNT SUM-OF-SQUARES)
;;                  (INCF SUM-OF-SQUARES (EXPT N 2)) (INCF SUM N) (INCF COUNT)
;;                  (FORMAT T "~&mean=~A~%var=~A~%stdev=~A~%~%"
;;                          (STATS-COUNTER-MEAN SELF)
;;                          (STATS-COUNTER-VARIANCE SELF)
;;                         (STATS-COUNTER-STDDEV SELF)))))
;;
;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;
;;
;; RESULT:
;;
;; CL-USER>  (progn (with-pandoric-slots (count) mysc (print count))
;;                  (mysc 5.0) (mysc 10) (mysc 5)
;;                  (with-pandoric-slots (count) mysc (print count)))     ==> 
;; 0 
;; mean=5.0
;; var=0
;; stdev=0.0
;;
;; mean=7.5
;; var=12.5
;; stdev=3.535534
;;
;; mean=6.6666665
;; var=8.333336
;; stdev=2.886752
;; 3
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; local pandoric object with lexical scope limited to the enclosing form
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; :FN
;; ===
;;
;; #<FUNCTION (FLET CTR) {1010A5559B}>
;;   [compiled function]
;;
;; Lambda-list: (&REST ARGS)
;; Derived type: (FUNCTION (&REST T) *)
;;
;; :VAR
;; ====
;;
;; #<CLOSURE (LAMBDA (&REST #:ARGS6) :IN MAKE-STATS-COUNTER) {10109D1C2B}>
;;   [compiled closure]
;;
;; Lambda-list: (&REST ARGS6)
;; Derived type: (FUNCTION (&REST T) *)
;; Source form:
;;   (SB-INT:NAMED-LAMBDA MAKE-STATS-COUNTER
;;       (&KEY (COUNT 0) (SUM 0) (SUM-OF-SQUARES 0))
;;     (BLOCK MAKE-STATS-COUNTER
;;       (PLAMBDA (N) (SUM COUNT SUM-OF-SQUARES) (INCF SUM-OF-SQUARES (EXPT N 2))
;;                (INCF SUM N) (INCF COUNT)
;;                (FORMAT T "~&mean=~A~%var=~A~%stdev=~A~%~%"
;;                        (STATS-COUNTER-MEAN SELF) (STATS-COUNTER-VARIANCE SELF)
;;                        (STATS-COUNTER-STDDEV SELF)))))
;;
;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;  ;;;
;;
;; RESULT:
;;
;; CL-USER>  (with-stats-counter (ctr)
;;                (format t "~%~S~%===~%~%"  :fn)
;;                (describe #'ctr)
;;                (format t "~%~S~%====~%~%" :var)
;;                (describe ctr)
;;                (with-pandoric-slots (count) ctr (print count))
;;                (ctr 5.0)
;;                (ctr 10)
;;                (ctr 5)
;;                (with-pandoric-slots (count) ctr (print count)))      ==>
;; 0 
;; mean=5.0
;; var=0
;; stdev=0.0
;;
;; mean=7.5
;; var=12.5
;; stdev=3.535534
;;
;; mean=6.6666665
;; var=8.333336
;; stdev=2.886752
;; 3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

