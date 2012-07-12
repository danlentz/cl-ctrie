;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Helpful Utility Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+swank 
(defun ^ (thing &optional wait)
  (swank:inspect-in-emacs thing :wait wait))

(define-symbol-macro ?  (prog1 * (describe *)))

#+swank
(define-symbol-macro ?^ (prog1 * (^ *)))


;;; lmj/lparallel
#-lparallel
(defmacro let1 (var value &body body)
  "Make a single `let' binding, heroically saving three columns."
  `(let ((,var ,value))
     ,@body))

;;; lmj/lparallel
#-lparallel
(defmacro defun/inline (name args &body body)
  "Like `defun' but declare the function as inline"
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))


(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
           (exp (macroexpand exp1))
           (*print-circle* nil))
     (format *trace-output* "~%;; Form: ~W"  (quote ,form))
     #+() (pprint (quote ,form) *trace-output*)
     (cond ((equal exp exp1)
             (format *trace-output* "~%;;~%;; Macro expansion:~%")
             (pprint exp *trace-output*))
       (t (format *trace-output* "~&;; First step of expansion:~%")
         (pprint exp1 *trace-output*)
         (format *trace-output* "~%;;~%;; Final expansion:~%")
         (pprint exp *trace-output*)))
     (format *trace-output* "~%;;~%;; ")
     (values)))

;;; place utils (from ??)
(defmacro place-fn (place-form)
  "This creates a closure which can write to and read from the \"place\"
   designated by PLACE-FORM."
  (with-gensyms (value value-supplied-p)
    `(sb-int:named-lambda place (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
         ,place-form))))

(defmacro post-incf (place &optional (delta 1))
  "place++ ala C"
  `(prog1 ,place (incf ,place ,delta)))


;;; gensymmetry (from ??)
(defun gensym-list (length)
  (loop repeat length collect (gensym)))
  
(defmacro gensym-values (num)
  `(values ,@(loop REPEAT num COLLECT '(gensym))))

(defmacro gensym-let ((&rest symbols) &body body)
  (let ((n (length symbols)))
    `(multiple-value-bind ,symbols (gensyms-values ,n)
       ,@body)))


;; KMRCL/USENET
(defmacro deflex (var val &optional (doc nil docp))
  "Defines a top level (global) lexical VAR with initial value VAL,
      which is assigned unconditionally as with DEFPARAMETER. If a DOC
      string is provided, it is attached to both the name |VAR| and the
      name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
      kind 'VARIABLE. The new VAR will have lexical scope and thus may
      be shadowed by LET bindings without affecting its global value."
  (let* ((s0 (load-time-value (symbol-name '#:*storage-for-deflex-var-)))
         (s1 (symbol-name var))
         (p1 (symbol-package var))
         (s2 (load-time-value (symbol-name '#:*)))
         (backing-var (intern (concatenate 'string s0 s1 s2) p1)))
    `(progn
      (defparameter ,backing-var ,val ,@(when docp `(,doc)))
      ,@(when docp
              `((setf (documentation ',var 'variable) ,doc)))
       (define-symbol-macro ,var ,backing-var))))


;;; anaphora
(defmacro anaphoric (op test &body body)
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


;;; emacs?
(defun mapappend (fun &rest args)
   (if (some 'null args)
       '()
       (append (apply fun (mapcar 'car args))
         (mapappend fun (mapcar 'cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic Update (sbcl src copied over until i update to a more recent release)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: unused

(defmacro atomic-update (place update-fn &rest arguments &environment env) 
  "Updates PLACE atomically to the value returned by calling function
designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.
PLACE may be read and UPDATE-FN evaluated and called multiple times before the
update succeeds: atomicity in this context means that value of place did not
change between the time it was read, and the time it was replaced with the
computed value. PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))

;;; Examples:
;;
;; Conses T to the head of FOO-LIST.
;;
;;   (defstruct foo list)
;;   (defvar *foo* (make-foo))
;;   (atomic-update (foo-list *foo*) #'cons t)
