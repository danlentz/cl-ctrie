;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #` Reader
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


(defmacro with-macro-character ((ch func) &body body)
  "Bind the reader macro func to ch and execute the body in this
  environment.  Restore the original reader-macros when this form is
  done.
  ;;;
  ;;; (with-macro-character (ch func) body)
  ;;;"
  (let ((c (gensym))
        (f (gensym))
        (o (gensym)))
    `(let ((,c ,ch)
           (,f ,func))
       (let ((,o (get-macro-character ,c)))
         (set-macro-character ,c ,f)
         (unwind-protect
              (progn ,@body)
           (set-macro-character ,c ,o))))))


(defmacro with-macro-characters (pairs &body body)
  "Bind the reader macro func1 to ch1, and so on, and execute the body
   in this environment. Restore the original reader-macros when this
   form is done.
   ;;;
   ;;; (with-macro-characters ((ch1 func1) (ch1 func2) ...) body)
   ;;;"
  (if (null pairs)
      `(progn ,@body)
      `(if (oddp (length ',(car pairs)))
           (error "with-macro-characters: ~A must be a pair of a
           character and a reader-macro-function" ',(car pairs))
           (with-macro-character ,(car pairs)
             (with-macro-characters ,(cdr pairs)
               ,@body)))))


;; (defun try-it ()
;;   (with-macro-characters
;;       ((#\! #'reader-iota-0)
;;        (#\@ #'reader-iota-1))
;;     (concatenate 'list
;;                  '(first)
;;                  (read-from-string "!10")
;;                  '(second)
;;                  (read-from-string "@10"))))


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


;; (defmacro/once dlambda-bind (spec)
;;   `(dlambda ,@spec))


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


(eval-when (:load-toplevel :compile-toplevel :execute)
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
       (t (error "Unknown pandoric set: ~a" sym)))))


(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this) (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body)) ,@(butlast body)
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


(defmacro pandoric-eval (vars expr)
  `(let ((*pandoric-eval-tunnel* (plambda () ,vars t)))
     (eval `(with-pandoric-slots ,',vars *pandoric-eval-tunnel* ,,expr))))


(defmacro define-pandoric-function (name args &body body)
  `(progn
     (defun ,name (self)
     ,(if args
        `(with-pandoric-slots ,args self ,@body)
        `(progn ,@body)))))



;; (ppmx
;;   (defmacro define-pandoric-interface (name implements) ;; &rest forms)
;;     (with-unique-names  (me supers)
;;       `(defparameter ,name  
;;          (alet ((,me ,name) (,supers ',implements))
;;            (dlambda
;;              (:reset      () t)
;;              (:name       () ,me)
;;              (:implements () ,supers)
;;              (t (&rest args) (apply this args)))))))) 
;;                ,@forms
;;   ;; (unless ,name (apply this :reset ,name ,supers))
;;(setf implements (mapcar #`(,a1) ,supers)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;; CTRIE-LAMBDA ;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro/once ctrie-lambda (&once ctrie &rest rest)
  "Pandoric Object and Inter-Lexical Communication Protocol
  this macro builds the most unencumbered and widely applicable
  'purist edition' Of our PLAMBDA based form.  Even as such,
  a lot of care has been given to many subtle ways it has been
  refined to offer the most convenient and natural tool possible.
  ```;;;
     ;;; (plambda (#<CLOSURE (LAMBDA (&REST ARGS)) {100929EB1B}> )
     ;;;
     ;;; DISPATCHING to FUNCTIONAL MAPPING:
     ;;;   (IF (REST ARGS)
     ;;;          (APPLY ARG (REST ARGS))
     ;;;          (FUNCALL ARG #'IDENTITY)) =>
     ;;; ------------------------------------------------------------
     ;;; INITIALIZING PLAMBDA
     ;;; ------------------------------------------------------------
     ;;;   IT => #S(CTRIE
     ;;;               :READONLY-P NIL
     ;;;               :TEST EQUAL
     ;;;               :HASH SXHASH
     ;;;               :STAMP #<CLOSURE (LAMBDA # :IN CONSTANTLY) {10092B516B}>
     ;;;               :ROOT #S(INODE
     ;;;                        :GEN #:|ctrie2196|
     ;;;                        :REF #S(REF
     ;;;                                :STAMP @2012-08-19T13:34:58.314457-04:00
     ;;;                                :VALUE #S(CNODE :BITMAP 0 :ARCS #())
     ;;;                                :PREV NIL)))
     ;;;   PLIST => (:CONTAINER #<CLOSURE (LAMBDA #) {100929EACB}> 
     ;;;             :TIMESTAMP @2012-08-19T13:34:58.314464-04:00)
     ;;;   STACK => (#<CLOSURE (LAMBDA #) {100929EACB}>)
     ;;; 
     ;;; ------------------------------------------------------------
     ;;;  #<CLOSURE (LAMBDA (&REST #:ARGS55)) {100929EACB}>
     ;;;```"
  `(alet (IT AT ME PLIST STACK 
           ,@(remove-if #'boundp `(,@rest)))          
     
     ;; INITIALIZATION THUNK
     ;; ---------------------------
     (unless it (funcall this
                  #'(lambda (&rest args)
                      (declare (ignorable args))
                      (printv :hr "INITIALIZING PLAMBDA" :hr)
                      (prog1 this
                        (setf it (or ,ctrie (make-ctrie)))
                      ;;   at (dlambda
                      ;;        (:test () (root-node-access it))
                      ;;        (t     () (funcall this :test))))                        
                      ;;   (setf
                      ;;     mbox  (sb-concurrency:make-mailbox)
                      ;;     queue (sb-concurrency:make-queue)))
                        (push this stack)
                        (setf
                          (getf plist :timestamp) (local-time:now)
                          (getf plist :container) this)
                        (printv it)
                        (printv plist)
                        (printv stack "" :hr)))))
     
  ;; OPERATIONAL DISPATCH
  ;; --------------------
     (setf me (PLAMBDA (&rest args &aux (arg (car args)))
                    (IT ME AT THIS PLIST STACK  ,@REST)
                
                (printv :hr " ")
                (format *printv-output* "~%;;; (plambda (~S~{ ~S~})~%;;;"
                  arg (rest args))

                (typecase arg
                  
                  (function (printv "DISPATCHING to FUNCTIONAL MAPPING:"
                              (apply arg it (rest args))))  ;; (if (rest args)
                                                            ;;   (apply arg (rest args))
                                                            ;;   (funcall arg #'identity))))

                  (t        (printv "DISPATCHING to STRUCTURAL MAP:" (rest args) arg)
                              (cond
                                ((and (atom arg) (= 1 (length args)))
                                  (printv (apply #'ctrie-get it args)))
                                ((and (atom arg) (= 2 (length args)))
                                  (printv (apply #'ctrie-put it args)))
                                ((every #'atom args)
                                  (printv (mapcar (lambda (key) (ctrie-get it key)) args)))
                                ((and (every #'consp args) (every #'atom (mapcar #'cdr args)))
                                  (printv (mapcar (lambda (pair) (ctrie-put it (car pair) (cdr pair))) args)))
                                (t (error "incongruent argument list ~s" args)))))))))



(define-symbol-macro |it|   #'identity)
(define-symbol-macro |pp|   #'ctrie-pprint)
(define-symbol-macro |k*|   #'ctrie-keys)
(define-symbol-macro |v*|   #'ctrie-values)
(define-symbol-macro |kv*|  #'ctrie-to-alist)


(defgeneric ctrie-lambda-ctrie (ctrie-designator)
  (:method ((ctrie transient-ctrie))
    ctrie)
  (:method ((ctrie persistent-ctrie))
    ctrie)
  (:method ((ctrie function))
    (with-pandoric-slots (it) ctrie
      it)))


(defgeneric ensure-lambda (ctrie-designator &key &allow-other-keys)
  (:method ((ctrie transient-ctrie) &key)
    (let* (ctrie lambda)
      (setf lambda
        (ctrie-lambda ctrie container lambda))))
  (:method ((ctrie persistent-ctrie) &key)
    (let* (ctrie lambda)
      (setf lambda
        (ctrie-lambda ctrie container lambda))))
  (:method ((self function) &key)
    self))


(defun ctrie-lambda-spawn (self &key read-only)
  "Causes the atomic clone of enclosed ctrie structure and builds a new
  lexical closure to operate on it.  Does not bother to reproduce fancy
  (expensive) object, class, bindings, but provides almost identical
  functionality.  May be used to more efficintly distribute workload
  in parallel"
  (ctrie-lambda
    (funcall #'ctrie-snapshot
      (ctrie-lambda-ctrie self)
      :read-only read-only)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Ref Reader
;; (pipeop --  http://cadr.g.hatena.ne.jp/g000001/20081125/1227612201)
;; (ppmx (progn #/person/car/father/name/last/1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+notyet
(defun pipeop (q)
  "_REFERENCE PIPELINE_
   ```
   ;;;   
   ;;; (progn #/person/car/father/name/last/1)
   ;;;
   ;;; => (PROGN (NTH 0 (LAST (NAME (FATHER (CAR (PERSON)))))))
   ;;; ```"
  (labels ((pipeop-n (expr rest)
             (let ((expr (read-from-string expr)))
               `(,@(if (numberp expr)
                     `(nth ,(1- expr))
                     (list expr))
                  ,@(if rest (list rest) rest))))
            (recur (q acc)
              (let* ((cmds (string q))
                      (pos (position #\/ cmds)))
                (if pos
                  (recur (subseq cmds (1+ pos))
                    (pipeop-n (subseq cmds 0 pos) acc))
                  (pipeop-n cmds acc)))))
    (recur q () )))

#+()
(set-dispatch-macro-character #\# #\/
  (lambda (str char arg)
    (declare (ignore char arg))
    (pipeop (read str nil nil nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-LAMBDA Dispatch
;; (notyet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter +simple-dispatch+
  (dlambda
    (:from   (arg) arg)
    (:to     (arg) arg)
    (:domain (arg) arg)
    (:range  (arg) arg)))


(defparameter +pax-romana+
  (dlambda
    (:from   (arg) arg)
    (:to     (arg) arg)
    (:domain (arg) arg)
    (:range  (arg)
      (if  (and (numberp arg) (plusp arg) (< arg 2020))
        (format nil "~@r" arg)
        (format nil "~:r" arg)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-LAMBDA Metaclass (expoloratory)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ctrie-lambda-class (sb-mop:funcallable-standard-class)
  ())

(defmethod sb-mop:validate-superclass ((sub ctrie-lambda-class)
                                        (super funcallable-standard-class))
  t)

(defclass ctrie-lambda-object (sb-mop:funcallable-standard-object)
  ((dispatch
     :initarg :dispatch
     :accessor ctrie-lambda-dispatch)
    (ctrie
      :initarg :ctrie
      :accessor ctrie-lambda-ctrie)
    (function
      :initarg :function
      :accessor ctrie-lambda-function))      
  (:metaclass ctrie-lambda-class))


(defmethod slot-unbound (class (instance ctrie-lambda-object) (slot (eql 'ctrie)))
  (funcall instance #'identity))

#+notyet
(defmethod slot-unbound (class (instance ctrie-lambda-object) (slot (eql 'dispatch)))
  (ctrie-lambda-dispatch-table instance))

(defmethod slot-unbound (class (instance ctrie-lambda-object) (slot (eql 'function)))
  (fdefinition instance))


#+notyet
(defclass ctrie-transactional-class (sub ctrie-lambda-class) ())

#+notyet
(defmethod sb-mop:validate-superclass
  ((sub ctrie-lambda-class) (super ctrie-transactional-class)) t)

#+notyet
(defclass ctrie-transactional-object (ctrie-lambda-object transactional-collection)
  () (:metaclass ctrie-transactional-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-LAMBDA binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-ctrie (name ctrie &rest args &key (object t) spec)
  "Define a 'functional' __CTRIE-LAMBDA__ that combines all the the
  capabilities of the raw data structure with behavior and semantics
  one would expect of any other ordinary common-lisp function.  The
  resulting symbol defined as 'name will be bound in three distinct
  namespaces: the `SYMBOL-VALUE` will be bound to the LAMBDA CLOSURE
  object, `SYMBOL-FUNCTION` (fdefinition) will be FBOUND to the
  compiled function, and the corresponding '(SETF NAME) form will be
  SETF-BOUND.  the syntax for invoking NAME is as in a LISP1; i.e., no
  'funcall' is required (but still works if you prefer).
  Calling `(NAME key)` returns the value mapped to key, or `NIL` just
  as if by `(CTRIE-GET ctrie-name key).` Analogously when used as a
  setf-able place such as by `(setf (NAME key) value)` it has the
  equivalent behavior to the operation `(CTRIE-PUT ctrie-name key
  value).` Use of this type of binding technique has some really
  convenient effects that I've quickly started to become quite fond
  of.  One such idiom, for example, `(mapcar MY-CTRIE '(key1 key2 key3
  key4 ...))` returns a list containing all the mapped values
  corresponding to the respective keys.  One additional feature that
  I've found extremely useful is included _under the hood:_ Invoking
  MY-CTRIE on an object of type FUNCTION will not search the ctrie for
  an entry having that function ast its key, but will instead APPLY
  that function to the actual CTRIE structure wrapped within the
  closure.  Thus, `(MY-CTRIE #'identity)` will return the underlying
  ctrie as just an ordinary instance of a CTRIE STRUCTURE.  
  There are many other functions this is handy with, like
  `(MY-CTRIE #'ctrie-size)` `(MY-CTRIE #'ctrie-to-hashtable)`
  etc.  Some additional examples are provided below.
  ```
  ;;;  (define-ctrie my-ctrie)
  ;;;    =>  MY-CTRIE
  ;;;
  ;;;  (describe 'my-ctrie)
  ;;;
  ;;;     CL-CTRIE::MY-CTRIE
  ;;;       [symbol]
  ;;;    
  ;;;     MY-CTRIE names a special variable:
  ;;;       Value: #<CLOSURE (LAMBDA # :IN MAKE-CTRIE-LAMBDA) {100F73261B}>
  ;;;    
  ;;;     MY-CTRIE names a compiled function:
  ;;;       Lambda-list: (&REST ARGS1)
  ;;;       Derived type: FUNCTION
  ;;;    
  ;;;     (SETF MY-CTRIE) names a compiled function:
  ;;;       Lambda-list: (VALUE KEY)
  ;;;       Derived type: (FUNCTION (T T) *)
  ;;;
  ;;;
  ;;;   (my-ctrie :HONG-KONG :FOOY)
  ;;;     =>  :FOOY
  ;;;
  ;;;   (my-ctrie :HONG-KONG)
  ;;;     =>  :FOOY ; T
  ;;;
  ;;;   (map 'list #'eval (mapcar #`(my-ctrie ,a1 ,a1) (iota 12)))
  ;;;     =>  (0 1 2 3 4 5 6 7 8 9 10 11)
  ;;;
  ;;;   (mapcar my-ctrie (iota 12))
  ;;;     =>  (0 1 2 3 4 5 6 7 8 9 10 11)
  ```"
  (declare (ignorable args object spec))
  (with-gensyms (ctrie-lambda)
    `(let1 ,ctrie-lambda (ctrie-lambda ,ctrie)
       (proclaim '(special ,name))
       (setf (symbol-value ',name)
         (if ,object (make-instance 'ctrie-lambda-object) ,ctrie-lambda))
       (when ,object (sb-mop:set-funcallable-instance-function
                       (symbol-value ',name) ,ctrie-lambda))
       (setf (fdefinition  ',name) ,ctrie-lambda)
       (setf (fdefinition  '(setf ,name))
         #'(lambda (value key)
             (funcall ,ctrie-lambda key value)))        
       ',name)))


(defun new-ctrie (&key (hash 'sxhash) (test 'equal))
  (let* ((ctrie (make-ctrie :hash hash :test test))
          (fn (ctrie-lambda ctrie))
          (obj (make-instance 'ctrie-lambda-object)))
    (prog1 obj
      (sb-mop:set-funcallable-instance-function obj fn))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-LAMBDA Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(define-pandoric-function ctrie-lambda-dispatch-table (dispatch-table)
  dispatch-table)

#+()
(defun (setf ctrie-lambda-dispatch-table) (dispatch ctrie-lambda)
  (with-pandoric-slots (dispatch-table) ctrie-lambda
    (setf dispatch-table dispatch)))

#+()
(define-pandoric-function ctrie-lambda-reset (at top up path)
  (prog1 self
    (setf at (root-node-access top))
    (setf up top)
    (setf path nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-CURSOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun ctrie-cursor-reset (self)
;;   (ctrie-lambda-reset self))

;; (define-pandoric-function ctrie-cursor-timestamp (meta)
;;   (getf meta :timestamp))

;; (define-pandoric-function ctrie-cursor-ctrie (top)
;;   top)

;; (define-pandoric-function ctrie-cursor-looking-at (at)
;;   (type-of at))

;; (define-pandoric-function ctrie-cursor-at-top-p (up top)
;;   (eq up top))

;; (define-pandoric-function ctrie-cursor-up (at up path)
;;   nil)

;; (define-pandoric-function ctrie-cursor-down (at up path)
;;   (when (inode-p at)
;;     (push (cons at up) path)
;;     (setf up at)
;;     (setf at (inode-read at))))

;; (define-pandoric-function ctrie-cursor-next (at up path)
;;   nil)

;; (define-pandoric-function ctrie-cursor-has-next-p (at up path)
;;   nil)

;; (define-pandoric-function ctrie-cursor-prev (at up path)
;;   nil)

;; (define-pandoric-function ctrie-cursor-has-prev-p (at up path)
;;   nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit/Regression Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-fbind
  (fbind (foo (lambda (x) (list 'foo x)))
    (assert-equalp (foo 1)  '(foo 1))
    (assert-equalp (foo :x) '(foo :x))
    (assert-equalp (foo (foo t)) '(foo (foo t))))) 


(define-test check-alet-fsm
  (flet ((make-test-fsm ()
           (alet ((acc 0))
             (alet-fsm
               (going-up (n)
                 (if (eq n 'invert)
                   (state going-down)
                   (incf acc n)))
               (going-down (n)
                 (if (eq n 'invert)
                   (state going-up)
                   (decf acc n)))))))
    (fbind (fsm (make-test-fsm))
      (assert-eql  0 (fsm 0))
      (assert-eql  5 (fsm 5))
      (assert-eql  5 (fsm 0))
      (assert-eql  6 (fsm 1))
      (fsm 'invert)
      (assert-eql  0 (fsm 6))
      (assert-eql -5 (fsm 5))
      (fsm 'invert)
      (assert-eql  0 (fsm 5)))))



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

