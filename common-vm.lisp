;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :vm
  (:use :cl :sb-ext :sb-vm)
  (:export :copy-mem
    :primobj-or-lose
    :all-primitive-objects
    :sap-ref-lispobj
    :lispobj-sap
    :lispobj-ref-slot
    :real-widetag-of
    :tag-case
    :with-primobj-data
    :dump-object-fasl
    :funs-in-fun
    :make-env-list
    :lexenv-p
    :make-null-lexenv
    :copy-lexenv
    :extend-lexenv
    :get-env
    :set-env))

(in-package :vm)


(defparameter *reexport*
  '(sb-sys:sap-ref-word sb-sys:sap-ref-16 sb-sys:sap-ref-32
     sb-sys:sap-ref-8 sb-sys:sap-ref-octets sb-sys:int-sap
     sb-vm:n-word-bits sb-kernel:get-lisp-obj-address
     sb-sys:vector-sap sb-sys:vector-sap sb-sys:with-pinned-objects
     sb-vm:lowtag-mask sb-vm:primitive-object-name
     sb-vm:widetag-mask sb-vm:n-widetag-bits sb-kernel:lowtag-of
     sb-vm:primitive-object-lowtag sb-vm:primitive-object-widetag
     sb-vm:primitive-object-variable-length-p
     sb-vm:primitive-object-size sb-vm:primitive-object-slots ))

(import *reexport*)
(export *reexport*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAP Transfers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline copy-mem))
(defun copy-mem (from to length)
  (loop for i below length by sb-vm:n-word-bytes
        do (setf (sb-sys:sap-ref-word to i)
             (sb-sys:sap-ref-word from i))))
#+()
(defun count-heap-instances (class)
  (let ((stat (make-object-type-stat :type (class-name class))))
    (sb-vm::map-allocated-objects (lambda (object type-code size)
                                    (when (and (= type-code 82)
                                               (eq (class-of object) class))
                                      (incf (object-type-stat-count stat))
                                      (incf (object-type-stat-total-size stat) size)))
                                  :dynamic)
    stat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun listify (x)
  (if (listp x)
    x
    (list x)))

(defun all-primitive-objects ()
  sb-vm:*primitive-objects*)

(defun all-primitive-object-names ()
  (mapcar #'sb-vm:primitive-object-name  sb-vm:*primitive-objects*))

 ;; (SB-VM::THREAD SB-VM::COMPLEX-DOUBLE-FLOAT SB-VM::COMPLEX-SINGLE-FLOAT SYMBOL
 ;; SB-C:CATCH-BLOCK SB-VM::UNWIND-BLOCK SB-VM::BINDING SB-EXT:WEAK-POINTER
 ;; SB-VM::SAP SB-VM::VALUE-CELL SB-KERNEL:FUNCALLABLE-INSTANCE SB-KERNEL:CLOSURE
 ;; SB-VM::RETURN-PC SB-KERNEL:SIMPLE-FUN SB-KERNEL:FDEFN SB-VM::CODE VECTOR ARRAY
 ;; COMPLEX DOUBLE-FLOAT RATIO BIGNUM SB-KERNEL:INSTANCE CONS))

  
(defun primobj-or-lose (name &key (test #'eq))
  (or (find name
        sb-vm:*primitive-objects*
        :key #'sb-vm:primitive-object-name
        :test test)
    (error "Unknown primitive object type: ~A" name)))




(declaim (inline sap-ref-lispobj))
(defun sap-ref-lispobj (sap &optional (offset 0))
  (declare (type sb-sys:system-area-pointer sap)
           (type (signed-byte #.sb-vm:n-word-bits) offset))
  (sb-kernel:%make-lisp-obj (sb-sys:sap-ref-word sap offset)))


(declaim (inline lispobj-sap))
(defun lispobj-sap (x)
  (sb-sys:int-sap (sb-kernel:get-lisp-obj-address x)))


(declaim (inline lispobj-ref-slot))
(defun lispobj-ref-slot (x slot)
  (declare (type (and unsigned-byte fixnum) slot))
  (let* ((aligned-addr (logandc2 (sb-kernel:get-lisp-obj-address x)
                                 sb-vm:lowtag-mask)))
    (sap-ref-lispobj (sb-sys:int-sap aligned-addr)
                     (* sb-vm:n-word-bytes slot))))


;; (lispobj-sap *features*)
;; (lispobj-sap (make-hash-table ))


(declaim (inline real-widetag-of))
(defun real-widetag-of (x)
  (let* ((addr   (sb-kernel:get-lisp-obj-address x))
         (lowtag (logand addr sb-vm:lowtag-mask))
         (header (sb-sys:sap-ref-word (sb-sys:int-sap addr)
                                      (- lowtag))))
    (values (logand header sb-vm:widetag-mask)
            (ash header (- sb-vm:n-widetag-bits)))))


(defmacro %widetag-case ((object &optional default-op) &body specs)
  (let ((cases (make-hash-table)))
    (loop for (type . spec) in (mapcar 'listify specs)
          for case = (typecase type
                       (cons (cdr type))
                       (t (symbol-value
                           (sb-vm:primitive-object-widetag
                            (primobj-or-lose type :test #'string=)))))
          do (let ((case (if (member case '(t nil otherwise))
                           t
                           case)))
               (assert (null (gethash case cases)))
               (setf (gethash case cases)
                     (or spec `((,default-op ,type ,object))))))
    `(,(if (gethash t cases) 'case 'ecase) (real-widetag-of ,object)
       ,@(sort (loop
                 for tag being the hash-key in cases
                 using (hash-value spec)
                 collect `(,tag ,@spec))
               (lambda (x y)
                 (if (and (numberp x)
                          (numberp y))
                   (< x y)
                   (numberp x)))
               :key #'car))))


(defmacro tag-case ((object &optional default-op) &body specs)
  (let ((cases   (make-hash-table))
        (default nil)
        (_obj    (gensym "OBJ")))
    (loop for spec in (mapcar 'listify specs)
          for type = (first spec)
          for case = (typecase type
                       ((member t otherwise) type)
                       (cons                 (car type))
                       (t (symbol-value
                           (sb-vm:primitive-object-lowtag
                            (primobj-or-lose type :test #'string=)))))
          do (cond ((member case '(t otherwise nil))
                    (assert (null default))
                    (setf default (rest spec)))
                   (t (push spec (gethash case cases)))))
    `(let ((,_obj ,object))
       (,(if default 'case 'ecase) (sb-kernel:lowtag-of ,_obj)
         ,@(sort (loop
                   for tag being the hash-key in cases
                   using (hash-value specs)
                   collect `(,tag (%widetag-case (,_obj ,default-op)
                                    ,@(nreverse specs))))
                 #'< :key #'first)
         ,@(when default
             `((t ,@default)))))))


(defmacro with-primobj-data ((name obj &key data length)
                             &body body)
  (let* ((objdef       (primobj-or-lose name :test #'string=))
         (var-length-p (sb-vm:primitive-object-variable-length-p objdef))
         (_obj         (gensym "OBJ")))
    (when (and var-length-p (not data))
      (setf data (gensym "DATA")))
    `(let* ((,_obj ,obj)
            ,@(when data
                `((,data (nth-value 1 (real-widetag-of ,_obj)))))
            ,@(when length
               `((,length ,(if var-length-p
                             `(1+ ,data)
                             (sb-vm:primitive-object-size objdef))))))
       (declare (ignorable ,_obj))
       ,@body)))


(defmacro with-fasl-vector-output ((out) &body thing)
  `(let ((sb-fasl::*LOAD-CODE-VERBOSE* t))
     (flex:with-output-to-sequence (,out)
       (sb-fasl::dump-object    ,@thing
         (sb-fasl::make-fasl-output :stream ,out)))))

;; (with-fasl-vector-output (s)
;;   "string")

;; (with-fasl-vector-output (s)
;;   :keyword)

(defun dump-object-fasl (object)
  (with-fasl-vector-output (o)
    object))

;;(sb-vm:print-allocated-objects :static)
;;(sb-vm:print-allocated-objects :dynamic)


(defun lexenv-p (obj)
  (sb-c::lexenv-p obj))

(defun make-null-lexenv ()
  (sb-c::make-null-lexenv))

(defun copy-lexenv (lexenv)
  (sb-c::make-lexenv :default lexenv))

(defun extend-lexenv (lexenv &key funs vars)
  (sb-c::make-lexenv :default lexenv
		     :vars    vars
		     :funs    funs))

(defmacro get-env (&environment env)
  `(quote ,(list (copy-list (sb-c::lexenv-vars env))
		 (copy-list (sb-c::lexenv-funs env)))))

(defmacro set-env (var &environment env)
  (set var (list (copy-list (sb-c::lexenv-vars env))
		 (copy-list (sb-c::lexenv-funs env))))
  nil)


(defun funs-in-fun (fun)
  (sb-c::lexenv-funs (sb-c::functional-lexenv (cdr fun))))

(defun special-varp (var)
  (etypecase var
    (sb-c::lambda-var
     (sb-c::lambda-var-specvar var))
    (sb-c::global-var
      (eq :special (sb-c::global-var-kind var)))))

(defpackage "S"
  (:use )
  (:export #:d ; dyn-bind
           #:c ; catch
           #:f ; closure (function)
           #:_ ; ignored value
    ))

(defmacro %%make-env-list (ignore-unused-p positions
                           &environment env)
  (let* ((vars (sb-c::lexenv-vars env))
         (tmps (mapcar 'rest
                       (sort (loop
                                for position     in (reverse positions)
                                for (name . var) in (reverse vars)
                                when position
                                collect (list* position
                                               (gensym (symbol-name name))
                                               var))
                             '< :key 'first))))
    (setf (sb-c::lexenv-vars env)
          (append tmps
                  vars))
    `(list ,@(mapcar (if ignore-unused-p
                         (lambda (tmp)
                           (if (sb-c::lambda-var-refs (cdr tmp))
                               (car tmp)
                               ''s:_))
                         'car)
                     tmps))))

(defmacro make-env-list ((&key ignore-unused) &rest positions)
  (let ((x (gensym "RANDOM-BINDING")))
    `(let (,x) ;; needed to make sure we get a fresh lexenv object
       (declare (ignore ,x)) ;; just for the expression
       (%%make-env-list ,ignore-unused ,positions))))






;; (primobj-or-lose 'sb-vm::binding)
;; #S(SB-VM:PRIMITIVE-OBJECT
;;    :NAME SB-VM::BINDING
;;    :WIDETAG NIL
;;    :LOWTAG NIL
;;    :OPTIONS NIL
;;    :SLOTS (#S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME SB-VM::VALUE
;;               :DOCS NIL
;;               :REST-P NIL
;;               :OFFSET 0
;;               :OPTIONS NIL)
;;            #S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME SYMBOL
;;               :DOCS NIL
;;               :REST-P NIL
;;               :OFFSET 1
;;               :OPTIONS NIL))
;;    :SIZE 2
;;    :VARIABLE-LENGTH-P NIL)

;; (primobj-or-lose 'cons)
;; #S(SB-VM:PRIMITIVE-OBJECT
;;    :NAME CONS
;;    :WIDETAG NIL
;;    :LOWTAG SB-VM:LIST-POINTER-LOWTAG
;;    :OPTIONS NIL
;;    :SLOTS (#S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME CAR
;;               :DOCS NIL
;;               :REST-P NIL
;;               :OFFSET 0
;;               :OPTIONS (:REF-TRANS CAR :SET-TRANS SB-KERNEL:%RPLACA :INIT :ARG
;;                         :CAS-TRANS SB-KERNEL:%COMPARE-AND-SWAP-CAR))
;;            #S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME CDR
;;               :DOCS NIL
;;               :REST-P NIL
;;               :OFFSET 1
;;               :OPTIONS (:REF-TRANS CDR :SET-TRANS SB-KERNEL:%RPLACD :INIT :ARG
;;                         :CAS-TRANS SB-KERNEL:%COMPARE-AND-SWAP-CDR)))
;;    :SIZE 2
;;    :VARIABLE-LENGTH-P NIL)

;; (primobj-or-lose 'sb-kernel:closure)
;; #S(SB-VM:PRIMITIVE-OBJECT
;;    :NAME SB-KERNEL:CLOSURE
;;    :WIDETAG SB-VM:CLOSURE-HEADER-WIDETAG
;;    :LOWTAG SB-VM:FUN-POINTER-LOWTAG
;;    :OPTIONS NIL
;;    :SLOTS (#S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME SB-VM::FUN
;;               :DOCS NIL
;;               :REST-P NIL
;;               :OFFSET 1
;;               :OPTIONS (:INIT :ARG :REF-TRANS SB-KERNEL:%CLOSURE-FUN))
;;            #S(SB-VM::PRIM-OBJECT-SLOT
;;               :NAME SB-INT:INFO
;;               :DOCS NIL
;;               :REST-P T
;;               :OFFSET 2
;;               :OPTIONS NIL))
;;    :SIZE 2
;;     :VARIABLE-LENGTH-P T)
