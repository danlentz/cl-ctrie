;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM/HOST data transfer and conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cvm::*debug-flags* (list :allocate :gc :objects :ct
                             :range :check :ng :ld :gc :allocate
                             :objects :gcct :bitmap))

(defvar cvm::*check-args*  (list
                             :dump-bitmap t
                             :dump-header t
                             :dump-free t
                             :dump-allocated t))
                             
(defvar cvm:*inhibit-gc* nil)

(defun cvm:gc (memory &optional (verbose t))
  (when verbose (:printv :hr :ts :hr ""))
  (let ((heap::*gc-memory* memory)
         (heap::*debug* (when verbose cvm::*debug-flags*))
         (heap::*check-args* (when verbose cvm::*check-args*)))         
    (if cvm:*inhibit-gc*
      (warn "GC was just prevented")
      (heap::gc-collect-garbage))))

(defun cvm:gc-check (memory &optional (verbose t))
  (let1 mem (atypecase memory
              (cvm:memory it)
              (ctrie (cvm:memory-of it)))
    (let ((heap::*gc-memory* mem)
           (heap::*debug* (when verbose cvm::*debug-flags*))
           (heap::*check-args* (when verbose cvm::*check-args*)))         
      (when verbose (:printv :hr :ts :hr ""))
      (heap::gc-check  :dump-all verbose))))

(defun cvm::debug ()
  (setf heap::*debug* cvm::*debug-flags*))

(defun cvm::undebug ()
  (setf heap::*debug* nil))

(defgeneric cvm:ref   (thing &key &allow-other-keys))
(defgeneric cvm:deref (thing &key type &allow-other-keys))
(defgeneric cvm:addr  (thing &key &allow-other-keys))


(defmethod cvm:ref :around (thing &key &allow-other-keys)
  (if heap::*ld-values*
    (call-next-method)
    (heap::with-loop-detection 
      (call-next-method))))

(defmethod cvm:deref :around (thing &key type &allow-other-keys)
  (declare (ignorable type))
  (if heap::*ld-values*
    (call-next-method)
    (heap::with-loop-detection 
      (call-next-method))))

(defmethod cvm:ref :around ((thing t) &key &allow-other-keys)
  (values
    (if heap::*ld-values*
      (call-next-method)
      (heap::with-loop-detection 
        (call-next-method)))
    (cvm:memory-id heap::*gc-memory*)))

(defmethod cvm:ref ((thing t) &key &allow-other-keys)
  (heap::cfi-copy-to-common thing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last-Resort: Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric cvm:codec-for (thing)
  (:method ((thing t))           :dwim)
  (:method ((thing condition))   :clstore)
  (:method ((thing asdf:system)) :clstore))
  

(heap::cvm-define-structure serial-box ("CVM-SERIAL-BOX" . "CL-CTRIE")
  codec length octets)

;; TODO: pack octets into 64-bit word

(defun cvm:serial-box (thing)
  (heap::with-generation ()
    (let* ((codec (cvm:codec-for thing))
            (encoded-data (funcall (get-serializer codec) thing))
            (data-length  (length encoded-data))
            (serial-box   (heap::cvm-make-structure (cvm:ref 'cvm-serial-box) 3))
            (octet-vector (heap::cvm-make-vector heap::ct-unsigned-byte-8 data-length)))
      ;;                          (* 8 (ceiling data-length 8)))))
      (prog1 serial-box
        (loop for octet across encoded-data for index from 0
          do (heap::cvm-svset octet-vector index octet))
        (cvm-serial-box-set-codec serial-box (cvm:ref codec))
        (cvm-serial-box-set-length serial-box (cvm:ref data-length))
        (cvm-serial-box-set-octets serial-box octet-vector)))))

(defun cvm:serial-unbox (value)
  (heap::with-loop-detection 
    (if (not (cvm:serial-box-p value))
      (warn "~D is not a serial-box" value)
      (let* ((codec (cvm:deref (cvm-serial-box-codec value)))
              (octet-vector (cvm-serial-box-octets value))
              (payload-length (cvm:deref (cvm-serial-box-length value)))
              (encoded-data (make-array payload-length
                              :element-type '(unsigned-byte 8))))
        (loop for i from 0 below payload-length do
          (setf (aref encoded-data i) (heap::cvm-svref octet-vector i)))
        (funcall (get-deserializer (get-id-for-keyword codec)) encoded-data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM Structures 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cvm:structure-class (value)
  (if (heap::cvm-structure-p value)
    (heap::cvm-structure-ref value 0)
    (error "~D has type ~D, not ad address referencing a structure"
      value (cvm:type-of value))))

(defun cvm:host-structure-class (value)
  (heap::with-loop-detection 
    (if (heap::cvm-structure-p value)
      (heap::cfi-copy-from-common
        (heap::cvm-structure-ref value 0))
      (error "~D has type ~D, not an address referencing a structure"
        value (cvm:type-of value)))))

(defun cvm:structure-p (value)
  (heap::cvm-structure-p value))

(deftype cvm:structure ()
  '(satisfies cvm:structure-p))

(defclass cvm:reference (simple-reference)
  ((value :accessor cvm:address-of :initarg :address :initform nil)
    (mem :accessor cvm:memory-of :initarg :memory :initform nil)))

(defmethod cvm:memory-id ((thing cvm:reference))
  (cvm:memory-id (cvm:memory-of thing)))

(defun cvm:host-structure (cvm-structure)
  (heap::with-loop-detection 
    (let1 class (cvm:host-structure-class cvm-structure)
      (funcall #'make-instance (if (find-class class nil) class 'cvm:reference)
        :address cvm-structure :memory heap::*gc-memory*))))

(defmethod cvm:ref :around ((thing cvm:reference) &key)
  (let1 heap::*gc-memory* (cvm:memory-of thing)
    (values
      (call-next-method)
      (cvm:memory-id thing))))

(defmethod cvm:ref ((thing cvm:reference) &key)
  (cvm:address-of thing))


(defmethod cvm:deref ((thing cvm:reference) &key)
  thing)

(defmethod cvm:ref ((thing standard-object) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing structure-object) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing condition) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing ratio) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing double-float) &key)
  (heap::cfi-copy-to-common (coerce thing 'float)))

(defmethod cvm:ref ((thing complex) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing pathname) &key)
  (cvm:serial-box thing))

(defmethod cvm:ref ((thing array) &key)
  (cvm:serial-box thing))

(defmethod cvm:addr (thing &key)
  (heap::cvm-deref thing))

(defmethod cvm:addr ((thing cvm:reference) &key)
  (heap::cvm-deref (cvm:ref thing)))


(defun cvm:type-of (target-object)
  (if (typep target-object 'cvm:reference)
    (let1 heap::*gc-memory* (cvm:memory-of target-object)
      (heap::cvm-type-of (cvm:ref target-object)))
    (heap::cvm-type-of target-object)))



;; (defun cvm:heap-address-bounds-check (addr &key (segment heap::*gc-memory*))
;;   (let1 segment (etypecase segment
;;                   (mem:memory segment)
;;                   (fixnum     (when (< segment (length (cvm:segments-of <vm>)))
;;                                 (cvm:segment-ref (cvm:segments-of <vm>) segment))))
;;       (and (numberp addr) (typep segment 'mem:memory)
;;               (<= 0 addr (length (mem::bytes segment))))))

;; (defun cvm:heap-address-alignment-check (addr &key (segment heap::*gc-memory*))
;;   (let1 segment (etypecase segment
;;                   (mem:memory segment)
;;                   (fixnum     (when (< segment (length (cvm:segments-of <vm>)))
;;                                 (cvm:segment-ref (cvm:segments-of <vm>) segment))))
;;       (and
;;         (numberp addr)
;;         (typep segment 'mem:memory)
;;         (plusp (mem::base segment))
;;         (plusp (length (mem::bytes segment)))
;;         (zerop (mod (mem::base segment) sb-vm:n-word-bytes))
;;         (zerop (mod (length (mem::bytes segment)) sb-vm:n-word-bytes)))))
      
;; (deftype cvm:heap-address ()
;;   `(and fixnum
;;      (satisfies cvm:heap-address-bounds-check)
;;      (satisfies cvm:heap-address-alignment-check)))

;; (defun cvm:heap-address-p (value)
;;   (typep value 'cvm:heap-address))

(defun cvm:address-p (value)
  (eql (cvm:type-of value) heap::ct-address))

(defun cvm:readable-p (value)
  (eql (cvm:type-of value) heap::ct-readable))

(defun cvm:serial-box-p (value)
  (and
    (cvm:structure-p value)
    (eq  (cvm:host-structure-class value) 'cvm-serial-box)))
  
(defun cvm:symbol-p (value)
  (heap::cvm-symbol-p value))

(defun cvm:string-p (value)
  (heap::cvm-string-p value))

(defun cvm:cons-p (value)
  (heap::cvm-cons-p value))

(defmethod cvm:deref ((cvm-value fixnum) &key typecode &allow-other-keys)
  (case (or typecode (heap::cvm-type-of cvm-value))
    ((#.heap::ct-nil)              nil)
    ((#.heap::ct-t)                t)
    ((#.heap::ct-unbound)          (error "Trying to convert an unbound cvm-value"))
    ((#.heap::ct-bit)              (ldb (byte  1 0) cvm-value))
    ((#.heap::ct-character-8)      (code-char (ldb (byte  8 0) cvm-value)))
    ((#.heap::ct-character-16)     (code-char (ldb (byte 16 0) cvm-value)))
    ((#.heap::ct-character-24)     (code-char (ldb (byte 32 0) cvm-value)))
    ((#.heap::ct-character-32)     (code-char (ldb (byte 32 0) cvm-value)))
    ((#.heap::ct-signed-byte-8)    (error "(signed-byte  8) unsupported yet."))
    ((#.heap::ct-signed-byte-16)   (error "(signed-byte 16) unsupported yet."))
    ((#.heap::ct-signed-byte-24)   (error "(signed-byte 24) unsupported yet."))
    ((#.heap::ct-signed-byte-32)   (error "(signed-byte 32) unsupported yet."))
    ((#.heap::ct-signed-byte-40)   (error "(signed-byte 40) unsupported yet."))
    ((#.heap::ct-signed-byte-48)   (error "(signed-byte 48) unsupported yet."))
    ((#.heap::ct-signed-byte-56)   (heap::cvm-fixnum-value cvm-value))
    ((#.heap::ct-signed-byte-64)   (error "(signed-byte 64) unsupported yet."))
    ((#.heap::ct-unsigned-byte-8)  (ldb (byte  8 0) cvm-value))
    ((#.heap::ct-unsigned-byte-16) (ldb (byte 16 0) cvm-value))
    ((#.heap::ct-unsigned-byte-24) (ldb (byte 24 0) cvm-value))
    ((#.heap::ct-unsigned-byte-32) (ldb (byte 32 0) cvm-value))
    ((#.heap::ct-unsigned-byte-40) (ldb (byte 40 0) cvm-value))
    ((#.heap::ct-unsigned-byte-48) (ldb (byte 48 0) cvm-value))
    ((#.heap::ct-unsigned-byte-56) (ldb (byte 56 0) cvm-value))
    ((#.heap::ct-unsigned-byte-64) (ldb (byte 64 0) cvm-value))
    ((#.heap::ct-float-8)          (error "float-8 unsupported yet."))
    ((#.heap::ct-float-16)         (error "float-16 unsupported yet."))
    ((#.heap::ct-float-24)         (error "float-24 unsupported yet."))
    ((#.heap::ct-float-32)         (heap::cvm-single-float-value cvm-value))
    ((#.heap::ct-float-40)         (error "float-40 unsupported yet."))
    ((#.heap::ct-float-48)         (error "float-48 unsupported yet."))
    ((#.heap::ct-float-56)         (error "float-56 unsupported yet."))
    ((#.heap::ct-float-64)         (error "float-64 unsupported yet."))
    ((#.heap::ct-structure)        (error "Needs an address to convert a structure."))
    ((#.heap::ct-vector)           (error "Needs an address to convert a vector."))
    ((#.heap::ct-vector-fp)        (error "Needs an address to convert a vector-fp."))
    ((#.heap::ct-array)            (error "Needs an address to convert a array."))
    ((#.heap::ct-cons)             (error "Needs an address to convert a cons."))
    ((#.heap::ct-address #.heap::ct-readable) ;; TODO: readable needs post processing!
     (let ((ref-value  (heap::gc-load (heap::cvm-deref cvm-value))))
       (case (heap::cvm-type-of ref-value)
         ((#.heap::ct-cons)
          (or (heap::ld-get cvm-value)
              (let ((cons (heap::ld-put cvm-value (cons nil nil))))
                (setf (car cons) (cvm:deref (heap::cvm-car cvm-value))
                      (cdr cons) (cvm:deref (heap::cvm-cdr cvm-value)))
                cons)))
         ((#.heap::ct-structure) 
          (cond 
            ((heap::cvm-symbol-p cvm-value) 
             (or (heap::ld-get cvm-value)
                 (let  ((lsym (heap::ld-put cvm-value (heap::cfi-make-symbol cvm-value))))
                   ;; PERHAPS we don't want to copy the value of a random symbol
                   #|| (cfi-symbol-copy-from-common lsym cvm-value) ||#
                   lsym)))
            ((heap::cvm-package-p cvm-value) 
             (or (heap::ld-get cvm-value)
                 (heap::ld-put cvm-value
                         (let* ((packname (cvm:deref (heap::cvm-package-name cvm-value)))
                                (pack (find-package packname)))
                           (or pack (make-package packname #|TODO:nickname|#))))))
            ((subtypep (cvm:host-structure-class cvm-value) 'cvm:reference)
              (cvm:host-structure cvm-value))
            ((cvm:serial-box-p cvm-value)
              (or (heap::ld-get cvm-value)
                (heap::ld-put cvm-value (cvm:serial-unbox cvm-value))))
            (t           (error "unsupported structure class."))))
         ((#.heap::ct-vector #.heap::ct-vector-fp)
          (cond
            ((heap::cvm-string-p cvm-value)
             (or (heap::ld-get cvm-value)
                 (heap::ld-put cvm-value (heap::cvm-string-value cvm-value))))
            (t           (error "vector unsupported yet."))))
         ((#.heap::ct-array) (error "array unsupported yet."))
         (otherwise (cvm:deref (ldb heap::+ex-cons+ ref-value))))))
    ((#.heap::ct-free-block) (error "Trying to convert a free block at ~8,'0X"
                              (heap::cvm-deref cvm-value)))
    (otherwise         (error "Invalid type code (~D)" 
                              (or typecode (heap::cvm-type-of cvm-value))))))


(defmethod describe-object ((self cvm:reference) stream)
  (let1 heap::*gc-memory* (cvm:memory-of self)
    (printv:with-printv-output-to (stream)
      (let ((addr (cvm:addr self))
             (mem (cvm:memory-id (cvm:memory-of self)))
             (class (cvm:host-structure-class (cvm:address-of self)))
             (cell  (heap::gc-dump-object (cvm:ref self) :stream nil)))
        (:printv :hr class addr mem :hr)
        (format stream ";;;   ~A" cell)
        (:printv :hr)
        (heap::gc-dump-cell (cvm:ref self) :stream stream)
        (:printv "")
        (call-next-method)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cvm:char (character)
  (heap::cvm-form-character (char-code character)))

(defun cvm:host-char (cvm-char)
  (heap::cfi-copy-from-common cvm-char))

(defmethod cvm:ref ((thing character) &key)
  (cvm:char thing))


(defun cvm:string (string)
  (heap::cvm-make-string :contents string))

(defun cvm:host-string (cvm-string)
  (heap::cvm-string-value cvm-string))

(defun cvm:host-symbol-name (cvm-symbol)
  (cvm::host-string (heap::cvm-symbol-name cvm-symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (fdefinition 'cvm:all-packages) (fdefinition 'heap::cvm-list-all-packages))

(defun cvm:ensure-package (name)
  (let ((maybe-pkg (heap::cvm-find-package name)))
    (if (zerop maybe-pkg)
      (heap::cvm-make-package (cvm:string name))
      maybe-pkg)))

(defun cvm:package (thing)
  (etypecase thing
    (string  (cvm:ensure-package thing))
    (package (cvm:ensure-package (package-name thing)))
    (keyword (cvm:ensure-package (symbol-name thing)))))

(defun cvm:host-package (cvm-package)
  (let1 package-name (cvm:host-string (heap::cvm-package-name cvm-package))
    (or (find-package package-name) (make-package package-name))))

(defmethod cvm:ref ((thing package) &key)
  (cvm:package thing))

(defun cvm:host-all-packages ()
  (mapcar 'cvm:host-package (heap::cvm-list-all-packages)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cvm:symbol (symbol)
  (cvm:with-loop-detection
    (cvm:with-generation ()
      (let ((pkg (cvm:ensure-package (package-name (symbol-package symbol)))))
        (heap::cvm-intern (cvm::string (symbol-name symbol)) pkg)))))

(defun cvm:make-symbol (symbol &key
                         (value  heap::+cvm-unbound+ vsp)
                         (plist nil psp)
                         (function  heap::+cvm-unbound+ fsp))
  (cvm:with-loop-detection
    (cvm:with-generation ()
      (let ((pkg (cvm:ensure-package (package-name (symbol-package symbol)))))
        (aprog1 (heap::cvm-intern (cvm::string (symbol-name symbol)) pkg)
          (heap::cvm-symbol-set-value it
            (if vsp          
              (heap::cfi-copy-to-common value)
              value))
          (heap::cvm-symbol-set-function it
            (if fsp
              (let1 exp (function-lambda-expression (ensure-function function))
                (or exp heap::+cvm-unbound+))
              function))                                    
          (when psp
            (heap::cvm-symbol-set-plist it (if (and plist (heap::cvm-cons-p plist))
                                             plist
                                             (cvm:ref plist)))))))))

(defun cvm:host-symbol (cvm-symbol)
  (cvm:with-loop-detection 
    (let ((symbol-name (cvm:host-symbol-name cvm-symbol))
           (symbol-package (cvm:host-string
                             (heap::cvm-package-name
                               (heap::cvm-symbol-package cvm-symbol)))))
      (heap::cfi-symbol-copy-from-common (intern symbol-name
                                           (or (find-package symbol-package)
                                             (make-package symbol-package)))
        cvm-symbol))))


(defmethod cvm:ref ((thing symbol) &key)
  (cvm:symbol thing))


(defun cvm::symbol-plist (symbol)
  (cvm:deref (heap::cvm-symbol-plist (cvm:symbol symbol))))

(defun cvm::symbol-value (symbol)
  (cvm:deref (heap::cvm-symbol-value (cvm:symbol symbol))))

(defun (setf cvm:symbol-value) (value symbol)
  (cvm:with-loop-detection 
    (cvm:with-generation ()
      (heap::cvm-symbol-set-value (cvm:symbol symbol) (cvm:ref value)))))

(defun cvm::push-to-root (sym)
  (cvm:with-common-lock 
    (cvm:with-generation ((sym sym))
      (heap::cvm-hh-set-root heap::+gc-heap-header+
        (heap::cvm-make-cons sym (heap::cvm-hh-root heap::+gc-heap-header+))))))

(defun cvm:delete-from-root (item)
  (cvm:with-common-lock 
    (heap::cvm-hh-set-root heap::+gc-heap-header+
      (heap::cvm-list-delete-eq
        (heap::cvm-hh-root heap::+gc-heap-header+)
        item))))

(defun cvm:get (symbol indicator)
  (cvm:with-loop-detection 
    (cvm:with-generation ()
      (cvm:deref
        (cvm:car
          (cvm:cdr
            (heap::cvm-member-eq (cvm:symbol indicator)
              (cvm:ref (cvm:symbol-plist symbol)))))))))


;; ??
#+()
(defun cvm:put (symbol indicator value)
  (cvm:with-loop-detection 
    (cvm:with-generation ()
      (let* ((heapsym (cvm:ref symbol))
              (heapind (cvm:ref indicator))
              (heappl  (heap::cvm-symbol-plist heapsym))
              (maybe-found (heap::cvm-member-eq heapind heappl)))                      
        (if (not (heap::cvm-null maybe-found))
          (heap::cvm-setcdr maybe-found
            (heap::cvm-make-cons (cvm:ref value)
              (heap::cvm-pop maybe-found)))
          (heap::cvm-symbol-set-plist heapsym
            (heap::cvm-make-cons heapind
              (heap::cvm-make-cons (cvm:ref value) heappl))))))))

      
(defun cvm:symbol-set-plist (symb plist)
  (cvm:with-generation ()
    (cvm:with-loop-detection 
      (heap::cvm-symbol-set-plist (if (symbolp symb) (cvm:symbol symb) symb)
        (if (and plist (numberp plist) (cvm:cons-p plist))
          plist
          (cvm:ref plist))))))

(defun cvm::check-symbol-plists (ctrie)
  (let1 plist '(:one 1 :two 2 :three (:a "a" :b "b" :c "c"))
    (with-ctrie ctrie
      (cvm:symbol-set-plist 'cl-ctrie plist)
      (assert (equalp plist (cvm:symbol-plist 'cl-ctrie)))
      (assert (eql 1 (cvm:get 'cl-ctrie :one)))
      (assert (eql 2 (cvm:get 'cl-ctrie :two)))
      (assert (equalp (getf plist :three) (cvm:get 'cl-ctrie :three))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cvm:cons (x y)
  (heap::cvm-make-cons x y))

(defun cvm:car (cons)
  (cond
    ((heap::cvm-null cons)  heap::ct-nil)
    ((heap::cvm-cons-p cons)
      (ldb heap::+ex-cons+ (heap::gc-load (1+ (cvm:addr cons)))))
    (t (error "CVM:CAR of non-list object: ~A"
         (with-output-to-string (out)
           (heap::gc-dump-object cons :stream out))))))

(defun cvm:cdr (cons)
  (cond
    ((heap::cvm-null cons)  heap::ct-nil)
    ((heap::cvm-cons-p cons)
      (ldb heap::+ex-cons+ (heap::gc-load (cvm:addr cons))))
    (t (error "CVM:CDR of non-list object: ~A"
         (with-output-to-string (out)
           (heap::gc-dump-object cons :stream out))))))


(defun cvm:rplaca (cons value)
  (cond
;;    ((typep value 'cvm:reference) (rplaca cons (cvm:ref value)))
    ((heap::cvm-null cons)        (error "CVM:RPLACA of NIL"))
    ((heap::cvm-cons-p cons)      (heap::gc-store
                                    (1+ (cvm:addr cons))
                                    (dpb 1 heap::+in-cons+ (cvm:ref value))))
    (t
      (error "CVM:RPLACA of non-list object: ~A"
        (with-output-to-string (out)
          (heap::gc-dump-object cons :stream out))))))

(defun cvm:rplacd (cons value)
  (cond
 ;;   ((typep value 'cvm:reference) (rplacd cons (cvm:ref value)))
    ((heap::cvm-null cons)        (error "CVM:RPLACD of NIL"))
    ((heap::cvm-cons-p cons)      (heap::gc-store
                                    (heap::cvm-deref cons)
                                    (dpb 1 heap::+in-cons+ (cvm:ref value))))
    (t (error "CVM:RPLACD of non-list object: ~A"
              (with-output-to-string (out) 
                (heap::gc-dump-object cons :stream out))))))

                                    
#+()
(defun cvm-setcdr (cons value)
  (cond
    ((cvm-null cons)
     (error "CVM-SETCDR of NIL"))
    ((cvm-cons-p cons)
     (gc-store (cvm-deref cons) (dpb 1 +in-cons+ value)))
    (t (error "CVM-SETCDR of non-list object: ~A"
              (with-output-to-string (out) 
                (gc-dump-object cons :stream out))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((*package* (find-package :cvm)))
  (cl:defstruct (cvm:slot (:type cl:list))
    (cvm::name (cl:error "required") :type cl:symbol)
    (cvm::initform cl:nil :type cl:t)
    (cvm::type 'cl:t :type (cl:or cl:cons cl:symbol))
    (cvm::location (cl:error "required") :type (cl:integer 3 cl:*))
    (cvm::reader (cl:error "required") :type cl:symbol)
    (cvm::writer (cl:error "required") :type cl:symbol)))
  
(defmacro cvm:define-struct (name type &rest fields)
  ;; TODO: we could do without the defconstant for structure fields...
  (heap::wsiosbp
   `(progn
      (defun ,(intern (format nil "CVM-~A-P" name)) (self)
        (and (cvm-structure-p self)
             (= ,(1+ (length fields)) (cvm-length self))
             (= ct-t   (cvm-element-type self))
             (= ,(cond
                  ((eq 'symbol  name) '*gc-symbol*)
                  ((eq 'package name) '*gc-package*)
                  (t `(cvm-find-symbol ,(car type) 
                                       (cvm-find-package ,(cdr type)))))
                (cvm-structure-ref self 0))))
      ,@(loop
           :for field :in fields
           :for index :from 1
          :append (let ((cst (intern (format nil "+~A-~A+" name field)))
                        (get (intern (format nil "CVM-~A-~A" name field)))
                        (set (intern (format nil "CVM-~A-SET-~A" name field))))
                    (list
                     `(defconstant ,cst ,index)
                     `(defun ,get (self) (cvm-structure-ref self ,cst))
                     `(defun ,set (self value)
                        (cvm-structure-store self ,cst value))
                     `(defsetf ,get ,set)))))))
|#

#+()
(defmacro cvm-define-structure (name type &rest fields)
  ;; TODO: we could do without the defconstant for structure fields...
  (heap::wsiosbp
   `(progn
      (defun ,(intern (format nil "CVM-~A-P" name)) (self)
        (and (cvm-structure-p self)
             (= ,(1+ (length fields)) (cvm-length self))
             (= ct-t   (cvm-element-type self))
             (= ,(cond
                  ((eq 'symbol  name) '*gc-symbol*)
                  ((eq 'package name) '*gc-package*)
                  (t `(cvm-find-symbol ,(car type) 
                                       (cvm-find-package ,(cdr type)))))
                (cvm-structure-ref self 0))))
      ,@(loop
           :for field :in fields
           :for index :from 1
          :append (let ((cst (intern (format nil "+~A-~A+" name field)))
                        (get (intern (format nil "CVM-~A-~A" name field)))
                        (set (intern (format nil "CVM-~A-SET-~A" name field))))
                    (list
                     `(defconstant ,cst ,index)
                     `(defun ,get (self) (cvm-structure-ref self ,cst))
                     `(defun ,set (self value)
                        (cvm-structure-store self ,cst value))
                     `(defsetf ,get ,set)))))))
