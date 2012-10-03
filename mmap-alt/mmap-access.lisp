;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Invariants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-root-class* :root)

(defconstant +header-cookie+ #xCAFEBABE)
(defconstant +footer-cookie+ #xDEADBEEF)

(defvar *machine-endianness*
  (let ((v (cffi:foreign-alloc :int :count 1 :initial-element 1)))
    (prog1 (if (zerop (cffi:mem-aref v :uint8))
             :big-endian
             :little-endian)
      (cffi:foreign-free v))))

(defconstant +n-pages+  #+:DARWIN 256 #-:DARWIN 256
  "max nbr of mappers that can be active for each mmapped file")

(defconstant +growby-small+ #.(ash 1 20))  ;; 1 MB

(defconstant +growby-large+ #.(ash 1 24))  ;; 16 MB

(defvar +page-size+ (osicat-posix:getpagesize))

(defvar +invalid-base-pointer+ (make-pointer +page-size+)
  "used in lieu of NULL-POINTE* for invalidated mapper objects,
  to avoid difficulties with various operations on pointers that
  have a zero address FOREIGN-ARRAY-POINTER which returns NIL when
  the base address of an array is a null pointer, and similarly for
  FOREIGN-SLOT-POINTER")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique-ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-unique-id-byte-vector ()
  (or
    #+:unicly (unicly:uuid-bit-vector-to-byte-array
               (unicly:uuid-to-bit-vector
                 (unicly:make-v4-uuid)))
    #-:unicly nil
    #+:uuid   (uuid:uuid-to-byte-array (uuid:make-v4-uuid))
    #-:uuid   nil
    (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for i from 0 to 15
        do (setf (aref bytes i) (random 255)))
      bytes)))

(defun create-null-id-byte-vector ()
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

(defun byte-vector-to-hex-string (vector)
  (with-output-to-string (out)
    (loop for byte across vector do (format out "~2,'0x" byte))))

(defun hex-string-to-byte-vector (string)
  (let ((len (length string))
         (*read-base* 16))
    (loop 
      with bytes = (make-array (ceiling (/ len 2)) :element-type '(unsigned-byte 8))
      for i from 0 by 2    
      for j from 0 to (ceiling (/ len 2))
      while (< i (1- len))
      do (setf (aref bytes j) (read-from-string string nil 0 :start i :end (+ 2 i)))
      finally (return bytes))))

(defun test-byte-vector-hex-string-roundrip ()
  (let* ((bv0 (create-unique-id-byte-vector))
          (bv1 (hex-string-to-byte-vector (byte-vector-to-hex-string bv0))))
    (assert (equalp bv0 bv1)) 
    (values bv0 bv1)))

;; (test-byte-vector-hex-string-roundrip)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;
;; (test-byte-vector-hex-string-roundrip)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDEXED-DATA-FILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *indexed-data-file-pathname-cache-directory*
  (ensure-directories-exist 
    (merge-pathnames #p".cache/indexed-data-file-pathnames/" (user-homedir-pathname))))

(defvar *indexed-data-files* (make-hash-table :test #'equalp
                               #+sbcl :weakness #+lispworks :weak-type
                               #+(or sbcl lispworks) :value))

(defgeneric register-indexed-data-file (thing unique-id)
  (:method ((thing pathname) (unique-id vector))
    (setf (gethash unique-id *indexed-data-files*) thing)
    (let ((fn (merge-pathnames (byte-vector-to-hex-string unique-id)
                           *indexed-data-file-pathname-cache-directory*)))
      (setf (pointer:deref fn) (prin1-to-string thing))
      ;; (log:info "registered ~S and cached pathname to ~S"
      ;;   (read-from-string (pointer:deref fn)) fn) 
      )))

(defun find-indexed-data-file (unique-id)
  (let ((found (or
                 (gethash unique-id *indexed-data-files*)
                 (let ((fn (merge-pathnames (byte-vector-to-hex-string unique-id)
                             *indexed-data-file-pathname-cache-directory*)))    
                   (when (probe-file fn)
                     (read-from-string (pointer:deref fn)))))))
    (cond
      ((not found)
        (error "no cached reference or open file with unique-id ~A.~%"
          (byte-vector-to-hex-string unique-id)))
      ((not (probe-file found))
        (error "unable to locate data file with unique-id ~A last seen at ~S."
          (byte-vector-to-hex-string unique-id) found))
      (t found))))
    
(defvar +indexed-data-file-version-major+ 1)
(defvar +indexed-data-file-version-minor+ 0)

(defgeneric header-type-for (storage-designator)
  (:method (default) 'standard-indexed-data-file-header))

(defun header-size-of (storage-designator)
  (cffi:foreign-type-size (header-type-for storage-designator)))

(defgeneric footer-type-for (storage-designator)
  (:method (default) 'standard-indexed-data-file-footer))

(defun footer-size-of (storage-designator)
  (cffi:foreign-type-size (footer-type-for storage-designator)))

#+()
(define-condition data-file-error (file-error)
  ((filename :initarg :filename :reader filename)))

(define-condition file-corruption (file-error)
  ())

(define-condition file-version-mismatch (file-error)
  ((version-major-found :initarg :major :initarg :version-major-found :reader version-major-found)
    (version-major-found :initarg :minor :initarg :version-minor-found :reader version-minor-found)
    (version-major-expected :initarg :version-major-expected :reader version-major-expected))
  (:default-initargs :version-major-expected +indexed-data-file-version-major+)
  (:report (lambda (c stream)
             (format stream "Expected version ~D.x Found version ~D.~D"
               (version-major-expected c) (version-major-found c) (version-minor-found c)))))

(defgeneric check-version (object &optional expected-major)
  (:method ((object standard-indexed-data-file-header)
             &optional (expected-major +indexed-data-file-version-major+))
    (let ((found-major (version-major object)))
      (eql found-major expected-major))))

(defun report-invalid-cookie (stream filename found expected)
  (format stream "File: ~A~%  Invalid cookie ~x (looking for ~x)"
    filename found expected))

(define-condition cookie-invalid (file-corruption)
  ((cookie-found :initarg :found :initarg :cookie-found :reader cookie-found)
    (cookie-expected :initarg :expected :initarg :cookie-expected :reader cookie-expected))
  (:report (lambda (c stream)
             (report-invalid-cookie stream
               (file-error-pathname c) (cookie-found c) (cookie-expected c)))))

(define-condition header-cookie-invalid (cookie-invalid)
  ()
  (:default-initargs :expected +header-cookie+))

(define-condition footer-cookie-invalid (cookie-invalid)
  ()
  (:default-initargs :expected +footer-cookie+))

(defgeneric check-cookie (object)
  (:method ((object standard-indexed-data-file-header))
    (eql (cookie object) +header-cookie+))
  (:method ((object standard-indexed-data-file-footer))
    (eql (cookie object) +footer-cookie+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed-Pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric pointer-address (pointer-designator))

(defmethod  pointer-address (pointer)
  (cffi:pointer-address pointer))

(defmethod  pointer-address ((pointer sb-alien:system-area-pointer))
  (sb-sys:sap-int pointer))

(defmethod  pointer-address ((pointer integer))
  pointer)

(defmethod  pointer-address ((pointer typed-pointer))
  (element-offset pointer))

(defgeneric set-pointer-address (addr pointer-designator))

(defmethod  set-pointer-address (addr (pointer typed-pointer))
  (setf (element-offset pointer) (pointer-address addr)))

(defgeneric pointer-element-type (pointer-designator))

(defmethod  pointer-element-type ((pointer sb-alien:system-area-pointer))
  (warn "assuming element-type ~S for untyped pointer ~S" :uint8 pointer)
  :uint8)

(defmethod  pointer-element-type ((pointer typed-pointer))
  (element-type pointer))

(defgeneric pointer-element-size (pointer-designator))

(defmethod  pointer-element-size (pointer)
  (cffi:foreign-type-size (pointer-element-type pointer)))

(defmethod  pointer-element-size ((pointer typed-pointer))
  (cffi:foreign-type-size (element-type pointer)))

(defgeneric pointer-element-count (pointer-designator))

(defmethod  pointer-element-count ((pointer typed-pointer))
  (element-count pointer))
















#|
(define-foreign-type parray (freeable)
  ((element-type :initarg :type :accessor element-type)
    (mmapped-file-designator :initarg :mf :accessor mf))
  (:actual-type :pointer))

(define-parse-method parray (type &key free mf)
  (make-instance 'parray :type type :free free :mf mf))

(defmethod translate-to-foreign (value (parray parray))
  (if (pointerp value)
      value
      (let* ((length (length value))
              (type (element-type parray))
              (mf (mf parray))
              (res (mmptr->ptr (mmalloc mf type :count (+ 1 length)))))
        (dotimes (i length (values res t))
          (setf (mem-aref res type i) (elt value i)))
        (setf (mem-aref res :pointer length) (null-pointer))
        res)))

(defmethod translate-from-foreign (ptr (parray parray))
  (let* ((res nil)
         (el-type (element-type parray)))
    (do ((i 0 (+ i 1))) ((null-pointer-p (mem-aref ptr :pointer i)))
      (push (mem-aref ptr el-type i) res))
    (coerce (nreverse res) 'array)))

(define-foreign-type pvector (freeable)
  ((mmapped-file-designator :initarg :mf :accessor mf)
    (element-type :initarg :type :accessor element-type))
  (:actual-type :pointer))

(define-parse-method pvector (type &key mf free)
  (make-instance 'pvector :mf mf :type type :free free))

(defmethod translate-to-foreign (value (pvector pvector))
  (if (pointerp value)
    value
    (let* ((length (length value))
            (type  (element-type pvector))
            (res   (mmptr->ptr (mmalloc (mf pvector) type :count length))))
      (dotimes (i length (values res t))
        (setf (mem-aref res type i) (elt value i)))
      res)))

(defmethod translate-from-foreign (ptr (pvector pvector))
  (let ((length (mem-ref *array-length* :uint)))
    (let* ((res (make-array *array-length*))
            (eltype (element-type pvector)))
      (dotimes (i length)
        (setf (aref res i) (mem-aref ptr eltype i)))
      res)))

(time (translate-to-foreign (hu.dwim.serializer:serialize *features*)
        (make-instance 'parray :type :uint8 :mf xoom)))
            
(defcstruct* xy
  (pv (pvector :uint8 :mf xoom)))
(make-instance 'xy :pv #(0 1 2 3 4 5 6 7 8 9))

(defcstruct* pcons
  (pcar typed-pointer)
  (pcdr typed-pointer))

;; notyet

(defgeneric deref (pointer &key type index))

(defmethod deref ((pointer typed-pointer) &key type (index 0))
  (cffi:mem-aref (ref pointer) (or type (element-type pointer)) index))

(defgeneric (setf deref) (value pointer &key type index))

(defun incf-fill-pointer (base delta) 
  (incf-pointer (mem-ref base :pointer) delta))

(make-typed-pointer nil :int8)
(object-by-id 'T2)
|#
