;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Significant parts of this file are derived from a prior work, authored by    
;;; Dr. David McClain, which implemented a LispWorks-only memory-mapped file
;;; interface as part of the "MMapper" project.  Although this current source
;;; code has been reworked for full cross platform operations, and the focus of
;;; which has -- and continues to -- diverge significantly from the original
;;; work, I'd like to thank Dr. McClain for providing a really nice example from 
;;; which to learn general techniques as well as detailed architectural specifics
;;; of practical memory-map based development in Common-Lisp, and to subsequently
;;; build upon in realization of some ideas of my own.
;;;
;;; Portions of this file are Copyright (C) 2008-2010 by SpectroDynamics, LLC.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mmapper ()
  ((base    :accessor mm-base    :initarg :base  :initform +invalid-base-pointer+)
    (off    :accessor mm-off     :initarg :off   :initform 0)
    (len    :accessor mm-len     :initarg :len   :initform 0)
    (mf     :accessor mm-mf      :initarg :mf))
  (:documentation "contains particulars for a file mapping"))

(defgeneric  mmapper-valid-p (mm)
  (:documentation "non-mapper objects are never valid as mappers"))

(defmethod mmapper-valid-p (mm)
  "non-mapper objects are never valid as mappers"
  nil)

(defmethod mmapper-valid-p ((mm mmapper))
  "mapper objects are valid if their base address is not +INVALID-BASE-POINTER+"
  (not (eq (mm-base mm) +invalid-base-pointer+)))

(defun invalidate-mmapper (mm)
  "mark a mapper as invalid"
  (setf (mm-base mm) +invalid-base-pointer+))

(defun mapper-contains-range-p (mm offset len)
  "return true if the mapper mm covers the indicated file offset and length"
  (and (mmapper-valid-p mm)
       (let ((mmoff (mm-off mm)))
         (<= mmoff offset (+ offset len) (+ mmoff (mm-len mm))) )))

(defclass doubly-linked-mixin ()
  ((left    :accessor dlm-left    :initarg :left  :initform nil)
    (right  :accessor dlm-right   :initarg :right :initform nil))
  (:documentation "define a mixin behavior (DLM) to support items that
  can be stored in a doubly-linked list"))

(defstruct lru
  "least-recently-used queue of doubly-linked-mixin (DLM)
    objects Also contains an index (hashtable) of active mappers, a
    constructor function for generating new mapper objects"
  (index (make-hash-table)) ;; index by base offset of active mappings
  (count +N-PAGES+)         ;; max count of new mappings to be constructed
  constr                    ;; constructor function for new mappings
  head                      ;; LRU list head
  tail)                     ;; LRU list tail

(defun do-for-each-active-mapper (lru fn)
  "iterate fn over all active mappers"
  (maphash (lambda (k mm)
             (declare (ignore k))
             (when (mmapper-valid-p mm)
               (funcall fn mm)))
           (lru-index lru)))

(defun detach-dlm (lru mm)
  "removes a DLM object from its position in the doubly-linked list"
  (alexandria:when-let (rt (dlm-right mm))
    (let ((lf (dlm-left mm)))
      (if lf
        (setf (dlm-right lf) rt)
        (setf (lru-head lru) rt))
      (setf (dlm-left rt) lf))))

(defun reattach-dlm (lru mm)
  "reattaches a DLM object to the tail of the doubly-linked list"
  (when (dlm-right mm)
    (let ((tl (lru-tail lru)))
      (setf (dlm-left mm)  tl
            (dlm-right tl) mm
            (dlm-right mm) nil
            (lru-tail lru) mm))))

(defun reorder-lru (lru mm)
  "move mm from its current position to the tail of the LRU list"
  (detach-dlm lru mm)
  (reattach-dlm lru mm))

(defun lookup-mapper (lru base)
  "find an active mapper (if any), for the base address"
  (gethash base (lru-index lru)))

(defun create-new-mmapper (lru)
  "construct a new mmapper object and attach to the tail of the LRU list"
  (let* ((tl  (lru-tail lru))
          (elt (funcall (lru-constr lru) tl)))
    (if (lru-head lru)
      (setf
        (dlm-right tl) elt
        (lru-tail lru) elt)
      (setf
        (lru-head lru) elt
        (lru-tail lru) elt))
    elt))

(defun get-avail-mapper (lru)
  "return the oldest mapper from the doubly-linked list, if the number of
   mappers equals the limit, or else decrement the count and construct a new one"
  (if (zerop (lru-count lru))
    (lru-head lru)
    (progn
      (decf (lru-count lru))
      (create-new-mmapper lru))))

(defun get-mapper-or-oldest (lru base)
  "find an active mapper for the base address, or else return the"
  ;; oldest one in the doubly-linked list
  (or (lookup-mapper lru base)
      (get-avail-mapper lru)))

(defun remove-mapper (lru mm)
  "remove a mapper from the active directory"
  (remhash (mm-off mm) (lru-index lru)))

(defgeneric remove-all-mappers (where))

(defmethod remove-all-mappers ((lru lru))
  "remove all active mappers from the active directory"
  (clrhash (lru-index lru)))

(defun add-mapper (lru mm)
  "add a mapper to the active directory"
  (setf (gethash (mm-off mm) (lru-index lru)) mm))

(defun set-constructor (lru max-mappers fn)
  "set the mapper constructor function"
  (setf (lru-count lru)  max-mappers)
  (setf (lru-constr lru) fn))


(defclass dlm-mmapper (mmapper doubly-linked-mixin)
  ()
  (:documentation "DLM-MAPPER is a mapper object with inherited mixin behavior to
   support insertion into a doubly-linked list (the LRU mapper queue)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMapped-File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass closed-mmapped-file ()
  ((name
     :accessor mmapped-file-name
     :initarg :name)
    (fsize
      :accessor mmapped-file-size
      :initarg :fsize)
    (growby
      :accessor mmapped-file-growby
      :initform +page-size+
      :initarg :growby)
    (byte-order
      :accessor mmapped-file-byte-order
      :initform *machine-endianness*
      :initarg  :byte-order))
  (:documentation "common shared information between open and closed
  states of memory-mapped files"))


(defclass mmapped-file (closed-mmapped-file)
  ((fd
     :accessor mmapped-file-fd
     :initarg :fd
     :documentation "open file descriptor")
    (mappers
      :accessor mmapped-file-mappers
      :initform (make-lru)
      :documentation "LRU queue of mappers for this file")
    (needs-fd-close
      :accessor mmapped-file-needs-fd-close
      :initform nil
      :initarg :needs-fd-close
      :documentation "needs-fd-close -- true (non-nil) when this object was
      created by use with fd-open and so needs a corresponding fd-close")
    (lock
      :reader   mmapped-file-lock
      :initform (bt:make-recursive-lock "MMFile"))
    (cache
      :reader   mmapped-file-cache
      :initform (make-hash-table #+lispworks :weak-kind #+sbcl :weakness :value)
      :documentation "a cached of items for this file -- used by higher-level
     persistent-class protocol to avoid mmapped memory access for
     frequently used items"))
  (:documentation "augmented class describing an open mmapped file"))


(defclass mmapped-indexed-data-file (mmapped-file)
  ((header-ptr
     :accessor mmapped-file-header-ptr
     :initform nil))
  (:documentation "a mmapped-file with specialized instrumentation for management
  of data structure and allocation"))


(defmacro with-locked-mmf ((mmf) &body body)
  `(bt:with-recursive-lock-held  ((mmapped-file-lock ,mmf))
     ,@body))

;; (defmethod set-byte-order ((mf mmapped-file) byte-order)
;;   (setf (mmapped-file-byte-order mf) byte-order))

;; (defun opposite-byte-order (byte-order)
;;   (case byte-order
;;     (:little-endian :big-endian)
;;     (:big-endian    :little-endian)))

;; (defmethod flip-byte-order ((mf mmapped-file))
;;   (setf (mmapped-file-byte-order mf) (opposite-byte-order (mmapped-file-byte-order mf))))


(defmacro for-each-active-mapper ((mapper mf) &body body)
  "iterate fn over all the active mappers belonging to the memory-mapped file"
  `(do-for-each-active-mapper (mmapped-file-mappers ,mf)
     (lambda (,mapper)
       ,@body)))

(defmethod remove-all-mappers ((mf mmapped-file))
  (remove-all-mappers (mmapped-file-mappers mf)))
  
(defun ceiling-pagesize (nb)
  (let ((pgszm1 (1- +page-size+)))
    (logandc2 (+ nb pgszm1) pgszm1)))

(defun floor-pagesize (off)
  (let ((pgszm1 (1- +page-size+)))
    (logandc2 off pgszm1)))

(defun remap (mm offset len)
  "remap a mapper to cover a different file offset and length"
  (let* ((mf       (mm-mf mm))
         (mappers  (mmapped-file-mappers mf)))
    (when (mmapper-valid-p mm)
      (osicat-posix:munmap (mm-base mm) (mm-len mm))
      (remove-mapper mappers mm)
      (invalidate-mmapper mm))
    (let* ((new-offset (floor-pagesize offset))
           (new-len    (- (ceiling-pagesize (+ offset len)) new-offset)))
      ;; grow the file if needed
      (ensure-mmapped-file-size mf (+ new-offset new-len))
      ;; remap the mapper
      (setf (mm-base  mm) (get-mmap-ptr mf new-offset new-len)
            (mm-len   mm) new-len
            (mm-off   mm) new-offset)
      ;; add mapper back to index of active mappers
      (add-mapper mappers mm))))

(defun find-mapper (mf offset)
  "find a mapper for the offset into the file, or else return the
  oldest mapper object in the LRU queue"
  (let* ((mappers   (mmapped-file-mappers mf))
         (base      (floor-pagesize offset)))
    (get-mapper-or-oldest mappers base)))

(defun find-mapper-for-range (mf offset len)
  "get a mapper for the indicated offset and length into the file.
   If a mapper is not already active for this region, then grab the
   oldest mapper from the LRU queue and remap it to this region.
   while debugging -- put in some safety limits"
  (assert (<= 0 offset (max +growby-small+ (ash (the integer (mmapped-file-size mf)) 3))))
  (assert (<= 0 len +growby-large+))
  (let ((new-mm (find-mapper mf offset)))
    (unless (mapper-contains-range-p new-mm offset len)
      (remap new-mm offset len))
    new-mm))

(defmethod stream-file-handle ((stream stream))
  "return the file descriptor for the file underlying the stream"
  (osicat-sys::get-stream-fd stream))

(defun fd-file-size (fd)
  (osicat-posix:stat-size (osicat-posix:fstat fd)))

(defmethod slot-unbound (class (mmapped-file mmapped-file) (slot (eql 'fsize)))
  (fd-file-size (mmapped-file-fd mmapped-file)))

(defbitfield fd-open-flags
  (:rdonly #.osicat-posix:O-RDONLY)
  (:wronly #.osicat-posix:O-WRONLY)
  (:rdwr   #.osicat-posix:O-RDWR)
  (:append #.osicat-posix:O-APPEND)
  (:creat  #.osicat-posix:O-CREAT)
  (:trunc  #.osicat-posix:O-TRUNC)
  (:excl   #.osicat-posix:O-EXCL))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMapped File User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-mmapped-file (file-name &key (class 'mmapped-indexed-data-file) file-handle
                           (growby +page-size+) (max-mappers +N-PAGES+)
                           (byte-order *machine-endianness*)) 
  (let* ((fd (or file-handle (osicat-posix:open (osicat:native-namestring file-name)
                               (foreign-bitfield-value 'fd-open-flags '(:rdwr :creat)) #o666))))
    (unless (plusp fd) (error "Can't open mapped file: ~A" file-name))
    (make-instance class
      :name           file-name
      :fd             fd
      :growby         (ceiling-pagesize growby)
      :byte-order     byte-order
      :needs-fd-close (not file-handle)
      :max-mappers    (1- max-mappers)))) ;; leave 1 for header pointer

(defvar *mapped-fds* nil
  "list of actively managed memory-mapped files (for debugging)")

(defvar *mapped-fds-unmanaged* nil)

(defun all-open-mmapped-files ()
  *mapped-fds*)

(defun all-unmanaged-mmapped-files ()
  *mapped-fds-unmanaged*)

(defun all-mmapped-files ()
  (union *mapped-fds* *mapped-fds-unmanaged*))


(defmethod initialize-instance :after ((mf mmapped-file)
                                        &key file-handle max-mappers &allow-other-keys)
  "After-method to initialize the LRU mapper queue with a mapper
  constructor function. Also records the new mapped file in the list
  of actively managed memory-mapped files."  
  (unless file-handle (pushnew mf *mapped-fds*))
  (set-constructor (mmapped-file-mappers mf) max-mappers
    (lambda (left)
      (make-instance 'dlm-mmapper :mf mf :left left))))
        

(defun do-with-mmapped-file (filename fn &rest args)
  "for a named file, create a mapped file object with indicated
   args, and perform the function on the mapped file object. Ensure
  file is closed at exit from scope."
  (let ((mf (apply #'make-mmapped-file filename args)))
    (unwind-protect
        (funcall fn mf)
      (close-mmapped-file mf)) ))

(defmacro with-mmapped-file ((mf filename &rest args) &body body)
  `(do-with-mmapped-file ,filename (lambda (,mf)
                                     ,@body)
     ,@args))
  
(defun ensure-mmapped-file-size (mf end)
  "write a zero byte at the last position of the desired file size"
  (when (> end (mmapped-file-size mf))
    (let ((end (ceiling-pagesize (max end (+ (mmapped-file-size mf)
                                            (mmapped-file-growby mf))))))
      (with-foreign-object (octet :uint8)
        (setf (mem-aref octet :uint8) 0)
        (osicat-posix:pwrite (mmapped-file-fd mf) octet 1 (- end 1))))))

(defmethod sync ((mf mmapped-file))
  "flush updated pages to disk"
  (with-locked-mmf (mf)
    (for-each-active-mapper (mm mf)
      (osicat-posix:msync (mm-base mm) (mm-len mm) osicat-posix:ms-sync))
    (osicat-posix:fsync (mmapped-file-fd mf))))

(defmethod close-mmapped-file ((mf mmapped-file))
  "close all mmappers first, then close the file"
  (with-locked-mmf (mf)
    (setf *mapped-fds* (delete mf *mapped-fds*))
    (for-each-active-mapper (mm mf)
      (osicat-posix:munmap (mm-base mm) (mm-len mm))
      (invalidate-mmapper mm))
    (remove-all-mappers mf)
    (sync mf)
    (when (mmapped-file-needs-fd-close mf)
      (osicat-posix:close (mmapped-file-fd mf)))
    (change-class mf 'closed-mmapped-file)))

;; MAPPING-FAILURE -- a condition raised when we cannot map a region
;; of a file

(define-condition mapping-failure (error)
  ())

(defun map-failed-p (p)
  (null-pointer-p p))

(defun get-mmap-ptr (mf offset map-len)
  "return a pointer to the base (physical) address of the mapped
   region of the file. Raise MAPPING-FAILURE if it cannot be had"
  (if (> map-len 0)
    (let ((p (osicat-posix:mmap
               (cffi:null-pointer)
               map-len
               (+ osicat-posix:prot-read osicat-posix:prot-write)
               osicat-posix:map-shared
               (mmapped-file-fd mf)
               offset)))
        (when (map-failed-p p)
          (error (make-condition 'mapping-failure)))
        p)
    ;; else -- zero length mappings are invalid
    +invalid-base-pointer+))


(defun make-mapper-recent (mm)
  "make the mapper mm become the most recently used mapper"
  (let* ((mf  (mm-mf mm))
         (lru (mmapped-file-mappers mf)))
    (reorder-lru lru mm)))


(defmethod get-page-stats ((mf mmapped-file))
  "list number of mappers for various mapping lengths"
  (let ((stats nil))
    (for-each-active-mapper (mm mf)
      (let* ((len  (mm-len mm))
             (pair (assoc len stats)))
        (if pair
            (incf (cdr pair))
          (push (cons len 1) stats))))
    stats))

(defmethod describe-object :after ((thing mmapped-file) stream)
  (format stream "~&~%Memory-mapped Page Statistics:~%~%~S~%" (get-page-stats thing)))

(defgeneric mmapped-file-of (designator)
  (:method ((mmf mmapped-file))
    mmf)
  (:method ((mm  mmapper))
    (mm-mf mm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Header Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sync-header (thing)
  "flush header page to disk"
  (let ((mf (mmapped-file-of thing)))
    (with-locked-mmf (mf)
      (osicat-posix:msync (mmapped-file-header-ptr mf) +page-size+ osicat-posix:ms-sync)
      (osicat-posix:fsync (mmapped-file-fd mf)))))

(defmethod find-header ((mmapped-file mmapped-indexed-data-file))
  (mem-ref (mmapped-file-header-ptr mmapped-file) (header-type-for mmapped-file)))


(defmethod find-header ((mmapper mmapper))
  (find-header (mm-mf mmapper)))

(defun header (mf-designator)
  (find-object (find-header mf-designator) (header-type-for mf-designator)))


(defun header-unique-id (thing)
  (let* ((mf (mmapped-file-of thing))
          (ptr (inc-pointer (find-header mf) (foreign-slot-offset (header-type-for mf) 'unique-id))))
    (loop with bytes = (make-array 16 :element-type '(unsigned-byte 8))
      for i from 0 to 15 do (setf (aref bytes i) (mem-aref ptr :uint8 i))
      finally (return bytes))))
   

(defun header-value (thing slot-name)
  (foreign-slot-value (mem-ref (find-header thing) (header-type-for thing))
    (header-type-for thing) slot-name))

(defun set-header-value (thing slot-name value)
  (prog1 value
    (setf (foreign-slot-value (mem-ref (find-header thing) (header-type-for thing))
            (header-type-for thing) slot-name)
      value)
    (sync-header thing)))

(defun (setf header-value) (value thing slot-name)
  (set-header-value thing slot-name value))

(defun get-free-space-start (mf-designator)
  (header-value mf-designator 'free-space-start))

(defun set-free-space-start (mf-designator new-addr)
  (set-header-value mf-designator 'free-space-start new-addr))

;; (defun (setf free-space-start) (new-addr mf-designator)
;;   (set-free-space-start mf-designator new-addr))

(defun incf-free-space-start (mf-designator &optional (delta 1))
  (set-free-space-start mf-designator (+ (get-free-space-start mf-designator) delta)))


(defmethod initialize-instance :after ((mf mmapped-indexed-data-file) &key &allow-other-keys)
  (if (zerop (fd-file-size (mmapped-file-fd mf))) 
    ;; initialize new indexed data file    
    (let ((bytes (create-unique-id-byte-vector)))
      (ensure-mmapped-file-size mf +growby-small+)
      (setf (mmapped-file-header-ptr mf) (get-mmap-ptr mf 0 +page-size+))
      (set-free-space-start mf (header-size-of (header-type-for mf)))
      (setf (header-value mf 'cookie)  +header-cookie+)
      (setf (header-value mf 'root-class) *default-root-class*) 
      (setf (header-value mf 'version-major) +indexed-data-file-version-major+)
      (setf (header-value mf 'version-minor) +indexed-data-file-version-minor+)
      (register-indexed-data-file (mmapped-file-name mf) bytes)      
      (log:info "initializing new data file ~A: ~s" (mmapped-file-name mf) mf)
      (log:sexp :header (mmapped-file-header-ptr mf) (header mf))
      (log:sexp (get-free-space-start mf))
      (log:sexp (header-value mf 'cookie))
      (log:sexp (header-value mf 'version-major) (header-value mf 'version-minor))    
    (let ((ptr (inc-pointer (find-header mf)
                 (foreign-slot-offset 'standard-indexed-data-file-header 'unique-id))))       
        (loop for i from 0 to 15 do (setf (mem-aref ptr :uint8 i) (aref bytes i))))
      (sync-header mf))
    ;; reinstantiate existing indexed data file
    (let* ((head-ptr (get-mmap-ptr mf 0 +page-size+))
            (header (find-object (mem-ref head-ptr (header-type-for mf)) (header-type-for mf))))
      (cond
        ((not (check-cookie header))  (error (make-condition 'header-cookie-invalid
                                               :found    (cookie header)
                                               :pathname (pathname (mmapped-file-name mf)))))
        ((not (check-version header)) (error (make-condition 'file-version-mismatch
                                               :major (version-major header)
                                               :minor (version-minor header)
                                               :pathname (pathname (mmapped-file-name mf))))))
      (log:info "found valid header for file ~A: ~S" (mmapped-file-name mf) header)
      (setf (mmapped-file-header-ptr mf) head-ptr)
      (register-indexed-data-file (mmapped-file-name mf) (header-unique-id mf)))))
            
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMPTR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (mmptr (:copier nil))
  "a pointer-like object that refers to mapped memory locations."
  mapper  ;; our mapper object
  offset  ;; the offset of this pointer (measured from 0 at start of file)
  type    ;; the c-type of this pointer
  count)  ;; the number of sequential elements of type 'type' allocated


(defmethod find-header ((mmptr mmptr))
  (find-header (mmptr-mapper mmptr)))

(defmethod mmapped-file-of ((mmptr mmptr))
  (mmapped-file-of (mmptr-mapper mmptr)))


(defmacro with-locked-mmptr ((mmptr) &body body)
  `(with-locked-mmf ((mm-mf (mmptr-mapper ,mmptr)))
     ,@body))

(defmethod mapped-file-of-mmptr ((mmptr mmptr))
  "obtain the mapped file object corresponding to this pointer"
  (mm-mf (mmptr-mapper mmptr)))

(defmethod sync ((mmptr mmptr))
  (sync (mapped-file-of-mmptr mmptr)))

(defmethod close-mmapper ((mmptr mmptr))
  "close the file associated with the pointer mmptr"
  (close-mmapped-file (mapped-file-of-mmptr mmptr)))

;; (defun mmptr-byte-order (mmptr)
;;   (mmapped-file-byte-order (mapped-file-of-mmptr mmptr)))

;; (defmethod set-byte-order ((mmptr mmptr) byte-order)
;;   (set-byte-order (mapped-file-of-mmptr mmptr) byte-order))

;; (defmethod flip-byte-order ((mmptr mmptr))
;;   (flip-byte-order (mapped-file-of-mmptr mmptr)))

(defun check-limits (mmptr offset len)
  "if the current pointer's mapper does not cover the indicated region
  find or make a mapper for this region.  in any event, make the
  current mapper become the most recently used"
  (let ((mm (mmptr-mapper mmptr)))
    (unless (mapper-contains-range-p mm offset len)
      (let ((mf (mm-mf mm)))
        (setf mm (find-mapper-for-range mf offset len))
        (setf (mmptr-mapper mmptr) mm)))
    (make-mapper-recent mm)))

(defun create-mmptr (mf offset count &optional (type :uint8))
  "construct a mem-mapped pointer to the indicated region of the file"
  (assert (>= offset 0))
  (assert (>= count 0))
  (make-mmptr :mapper (find-mapper mf offset)
    :offset offset
    :count  count
    :type   type))


;; MMPTR -- various ways of creating a memory mapped file
;; object. All return a default mmptr to the indicated region of the
;; file, which defaults to its start.

(defgeneric mmptr (place-designator &rest args &key &allow-other-keys))

(defmethod mmptr ((s stream) &rest args &key (offset 0) (count 0) (type :uint8)
                          (class 'mmapped-indexed-data-file))
  "mmptr on a stream"
  (declare (ignorable class))
  (let ((mf (apply #'make-mmapped-file (pathname s) 
              :file-handle (stream-file-handle s)
              args)))
    (create-mmptr mf offset count type)))

(defmethod mmptr ((fd integer) &rest args &key (offset 0) (count 0) (type :uint8)
                          (class 'mmapped-indexed-data-file))
  "mmptr on an existing file descriptor"
  (declare (ignorable class))
  (let ((mf (apply #'make-mmapped-file nil 
              :file-handle fd
              args)))
    (create-mmptr mf offset count type)))

(defmethod mmptr ((mf mmapped-file) &key (offset 0) (count 0) (type :uint8))
  "mmptr on an already existing memory-mapped file object"
  (create-mmptr mf offset count type))

(defmethod mmptr ((fname string) &rest args &key (offset 0) (count 0) (type :uint8)
                          (class 'mmapped-indexed-data-file))
  "mmptr on a named file namestring"
  (declare (ignorable class))
  (let ((mf (apply #'make-mmapped-file fname args)))
    (create-mmptr mf offset count type)))

(defmethod mmptr ((fname pathname) &rest args &key (offset 0) (count 0) (type :uint8)
                          (class 'mmapped-indexed-data-file))
  "mmptr on a named file pathname"
  (declare (ignorable class))
  (let ((mf (apply #'make-mmapped-file fname args)))
    (create-mmptr mf offset count type)))

(defmethod mmptr ((mm mmapper) &key (offset 0) (count 0) (type :uint8))
  "mmptr on an already existing mmapped file on which this
   mapper object is defined."  
  (create-mmptr (mm-mf mm) offset count type))

(defmethod mmptr ((mmptr mmptr) &key (offset 0) (count 0) (type :uint8))
  "mmptr on an already existing mmapped file to which this
   mapped-pointer is defined"
  (let ((mf (mm-mf (mmptr-mapper mmptr))))
    (create-mmptr mf offset count type)))

(defmethod mmptr ((vector vector) &rest args &key (offset 0) (count 0) (type :uint8)
                   (class 'mmapped-indexed-data-file))
  "mmptr on a unique-id vector retrieves file from cache of
  last seen locations"
  (declare (ignorable offset count type class))
  (let ((pathname (find-indexed-data-file vector)))
    (apply #'mmptr pathname args)))

(defun do-with-mmapper (mf fn &key (offset 0) (count +page-size+) (type :uint8))
  (let ((mmptr (mmptr mf :offset offset :count count :type type)))
    (funcall fn mmptr)))


(defmacro with-mmapper ((mmptr mf) &body body)
  `(do-with-mmapper ,mf (lambda (,mmptr) ,@body)))


(defmacro with-mmptr ((mmptr mf &optional (offset 0) (type :uint8) (count 1)) &body body)
  `(do-with-mmapper ,mf (lambda (,mmptr) ,@body) :offset ,offset :count ,count :type ,type))


(defmethod find-header (default-thing)
  (find-header (mmptr default-thing)))

(defmethod mmapped-file-of (default-thing)
  (mmapped-file-of (mmptr default-thing)))
