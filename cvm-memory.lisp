;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer cvm (allocation-group)
  ()
  (:metaclass grouped-layer-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM:MEMORY File Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * octets (#x00 to #x7F) reserved for file header
;; * cvm heap begins at #x80, which contains gc-cookie (#xDEADBEEFCAFE0000)
;; * presently header only using bytes 0-47 (#x00 to #x2f)
;;
;;   - #x000000: cvm:+cookie+ (#xFFAA204549525443) (file begins with text "CTRIE ")
;;   - #x000008: tagged cvm-address of actual ctrie cvm-structure in this heap
;;               which can be easily dereferenced to instantiate host cvm-ctrie
;;               instance
;;   - #x000010: major version identifier for this heap file format
;;   - #x000010: minor version identifier for this heap file format
;;   - #x000018: 16 octet v4-uuid uniquely identifies this heap without regard to
;;               physical location.  This "fingerprint" is used to support a
;;               flexible, location-agnostic inter-heap adressing strategy.
;;   - #x000030: unused
;;   - #x000080: cvm gc magic-cookie (#xDEADBEEFCAFE0000)
;;   - #x000088: cvm gc address 0 (nil)
;;
;; * Example:
;;
;;      0000000 43 54 52 49 45 20 aa ff a6 01 00 00 00 00 00 24
;;      0000010 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;;      0000020 81 70 31 0d ef 90 41 e7 b0 80 e8 60 5e ea fe eb
;;      0000030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;;      ::::::: :: :: :: :: :: :: :: :: :: :: :: :: :: :: :: ::
;;      0000080 00 00 fe ca ef be ad de 00 00 00 00 00 00 00 00
;;      0000090 08 00 00 00 00 00 00 20 06 00 00 00 00 00 00 01
;;      00000a0 c7 00 00 00 00 00 00 24 fe 07 00 00 00 00 00 0e
;;      00000b0 0a 00 00 00 00 00 00 24 86 01 00 00 00 00 00 24
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf heap::*gc-magic-cookie* #xdeadbeefcafe0000)


(defvar cvm:+cookie+           #xffaa204549525443)
(defvar cvm:+version-major+    1)
(defvar cvm:+version-minor+    0)
(defvar cvm:+page-size+        (osicat-posix:getpagesize))
(defvar cvm:+header-size+      (/ cvm:+page-size+ 8))
(defvar cvm:+growby+           2)
(defvar cvm:*memory-pathnames* (make-hash-table :test 'equal :synchronized t))
(defvar cvm:*memory-ids*       (make-hash-table :test 'equalp :synchronized t))
(defvar cvm::*memories-lock*   (bt:make-lock))

(defvar cvm:*default-storage-directory* (merge-pathnames #p".ctrie-store/"
                                          (user-homedir-pathname)))

(defun cvm::make-cas-lock ()
  (cons nil nil))

(unintern 'heap::*gc-heap-size* :heap)

;; (define-symbol-macro heap::*gc-heap-size* (/ (mem:size heap::*gc-memory*) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM VM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass cvm:memory (mem:memory)
  ((cvm::pathname :accessor cvm:memory-pathname :initarg :pathname)
    (cvm::stream :accessor cvm:memory-stream :initarg :stream)
    (cvm::fd :accessor cvm:memory-fd :initarg :fd :initform -1)
    (cvm::lock :accessor cvm:memory-lock :initform (sb-thread:make-mutex :name "admin"))
    (cvm::gate :accessor cvm:memory-gate :initform (sb-concurrency:make-gate :name "user"))
    (cvm::gc :accessor cvm:memory-gc :initform (cvm::make-cas-lock))
    (cvm::sap :accessor cvm:memory-sap :initarg :sap)
    (cvm::id :accessor cvm:memory-id :initarg :id)))

(defmethod mem:memory-prolog ((mem cvm:memory))
  (heap::when-debug (:gc :allocate :gcct)
    (:printv "preparing for exclusive memory access")
    (:printv (cvm:memory-id mem)))  ;; printv:*printv-output*))
  (sb-concurrency:wait-on-gate (cvm:memory-gate mem))
  (call-next-method))

(defmethod mem::memory-operate ((mem cvm:memory) thunk)
  (sb-thread:with-mutex ((cvm:memory-lock mem))
    (sb-concurrency:close-gate (cvm:memory-gate mem))
    (let1 heap::*gc-memory* mem
      (heap::when-debug (:gc :allocate :gcct)
        (:printv :GATE-CLOSED))
      (call-next-method))))
    
(defmethod mem:memory-epilog ((mem cvm:memory))
  (sb-concurrency:open-gate (cvm:memory-gate mem))
    (heap::when-debug (:gc :allocate :gcct)
      (:printv :GATE-OPEN))
  (call-next-method))

(defmacro cvm:when-ready ((memory) &body body)
  (with-gensyms (mem)
    `(let ((,mem ,memory))
       (sb-concurrency:wait-on-gate (cvm:memory-gate ,mem))
       (assert (plusp (sb-sys:sap-int (cvm:memory-sap ,mem))))
       ,@body)))

(defun mem::fd-file-length (fd)
  (osicat-posix:stat-size (osicat-posix:fstat fd)))

(defmethod mem:valid-address-p ((mem cvm:memory) addr)
  (<= (mem:base mem) addr (+ (mem:base mem) (1- (* 8 (mem:size mem))))))
  
(defmethod mem:peek-uint64 ((mem cvm:memory) addr)
  (assert (zerop (mod addr 8)))
  (assert (mem:valid-address-p mem addr))
  (sb-sys:sap-ref-64
    (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
    (- addr (mem:base mem))))

(defmethod mem:peek-uint32 ((mem cvm:memory) addr)
  (assert (zerop (mod addr 4)))
  (assert (mem:valid-address-p mem addr))
  (sb-sys:sap-ref-32
    (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
    (- addr (mem:base mem))))

(defmethod mem:peek-uint16 ((mem cvm:memory) addr)
  (assert (zerop (mod addr 2)))
  (assert (mem:valid-address-p mem addr))
  (sb-sys:sap-ref-16
    (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
    (- addr (mem:base mem))))

(defmethod mem:peek-uint8 ((mem cvm:memory) addr)
  (assert (mem:valid-address-p mem addr))
  (sb-sys:sap-ref-8
    (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
    (- addr (mem:base mem))))

(defmethod mem:poke-uint64 ((mem cvm:memory) addr value)
  (assert (zerop (mod addr 8)))
  (assert (mem:valid-address-p mem addr))
  (setf (sb-sys:sap-ref-64
          (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
          (- addr (mem:base mem)))
    value))

(defmethod mem:poke-uint32 ((mem cvm:memory) addr value)
  (assert (zerop (mod addr 4)))
  (assert (<= 0 value #.(1- (expt 2 32))))
  (assert (mem:valid-address-p mem addr))
  (setf (sb-sys:sap-ref-32
          (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
          (- addr (mem:base mem)))
    value))

(defmethod mem:poke-uint16 ((mem cvm:memory) addr value)
  (assert (zerop (mod addr 2)))
  (assert (<= 0 value #.(1- (expt 2 16))))
  (assert (mem:valid-address-p mem addr))
  (setf (sb-sys:sap-ref-16
          (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
          (- addr (mem:base mem)))
    value))

(defmethod mem:poke-uint8 ((mem cvm:memory) addr value)
  (assert (<= 0 value #.(1- (expt 2 8))))
  (assert (mem:valid-address-p mem addr))
  (setf (sb-sys:sap-ref-8
          (sb-sys:sap+ (cvm:memory-sap mem) cvm:+header-size+)
          (- addr (mem:base mem)))
    value))

(defun unpacked-octets (vec)
  (aprog1 (make-array (* 8 (length vec)) :element-type '(unsigned-byte 8))           
    (loop for index from 0 below (length vec)
      do (loop for offset from 0 below 8
           do (setf
                (aref it (+ (* 8 index) offset))
                (ldb (byte 8 (* 8 offset)) (aref vec index)))))))

(defun word-ref (vec index)
  (loop for i from index below (+ index 8)
    sum (ash (aref vec i) (* (mod i 8) 8))))

(defun packed-octets (vec)
  (assert (eql 0 (mod (length vec) 8)))
  (make-array (/ (length vec) 8) :element-type '(unsigned-byte 64)
    :initial-contents (loop for i from 0 below (length vec) by 8
                        collect (word-ref vec i))))

(defun test-packed-octets-roundrip ()
  (loop repeat 100
    do (assert (let1 id (create-unique-id-byte-vector)
                 (equalp id (unpacked-octets (packed-octets id)))))))

(defgeneric cvm:find-memory (place))

(defmethod cvm:find-memory ((place null))
  place)

(defmethod cvm:find-memory ((id vector))
  (cvm:find-memory (gethash id cvm:*memory-ids*)))

(defmethod cvm:find-memory ((pathname pathname))
  (when (probe-file pathname)
    (or (gethash (truename pathname) cvm:*memory-pathnames*)
      (setf (gethash (truename pathname) cvm:*memory-pathnames*)
        (let* ((file (open pathname :direction :io :if-exists :append))
                (sap (sb-posix:mmap nil (file-length file)
                      ;; (* 2 (file-length file))
                       (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
                       sb-posix:map-shared (sb-sys:fd-stream-fd file) 0)))
          (assert (eql (sb-sys:sap-ref-64 sap 0) cvm:+cookie+))
          (assert (eql (sb-sys:sap-ref-64 sap 16) cvm:+version-major+))
          (assert (eql (sb-sys:sap-ref-64 sap 24) cvm:+version-minor+))
          (let* ((id-low (sb-sys:sap-ref-64 sap 32))
                  (id-high (sb-sys:sap-ref-64 sap 40))
                  (id (unpacked-octets (vector id-low id-high))))
            (setf (gethash id cvm:*memory-ids*) (truename pathname))
            (make-instance 'cvm:memory :pathname (truename pathname)
              :fd (sb-sys:fd-stream-fd file)
              :size (- (file-length file) (* cvm:+header-size+ 8))
              :stream file :id id :sap sap :base ;; (sb-sys:sap-int sap)
              (ash 1 32))))))))

(defun cvm:extend-memory (memory)
  (let* ((heap::*gc-memory* memory)
          (fd (cvm:memory-fd memory))
          (sz (mem::fd-file-length fd))
          (len  (mem:size memory))
          (end (* 2 sz)))
    (:printv
    (cffi:with-foreign-object (octet :uint8)
      (setf (cffi:mem-aref octet :uint8) 0)
      (osicat-posix:pwrite fd octet 1 (- end 1)))
    (setf  (slot-value memory 'mem:size) (- (* 2 sz) (* 8 cvm:+header-size+)))
    (heap::cvm-hh-set-size heap::+gc-heap-header+
      (heap::cvm-form-fixnum (/  (slot-value memory 'mem:size) 8)))
    (sb-posix:mmap  (sb-sys:sap+ (cvm:memory-sap memory) sz) sz
      (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
      (boole boole-ior sb-posix:map-shared sb-posix:map-fixed) fd sz)
    (heap::gc-store (/ len 8) (heap::cvm-form-free-block (/ (- (mem::fd-file-length fd) sz) 8) 0))
    (heap::fb-insert (heap::cvm-deref (heap::cvm-hh-free-blocks heap::+gc-heap-header+)) (/ len 8))
    
    )))

(defun cvm:create-memory (pathname pages)
  (with-open-file (map-file pathname :direction :io :element-type '(unsigned-byte 64)
                    :if-does-not-exist :create :if-exists :supersede)
    (write-byte cvm:+cookie+ map-file) ;; magic cookie
    (write-byte 0 map-file)            ;; placeholder for cvm-ctrie instance
    (write-byte cvm:+version-major+ map-file)
    (write-byte cvm:+version-minor+ map-file)
    (let1 id (create-unique-id-byte-vector)
      (write-byte (word-ref id 0) map-file)
      (write-byte (word-ref id 8) map-file))
    (loop for addr from (file-position map-file) below (/ cvm:+header-size+ 8)
      do (write-byte 0 map-file))
    (loop for addr from (/ cvm:+header-size+ 8) below (* pages 512)
      do (write-byte 0 map-file))
    (force-output map-file))
  (aprog1 (cvm:find-memory pathname)
    (sb-concurrency:open-gate (cvm:memory-gate it))
    (let1 heap::*gc-memory* it
      (heap:common-initialize it)
      (sb-posix:msync (cvm:memory-sap it)
        (file-length (cvm:memory-stream it)) sb-posix:ms-sync))))

(defun cvm:memory (pathname)
  (bt:with-lock-held (cvm::*memories-lock*)
    (let1 heap::*gc-memory* nil
      (or
        (cvm:find-memory pathname)
        (cvm:create-memory pathname 32)))))

(defun cvm:heap-sap (thing &optional (addr 0))
  (let1 heap-start-sap (typecase thing
                         (cvm:memory (sb-sys:sap+ (cvm:memory-sap thing) cvm:+header-size+))
                         (cvm-ctrie  (cvm:heap-sap (mem-of thing))))
    (sb-sys:sap+ heap-start-sap (* addr 8))))


(defun cvm::dump-line (ptr start width count)
  (let ((bytes (loop for i below count
                     collect (cffi:mem-ref ptr :uint8 (+ start i)))))
    (format t "~&~8,'0X ~{~2,'0X ~}~vt| ~{~C~}"
            start
            bytes
            (+ 10 (* width 3))
            (loop for byte in bytes
                  for char = (code-char byte)
                  collect (if (graphic-char-p char)
                              char
                              #\.)))))

(defun cvm::pprint-mem (ptr count width)
  (loop for i below count by width
     do (cvm::dump-line ptr i width (min width (- count i))))
  (terpri))


