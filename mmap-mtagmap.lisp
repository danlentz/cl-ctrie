;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)


(defun mtagmap-byte (mtagmap index)
  (declare (type mindex index))
  (d (mtagmap-ptr mtagmap) index))


(defun (setf mtagmap-byte) (val mtagmap index)
  (declare (type mindex index) (type fixnum val))
  (setf (d (mtagmap-ptr mtagmap) index) (logand #xff val)))


(declaim (ftype (function (mtagmap mindex) word) mtagmap-word))
(defun mtagmap-word (mtagmap windex)
  (declare (type mindex windex))
  (d (mtagmap-ptr mtagmap) windex word))


(declaim (ftype (function (word mtagmap mindex) word) (setf mtagmap-word)))
(defun (setf mtagmap-word) (val mtagmap windex)
  (declare (type mindex windex))
  (declare (type (unsigned-byte 64) val))
  (setf (d (mtagmap-ptr mtagmap) windex word) val))


(defmacro mtagmap-next (mtagmap)
  `(mtagmap-word ,mtagmap 0))


(defun mtagmap-first-index (mtagmap)
  (declare (ignore mtagmap))
  +word-length+)


(defun mtagmap-last-index (mtagmap)
  (mtagmap-next mtagmap))


(defun mtagmap-elem-pos (mtagmap index)
  (/ (- index (mtagmap-first-index mtagmap)) (mtagmap-elem-len mtagmap)))


(defun mtagmap-elem-pos-to-index (mtagmap pos)
  (+ (mtagmap-first-index mtagmap) (* (mtagmap-elem-len mtagmap) pos)))


(defun mtagmap-count (mtagmap)
  (if (zerop (mtagmap-elem-len mtagmap))
      0
      (/ (- (mtagmap-last-index mtagmap) (mtagmap-first-index mtagmap))
	 (mtagmap-elem-len mtagmap))))


(defun round-up-to-pagesize (bytes)
  (let ((pagesize (osicat-posix:getpagesize)))
    (* pagesize (max 1 (ceiling bytes pagesize)))))


(defun mtagmap-finalize (m)
  (check-type (mtagmap-class m) mm-metaclass)
  (setf (mtagmap-instantiator m) (mm-metaclass-custom-function (mtagmap-class m) 'instantiator)
	(mtagmap-walker m)       (mm-metaclass-custom-function (mtagmap-class m) 'walker)
	(slot-value (mtagmap-class m) 'mtagmap) m
	(mtagmap-elem-len m)     (mm-metaclass-len (mtagmap-class m)))
  (check-type (mtagmap-instantiator m) function)
  (check-type (mtagmap-walker m) (or null function))
  (when (mtagmap-closed-p m)
    (setf (mtagmap-layout m) (mm-metaclass-slot-layout (mtagmap-class m))))
  (mtagmap-check m))


(defun mtagmap-check (m)
  (cond ((mtagmap-closed-p m)
	 (assert (cffi:null-pointer-p (mtagmap-ptr m)))
	 (assert (zerop (mtagmap-len m))))
	(t
	 (assert (not (cffi:null-pointer-p (mtagmap-ptr m))))
	 (assert (>= (mtagmap-next m) (mtagmap-first-index m)))
	 (assert (>= (mtagmap-len m) (mtagmap-next m)))))
  (let ((class (mtagmap-class m)))
   (when class
     (check-type class mm-metaclass)
     (assert (layout-compatible-p (mtagmap-layout m) (mm-metaclass-slot-layout class)))
     #-(and) (assert (eq (mtagmap (mm-metaclass-tag class)) m))
     #-(and) (assert (eq (mm-metaclass-mtagmap class) m))))
  m)


(defun fd-file-length (fd)
  (osicat-posix:stat-size (osicat-posix:fstat fd)))


(defun mtagmap-file-length (mtagmap)
  (assert (not (mtagmap-closed-p mtagmap)))
  (fd-file-length (mtagmap-fd mtagmap)))


(defun check-allocate-okay ()
  (assert *mmap-may-allocate*))


(defun check-mmap-truncate-okay ()
  (assert (not (zerop (logand osicat-posix:MAP-SHARED *mmap-sharing*))))
  (check-allocate-okay))


(defun mtagmap-default-filename (mtagmap)
  (mm-metaclass-pathname (mtagmap-class mtagmap)))


(defun mtagmap-open (mtagmap &key (file (mtagmap-default-filename mtagmap)) (finalize t)
                      (min-bytes 0) (sharing *mmap-sharing*) (protection *mmap-protection*))
  (assert (mtagmap-closed-p mtagmap))
  (incf min-bytes +word-length+)
  (setf min-bytes (round-up-to-pagesize min-bytes))
  (when finalize  (mtagmap-finalize mtagmap))
  (let ((fd (osicat-posix:open file (logior osicat-posix:O-CREAT osicat-posix:O-RDWR))))
    (unwind-protect (let ((bytes (fd-file-length fd)))
                      (when (> min-bytes bytes)
                        (check-mmap-truncate-okay)
                        (osicat-posix:ftruncate fd min-bytes)
                        (setf bytes min-bytes))
                      (assert (>= bytes +word-length+))	   
                      (let ((ptr (osicat-posix:mmap (cffi:null-pointer) bytes
                                   protection sharing fd 0)))
                        (unwind-protect
                          (let ((new-mtagmap (make-mtagmap :fd fd :ptr ptr :len bytes)))
                            (when (zerop (mtagmap-next new-mtagmap))
                              (setf (mtagmap-next new-mtagmap) +word-length+))
                            (mtagmap-check new-mtagmap)
                            (setf
                              (mtagmap-fd mtagmap) fd
                              (mtagmap-ptr mtagmap) ptr
                              (mtagmap-len mtagmap) bytes
                              fd nil ptr nil))
                          (when ptr (osicat-posix:munmap ptr bytes)))))
      (when fd (osicat-posix:close fd))))
  mtagmap)


(defun mtagmap-resize (mtagmap new-len)
  (assert (not (mtagmap-closed-p mtagmap)))
  (check-mmap-truncate-okay)
  (symbol-macrolet ((len (mtagmap-len mtagmap)))
    (flet ((trunc ()
	     (osicat-posix:ftruncate (mtagmap-fd mtagmap) new-len))
            (remap ()
              #+linux
              (progn
                ;; linux supports MREMAP
                (setf (mtagmap-ptr mtagmap) (osicat-posix:mremap (mtagmap-ptr mtagmap)
                                              len new-len osicat-posix:MREMAP-MAYMOVE))
                (setf (mtagmap-len mtagmap) new-len))
              #-linux
              (progn
                ;; others require MUNMAP/MMAP
                (osicat-posix:munmap (mtagmap-ptr mtagmap) len)
                (setf (mtagmap-ptr mtagmap) (osicat-posix:mmap (cffi:null-pointer)
                                              new-len *mmap-protection*
                                              *mmap-sharing* (mtagmap-fd mtagmap) 0))
                (setf (mtagmap-len mtagmap) new-len))))
      (let (done)
	(unwind-protect (progn
                          (cond
                            ((> len new-len) (remap) (trunc))
                            (t               (trunc) (remap)))
                          (setf done t))
	  (unless done (mtagmap-close mtagmap))))))  
  (mtagmap-check mtagmap))


(defun mtagmap-extend-alloc (mtagmap bytes)
  (check-type bytes mindex)
  (let ((len (mtagmap-len mtagmap)))
    (let ((next (mtagmap-next mtagmap)) (new-len (* 2 len)))
      (assert (> len 0))
      (assert (>= len next))
      (check-type next mindex)
      (mtagmap-check mtagmap)
      (loop while (> (+ next bytes) new-len)
	    do (setf new-len (* 2 new-len)))
      (mtagmap-resize mtagmap new-len))))


(defun mtagmap-alloc (mtagmap bytes)
  (declare (type mindex bytes))
  (check-allocate-okay)
  (symbol-macrolet ((len (mtagmap-len mtagmap)))
    (when (zerop len) (mtagmap-open mtagmap))
    (let ((next (mtagmap-next mtagmap)))
      (declare (type mindex next))
      (when (> (the mindex (+ next bytes)) (the mindex len))
	(mtagmap-extend-alloc mtagmap bytes))
      (setf (mtagmap-next mtagmap) (the mindex (+ next bytes)))
      next)))


(defun mtagmap-check-read (mtagmap)
  (loop for i below (mtagmap-len mtagmap)
	summing (mtagmap-byte mtagmap i)))


(defun mtagmap-check-invert (mtagmap)
  (loop for i below (mtagmap-len mtagmap)
	for c = (mtagmap-byte mtagmap i)
	do (setf (mtagmap-byte mtagmap i) (lognot c))))


(defun mtagmap-check-write (mtagmap)
  (mtagmap-check-invert mtagmap)
  (mtagmap-check-invert mtagmap))


(defun mtagmap-closed-p (mtagmap)
  (= -1 (mtagmap-fd mtagmap)))


(defun mtagmap-close (mtagmap)
  (check-type mtagmap mtagmap)
  (let ((fd (mtagmap-fd mtagmap))
	(ptr (mtagmap-ptr mtagmap))
	(len (mtagmap-len mtagmap)))
    (mtagmap-detach mtagmap)
    (unwind-protect (unless (cffi:null-pointer-p ptr)
                      (osicat-posix:munmap ptr len))
      (unless (minusp fd)
	(osicat-posix:close fd))))
  mtagmap)


(defun mtagmap-detach (mtagmap)
  (setf (mtagmap-fd mtagmap) -1
	(mtagmap-len mtagmap) 0
	(mtagmap-ptr mtagmap) (cffi:null-pointer)))


(defun mtagmap-shrink (mtagmap)
  (assert (not (mtagmap-closed-p mtagmap)))
  (mtagmap-check mtagmap)
  (let* ((next (mtagmap-next mtagmap))
	 (bytes (round-up-to-pagesize next)) (file-len (mtagmap-file-length mtagmap)))
    (assert (>= file-len bytes))
    (unless (= next bytes)
      (osicat-posix:memset (cffi:inc-pointer (mtagmap-ptr mtagmap) next) 0 (- bytes next)))
    (unless (= bytes file-len)
      (assert (>= bytes next))
      (mtagmap-resize mtagmap bytes))))


(defun mtagmap-schema (mtagmap)
  (let ((class (mtagmap-class mtagmap)))
    (mm-metaclass-schema class)))


(defmethod print-object ((m mtagmap) stream)
  (print-unreadable-object (m stream :type t)
    (unless (mtagmap-closed-p m)
      (format stream "~A (~D): ~D objects, ~D bytes, ~D bytes mapped (~A)"
	      (class-name (mtagmap-class m))
	      (force-tag m)
	      (mtagmap-count m)
	      (mtagmap-next m)
	      (mtagmap-len m)
	      (mtagmap-default-filename m)))))

