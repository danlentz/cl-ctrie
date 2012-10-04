;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)

(defvar *mm-fixed-string-uncropper* 'mm-fixed-string-uncropper)


(defun mm-fixed-string-uncropper (string original-length)
  (declare (ignore original-length))
  (concatenate 'string string "..."))


(defun mm-fixed-string-value (mfs)
  "The string stored in the fixed length string MFS. If the string was
  cropped, then append ... to the stored value. Can be set with setf. If
  the new value is too long then it will be silently cropped."
  (with-pointer-slots (cropped-length length)
      ((mm-object-pointer mfs) mm-fixed-string)
    (let ((base-string (cl-irregsexp.bytestrings:force-string  
			  (subseq
			   (cl-irregsexp.bytestrings:force-byte-vector 
			    (tag-general-unbox-array (mptr-tag (ptr mfs)) (mptr-index (ptr mfs))))
			   0
			   (min cropped-length length)))))
      (if (> cropped-length length)
	  (funcall *mm-fixed-string-uncropper* base-string cropped-length)
	  base-string))))


(with-constant-tag-for-class (element-tag boxed-byte) 
  (defun make-mm-fixed-string (length &key value)
    "Create a fixed length string object of size LENGTH; stores into
    it the string in VALUE if given. fixed length string allows string
    objects to be modified in the datastore without allocating more space."
    (let ((mfs (make-instance 'mm-fixed-string 
                 :length length 
                 :base (make-mptr element-tag
                         (mtagmap-alloc (mtagmap element-tag) 
                           (* length #.(stored-type-size '(unsigned-byte 8))))))))
      (when value (mm-fixed-string-store mfs value))
      mfs)))


(defun mm-fixed-string-store (mfs string)
  (with-pointer-slots (cropped-length length base) ((mm-object-pointer mfs) mm-fixed-string)
    (let ((bv (cl-irregsexp.bytestrings:force-byte-vector string)) (ptr (mptr-pointer base)))
      (setf cropped-length (length bv))
      (loop for x across bv
	    for i below length
	    do (setf (d ptr i (unsigned-byte 8)) x))))
  mfs)


(defun (setf mm-fixed-string-value) (string mfs)
  (mm-fixed-string-store mfs string)
  string)

