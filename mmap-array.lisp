;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)


(defun marray-ref (marray i)
  "Like aref, but for memory mapped arrays. note: doesn't work on specialised arrays"
  (declare (type mindex i))
  (mptr-to-lisp-object (dw (mptr-pointer (marray-base marray)) i)))


(defun (setf marray-ref) (new marray i) 
  (declare (type mindex i))
  (let ((new (lisp-object-to-mptr new)))
    (setf (dw (mptr-pointer (marray-base marray)) i) new))
  new)


(defclause-sequence in-marray index-of-marray
  :access-fn     'marray-ref
  :size-fn       'marray-length
  :sequence-type 'marray
  :element-type  t
  :element-doc-string "Elements of an marray"
  :index-doc-string   "Indices of marray")


(defun marray-to-list (marray)
  "Converts a memory mapped array to a Lisp list; nil is converted to nil"
  (when marray (iter (for c in-marray marray) (collect c))))


(defun list-to-marray (list)
  "Converts a Lisp list to a memory-mapped array object; nil is converted to nil"
  (when list (make-marray (length list) :initial-contents list)))
