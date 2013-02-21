;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb-test)

(in-suite manardb-test)

(defmmclass tree ()
  ((numval
     :type (unsigned-byte 50)
     :initform 666
     :initarg :numval)
    (general-val
      :initform 'gen-val
      :initarg :general-val
      :accessor tree-general-val)
    (id
      :initform (random (ash 1 40))
      :type (unsigned-byte 40)
      :accessor tree-id)
    (val
      :initarg :val
      :accessor tree-val)
    (left
      :type tree
      :initform nil
      :accessor tree-left)
    (right
      :type tree
      :initform nil
      :accessor tree-right)
    (parent
      :initarg :parent 
      :initform nil 
      :accessor tree-parent)
    (temporary-slot
      :persistent nil)))


(defmethod print-object ((tree tree) stream)
  (print-unreadable-object (tree stream :type t)
    (format stream "id ~A ptr ~A" (tree-id tree) (lisp-object-to-mptr tree))
    (loop for slot in '(left right parent)
      do (format stream " ~A ~A"
           slot (lisp-object-to-mptr (slot-value tree slot))))
    (format stream " children ~A" (marray-to-list (tree-val tree)))))


(deftest create-tree-class-test ()
  (is (find-class 'tree)))


(deftest simple-tree-create-test (&optional (gen-val "This is a string"))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :general-val gen-val)))
    (setf (tree-left tree) tree)
    (setf (tree-right (tree-left tree)) 'right)
    (is (eq 'right (tree-right (tree-left tree))))
    (setf (tree-left (tree-left tree)) nil)
    (is (eq nil (tree-left tree)))
    (setf (tree-val tree) (make-marray 10 :initial-element tree))     
    (simple-tree-consistency-test tree gen-val)
    tree))


(deftest simple-tree-consistency-test (tree gen-val)
  (is (not (slot-boundp tree 'temporary-slot)))
  (is (eq (tree-right tree) 'right))
  (is (equalp (slot-value tree 'numval) 666))
  (is (equalp (tree-general-val tree) gen-val))
  (is (not (eq (tree-general-val tree) gen-val)))
  (is (= 10 (marray-length (tree-val tree))))
  (iter (for a in-marray (tree-val tree))
	(is (meq a tree))))


(deftest symbol-slot-tree-create-test (&optional (symbol :keyword))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :val symbol)))
    (is (eq symbol (funcall 'tree-val tree)))
    (is (eq symbol (slot-value tree 'val)))
    (is (eq 'gen-val (slot-value tree 'general-val)))
    tree))


(deftest test-make-complex-tree (&optional (depth 3) parent)
  (cond ((plusp depth)
          (let ((tree
                  (make-instance 'tree :numval depth :parent parent)))
            (setf (slot-value tree 'temporary-slot) :complex)
            (setf (tree-val tree)
              (let ((m (make-marray depth)))
                (loop for i below depth 
                  for last-tree = nil then new-tree
                  for new-tree = (funcall 'test-make-complex-tree (1- depth) tree)
                  do 
                  (when last-tree
                    (setf (tree-right last-tree) new-tree)
                    (setf (tree-left new-tree) last-tree))
                  (setf (marray-ref m i) 
                    new-tree))
                m))
            (test-consistency-of-complex-tree tree depth)
            (is (eq (slot-value tree 'temporary-slot) :complex))
            tree))
    (t
      'leaf)))


(deftest test-consistency-of-complex-tree (tree &optional (depth (slot-value tree 'numval))
                                            (parent nil parent-given-p))
  (let ((marray (tree-val tree)))
    (is (= depth (slot-value tree 'numval)))
    (when parent-given-p
      (is (meq parent (slot-value tree 'parent))))
    (loop for i below depth
      for last-ref = nil then ref
      for ref = (marray-ref marray i)
      do
      (cond ((= 1 depth)
              (is (eq 'leaf ref)))
        (t
          (is (typep ref 'tree))
          (is (not (slot-boundp ref 'temporary-slot)))
          (when last-ref
            (is (not (meq ref (tree-right ref))))
            (is (meq ref (tree-right last-ref)))
            (is (meq (tree-left ref) last-ref)))
          (funcall 'test-consistency-of-complex-tree ref (1- depth) tree))))))
