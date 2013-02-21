;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :cl-ctrie-test)

(in-suite ctrie/mm/class)


(deftest test-simple-defclass ()
  (eval `(defclass test-empty-class ()
	   ()
	   (:metaclass mm-metaclass)))
  (eval `(defclass test-byte-class ()
	   ((slot :type (unsigned-byte 8)))
	   (:metaclass mm-metaclass)))
  (eval `(defclass test-boxed-class ()
	   ((slot :initform 1))
	   (:metaclass mm-metaclass))))


(deftest test-create-two-slot-class ()
  (eval 
    `(mm:defmmclass two-slot ()
       ((basic-slot
          :initarg :basic-slot
          :initform (error "Please provide a value for the basic slot"))
         (marray
           :initarg :marray
           :initform (mm:make-marray 1000 :initial-element nil))))))


(deftest test-nil-slots-are-not-created ()
  (test-create-two-slot-class)
  (let ((m (make-instance 'two-slot :basic-slot nil)))
    (is (not (slot-value m 'basic-slot)))
    m))


(deftest test-create-instance-of-two-slot (&optional (vals (list nil 0 :keyword "string")))
  (test-create-two-slot-class)
  (loop for val in vals do
    (let ((m (make-instance 'two-slot :basic-slot val :marray val)))
      (is (equalp val (slot-value m 'marray)))
      (is (equalp val (slot-value m 'basic-slot))))))
