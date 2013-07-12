;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :index)

(defvar *root* (make-instance 'cl-ctrie::transient-ctrie))

(defun root ()
  *root*)

(defclass dynamic-class (standard-class)
  ())

(defun table-of (thing)
  (declare (ignore thing))
  *root*)

(defmethod validate-superclass ((class dynamic-class) (super standard-class))
  t)

(defun dynamic-slot-p (slot)
  (eq (slot-definition-allocation slot) :dynamic))

(defun allocate-table-entry (instance)
  (ctrie-put (table-of instance) instance (make-instance 'cl-ctrie::transient-ctrie)))

(defun read-dynamic-slot-value (instance slot-name)
  (let* ((data (ctrie-get (table-of instance) instance))
          (entry (ctrie-get data slot-name)))
    (or entry (error "slot ~S unbound in ~S" slot-name instance))))

(defun write-dynamic-slot-value (new-value instance slot-name)
  (let* ((data (ctrie-get (table-of instance) instance)))
    (ctrie-put data slot-name new-value)))

(defun dynamic-slot-names (instance)
  (ctrie-keys (ctrie-get (table-of instance) instance)))

(defun dynamic-slot-boundp (instance slot-name)
  (let* ((data (ctrie-get (table-of instance) instance)))
    (nth-value 1 (ctrie-get data slot-name))))
  
(defun dynamic-slot-makunbound (instance slot-name)
  (prog1 instance
    (let* ((data (ctrie-get (table-of instance) instance)))
      (ctrie-drop data slot-name))))

(defmethod allocate-instance ((class dynamic-class) &key)
  (let ((instance (call-next-method)))
    (allocate-table-entry instance)
    instance))

(defmethod slot-value-using-class ((class dynamic-class) instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (read-dynamic-slot-value instance (slot-definition-name slotd))
        (call-next-method))))

(defmethod (setf slot-value-using-class) (new-value (class dynamic-class) instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (write-dynamic-slot-value new-value instance (slot-definition-name slotd))
        (call-next-method))))

(defmethod slot-boundp-using-class ((class dynamic-class) instance slotd)
  (let ((slot (find slotd (class-slots class))))

    (if     (:printv  (and slot (dynamic-slot-p slot)))
        (dynamic-slot-boundp instance (slot-definition-name slotd))
        (call-next-method))))

(defmethod slot-makunbound-using-class ((class dynamic-class) instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (dynamic-slot-makunbound instance (slot-definition-name slotd))
        (call-next-method))))

(defclass test-class-1 ()
  ((slot1 :initarg :slot1 :allocation :dynamic)
    (slot2 :initarg :slot2 :initform nil))
  (:metaclass dynamic-class))

(defclass test-class-2 (test-class-1)
  ((slot2 :initarg :slot2 :initform t :allocation :dynamic)
    (slot3 :initarg :slot3))
  (:metaclass dynamic-class))

(defun test-dynamic-class ()
  (let ((one (make-instance 'test-class-1))
         (two (make-instance 'test-class-2 :slot3 1)))
    (assert (not (slot-boundp one 'slot1)))
    (assert (null (slot-value one 'slot2)))
    (assert (eq t (slot-value two 'slot2)))
    (assert (= 1 (slot-value two 'slot3)))))



#|
(defclass dynamic-slot-subclass (dynamic-class) ())

(defmethod slot-value-using-class ((class dynamic-slot-subclass)
                                   instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (read-dynamic-slot-value instance (slot-definition-name slotd))
        (call-next-method))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class dynamic-slot-subclass)
                                          instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (write-dynamic-slot-value new-value instance (slot-definition-name slotd))
        (call-next-method))))

(defmethod slot-boundp-using-class ((class dynamic-slot-subclass)
                                    instance slotd)
  (let ((slot (find slotd (class-slots class))))
    (if (and slot (dynamic-slot-p slot))
        (dynamic-slot-boundp instance (slot-definition-name slotd))
        (call-next-method))))

(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform t :allocation :dynamic)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))

(defvar *three* (make-instance 'test-class-3 :slot3 3))
(assert (not (slot-boundp *three* 'slot1)))
(assert (eq (slot-value *three* 'slot2) t))
(assert (= (slot-value *three* 'slot3) 3))

(defmethod slot-missing ((class dynamic-class) instance slot-name operation &optional v)
  (declare (ignore v))
  (list :slot-missing slot-name))

;;; Test redefinition adding a dynamic slot
(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform t :allocation :dynamic)
   (slot3 :initarg :slot3)
   (slot4 :initarg :slot4 :initform 42 :allocation :dynamic))
  (:metaclass dynamic-slot-subclass))
(assert (= 42 (slot-value *three* 'slot4)))

;;; Test redefinition removing a dynamic slot
(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform t :allocation :dynamic)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))
(assert (equal (list :slot-missing 'slot4) (slot-value *three* 'slot4)))

;;; Test redefinition making a dynamic slot local
;;;
;;; NOTE: seriously underspecified. We muddle somehow.
(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform 'ok :allocation :instance)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))
(let* ((slots (class-slots (find-class 'test-class-3)))
       (slot (find 'slot2 slots :key #'slot-definition-name)))
  (assert (eq :instance (slot-definition-allocation slot)))
  (assert (eq 'ok (slot-value *three* 'slot2))))

;;; Test redefinition making a local slot dynamic again
;;;
;;; NOTE: seriously underspecified. We muddle somehow.
;;; This picks up the old value from the table, not the
;;; new initform.
(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform 'ok? :allocation :dynamic)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))
(let* ((slots (class-slots (find-class 'test-class-3)))
       (slot (find 'slot2 slots :key #'slot-definition-name)))
  (assert (eq :dynamic (slot-definition-allocation slot)))
  (assert (eq t (slot-value *three* 'slot2))))

;;; Test redefinition making a dynamic slot local, with
;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS unbinding the dynamic slot.
;;; Then we make it dynamic again.
;;;
;;; NOTE: seriously underspecified. We muddle somehow.
(defmethod update-instance-for-redefined-class :after ((obj test-class-3) add drop plist
                                                       &rest inits)
  (declare (ignore inits))
  (let* ((class (class-of obj))
         (slots (class-slots class)))
    (dolist (name (dynamic-slot-names obj))
      (let ((slotd (find name slots :key #'slot-definition-name)))
        (unless (and slotd (eq :dynamic (slot-definition-allocation slotd)))
          (dynamic-slot-makunbound obj name))))))

(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform 'ok :allocation :instance)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))
(let* ((slots (class-slots (find-class 'test-class-3)))
       (slot (find 'slot2 slots :key #'slot-definition-name)))
  (assert (eq :instance (slot-definition-allocation slot)))
  (assert (eq 'ok (slot-value *three* 'slot2))))
(defclass test-class-3 (test-class-1)
  ((slot2 :initarg :slot2 :initform 'ok! :allocation :dynamic)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-subclass))
(let* ((slots (class-slots (find-class 'test-class-3)))
       (slot (find 'slot2 slots :key #'slot-definition-name)))
  (assert (eq :dynamic (slot-definition-allocation slot)))
  (assert (eq 'ok! (slot-value *three* 'slot2))))
|#
