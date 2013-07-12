;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :ring
  (:use :cl)
  (:shadow :push :pop :rotate :length)
  (:export :make :push :pop :rotate :length :ref :with-ring-locked))
  

(in-package :ring)

(defun %print-ring (obj stream depth)
  (declare (ignore depth obj))
  (write-string "#<Ring>" stream))

(defstruct (ring (:predicate ringp)
		 (:constructor internal-make-ring)
		 (:print-function %print-ring))
  "The ring data structure: An empty ring is indicated by an negative
   First value.  The Bound is made (1- (- Size)) to make length work.
   Things are pushed at high indices first."
  (first -1 :type fixnum)	   ;The index of the first position used.
  (bound -1 :type fixnum)          ;The index after the last element.
  delete-function                  ;The function  to be called on deletion. 
  (vector #() :type simple-vector) ;The vector.
  (lock (bt:make-lock)))


(defmacro with-ring-locked ((ring) &body body)
  `(bt:with-lock-held  ((ring-lock ,ring))
    ,@body))

(defun make (size &optional (delete-function #'identity))
  "Make a ring-buffer which can hold up to Size objects.  Delete-Function
  is a function which is called with each object that falls off the
  end."
  (unless (and (typep size 'fixnum) (> size 0))
    (error "Ring size, ~S is not a positive fixnum." size))
  (internal-make-ring
    :delete-function delete-function
    :vector (make-array size)
    :bound  (1- (- size))))

(defun push (object ring)
  "Push an object into a ring, deleting an element if necessary."
  (with-ring-locked (ring)
    (let ((first (ring-first ring))
          (vec (ring-vector ring))
          (victim 0))
      (declare (simple-vector vec) (fixnum first victim))
      (cond
        ;; If zero, wrap around to end.
        ((zerop first)
         (setq victim (1- (cl:length vec))))
        ;; If empty then fix up pointers.
        ((minusp first)
         (setf (ring-bound ring) 0)
         (setq victim (1- (cl:length vec))))
        (t
         (setq victim (1- first))))
      (when (= first (ring-bound ring))
        (funcall (ring-delete-function ring) (aref vec victim))
        (setf (ring-bound ring) victim))
      (setf (ring-first ring) victim)
      (setf (aref vec victim) object))))


(defun pop (ring)
  "Pop an object from a ring and return it."
  (with-ring-locked (ring)
    (let* ((first (ring-first ring))
           (vec (ring-vector ring))
           (new (if (= first (1- (cl:length vec))) 0 (1+ first)))
           (bound (ring-bound ring)))
      (declare (fixnum first new bound) (simple-vector vec))
      (cond
        ((minusp bound)
         (error "Cannot pop from an empty ring."))
        ((= new bound)
         (setf (ring-first ring) -1  (ring-bound ring) (1- (- (cl:length vec)))))
        (t
         (setf (ring-first ring) new)))
      (shiftf (aref vec first) nil))))


(defun length (ring)
  "Return as values the current and maximum size of a ring."
  (with-ring-locked (ring)
    (let ((diff (- (ring-bound ring) (ring-first ring)))
          (max (cl:length (ring-vector ring))))
      (declare (fixnum diff max))
      (values (if (plusp diff) diff (+ max diff)) max))))

(defun ref (ring index)
  (declare (fixnum index))
  "Return the index'th element of a ring.  This can be set with Setf."
  (with-ring-locked (ring)
    (let ((first (ring-first ring)))
      (declare (fixnum first))
      (cond
        ((and (zerop index) (not (minusp first)))
         (aref (ring-vector ring) first))
        (t
         (let* ((diff (- (ring-bound ring) first))
                (sum (+ first index))
                (vec (ring-vector ring))
                (max (cl:length vec)))
           (declare (fixnum diff max sum) (simple-vector vec))
           (when (or (>= index (if (plusp diff) diff (+ max diff)))
                     (minusp index))
             (error "Ring index ~D out of bounds." index))
           (aref vec (if (>= sum max) (- sum max) sum))))))))



(defun %set-ring-ref (ring index value)
  (declare (fixnum index))
  (with-ring-locked (ring)
    (let* ((first (ring-first ring))
           (diff (- (ring-bound ring) first))
           (sum (+ first index))
           (vec (ring-vector ring))
           (max (cl:length vec)))
      (declare (fixnum diff first max) (simple-vector vec))
      (when (or (>= index (if (plusp diff) diff (+ max diff))) (minusp index))
        (error "Ring index ~D out of bounds." index))
      (setf (aref vec (if (>= sum max) (- sum max) sum)) value))))

(eval-when (:compile-toplevel :execute)
  (defmacro 1+m (exp base)
    `(if (= ,exp ,base) 0 (1+ ,exp)))
  (defmacro 1-m (exp base)
    `(if (zerop ,exp) ,base (1- ,exp))))

(defun rotate (ring offset)
  "Rotate a ring forward, i.e. second -> first, with positive offset,
  or backwards with negative offset. blt'ing elements as necessary."
  (declare (fixnum offset))
  (with-ring-locked (ring)
    (let* ((first (ring-first ring))
           (bound (ring-bound ring))
           (vec (ring-vector ring))
           (max (cl:length vec)))
      (declare (fixnum first bound max) (simple-vector vec))
      (cond
        ((= first bound)
         (let ((new (rem (+ offset first) max)))
           (declare (fixnum new))
           (if (minusp new) (setq new (+ new max)))
           (setf (ring-first ring) new)
           (setf (ring-bound ring) new)))
        ((not (minusp first))
         (let* ((diff (- bound first))
                (1-max (1- max))
                (length (if (plusp diff) diff (+ max diff)))
                (off (rem offset length)))
           (declare (fixnum diff length off 1-max))
           (cond
             ((minusp offset)
              (do ((dst (1-m first 1-max) (1-m dst 1-max))
                   (src (1-m bound 1-max) (1-m src 1-max))
                   (cnt off (1+ cnt)))
                  ((zerop cnt)
                   (setf (ring-first ring) (1+m dst 1-max))
                   (setf (ring-bound ring) (1+m src 1-max)))
                (declare (fixnum dst src cnt))
                (shiftf (aref vec dst) (aref vec src) nil)))
             (t
              (do ((dst bound (1+m dst 1-max))
                   (src first (1+m src 1-max))
                   (cnt off (1- cnt)))
                  ((zerop cnt)
                   (setf (ring-first ring) src)
                   (setf (ring-bound ring) dst))
                (declare (fixnum dst src cnt))
                (shiftf (aref vec dst) (aref vec src) nil)))))))))
  ring)
