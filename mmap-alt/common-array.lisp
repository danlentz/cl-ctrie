;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :array
  (:use :cl)
  (:shadow := :map)
  (:export
    :=
    :map-update
    :subst-all
    :find-original
    :displace
    :undisplace
    :flatten
    :map
    :map!
    :convert
    :transpose
    :copy))

(in-package :array)


(defun = (a1 a2)
  (and (equal (array-dimensions a1)
              (array-dimensions a2))
       (dotimes (i (apply #'* (array-dimensions a1)) t)
         (unless (equal (row-major-aref a1 i)
                        (row-major-aref a2 i))
           (return nil)))))



(defun map-update (fun src &rest lists)
  (funcall
    (reduce (lambda (cont list-elms)
              (lambda ()
                (apply fun (funcall cont) list-elms) ))
            (apply #'mapcar #'list lists)
            :initial-value (constantly src) )))

(defun subst-all (to-list from-list src)
  (map-update
    (lambda (src from to) (subst to from src))
    src from-list to-list ))

;;(subst-all '(1 2 3) '(a b c) '(a b c d c b a))  ; => '(1 2 3 d 3 2 1)


(defun find-original (array)
  "Find the original parent of a displaced array, return this and the
sum of displaced index offsets."
  (let ((sum-of-offsets 0))
    (tagbody
     check-displacement
       (multiple-value-bind (displaced-to displaced-index-offset)
	   (array-displacement array)
	 (when displaced-to
	   (setf array displaced-to)
	   (incf sum-of-offsets displaced-index-offset)
	   (go check-displacement))))
    (values array sum-of-offsets)))



(defun displace (array dimensions index-offset)
  "Make a displaced array from array with the given dimensions and the
index-offset and the same element-type as array.  Tries to displace
from the original array."
  (multiple-value-bind (original-array sum-of-offsets)
      (find-original array)
    (make-array dimensions 
		:element-type (array-element-type array)
		:displaced-to original-array
      :displaced-index-offset (+ sum-of-offsets index-offset))))


(defun undisplace- (array)
  "Return the fundamental array and the start and end positions into
it of a displaced array."
  (let ((length (length array))
        (start 0))
    (loop
      (multiple-value-bind (to offset) (array-displacement array)
        (if to
            (setq array to
                  start (+ start offset))
          (return (values array start (+ start length))))))))



;; (with-input-from-string (stream "Hello World!  How do you do? ")
;;   (let ((c (make-string 4)))
;;     (loop
;;       :for pos = (read-sequence c stream)
;;       :while (= pos (length c))
;;       :do (print c)
;;       :finally (terpri))))

;; "Hell" 
;; "o Wo" 
;; "rld!" 
;; "  Ho" 
;; "w do" 
;; " you" 
;; " do?" 


;; This can also be done using a displaced adjustable array, without copying the data that is read:

(defun make-vector-cursor (vector &key (size 0) (offset 0))
  (make-array size
              :element-type (array-element-type vector)
              :adjustable t
              :displaced-to vector
              :displaced-index-offset offset))

(defun advance-cursor (cursor &key (size (length cursor)))
  (multiple-value-bind (vector offset) (array-displacement cursor)
    (adjust-array cursor size
                  :element-type (array-element-type vector)
                  :displaced-to vector
      :displaced-index-offset (+ offset (length cursor)))))

;; (let ((c (make-vector-cursor "Hello World!  How do you do? " :size 4)))
;;   (loop
;;     :do (print c)
;;     :while (ignore-errors (advance-cursor c))
;;     :finally (terpri)))

;; "Hell" 
;; "o Wo" 
;; "rld!" 
;; "  Ho" 
;; "w do" 
;; " you" 
;; " do?" 
;; NIL

;; (let ((c (make-vector-cursor "Hello World!  How do you do? " :size 1)))
;;   (loop
;;     :for size :from 1
;;     :do (print c)
;;     :while (ignore-errors (advance-cursor c :size size))
;;     :finally (terpri)))

;; "H" 
;; "e" 
;; "ll" 
;; "o W" 
;; "orld" 
;; "!  Ho" 
;; "w do y" 
;; "ou do? " 
;; nil



(defun flatten (array)
  "Return a flat (ie rank 1) displaced version of the array."
  (displace array (array-total-size array) 0))
  
;; DEPRECATED
;; (defun find-or-displace-to-flat-array (array)
;;   "Find a flat array that array is displaced to, or create one that is
;; displaced to the original array.  Also return the index-offset and
;; length (total size).  Useful for passing to reduce etc."
;;   (bind ((total-size (array-total-size array))
;; 	 ((:values original-array index-offset) (find-original-array array)))
;;     (if (= (array-rank original-array) 1)
;; 	(values original-array index-offset total-size)
;; 	(values (displace-array original-array total-size index-offset)
;; 		0 total-size))))

  
;;(defparameter *a* #2A((1 2) (3 4)))
#+()
(defun map (function array 
		  &optional (element-type (array-element-type array)))
  "Map an array into another one elementwise using function.  The
resulting array has the given element-type."
  (bind ((result (make-array (array-dimensions array) :element-type element-type))
	 ((:values original index-offset) (find-original-array array)))
    (iter
      (for result-index :from 0 :below (array-total-size array))
      (for original-index :from index-offset)
      (setf (row-major-aref result result-index)
	    (funcall function (row-major-aref original original-index))))
    result))

#+()
(defun array-copy (array)
  "Copy the elements of array.  Does not copy the elements themselves
recursively, if you need that, use array-map."
  (array-map #'identity array))

(defun map! (function array)
  "Replace each element 'elt' of an array with (funcall function elt),
and return the modified array."
  (dotimes (i (array-total-size array))
    (setf (row-major-aref array i) (funcall function (row-major-aref array i)))))

#+()
(defun convert (element-type array)
  "Convert array to desired element type.  Always makes a copy, even
if no conversion is required."
  (let ((element-type (upgraded-array-element-type element-type)))
    (if (equal (array-element-type array) element-type)
	(array-copy array)
	(array-map #'(lambda (x) (coerce x element-type)) array element-type))))

#+()
(defun transpose (matrix)
  "Transpose a matrix."
  (check-type matrix (array * (* *)))
  (bind (((rows cols) (array-dimensions matrix))
	 (transpose (make-array (list cols rows) 
				:element-type (array-element-type matrix))))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref transpose j i) (aref matrix i j))))
    transpose))


(defun copy-array (array &key (undisplace nil))
  "Shallow copies the contents of any array into another array with
equivalent properties.  If array is displaced, then this function will
normally create another displaced array with similar properties,
unless UNDISPLACE is non-NIL, in which case the contents of the array
will be copied into a completely new, not displaced, array."
  (declare (type array array))
  (let ((copy (%make-array-with-same-properties array undisplace)))
    (unless (array-displacement copy)
      (dotimes (n (array-total-size copy))
        (setf (row-major-aref copy n) (row-major-aref array n))))
    copy))

(defun %make-array-with-same-properties (array undisplace)
  "Make an array with the same properties (size, adjustability, etc.)
as another array, optionally undisplacing the array."
  (apply #'make-array
	 (list* (array-dimensions array)
		:element-type (array-element-type array)
		:adjustable (adjustable-array-p array)
		:fill-pointer (when (array-has-fill-pointer-p array)
				(fill-pointer array))
		(multiple-value-bind (displacement offset)
		    (array-displacement array)
		  (when (and displacement (not undisplace))
		    (list :displaced-to displacement
                      :displaced-index-offset offset))))))


(defun make-vector-concatenation-accessor (vectors)
  (lambda (index)
    (if (minusp index)
        (error "Invalid index (must not be negative)")
        (loop
           :named indexing
           :for vec :in vectors
           :for max = (length vec)
           :sum max :into total
           :if (< index max) :do (return-from indexing (values vec index))
           :else :do (decf index max)
           :finally (error "Invalid index (must be less than ~A)" total)))))

(defstruct concatenated-vector
  reader writer)

(defun concatenate-vectors (&rest vectors)
  (let ((accessor (make-vector-concatenation-accessor vectors)))
    (make-concatenated-vector 
     :reader (lambda (index) (multiple-value-bind (vec ind) (funcall accessor index)
                               (aref vec ind)))
     :writer (lambda (value index) (multiple-value-bind (vec ind) (funcall accessor index)
                                     (setf (aref vec ind) value))))))

(defmethod ref ((vec concatenated-vector) index)
  (funcall (concatenated-vector-reader vec) index))

(defmethod (setf ref) (new-value (vec concatenated-vector) index)
  (funcall (concatenated-vector-reader vec) new-value index))
