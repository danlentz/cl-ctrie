;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb-test)

(in-suite manardb-test)

(defun box-unbox (object)
  (mptr-to-lisp-object (lisp-object-to-mptr object)))


(deftest box-numbers-test ()
  (loop for num in '(0 1 -1 255 -127 -128 1000 1000000000 -10000000)
	do (is (= (box-unbox num) num))))


(deftest unbox-nil-test ()
  (is (eq nil (mptr-to-lisp-object 0)))
  (is (= 0 (lisp-object-to-mptr nil))))


(deftest box-cons-test ()
  (loop repeat 10
    for cons = nil then (cons cons nil)
    do (is (equal cons (box-unbox cons))))
  (loop for list in '((1 2 box fail (x y))
                       (1 . 2) (((nil . 2)))
                       (((1 2 3 (3)) 1) 1 2 2 . ( 1  2 . 3)))
    do (is (equal list (box-unbox list)))))


(deftest box-unspecialized-array-test ()
  (loop for array in (list 
		      (make-array 0)
		      (make-array 10 :element-type t :initial-element nil))
	do
	(is (equalp array (box-unbox array)))))


(deftest box-numeric-array-test ()
  (loop for (limit type) in `((256 (unsigned-byte 8))
                               (,(ash 1 9) (unsigned-byte 64))
                               (,(ash 1 31) (unsigned-byte 64))
                               (,(ash 1 30) (signed-byte 64))
                               (,most-positive-double-float double-float)
                               (,most-positive-single-float single-float))
    do (loop for len in '(2 1 10 100 1000 10000) do
         (let ((array (make-array len :element-type type)))
           (loop for i below len do
             (setf (aref array i) (random limit)))
           (is (equalp array (box-unbox array)))))))


(deftest box-string-test ()
  (loop for string in '("" "a" "one two three" #.(string (code-char 1000))) do
    (is (string= string (box-unbox string))))) 
