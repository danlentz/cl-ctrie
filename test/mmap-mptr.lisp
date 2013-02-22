;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite* (cl-ctrie/mmap/mptr :in cl-ctrie/mmap))

(deftest check-make-mptr/all ()
  (loop for mtag below mm::+mtags+
    do (loop 
         repeat 1000
         for mindex = (random (ash 1 mm::+mindex-bits+))
         for mptr = (mm::make-mptr mtag mindex)
         do 
         (is (= (mm::mptr-index mptr) mindex))
         (is (= (mm::mptr-tag mptr)   mtag)))))

