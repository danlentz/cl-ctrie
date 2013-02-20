;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :manardb-test)

(stefil:in-suite manardb-test)

(stefil:deftest make-mptr-all ()
  (loop for mtag below +mtags+
	do (loop 
		 repeat 1000
		 for mindex = (random (ash 1 +mindex-bits+))
		 for mptr = (make-mptr mtag mindex)
		 do 
		 (stefil:is (= (mptr-index mptr) mindex))
		 (stefil:is (= (mptr-tag mptr) mtag)))))

