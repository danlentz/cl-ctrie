;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)

(defconstant +mptr-bits+   64)
(defconstant +mtag-bits+   8)
(defconstant +mtags+       (ash 1 +mtag-bits+))
(defconstant +mindex-bits+ (- +mptr-bits+ +mtag-bits+))
(defconstant +word-length+ 8)

(deftype word ()
  `(unsigned-byte 64))
