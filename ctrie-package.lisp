;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


#-sbcl (cl:error "~A not supportred" (cl:lisp-implementation-type))

(defpackage :cl-ctrie
  (:nicknames :ctrie)
  (:use :common-lisp :alexandria :lisp-unit)
  (:shadowing-import-from :alexandria :set-equal)
  (:import-from :sb-ext :get-cas-expansion :define-cas-expander :cas
    :compare-and-swap :atomic-incf :atomic-decf :defcas :defglobal)
  (:export
    :run-ctrie-tests
    :make-ctrie
    :ctrie
    :ctrie-test
    :ctrie-readonly-p
    :ctrie-do
    :ctrie-put
    :ctrie-get
    :ctrie-drop
    :ctrie-map
    :ctrie-map-keys
    :ctrie-map-values
    :ctrie-map-into
    :ctrie-keys
    :ctrie-values
    :ctrie-size
    :ctrie-error
    :ctrie-to-alist
    :ctrie-to-hashtable
    :ctrie-from-hashtable
    :ctrie-from-alist
    :ctrie-empty-p
    :ctrie-ensure-get
    :ctrie-error
    :ctrie-structural-error
    :ctrie-operational-error
    :ctrie-operation-retries-exceeded
    :ctrie-not-implemented
    :ctrie-not-supported)
  (:documentation "This is a common-lisp implementation of the CTrie
     unordered map data-structure described in the paper 'Concurrent
     Tries with Efficient Non-Blocking Snapshots, (c) ACM 2-25-2012'
     by Prokopec, Bronson, Bagwell, and Odersky."))
