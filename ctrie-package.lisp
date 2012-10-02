;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

#-sbcl (cl:error "~A not supportred" (cl:lisp-implementation-type))

(defpackage :cl-ctrie  
  (:documentation "This is a common-lisp implementation of the CTrie
     unordered map data-structure described in the paper 'Concurrent
     Tries with Efficient Non-Blocking Snapshots, (c) ACM 2-25-2012'
     by Prokopec, Bronson, Bagwell, and Odersky.")
  (:shadow :once-only)
  (:use :closer-common-lisp :closer-mop :alexandria :lisp-unit)
  (:shadowing-import-from :lisp-unit :set-equal)
  (:import-from :sb-ext :get-cas-expansion :define-cas-expander :cas
    :compare-and-swap :atomic-incf :atomic-decf :defcas :defglobal)
  (:export
    :make-ctrie
    :ctrie
    :ctrie-test
    :ctrie-hash
    :ctrie-do
    :ctrie-put
    :ctrie-get
    :ctrie-drop
    :ctrie-map
    :ctrie-map-keys
    :ctrie-map-values
    :ctrie-keys
    :ctrie-values
    :ctrie-size
    :ctrie-clear
    :ctrie-pprint
    :ctrie-error
    :ctrie-to-alist
    :ctrie-to-hashtable
    :ctrie-from-hashtable
    :ctrie-from-alist
    :ctrie-empty-p
    :ctrie-readonly-p
    :ctrie-max-depth
    :ctrie-min-depth
    :ctrie-save
    :ctrie-load
    :ctrie-export
    :ctrie-import
    :ctrie-snapshot
    :ctrie-fork
    :ctrie-lambda
    :ctrie-lambda-ctrie
    :ctrie-lambda-spawn
    :ctrie-lambda-class
    :ctrie-lambda-object
    :new-ctrie
    :define-ctrie
    :ctrie-enable-pooling
    :ctrie-disable-pooling
    :ctrie-pooling-enabled-p
    :ctrie-error
    :ctrie-structural-error
    :ctrie-operational-error
    :ctrie-operation-retries-exceeded
    :ctrie-not-implemented
    :ctrie-not-supported
    :ctrie-invalid-dynamic-context
    :ctrie-generational-mismatch
    :ctrie-modification-failed))




  
