;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

#-sbcl (cl:error "~A not supportred" (cl:lisp-implementation-type)
         (:export
           #:defun/inline
           #:let1))

(defpackage :cl-ctrie  
  (:documentation "This is a common-lisp implementation of the CTrie
     unordered map data-structure described in the paper 'Concurrent
     Tries with Efficient Non-Blocking Snapshots, (c) ACM 2-25-2012'
     by Prokopec, Bronson, Bagwell, and Odersky.")
  #+:package-local-nicknames
  (:local-nicknames
    (:mem  :com.informatimago.common-lisp.heap.memory)
    (:heap :com.informatimago.common-lisp.heap.heap))
  ;; (:fast :fast-io))
    ;; (:os :uiop/os)
    ;; (:fs :uiop/filesystem)
    ;; (:pkg :uiop/package)
    ;; (:utl :uiop/utility))
  (:shadow :once-only :map :set)
  (:use :closer-common-lisp :closer-mop :contextl :alexandria :iterate 
    :macro)
  ;; (:shadowing-import-from :lisp-unit :set-equal)
  (:import-from :sb-ext :get-cas-expansion :define-cas-expander :cas
    :compare-and-swap :atomic-incf :atomic-decf :defcas :defglobal)
  (:export
    :make-ctrie
    :ctrie
    :ctrie-test
    :ctrie-hash
    :ctrie-do
    :ctrie-put
    :ctrie-put-if
    :ctrie-put-if-not
    :ctrie-put-ensure
    :ctrie-put-replace
    :ctrie-put-replace-if
    :ctrie-put-update
    :ctrie-put-update-if
    :ctrie-get
    :ctrie-drop
    :ctrie-drop-if
    :ctrie-drop-if-not    
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
;;    :def-ctrie
    :define-ctrie
    :ctrie-gc
    :ctrie-index
    :all-ctries
    :ctrie-names
    :find-ctrie
    :ctrie-name
    :ctrie-next-id
    :ctrie-persistent-store
    :ctrie-enable-pooling
    :ctrie-disable-pooling
    :ctrie-pool-status
    :ctrie-ps
    :ctrie-pooling-enabled-p
    :ctrie-error
    :ctrie-structural-error
    :ctrie-operational-error
    :ctrie-operation-retries-exceeded
    :ctrie-not-implemented
    :ctrie-not-supported
    :ctrie-invalid-dynamic-context
    :ctrie-generational-mismatch
    :ctrie-modification-failed
    :ctrie-registry
    :*vm*
    :type-of))

  
(unless (find-package :mem)
  (rename-package
    "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY"
    "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY"
    '(:mem)))

(unless (find-package :heap)
  (rename-package
    "COM.INFORMATIMAGO.COMMON-LISP.HEAP.HEAP"
    "COM.INFORMATIMAGO.COMMON-LISP.HEAP.HEAP"
    '(:HEAP)))

(defpackage :ctrie
  (:use))



  
