;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(defpackage :index
  (:use :closer-common-lisp :closer-mop :contextl
    :alexandria :iterate :macro :cl-ctrie)
  (:shadowing-import-from :macro :once-only)
  (:export
    :index-add :index-get :index-remove :index-keys :index-values
    :index-reinitialize :index-clear :index-create :index-mapvalues
    ;;
    :initialize-indexed-instance
    :*initialized-indexed-instance*
    :*indexed-class-override*
    :index-existing-error
    ;;
    :defindex
    :all-index-types
    :slot-index
    :unique-index
    :string-unique-index
    :hash-index
    :hash-list-index
    :class-index
    ;;
    :clear-class-indices
    :clear-slot-indices
    :class-slot-indices
    :class-slot-index
    ;;
    :indexed-class
    :indexed-class-index-named
    :index-direct-slot-definition
    :index-effective-slot-definition
    :destroy-object
    :object-destroyed-p
    ;;
    :category-index
    :tree-find-children
    :tree-find-siblings
    :parent-category
    :parent-categories
    :tree-categories
    ;;
    :dynamic-class
    :root))


;; (defpackage :index  
;;   #+:package-local-nicknames
;;   (:local-nicknames)
;;     ;; (:fast :fast-io))
;;     ;; (:os :uiop/os)
;;     ;; (:fs :uiop/filesystem)
;;     ;; (:pkg :uiop/package)
;;     ;; (:utl :uiop/utility))
;;   (:shadow :once-only :get :remove :values :class)
;;   (:use :closer-common-lisp :closer-mop :contextl :alexandria :iterate 
;;     :macro :cl-ctrie)
;;   (:export
;;     :add :get :remove :keys :values :initialize :reinitialize :clear :create
;;     :map-keys :map-values :dynamic-class :root :dynamic-class)) 
