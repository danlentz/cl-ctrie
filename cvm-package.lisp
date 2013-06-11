;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :cvm
  (:use)
  (:import-from :heap :*gc-memory* :with-common-lock :with-generation :gen-with-generation
    :with-loop-detection)
  (:export
    :*gc-memory* :with-common-lock :with-generation :gen-with-generation
    :with-loop-detection :*default-storage-directory* :when-ready :memory-fd
    :*inhibit-gc* :+cookie+ :+header-size+ :+version-major+ :+version-minor+ :+growby+
    :+page-size+ :extend-memory
    :*memory-pathnames* :*memory-ids* :memory :create-memory :find-memory :memory-of
    :memory-pathname :memory-sap :memory-stream :memory-lock :memory-id :memory-gate :memory-gc
    :heap-sap :string :host-string :symbol :make-symbol :host-symbol :host-symbol-name
    :ref :deref :structure :host-structure-class :host-structure :structure-class :structure-p   
    :char :host-char  :target-addr :host-value :type-of :gc :gc-check :debug :undebug
    :serial-box :serial-unbox :codec-for :print-all-package-names
    :all-packages :host-all-packages :package :ensure-package :host-package 
    :heap-address-p :serial-box-p :addr :string-p :cons-p :symbol-p :address-p
    :readable-p :heap-address :heap-address-p :heap-address-bounds-check
    :heap-address-alignment-check
    :address-of :reference
    :defcommon :symbol-value :get :put :*tip* :tip :cons
    :consp :delete-from-root :push-to-root :symbol-plist :car :cdr :rplaca :rplacd
    :symbol-set-plist :pprint-mem :heap-sap :extend-memory
    ;; :define-struct :slot :slot-name :slot-initform :slot-type
    ;; :slot-location :slot-writer :slot-reader :make-slot
    
    ))

