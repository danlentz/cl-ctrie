;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :cl-ctrie-test
  (:shadow :once-only)
  (:use :closer-common-lisp :closer-mop :alexandria :cl-ctrie :mm :iterate :hu.dwim.stefil)
;;  (:shadowing-import-from :lisp-unit :set-equal)
  (:import-from :sb-ext :get-cas-expansion :define-cas-expander :cas
    :compare-and-swap :atomic-incf :atomic-decf :defcas :defglobal)
  (:shadowing-import-from :mm :with-transaction :emptyp)
  (:import-from :cl-ctrie
    :let1 :flag :flag-arc-position :catch-case
    :create-unique-id-byte-vector
    :hex-string-to-byte-vector
    :byte-vector-to-hex-string
    :random-string
    :gensym-list
    :printv
    :printv-enable
    :printv-disable
    :defmacro/once
    :defun/inline
    :deflex
    :ppmx
    :multiple-setf
    :nilf
    :conc1f
    :aconsf
    :?
    :?^
    :^
    :internal-symbols
    :external-symbols
    :home-symbols
    :home-functions
    :atomic-update     
    :enlist
    :snode
    :lnode-search
    :lnode-length
    :lnode-inserted
    :lnode-removed
    :fbind
    :awhen
    :atypecase
    :alet
    :it
    :*debug*
    :*ctrie*
    :with-ctrie
    :*hash-code*
    :alet-fsm
    :state
    :ppmx)
  (:export
 ;;   :fixture
 ;;   :define-fixture
    ;; :$
    :run-all-tests
    ;;:run-ctrie-tests
    ;;:run-ctrie-benchmarks
    ;;:run-ctrie-deterministic-profile
    ;;:run-ctrie-statistical-profile
    ))


;; (defpackage :manardb-test
;;   (:export #:test-all-manardb)
;;   (:use :cl :mm :hu.dwim.stefil :iterate)
;;   (:import-from :mm :with-transaction))
  

(in-package :cl-ctrie-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defixture temp-directory
  (declare (special -dir-))
  (let ((-dir- (format nil "/tmp/ctrie-temp-~36R/"
               (random most-positive-fixnum (make-random-state t)))))
    (ensure-directories-exist -dir-)
     (unwind-protect (-body-)
       (ignore-errors
         (osicat:delete-directory-and-files -dir- :if-does-not-exist nil)))))
    
(defixture temp-mmap-workspace
  (let ((-dir- (format nil "/tmp/ctrie-mmap-~36R/"
               (random most-positive-fixnum (make-random-state t))))
         (mm::*mmap-may-allocate* t))
    (mm::use-mmap-dir -dir- :if-does-not-exist :create)
    (unwind-protect (-body-)
      (ignore-errors
        (osicat:delete-directory-and-files -dir- :if-does-not-exist nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsuite (cl-ctrie :in root-suite) (&key (verbose t))
  (let* ((banner (format nil "Starting test run on ~A"
                   (sb-int:format-universal-time nil (get-universal-time))))
          (uscores (make-string (length banner) :initial-element #\-)))
    (format verbose "~%~%~A~%~A~%~%" banner uscores)
    (-run-child-tests-)
    (format verbose "~%~%")
    (finish-output)))

(defsuite (cl-ctrie/common :in cl-ctrie) ()
  (with-fixture temp-directory
    (-run-child-tests-)))

(defsuite (cl-ctrie/mmap :in cl-ctrie) ()
  (with-fixture temp-mmap-workspace
    (-run-child-tests-)))

(defsuite (cl-ctrie/ctrie :in cl-ctrie) ()
  (with-fixture temp-mmap-workspace
    (-run-child-tests-)))
  
(defun run-all-tests ()
  (funcall-test-with-feedback-message 'cl-ctrie))



(defsuite* (cl-ctrie/ctrie/smoke     :in cl-ctrie/ctrie))
(defsuite* (cl-ctrie/mmap/class      :in cl-ctrie/mmap))
(defsuite* (cl-ctrie/mmap/box        :in cl-ctrie/mmap))
(defsuite* (cl-ctrie/mmap/symbol     :in cl-ctrie/mmap))
(defsuite* (cl-ctrie/mmap/mptr       :in cl-ctrie/mmap))
(defsuite* (cl-ctrie/mmap/tree       :in cl-ctrie/mmap))
(defsuite* (cl-ctrie/mmap/gc         :in cl-ctrie/mmap))
