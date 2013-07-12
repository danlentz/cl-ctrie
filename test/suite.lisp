;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl/user)

(defpackage :cl-ctrie-test
  (:use :closer-common-lisp :macro :mm :hu.dwim.stefil :iterate :alexandria)
  (:clones :cl-ctrie))
   

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
  (-run-child-tests-))

(defsuite (cl-ctrie/ctrie :in cl-ctrie) ()
  (-run-child-tests-))

(defsuite (cl-ctrie/tree :in cl-ctrie) ()
  (-run-child-tests-))

(defsuite (cl-ctrie/stm :in cl-ctrie) ()
  (-run-child-tests-))

(defsuite (cl-ctrie/collections :in cl-ctrie) ()
  (-run-child-tests-))

(defun run-all-tests ()
  (funcall-test-with-feedback-message 'cl-ctrie))




;; (defsuite* (cl-ctrie/ctrie/smoke     :in cl-ctrie/ctrie))






