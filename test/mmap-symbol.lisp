;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite* (cl-ctrie/mmap/symbol :in cl-ctrie/mmap))

(deftest check-symbol-tag/zero ()
  (is (= (mm::mm-metaclass-tag (find-class 'mm::mm-symbol)) 0)))


(deftest check-store-symbol/keyword ()
  (loop for keyword in '(:keyword :key :errorp)
    for mptr = (mm:lisp-object-to-mptr keyword)
    do (is (eq keyword (mm:mptr-to-lisp-object mptr)))
       (is (= mptr (mm:lisp-object-to-mptr keyword)))))


(deftest check-store-symbol/uninterned ()
  (is (notany #'null (mapcar #'symbol-package mm::*stored-symbols*)))
  (is (packagep (find-package "")))
  (let ((sym (gensym)))
    (is (eq sym (mm:mptr-to-lisp-object
                  (mm:lisp-object-to-mptr sym))))
    (mapc (lambda (sym2) (is (eq sym2 sym)))
      (loop repeat 16 collect (mm:mptr-to-lisp-object
                                (mm:lisp-object-to-mptr sym))))
    (is (eq sym (mm:mptr-to-lisp-object
                  (mm:lisp-object-to-mptr (make-symbol (symbol-name sym))))))))


(deftest check-store-symbol/all (&optional (packages (list (find-package :cl)
                                                       (find-package :uuid)
                                                       (find-package :mm))))
  (macrolet ((do-all-syms ((var) &body body)
	       (alexandria:with-gensyms (package)
		 `(loop for ,package in packages do
                    (do-symbols (,var ,package)
                      ,@body)))))
    (without-test-progress-printing 
      (let ((table (make-hash-table)))
	(flet ((add (sym mptr)
		 (let ((orig (gethash sym table)))
		   (when orig (is (= orig mptr)))
		   (setf (gethash sym table) mptr))))
          (do-all-syms (sym)
            (add sym (mm:lisp-object-to-mptr sym)))
          (iter (for (sym mptr) :in-hashtable table)
            (is (= mptr (mm:lisp-object-to-mptr sym))))
          (do-all-syms (sym)
            (is (eq sym (mm:mptr-to-lisp-object (mm:lisp-object-to-mptr sym)))))
          (iter (for (sym mptr) :in-hashtable table)
            (is (eq sym (mm:mptr-to-lisp-object mptr))))
          (do-all-syms (sym)
            (add sym (mm:lisp-object-to-mptr sym))))))))

