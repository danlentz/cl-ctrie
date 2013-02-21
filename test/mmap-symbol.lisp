;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :manardb-test)

(in-suite manardb-test)

(deftest symbol-tag-is-zero-test ()
  (is (= (manardb::mm-metaclass-tag (find-class 'manardb::mm-symbol)) 0)))


(deftest store-keyword-test ()
  (loop for keyword in '(:keyword :key :errorp)
    for mptr = (lisp-object-to-mptr keyword)
    do (is (eq keyword (mptr-to-lisp-object mptr)))
       (is (= mptr (lisp-object-to-mptr keyword)))))


(deftest store-all-symbols-test (&optional (packages (list (find-package :cl)
                                                       (find-package :manardb))))
  (macrolet ((do-all-syms ((var) &body body)
	       (alexandria:with-gensyms (package)
		 `(loop for ,package in packages do
                    (do-all-symbols (,var ,package)
                      ,@body)))))
    (without-test-progress-printing 
      (let ((table (make-hash-table)))
	(flet ((add (sym mptr)
		 (let ((orig (gethash sym table)))
		   (when orig
		     (is (= orig mptr)))
		   (setf (gethash sym table) mptr))))
          (do-all-syms (sym)
            (add sym (lisp-object-to-mptr sym)))
          (iter (for (sym mptr) :in-hashtable table)
            (is (= mptr (lisp-object-to-mptr sym))))
          (do-all-syms (sym)
            (is (eq sym (mptr-to-lisp-object (lisp-object-to-mptr sym)))))
          (iter (for (sym mptr) :in-hashtable table)
            (is (eq sym (mptr-to-lisp-object mptr))))
          (do-all-syms (sym)
            (add sym (lisp-object-to-mptr sym))))))))

