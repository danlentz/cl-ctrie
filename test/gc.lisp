;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb-test)

(in-suite manardb-test)

(deftest test-gc-nil (&key (gc 'gc))
  (with-transaction (:message "GC")
    (loop repeat 10 do
	  (simple-tree-create-test)
	  (is (plusp (count-all-instances 'tree)))
	  (funcall gc nil)
	  (is (zerop (count-all-instances 'tree))))))


(deftest test-gc-one (&key (genval "GC") (gc 'gc))
  (with-transaction (:message "GC one")
    (funcall gc nil)
    (let ((obj (simple-tree-create-test genval)))
      (loop repeat 10 do
	    (gc (list (lisp-object-to-mptr obj)))
	    (is (= 1 (count-all-instances 'tree)))
	    (simple-tree-consistency-test obj genval)))))


(defun test-gc-marray (&key (first t) (len 10) (depth 6) (gc 'gc) (gc-repeat 5) print)
  (assert (evenp len))
  (with-transaction (:message "GC marray")
    (funcall gc nil)
    (let* ((list (loop repeat len collect (test-make-complex-tree depth)))
            (count (count-all-instances 'tree)))     
      (labels ((remaining ()
		 (let ((remaining (remove-if 'tree-parent (retrieve-all-instances 'tree))))
		   (is (= (length remaining) len))
		   (when print (format t "~&Remaining = ~A~%" remaining))
		   remaining))
                (consistent ()
                  (without-test-progress-printing ;;; too much progress
                    (mapcar 'test-consistency-of-complex-tree (remaining)))
                  (is (= count (count-all-instances 'tree)))))
	(consistent)
	(loop repeat gc-repeat do
          (gc (remaining))
          (consistent)))    
      (test-gc-marray-half :first first :count count :list list
        :gc gc :gc-repeat gc-repeat :print print))))


(defun test-gc-marray-half (&key first count list gc (gc-repeat 5) print)
  (let ((half (if first 
                (subseq list 0 (/ (length list) 2))
                (loop for x on list by #'cddr collect (second x)))))
    (funcall gc half)
    (is (= count (* 2 (count-all-instances 'tree))))

    (flet ((remaining ()
	     (let ((remaining (remove-if 'tree-parent (retrieve-all-instances 'tree))))
	       (is (= (length half) (length remaining)))
	       (when print (format t "~&Remaining = ~A~%" remaining))
	       remaining)))
      (without-test-progress-printing ;;; too much progress
	(mapcar 'test-consistency-of-complex-tree (remaining)))
      (loop repeat gc-repeat 
        do
        (funcall gc (remaining))
        (is (= count (* 2 (count-all-instances 'tree))))
        (without-test-progress-printing ;;; too much progress
          (mapcar 'test-consistency-of-complex-tree (remaining)))))))


(deftest test-rewrite-gc ()
  (test-gc-marray :gc (lambda (seq) (rewrite-gc seq))))


(deftest test-gc ()
  (test-gc-marray :first nil)
  (test-gc-marray :first t ))
