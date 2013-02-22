;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite* (cl-ctrie/mmap/gc :in cl-ctrie/mmap))


(deftest check-gc/nil (&key (gc 'mm:gc))
  (with-transaction (:message "GC")
    (loop repeat 10 do
	  (check-make-instance/simple-tree)
	  (is (plusp (mm:count-all-instances 'tree)))
	  (funcall gc nil)
	  (is (zerop (mm:count-all-instances 'tree))))))


(deftest check-gc/one (&key (genval "GC") (gc 'mm:gc))
  (with-transaction (:message "GC one")
    (funcall gc nil)
    (let ((obj (check-make-instance/simple-tree genval)))
      (loop repeat 10 do
	    (gc (list (mm:lisp-object-to-mptr obj)))
	    (is (= 1 (mm:count-all-instances 'tree)))
        (check-consistency/simple-tree obj genval)))))


(deftest (check-gc/marray-half :auto-call nil) (&key first count list gc (gc-repeat 5) print)
  (let ((half (if first 
                (subseq list 0 (/ (length list) 2))
                (loop for x on list by #'cddr collect (second x)))))
    (funcall gc half)
    (is (= count (* 2 (mm:count-all-instances 'tree))))
    (flet ((remaining ()
	     (let ((remaining (remove-if 'tree-parent (mm:retrieve-all-instances 'tree))))
	       (is (= (length half) (length remaining)))
	       (when print (format t "~&Remaining = ~A~%" remaining))
	       remaining)))
      (without-test-progress-printing ;;; too much progress
	(mapcar 'check-consistency/complex-tree (remaining)))
      (loop repeat gc-repeat 
        do (funcall gc (remaining))
           (is (= count (* 2 (mm:count-all-instances 'tree))))
        (without-test-progress-printing ;;; too much progress
          (mapcar 'check-consistency/complex-tree (remaining)))))))


(deftest (check-gc/marray :auto-call nil) (&key (first t) (len 10) (depth 6)
                                            (gc 'mm:gc) (gc-repeat 5) print)
  (assert (evenp len))
  (with-transaction (:message "GC marray")
    (funcall gc nil)
    (let* ((list (loop repeat len collect (check-make-instance/complex-tree depth)))
            (count (mm:count-all-instances 'tree)))     
      (labels ((remaining ()
		 (let ((remaining (remove-if 'tree-parent (mm:retrieve-all-instances 'tree))))
		   (is (= (length remaining) len))
		   (when print (format t "~&Remaining = ~A~%" remaining))
		   remaining))
                (consistent ()
                  (without-test-progress-printing ;;; too much progress
                    (mapcar 'check-consistency/complex-tree (remaining)))
                  (is (= count (mm:count-all-instances 'tree)))))
	(consistent)
	(loop repeat gc-repeat do (gc (remaining)) (consistent)))    
      (check-gc/marray-half :first first :count count :list list
        :gc gc :gc-repeat gc-repeat :print print))))


(deftest check-gc/rewrite-gc ()
  (check-gc/marray :gc (lambda (seq) (mm:rewrite-gc seq))))


(deftest check-gc/copy-gc ()
  (check-gc/marray :first nil)
  (check-gc/marray :first t ))
