;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/tree/node :in cl-ctrie/tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest (check-node-instance-access :auto-call nil) (m)
  (is (equalp (pointer:deref m) (vector 'tree:node :k :v :l :r :x)))
  (is (eq :k (tree:node/k m)))
  (is (eq :v (tree:node/v m)))
  (is (eq :l (tree:node/l m)))
  (is (eq :r (tree:node/r m)))
  (is (eq :x (tree:node/x m)))
  (is (equal '(:k :v) (tree:node/kv m)))
  (is (equal '(:l :r) (tree:node/lr m)))
  (is (equal '(:k :v :l :r) (tree:node/kvlr m)))
  (is (equal '(:k :v :l :r :x) (tree:node/kvlrx m)))
  (is (equal '(:k :v :l :r :x) (tree:node/constituents m)))
  (is (equal '(:k :v :l :r :x) (multiple-value-list (tree:node/values m)))))


(deftest check-instance-access/transient-node ()
  (check-node-instance-access (vector 'tree:node :k :v :l :r :x))
  (check-node-instance-access (make-array 6 :initial-contents
                                (list 'tree:node :k :v :l :r :x)))
  (check-node-instance-access (with-inactive-layers (persistent)
                                (tree::make-node :k :v :l :r :x))))

(deftest check-instance-access/persistent-node ()
  (check-node-instance-access (mm:make-marray 6 :marray-class 'tree::persistent-node
        :initial-contents (list 'tree:node :k :v :l :r :x)))
  (check-node-instance-access (with-active-layers (persistent)
                                (with-inactive-layers (persistent/cache)
                                  (tree::make-node :k :v :l :r :x)))))

(deftest check-instance-access/persistent-cache-node ()
  (check-node-instance-access (mm:make-marray 6 :marray-class 'tree::persistent/cache-node
        :initial-contents (list 'tree:node :k :v :l :r :x)))
  (check-node-instance-access (with-active-layers (persistent/cache)
                                (tree::make-node :k :v :l :r :x))))
  


