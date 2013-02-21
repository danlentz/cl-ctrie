;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/common/ord :in cl-ctrie/common))

(defclass foo ()
  ((a :initarg :a)
    (b :initarg :b)))

(defclass bar () ())

(deftest (check-compare-ordinality :auto-call nil) (lesser greater)
  (is (eql (ord:compare lesser  greater) -1))
  (is (eql (ord:compare greater lesser)   1))
  (is (eql (ord:compare lesser  lesser)   0))
  (is (eql (ord:compare greater greater)  0))
  (is (ord:compare< lesser greater))
  (is (not (ord:compare< greater lesser)))
  (is (ord:compare<= lesser greater))
  (is (not (ord:compare<= greater lesser)))
  (is (not (ord:|COMPARE>| lesser greater)))
  (is (ord:|COMPARE>| greater lesser))
  (is (not (ord:compare>= lesser greater)))
  (is (ord:compare>= greater lesser))
  (is (ord:compare= lesser lesser))
  (is (ord:compare= greater greater))
  (is (ord:compare<= lesser lesser))
  (is (ord:compare>= lesser lesser))
  (is (ord:compare<= greater greater))
  (is (ord:compare>= greater greater))
  (is (not (ord:compare= lesser greater)))
  (is (not (ord:compare= greater lesser))))
  
(deftest check-compare-ordinality/various-types ()
  (progn 
    (check-compare-ordinality #\a  #\z)
    (check-compare-ordinality 1    2)
    (check-compare-ordinality 10   20)
    (check-compare-ordinality 10.0 20.0)
    (check-compare-ordinality 10   20.0)
    (check-compare-ordinality 10.0 20)
    (check-compare-ordinality  0   :x)
    (check-compare-ordinality 'x   :x)
    (check-compare-ordinality 10.0 'x)
    (check-compare-ordinality :x   10.0)
    (check-compare-ordinality "aardvark" "zebra")
    (check-compare-ordinality "AARDVARK" "aardvark")
    (check-compare-ordinality :aardvark :zebra)
    (check-compare-ordinality '#:aardvark '#:zebra)
    (check-compare-ordinality 'aardvark 'zebra)
    (check-compare-ordinality (make-instance 'standard-object) (make-instance 'standard-object))
    (check-compare-ordinality (make-instance 'foo :a 0 :b 0)   (make-instance 'foo :a 1 :b 1))
    (check-compare-ordinality (make-instance 'bar) (make-instance 'foo :a 0 :b 0))
    (check-compare-ordinality (find-package :common-lisp) (find-package :keyword))
    (check-compare-ordinality #p"/tmp/aardvark" #p"/tmp/zebra")))


