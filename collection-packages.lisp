;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :set (:use)
  (:export :typed? :type :add :min :max :remove-min :remove-max :split
    :empty :empty? :member? :singleton :remove :union :diff :intersect
    :compare :equal? :subset? :each :do :foldl :foldr :for-all :some?
    :filter :partition :size :enum :dup :of)) 

(defpackage :map
  (:use)
  (:export))

(defpackage :seq
  (:use)
  (:export))

(defpackage :bag
  (:use)
  (:export))


