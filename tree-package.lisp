;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :tree
  (:documentation "")
  (:use :closer-common-lisp :contextl :macro)
  (:import-from :cl-ctrie :transient :persistent)
  (:export
    :balanced
    :height-balanced
    :weight-balanced
    :merge-direction
    :leaf
    :empty
    :empty?
    :node
    :kv
    :lr
    :kvlr
    :kvlrx
    :kvlrs
    :kvlrh
    :node/k
    :node/v
    :node/l
    :node/r
    :node/x
    :node/s
    :node/h
    :node/kv
    :node/lr
    :node/kvlr
    :node/kvlrx
    :node/kvlrs
    :node/kvlrh
    :node/constituents
    :node/values
    :node/call
    :node/size
    :node/weight
    :node/height
    :node/create
    :node/join
    :node/singleton
    :node/least
    :node/greatest
    :node/remove-least
    :node/remove-greatest
    :node/concat2
    :node/inorder-fold
    :node/iter
    :node/at-index
    :node/find
    :node/add
    :node/remove
    :node/concat3
    :node/concat
    :node/split-lesser
    :node/split-greater
    :node/split
    :node/union
    :node/union-merge
    :node/intersection
    :node/difference
    :node/subset?
    :node/from
    :node/member?
    :node/cons-enum
    :node/contains?
    :node/rank
    :node/for-all
    :+delta+
    :+gamma+
    :node/empty?
    :layer
    :allocation))
