;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :cl-mmap
  (:nicknames :mm)
  (:documentation   "ManarDB is a performant Memory-Mapped storage allocation system based on
                     the common-lisp object system meta-object-protocol.

                     This system defines an enhanced fork of the original ManarDB distribution,
                     (version designation '0.1.20090911) that provides support for non-linux
                     platforms, compatibility with current releases of the required libraries,
                     updates supporting current lisp platform distributions, and a number of
                     miscellaneous fixes and feature enhancements.  It does not necessarily
                     seek to maintain backward compatibility with the API provided by the
                     original distribution in all cases.

                     Based on [ManarDB version 0.1.20090911]
                     (http://cl-www.msi.co.jp/projects/manardb/index.html)
                     by [Mathematical Systems Incorporated](http://www.msi.co.jp).

                     - Thanks to John Fremlin for a nice platform to hack and extend.
                     - Thanks to Pascal Costanza for MOP conformance and other advice.")
  (:use :closer-common-lisp :closer-mop :iterate)
  (:export
    :*mmap*
    :*mtagmaps-may-mmap*
    :*allocate-base-pathname*

    :lisp-object-to-mptr-impl
    :lisp-object-to-mptr
    :mptr-to-lisp-object
    :mptr
    :meq
    
    :mm-object
    :defmmclass
    :mm-metaclass
    
    :gc
    :rewrite-gc

    :print-all-mmaps
    :close-all-mmaps
    :open-all-mmaps
    :wipe-all-mmaps

    :doclass
    :dosubclasses
    :mm-subclasses
    :count-all-instances
    :retrieve-all-instances

    :marray
    :make-marray
    :marray-ref
    :marray-length
    :marray-to-list
    :list-to-marray
    :index-of-marray
    :in-marray

    :make-mm-fixed-string
    :mm-fixed-string-value

    :ensure-manardb
    :clean-mmap-dir
    :clear-caches
    :clear-caches-hard    
    :with-object-cache
    :with-cached-slots
    :direct-slot-numeric-maref
    
    :mcons
    :mcar
    :mcdr
    :mcadr
    :mcddr
    :mconsp
    :empty
    :emptyp
    :mlist
    :as-list
    :mpush
    :mpop))



