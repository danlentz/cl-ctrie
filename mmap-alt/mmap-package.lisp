;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


;; TODO: lift dependencies to encompassing asdf system once stabilized

(unless (every #'find-package
          #1='(:cffi-objects :osicat :closer-mop :contextl :alexandria :puri
                :hu.dwim.serializer :flexi-streams :rucksack :cl-store))
  (warn "Manually quick-loading required systems: ~{~A ~}"
    (loop for sys in #1# when (not (find-package sys)) collect sys))
  (:ql #1#))


(unless (every #'identity #1=(mapcar #'find-package #2='(:io :pointer)))
  (error "required packages not found: ~{~S ~}"
     (loop with remaining = #2#
       for pkg in (remove-if #'null #1#)
       do (setf remaining (delete (package-name pkg) remaining :test #'string=))
       finally (return remaining))))


(defpackage :mmap
  (:shadow :pointer-address :incf-pointer :decf-pointer :last-elt-p)
  (:use :closer-common-lisp :closer-mop :contextl :cffi-objects :alexandria)
  (:shadowing-import-from :cl-ctrie  :deflex :defmacro/once :once-only :with-gensyms
     :aconsf :let1 :aprog1 :awhen  :? :ppmx :printv) 
  (:export
    :all-open-mmapped-files
    :all-unmanaged-mmapped-files
    :all-mmapped-files    
    :remap
    :sync
    :mmapper
    :mm-base
    :mm-off
    :mm-len
    :mm-mf
    :make-mmapped-file
    :with-locked-mmf
    :with-mmapped-file
    :closed-mmapped-file
    :mmapped-file
    :mmapped-indexed-data-file
    :mmapped-file-header-ptr    
    :mmapped-file-fd
    :mmapped-file-mappers
    :mmapped-file-needs-fd-close
    :mmapped-file-lock
    :mmapped-file-cache
    :mmapped-file-name
    :mmapped-file-size
    :mmapped-file-growby
    :mmapped-file-byte-order
    :ensure-mmapped-file-size
    :close-mmapped-file
    :mapping-failure
    :get-page-stats
    :mmapped-file-of
    :sync-header
    :header
    :header-slot-pointer
    :header-unique-id
    :header-value
    :set-header-value
    :get-free-space-start
    :set-free-space-start
    :incf-free-space-start
    :mmptr
    :with-locked-mmptr
    :check-limits
    :with-mmptr))

