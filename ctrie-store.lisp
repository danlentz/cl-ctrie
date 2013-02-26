;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Storage and Layered Allocation Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compute-default-location-for-store ()
  (ensure-directories-exist (merge-pathnames #p".ctrie-store/"
                              (user-homedir-pathname))
    :verbose t))


(defun open-store (&optional (directory-pathname (compute-default-location-for-store)))
  (let ((in-place (and (boundp 'mm::*mmap-base-pathname*)
                    (equal (truename directory-pathname)
                      (truename mm::*mmap-base-pathname*)))))
    (when (and in-place (mm::schema))
      (return-from open-store (truename directory-pathname)))
    (when (mm::schema)
      (mm:close-all-mmaps))
    (aprog1 (mm:ensure-manardb directory-pathname)
      ;; (ctrie-gc)
      (mm::check-schema it))))


(clear-layer-caches)

#+()
(deflayer printv ()
  ((stream :initform *trace-output* :initarg :stream :accessor printv-stream)))

#+notyet
(deflayer allocation ()
  ((pool-p :accessor allocation-pool-p :initarg :pool-p :initform t)
    (pool-list :accessor allocation-pool-list :initarg :pool-list :initform nil)
    (pool-queue :accessor allocation-pool-queue :initarg :pool-queue :initform nil)
    (pool-worker :accessor allocation-pool-worker :initarg :pool-worker :initform nil)
    (pool-limit :accessor allocation-pool-limit :initarg :pool-limit :initform nil)
    ))



(deflayer allocation)

(deflayer transient  (allocation))
  
(deflayer persistent (allocation)
  ((storage-directory-pathname
     :accessor storage-directory-pathname
     :initform (apply 'open-store (ensure-list *default-mmap-dir*)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store Initialization and Management Utilities
;;
;; The CTRIE-PERSISTENT-STORE implements automatic self-initialization and a
;; stateless interface that should not require activation or user interaction
;; during the course of normal operation.  The following utilities are exported
;; as part of the public API with only the intention to provide a means of
;; simple diagnostic and troubleshooting support in the event of failure or 
;; other exceptional situations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ctrie-persistent-store ()
  (if (find 'persistent (active-layers) :key #'layer-name)
    (storage-directory-pathname
      (find-layer 'persistent))
    (with-active-layers (persistent)
      (storage-directory-pathname
        (find-layer 'persistent)))))


(defun ctrie-gc (&rest additional-roots)
  (let* ((store (ctrie-persistent-store))
         (message (format nil "Compacting CTRIE-PERSISTENT-STORE: ~A" store)))
    (mm::with-transaction (:message message)
      (funcall #'mm:gc
        (list*
          (first (mm:retrieve-all-instances 'ctrie-index))
           additional-roots) :verbose t))))


