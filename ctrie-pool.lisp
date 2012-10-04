;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threaded Pre-Allocation and Memory Pooling
;;
;; This Memory Alocation Pooling System is completely 'stateless' and requires
;; no special interaction or management by the developer/end-user.  No processing
;; or consumption of resources will occur until the first user-requested allocation,
;; at which time the worker thread will initialize and fill all defined pools and
;; their bucket vectors.  After this point, all further preallocation will occur
;; lazily on-demand.
;;
;; One fiddly-bit that one might care to make use-of is #'CTRIE-POOL-STATUS,
;; which will return an alist describing the current fill-level for all currently
;; defined pools and their constituent bucket vectors.
;;
;; I especially like the way this came together.  Still, there is plenty of
;; room for improvement, of course -- most notably some more effort toward 
;; optimization, condition handling, and api refinements would be worthwhile.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ctrie-enable-pooling ()
  (setf *pool-enabled* t))

(defun ctrie-disable-pooling ()
  (setf *pool-enabled* nil))

(defun ctrie-pooling-enabled-p ()
  (not (null *pool-enabled*)))

(defun pool-buckets (class-name)
  (or (get class-name :pool-buckets) 0))

(defun pool-serial (class-name)
  (or (get class-name :pool-serial)
    (setf (get class-name :pool-serial) 0)))

(defun (setf pool-buckets) (value class-name)
  (setf (get class-name :pool-buckets) (or value 0)))

(defun pool-alloc (class-name)
  (get class-name :pool-alloc))

(defun (setf pool-alloc) (fn class-name)
  (setf (get class-name :pool-alloc) fn))

(defun find-pool (class-name)
  (or (getf *pool-list* class-name)
    (setf (getf *pool-list* class-name)
      (let1 buckets (pool-buckets class-name)
        (make-array (1+ buckets) :initial-contents
          (loop repeat (1+ buckets) collect (cons nil nil)))))))

(defmacro define-pool (name (&optional (buckets 0)) &body body)
  (with-unique-names (num-buckets)
    `(let ((,num-buckets ,buckets))
       (prog1 ',name
         (setf (get ',name :pool-serial) 0)
         (setf (pool-alloc ',name) (lambda (&optional bucket)
                                     (declare (ignorable bucket))
                                     ;; (sb-ext:atomic-incf
                                     ;;   (getf (symbol-plist ',name) :pool-serial))
                                     ,@body))
         (setf (pool-buckets ',name) ,num-buckets)
         (find-pool ',name)))))

(defun ctrie-pool-status ()
  (loop for class-name in *pool-list* by #'cddr
    when class-name
    collect (cons class-name
              (coerce (loop for p across (find-pool class-name)
                        collect (length (cdr p)))
                'vector))))
  
(defun ps ()
  (ctrie-pool-status))

(defun ctrie-ps ()
  (ctrie-pool-status))

(defun/inline pool-list ()
  *pool-list*)

(defun/inline pool-queue ()
  (or *pool-queue* 
    (setf *pool-queue*
      (sb-concurrency:make-mailbox :name "pool-queue"))))

(defun destroy-pool-list ()
  (setf *pool-list* nil))

(defun destroy-pool-queue ()
  (setf *pool-queue* nil))

(defun fill-pool (class-name &optional (bucket 0))
  (let1 pool (svref (find-pool class-name) bucket)
    (unless (cdr pool)
      (when *debug* (printv "filling pool" class-name bucket ""))
      (sb-thread::with-cas-lock ((car pool))
        (setf (cdr pool)
          (loop repeat *pool-high-water*
            collect (apply (pool-alloc class-name)
                      (ensure-list (unless (zerop bucket) bucket)))))))))

(defun fill-all-pools ()
  (loop for (class-name pool) on (pool-list) by #'cddr
    when class-name
    do (printv class-name)
    do (dolist (bucket (iota (1+ (pool-buckets class-name)) :start 0))
         (sb-concurrency:send-message (pool-queue) (cons class-name bucket)))))

(defun pool-work-loop ()
  (loop with terminate until terminate
    for request = (ensure-list (sb-concurrency:receive-message (pool-queue)))
    if (and (cdr request) (minusp (cdr request))) do (setf terminate t)
    else do (fill-pool (car request) (or (cdr request) 0))))

(defun/inline pool-worker ()
  (if (and *pool-worker* (sb-thread:thread-alive-p *pool-worker*))
    *pool-worker*
    (setf *pool-worker*
      (sb-thread:make-thread
        (lambda ()
          (fill-all-pools)
          (pool-work-loop)
          (destroy-pool-list)
          (destroy-pool-queue)
          (setf *pool-worker* nil)
          (sb-thread:terminate-thread sb-thread:*current-thread*))
        :name "pool-worker"))))

(defun/inline allocate (class-name &optional (bucket 0))
  (if (minusp bucket)
    (sb-concurrency:send-message (pool-queue) (cons class-name bucket)) 
    (flet ((bail ()
             (pool-worker)
             (sb-concurrency:send-message (pool-queue) (cons class-name bucket)) 
             (return-from allocate
               (apply (pool-alloc class-name) (ensure-list (unless (zerop bucket) bucket))))))
      (let1 pool (svref (find-pool class-name) bucket)
        (when (null (cdr pool)) (bail))
        (sb-thread::with-cas-lock ((car pool))
          (unless (cdr pool) (bail))
          (prog1 (pop (cdr pool))
            (unless (cdr pool)
              (pool-worker)
              (sb-concurrency:send-message (pool-queue) (cons class-name bucket))))))))) 

