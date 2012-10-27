;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threaded Pre-Allocation and Memory Pooling
;;
;; This Memory Alocation Pooling System is effectively 'stateless' and requires
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-Level API for  Enable/Disable Pooled Allocation Services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ctrie-enable-pooling ()
  (setf *pool-enabled* t))

(defun ctrie-disable-pooling ()
  (setf *pool-enabled* nil))

(defun ctrie-pooling-enabled-p ()
  (not (null *pool-enabled*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pools and Bucket Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype bucket ()
  '(cons t vector))

(defun make-bucket ()
  (cons nil (make-array *pool-high-water* :fill-pointer 0)))

(defun bucket? (thing)
  (typep thing 'bucket))

(defun bucket-low? (bucket)
  (< (length (cdr bucket)) (/ *pool-high-water* 4)))

(defun bucket-empty? (bucket)
  (zerop (length (cdr bucket))))

(defun pool-bucket-count (class-name)
  (or (get class-name :pool-bucket-count) 0))

(defun (setf pool-bucket-count) (value class-name)
  (setf (get class-name :pool-bucket-count) (or value 0)))

(defun pool-allocator (class-name)
  (get class-name :pool-allocator))

(defun (setf pool-allocator) (fn class-name)
  (setf (get class-name :pool-allocator) fn))

(defun find-pool (class-name)
  (or (getf *pool-list* class-name)
    (setf (getf *pool-list* class-name)
      (make-array (1+ (pool-bucket-count class-name)) :adjustable t
        :initial-contents (loop repeat (1+ (pool-bucket-count class-name))
                            collect (make-bucket))))))

(defun find-bucket (class-name index)
  (aref (aprog1 (find-pool class-name)
          (when (> index (1- (length it)))
            (loop for elt from 0
              for bucket across (adjust-array it (1+ index))
              unless (bucket? bucket) do (setf (aref it elt) (make-bucket))
              finally (setf (pool-bucket-count class-name) index))))
    index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE-POOL macro encapsulates the principle developer interface from which
;; pooled allocation systems may be constructed in a simple and versitile manner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-pool (name (&optional (initial-buckets 0)) &body body)
  (with-unique-names (num-buckets)
    `(let ((,num-buckets ,initial-buckets))
       (prog1 ',name       
         (setf (pool-allocator ',name) (lambda (&optional bucket)
                                         (declare (ignorable bucket))
                                         ,@body))
         (setf (pool-bucket-count ',name) ,num-buckets)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pool Worker Facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun fill-bucket (class-name index)
  (aprog1 (find-bucket class-name index)
    (when (bucket-low? it)
      (sb-thread::with-cas-lock ((car it))
        (loop repeat *pool-high-water*
          always (< (length (cdr it)) *pool-high-water*)
          do (vector-push (funcall (pool-allocator class-name) index)
               (cdr it)))))))

(defun fill-pool (class-name)
  (loop for index from 0 for bucket across (find-pool class-name)
    do (fill-bucket class-name index)))
      
(defun fill-all-pools ()
  (loop for (class-name pool) on (pool-list) by #'cddr
    when (and class-name pool) do (fill-pool class-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple (single cons-cell) Message Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype negative-number ()
  '(satisfies minusp))

(deftype request-for-termination ()
  '(cons null null))

(defun request-for-termination-p (message)
  (typep message 'request-for-termination))

(defun request-termination! ()
  (sb-concurrency:send-message (pool-queue)
    (cons nil nil)))

(deftype request-for-preallocation ()
  '(cons symbol number))

(defun request-preallocation! (class-name bucket-index)
  (assert (sb-thread:thread-alive-p (pool-worker)))
  (sb-concurrency:send-message (pool-queue)
    (cons class-name bucket-index)))

(deftype request-for-reallocation ()
  '(cons t null))

(defun request-reallocation! (old-thing)
  (sb-concurrency:send-message (pool-queue)
    (list old-thing)))

(deftype request-for-gc ()
  '(cons null negative-number))

(defun request-gc! ()
  (sb-concurrency:send-message (pool-queue)
    (cons nil -1)))

(deftype request-for-dynamic-action ()
  '(cons function cons))

(defun request-dynamic-action! (fn args)
  (assert (sb-thread:thread-alive-p (pool-worker)))
  (sb-concurrency:send-message (pool-queue)
    (cons fn (ensure-list args))))

(defun request-ping! ()
  (assert (sb-thread:thread-alive-p (pool-worker)))
  (sb-concurrency:send-message (pool-queue)
    (cons #'(lambda (x)
              (let1 reply  (format nil "~A! ~D Messages Queued ~A"
                           x (sb-concurrency:mailbox-count (pool-queue)) (local-time:now))
                (printv reply)))
      (list :ping))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message Processing and  Dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fulfill-request (message)
  (atypecase message
    (request-for-preallocation  (fill-bucket (car it) (cdr it)))
    (request-for-reallocation   (warn "reallocation not yet implemented"))
    (request-for-gc             (ctrie-gc (pool-list)))
    (request-for-dynamic-action (apply (car it) (cdr it) nil))
    (t                          (error "unhandled message: ~S" it))))


(defun pool-work-loop ()
  (loop
    initially (warn "worker started")
    with terminate until terminate
    for message = (sb-concurrency:receive-message (pool-queue))
    do (printv "MESSAGE:" message)
    if (request-for-termination-p message) do (setf terminate t)
    else do (fulfill-request message)
    finally (warn "worker terminated")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dedicated Storage Allocation Management Thread (Pool Worker)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-Level User API for Pooled Resource Services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun/inline allocate (class-name &optional (bucket-index 0) &aux (attempts 0))
  (labels ((bail ()
             (request-termination!)
             (error "giving up on ~A" (pool-worker)))
            (retry ()
              (when (zerop (mod attempts 16))
                (request-preallocation! class-name bucket-index))
              (incf attempts)
              (sleep (* .01 attempts))
              (throw 'try-again nil)))
    (sb-ext:with-timeout 15
      (assert (pool-worker))
      (loop until
        (catch 'try-again
          (if (> attempts 1000)
            (bail)
            (let1 bucket (find-bucket class-name bucket-index)
              (when (bucket-low? bucket)
                (if (bucket-empty? bucket)
                  (retry)
                  (when (zerop (mod attempts 48))
                    (request-preallocation! class-name bucket-index))))
              (sb-thread::with-cas-lock ((car bucket))
                (if (bucket-empty? bucket)
                  (retry)
                  (return-from allocate
                    (vector-pop (cdr bucket))))))))))))


