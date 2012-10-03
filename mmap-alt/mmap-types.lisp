;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UUID Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :uuid)
    (push :uuid *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcstruct* unique-id
    (bytes :uint8 :count 16)))

(defmethod pointer:deref ((unique-id unique-id) &optional
                           (type 'unicly:unique-universal-identifier) &rest args)
  (declare (ignore args))
  (case type
    #+unicly
    (unicly:unique-universal-identifier
      (unicly:uuid-from-bit-vector 
        (unicly:uuid-byte-array-to-bit-vector
          (bytes unique-id))))
    #+uuid
    (uuid:uuid
      (uuid:byte-array-to-uuid (bytes unique-id)))
    (vector
      (bytes unique-id))))

;; (pointer:deref (make-instance 'unique-id :bytes (create-unique-id-byte-vector)))
;;   d12dd006-5876-4edf-9f23-68a2d72a20fa
;;
;; (pointer:deref (make-instance 'unique-id :bytes (create-null-id-byte-vector)))
;;   00000000-0000-0000-0000-000000000000
;;
;; (pointer:deref (make-instance 'unique-id :bytes (create-unique-id-byte-vector)) 'vector)
;;   #(22 191 232 233 127 79 66 158 144 160 169 198 198 223 141 101)
;;
;; (pointer:deref (make-instance 'unique-id :bytes (create-unique-id-byte-vector)) 'uuid:uuid)
;;   73FD72B4-BE51-483C-B8FE-6303AF69BE34

;; (defmethod (setf pointer:deref) ((uuid unicly:unique-universal-identifier)
;;                                   (location mmapped-indexed-data-file)
;; &optional (type 'unique-id) &rest args)
;;
;;   (declare (ignore args))
;;   (let* ((unique-id-mmptr (mmalloc location type))
;;           (ptr (mmptr->ptr unique-id-mmptr))
;;           (bytes (unicly:uuid-bit-vector-to-byte-array (unicly:uuid-to-bit-vector uuid))))
;;     (loop
;;       for byte across bytes for i from 0
;;       do (setf (mem-aref ptr :uint8 i) byte))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timestamp Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
(defcstruct* timestamp
  (day  :int)
  (sec  :uint16)
  (nsec :uint32)))

(defmethod pointer:deref ((timestamp timestamp) &optional (type 'local-time:timestamp) &rest args)
  (declare (ignorable type args))
  (apply #'make-instance type
    (loop with slot-names = (foreign-slot-names 'timestamp)
      for slot in slot-names
      collect (sb-int:keywordicate slot)
      collect (funcall slot timestamp))))

;; (pointer:deref (make-instance 'timestamp :day 2345 :sec 55555 :nsec 3456578))
;;
;;   @2006-08-02T11:25:55.003456-04:00
;;   @2006-08-02T11:25:55.003456-04:00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed-Pointer Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcenum pointer-type
  :null
  :unbound
  :pointer 
  :uuid
  :far
  :box
  :root
  :tuple
  :set
  :map
  :seq)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defcstruct* typed-pointer
  (element-type   pointer-type)
  (element-count  :uint)
  (element-offset :uint)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defcstruct* pair
  (head typed-pointer)
  (tail  typed-pointer)))


(defcenum action
  :none
  :create
  :merge
  :gc
  :upgrade
  :clear)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
(defcstruct* standard-indexed-data-file-header
  (cookie              :uint)
  (unique-id           :uint8 :count 16)
  (version-major       :uint)
  (version-minor       :uint)
  (creation-timestamp  :uint)
  (free-space-start    :uint)
  (root-class          pointer-type)
  (footer-offset       :uint))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footer-Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
(defcstruct* standard-indexed-data-file-footer
  (cookie                 :uint)
  (serial                 :uint)
  (timestamp              :uint)
  (action                 action)
  (previous-footer-offset :uint)
  (root-offset            :uint)
  (root-class             pointer-type))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segmented Extent List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defcstruct* segmented-extent-list
  (first-elt         :uint)
  (last-elt          :uint)
  (segment-count     :uint)
  (segmment-type     pointer-type))


(defcstruct* sequence-elt
  (elt-num   :uint)
  (elt-up    :uint)
  (elt-prev  :uint)
  (elt-next  :uint)
  (elt-fill  :uint))


(defcstruct* data-block
  (segment-info sequence-elt)
  (segment-storage :uint8 :count 4000))
|#

;; (foreign-type-size 'segmented-extent-list)
;; 16

;; (foreign-type-size 'data-block)
;; 4020
  
