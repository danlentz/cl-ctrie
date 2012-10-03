;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMalloc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric mmalloc (mmapped-file-designator size-designator &key &allow-other-keys))

(defmethod mmalloc  (mf (amount number) &key &aux mmptr)
  (prog1 (setf mmptr (mmptr mf :offset (get-free-space-start mf)))
    (incf-free-space-start mf amount)
    (log:warn "~D raw (untyped) bytes allocated in ~A" amount
      (mmapped-file-name (mapped-file-of-mmptr mmptr)))
    (log:sexp (get-free-space-start mf))))

(defmethod mmalloc  (mf (foreign-type symbol) &key (count 1))                      
  (let ((len (* (foreign-type-size foreign-type) count))
         (off (get-free-space-start mf)))
    (with-mmptr (mmptr mf off len)
      (prog1 (mmptr mmptr :offset off :type foreign-type :count count)
        (setf (mmptr-type   mmptr) foreign-type)
        (setf (mmptr-offset mmptr) off)
        (setf (mmptr-count  mmptr) count)
        (check-limits mmptr off len)    
        (incf-free-space-start mf len)
        (log:info "~D bytes allocated at offset ~D of ~A for ~D instances of ~S"
          (- (get-free-space-start mf) off) off
          (mmapped-file-name (mapped-file-of-mmptr mmptr)) count foreign-type)
        (log:info "free-space now starts at ~D" (get-free-space-start mf))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMPTR Arithmatic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pointer-address ((mmptr mmptr))
  (mmptr-offset mmptr))

(defmethod set-pointer-address (addr (mmptr mmptr))
  (setf (mmptr-offset mmptr) addr))

(defmethod pointer-element-type ((mmptr mmptr))
  (mmptr-type mmptr))

(defmethod pointer-element-size ((mmptr mmptr))
  (cffi:foreign-type-size (mmptr-type mmptr)))

(defmethod pointer-element-count ((mmptr mmptr))
  (mmptr-count mmptr))

(defmethod incf-pointer ((mmptr mmptr) &optional (delta 1))
  (decf (mmptr-count  mmptr) delta)
  (when (not (plusp (mmptr-count mmptr)))
    (warn "Pointer ~A extended beyond allocation range" mmptr))
  (incf (mmptr-offset mmptr)
    (* delta (pointer-element-size mmptr))))

(defmethod decf-pointer ((mmptr mmptr) &optional (delta 1))
  (incf (mmptr-count  mmptr) delta)
  (decf (mmptr-offset mmptr)
    (* delta (pointer-element-size mmptr))))

(defmethod copy-pointer ((mmptr mmptr) &key type address rel-address index)
  "return a new mmptr using the same mapper as the supplied mmptr"
  (let ((type (or type (mmptr-type mmptr))))
    (make-mmptr
      :mapper (mmptr-mapper mmptr)
      :offset (+ 
                (or address (mmptr-offset mmptr))
                (or rel-address 0)
                (* (or index 0) (cffi:foreign-type-size type)))
      :count  (- (mmptr-count mmptr) (or index 0))
      :type   type)))

(defun unchecked-make-ptr (mm off type)
  "return a physical pointer for a mapper, an offset, and a specific
  c-type. Does not check first to ensure that the mapper is valid."
  (declare (ignorable type))
  (cffi:make-pointer  (+ (cffi:pointer-address (mm-base mm)) 
                        (- off (mm-off mm)))))

(defun unchecked-mmptr->ptr (mmptr &key (type (mmptr-type mmptr)) (address (mmptr-offset mmptr))
                              (offset  0) (index   0) &allow-other-keys)
  "convert an mmptr to a ptr, without checking the underlying mmapper"
  (unchecked-make-ptr (mmptr-mapper mmptr)
    (+ address offset (* index (cffi:foreign-type-size type))) type))

(defun mmptr->ptr (mmptr &key (type (mmptr-type mmptr)) (address (mmptr-offset mmptr))
                    (count (mmptr-count mmptr)) (offset 0) (index 0) &allow-other-keys)
  "convert an mmptr to a ptr, ensuring that the mmptr has a valid mapper"
  (let* ((tsize     (cffi:foreign-type-size type))
          (quantity (max 1 (- count index)))
          (offset (+ address offset (* index tsize))))
    (log:info "checking ~A for offset ~D type ~S tsize ~D quantity ~D [count ~D index ~D]"
      mmptr offset type tsize quantity count index)
    (check-limits mmptr offset (* tsize quantity))
    (unchecked-make-ptr (mmptr-mapper mmptr) offset type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Persistent" Typed Pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allocate-typed-pointer (mmapped-file-designator type &key (count 1)
                                pointer volatile free-after id)
  (apply #'make-instance 'typed-pointer
    :id id
    :element-type type
    :element-count count
    :volatile volatile
    :free-after free-after
    :pointer (list (or pointer
                     (mmptr->ptr
                       (mmalloc mmapped-file-designator 'typed-pointer))))))

(defun make-typed-pointer (mmapped-file-designator address type &key (count 1)
                            pointer volatile free-after id)
  (apply #'make-instance 'typed-pointer
    :id id
    :element-type type
    :element-count count
    :volatile volatile
    :free-after free-after
    :element-offset (if (and address (not (cffi:null-pointer-p address)))
                      address
                      (pointer-address (mmalloc mmapped-file-designator type :count count)))
    :pointer (list (or pointer
                     (mmptr->ptr
                       (mmalloc mmapped-file-designator 'typed-pointer))))))


(defgeneric box (thing place &key &allow-other-keys)
  (:method (thing place &key) 
    (let* ((mmref  (mmptr place))
            (mf    (mmapped-file-of mmref))
            (bytes (serialize thing))
            (len   (length bytes))
            (mmptr (mmalloc mmref :uint8 :count len))
            (ptr   (mmptr->ptr mmptr))
            (tp    (find-object (mmptr->ptr (mmalloc mmref 'typed-pointer)) 'typed-pointer)))
      (setf (gethash (mmptr-offset mmptr) (mmapped-file-cache mf)) thing)
      (log:info "cached reference to object of size ~D at offset ~D" len (mmptr-offset mmptr))
      (prog1 tp
        (loop for i from 0 for byte across bytes
          do (setf (mem-aref ptr :uint8 i) byte))
        (setf (element-type   tp) :box)
        (setf (element-count  tp) len)
        (setf (element-offset tp) (mmptr-offset mmptr)))))
  (:method ((thing typed-pointer) place &key)
    (let ((tp (find-object (mmptr->ptr (mmalloc (mmptr place) 'typed-pointer)) 'typed-pointer)))
      (prog1 tp
        (setf (element-type tp) (element-type thing))
        (setf (element-count tp) (element-count thing))
        (setf (element-offset tp) (element-offset thing))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footer Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric find-footer (designator)
  (:method ((mf mmapped-indexed-data-file))
    (let* ((header        (header mf))
            (offset       (footer-offset header))
            (has-footer-p (not (zerop offset)))
            (footer-mmptr (when has-footer-p
                            (mmptr mf
                              :offset offset
                              :len (footer-size-of mf))))
            (footer-ptr   (when has-footer-p
                            (mmptr->ptr footer-mmptr)))
            (footer       (when has-footer-p
                            (find-object (mem-ref footer-ptr (footer-type-for mf))
                              (footer-type-for mf)))))
      (prog1 footer
        (when has-footer-p
          (unless (check-cookie footer)
            (error (make-condition 'footer-cookie-invalid
                     :found (cookie footer)
                     :pathname (pathname (mmapped-file-name mf)))))))))
  (:method ((mmapper mmapper))
    (find-footer (mm-mf mmapper)))
  (:method ((mmptr mmptr))
    (find-footer (mmptr-mapper mmptr))))

              
(defun footer (mf-designator)
  (find-footer mf-designator))


(defgeneric get-serial (thing)
  (:method (thing)
    (let ((footer-offset (footer-offset (header thing))))
      (if (zerop footer-offset)
        0
        (serial (footer thing))))))


(defun make-footer (mmapped-file-designator root-offset action &key root-class serial
                     previous-offset (timestamp (get-universal-time)) (cookie +footer-cookie+))
  (let* ((header            (header mmapped-file-designator))
          (root-class       (or root-class (root-class header) *default-root-class*))
          (previous-offset  (or previous-offset (footer-offset header)))
          (previous-footer  (unless (zerop previous-offset)
                              (find-object (mmptr->ptr
                                             (mmptr mmapped-file-designator
                                               :offset previous-offset
                                               :type   (footer-type-for mmapped-file-designator)
                                               :count  1))
                                (footer-type-for mmapped-file-designator))))
          (serial           (or serial (if previous-footer
                                         (1+ (serial previous-footer))
                                         1)))
          (new-footer-mmptr (mmalloc mmapped-file-designator
                              (footer-type-for mmapped-file-designator)))
          (new-footer-ptr   (mmptr->ptr new-footer-mmptr))
          (new-footer       (find-object
                              (mem-ref new-footer-ptr (footer-type-for mmapped-file-designator))
                              (footer-type-for mmapped-file-designator))))
    (prog1 new-footer-mmptr
      (setf
        (cookie new-footer)                 cookie
        (action new-footer)                 action
        (serial new-footer)                 serial
        (timestamp new-footer)              timestamp
        (previous-footer-offset new-footer) previous-offset
        (root-offset  new-footer)           root-offset
        (root-class   new-footer)           root-class))))



(defgeneric update-indexed-data-file (designator root-offset action)
  (:documentation "two-phase commit updating footer at eof and then header with location of
   the new footer")
  (:method ((mf mmapped-file) root-offset action)
    (prog1 mf
      (set-header-value mf 'footer-offset (mmptr-offset (make-footer mf root-offset action)))
      (sync mf)))
  (:method ((mm mmapper) root-offset action)
    (update-indexed-data-file (mm-mf mm) root-offset action))
  (:method ((mmptr mmptr) root-offset action)
    (update-indexed-data-file (mmptr-mapper mmptr) root-offset action))
  (:method (thing root-offset action)
    (update-indexed-data-file (mmptr thing) root-offset action)))


;; todo: add specializations for other lisps' pointer types or figure out if cffi provides
;; a usable class specializer for values with type cffi::pointer-type
;; (why doesn't it appear to have this??)

#+sbcl 
(defmethod pointer:deref ((thing sb-alien:system-area-pointer) &optional (type :uint8)
                           &rest args &aux (count (if args (or (getf args :count) 1) 1)))
  (log:sexp "dereference: " thing type args count)
  (if (zerop (1- count))
    (mem-ref thing type)
    (let ((result (make-array count)))
      (prog1 result
        (loop for i from 0 to (1- count)
          do (setf (aref result i) (mem-aref thing type i)))))))


(defmethod pointer:deref ((thing mmptr) &optional type &rest args
                           &aux primary (secondary (cffi:null-pointer)))
  (declare (ignore args))
  (setf primary  (apply #'pointer:deref (mmptr->ptr thing) (mmptr-type thing)
                   (list :count (mmptr-count thing))))
  (when (and (eq type :unbox) (eq (mmptr-type thing) 'typed-pointer))
    (if (eq (element-type primary) :null)
      (setf secondary nil)
      (with-mmptr (box thing
                    (element-offset primary)
                    (* (foreign-type-size (element-type primary)) (element-count primary)))
        (setf secondary (deserialize (pointer:deref box))))))
  (values primary secondary))


(define-condition box-error (error)
  ())

(define-condition no-unboxed-value (box-error)
  ())


(defgeneric unbox (location)
  (:method ((location mmptr))
    (multiple-value-bind (mmptr unboxed-value) (pointer:deref location :unbox)
      (declare (ignorable mmptr))
      (if (cffi:null-pointer-p unboxed-value)
        (error (make-condition 'no-unboxed-value))
        unboxed-value)))
  (:method ((vector vector))
    (deserialize vector)))
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(defun pointer-offset-for-mapper (mm p)
  "given a mapper and a physical pointer, find the offset needed by
  an equivalent mmptr"
  (declare (optimize (speed 3) (safety 0) (float  0)))
  (+ (the integer (mm-off mm))
     (the integer (- (the integer (pointer-address p))
                     (the integer (pointer-address (mm-base mm)))))))

(defun unchecked-ptr->mmptr (ptr mapper &key type)
  "convert a ptr back to an mmptr, without checking on the underlying mmapper"
  (make-mmptr
   :mapper mapper
   :offset (pointer-offset-for-mapper mapper ptr)
   :type   (or type :uint8))) ;; (cffi:pointer-element-type ptr))))



(defun check-mmptr (mmptr &key (nelems 1))
  ;; ensure that an mmptr has a valid underlying mapper
  (declare (type fixnum nelems))
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (check-limits mmptr
                (mmptr-offset mmptr)
                (* nelems
                   (the fixnum (pointer-element-size mmptr)))))



;; user API
(defmethod fetch ((mmptr mmptr)
                  &rest args
                  &key
                  (type (mmptr-type mmptr))
                  address
                  offset
                  (index 0)
                  (byte-order (mmptr-byte-order mmptr)))
  (declare (ignore address offset index))
  (with-locked-mmptr (mmptr)
    (fetch (apply #'mmptr->ptr mmptr args)
           :type       type
           :byte-order byte-order)))



;; user API
(defmethod store (val (mmptr mmptr)
                      &rest args
                      &key
                      (type (mmptr-type mmptr))
                      address
                      offset
                      (index 0)
                      (byte-order (mmptr-byte-order mmptr)))
  (declare (ignore address offset index))
  (with-locked-mmptr (mmptr)
    (store val (apply #'mmptr->ptr mmptr args)
           :type       type
           :byte-order byte-order)))
  


(defun do-mmptr-from-intermediate-pointer (mmptr fn)
  ;; call fn on an fli:pointer derived (unchecked) from mmptr
  ;; fn is expected to return another fli:pointer, which is then
  ;; converted back to an mmptr
  (let* ((pbase (unchecked-mmptr->ptr mmptr))
         (pcell (funcall fn pbase)))
    (unchecked-ptr->mmptr pcell (mmptr-mapper mmptr)) ))

(defmacro mmptr-from-intermediate-pointer ((ptr mmptr) &body body)
  `(do-mmptr-from-intermediate-pointer ,mmptr (lambda (,ptr) ,@body)))


(defmethod foreign-array-pointer ((mmptr mmptr) &rest indices)
  ;; return an array element mmptr
  (mmptr-from-intermediate-pointer (p mmptr)
    (apply #'foreign-array-pointer p indices)))

(defmethod foreign-array-element-type ((mmptr mmptr))
  (foreign-array-element-type (mmptr-type mmptr)))
                                
(defmethod foreign-array-dimensions ((mmptr mmptr))
  (foreign-array-dimensions (mmptr-type mmptr)))



(defun chk-vector-range-limits (mmptr start &optional end)
  ;; ensure that a vector range is covered by a valid underlying
  ;; mapper
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (declare (type fixnum start))
  (let* ((dims (foreign-array-dimensions mmptr))
         (len  (reduce #'* dims))
         (end  (if end
                   (min (the fixnum end) len)
                 len))
         (nel  (- end start))
         (mmptr-start (foreign-array-pointer mmptr start)))
    (declare (type fixnum end nel))
    (check-mmptr mmptr-start :nelems nel)
    (values mmptr-start nel)))

;; user API
(defmethod fetch-vector ((mmptr mmptr)
                         &key
                         ltype
                         (start 0)
                         end
                         (byte-order (mmptr-byte-order mmptr)))
  (with-locked-mmptr (mmptr)
    ;; NB: problem lurking here for super-large transfers... fix this!
    (multiple-value-bind (mmptr-start nel)
        (chk-vector-range-limits mmptr start end)
      (let ((ptr (mmptr->ptr (copy-pointer mmptr
                                           :address (pointer-address mmptr-start)))))
        (fetch-vector ptr :start 0 :end nel :byte-order byte-order :ltype ltype) ))))

;; user API
(defmethod store-vector (arr (mmptr mmptr)
                             &key
                             (start1 0)
                             end1
                             (start2 0)
                             (byte-order (mmptr-byte-order mmptr)))
  (with-locked-mmptr (mmptr)
    ;; NB: problem lurking here for super-large transfers... fix this!
    (let ((nel (- (or end1 (length arr)) start1)))
      (multiple-value-bind (mmptr-start xnel)
          (chk-vector-range-limits mmptr start2 (+ start2 nel))
        (declare (ignore xnel))
        (let ((ptr (mmptr->ptr (copy-pointer mmptr
                                             :address (pointer-address mmptr-start)))))
          (store-vector arr ptr :start1 start1 :end1 end1 :start2 0
                        :byte-order byte-order) )))))



;; user API
(defmethod fetch-array ((mmptr mmptr)
                        &key
                        ltype
                        (byte-order (mmptr-byte-order mmptr)))
  (with-locked-mmptr (mmptr)
    (chk-vector-range-limits mmptr 0)
    (fetch-array (mmptr->ptr mmptr)
                 :ltype ltype
                 :byte-order byte-order)))

;; user API
(defmethod store-array (arr (mmptr mmptr)
                            &key
                            (byte-order (mmptr-byte-order mmptr)))
  (with-locked-mmptr (mmptr)
    (chk-vector-range-limits mmptr 0)
    (store-array arr (mmptr->ptr mmptr)
                 :byte-order byte-order)))


;; ASCII 8-bit Strings

(defmethod fetch-ascii-string ((mmptr mmptr) &key length null-terminated-p)
  (with-locked-mmptr (mmptr)
    (chk-vector-range-limits mmptr 0)
    (fetch-ascii-string (mmptr->ptr mmptr)
                        :length length
                        :null-terminated-p null-terminated-p)))

(defmethod store-ascii-string (str (mmptr mmptr) &key length null-terminated-p)
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (with-locked-mmptr (mmptr)
    (let* ((nstr (if null-terminated-p
                     (or (position #\null str)
                         (length str))
                   (length str)))
           (nel  (if length
                     (min (the fixnum length)
                          (the fixnum nstr))
                   nstr)))
      (chk-vector-range-limits mmptr 0 nel)
      (store-ascii-string str (mmptr->ptr mmptr)
                          :length nel
                          :null-terminated-p null-terminated-p))))



(defmethod foreign-slot-names ((mmptr mmptr))
  (foreign-slot-names (mmptr-type mmptr)))

(defmethod foreign-slot-offset ((mmptr mmptr) slot-name)
  (foreign-slot-offset (mmptr-type mmptr) slot-name))

(defmethod foreign-slot-type ((mmptr mmptr) slot-name)
  (foreign-slot-type (mmptr-type mmptr) slot-name))

;; user API
(defmethod foreign-slot-pointer ((mmptr mmptr) slot-name &rest args
                                 &key type object-type)
  ;; return an mmptr that points to a specific struct slot
  (declare (ignore type object-type))
  (mmptr-from-intermediate-pointer (p mmptr)
    (apply #'foreign-slot-pointer p slot-name args)))



(defun nbytes-forward-to-page-end (mmptr)
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (- (the fixnum +page-size+)
     (rem (the integer (mmptr-offset mmptr))
          (the fixnum +page-size+))))

(defun nbytes-back-to-page-start (mmptr)
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (let ((off (rem (the integer (mmptr-offset mmptr))
                  (the fixnum +page-size+))))
    (declare (type fixnum off))
    (if (zerop off)
        +page-size+
      off)))



(defmethod copy-from-region-to-fli-pointer ((mmptr-from mmptr) from-position pto nb)
  ;; mapped pointer to native pointer
  ;; okay to destructively modify pto because it was copied from original args
  (declare (type integer nb from-position))
  (declare (optimize (speed  3)
             (safety 0)
             (float  0)))
  (with-locked-mmptr (mmptr-from)
    (let ((pfrom (copy-pointer mmptr-from
                   :type :uint8
                   :index (* from-position
                            (the fixnum (fli:size-of (pointer-element-type mmptr-from)))))))
      (labels ((iter ()
                 (when (plusp nb)
                   (let ((nbx (min nb
                                (the fixnum (nbytes-forward-to-page-end pfrom)))))
                     (declare (type fixnum nbx))
                     (check-mmptr pfrom :nelems nbx)
                     (fli:replace-foreign-array pto (unchecked-mmptr->ptr pfrom)
                       :start1 0  :end1 nbx
                       :start2 0  :end2 nbx)
                     (incf-pointer pfrom nbx)
                     (incf-pointer pto nbx)
                     (decf nb nbx)
                     (iter)) )))
        (iter)) )))
    


(defmethod copy-from-region-to-mmptr ((pfrom fli::pointer) from-position pto nb)
  ;; native pointer to mapped pointer
  ;; okay to destructively modify pto because it was copied from original args
  (declare (type integer nb from-position))
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (with-locked-mmptr (pto)
    (let ((pfrom (copy-pointer pfrom
                               :type :uint8
                               :index (* from-position
                                         (the fixnum (fli:size-of (pointer-element-type pfrom)))))))
      (labels ((iter ()
                 (when (plusp nb)
                   (let ((nbx (min nb
                                   (the fixnum (nbytes-forward-to-page-end pto)))))
                     (declare (type integer nbx))
                     (check-mmptr pto :nelems nbx)
                     (fli:replace-foreign-array (unchecked-mmptr->ptr pto) pfrom
                                                :start1 0 :end1 nbx
                                                :start2 0 :end2 nbx)
                     (incf-pointer pfrom nbx)
                     (incf-pointer pto nbx)
                     (decf nb nbx)
                     (iter)) )))
        (iter)) )))


;; Lock pairs -- must be locked in consistent order to avoid deadlocks

(defun do-with-locked-mmptrs (mmptr1 mmptr2 fn)
  ;; avoid deadlock by locking in fd order
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (let* ((mf1 (mapped-file-of-mmptr mmptr1))
         (fd1 (mmapped-file-fd mf1))
         (mf2 (mapped-file-of-mmptr mmptr2))
         (fd2 (mmapped-file-fd mf2)))
    (declare (type integer fd1 fd2))
    (when (> fd1 fd2)
        (rotatef mf1 mf2))
    (with-locked-mmf (mf1)
      (with-locked-mmf (mf2)
        (funcall fn))) ))

(defmacro with-locked-mmptrs ((mmptr1 mmptr2) &body body)
  `(do-with-locked-mmptrs ,mmptr1 ,mmptr2 (lambda ()
                                            ,@body)))


(defmethod copy-from-region-to-mmptr ((from-mmptr mmptr) from-position pto nb)
  ;; mapped pointer to mapped pointer
  ;; okay to destructively modify pto because it was copied from original args
  (declare (type integer nb from-position))
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (with-locked-mmptrs (from-mmptr pto)
    (let ((pfrom (copy-pointer from-mmptr
                               :type  :uint8
                               :index (* from-position
                                         (the fixnum (fli:size-of (pointer-element-type from-mmptr)))))))
      (if (> (mmptr-offset pto) (mmptr-offset pfrom))
          ;; copy backwards in case of overlapping regions
          (progn
            (incf-pointer pfrom nb)
            (incf-pointer pto nb)
            (labels ((iter ()
                       (when (plusp nb)
                         (let ((nbx (min nb
                                         (the fixnum (nbytes-back-to-page-start pfrom))
                                         (the fixnum (nbytes-back-to-page-start pto)))))
                           (declare (type fixnum nbx))
                           (decf-pointer pfrom nbx)
                           (decf-pointer pto nbx)
                           (check-mmptr pfrom :nelems nbx)
                           (check-mmptr pto :nelems nbx)
                           (fli:replace-foreign-array (unchecked-mmptr->ptr pto)
                                                      (unchecked-mmptr->ptr pfrom)
                                                      :start1 0  :end1 nbx
                                                      :start2 0  :end2 nbx)
                           (decf nb nbx)
                           (iter))) ))
              (iter)))
        ;; else
        ;; copy forward
        (labels ((iter ()
                   (when (plusp nb)
                     (let ((nbx (min nb
                                     (the fixnum (nbytes-forward-to-page-end pfrom))
                                     (the fixnum (nbytes-forward-to-page-end pto)))))
                       (declare (type fixnum nbx))
                       (check-mmptr pfrom :nelems nbx)
                       (check-mmptr pto   :nelems nbx)
                       (fli:replace-foreign-array (unchecked-mmptr->ptr pto)
                                                  (unchecked-mmptr->ptr pfrom)
                                                  :start1 0 :end1 nbx
                                                  :start2 0 :end2 nbx)
                       (incf-pointer pfrom nbx)
                       (incf-pointer pto nbx)
                       (decf nb nbx)
                       (iter))) ))
          (iter)) ))))

(defmethod copy-region ((mmptr-to mmptr) to-position from from-position nel
                        &key type)
  ;; from somewhere to mapped pointer
  (declare (type integer nel from-position to-position))
  (declare (optimize (speed  3)
                      (safety 0)
                      (float  0)))
  (let* ((type (or type (pointer-element-type mmptr-to)))
         (nb   (* nel (the fixnum (fli:size-of type))))
         (pto  (copy-pointer mmptr-to
                             :type :uint8
                             :index (* to-position (the fixnum (fli:size-of type))))))
    (copy-from-region-to-mmptr from from-position pto nb)))

|#
