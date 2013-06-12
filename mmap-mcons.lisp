;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)


(defmmclass mcons (marray)
  ((mcar :reader mcar :persistent nil)
    (mcdr :reader mcdr :persistent nil)))


(defmethod slot-unbound (class (instance mcons) (slot-name (eql 'mcar))) 
  (setf (slot-value instance slot-name) (marray-ref instance 0)))

(defmethod slot-unbound (class (instance mcons) (slot-name (eql 'mcdr))) 
  (setf (slot-value instance slot-name) (marray-ref instance 1)))

(defgeneric mcar (cell))
(defgeneric mcdr (cell))
(defgeneric mcadr (cell))
(defgeneric mcddr (cell))
(defgeneric (setf mcar) (value cell))
(defgeneric (setf mcdr) (value cell))
(defgeneric mconsp (thing))
(defgeneric emptyp (thing))
(defgeneric as-list (thing))
(defgeneric mpush (element place))
(defgeneric mpop (element place))
  

(defmethod mcar ((cell null))
  nil)

(defmethod mcdr ((cell null))
  nil)

(defmethod mcadr ((cell mcons))
  (mcar (mcdr cell)))

(defmethod mcddr ((cell mcons))
  (mcdr (mcdr cell)))

(defmethod (setf mcar) (value (mcons mcons))
  (setf (marray-ref mcons 0) value)
  (setf (slot-value mcons 'mcar) value))

(defmethod (setf mcdr) (value (mcons mcons))
  (setf (marray-ref mcons 1) value)
  (setf (slot-value mcons 'mcdr) value))

(defun mcons (mcar mcdr)
  (make-marray 2 :marray-class 'mcons :initial-contents (list mcar mcdr)))

(defmethod mconsp (thing)
  nil)

(defmethod mconsp ((thing mcons))
  t)

(defmethod mconsp ((thing null))
  t)

(defmethod print-object ((self mcons) stream)
  (print-unreadable-object (self stream)
    (loop
      initially (format stream "(")
      for first = self then (mcdr first)
      while first do (typecase first
                       (mcons (if (mcar first)
                                (format stream " ~S" (mcar first))
                                (loop-finish)))
                       (atom  (format stream " . ~S" first) (loop-finish))
                       (null  (loop-finish)))
      finally (format stream " )"))))

(defun empty ()
  (mcons nil nil))

(defmethod emptyp ((cell null))
  t)

(defmethod emptyp ((cell mcons))
  (null (mcar cell)))

(defun mlist (&rest args)
  (loop with new = (mcons nil nil)
    for first = (nreverse args) then (cdr first)
    while first do (setf new (mcons (car first) new))
    finally (return new)))

(defmethod as-list ((mlist mcons))
  (loop 
    for first = mlist then (mcdr first)
    when (not (mconsp first)) collect first 
    when (and first (mconsp first) (not (emptyp first))) collect (mcar first)
    until (or (not (mconsp first)) (emptyp first))))


;; (as-list (mlist :a :b :c :D))       => (:A :B :C :D)
;; (as-list (mcons :a (mcons :b :c)))  => (:A :B :C)


(defmethod mpush (element (mlist null))
  (mcons element (empty)))

(defmethod mpush (element (mlist mcons))
  (prog1 mlist
    (let ((new (mcons (mcar mlist) (mcdr mlist))))
      (setf (mcar mlist) element)
      (setf (mcdr mlist) new))))

(defmethod mpop ((mlist null))
  nil)

(defmethod mpop ((mlist mcons))
  (prog1 (mcar mlist)
    (let* ((next-cell (mcdr mlist))
            (next-val (mcar next-cell))
            (rest     (mcdr next-cell)))
      (if (not (emptyp next-cell))
        (setf
          (mcar mlist) next-val
          (mcdr mlist) rest)
        (setf
          (mcar mlist) nil
          (mcdr mlist) nil
          mlist nil)
        ))))

#|

(let((m  (mlist 1 2 3 4)))
  (print (eq m (mpush 0 m)))
  (describe m)
  (terpri)
  (print (mpop m))
  (describe m)
  (let ((m0 m))
    (print (mpop m))
    (print (eq m m0)))
  (print (mpop m))
  (print (mpop m))
  (print (mpop m))
  (print (mpop m))
  (describe m)
  (terpri))

(let ((m (apply #'mlist *features*)))
  (loop
    until (emptyp m)
    with z = (empty)
    with y = (length *features*)
    for i from 1 to (+ 10 y)
    do (mpush (print (mpop m)) z)
    finally (print (list i y m z))))
    


(let ((x (mcons :a :b)))
  (prog1 x
    (setf (mcar x) :c)
    (print x)
    (setf (mcdr x) :d)
    (print x)
    (setf (mcar x) nil)
    (print x)
    ))

(mapcar (lambda (x)
          (describe x)
          (mcar x)
          (mcdr x)
          (describe x))  
  (list (mcons 1 (mcons :a (mcons :b (mcons 'mcar nil))))))





(defmmclass mlink (marray)
  ((prev     :reader prev    :persistent nil)
    (next    :reader next    :persistent nil)
    (lock    :accessor lock  :persistent nil :initform (bt:make-lock))
    (segment :reader segment :persistent nil)))

|#
