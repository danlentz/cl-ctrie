;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :cl-mmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standaed (Compacting) GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gc-compact (offsets-table)
  (loop for mtagmap across *mtagmaps*
    for offsets across offsets-table
    for tag from 0
    when offsets
    do (let ((elem-len (mtagmap-elem-len mtagmap)) (cur-offset (mtagmap-first-index mtagmap)))
         (loop for new-offset across offsets
           for old-offset from (mtagmap-first-index mtagmap) by elem-len
           do (unless (zerop new-offset)
                (assert (= cur-offset new-offset))
                (assert (>= old-offset new-offset))
                (osicat-posix:memmove (mpointer tag new-offset)
                  (mpointer tag old-offset) elem-len)
                (setf cur-offset (+ new-offset elem-len))))
         (setf (mtagmap-next mtagmap) cur-offset))))


(defun gc-calc-new-offsets (mtagmap table)
  (when table
    (let ((offsets (make-array (length table) :element-type 'mindex :initial-element 0)) 
	  (next (mtagmap-first-index mtagmap)) 
	  (elem-len (mtagmap-elem-len mtagmap)))
      (loop for refs across table for i from 0
	    do (when refs
		 (setf (aref offsets i) next)
		 (incf next elem-len)))
      offsets)))


(defun gc-rewrite-pointers-and-compact (refs-table)
  (clear-caches)
  (let ((offsets-table (map 'vector 'gc-calc-new-offsets *mtagmaps* refs-table)))
    (loop for mtagmap across *mtagmaps*
	  for tag from 0
	  for elem-len = (when mtagmap (mtagmap-elem-len mtagmap))
	  for table across refs-table 
	  for offsets across offsets-table
	  when table do (mtagmap-check mtagmap)
	  (loop for pos from 0
		for refs across table
		for old-offset from (mtagmap-first-index mtagmap) by elem-len
		for old-mptr = (make-mptr tag old-offset)
		for new-offset across offsets
		for new-mptr = (make-mptr tag new-offset)
		when refs do 
		(labels ((up (ref)
			      (declare (type mptr ref))
			      (unless (zerop ref) 
				(assert (= (d (mptr-pointer ref) 0 mptr) old-mptr))
				(unless (= old-mptr new-mptr)
				  (setf (d (mptr-pointer ref) 0 mptr) new-mptr)))))
                                ;; only write if necessary so pages not pointlessly dirtied
		      (typecase refs
			(array	 (loop for r across refs do (up r)))
			(t	 (up refs))))))
    (gc-compact offsets-table)))


(defun gc (root-objects-sequence &key verbose (collect-and-compact t))
  "Do a full and precise garbage collection over all objects in the
  memory mapped system.  If COLLECT-AND-COMPACT is true, then unused
  objeccts are removed. Uses at least two pointers of Lisp memory per
  object and more if objects are densely referenced. See REWRITE-GC for
  a sloppier alternative that does not need so much memory."
  (let ((refs-table (map 'vector (lambda (m) 
				   (unless (or (not m) (mtagmap-closed-p m))
                                     (make-array (mtagmap-count m) :initial-element nil))) 
                      *mtagmaps*))
         (root-objects-sequence (map '(vector mptr) #'force-mptr root-objects-sequence)))
    (macrolet ((r (mptr)
		 (check-type mptr symbol)
		 `(aref (aref refs-table (mptr-tag ,mptr))
                    (mtagmap-elem-pos (mtagmap (mptr-tag ,mptr)) (mptr-index ,mptr)))))
      (labels ((add-ref (mptr referrer)
                 (symbol-macrolet ((ref (r mptr)))
                   (let ((rref ref))
                     (typecase rref
                       (array      (when (zerop referrer) (return-from add-ref))
                                   (vector-push-extend referrer rref))
                       (null       (setf ref referrer))
                       (t          (cond
                                     ((zerop rref) (setf ref referrer))
                                     ((= rref referrer))
                                     (t  (setf ref (make-array 2 :adjustable t :fill-pointer 2
                                                     :initial-contents (list rref referrer)
                                                     :element-type 'mptr)))))))))
                (walk-ref (mptr referrer len)
                  (unless (zerop mptr)
                    (let ((first-time (not (r mptr))))
		      (add-ref mptr referrer)
		      (when first-time
			(let ((walker (mtagmap-walker (mtagmap (mptr-tag mptr)))))
			  (when walker (funcall walker mptr #'walk-ref))))
		      (unless (= 1 len)
			(walk-ref
                          (+ mptr (ash (mtagmap-elem-len (mtagmap (mptr-tag mptr))) +mtag-bits+))
                          0 (1- len)))))))
        (declare (dynamic-extent #'walk-ref #'add-ref))
        (iter (for o in-vector root-objects-sequence) (walk-ref o 0 1))
        (when verbose (loop for m across *mtagmaps*
                        for table across refs-table
                        do (when table (format t "~A total ~D used ~D~&" 
                                         (mtagmap-class m) (mtagmap-count m)
                                         (count-if-not #'not table)))))
        (when collect-and-compact
          (gc-rewrite-pointers-and-compact refs-table)
          (shrink-all-mmaps))
        (values)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewrite-GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rewrite-gc-walk (root-objects-sequence shared-tables new-mtagmaps &key progress)
  (let* ((print-step
	  (when progress
	    (ceiling (length root-objects-sequence) (if (numberp progress) progress 10))))
	 (print-next print-step)
	 (start-time (get-internal-real-time))
	 (root-objects-sequence (map '(vector mptr) #'force-mptr root-objects-sequence )))    
    (iter 
      (for o in-vector root-objects-sequence)
      (for count from 0)
      (when (and print-next (= count print-next))
	(let ((now (get-internal-real-time)))
	  (unless (= now start-time)
	    (format t "~&Added ~D objects; ~$ object/s~%" count
		    (/ (* count internal-time-units-per-second) (- now start-time)))))
	(incf print-next print-step))	
      (rewrite-gc-copy-one-root o shared-tables new-mtagmaps))))


(defun rewrite-gc-copy-one-root (mptr shared-tables new-mtagmaps)
  (let ((visited (map 'vector (lambda (x table) 
                                (or table (when x (make-hash-table :test 'eql)))) 
	      new-mtagmaps shared-tables)))
    (declare (dynamic-extent visited))
    (macrolet ((vref (mptr)
		 `(gethash (mptr-index ,mptr) 
			   (aref (the simple-vector visited) (mptr-tag ,mptr)))))    
      (labels ((allocate-ref (mptr num)
		 (declare (type mptr mptr)
			  (type mindex num))
		 (let* ((tag (mptr-tag mptr)) 
			(mtagmap (aref new-mtagmaps tag))
			(len (mtagmap-elem-len mtagmap))
			(total-len (* num len))
			(new-index (mtagmap-alloc mtagmap total-len))
			(new-mptr (make-mptr tag new-index))) 
		   (osicat-posix:memcpy 
		    (cffi:inc-pointer (mtagmap-ptr mtagmap) new-index)
		    (mptr-pointer mptr)
		    total-len)
		   new-mptr))
	       (walk-ref (mptr referrer num)
		 (declare (ignore referrer))
		 (cond ((zerop mptr) 0)
		       ((vref mptr))
		       (t
			(let* ((new-mptr (allocate-ref mptr num))
			       (mtagmap (mtagmap (mptr-tag mptr)))
			       (walker (mtagmap-walker mtagmap)))
			  (setf (vref mptr) new-mptr)
			  (when walker
			    (let ((old-index (mptr-index mptr)))
			      (labels ((reset-ref (child-mptr referrer num)
					 (declare (type mptr child-mptr referrer)
						  (type mindex num))
					 (let ((offset (- (mptr-index referrer) old-index))
					       (new-child-mptr (walk-ref child-mptr referrer num)))
					   (setf 
					    (dw 
					     (cffi:inc-pointer 
					      (mtagmap-ptr 
					       (aref new-mtagmaps (mptr-tag new-mptr)))
					      (+ offset (mptr-index new-mptr))))
					    new-child-mptr))))
				(declare (dynamic-extent #'reset-ref))
				(funcall walker mptr #'reset-ref)
				(unless (= 1 num)
				  (let* ((elem-len (mtagmap-elem-len mtagmap))
					 (step (ash elem-len +mtag-bits+)))
				    (loop for i from 1 below num do
					  (incf mptr step)
					  (incf new-mptr step)
					  (incf old-index elem-len)
					  (funcall walker mptr #'reset-ref))
				    (decf new-mptr (* step (1- num))))))))
                          new-mptr)))))
	(declare (dynamic-extent #'walk-ref))
	(walk-ref mptr 0 1)))))


(defun rewrite-gc-cleanup (new-mtagmaps new-files)
  (loop for new across new-mtagmaps
    for old across *mtagmaps*
    for new-file in new-files
    do (when new
         (mtagmap-close new)
         (let ((old-file (mtagmap-default-filename old)))
	   (mtagmap-close old)
	   (osicat-posix:rename new-file old-file)
	   (mtagmap-open old)))))


(defun rewrite-gc (root-objects-sequence &key (base-shared-classes '(mm-symbol))
                    shared-classes verbose progress)
  "An alternative, sloppier GC algorithm with a space complexity that
  is not proportional to the size of the datastore.Creates a new
  datastore by copying each element of ROOT-OBJECTS-SEQUENCE as if it
  were entirely self contained except for any shared objects in
  SHARED-CLASSES. Cannot handle pointers to the inside of arrays at all;
  they will be recreated pointing to fresh objects. Note that arrays
  pointing to complex objects (or any user defined classes) are stored
  as arrays of mmptrs, with each mptr pointing to the actual object; it
  is fine to have pointers to these objects, because the actual objects
  are notstored in the array."
  (check-mmap-truncate-okay)
  (let* ((new-mtagmaps
           (map '(vector (or null mtagmap)) 
             (lambda (m)
               (when (and m (not (mtagmap-closed-p m)))
                 (let ((m (copy-structure m)))
                   (mtagmap-detach m)
                   m)))
             *mtagmaps*))
          (shared-tables
            (make-array (length *mtagmaps*) :initial-element nil))
          (new-files 
            (loop for m across new-mtagmaps
              collect 
              (when m 
                (make-pathname :type "rewrite" :defaults
                  (mm-metaclass-pathname (mtagmap-class m)))))))
    (flet ((add-shared (seq)
	     (map nil (lambda (x)
			(setf (aref shared-tables (force-tag x))
                          (make-hash-table :test 'eql))) seq)))
      (add-shared base-shared-classes)
      (add-shared shared-classes))    
    (unwind-protect
      (progn
        (loop for m across new-mtagmaps 
          for f in new-files
          do (when m
               (ignore-errors (delete-file f))
               (mtagmap-open m :file f :finalize nil)))
        (rewrite-gc-walk root-objects-sequence shared-tables new-mtagmaps :progress progress)
        (when verbose
          (loop for new across new-mtagmaps
            for old across *mtagmaps*
            when new
            do (let ((cold (mtagmap-count old))
                      (cnew (mtagmap-count new)))
                 (cond ((zerop cold)
                         (assert (zerop cnew)))
                   (t
                     (format t "~&~A before ~D after ~D; change ~D~%" 
                       (mtagmap-class old) cold cnew (- cnew cold)))))))
        (rewrite-gc-cleanup new-mtagmaps new-files))
      (loop for m across new-mtagmaps 
        for f in new-files
        do (when m
             (mtagmap-close m)
             (ignore-errors (delete-file f)))))))

