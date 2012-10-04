;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :manardb)

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

