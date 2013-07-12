;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :index)

(defvar *index-types* nil)

(defun all-index-types ()
  (mapcar #'class-name *index-types*))

(defmacro defindex (name supers slots &rest options)
  `(progn
     (defclass ,name ,supers ,slots ,@options)
     (pushnew (find-class ',name) *index-types* :key #'class-name)))


(defindex slot-index ()
  ((hash-table
     :initarg :ctrie
     :initarg :hash-table
     :accessor slot-index-ctrie
     :accessor slot-index-hash-table
     :documentation "The internal ctrie used to index objects.")
    (slot-name
      :initarg :slot-name
      :reader slot-index-slot-name
      :documentation "The value of the slot with name SLOT-NAME is
      used as a key to the internal ctrie.")
    (index-nil
      :initarg :index-nil
      :reader slot-index-index-nil
      :initform t
      :documentation "If T, NIL is used as a valid slot value, else
      slots with NIL value are treated as unbound slots.")))


(defmethod initialize-instance :after ((index slot-index) &key (test #'eql) slot-name
                                        slots index-nil)
  (unless slots
    (setf slots (list slot-name)))
  (unless (= (length slots) 1)
    (error "Exactly one slot name in :SLOTS initarg required to create
    a SLOT-INDEX"))
  (with-slots (hash-table slot-name) index
    (setf hash-table (make-hash-table :test test #+sbcl #+sbcl :synchronized t)
      slot-name (first slots)
      (slot-value index 'index-nil) index-nil)))

(defmethod print-object ((object slot-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "SLOT: ~S SIZE: ~D"
	    (slot-index-slot-name object)
	    (hash-table-count (slot-index-hash-table object)))))

(defmethod index-get ((index slot-index) key)
  (gethash key (slot-index-hash-table index)))

(defmethod index-remove :around ((index slot-index) object)
  (let ((slot-name (slot-index-slot-name index)))
    (if (slot-boundp object slot-name)
	(call-next-method)
	(ignore-errors ;; guard against access to unbound slots in print method
	  (warn "Ignoring request to remove object ~A with unbound slot ~A."
		object slot-name)))))

(defmethod index-remove ((index slot-index) object)
  (remhash (slot-value object (slot-index-slot-name index)) (slot-index-hash-table index)))
  
(defmethod index-keys ((index slot-index))
  (loop for key being the hash-keys of (slot-index-hash-table index)
	collect key))

(defmethod index-values ((index slot-index))
  (loop for value being the hash-values of (slot-index-hash-table index)
	collect value))

(defmethod index-mapvalues ((index slot-index) fun)
  (maphash (lambda (key val) (declare (ignore key)) (funcall fun val))
	   (slot-index-hash-table index)))

(defmethod index-clear ((index slot-index))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table :test (hash-table-test hash-table) #+sbcl #+sbcl :synchronized t))))

(defmethod index-reinitialize ((new-index slot-index)
			       (old-index slot-index))
  "Reinitialize the slot-bound index from the old index by copying the
internal hash-table if the hash-table test is the same, or by
iterating over the values of the old-table and reentering them into
the new hash-table."
  (let ((new-hash (slot-index-hash-table new-index))
	(old-hash (slot-index-hash-table old-index)))
    (if (eql (hash-table-test new-hash)
	     (hash-table-test old-hash))
	(setf (slot-index-hash-table new-index)
	      old-hash)
	(loop for key being the hash-keys of old-hash using (hash-value value)
	      do (setf (gethash key new-hash) value)))
    new-index))

(defindex unique-index (slot-index)
  ())

(defmethod index-add ((index unique-index) object)
  "Add an object using the value of the specified slot as key. When
the hash-table entry already contains a value, an error is signalled."
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let* ((key (slot-value object (slot-index-slot-name index)))
	 (hash-table (slot-index-hash-table index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (multiple-value-bind (value presentp)
	(gethash key hash-table)
      (when (and presentp
		 (not (eql value object)))
	(error (make-condition 'index-existing-error
			       :index index :key key :value value)))
      (setf (gethash key hash-table) object))))


(defindex string-unique-index (unique-index)
  ())

(defmethod initialize-instance :after ((index string-unique-index) &key (test #'equal))
  (with-slots (hash-table) index
    (setf hash-table (make-hash-table :test test #+sbcl #+sbcl :synchronized t))))

(defmethod index-add :around ((index string-unique-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let* ((key (slot-value object (slot-index-slot-name index))))
    (unless (and (not (slot-index-index-nil index))
		 (string-equal key ""))
      (call-next-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot-bound keyword index

;;; A slot-bound index storing multiple objects under one key. 

(defindex hash-index (slot-index)
  ())

(defmethod index-add ((index hash-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (if (nth-value 1 (gethash key hash-table))
        (push object (gethash key hash-table))
        (setf (gethash key hash-table) (list object)))))

(defun delete-first (obj list &key (test #'eql))
  (if (funcall test (first list) obj)
      (cdr list)
      (do ((l list (cdr l))
	   (last nil l))
	  ((null l) list)
	(when (funcall test (car l) obj)
	  (rplacd last (cdr l))
	  (return list)))))

(defmethod index-remove ((index hash-index) object)
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (let ((new-value (delete-first object (gethash key hash-table))))
      (if (null new-value)
	  (remhash key hash-table)
	  (setf (gethash key hash-table) new-value)))))

(defmethod index-values ((index hash-index))
  (loop for value being the hash-values of (slot-index-hash-table index)
	appending value))

(defmethod index-mapvalues ((index hash-index) fun)
  (maphash (lambda (key val) (declare (ignore key))
		   (dolist (obj val) (funcall fun obj)))
	   (slot-index-hash-table index)))

;;; Index objects by their class

(defindex class-index (hash-index)
  ((index-superclasses :initarg :index-superclasses :initform nil
		       :reader class-index-index-superclasses)))
  
(defmethod initialize-instance :after ((index class-index) &key index-superclasses)
  (setf (slot-value index 'index-superclasses)
	index-superclasses))

(defmethod index-add ((index class-index) object)
  (labels ((index-object (object class)
	     (let ((key (class-name class))
		   (hash-table (slot-index-hash-table index)))
               (if (nth-value 1 (gethash key hash-table))
                   (push object (gethash key hash-table))
                   (setf (gethash key hash-table) (list object))))))
    
    (if (class-index-index-superclasses index)
	(dolist (class (cons (class-of object)
			     (class-all-indexed-superclasses (class-of object))))
	  (index-object object class))
	(index-object object (class-of object)))))

(defmethod index-remove ((index class-index) object)
    (flet ((remove-object (object class)
	     (let ((key (class-name class))
		   (hash-table (slot-index-hash-table index)))
	       (let ((new-value (delete-first object (gethash key hash-table))))
		 (if (null new-value)
		     (remhash key hash-table)
		     (setf (gethash key hash-table) new-value))))))
      (if (class-index-index-superclasses index)
	  (dolist (class (cons (class-of object)
			       (class-all-indexed-superclasses (class-of object))))
	  (remove-object object class))
	  (remove-object object (class-of object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot-bound keyword list index

;;; A keyword index, where the slot-value is a list of keys.

(defindex hash-list-index (slot-index)
  ())

(defmethod index-add ((index hash-list-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((keys (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (dolist (key keys)
      (if (nth-value 1 (gethash key hash-table))
          (push object (gethash key hash-table))
          (setf (gethash key hash-table) (list object))))))

(defmethod index-remove ((index hash-list-index) object)
  (let ((keys (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index)))
    (dolist (key keys)
      (let ((new-value (delete-first object (gethash key hash-table))))
	(if (null new-value)
	    (remhash key hash-table)
	    (setf (gethash key hash-table) new-value))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Categorical Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-node (name children)
  (cons name children))

(defun node-name (node)
  (car node))

(defun (setf node-name) (new-value node)
  (setf (car node) new-value))

(defun node-children (node)
  (cdr node))

(defun node-children-empty-p (node)
  (null (node-children node)))

(defun (setf node-children) (new-value node)
  (setf (cdr node) new-value))

(defstruct category-tree
  (test #'eql)
  (root-node (make-node :root nil)))

(defun node-to-categories (node &optional parent-category)
  (let ((category (append parent-category (list (node-name node)))))
    (cons category (mapcan #'(lambda (child) (node-to-categories child category))
			   (node-children node)))))

(defun nodes-to-categories (nodes &optional parent-category)
  (mapcan #'(lambda (node) (node-to-categories node parent-category)) nodes))

(defun tree-categories (tree &optional category)
  (nodes-to-categories (node-children (category-tree-root-node tree)) category))

(defun tree-find-node (tree category)
  (unless (listp category)
    (setf category (list category)))
  (do* ((curnode (category-tree-root-node tree)
		 (find catname (node-children curnode)
		       :key #'node-name
		       :test (category-tree-test tree)))
	(curcat category (cdr curcat))
	(catname (car curcat) (car curcat)))
       ((or (null curnode)
	    (null curcat))
	curnode)))

(defun category-to-node (category)
  (if (null category)
      nil
      (let ((child (category-to-node (cdr category))))
	    (make-node (first category)
		       (when child (list child))))))

(defun tree-add-category (tree category)
  (unless (listp category)
    (setf category (list category)))
  (do* ((curnode (category-tree-root-node tree))
	(curcat category (cdr curcat))
	(catname (car curcat) (car curcat)))
       ((or (null curnode)
	    (null curcat))
	tree)
    (let ((node (find catname (node-children curnode)
		      :key #'node-name
		      :test (category-tree-test tree))))
      (if node
	  (setf curnode node)
	  (progn
	    (push (category-to-node curcat)
		  (node-children curnode))
	    (return-from tree-add-category tree))))))

(defun tree-remove-category (tree category)
  (unless (listp category)
    (setf category (list category)))
  (when category
    (let* ((parent-category (parent-category category))
	   (parent-node (tree-find-node tree parent-category)))
      (when parent-node
	(setf (node-children parent-node)
	      (cl:remove (category-name category)
		      (node-children parent-node)
		      :key #'car
		      :test (category-tree-test tree)))
	(when (node-children-empty-p parent-node)
	  (tree-remove-category tree parent-category)))))
  tree)

(defun parent-categories (category)
  (let (res)
    (dotimes (i (1-  (length category)))
      (push (butlast category (1+ i)) res))
    res))

(defun parent-category (category)
  (butlast category 1))

(defun category-name (category)
  (car (last category)))

(defun tree-find-children (tree category)
  (nodes-to-categories (node-children (tree-find-node tree category)) category))

(defun tree-find-siblings (tree category)
  (let ((len (length category)))
    (if (<= len 1)
	tree
	(let ((sib-cat (subseq category 0 (1- (length category)))))
	  (nodes-to-categories (tree-find-children tree sib-cat) sib-cat)))))

;;; category index

(defindex category-index (hash-index)
  ((tree :initform (make-category-tree)
	 :initarg :tree
	 :accessor category-index-tree))
  (:default-initargs :test #'equal))

(defmethod initialize-instance :after ((index category-index) &key (tree-test #'eql))
  (with-slots (tree) index
    (setf tree (make-category-tree :test tree-test))))

(defmethod index-get ((index category-index) category)
  (let* ((tree (category-index-tree index))
	 (hash (slot-index-hash-table index))
	 (categories (cons category
			   (tree-find-children tree category))))
    (mapcan #'(lambda (category)
		(copy-list (gethash category hash))) categories)))

(defmethod index-add ((index category-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index))
	(tree (category-index-tree index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (if (nth-value 1 (gethash key hash-table))
        (push object (gethash key hash-table))
        (progn
          (tree-add-category tree key)
          (setf (gethash key hash-table) (list object))))))

(defmethod index-remove ((index category-index) object)
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index))
	(tree (category-index-tree index)))
    (let ((new-value (delete-first object (gethash key hash-table))))
      (if (null new-value)
	  (progn
	    (tree-remove-category tree key)
	    (remhash key hash-table))
	  (setf (gethash key hash-table) new-value)))))

(defmethod index-keys ((index category-index))
  (tree-categories (category-index-tree index)))

(defmethod index-reinitialize :around ((new-index category-index)
				       (old-index category-index))
  (let* ((new-index (call-next-method))
	 (tree (category-index-tree new-index))
	 (new-hash (slot-index-hash-table new-index)))
    (loop for key being the hash-key of new-hash
	  do (tree-add-category tree key))
    new-index))

#|
;;
(defclass image ()
  ((category :index-type category-index
	     :index-reader images-with-category
	     :index-keys all-image-categories
	     :index-var *image-category-index*
	     :initarg :category
	     :reader image-category))
  (:metaclass indexed-class))

(make-instance 'image :category '(:photo :stills :nature))
(make-instance 'image :category '(:photo :naked :woman))
(make-instance 'image :category '(:painting :abstract :cubist))
;;
(defclass track ()
  ((category :index-type category-index
	     :index-initargs (:tree-test #'equal)
	     :index-reader tracks-with-category
	     :index-keys all-track-categories
	     :index-var *track-category-index*
	     :initarg :category
	     :reader track-category))
  (:metaclass indexed-class))

(make-instance 'track :category '("Rock" "New-Age" "Noise"))
(make-instance 'track :category '("Rock" "New-Age" "Techno"))
;;	     
|#
