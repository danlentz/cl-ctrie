;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  " * EXAMPLE
  ```;;; (-> (empty-map)
     ;;;   (with :a 100)
     ;;;   (with :b 200)
     ;;;   (less :a))
     ;;;
     ;;; #{| (:B 200) |}
 ```"
  (if form-supplied-p
    (if more
      `(-> (-> ,x ,form) ,@more)
      (if (listp form)
        `(,(car form) ,x ,@(cdr form))
        (list form x)))
    x))

(setf (macro-function ':->) (macro-function '->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set:typed? (thing)
  "set collection type-predicate"
  (or
    (null thing)
    (cl:typep thing 'set)))

(deftype set:type ()
  "collection of arbitrary, unique, elements in order determined by defined ordinal
   comparison relations on element types and content values"
  `(satisfies set:typed?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set:name (collection)
  (collection-name-of collection))

(defun set:add (collection &rest elements)
  (let1 s (or collection (make-collection 'set))
    (with-update-to-collection (r s)
      (loop for e in elements
        for root-node = (tree:node/add r e t)
        then (tree:node/add root-node e t)
        finally (return root-node)))))

(defun set:add* (collection elements)
  (apply #'set:add collection (ensure-list elements)))

(defgeneric set:min (collection)
  (:documentation "return the smallest element present in the collection")
  (:method ((collection null)) nil)
  (:method ((collection set))
    (tree:node/k (tree:node/least (root-node-of collection)))))

(defun set::min-node (collection)
  (when collection
    (tree:node/least (root-node-of collection))))

(defgeneric set:max (collection)
  (:documentation "return the greatest element present in the collection")
  (:method ((collection null)) nil)
  (:method ((collection set))
    (tree:node/k (tree:node/greatest (root-node-of collection)))))

(defun set::max-node (collection)
  (when collection
    (tree:node/greatest (root-node-of collection))))

(defgeneric set:remove-min (collection)
  (:documentation  "return a collection with the smallest element removed")
  (:method ((collection null)) nil)
  (:method ((collection set))
    (with-update-to-collection (r collection)
      (tree:node/remove-least r))))

(defgeneric set:remove-max (collection)
  (:documentation  "return a collection with the greatest element removed")
  (:method ((collection null)) nil)
  (:method ((collection set))
    (with-update-to-collection (r collection)
      (tree:node/remove-greatest r))))

(defgeneric set:split (collection x)
  (:documentation "returns a triple (l present r) where:
     l       - is the set of elements of s that are < x
     r       - is the set of elements of s that are > x
     present - is false if s contains no element equal to x
               or true if s contains an element equal to x")
  (:method ((collection null) x) (list nil nil nil))
  (:method ((collection set) x)
    (with-collection (collection)      
      (destructuring-bind (l p r) (tree:node/split (root-node-of collection) x)
        (let ((l-set (when l (make-collection 'set)))
               (p-val (cdr p))
               (r-set (when r (make-collection 'set))))
          (when l (setf (root-node-of l-set) (if (tree::node-p l) l (set:add (set:empty) l))))
          (when r (setf (root-node-of r-set) (if (tree::node-p r) l (set:add (set:empty) r))))
          (list l-set p-val r-set))))))

(defun set:empty ()
  (make-collection 'set))
 
(defun set:empty? (collection)
  "return true if set contains no elements, otherwise false"
  (or (null collection) (null (root-node-of collection))))

(defun set:member? (collection x)
  "return true if set contains element x"
  (with-collection (collection)
    (not (null (tree:node/find x (root-node-of collection))))))

(defun set:singleton (x)
  "create set containing only the element x"
  (set:add (set:empty) x))

(defun set:remove (collection x)
  "return a collection the same as argument with the element 'x' removed if present"
  (with-update-to-collection (r collection)
    (tree:node/remove r x)))

(defun set:union (s1 &rest more-sets)
  "return a collection containing all the elements (without duplicates) of s1 and s2"
  (with-update-to-collection (r s1)
    (loop for sn in (mapcar #'root-node-of more-sets)
      for new-root = (tree:node/union r sn)
      then (tree:node/union new-root sn)
      finally (return new-root))))

(defun set:diff (s1 s2)
  (with-collection (s1)
    (let ((r1 (root-node-of s1))
           (r2 (root-node-of s2)))
      (with-update-to-collection (r (make-collection 'set))
        (tree:node/difference r1 r2)))))

(defun set:intersect (s1 &rest more-sets)
  "return a collection containing all elements that are present in both s1 and s2"
  (with-update-to-collection (r s1)
    (loop for sn in (mapcar #'root-node-of more-sets)
      for new-root = (tree:node/intersection r sn)
      then (tree:node/intersection new-root sn)
      finally (return new-root))))

(defun set:compare (s1 s2 &optional (cmp #'ord:compare)) 
  "return 3-way ordinal comparison of sets s1 and s2 with the following return-value semantics:
    0  -> set0 is EQAL-TO      set1
   -1  -> set0 is LESS-THAN    set1
    1  -> set0 is GREATER-THAN set1"
  (with-collection (s1)
    (let* ((s1 (root-node-of s1))
            (s2 (root-node-of s2))
            (e1 (tree:node/cons-enum s1 nil))
            (e2 (tree:node/cons-enum s2 nil)))
      (tagbody again
        (return-from set:compare
          (cond
            ((and (null e1) (null e2)) 0)
            ((null e1)                -1)
            ((null e2)                 1)
            (t                         (destructuring-bind (v1 r1 ee1) e1
                                         (destructuring-bind (v2 r2 ee2) e2
                                           (let ((c (funcall cmp v1 v2)))
                                             (if (zerop c)
                                               (progn
                                                 (setf
                                                   e1 (tree:node/cons-enum r1 ee1)
                                                   e2 (tree:node/cons-enum r2 ee2))
                                                 (go again))
                                               c)))))))))))

(define-layered-method ord:compare :in t ((s1 set) (s2 set))
  (with-collection (s1)
    (set:compare s1 s2)))
                           
(defun set:equal? (s1 s2)
  "return true if both hold:  s1 is a subset of s2, and s2 is a subset of s1"
  (zerop (ord:compare s1 s2)))

(defun set:subset? (s1 s2)
  "return true if all elements of s2 are present in s1"
  (with-collection (s1)
    (let* ((r1 (root-node-of s1))
            (r2 (root-node-of s2)))
      (tree:node/subset? r2 r1))))

(defun applied-to-keys-and-accum (fn)
  (lambda (k v a)
    (declare (ignore v))
    (funcall fn k a)))

(defun set:foldl (s fn base)
  (with-collection (s)
    (tree:node/inorder-fold (applied-to-keys-and-accum fn) base (root-node-of s))))

(defun set:foldr (s fn base)
  (with-collection (s)
    (tree:node/reverse-fold (applied-to-keys-and-accum fn) base (root-node-of s))))

(defun applied-to-keys (fn)
  (lambda (k v a)
    (declare (ignore v a))
    (funcall fn k)))

(defun set:each (s fn)
  "funcall fn on each element of set s"
  (with-collection (s)
    (tree:node/reverse-fold (applied-to-keys fn) nil (root-node-of s)))
  (values))
      
(defmacro/once set:do ((element &once set) &body body)
  "Iterate over elements of SET in the manner of the dolist and dotimes macros"
  `(set:each ,set #'(lambda (,element) ,@body)))

(defun set:for-all (s pred fn)
  "funcall fn on all elements of set s satisfying pred"
  (set:do (e s)
    (when (funcall pred e)
      (funcall fn e))))

(defun set:some? (s pred)
  "return true if any element of s satisfies pred"
  (set:do (e s)
    (when (funcall pred e)
      (return-from set:some? t))))

;; todo: this is not atomic
(defun set:filter (s pred)
  "return a new set containing all elements of s which satisfy pred"
  (prog1 s
    (set:do (e s)
      (unless (funcall pred e) (set:remove s e)))))

(defun set:partition (s pred)
  "return a list containing two new sets: the first containing those elements
   of s which satisfy pred, and the second containing those which do not"
  (with-collection (s)
    (aprog1 (list (set:empty) (set:empty))
      (set:do (e s)
        (if (funcall pred e)
          (set:add (first  it) e)
          (set:add (second it) e))))))

(defun set:size (s)
  "return the number of elements contained in set s"
  (with-collection (s)
    (tree:node/size (root-node-of s))))

(defun set:enum (s)
  "return a list containing all elements of s"
  (set:foldl s #'cons nil))

(defun set:dup (s &optional name)
  "return a new set which is set:equal the original s"
  (let* ((s0 (make-instance 'set
               :context (copy-list (collection-context-of s))
               :object (make-instance (class-of (collection-object-of s)))))
          (o0 (collection-object-of s0))) 
    (prog1 s0
      (setf
        (root-node-of          o0) (root-node-of s)
        (context-of            o0) (collection-context-of s0)
        (type-name-of          o0) (type-name-of (collection-object-of s))
        (symbol-name-of        o0) (byte-vector-to-hex-string
                                     (create-unique-id-byte-vector))
        (package-name-of       o0)  nil
        (creation-timestamp-of o0) (get-universal-time))
      (recompute-slot          o0  'symbol-for-binding))))
    
    
#|



(defun set:make (&optional (from (set:empty)))
  "end-user api for construction of a new set optionally initialized to contain
   elements derived from various types of source data"
  (etypecase from
    (null     (set:empty))
    (var      (set:make (value from)))
    (ord:proper-list  (let (set)
                        (dolist (elem from)
                          (if (or (atom elem) (ord:proper-list-p elem))
                            (setq set (set:add elem set))
                            (error "Cannot add ~S. Sets admit Only atom or proper-lists" from)))
                        set))
    (cons     (error "only proper-lists may be members of a set"))
    (seq:type (set:make (seq:list from)))
    (map:type (error "sets cannot be created from  maps"))
    (set:type (set:dup from))
    (string   (set:singleton from))
    (sequence (set:make (cl:coerce from 'cl:list)))
    (atom     (set:singleton from))))

|#
