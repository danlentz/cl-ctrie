;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Common Interface Layer, depends on only the public node-instance-access
;; api: node/k, node/v, node/l, node/r, node/x, node/kv, node/lr, node/kvlr,
;; node/kvlrx, node/constituents, node/values, kv, lr, kvlr, kvlrx.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer balanced)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Degenerate Instance API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty ()
  "a value which satisfies {defun tree::empty?}"
  (leaf))

(defun node/empty? (node)
  ;; this probably doesnt need to exist
  (null node))

(define-layered-function empty? (thing)
  (:documentation "returns t if THING contains no associations")
  (:method  ((thing null)) t)
  (:method  ((thing t)) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Primatives 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/call (node fn)
  "apply FN to the destructured constituent values of
   NODE. FN is a function taking four parameters: K, V, L, and R,
   where K is the key of NODE, V is the value of NODE, L is the left
   subtree of NODE, and R is the right subtree of NODE."
    (apply fn (node/kvlr node)))


(define-layered-function node/size (node)
  (:documentation "returns the number of associations in tree rooted at node"))

(define-layered-method node/size :in-layer t ((node null))
  0)

(define-layered-method node/size :in-layer t (node) 
  (+ 1 (node/size (node/l node)) (node/size (node/r node))))


(defun node/weight (node)
  "returns node weight as appropriate for rotation
  calculations using the 'revised non-variant algorithm' for weight
  balanced binary tree. For non-weight-balanced trees, this is not a
  useful statistic"
  (+ (node/size node) 1))


(define-layered-function node/height (node)
  (:documentation "The distance from NODE to its furthest leaf subnode"))

(define-layered-method node/height :in-layer t ((node null))
  0)

(define-layered-method node/height :in-layer t ((node t))
  (+ 1 (max (node/height (node/l node)) (node/height (node/r node)))))


(defun node/cons-enum (node &optional enum)
  "efficient mechanism to accomplish partial enumeration of
   tree-structure into a consp representation without incurring the
   overhead of operating over the entire tree.  Used internally for
   implementation of higher-level collection api routines"
  (cond
    ((empty? node) enum)
    (t
      (kvlr (k v l r) node
        (node/cons-enum l (list (cons k v) r enum))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction and Rotation Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-layered-function node/create (k v l r)
  (:documentation "Join left and right subtrees at root k/v.  Assumes
  all keys in l < k < all keys in r."))


(defun node/singleton (k &optional (v (unbound)))
  "create and return a newly allocated weight balanced
   tree containing a single association, that value V with key K."
  (node/create k v (leaf) (leaf)))


(define-layered-function node/join (k v l r)
  (:documentation "Join left and right subtrees at root k/v,
  performing a rotation operation to balance the resulting tree,
  if needed.  Assumes all keys in l < k < all keys in r, and the
  relative  balance of the left and right subtrees is such that no
  more than one rotation operation will be required to restore balance
  for the two provided subtrees, l and r"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordering Relation Independent Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/least (node)
  "Return the node containing the minimum key of the tree rooted at NODE"
  (cond
    ((empty? node)          (error "least: empty tree"))
    ((empty? (node/l node)) (return-from node/least  node))
    (t
      (node/least (node/l node)))))


(defun node/greatest (node)
  "Return the node containing the minimum key of the tree rooted at NODE"
  (cond
    ((empty? node)          (error "greatest: empty tree"))
    ((empty? (node/r node)) (return-from node/greatest node))
    (t
      (node/greatest (node/r node)))))


(defun node/remove-least (node)
  "Return a tree the same as the one rooted at NODE,
   with the node containing the minimum key removed. See {defun
   tree::node/least}"
  (cond
    ((empty? node)           (error "remove-least: empty tree"))
    ((empty? (node/l node))  (node/r node))
    (t
      (node/join (node/k node) (node/v node)
        (node/remove-least (node/l node)) (node/r node)))))


(defun node/remove-greatest (node)
  "Return a tree the same as the one rooted at NODE,
   with the node containing the maximum key removed. See {defun
   tree::node/greatest}"
  (cond
    ((empty? node)           (error "remove-greatest: empty tree"))
    ((empty? (node/r node))  (node/l node))
    (t
      (node/join (node/k node) (node/v node)
        (node/l node) (node/remove-greatest (node/r node))))))


(defun node/concat2 (node1 node2)
  "Join two trees, the left rooted at NODE1, and the right at NODE2,
   performing a single balancing operation on the resulting tree, if
   needed. Assumes all keys in NODE1 are smaller than all keys in
   NODE2, and the relative balance of NODE1 and NODE2 is such that no
   more than one rotation operation will be required to balance the
   resulting tree"
  (cond
    ((empty? node1) node2)
    ((empty? node2) node1)
    (t
      (kv (k v) (node/least node2)
        (node/join k v node1 (node/remove-least node2))))))


(defun node/inorder-fold (fn base node)
  (labels ((the-fn (k &optional v a)
             (funcall (alexandria:ensure-function fn) k v a))             
            (fold (base node)
              (if (empty? node) base
                (kvlr (k v l r)  node
                  (fold (the-fn k v (fold base r)) l)))))     
    (fold base node)))

(defun node/reverse-fold (fn base node)
  (labels ((the-fn (k &optional v a)
             (funcall (alexandria:ensure-function fn) k v a))             
            (fold (base node)
              (if (empty? node) base
                (kvlr (k v l r)  node
                  (fold (the-fn k v (fold base l)) r)))))     
    (fold base node)))


(defun node/iter (node fn)
  "For the side-effect, apply FN to each node of the tree rooted at NODE"
  (if (empty? node) nil
    (kvlr (k v l r) node
      (node/iter l fn)
      (funcall (alexandria:ensure-function fn) k v)
      (node/iter r fn))))



(defun node/for-all (node predicate fn)
  "For the side-effect, apply FN to each node of the tree rooted at
  NODE for which the predicate function returns a non-nil value"
  (if (empty? node) nil
    (kvlr (k v l r) node
      (node/for-all l predicate fn)
      (when (funcall predicate k)
        (funcall (alexandria:ensure-function fn) k v))
      (node/for-all r predicate fn))))



(define-layered-function node/at-index (node index &optional caller)
  (:documentation ""))


(define-layered-function node/rank (k node &optional rank)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordering Relation Dependent Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/find (k node)
  "find k (if exists) in only d comparisons (d is depth of tree)
   rather than the traditional compare/low compare/high which takes on
   avg (* 1.5 (- d 1))"
  (labels ((recur (this best)
             (cond
               ((empty? this)                    (return-from recur best))
               ((ORD:|COMPARE<| k (node/k this)) (recur (node/l this) best))
               (t                                (recur (node/r this) this)))))
    (let ((best (recur node nil)))
      (when best
        (unless (ORD:|COMPARE<| (node/k best) k)
          (return-from node/find best))))))


(defun node/add (node k &optional (v (unbound)))
  (if (empty? node)
    (node/singleton k v)
    (node/call node
      (lambda (key val l r)
        (cond
          ((ORD:|COMPARE<| k key) (node/join key val (node/add l k v) r))
          ((ORD:|COMPARE<| key k) (node/join key val l (node/add r k v)))
          (t
            (node/create key v l r)))))))

   
(defun node/remove (node k)
  (if (empty? node)
    (leaf)
    (node/call node
      (lambda (key val l r)
        (cond
          ((ORD:|COMPARE<| k key) (node/join key val (node/remove l k) r))
          ((ORD:|COMPARE<| key k) (node/join key val l (node/remove k r)))
          (t
            (node/concat2 l r)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full Concatentate Operations (Regardless of Weight)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-layered-function node/concat3 (k v l r)
  (:documentation ""))


(define-layered-function node/concat (node1 node2)
  (:documentation ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
(defun node/split-lesser (node k)
  (cond
    ((empty? node)                    (empty))
    ((ORD:|COMPARE<| k (node/k node)) (node/split-lesser (node/l node) k))    
    ((ORD:|COMPARE>| k (node/k node)) (node/concat3
                                        (node/k node)
                                        (node/v node)
                                        (node/l node)
                                        (node/split-lesser (node/r node) k)))
    (t (node/l node))))

  
(defun node/split-greater (node k)
  (cond
    ((empty? node)            (empty))
    ((ORD:|COMPARE<| (node/k node) k) (node/split-greater (node/r node) k))    
    ((ORD:|COMPARE>| (node/k node) k) (node/concat3
                                        (node/k node)
                                        (node/v node)
                                        (node/split-greater (node/l node) k)
                                        (node/r node)))
    (t (node/r node))))

  
(defun node/split (node k)
  "returns a triple (l present r) where: l is the set of elements of
  s that are < k, r is the set of elements of s that are > k, present
  is false if s contains no element equal to k, or
  (k . v) if s contains an element with key equal to k"
  (cond
    ((empty? node) (list nil nil nil))
    (t             (kvlr (ak v l r) node
                     (let ((c (ord:compare k ak)))
                       (cond
                         ((zerop  c) (return-from node/split 
                                       (list l (cons k v) r)))
                         ((minusp c) (destructuring-bind (ll pres rl) (node/split l k)
                                       (list ll pres (node/concat3 ak v rl r))))
                         ((plusp  c) (destructuring-bind (lr pres rr) (node/split r k)
                                       (list (node/concat3 ak v l lr) pres rr)))))))))


(defun node/union (node1 node2)
  (cond
    ((empty? node1) node2)
    ((empty? node2) node1)
    (t
      (kvlr (ak av l r) node2
        (let ((l1 (node/split-lesser node1 ak))
               (r1 (node/split-greater node1 ak)))
          (node/concat3 ak av
            (node/union l1 l)
            (node/union r1 r)))))))


(deftype merge-direction ()
  ""
  `(member :left :right))


(defun node/union-merge (node1 node2 &optional merge)
  (let ((do-merge-values
          (case merge
            (:left  #'first)
            (:right #'second)
            (t      (error "invalid merge type ~S, must be :left or :right" merge)))))
    (cond
      ((empty? node1) node2)
      ((empty? node2) node1)
      (t
        (kvlr (ak av l r) node2
          (let* ((node1 (node/find ak node1))
                  (l1  (node/split-lesser  node1 ak))
                  (r1  (node/split-greater node1 ak))
                  (val (if node1
                         (funcall do-merge-values (list (node/v node1) av))
                         av)))
            (node/concat3 ak val
              (node/union-merge l1 l merge)
              (node/union-merge r1 r merge))))))))


  
(defun node/intersection (node1 node2)
  (cond
    ((empty? node1) (empty))
    ((empty? node2) (empty))
    (t
      (kvlr (ak av l r) node2
        (let ((l1 (node/split-lesser node1 ak))
               (r1 (node/split-greater node1 ak)))
          (if (node/find ak node1)
            (node/concat3 ak av
              (node/intersection l1 l)
              (node/intersection r1 r))
            (node/concat
              (node/intersection l1 l)
              (node/intersection r1 r))))))))


  
(defun node/difference (node1 node2)
  (cond
    ((empty? node1) (empty))
    ((empty? node2) node1)
    (t
      (kvlr (ak av l r) node2
        (declare (ignore av))
        (let ((l1 (node/split-lesser node1 ak))
               (r1 (node/split-greater node1 ak)))
          (node/concat
            (node/difference l1 l)
            (node/difference r1 r)))))))




(define-layered-function node/subset? (node1 node2))
   
(define-layered-function node/from (thing)  )

(define-layered-function node/member? (node key)  )

(define-layered-function node/contains? (key node)  )





