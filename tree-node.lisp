;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The abstraction of a 5-tuple defines the low-level interface to the storage
;; allocation strategies that are shared by the various mechanisms defining
;; Node Instance Access. The default representation is based on a vector
;; defined as a "named" structure of "type" vector, which provides a standardized
;; structure-like api with optimized accessors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Degenerate Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar %leaf%     nil)
(defvar %unbound% '%unbound%)

(defun leaf ()
  %leaf%)

(defun unbound ()
  %unbound%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (node
             (:type vector) :named
             (:conc-name %node-)
             (:predicate %node-p)
             (:constructor %allocate-node))
  (k (unbound))
  (v (unbound))
  (l (leaf))
  (r (leaf))
  (x -1))

;;(fmakunbound 'node-p)

(define-layered-function make-node (k v l r x &optional allocator &rest args))

(defun node (k v l r x)
  (make-node k v l r x))

(define-layered-method make-node :in t (k v l r x &optional (allocator '%allocate-node)
                                         &rest args)
  (apply #'make-node k v l r x allocator args))
  
(define-layered-method make-node :in transient (k v l r x &optional (allocator '%allocate-node)
                                         &rest args)
    (apply allocator :k k :v v :l l :r r :x x args))

(defun looks-nodish-to-me (thing)
  (and (typep thing '(simple-vector 6))
         (eq (elt thing 0) 'node)))

(defgeneric node-p (x))

(deftype node ()
  '(satisfies node-p))

(defmethod node-p ((thing vector))
  (typep thing `(and (simple-vector 6) (satisfies looks-bnodish-to-me))))


(defmethod pointer:deref ((thing simple-vector) &optional (type 'transient-node) &rest args)
  (declare (ignore args))
  (if (typep thing type)
    (values thing t)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Node 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mm:defmmclass persistent-node (mm:marray)
  ()
  (mm::walker mm::walk-array))

(defmethod node-p ((thing persistent-node))
  t)

(defmethod pointer:deref ((node persistent-node) &optional type &rest args)
  (declare (ignore type args))
  (let ((retrieved-content (mm:marray-to-list node)))
    (coerce retrieved-content 'vector)))

(define-layered-method make-node :in-layer persistent (k v l r x &optional
                                                        (allocator 'persistent-node)
                                                        &rest args)
  (declare (ignore args))
  (mm:make-marray 6 :initial-contents (list 'node k v l r x) :marray-class allocator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Node with Lazy Cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(deflayer cl-ctrie::persistent/cache (persistent))
;(import 'cl-ctrie::persistent/cache :tree)

(mm:defmmclass persistent/cache-node (persistent-node)
  ((%content :reader %node-content :initform nil :persistent nil))
  (mm::walker mm::walk-array))


(defmethod pointer:deref ((node persistent/cache-node) &optional type &rest args)
  (declare (ignore type args))
    (or (%node-content node)
    (let ((retrieved-content (mm:marray-to-list node)))
      (setf (slot-value node '%content) (coerce retrieved-content 'vector)))))

(define-layered-method make-node :in-layer persistent (k v l r x &optional
                                                              (allocator 'persistent/cache-node)
                                                              &rest args)
  (declare (ignore args))
  (mm:make-marray 6 :initial-contents (list 'node k v l r x) :marray-class allocator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass cvm-node (cvm:reference)
  ((%content :reader %node-content :initform nil)))

(heap::cvm-define-structure node ("CVM-NODE" . "TREE")
  k v l r x)

(defmethod node-k ((node cvm-node))
  (cvm:deref (cvm-node-k (cvm:ref node))))

(defmethod node-v ((node cvm-node))
  (cvm:deref (cvm-node-v (cvm:ref node))))

(defmethod node-l ((node cvm-node))
  (cvm:deref (cvm-node-l (cvm:ref node))))

(defmethod node-r ((node cvm-node))
  (cvm:deref (cvm-node-r (cvm:ref node))))

(defmethod node-x ((node cvm-node))
  (cvm:deref (cvm-node-x (cvm:ref node))))

(defmethod node-p ((thing cvm-node))
  t)

(defun make-cvm-node (&key k v l r x)
  (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-node) 5)))
    (prog1 (make-instance 'cvm-node :address addr :memory heap::*gc-memory*)
      (cvm-node-set-k addr (cvm:ref k))
      (cvm-node-set-v addr (cvm:ref v))
      (cvm-node-set-l addr (cvm:ref l))
      (cvm-node-set-r addr (cvm:ref r))
      (cvm-node-set-x addr (cvm:ref x)))))

(defmethod pointer:deref ((node cvm-node) &optional type &rest args)
  (declare (ignore type args))
  (or (%node-content node)
    (setf (slot-value node '%content)
      (coerce (list* 'node (loop for i from 1 below 6 collect
                             (cvm:deref (heap::cvm-structure-ref (cvm:ref node) i))))
        'vector))))

(define-layered-method make-node :in-layer cvm (k v l r x &optional (allocator 'make-cvm-node)
                                                  &rest args)
  (declare (ignore args))
  (funcall allocator :k k :v v :l l :r r :x x))

;; ;;(import 'cl-ctrie::w7)
;; (describe (cl-ctrie::with-ctrie w7
;;             (pointer:deref (make-node 0 1 2 3 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Node Access Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These constitute the interface abstacting the underlying interaction with
;; storage and allocation facilities

(defun node/k (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the K constituent of node"
  (%node-k node))

(defun node/v (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the V constituent of node"
  (%node-v node))

(defun node/l (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the L constituent of node"
  (%node-l node))

(defun node/r (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the R constituent of node"
  (%node-r node))

(defun node/x (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the X constituent of node"
  (%node-x node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compound "convenience"  Accessors built on the above Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/kv (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k and v constituent values of node"
  (list (node/k node) (node/v node)))

(defun node/lr (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing l and r constituent values of node"
  (list (node/l node) (node/r node)))

(defun node/kvlr (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k, v, l, and r constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node)))

(defun node/kvlrx (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k, v, l, r, and x constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node) (node/x node)))

(defun node/constituents (putative-node &aux (node (pointer:deref putative-node)))
   "return a list containing all constituent values of node"
  (node/kvlrx node))

(defun node/values (putative-node &aux (node (pointer:deref putative-node)))
  "return all constituents of node as multiple values"
  (apply #'values (node/constituents node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro kv ((k v) node &body body)
  "destructure tree node: key, value"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree)))
          ,@body))))

(defmacro lr ((l r) node &body body)
  "destructure tree node: left, right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
          ,@body))))

(defmacro kvlr ((k v l r) node &body body)
  "destructure tree node: key, value, left, right"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
         ,@body))))

(defmacro kvlrx ((k v l r x) node &body body)
  "destructure tree node: key, value, left, right, balance-param"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree))
              (,x  (node/x ,gtree)))
         ,@body))))

