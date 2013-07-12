;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight Balanced Layer (Hirai-Yamamoto Tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "This is an implementation of a weight-balanced binary tree data
;;  structure based on the following references:
;; --  Adams (1992) 'Implementing Sets Efficiently in a Functional Language'
;;    Technical Report CSTR 92-10, University of Southampton.
;; --  Hirai and Yamamoto (2011) 'Balancing Weight-Balanced Trees'
;;    Journal of Functional Programming / 21 (3): 287-307.
;; --  MIT Scheme weight balanced tree as reimplemented by Yoichi Hirai and Kazuhiko Yamamoto
;;    using the revised non-variant algorithm recommended integer balance parameters from
;;    (Hirai/Yamomoto 2011).  {https://github.com/kazu-yamamoto/wttree}
;; -- Bounded Balance
;; -- Making Data Structures Persistent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar +delta+ 3
  "The primary balancing rotation parameter that is used for the determination
   whether two subtrees of a node are in balance or require adjustment by means of a
   rotation operation.  The specific rotation to be performed is determined by
   {defvar wb::+gamma+}")


(defvar +gamma+ 2
  "The secondary balancing rotation parameter that is used for the determination
   whether a single or double rotation operation should occur, once it has been
   decided based on {defvar wb::+delta+} that a rotation is indeed required.")


;; (deflayer weight-balanced (balanced)
;;   ((delta :initarg :delta :initform +delta+ :reader weight-balanced-delta)
;;     (gamma :initarg :gamma :initform +gamma+ :reader weight-balanced-gamma))
;;   (:documentation ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight Balanced Constituent Specializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro kvlrs ((k v l r s) node &body body)
  "destructure node node: left value right size"
  (let ((gnode (gensym (symbol-name :node-))))
    `(let ((,gnode ,node))
       (let ((,k  (node/k ,gnode))
              (,v  (node/v ,gnode))
              (,l  (node/l ,gnode))
              (,r  (node/r ,gnode))
              (,s  (node/x ,gnode)))
         ,@body))))

(define-layered-function node/s (node))
(define-layered-method   node/s :in-layer weight-balanced (node)
  (node/x node))

(define-layered-function node/kvlrs (node))
(define-layered-method   node/kvlrs :in-layer weight-balanced (node)
  (kvlrs (k v l r s) node
    (list k v l r s)))

(define-layered-method node/size :in-layer weight-balanced (node)
  "returns the number of associations in tree rooted at node, with constant time
   complexity"
  (cond ((empty? node) 0)
    (t
      (node/x node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight Balanced Constructor and Rotation Specializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-method node/create :in-layer weight-balanced (k v l r)
  "Join left and right subtrees at root k/v.  Assumes all keys in l < k < all keys in r.
  Also requires that the weight of each subtree is less than +delta+
  times the weight of the other."
  (node k v l r (+ 1 (node/size l) (node/size r))))


(defun single-l (a.k a.v x r)
  "Perform a single left rotation, moving Y, the left subtree of the right subtree of A,
  into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  left subtree of node B is less than the weight of the right subtree of node B
  multiplied by rotation coefficient {defvar wb::+gamma+} 
  ;;; .
  ;;;              ,---,                                  ,---,
  ;;;              | A |                                  | B |
  ;;;              :---:                                  :---:
  ;;;             :     :                                :     :
  ;;;        ,---:       :---,                      ,---:       :---, 
  ;;;        | X |       | B |           =>         | A |       | Z | 
  ;;;        '---'       :---:                      :---:       '---'
  ;;;               ,---:     :---,            ,---:     :---,
  ;;;               | Y |     | Z |            | X |     | Y |
  ;;;               '---'     '---'            '---'     '---'
  ;;; ."
  (node/call R
    (lambda (b.k b.v y z)
      (node/create b.k b.v (node/create a.k a.v x y) z))))



(defun double-l (a.k a.v x r)
  "Perform a double left rotation, moving Y1, the left subtree of the left subtree
  of the right subtree of A, into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  left subtree of node B is greater than or equal to the weight of the right subtree 
  of node B multiplied by rotation coefficient {defvar wb::+gamma+} 
  ;;; .
  ;;;              ,---,                                    ,---,             
  ;;;              | A |                                    | B |             
  ;;;           ___:---:___                             ____:---:____          
  ;;;      ,---:           :---,                   ,---:             :---,       
  ;;;      | X |           | C |                   | A |             | C |       
  ;;;      '---'           :---:         =>        :---:             :---:
  ;;;                 ,---:     :---,         ,---:     :---,   ,---:     :---,  
  ;;;                 | B |     | Z |         | X |     | y1|   | y2|     | Z |  
  ;;;                 :---:     '---'         '---'     '---'   '---'     '---'
  ;;;            ,---:     :---,   
  ;;;            | y1|     | y2|  
  ;;;            '---'     '---'
  ;;; ."
  (node/call R
    (lambda (c.k c.v b z)
      (node/call b
        (lambda (b.k b.v y1 y2)
          (node/create b.k b.v
            (node/create a.k a.v x y1)
            (node/create c.k c.v y2 z)))))))



(defun single-r (b.k b.v l z)
  "Perform a single right rotation, moving Y, the right subtree of the left subtree of B,
  into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node B is less then the weight of the left subtree of node B
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  right subtree of node A is less than the weight of the left subtree of node A
  multiplied by rotation coefficient {defvar wb::+gamma+} 
  ;;; .
  ;;;              ,---,                                  ,---,             
  ;;;              | B |                                  | A |             
  ;;;              :---:                                  :---:             
  ;;;             :     :                                :     :            
  ;;;        ,---:       :---,                      ,---:       :---,       
  ;;;        | A |       | Z |          =>          | X |       | B |       
  ;;;        :---:       '---'                      '---'       :---:       
  ;;;   ,---:     :---,                                    ,---:     :---,  
  ;;;   | X |     | Y |                                    | Y |     | Z |  
  ;;;   '---'     '---'                                    '---'     '---'  
  ;;; ."
  (node/call L
    (lambda (a.k a.v x y)
      (node/create a.k a.v x (node/create b.k b.v y z)))))



(defun double-r (c.k c.v l z)
  "Perform a double right rotation, moving Y2, the right subtree of the right subtree
  of the left subtree of C, into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node C is less then the weight of the left subtree of node C
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  right subtree of node B is greater than or equal to the weight of the left subtree 
  of node B multiplied by rotation coefficient {defvar wb::+gamma+} 
  ;;; .
  ;;;              ,---,                                    ,---,             
  ;;;              | C |                                    | B |             
  ;;;           ___:---:___                             ____:---:____          
  ;;;      ,---:           :---,                   ,---:             :---,       
  ;;;      | A |           | Z |                   | A |             | C |       
  ;;;      :---:           '---'        =>         :---:             :---:
  ;;; ,---:     :---,                         ,---:     :---,   ,---:     :---,  
  ;;; | X |     | B |                         | X |     | y1|   | y2|     | Z |  
  ;;; '---'     :---:                         '---'     '---'   '---'     '---'
  ;;;      ,---:     :---,   
  ;;;      | y1|     | y2|  
  ;;;      '---'     '---'
  ;;; ."
  (node/call L
    (lambda (a.k a.v x b)
      (node/call b
        (lambda (b.k b.v y1 y2)
          (node/create b.k b.v
            (node/create a.k a.v x y1)
            (node/create c.k c.v y2 z)))))))



(define-layered-method node/join :in-layer weight-balanced (k v l r)
  "Join left and right subtrees at root k/v, performing a single or double rotation
  to balance the resulting tree, if needed.  Assumes all keys in l < k < all keys in r,
  and the relative weight balance of the left and right subtrees is such that no more than
  one single/double rotation will result in each subtree being less than +delta+ times the
  weight of the other."
  (let ((l.w (node/weight l)) (r.w (node/weight r)))    
    (cond      
      ((> r.w (* +delta+ l.w))                       ;; -- right too big --
        (let ((r.l.w (node/weight (node/l r)))
               (r.r.w (node/weight (node/r r))))
          (if (< r.l.w (* +gamma+ r.r.w))
            (single-l k v l r)
            (double-l k v l r))))            
      ((> l.w (* +delta+ r.w))                       ;; -- left too big --
        (let ((l.l.w (node/weight (node/l l)))
               (l.r.w (node/weight (node/r l))))
          (if (< l.r.w (* +gamma+ l.l.w))
             (single-r k v l r)
             (double-r k v l r))))
      (t                                             ;; -- just right --
          (node/create k v l r)))))
               


(define-layered-method node/concat3 :in-layer weight-balanced (k v l r)
  (cond
    ((empty? l) (node/add r k v))
    ((empty? r) (node/add l k v))
    (t          (let ((w1 (node/weight l))
                       (w2 (node/weight r)))
                  (cond
                    ((< (* +delta+ w1) w2) (node/call r
                                             (lambda (k2 v2 l2 r2)
                                               (node/join k2 v2 (node/concat3 k v l l2) r2))))
                    ((< (* +delta+ w2) w1) (node/call l
                                             (lambda (k1 v1 l1 r1)
                                               (node/join k1 v1 l1 (node/concat3 k v r1 r)))))
                    (t
                      (node/create k v l r)))))))


(define-layered-method node/concat :in-layer weight-balanced (node1 node2)
  (cond
    ((empty? node1) node2)
    ((empty? node2) node1)
    (t
      (let ((minimum (node/least node2)))
        (node/concat3
          (node/k minimum)
          (node/v minimum)
          node1
          (node/remove-least node2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight Balanced Set Operator Specializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ord:compare 0 1)
;; (ord:compare< 0 1)

(define-layered-method node/subset? :in-layer weight-balanced (sub super)
  (labels  ((contains? (key node)
              (node/find key node))            
             (subset?  (node1 node2)
               (or (empty? node1)
                 (and (<= (node/size node1) (node/size node2))
                   (kvlr (k1 v1 l1 r1) node1
                     (declare (ignore v1))
                     (kvlr (k2 v2 l2 r2) node2
                       (declare (ignore v2))
                       (let ((c (ord:compare k1 k2)))
                         (cond
                           ((minusp c) (and
                                         (subset?   l1 l2)
                                         (contains? k1 node2)
                                         (subset?   r1 node2)))
                           ((plusp  c) (and
                                         (subset?   r1 r2)
                                         (contains? k1 node2)
                                         (subset?    l1 node2)))
                           (t          (and
                                         (subset?   l1 l2)
                                         (subset?   r1 r2)))))))))))
    (or (empty? sub) (subset? sub super))))



(define-layered-method node/at-index :in-layer weight-balanced (node index &optional caller)
  (labels ((recur (node index)
             (lr (l r) node
               (let ((l.size (node/size l)))
                 (cond
                   ((< index l.size) (recur l index))
                   ((> index l.size) (recur r (- index (+ 1 l.size))))
                   (t node))))))
    (let ((bound (node/size node)))
      (if (not (and (<= 0 index) (< index bound)))
        (error "illegal range argument ~D ~S" index caller))
      (recur node index))))



(define-layered-method node/rank :in-layer weight-balanced (k node &optional (rank 0))
  (cond
    ((empty? node)                    (return-from node/rank nil))
    ((ORD:|COMPARE<| k (node/k node)) (node/rank k (node/l node) rank))
    ((ORD:|COMPARE>| k (node/k node)) (node/rank k (node/r node)
                                        (+ 1 rank (node/size (node/l node)))))
    (t
      (+ rank (node/size(node/l node))))))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Height Balanced Layer (Red-Black Tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (deflayer height-balanced (balanced)
;;   ()
;;   (:documentation ""))

(defun invalid-argument (fn-name)
  (error "Invalid argument in: ~A" fn-name))

(defun not-found ()
   (error "Not found"))

(defmacro kvlrh ((k v l r h) node &body body)
  "destructure node node: left value right height"
  (let ((gnode (gensym (symbol-name :node-))))
    `(let ((,gnode ,node))
       (let ((,k  (node/k ,gnode))
              (,v  (node/v ,gnode))
              (,l  (node/l ,gnode))
              (,r  (node/r ,gnode))
              (,h  (node/x ,gnode)))
         ,@body))))


(define-layered-function node/kvlrh (node))

(define-layered-method node/kvlrh :in-layer height-balanced (node)
  (kvlrh (k v l r h) node
    (list k v l r h)))


(define-layered-function node/h (node))

(define-layered-method node/h :in-layer height-balanced (node)
  (node/x node))

(define-layered-method node/height :in-layer height-balanced (node)
  (cond ((empty? node) 0)
    (t
      (node/x node))))


(define-layered-method node/create :in-layer height-balanced (k v l r)
  "create a tree node with left son l, key k, value v, and right son r.
   Must have all elements of l < v < all elements of r.
   l and r must be balanced and have a height difference =< 2"
  (node k v l r (+ 1 (max (node/height l) (node/height r)))))


(define-layered-method node/join :in-layer height-balanced (k v l r)
  "Join left and right subtrees at root k/v, performing a rotation
  step to balance the resulting tree, if needed.  Assumes all keys in
  l < k < all keys in r, l and r balanced, and height difference <= 3"
  (let ((hl (node/height l))
         (hr (node/height r)))
    (cond
      ((> hl (+ 2 hr))    (kvlr (lk lv ll lr) l
                            (if (>= (node/height ll) (node/height lr))
                              (node/create lk lv ll (node/create k v lr r))
                              (kvlr (lrk lrv lrl lrr) lr
                                (node/create lrk lrv
                                  (node/create lr lv ll lrl)
                                  (node/create k v lrr r))))))
      ((> hr (+ 2 hl))    (kvlr (rk rv rl rr) r
                            (if (>= (node/height rr) (node/height rl))
                              (node/create rk rv (node/create k v l rl) rr)
                              (kvlr (rlk rlv rll rlr) rl
                                (node/create rlk rlv
                                  (node/create k v l rll)
                                  (node/create rk rv rlr rr))))))
        (t
          (node/create k v l r)))))


(define-layered-method node/concat3 :in-layer height-balanced (k v l r)
  (cond
    ((empty? l) (node/add r k v))
    ((empty? r) (node/add l k v))
    (t
      (kvlrh (lk lv ll lr lh) l
         (kvlrh (rk rv rl rr rh) r
           (cond
             ((> lh (+ 2 rh)) (node/join lk lv ll (node/concat3 k v lr r)))
             ((> rh (+ 2 lh)) (node/join rk rv (node/concat3 k v l rl) rr))
             (t               (node/create k v l r))))))))



(define-layered-method node/concat :in-layer height-balanced (node1 node2)
  (cond
    ((empty? node1) node2)
    ((empty? node2) node1)
    (t
      (let ((n2min (node/least node2)))
        (kv (k v) n2min
          (node/concat3 k v node1  (node/remove-least node2)))))))



;; (define-layered-method node/concat2 :in-layer height-balanced (t1 t2)
;;   "merge -- merge two trees l and r into one.
;;    All elements of l must precede the elements of r
;;    Assume height difference <= 2"
;;    (cond ((null t1) t2)
;;          ((null t2) t1)
;;          (t (bal t1 (tree:min t2) (remove-min t2)))))
