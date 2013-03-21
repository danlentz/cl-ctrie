;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Height Balanced Layer (Red-Black Tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer height-balanced (balanced)
  ()
  (:documentation ""))

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
