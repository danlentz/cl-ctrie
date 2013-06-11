;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM-CTRIE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod node-eq ((node1 cvm:reference) (node2 cvm:reference))
  (eql (cvm:ref node1) (cvm:ref node2)))

(defclass cvm-ctrie (cvm:reference)
  ((mem :accessor mem-of :initarg :mem)
    (env :accessor env-of :initarg :env)))

(defmethod describe-object :after ((ctrie cvm-ctrie) stream)
  t)

(defmethod describe-object :around ((ctrie cvm-ctrie) stream)
  (with-ctrie ctrie
    (let1 ctrie-report (format nil "pathname: ~A" (cvm:memory-pathname (mem-of ctrie))) 
      (printv:with-printv-output-to (stream)
        (:printv :hr ctrie-report :hr)
        (terpri stream)
        (call-next-method)
        (terpri stream)
        (describe-object (mem-of ctrie) stream)
        (terpri stream)
        (:printv (cvm:ref (ctrie-root ctrie)))
        (let1 cnode-ref (cvm:ref (ref-value (inode-ref (ctrie-root ctrie))))
          (:printv
            cnode-ref
            (cvm:host-structure-class cnode-ref)
            (arcs-len (cvm:deref cnode-ref)) 
            (cnode-bitmap (cvm:deref cnode-ref))))
        (:printv "")
        (:printv (ctrie-name ctrie))
        (:printv (ctrie-context ctrie))
        (:printv (ctrie-hash ctrie))
        (:printv (ctrie-test ctrie))
        (:printv (ctrie-readonly-p ctrie))
        (:printv (ctrie-index ctrie))
        (:printv "" :hr)
        (terpri stream)))))
  
  
(defun make-cvm-ctrie (&key address mem root index name readonly-p test hash
                        stamp context env)
  (with-active-layers (cvm)
    (make-instance 'cvm-ctrie :address address :mem mem
      :root root :index index :name name :readonly-p readonly-p
      :test test :hash hash :stamp stamp :context context :env env)))

(heap::cvm-define-structure ctrie ("CVM-CTRIE" . "CL-CTRIE")
  root name test hash readonly-p stamp context index)

(defmethod initialize-instance :after ((self cvm-ctrie) &key root index name mem
                                        readonly-p test hash stamp context env
                                        &allow-other-keys) 
  (unless (cvm:address-of self)
    (let1 heap::*gc-memory* mem
      (cvm:ensure-package "CL-CTRIE")
      (let* ((addr (heap::cvm-make-structure (cvm:symbol 'cvm-ctrie) 8))) 
        (setf (cvm:address-of self)   addr)
        (setf (mem-of self) mem)                                                                
        (setf (env-of self) env)
        (cvm-ctrie-set-root       addr (cvm:ref root))
        (cvm-ctrie-set-index      addr (cvm:ref index))
        (cvm-ctrie-set-name       addr (cvm:ref name))
        (cvm-ctrie-set-readonly-p addr (cvm:ref readonly-p))
        (cvm-ctrie-set-test       addr (cvm:ref test))
        (cvm-ctrie-set-hash       addr (cvm:ref hash))
        (cvm-ctrie-set-stamp      addr (cvm:ref stamp))
        (cvm-ctrie-set-context    addr (cvm:ref context))))))

(defun new-cvm-root ()
  (with-active-layers (cvm)
    (make-inode (make-cnode) (make-generational-descriptor))))

(defmethod make-ctrie ((place pathname) &key name (readonly-p nil)
                        ;;(if-exists :open) (if-does-not-exist :create)
                        (context '(cvm)) (test 'equal) (hash 'sxhash)
                        (stamp 'constantly-nil))
  (funcall-with-layer-context (configuration-context context)
    (lambda ()
      (let* ((heap::*gc-memory* (cvm:memory place))
              (addr (sb-sys:sap-ref-64 (sb-sys:sap+ (cvm:memory-sap heap::*gc-memory*) 8) 0)))
        (cvm:with-generation ()
          (if (zerop addr) 
            (aprog1 (make-cvm-ctrie
                      :name (or name
                              (byte-vector-to-hex-string
                                (cvm:memory-id heap::*gc-memory*)))
                      :context context :readonly-p readonly-p :test test :hash hash :env nil
                      :stamp stamp :mem heap::*gc-memory* :index 0
                      :root (make-inode (make-cnode) (make-generational-descriptor)))
              (setf (sb-sys:sap-ref-64 (sb-sys:sap+ (cvm:memory-sap (mem-of it)) 8) 0)
                (cvm:address-of it))
              (heap::cvm-symbol-set-value (cvm:symbol 'cvm:*tip*) (cvm:address-of it))
              (pushnew 'cvm:*tip* heap::*common-variables*)
              (pushnew 'cvm:*tip* heap::*defined-common-variables*))
          (make-cvm-ctrie :mem heap::*gc-memory* :address addr)))))))

(defmethod ctrie-root ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref (cvm-ctrie-root (cvm:ref ctrie)))))

(defmethod ctrie-name ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-name (cvm:ref ctrie)))))

(defmethod ctrie-readonly-p ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-readonly-p (cvm:ref ctrie)))))

(defmethod ctrie-test ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-test (cvm:ref ctrie)))))

(defmethod ctrie-hash ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-hash (cvm:ref ctrie)))))

(defmethod ctrie-stamp ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-stamp (cvm:ref ctrie)))))

(defmethod ctrie-context ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-context (cvm:ref ctrie)))))

(defmethod ctrie-index ((ctrie cvm-ctrie))
  (let1 heap::*gc-memory* (mem-of ctrie)
    (cvm:deref
      (cvm-ctrie-index (cvm:ref ctrie)))))

(defmethod ctrie-env ((thing cvm-ctrie) &optional (default-context contextl::*active-context*))
  (or (env-of thing)
    (setf (env-of thing)
      (apply #'combined-layer-context default-context
        (ctrie-context thing)))))

(defmethod ctrie-p ((thing cvm-ctrie))
  t)

(defmethod funcall-with-ctrie-context ((ctrie cvm-ctrie) thunk)
  (let* ((*ctrie* ctrie)
          (heap::*gc-memory* (mem-of *ctrie*)))
    (funcall-with-layer-context (ctrie-env ctrie) thunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM-REF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-ref (cvm:reference)
  ())

(heap::cvm-define-structure ref ("CVM-REF" . "CL-CTRIE")
  prev stamp value)

(defmethod ref-prev ((thing cvm-ref)) (:printv
  (let* ((heap::*gc-memory* (cvm:memory-of thing))
          (prev (cvm:deref (cvm-ref-prev (cvm:ref thing)))))
    (when prev
      (make-instance 'cvm-ref
        :address (cvm:ref prev)
        :memory heap::*gc-memory*)))))

(defmethod ref-stamp ((thing cvm-ref))
  (cvm:deref
    (cvm-ref-stamp (cvm:ref thing))))

(defmethod ref-value ((thing cvm-ref)) (:printv
  (cvm:deref
    (cvm-ref-value (cvm:ref thing)))))

(defmethod ref-p ((thing cvm-ref))
  t)

(define-layered-method   make-ref :in cvm (&rest args &key stamp value prev)
  (declare (ignorable args))
  (cvm:with-generation ()
    (:printv :make-ref stamp value prev)
    (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-ref) 3)))
      (prog1 (make-instance 'cvm-ref :address addr :memory heap::*gc-memory*)
        (cvm-ref-set-prev  addr (cvm:ref prev))
        (cvm-ref-set-stamp addr (cvm:ref stamp))
        (cvm-ref-set-value addr (cvm:ref value))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM-FAILED-REF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-failed-ref (cvm-ref)
  ())

(heap::cvm-define-structure failed-ref ("CVM-FAILED-REF" . "CL-CTRIE")
  prev stamp value)

(defmethod ref-prev ((thing cvm-failed-ref)) (:printv
  (cvm:deref
    (cvm-failed-ref-prev (cvm:ref thing)))))

(defmethod ref-stamp ((thing cvm-failed-ref))
  (cvm:deref
    (cvm-failed-ref-stamp (cvm:ref thing))))

(defmethod ref-value ((thing cvm-failed-ref)) (:printv
  (cvm:deref
    (cvm-failed-ref-value (cvm:ref thing)))))

(defmethod failed-ref-p ((thing cvm-failed-ref))
  t)

(define-layered-method make-failed-ref :in cvm (&key stamp value prev)
  (cvm:with-generation ()
    (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-failed-ref) 3)))
      (:printv
        (prog1 (make-instance 'cvm-failed-ref :address addr :memory heap::*gc-memory*)
          (cvm-failed-ref-set-prev  addr (cvm:ref prev))
          (cvm-failed-ref-set-stamp addr (cvm:ref stamp))
          (cvm-failed-ref-set-value addr (cvm:ref value)))))
    ))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVM-INODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-inode (cvm:reference)
  ())

(heap::cvm-define-structure inode ("CVM-INODE" . "CL-CTRIE")
  ref gen)

(defmethod inode-ref ((thing cvm-inode)) (:printv
  (cvm:deref
    (cvm-inode-ref (cvm:ref thing)))))

(defmethod inode-gen ((thing cvm-inode)) (:printv
  (cvm:deref
    (cvm-inode-gen (cvm:ref thing))))
 )

(defmethod inode-p ((thing cvm-inode))
  t)

(defun make-cvm-inode (&key ref gen)
  (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-inode) 2)))
    (prog1 (make-instance 'cvm-inode :address addr :memory heap::*gc-memory*)
      (cvm-inode-set-ref addr (cvm:ref ref))
      (cvm-inode-set-gen addr (cvm:ref gen)))))

(define-layered-method   make-inode :in cvm (link-to &optional gen stamp prev)
  (cvm:with-generation ()
    (:printv link-to prev
      (make-cvm-inode
        :gen (or gen (make-generational-descriptor))
        :ref (make-ref :value link-to :stamp (or stamp (ctstamp)) :prev prev)))
    ))


(defmethod describe-object ((self cvm-inode) stream)
  (let ((heap::*gc-memory* (cvm:memory-of self)))
    (printv:with-printv-output-to (stream)
      (call-next-method)
      (let ((ref-class (cvm:host-structure-class
                         (cvm:address-of (inode-ref self))))
             (ref-stamp (ref-stamp (cvm:deref (inode-ref self))))
             (points-to  (cvm:host-structure-class
                           (cvm:address-of (inode-read self)))))
        (:printv "" :REF ref-class ref-stamp points-to "")
        (heap::gc-dump-cell (cvm:ref (inode-ref self)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAS ATOMICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cas-ctrie-root ((ctrie cvm-ctrie) (old cvm-inode) (new cvm-inode)) (:printv
  (let* ((heap::*gc-memory* (mem-of ctrie))
          (root-sap (cvm:heap-sap ctrie (+ 3 (cvm:addr ctrie)))))
    (eql (cvm:ref old)
      (cas-word-sap root-sap
        (cvm:ref old) (cvm:ref new)))))
)
(defmethod gcas-compare-and-set ((obj cvm-inode) expected new expected-stamp new-stamp prev)
  (declare (ignorable expected-stamp))
  (assert (eq heap::*gc-memory* (mem-of *ctrie*)))
    (:printv obj expected new prev new-stamp 
      (let* ((ref      (inode-ref obj))
              (val     (ref-value ref))
              (new-ref (cvm:ref (make-ref :value new :stamp new-stamp :prev prev)))
              (ref-sap (cvm:heap-sap heap::*gc-memory* (+ 3 (cvm:addr obj)))))
        (and (node-eq val expected)
          (eql (cvm:ref ref)
            (cas-word-sap ref-sap
              (cvm:ref ref) new-ref))))))

(defmethod cas-failed-ref ((inode cvm-inode) ref)
  (assert (and (eq heap::*gc-memory* (cvm:memory-of inode))
            (eq (cvm:memory-of inode) (mem-of *ctrie*)))) (:printv
  (let* ((prev (ref-prev ref))
          (failed-ref-prev (failed-ref-prev prev)))
    (eql (cvm:ref ref)
      (cas-word-sap (cvm:heap-sap heap::*gc-memory* (+ 3 (cvm:addr inode)))
        (cvm:ref ref) (cvm:ref failed-ref-prev)))))
  )

(defmethod cas-ref-prev ((ref cvm-ref) prev new-generator)
  (assert (and (eq heap::*gc-memory* (mem-of *ctrie*))))
(:printv
 (cas-word-sap (cvm:heap-sap heap::*gc-memory* (+ 3 (cvm:addr ref)))
    (cvm:ref prev) (cvm:ref (funcall new-generator)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-snode (cvm:reference)
  ())

(heap::cvm-define-structure snode ("CVM-SNODE" . "CL-CTRIE")
  key value)

(defmethod snode-key ((snode cvm-snode))
  (cvm:deref
    (cvm-snode-key (cvm:ref snode))))

(defmethod snode-value ((snode cvm-snode))
  (cvm:deref
    (cvm-snode-value (cvm:ref snode))))

(defmethod snode-p ((thing cvm-snode))
  t)

(defun make-cvm-snode (&key key value)
  (cvm:with-generation ()
    (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-snode) 2)))
      (prog1 (make-instance 'cvm-snode :address addr :memory heap::*gc-memory*)
        (cvm-snode-set-key addr (cvm:ref key))
        (cvm-snode-set-value addr (cvm:ref value))))))

(define-layered-method snode :in cvm (key value &key)
  (make-cvm-snode :key key :value value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          
(defclass cvm-lnode (cvm:reference)
  ())

(heap::cvm-define-structure lnode ("CVM-LNODE" . "CL-CTRIE")
  elt next)

(defmethod lnode-elt ((self cvm-lnode))
  (cvm:deref (cvm-lnode-elt (cvm:ref self))))

(defmethod lnode-next ((self cvm-lnode))
  (cvm:deref (cvm-lnode-next (cvm:ref self))))

(defmethod lnode-p ((thing cvm-lnode))
  t)

(defun make-cvm-lnode (&key elt next)
  (cvm:with-generation ()
    (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-lnode) 2)))
      (prog1 (make-instance 'cvm-lnode :address addr :memory heap::*gc-memory*)
        (cvm-lnode-set-elt  addr (cvm:ref elt))
        (cvm-lnode-set-next addr (cvm:ref next))))))

(define-layered-method make-lnode :in cvm (&key elt next)
  (make-cvm-lnode :elt elt :next next))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-tnode (cvm:reference)
  ())

(heap::cvm-define-structure tnode ("CVM-TNODE" . "CL-CTRIE")
  cell)

(defmethod tnode-cell ((thing cvm-tnode))
  (cvm-tnode-cell (cvm:ref thing)))

(defmethod tnode-p ((thing cvm-tnode))
  t)

(defun make-cvm-tnode (&key cell)
  
  (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-tnode) 1)))
    (prog1 (make-instance 'cvm-tnode :address addr :memory heap::*gc-memory*)
      (cvm-tnode-set-cell addr (cvm:ref cell)))))

(define-layered-method make-tnode :in cvm (&key cell)
  (make-cvm-tnode :cell cell))

(defmethod entomb ((lnode cvm-lnode))
  "Entomb an LNODE in a newly created TNODE"
  (with-active-layers (cvm)
    (make-tnode :cell lnode)))

(defmethod entomb ((snode cvm-snode))
  "Entomb an SNODE in a newly created TNODE"
  (with-active-layers (cvm)
    (make-tnode :cell snode)))

(defmethod resurrect ((node cvm-inode))
  (atypecase (inode-read node)
    (tnode (tnode-cell it))
    (t     node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-cnode (cvm:reference)
  ())

(heap::cvm-define-structure cnode ("CVM-CNODE" . "CL-CTRIE")
  bitmap arcs)

(defmethod cnode-bitmap ((thing cvm-cnode))
  (cvm:deref (cvm-cnode-bitmap (cvm:ref thing))))

(defmethod cnode-arcs ((thing cvm-cnode))
  (cvm-cnode-arcs (cvm:ref thing)))

(defmethod cnode-p ((thing cvm-cnode))
  t)

(defun make-cvm-cnode (&key (bitmap 0))
  (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-cnode) 2)))
    (prog1 (make-instance 'cvm-cnode :address addr :memory heap::*gc-memory*)
      (cvm-cnode-set-bitmap addr (cvm:ref bitmap))
      (cvm-cnode-set-arcs addr (heap::cvm-make-vector heap::ct-t
                                 (logcount bitmap))))))

(define-layered-method make-cnode :in cvm (&key (bitmap 0) initial-contents)
  (:printv
  (let* ((cnode (make-cvm-cnode :bitmap bitmap))
          (arcs (cnode-arcs cnode)))
    (prog1 cnode
      (loop for i from 0
        for e in initial-contents do
        (heap::cvm-svset arcs i (cvm:ref e)))))))
 
(defmethod arc-aref ((v integer) index)
  (warn "aref ~D ~D" v index)
  (cvm:deref (heap::cvm-svref v index)))

(defmethod arc-aref ((cnode cvm-cnode) index)
  (cvm:deref (heap::cvm-svref (cnode-arcs cnode) index)))

(defmethod (setf arc-aref) (value (v integer) index)
  (warn "aref ~D ~D ~A" v index value)
  (heap::cvm-svset v index (cvm:ref value)))

(defmethod (setf arc-aref) (value (cnode cvm-cnode) index)
  (heap::cvm-svset (cnode-arcs cnode) index (cvm:ref value)))

(defmethod arcs-len ((cnode cvm-cnode))
  (logcount (cnode-bitmap cnode)))


(defmethod refresh ((cnode cvm-cnode) gen)
  "Return a new cnode structure identical to CNODE, but with any
    arcs that are INODES refreshed to generational descriptor GEN"
  (map-cnode (lambda (arc) (refresh arc gen)) cnode))

(defmethod refresh ((inode cvm-inode) gen)
  (multiple-value-bind (val stamp) (inode-read inode)
    (declare (ignore stamp))
    (make-inode val gen (ctstamp))))

(defmethod refresh ((snode cvm-snode) gen)
  (declare (ignore gen))
  snode)

(defmethod leaf-node-key ((snode cvm-snode))
  (snode-key snode))

(defmethod leaf-node-key ((tnode cvm-tnode))
  (leaf-node-key (tnode-cell tnode)))

(defmethod leaf-node-value ((snode cvm-snode))
  (snode-value snode))

(defmethod leaf-node-value ((tnode cvm-tnode))
  (leaf-node-value (tnode-cell tnode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDCSS-DESCRIPTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cvm-rdcss-descriptor (cvm:reference)
  ())

(heap::cvm-define-structure rdcss-descriptor ("CVM-RDCSS-DESCRIPTOR" . "CL-CTRIE")
  ov ovmain nv committed)

(defmethod rdcss-descriptor-p ((thing cvm-rdcss-descriptor))
  t)

(defmethod rdcss-descriptor-ov ((thing cvm-rdcss-descriptor))
  (cvm:deref (cvm-rdcss-descriptor-ov (cvm:ref thing))))

(defmethod rdcss-descriptor-ovmain ((thing cvm-rdcss-descriptor))
  (cvm:deref (cvm-rdcss-descriptor-ovmain (cvm:ref thing))))

(defmethod rdcss-descriptor-nv ((thing cvm-rdcss-descriptor))
  (cvm:deref (cvm-rdcss-descriptor-nv (cvm:ref thing))))

(defmethod rdcss-descriptor-committed ((thing cvm-rdcss-descriptor))
  (cvm:deref (cvm-rdcss-descriptor-committed (cvm:ref thing))))

(defun make-cvm-rdcss-descriptor (&key ov ovmain nv committed)
  (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-rdcss-descriptor) 4)))
    (prog1 (make-instance 'cvm-rdcss-descriptor :address addr :memory heap::*gc-memory*)
      (cvm-rdcss-descriptor-set-ov addr (cvm:ref ov))
      (cvm-rdcss-descriptor-set-ovmain addr (cvm:ref ovmain))
      (cvm-rdcss-descriptor-set-nv addr (cvm:ref nv))
      (cvm-rdcss-descriptor-set-committed addr (cvm:ref committed)))))

(define-layered-method   make-rdcss-descriptor :in cvm  (&key ov ovmain nv committed)
  (:printv ov ovmain nv committed
  (make-cvm-rdcss-descriptor :ov ov :ovmain ovmain :nv nv :committed committed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass cvm-bnode (cvm:reference)
;;   ((%content :reader %bnode-content :initform nil)))

;; (heap::cvm-define-structure bnode ("CVM-BNODE" . "CL-CTRIE")
;;   k v l r x)

;; (defmethod bnode-k ((bnode cvm-bnode))
;;   (cvm:deref (cvm-bnode-k (cvm:ref bnode))))

;; (defmethod bnode-v ((bnode cvm-bnode))
;;   (cvm:deref (cvm-bnode-v (cvm:ref bnode))))

;; (defmethod bnode-l ((bnode cvm-bnode))
;;   (cvm:deref (cvm-bnode-l (cvm:ref bnode))))

;; (defmethod bnode-r ((bnode cvm-bnode))
;;   (cvm:deref (cvm-bnode-r (cvm:ref bnode))))

;; (defmethod bnode-x ((bnode cvm-bnode))
;;   (cvm:deref (cvm-bnode-x (cvm:ref bnode))))

;; (defmethod bnode-p ((thing cvm-bnode))
;;   t)


;; (defun make-cvm-bnode (&key k v l r x)
;;   (let ((addr (heap::cvm-make-structure (cvm:ref 'cvm-bnode) 5)))
;;     (prog1 (make-instance 'cvm-bnode :address addr :memory heap::*gc-memory*)
;;       (cvm-bnode-set-k addr (cvm:ref k))
;;       (cvm-bnode-set-v addr (cvm:ref v))
;;       (cvm-bnode-set-l addr (cvm:ref l))
;;       (cvm-bnode-set-r addr (cvm:ref r))
;;       (cvm-bnode-set-x addr (cvm:ref x)))))


;; (defmethod pointer:deref ((bnode cvm-bnode) &optional type &rest args)
;;   (declare (ignore type args))
;;   (or (%bnode-content bnode)
;;     (setf (slot-value bnode '%content)
;;       (coerce (loop for i from 0 below 6 collect
;;                 (cvm:deref (heap::cvm-structure-ref (cvm:ref bnode) i)))
;;         'vector))))


;; (define-layered-method make-bnode :in-layer cvm (k v l r x &optional (allocator 'make-cvm-bnode)
;;                                                   &rest args)
;;   (declare (ignore args))
;;   (funcall allocator :k k :v v :l l :r r :x x))


;; (describe
;;   (with-ctrie w7 (pointer:deref 
;;   (make-bnode 0 1 2 3 4))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod map-node ((node cvm-inode) fn)
  (map-node (inode-read node) fn))

(defmethod map-node ((snode cvm-snode) fn)
  (funcall fn (snode-key snode) (snode-value snode)))

(defmethod map-node ((node cvm-tnode) fn)
  (map-node (tnode-cell node) fn))

(defmethod map-node ((node cvm-lnode) fn)
  (map-node (lnode-elt node) fn)
  (map-node (lnode-next node) fn))

(defmethod map-node ((node cvm-cnode) fn)
  (loop for arc across (cnode-arcs node)
    do (map-node arc fn)))
