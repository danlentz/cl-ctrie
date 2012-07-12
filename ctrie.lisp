;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

(defparameter *debug*   nil)
(defparameter *retries* 8192)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ctrie-error (error) 
  ((ctrie :initarg :ctrie :reader ctrie-of))
  (:documentation "Abstract superclass of CTrie related conditions."))

(define-condition ctrie-structural-error (ctrie-error)
  ((node :initarg :node :reader node-of))
  (:documentation "Condition designating that the ctrie data structure has been
   determined to be invalid."))

(define-condition ctrie-operational-error (ctrie-error)
  ((op :initarg :op :reader op-of))
  (:documentation "Condition for when an operational failure or inconsistency has occurred."))

(define-condition ctrie-operation-retries-exceeded (ctrie-operational-error)
  ((retries :initarg :retries :reader retries-of))
  (:documentation "Condition indicating an operation has failed the maximum number of
   times specified by the special-variable *retries*"))

(define-condition ctrie-not-implemented (ctrie-error) ()
  (:documentation "Condition designating functionality for which the implementation has not
   been written, but not deliberately excluded."))

(define-condition ctrie-not-supported (ctrie-error) ()
  (:documentation "Condition designating functionality that is deliberately not supported."))

(defmacro ctrie-error (ctrie condition &rest args)
  `(error (make-condition ',condition :ctrie ,ctrie ,@args)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-Catch/Case
;; experimenting with catch/throw as possibly a lighter-weight mechanism
;; than the condition system to deal with the unusual, case-wise exceptions
;; and non-local control-flow described by some of the ctrie algorithms.
;; I believe this originated with cilk (?) by radshaw (?) Terry but i need to
;; check on that and then attribute correctly.  The intent is to have a lighter
;; weight vehicle for control flow than would be possible with conditions and
;; handlers for use in tight CAS loops, as in some cases I've seen that the
;; condition system of many lisp implementations does not seem to be highly
;; optimized for performance-critical regions.   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro multi-catch (tag-list &body forms)
    "Macro allowing catch of multiple tags at once and
  finding out which tag was thrown.
    RETURNS:   (values RESULT TAG):
       *  RESULT is either the result of evaluationg FORMS or the value
          thrown by the throw form.
       *  TAG is NIl if evaluation of the FORMS completed normally
          or the tag thrown and cought.
    EXAMPLE: (multiple-value-bind (result tag)
               (multi-catch (:a :b)
                    ...FORMS...)
                 (case tag 
                    (:a ...)
                    (:b ...)
                    (t ...)))"
    (let ((block-name (gensym)))
      `(block ,block-name
         ,(multi-catch-1 block-name tag-list forms))))

  (defun multi-catch-1 (block-name tag-list forms)
    "Helper for multi-catch macro"
    (if (null tag-list) `(progn ,@forms)
      (let ((tmp (gensym)))
        `(let ((,tmp (catch ,(first tag-list)
                       (return-from ,block-name 
                         (values ,(multi-catch-1 block-name (rest tag-list) forms))))))
           (return-from ,block-name (values ,tmp ,(first tag-list)))))))

  (defmacro catch-case (form &rest cases)
    (let ((tags (remove t (mapcar #'first cases)))
           (tag (gensym)))
      `(multiple-value-bind (it ,tag)
         (multi-catch ,tags ,form)
         (cond 
           ,@(loop for case in cases
               collect `(,(if (eq (first case) t) t  
                            `(eql ,tag ,(first case)))
                          ,@(rest case))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flag Vector / Bitmap Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flag (key level)
  (ash 1 (logand (ash (sxhash key) (- level)) #x1f)))

(defun flag-present-p (flag bitmap)
  (plusp (logand flag bitmap)))

(defun flag-arc-position (flag bitmap)
  (logcount (logand (- flag 1) bitmap)))

(defun flag-vector (&optional (content 0))
  (loop with new-vector = (make-array 32 :element-type 'bit)
    for i from 0 to 31 when (logbitp i content)
    do (setf (sbit new-vector i) 1)
    finally (return new-vector)))


(defvar %empty-map%
  (vector))

(defvar %no-flags%
  (flag-vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ref
  "Atomically Stamped Reference"
  (stamp 0   :type fixnum)
  (value t   :type t)
  (prev  nil :type t))

(defstruct (inode (:constructor %make-inode))
  "Mutable Indirection Node"
  ref)

(defmethod print-object ((o inode) stream)
  (terpri stream)
  (format stream "    ")
  (if (ref-p (inode-ref o))
    (print-unreadable-object (o stream)
      (format stream "~a ~C~A ~A~C~%            ~s ~s~%            ~s ~s" 
        :inode #\( :has-prev? (not (null (ref-prev (inode-ref o)))) #\)
        :stamp (ref-stamp (inode-ref o))
        :value (ref-value (inode-ref o))))
    (call-next-method)))

(defun make-inode (val &optional initial-stamp prev)
  (%make-inode :ref (make-ref :value val :stamp (or initial-stamp 0) :prev prev)))

(defgeneric deref (obj)
  (:method (obj)
    (values obj nil))
  (:method ((obj inode))
    (let ((o (inode-ref obj)))
      (values (ref-value o) (ref-stamp o) (ref-prev o)))))
  
(defgeneric deref-set! (object value &optional stamp prev)
  (:method ((object inode) value &optional stamp prev)
    (setf (inode-ref object) (make-ref :value value :stamp (or stamp 0) :prev prev))))

(defmacro inode-compare-and-set (obj expected new expected-stamp new-stamp &optional prev)
  `(let ((ref (inode-ref ,obj)))
     (and
       (=   (ref-stamp ref) ,expected-stamp)
       (eql (ref-value ref) ,expected)
       (eq  (sb-ext:compare-and-swap (inode-ref ,obj) ref
              (make-ref :value ,new :stamp ,new-stamp :prev ,prev))
         ref))))

(defun inode-mutate (inode expected-value new-value &optional expected-stamp new-stamp prev)
  (multiple-value-bind (x s) (deref inode)
    (declare (ignore x))
    (when (inode-compare-and-set inode expected-value new-value
            (or expected-stamp s) (or new-stamp (1+ s)) prev)
      inode))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct snode
  "Storage Node"
  key value)
  
(defun snode (k v)
  (make-snode :key k :value v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct lnode
  "List Node"
  elt next)

(defun enlist (&rest rest)
  (unless (null rest)
    (awhen (car rest)
      (make-lnode :elt it :next (apply #'enlist (cdr rest))))))

(defun lnode-removed (orig-lnode key test)
  (let1 elts (loop for lnode = orig-lnode then (lnode-next lnode) while lnode
               unless (funcall test key (leaf-node-key (lnode-elt lnode)))
               collect (lnode-elt lnode))
    (apply #'enlist elts)))

(defun lnode-inserted (orig-lnode key value test)
  (let1 elts (loop for lnode = orig-lnode then (lnode-next lnode) while lnode
               unless (funcall test key (leaf-node-key (lnode-elt lnode)))
               collect (lnode-elt lnode))
    (apply #'enlist (push (snode key value) elts))))

(defun lnode-search (lnode key test)
  (loop for current = lnode then (lnode-next current) while current
    when (funcall test key (leaf-node-key (lnode-elt current)))
    return (values (leaf-node-value (lnode-elt current)) t)
    finally (return (values nil nil))))

(defun lnode-length (lnode)
  (loop with len = 0
    for current = lnode then (lnode-next current)
    while current do (incf len)
    finally (return len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tnode
  "Tomb Node"
  cell)

(defgeneric entomb (node)
  (:method (node)          (error "Entombment of ~A (type ~S) is undefined." node (type-of node)))
  (:method ((lnode lnode)) (make-tnode :cell lnode))
  (:method ((snode snode)) (make-tnode :cell snode)))

(defgeneric resurrect (node)
  (:method (node)
    node)
  (:method ((node inode))
    (atypecase (deref node)
      (tnode (tnode-cell it))
      (t     node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (cnode (:constructor %make-cnode))
  "Content Node"
  (bitmap 0)
  (flags  %no-flags%)
  (arcs   %empty-map%))

(defun make-cnode (&optional (bitmap 0))
  (%make-cnode
    :bitmap bitmap
    :flags (flag-vector bitmap)
    :arcs  (make-array (logcount bitmap))))

(defun cnode-extended (cnode flag position value)
  (let* ((new-bitmap (logior flag (cnode-bitmap cnode)))
          (new-cnode  (make-cnode new-bitmap)))
    (prog1 new-cnode
      (map-into (cnode-arcs new-cnode) #'identity (cnode-arcs cnode))
      (setf (svref (cnode-arcs new-cnode) position) value)
      (unless (> position (length (cnode-arcs cnode)))
        (replace (cnode-arcs new-cnode) (cnode-arcs cnode)
          :start1 (+ position 1) :start2 position)))))

(defun cnode-updated (cnode position value)
  (let ((new-cnode (make-cnode (cnode-bitmap cnode))))
    (prog1 new-cnode
      (replace (cnode-arcs new-cnode) (cnode-arcs cnode))
      (setf (svref (cnode-arcs new-cnode) position) value))))

(defun cnode-truncated (cnode flag pos)
  (let* ((new-bitmap (logxor flag (cnode-bitmap cnode)))
          (new-cnode (make-cnode new-bitmap)))
    (prog1 new-cnode
      (loop with src-index = 0
        for dest-index from 0
        for dest across (cnode-arcs new-cnode)
        when  (= src-index pos) do (incf src-index)
        do (setf (svref (cnode-arcs new-cnode) dest-index)
             (svref (cnode-arcs cnode) (post-incf src-index)))))))

(defun map-cnode (fn cnode)
  (check-type fn function)
  (check-type cnode cnode)
  (aprog1 (make-cnode (cnode-bitmap cnode))
    (map-into (cnode-arcs it) fn (cnode-arcs cnode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compression Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cnode-contracted (cnode level)
  (let1 arcs (cnode-arcs cnode)
    (if (and (plusp level) (eql 1 (logcount (cnode-bitmap cnode))))
      (atypecase (svref arcs 0)
        (snode (entomb it))
        (t     cnode))
      cnode)))

(defun cnode-compressed (cnode level)
  (check-type cnode cnode)
  (cnode-contracted (map-cnode #'resurrect cnode) level))

(defun clean (inode level)
  (check-type inode inode)
  (check-type level fixnum)
  (loop until (let1 node (deref inode)
                (if (cnode-p node)
                  (inode-mutate inode node (cnode-compressed node level))
                  t))))

(defun clean-parent (parent-inode target-inode key level)
  (check-type parent-inode (or null inode))
  (check-type target-inode inode)
  (check-type level        fixnum)
  (loop while (let ((parent-ref  (deref parent-inode))
                     (target-ref (deref target-inode)))
                (when (cnode-p parent-ref)
                  (let* ((bmp   (cnode-bitmap parent-ref))
                          (flag (flag key level))
                          (pos  (flag-arc-position flag bmp)))
                    (when (and (flag-present-p flag bmp) (tnode-p target-ref)
                            (eq target-inode (svref (cnode-arcs parent-ref) pos))                       
                            (let1 new-cnode (cnode-updated parent-ref pos (resurrect target-inode))
                              (not (inode-mutate parent-inode parent-ref
                                     (cnode-contracted new-cnode level)))))))))))
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node type abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype leaf-node ()
  `(or snode tnode))

(deftype branch-node ()
  `(or inode snode))

(deftype main-node ()
  `(or cnode lnode tnode))

(defgeneric leaf-node-key (resource)
  (:method (default)
    (error "Can't get leaf-node key: ~S is not a leaf resource." default))
  (:method ((snode snode))
    (snode-key snode))
  (:method ((tnode tnode))
    (leaf-node-key (tnode-cell tnode))))

(defgeneric leaf-node-value (resource)
  (:method (default)
    (error "Can't get leaf-node value: ~S is not a leaf resource." default))
  (:method ((snode snode))
    (snode-value snode))
  (:method ((tnode tnode))
    (leaf-node-value (tnode-cell tnode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE Root Container
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ctrie
  (readonly-p  nil)
  (test       'equal)
  (root       (make-inode (make-cnode))))

(defgeneric find-ctrie-root (ctrie-designator)
  (:method ((ctrie ctrie))
    (ctrie-root ctrie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cas-ctrie-root / This Operation Appears to have changed in recently published
;; descriptions of the ctrie which change the protocol to eliminate this
;; possibility by starting with a fully initialized level 0 cnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Todo: Deprecate

(defun cas-ctrie-root (ctrie new-value)
  "Perform operation on root-inode of ctrie, potentially applying the specified
  'null-root-inode' normalization steps first, if required."
  (labels ((try (n)
           (when (minusp (decf n)) (error "cas failed"))
           (let ((current (ctrie-root ctrie)))
             (etypecase current
               (null   (or (awhen (cas (ctrie-root ctrie) nil new-value)
                             (if (not (null new-value))
                               (return-from cas-ctrie-root it)
                               (return-from cas-ctrie-root nil)))
                             (try n)))
               (inode  (if (null (deref current))
                         (if (eq current (cas (ctrie-root ctrie) current new-value))
                            (return-from cas-ctrie-root current)
                           (try n))))))))
    (try 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT/INSERT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %insert (Inode key value level parent test)
  (declare (ignorable parent))
  (check-type level fixnum)
  (atypecase (deref inode)
    (tnode (throw :restart (clean parent (- level 5))))          
    (lnode (or (inode-mutate inode it (lnode-inserted it key value test))
             (throw :restart it)))
    (cnode (let* ((Cnode it)
                   (bmp  (Cnode-bitmap Cnode))
                   (flag (flag key level))
                   (pos  (flag-arc-position flag bmp)))
             (if (not (flag-present-p flag bmp))
               (let ((new-Cnode (Cnode-extended Cnode flag pos (snode key value))))
                 (if (inode-mutate inode cnode new-cnode)
                   (return-from %insert value)
                   (throw :restart Cnode)))
               (let ((present (svref (Cnode-arcs Cnode) pos))) 
                 (typecase present
                   (Inode (%insert present key value (+ level 5) Inode test))
                   (Snode (if (funcall test key (snode-key present))
                            (let ((new-Cnode (Cnode-updated Cnode pos (snode key value))))
                              (if (inode-mutate inode cnode new-cnode)
                                (return-from %insert value)
                                (throw :restart present)))
                            (let* ((new-flag-this (flag key (+ level 5)))
                                    (new-flag-other (flag (snode-key present) (+ level 5)))
                                    (new-bitmap (logior new-flag-this new-flag-other))
                                    (new-Cnode (make-Cnode new-bitmap))
                                    (new-Inode (make-inode new-Cnode)))
                              (setf (svref (Cnode-arcs new-Cnode)
                                      (flag-arc-position new-flag-this new-bitmap))
                                (snode key value))
                              (setf (svref (Cnode-arcs new-Cnode)
                                      (flag-arc-position new-flag-other new-bitmap))
                                present)
                              (if (inode-mutate inode cnode (Cnode-updated Cnode pos new-Inode))
                                (return-from %insert value)
                                (throw :restart present))))))))))))

(defun ctrie-put (ctrie key value)
  (check-type ctrie ctrie)
  (loop  with d = 0 and p = nil for try from 1
    for  r =  (find-ctrie-root ctrie)
    when (catch-case (%insert r key value d p (ctrie-test ctrie))
           (:restart  (when *debug*
                      (format t  "~8A  retry ~D insert (~A . ~A)~%" it try key value)))
           (t       (prog1 it
                      (when *debug*
                        (format t  "~8S  done on try ~D.~%~S~%" it try ctrie)))))
    return it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET/LOOKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %lookup (Inode key level parent test)
  (declare (ignorable parent))
  (check-type level fixnum)
  (let1 node (deref inode) 
    (typecase node
      (tnode (throw :restart (clean parent (- level 5))))
      (lnode (or (lnode-search node key test)
               (throw :notfound node)))
      (cnode (let* ((Cnode node)
                     (bmp  (Cnode-bitmap Cnode))
                     (flag (flag key level))
                     (pos  (flag-arc-position flag bmp)))
               (if (not (flag-present-p flag bmp))
                 (throw :notfound cnode)
                 (let ((found (svref (Cnode-arcs Cnode) pos)))
                   (typecase found
                     ((or snode tnode)
                       (if (funcall test key (leaf-node-key found))
                         (return-from %lookup (values (leaf-node-value found) t))
                         (throw :notfound found)))
                     (inode (return-from %lookup
                              (%lookup found key (+ level 5) inode test)))))))))))

(defun ctrie-get (ctrie key)
  (check-type ctrie ctrie)
  (flet ((attempt-get (root key level parent test try)
           (catch-case (%lookup root key level parent test)
             (:notfound  (return-from ctrie-get
                           (values nil nil)))
             (:restart   (multiple-value-prog1 (values it nil)
                           (when *debug* (format t  "~8S  restarting after try ~D.~%" it try))))
             (t          (multiple-value-prog1 (values it t)
                           (when *debug* (format t  "~8S  done on try ~D.~%" it try)))))))
    (loop with vals with d = 0 and p = nil
      for  r = (find-ctrie-root ctrie) for try from 1
      do   (setf vals (multiple-value-list (attempt-get r key d p (ctrie-test ctrie) try)))
      when (> try 1000)  do (error "try ~D" try)
      when (second vals) do (return-from ctrie-get (values (first vals) t)))))

(defun (setf ctrie-get) (value ctrie key)
  (multiple-value-prog1 (values value key ctrie)
    (ctrie-put ctrie key value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DROP/REMOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         
(defun %remove (inode key level parent test)
  (check-type inode inode)
  (atypecase (deref inode)
    (tnode (values (clean parent (- level 5)) :restart))
    (lnode (let* ((new-lnode      (lnode-removed it key test))
                   (groomed-lnode (if (null (lnode-next new-lnode))
                                    (entomb new-lnode)
                                    new-lnode))
                   (leaf-value    (%lookup inode key level parent test)))
             (if (inode-mutate inode it groomed-lnode)
               (values leaf-value t)
               (values it :restart))))
    (cnode (let* ((bmp  (cnode-bitmap it))
                   (flag (flag key level))
                   (pos  (flag-arc-position flag bmp)))
             (if (not (flag-present-p flag bmp))
               (values key :notfound)
               (let1 found (svref (cnode-arcs it) pos)
                 (multiple-value-bind (result-value result-kind)
                   (typecase found
                     (inode (%remove found key (+ level 5) inode test))
                     ((or snode tnode)
                       (if (funcall test key (leaf-node-key found))
                              (let1 new-cnode (cnode-truncated it flag pos)
                                (if (inode-mutate inode it (cnode-contracted new-cnode level))
                                  (values (leaf-node-value found) t)
                                  (values nil :restart)))
                              (values nil :notfound))))
                   (case result-kind
                     (:restart  (values result-value :restart))
                     (:notfound (values nil nil))
                     (t         (multiple-value-prog1 (values result-value t)                        
                                  (when (tnode-p (deref inode))
                                    (clean-parent parent inode key (- level 5)))))))))))))

  
(defun ctrie-drop (ctrie key)
  (check-type ctrie ctrie)
  (let1 r (find-ctrie-root ctrie)
    (multiple-value-bind (val kind) (%remove r key 0 nil (ctrie-test ctrie))
      (case kind
        (:restart  (return-from ctrie-drop (ctrie-drop ctrie key)))
        (:notfound (values val nil))
        (t         (values val t))))))

                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-MAP & Friends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-node (node fn))

(defmethod map-node ((node inode) fn)
  (map-node (deref node) fn))

(defmethod map-node ((node snode) fn)
  (funcall fn (snode-key node) (snode-value node)))

(defmethod map-node ((node tnode) fn)
  (map-node (tnode-cell node) fn))

(defmethod map-node ((node lnode) fn)
  (loop for node = node then (lnode-next node)
    while node do (map-node (lnode-elt node) fn)))

(defmethod map-node ((node cnode) fn)
  (loop for arc across (cnode-arcs node)
    do (map-node arc fn)))

(defun ctrie-map (ctrie fn &aux accum)
  (declare (special accum))
  (let ((root (find-ctrie-root ctrie)))
    (map-node root fn))
  accum)

(defmacro ctrie-do ((key value ctrie) &body body)
  "Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t \"~&~8S => ~10S~%\" k v))"
  `(ctrie-map ,ctrie
     #'(lambda (,key ,value)
         ,@body)))

(defun ctrie-map-keys (ctrie fn)
  (ctrie-map ctrie
    (lambda (k v) (declare (ignore v))
      (funcall fn k))))

(defun ctrie-map-values (ctrie fn)
  (ctrie-map ctrie
    (lambda (k v) (declare (ignore k))
      (funcall fn v))))

(defgeneric ctrie-map-into (ctrie place fn)
  (:method ((ctrie ctrie) (place ctrie) fn)
      (ctrie-map ctrie
        (lambda (k v) 
          (apply #'ctrie-put place (funcall fn k v))))))

(defun print2 (x y &optional (stream t))
  (format stream " (~W . ~W) " x y))

(defun collect2 (x y)
  (declare (special accum))
  (push (cons x y) accum))

(defun collect-keys (x y)
  (declare (special accum) (ignore y))
  (push x accum))

(defun collect-values (x y)
  (declare (special accum) (ignore x))
  (push y accum))

(defun ctrie-keys (ctrie)
  (ctrie-map ctrie #'collect-keys))

(defun ctrie-values (ctrie)
  (ctrie-map ctrie #'collect-values))

(defun ctrie-size (ctrie &aux (accum 0))
  (ctrie-map-keys ctrie
    (lambda (x) (declare (ignore x))
      (incf accum)))
  accum)

(defun ctrie-empty-p (ctrie)
  (eql 0 (cnode-bitmap (deref (find-ctrie-root ctrie)))))

(defun ctrie-ensure-get (ctrie key default)
  "Like CTRIE-GET, but if KEY is not found in CTRIE, automatically adds 
   the entry (KEY. DEFAULT). Secondary return value is true if key was
   already in the table."
  (multiple-value-bind (val ok) (ctrie-get ctrie key)
      (if ok
        (values val t)
        (values (ctrie-put ctrie key default) nil))))

(defun ctrie-to-alist (ctrie)
  (ctrie-map ctrie #'collect2))

(defun ctrie-to-hashtable (ctrie)
  (alexandria:alist-hash-table
    (ctrie-map ctrie #'collect2)))

(defun ctrie-from-alist (alist)
  (let ((ctrie (make-ctrie)))
    (prog1 ctrie
      (mapc (lambda (pair) (ctrie-put ctrie (car pair) (cdr pair)))
        alist))))

(defun ctrie-from-hashtable (hashtable)
  (let ((ctrie (make-ctrie)))
    (prog1 ctrie
      (maphash (lambda (k v) (ctrie-put ctrie k v))
        hashtable))))

