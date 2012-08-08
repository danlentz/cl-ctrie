;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar       *ctrie*         nil
  "Within the dynamic extent of a CTRIE operation this variable will
  be bound to the root-container CTRIE operand.  It is an error if an
  operation is defined that attempts access to a CTRIE without this
  binding, which is properly established by wrapping the operation in
  an appropriate WITH-CTRIE form.")


(defparameter *retries*        16
  "Establishes the number of restarts permitted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-RETRIES-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.")


(defparameter *timeout*         2
  "Establishes the duration (in seconds) allotted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-TIMEOUT-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.")


(defparameter *context*       nil
  "Diagnostic value, not used in production, used to maintain
  additional information tracking the current dynamic state.")


(defparameter *debug*         nil
  "Debugging flag, not used in production, to enable generation of
  additional diagnostic and reporting information.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ctrie-error (error) 
  ((ctrie :initform *ctrie* :initarg :ctrie :reader ctrie-of))
  (:documentation "Abstract superclass of CTRIE related conditions."))


(define-condition ctrie-structural-error (ctrie-error)
  ((node :initarg :node :reader node-of))
  (:documentation "Condition designating that the CTRIE data structure
   has been determined to be invalid."))


(define-condition ctrie-generational-mismatch (ctrie-structural-error)
  ((expected :initarg :expected :reader expected)
    (found   :initarg :found    :reader found))
  (:documentation "Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal"))


(define-condition ctrie-operational-error (ctrie-error)
  ((op :initarg :op :reader op-of))
  (:documentation "Conditixon for when an operational failure or
  inconsistency has occurred."))


(define-condition ctrie-invalid-dynamic-context (ctrie-operational-error)
  ((ctrie-context :initform *ctrie* :initarg :ctrie-context :reader ctrie-context)
    (gen-context  :initarg :gen-context   :reader gen-context))
  (:documentation "Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form"))


(define-condition ctrie-operation-timeout-exceeded (ctrie-operational-error)
  ((seconds :initform *timeout* :initarg :seconds :reader seconds-of))
  (:documentation "Condition indicating an operation has failed the
   maximum number of times specified by the s-variable *retries*"))


(define-condition ctrie-operation-retries-exceeded (ctrie-operational-error)
  ((retries :initform *retries* :initarg :retries :reader retries-of))
  (:documentation "Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*"))


(define-condition ctrie-not-implemented (ctrie-error) ()
  (:documentation "Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded."))


(define-condition ctrie-not-supported (ctrie-error) ()
  (:documentation "Condition designating functionality that is
  deliberately not supported."))


(defmacro ctrie-error (ctrie condition &rest args)
  "Signal a CTRIE related condition."
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
    * RETURNS: (values RESULT TAG)
       -  RESULT is either the result of evaluationg FORMS or the value
          thrown by the throw form.
       -  TAG is NIl if evaluation of the FORMS completed normally
          or the tag thrown and cought.
    * EXAMPLE:
    ;;; (multiple-value-bind (result tag)
    ;;;            (multi-catch (:a :b)
    ;;;                 ...FORMS...)
    ;;;              (case tag 
    ;;;                 (:a ...)
    ;;;                 (:b ...)
    ;;;                 (t ...)))"
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
    "User api encapsulating the MULTI-CATCH control-structure in a
    syntactic format that is identical to that of the familiar CASE
    statement, with the addition that within the scope of each CASE
    clause, a lexical binding is established between the symbol IT and
    the value caught from the throw form."
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

(defun cthash (key)
  "Compute the hash value of KEY using the hash function defined by
  the CTRIE designated by the innermost enclosing WITH-CTRIE form."
  (funcall (ctrie-hash *ctrie*) key))


(defun ctequal (x y)
  "Test the equality of X and Y using the equality predicate defined
  by the CTRIE designated by the innermost enclosing WITH-CTRIE form."
  (funcall (ctrie-test *ctrie*) x y))


(defun flag (key level)
  "For a given depth, LEVEL, within a CTRIE, extract the correspondant
  sequence of bits from the computed hash of KEY that indicate the
  logical index of the arc on the path to which that key may be found.
  Note that the logical index of the arc is most likely not the same
  as the physical index where it is actually located -- for that see
  'flag-arc-position'"
  (ash 1 (logand (ash (cthash key) (- level)) #x1f)))


(defun flag-present-p (flag bitmap)
  "Tests the (fixnum) BITMAP representing the logical index of all
  arcs present in a CNODE for the presence of a particular arc whose
  logical index is represented by FLAG."
  (plusp (logand flag bitmap)))


(defun flag-arc-position (flag bitmap)
  "Given FLAG representing the logical index of an arc, and BITMAP
  representing all arcs present, compute a physical index for FLAG in
  such a manner as to always ensure all arcs map uniquely and
  contiguously to the smallest vector that can contain the given
  arcs."
  (logcount (logand (- flag 1) bitmap)))


(defun flag-vector (&optional (content 0))
  "FLAG-VECTOR is a bit-vector representation of the (fixnum)
  BITMAP. It is currently not used for any calculation, however it is
  included within each CNODE as a convenience because it makes it
  immediately clear from visual inspection which logical arc indexes
  are represented in the node. For example, from the bit-vector
  #*10010000000000000000000000000000 one can easily see that the first
  and fourth positions are occupied, and the rest empty."
  (loop with new-vector = (make-array 32 :element-type 'bit)
    for i from 0 to 31 when (logbitp i content)
    do (setf (sbit new-vector i) 1)
    finally (return new-vector)))


(defvar %empty-map% (vector)
  "Defines the initial value of an empty CNODE arc-vector.")

(defvar %no-flags%  (flag-vector)
  "Defines the initial value of a flag-vector representing a
  cnode containing an empty arc-vector.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (ref
             (:copier nil))
  "Atomically Stamped Reference structure [Herlithy, TAOMP] that
  encapsulates the mutable slots within an inode, although each ref
  structure is, itself, never mutated.  Incorporation of the ref
  structure provides the capability to 'bundle' additional metadata in
  an inode while still providing atomic compare and swap based a the
  single comparison of the aggregate ref instance.
   - STAMP defines a slot containing implementation-specific metadata
     that may be maintained internally as a means of tracking inode
     modification and update behavior.  It should not be referenced by
     user code, and the format of its contents should not be relied apon.
   - VALUE defines a slot that contains a reference to the MAIN-NODE
     that the enclosing inode should be interpreted as 'pointing to'
   - PREV defines a slot which, during the INODE-COMMIT phase of the
     _GCAS_INODE_PROTOCOL_, maintains a reference to the last valid
     inode state, which may be restored, if necessary, during the
     course of the INODE-READ/INODE-COMMIT arbitration."
  (stamp (local-time:now))
  (value t   :type t)
  (prev  nil :type t))


(defstruct (failed-ref
             (:include ref)
             (:copier nil))
  "A FAILED-REF is a structure that is used to preserve the linkage to
  prior inode state following a failed GCAS.  Any inode access that
  detects a FAILED-REF will immediately invoke a commit to restore the
  inode to the state recorded in FAILED-REF-PREV.")


(defstruct (inode
             (:constructor %make-inode)
             (:copier nil))
  "An INODE, or 'Indirection Node' is the mutable structure
  representing the link between other 'main-node' structures found in
  a ctrie.  An inode is the only type of node that may change the
  value of its content during its lifetime.  In this implementation,
  all such values as may change are encapsulated within a REF
  substructure.  Each inode also contains a generational descriptor
  object, comparible by identity only, which is used to identify the
  state of synchronization between the inode and the current
  'generation' indicated by the root inode at the base of the
  CTRIE. As an inode may not change its 'gen identity' during its
  lifetime, this disparity with the generation of the root node will
  immediately result in the replacement of the inode with a new one
  properly synchronized with the root's GEN object. In this
  implementation, GEN objects are implemented by GENSYMS -- unique,
  uninterned symbols which inherently provide very similar symantics
  to those required of the generational descriptor."
  gen ref)


(defun make-inode (link-to &optional gen stamp prev)
  "Construct a new INODE that represents a link to LINK-TO, optionally
  augmented with a specified generational descriptor, timestamp, and/or
  previous state."
  (%make-inode
    :gen (or gen (gensym "ctrie"))
    :ref (make-ref :value link-to :stamp (or stamp (local-time:now)) :prev prev)))


(defmethod print-object ((o inode) stream)
  (if (ref-p (inode-ref o))
    (print-unreadable-object (o stream)
      (format stream
        "~a ~a ~s~%           ~C~A ~A~C~%           ~s ~s" 
        :inode (inode-gen o) (ref-stamp (inode-ref o))        
        #\( :has-prev? (not (null (ref-prev (inode-ref o)))) #\)
        :=> (ref-value (inode-ref o))))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAS / GCAS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gcas-compare-and-set (obj expected new expected-stamp new-stamp prev)
  "A thin, macro layer abstraction over the basic compare-and-swap
  primitive which provides a consistent interface to the underlying
  inode structure and manages additional metadata, providing
  reasonable defaults when they are not specified."
  `(let ((ref (inode-ref ,obj)))
     (and
       (local-time:timestamp= (ref-stamp ref) ,expected-stamp)
       (eql (ref-value ref) ,expected)
       (eq  (sb-ext:compare-and-swap (inode-ref ,obj) ref
              (make-ref :value ,new :stamp ,new-stamp :prev ,prev))
         ref))))


(defun/inline INODE-READ (inode)
  "INODE-READ provides the top-level interface to the inode GCAS ACCESS api,
  which is the mechanism which must be used to gain access to the
  content of any NON-ROOT inode. For access to the root inode, refer
  to the RDCSS inode api 'ROOT-NODE-ACCESS'. Returns as four values,
  the MAIN-NODE, the STAMP, the PREVIOUS STATE (if any), and the REF
  structure encapsulated by the inode."
  (let (ref)
    (sb-thread:barrier (:read)
      (setf ref (inode-ref inode))
      (multiple-value-bind (val stamp prev)
        (values (ref-value ref) (ref-stamp ref) (ref-prev ref))
        (if (null prev)
          (return-from inode-read (values val stamp prev ref))
          (let1 result (inode-commit inode ref)
            (return-from inode-read (values
                                      (ref-value result)
                                      (ref-stamp result)
                                      (ref-prev result)
                                      result))))))))


(defun/inline INODE-MUTATE (inode old-value new-value)
  "INODE-MUTATE provides the top-level interface to the inode GCAS
  MODIFICATION api, which is the mechanism which must be used to
  effect any change in a NON-ROOT inode.  For modification of the
  root-inode, refer to the RDCSS inode api
  'ROOT-NODE-REPLACE'. Returns a boolean value which indicates the
  success or failure of the modification attempt."
  (multiple-value-bind (val stamp prev ref) (inode-read inode)
    (declare (ignore val prev ref))
    (if (gcas-compare-and-set inode
          old-value new-value stamp (local-time:now) old-value)
      (return-from inode-mutate
        (null (ref-prev (inode-commit inode (inode-ref inode)))))
      (return-from inode-mutate
        nil))))


(defun INODE-COMMIT (inode ref)
  "INODE-COMMIT implements the GCAS COMMIT protocol which is invoked
  as necessary by the INODE-READ and INODE-MUTATE entry-points.  It is
  not meant to be invoked directly, as this would most likely result
  in corruption. Returns the REF structure representing the content of
  whatever root inode wound up successfully committed -- either the
  one requested, or one represented by a previous state."
  (let1 prev (ref-prev ref)
    (typecase prev
      (null        (return-from inode-commit ref))
      (failed-ref  (if (cas (inode-ref inode) ref (failed-ref-prev prev))
                     (return-from inode-commit
                       (failed-ref-prev prev))
                     (return-from inode-commit
                       (inode-commit inode (inode-ref inode)))))
      (t            (if (and (not (ctrie-readonly-p *ctrie*))
                          (eql (inode-gen (find-ctrie-root *ctrie*))
                            (inode-gen inode)))
                      (if (cas (ref-prev ref) prev nil)
                        (return-from inode-commit ref)
                        (return-from inode-commit
                          (inode-commit inode ref)))
                      (progn (cas (ref-prev ref) prev
                               (make-failed-ref
                                 :value (ref-value prev)
                                 :stamp (ref-stamp prev)
                                 :prev  (ref-prev  prev)))
                        (return-from inode-commit
                          (inode-commit inode (inode-ref inode)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (snode
             (:copier nil))
  "SNODE, i.e., 'Storage Node', is the LEAF-NODE structure ultimately
  used for the storage of each key/value pair contained in the CTRIE.
  An SNODE is considered to be immutable during its lifetime.
   - KEY defines the slot containing an element of the map's domain.
   - VALUE defines the slot containing the range-element mapped to KEY."
  key value)
  
(defun snode (key value)
  "Construct a new SNODE which represents the mapping from
  domain-element KEY to range-element VALUE."
  (make-snode :key key :value value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (lnode
             (:copier nil))
  "LNODE, i.e., 'List Node', is a special structure used to enclose
  SNODES in a singly-linked chain when the hash-codes of the
  respective SNODE-KEYS collide, but those keys are determined to be
  unique by the CTRIE-TEST function defined for that ctrie. The order
  of the list is implemented (arbitrarily) as most recently added first,
  analogous to CL:PUSH.  An LNODE (and therefore a chain of LNODEs) is
  considered to be immutable during its lifetime.
   - ELT defines the slot containing an enclosed SNODE
   - NEXT defines a slot referencing the next LNODE in the chain, or NIL
     if no further LNODES remain."
  elt next)

(defun enlist (&rest rest)
  "Construct a chain of LNODE structures enclosing the values supplied.
  It is assumed (elsewhere) that each of these values is a valid SNODE
  structure."
  (unless (null rest)
    (awhen (car rest)
      (make-lnode :elt it :next (apply #'enlist (cdr rest))))))

(defun lnode-removed (orig-lnode key test)
  "Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but with any LNODE containing an SNODE equal to KEY
  removed.  Equality is tested as by the predicate function passed as
  the argument TEST. The order of nodes in the resulting list will
  remain unchanged."
  (let1 elts (loop for lnode = orig-lnode then (lnode-next lnode) while lnode
               unless (funcall test key (leaf-node-key (lnode-elt lnode)))
               collect (lnode-elt lnode))
    (apply #'enlist elts)))

(defun lnode-inserted (orig-lnode key value test)
  "Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but ensured to contain an LNODE enclosing an SNODE of
  KEY and VALUE.  If the given KEY equal to a key already present somewhere
  in the chain (as compared with equality predicate TEST) it will be
  replaced.  Otherwise a new LNODE will be added. In either case, the LNODE
  containing (SNODE KEY VAlUE) will be the first node in the resulting
  list."
  (let1 elts (loop for lnode = orig-lnode then (lnode-next lnode) while lnode
               unless (funcall test key (leaf-node-key (lnode-elt lnode)))
               collect (lnode-elt lnode))
    (apply #'enlist (push (snode key value) elts))))

(defun lnode-search (lnode key test)
  "Within the list of lnodes beginning with LNODE, return the range value
  mapped to by the first SNODE containing a key equal to KEY as determined
  by equality predicate TEST, or NIL if no such key is found.  As a
  second value, in order to support storage of NIL as a key, return T to
  indicate that the KEY was indeed found during search, or NIL to indicate
  that no such key was present in the list."
  (loop for current = lnode then (lnode-next current) while current
    when (funcall test key (leaf-node-key (lnode-elt current)))
    return (values (leaf-node-value (lnode-elt current)) t)
    finally (return (values nil nil))))

(defun lnode-length (lnode)
  "Return the number of LNODES present in the chain beginning at LNODE"
  (loop with len = 0
    for current = lnode then (lnode-next current)
    while current do (incf len)
    finally (return len)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (tnode
             (:copier nil))
  "Tomb Node"
  cell)

(defgeneric entomb (node)
  (:method (node)
    (error "Entombment of ~A (type ~S) is undefined." node (type-of node)))
  (:method ((lnode lnode)) (make-tnode :cell lnode))
  (:method ((snode snode)) (make-tnode :cell snode)))

(defgeneric resurrect (node)
  (:method (node)
    node)
  (:method ((node inode))
    (atypecase (inode-read node)
      (tnode (tnode-cell it))
      (t     node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (cnode
             (:constructor %make-cnode)
             (:copier nil))
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

(defgeneric refresh (place gen)
  (:method ((cnode cnode) gen)
    (map-cnode (lambda (arc) (refresh arc gen)) cnode))
  (:method ((inode inode) gen)
    (multiple-value-bind (val stamp) (inode-read inode)
      (declare (ignore stamp))
      (make-inode val gen (local-time:now))))
  (:method ((snode snode) gen)
    snode))

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
  (loop until (let1 node (inode-read inode)
                (if (cnode-p node)
                  (inode-mutate inode node (cnode-compressed node level))
                  t))))

(defun clean-parent (parent-inode target-inode key level)
  (check-type parent-inode (or null inode))
  (check-type target-inode inode)
  (check-type level        fixnum)
  (if (not (eq #1=(inode-gen target-inode) (inode-gen (find-ctrie-root *ctrie*))))
    (throw :restart #1#)
    (loop while (let ((parent-ref  (inode-read parent-inode))
                       (target-ref (inode-read target-inode)))
                  (when (cnode-p parent-ref)
                    (let* ((bmp   (cnode-bitmap parent-ref))
                            (flag (flag key level))
                            (pos  (flag-arc-position flag bmp)))
                      (when (and (flag-present-p flag bmp) (tnode-p target-ref)
                              (eq target-inode (svref (cnode-arcs parent-ref) pos))
                              (let1 new-cnode (cnode-updated parent-ref pos (resurrect target-inode))
                                (not (inode-mutate parent-inode parent-ref
                                       (cnode-contracted new-cnode level))))))))))))
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node type abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype leaf-node ()
  "A LEAF-NODE represents a terminal value in a CTRIE arc.
  LEAF-NODEs always contain a unit-payload of CTRIE data
  storage; For example, an SNODE contains a key/value pair."
  `(or snode tnode))

(deftype branch-node ()
  `(or inode snode))

(deftype main-node ()
  `(or cnode lnode tnode))

(defgeneric leaf-node-key (resource)
  (:method ((snode snode))
    (snode-key snode))
  (:method ((tnode tnode))
    (leaf-node-key (tnode-cell tnode))))

(defgeneric leaf-node-value (resource)
  (:method ((snode snode))
    (snode-value snode))
  (:method ((tnode tnode))
    (leaf-node-value (tnode-cell tnode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE Root Container
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (ctrie (:constructor %make-ctrie))
  "A CTRIE root container uniquely identifies a CTRIE instance, and
  contains the following perameters which specify the customizable
  aspects of each CTRIE instance:
   - READONLY-P, if true, prohibits any future modification or
  cloning of this instance.
   - TEST is a designator for an equality predicate that will be applied to
  determine the disambiguation of any two keys. It is recommened that
  this value be a symbol that is fboundp, to retain capability of
  externalization (save/restore). At present, though, this is not
  enforced and a function object or lambda expression will also be
  accepted, albeit without the ability of save/restore.
   - HASH is a designator for a hash function, which may be desirable to
  customize when one has specific knowledge about the set of keys
  which will populate the table.  At this time, a 32-bit hash is
  recommended as this is what has been used for development and
  testing and has been shown to provide good performance in
  practice. As with TEST, it is recommended that HASH be specified
  by a symbol that is fboundp.
   - ROOT is the slot used internally for storage of the root inode
  structure that maintains the reference to the contents of the ctrie
  proper.  The ctrie-root must only be accessed using the RDCSS root
  node protocol defined by the end-user entrypoints ROOT-NODE-ACCESS
  and ROOT-NODE-COMMIT."
  (readonly-p  nil)
  (test       'equal)
  (hash       'sxhash)
  (root       (make-inode (make-cnode) (gensym "ctrie"))))


(defun make-ctrie (&rest args &key name root (readonly-p nil)
                    (test 'equal) (hash 'sxhash))
  "CREATE a new CTRIE instance. This is the entry-point constructor api
  intended for use by the end-user."
  (declare (ignorable name readonly-p test hash root))
  (apply #'%make-ctrie args))


(defmacro/once with-ctrie (&once ctrie &body body)
  "Configure the dynamic environment with the appropriate condition handlers,
  control fixtures, and instrumentation necessary to perform the
  operations in BODY on the specified CTRIE. Unless specifically
  documented, the particular configuration of this dynamic environment
  should be considered an implementation detail and not relied upon. A
  particular exception, however, is that within the dynamic extent of
  a WITH-CTRIE form, the code implementing a CTRIE operation may
  expect that the special variable *CTRIE* will be bound to the root
  container of CTRIE operated upon.  See also the documentation for
  '*CTRIE*'"
  `(let* ((*ctrie* ,ctrie))
     (sb-ext:with-timeout *timeout*     
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDCSS root-inode protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defgeneric find-ctrie-root (ctrie-designator)
  (:documentation "FIND-CTRIE-ROOT is a subprimitive used by the
  internal CTRIE implementation to access the root inode of a given
  ctrie root-container. It does not provide safe access to the
  contents of that inode and should not be referenced by the
  higher-level implementation or end-user code.  The purpose of
  FIND-CTRIE-ROOT is to incorporate a level of indirection specialized
  on the class of the root container to facilitate future extension
  with alternate storage models, e.g., an external persistent disk-based
  store. See {defgeneric (cas cl-ctrie::find-ctrie-root)}")
  (:method ((ctrie ctrie))
    (ctrie-root ctrie)))


(defgeneric (cas find-ctrie-root) (ctrie-designator old new)
  (:documentation "(CAS FIND-CTRIE-ROOT) implements the atomic
  compare-and swap subprimitive that is the operational analogue to
  {defgeneric cl-ctrie::find-ctrie-root}. It should not be referenced
  by the higher-level implementation or end-user code. The purpose of
  (CAS FIND-CTRIE-ROOT) is to incorporate a level of indirection specialized
  on the class of the root container to facilitate future extension
  with alternate storage models, e.g., an external persistent disk-based
  store.")
  (:method ((ctrie ctrie) old new)
    (cas (ctrie-root ctrie) old new)))


(defstruct rdcss-descriptor 
  "An RDCSS-DESCRIPTOR object represents a 'plan' for a proposed RDCSS
  (restricted double compare single swap) operation. The use of this
  descriptor object provides the means to effect an atomic RDCSS in
  software, requiring only hardware support for single CAS, as that is
  what is commonly available on curent consumer hardware.
   - OV         designates a slot containing the OLD (current) root inode.
                If the swap is unsuccessful, the resulting ctrie will revert
                to this structure as the root inode. 
   - OVMAIN     designates a slot containing the CNODE that is referenced
                by the OLD (current) root inode.
   - NV         designates a slot containing a fully assembled replacement
                root inode referencing a valid CNODE. This pair will become
                the root inode and level-0 main-node of the ctrie if the
                swap is successful.
   - COMMITTED  designates a flag which, when not NIL, indicates that the
                RDCSS plan defined by this descriptor has completed
                successfully."
  ov ovmain nv committed)


(defun/inline root-node-access (ctrie &optional abort)
  "ROOT-NODE-ACCESS extends {defgeneric cl-ctrie::FIND-CTRIE-ROOT},
  implementing the RDCSS root node api for access to root inode of
  CTRIE.  In particular, it ensures that if, instead of an inode, the
  root of CTRIE contains an RDCSS descriptor of a proposed root-node
  update, that it will immediately invoke {defun
  cl-ctrie::ROOT-NODE-COMMIT} to act on that descriptor and return an
  INODE struct that is the result of the completed commit process.
  ROOT-NODE-ACCESS only provides access to the root inode
  _structure_. It does not provide safe access to the _contents_ of
  that inode itself. In order to access those contents, the root inode
  returned by ROOT-NODE-ACCESS must be further processed by {defun
  cl-ctrie::INODE-READ} in order to still properly comply with the
  undrlying GCAS protocol implementation for inodes."
  (sb-thread:barrier (:read)
    (atypecase (find-ctrie-root ctrie)
      (inode it)
      (t (root-node-commit ctrie abort)))))


(defun/inline root-node-replace (ctrie ov ovmain nv)
  "rdcss api for replacement of root ctrie inode"
  (let1 desc (make-rdcss-descriptor :ov ov :ovmain ovmain :nv nv)
    (if (cas (find-ctrie-root ctrie) ov desc)
      (prog2 (root-node-commit ctrie nil)
        (rdcss-descriptor-committed desc))
      nil)))


(defun root-node-commit (ctrie &optional abort)
  "rdcss api to complete a root-node transaction"
  (sb-thread:barrier (:read)
    (atypecase (find-ctrie-root ctrie)
      (inode it)
      (rdcss-descriptor (if abort
                          (if (cas (find-ctrie-root ctrie) it (rdcss-descriptor-ov it))
                            (rdcss-descriptor-ov it)
                            (root-node-commit ctrie abort))
                          (let1 oldmain (inode-read (rdcss-descriptor-ov it))
                            (if (eq oldmain (rdcss-descriptor-ovmain it))
                              (if (cas (find-ctrie-root ctrie) it (rdcss-descriptor-nv it))
                                (prog1 (rdcss-descriptor-nv it)
                                  (setf (rdcss-descriptor-committed it) t))
                                (root-node-commit ctrie abort))
                              (if (cas (find-ctrie-root ctrie) it (rdcss-descriptor-ov it))
                                (rdcss-descriptor-ov it)
                                (root-node-commit ctrie abort)))))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feature impl. using RDCSS root-node protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ctrie-snapshot (ctrie &key read-only)
  ""
  (loop until
    (let (root main)
      (sb-thread:barrier (:read)
        (setf root (root-node-access ctrie))
        (setf main (inode-read root))
        (when (root-node-replace ctrie root main (make-inode main (gensym "ctrie")))
          (return-from ctrie-snapshot
            (if read-only
              (make-ctrie :readonly-p t :root root)
              (make-ctrie :root (make-inode main (gensym "ctrie"))))))))))


(defun ctrie-clear (ctrie)
  ""
  (loop until
    (let (root main)
      (sb-thread:barrier (:read)
        (setf root (root-node-access ctrie))
        (setf main (inode-read root))
        (when (root-node-replace ctrie root main (make-inode (make-cnode) (gensym "ctrie")))
          (return-from ctrie-clear ctrie))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cas-ctrie-root / This Operation Appears to have changed in recently published
;; descriptions of the ctrie which change the protocol to eliminate this
;; possibility by starting with a fully initialized level 0 cnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Todo: Deprecate
#+()
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
               (inode  (if (null (inode-read current))
                         (if (eq current (cas (ctrie-root ctrie) current new-value))
                            (return-from cas-ctrie-root current)
                           (try n))))))))
    (try 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT/INSERT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %insert (inode key value level parent startgen)
  (atypecase (inode-read inode)
    (tnode (throw :restart (clean parent (- level 5))))          
    (lnode (or (inode-mutate inode it
                 (lnode-inserted it key value #'ctequal))
             (throw :restart it)))
    (cnode (let* ((cnode it)
                   (bmp  (cnode-bitmap cnode))
                   (flag (flag key level))
                   (pos  (flag-arc-position flag bmp)))
             (if (not (flag-present-p flag bmp))
               (let ((new-cnode (cnode-extended cnode flag pos (snode key value))))
                 (if (inode-mutate inode cnode new-cnode)
                   (return-from %insert value)
                   (throw :restart cnode)))
               (let ((present (svref (cnode-arcs cnode) pos))) 
                 (etypecase present
                   (inode (if (eq startgen (inode-gen present))
                            (return-from %insert
                              (%insert present key value (+ level 5) inode startgen))
                            (if (inode-mutate inode it (refresh it startgen))
                              (%insert inode key value level parent startgen)
                              (throw :restart startgen))))                            
                   (snode (if (ctequal key (snode-key present))
                            (let ((new-cnode (cnode-updated cnode pos (snode key value))))
                              (if (inode-mutate inode cnode new-cnode)
                                (return-from %insert value)
                                (throw :restart present)))
                            (let* ((new-flag-this (flag key (+ level 5)))
                                    (new-flag-other (flag (snode-key present) (+ level 5)))
                                    (new-bitmap (logior new-flag-this new-flag-other))
                                    (new-cnode (make-cnode new-bitmap))
                                    (new-inode (make-inode new-cnode startgen)))
                              (setf
                                (svref (cnode-arcs new-cnode) (flag-arc-position new-flag-this new-bitmap))
                                (snode key value))
                              (setf
                                (svref (cnode-arcs new-cnode) (flag-arc-position new-flag-other new-bitmap))
                                present)
                              (if (inode-mutate inode cnode (cnode-updated cnode pos new-inode))
                                (return-from %insert value)
                                (throw :restart present))))))))))))


(defun ctrie-put (ctrie key value)
  (check-type ctrie ctrie)
    (with-ctrie ctrie 
      (loop  with d = 0 and p = nil 
        for  root =  (root-node-access *ctrie*)
        when (catch-case (%insert root key value d p (inode-gen root))
               (:restart  (when *debug*
                            (format t  "~8A  timeout insert (~A . ~A)~%" it  key value)))
               (t       (prog1 it
                          (when *debug*
                            (format t  "~8S  done .~%~S~%" it *ctrie*)))))
        return it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET/LOOKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %lookup (inode key level parent startgen)
  (let1 node (inode-read inode)
    (typecase node
      (tnode (throw :restart (clean parent (- level 5))))
      (lnode (or (lnode-search node key #'ctequal)
               (throw :notfound node)))
      (cnode (let* ((cnode node)
                     (bmp  (cnode-bitmap cnode))
                     (flag (flag key level))
                     (pos  (flag-arc-position flag bmp)))
               (if (not (flag-present-p flag bmp))
                 (throw :notfound cnode)
                 (let ((found (svref (cnode-arcs cnode) pos)))
                   (typecase found
                     ((or snode tnode)
                       (if (ctequal key (leaf-node-key found))
                         (return-from %lookup (values (leaf-node-value found) t))
                         (throw :notfound found)))
                     (inode (if (eq startgen (inode-gen found))
                              (return-from %lookup (%lookup found key (+ level 5) inode startgen))
                              (if (inode-mutate inode node (refresh node startgen))
                                (%lookup inode key level parent startgen)
                                (throw :restart startgen))))))))))))


(defun ctrie-get (ctrie key)
  (check-type ctrie ctrie)
    (with-ctrie ctrie
      (flet ((attempt-get (root key level parent gen try)
               (catch-case (%lookup root key level parent gen)
                 (:notfound  (return-from ctrie-get (values nil nil)))
                 (:restart   (multiple-value-prog1 (values it nil)
                               (when *debug* (format t  "~8s  restarting after try ~d.~%" it try))))
                 (t          (multiple-value-prog1 (values it t)
                               (when *debug* (format t  "~8s  done on try ~d.~%" it try)))))))
        (loop with vals with d = 0 and p = nil
          for  r = (root-node-access ctrie) for try from 1
          do   (setf vals (multiple-value-list (attempt-get r key d p (inode-gen r) try)))
          when (> try 1000)  do (error "try ~d" try)
          when (second vals) do (return-from ctrie-get (values (first vals) t))))))


(defun (setf ctrie-get) (value ctrie key)
    (multiple-value-prog1 (values value key ctrie)
      (ctrie-put ctrie key value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DROP/REMOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         
(defun %remove (inode key level parent startgen)
  (atypecase (inode-read inode)
    (tnode (values (clean parent (- level 5)) :restart))
    (lnode (let* ((new-lnode      (lnode-removed it key #'ctequal))
                   (groomed-lnode (if (null (lnode-next new-lnode))
                                    (entomb new-lnode) new-lnode))
                   (leaf-value    (%lookup inode key level parent startgen)))
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
                     (inode
                       (if (eq startgen (inode-gen found))
                         (return-from %remove (%remove found key (+ level 5) inode startgen))
                         (if (inode-mutate inode it (refresh it startgen))
                           (%remove inode key level parent startgen)
                           (throw :restart startgen))))
                     ((or snode tnode)
                       (if (ctequal key (leaf-node-key found))
                         (let1 new-cnode (cnode-truncated it flag pos)
                           (if (inode-mutate inode it (cnode-contracted new-cnode level))
                             (values (leaf-node-value found) t)
                             (values nil :restart)))
                         (values nil :notfound))))
                   (case result-kind
                     (:restart  (values result-value :restart))
                     (:notfound (values nil nil))
                     (t         (multiple-value-prog1 (values result-value t)                       
                                  (when (tnode-p (inode-read inode))
                                    (clean-parent parent inode key (- level 5)))))))))))))

  
(defun ctrie-drop (ctrie key)
  "Remove KEY and it's value from the CTRIE."
  (check-type ctrie ctrie)
    (with-ctrie ctrie
      (let1 r (root-node-access ctrie)
        (multiple-value-bind (val kind) (%remove r key 0 nil (inode-gen r))
          (case kind
            (:restart  (return-from ctrie-drop (ctrie-drop ctrie key)))
            (:notfound (values val nil))
            (t         (values val t)))))))

                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-MAP & Friends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-node (node fn))

(defun ctrie-map (ctrie fn &key atomic &aux accum)
  (declare (special accum))
  (check-type ctrie ctrie)
  (with-ctrie (if atomic (ctrie-snapshot ctrie :read-only t) ctrie)
    (let ((root (root-node-access *ctrie*)))
      (map-node root fn))
    accum))

(defmacro ctrie-do ((key value ctrie &key atomic) &body body)
  "Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t \"~&~8S => ~10S~%\" k v))"
  `(ctrie-map ,ctrie #'(lambda (,key ,value) ,@body)
     :atomic ,atomic))

(defmethod map-node ((node inode) fn)
  (map-node (inode-read node) fn))

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

(defun ctrie-map-keys (ctrie fn &key atomic)
  (ctrie-map ctrie
    (lambda (k v) (declare (ignore v))
      (funcall fn k)) :atomic atomic))

(defun ctrie-map-values (ctrie fn &key atomic)
  (ctrie-map ctrie
    (lambda (k v) (declare (ignore k))
      (funcall fn v)) :atomic atomic))

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

(defun ctrie-keys (ctrie &key atomic)
  (ctrie-map ctrie #'collect-keys :atomic atomic))

(defun ctrie-values (ctrie &key atomic)
  (ctrie-map ctrie #'collect-values :atomic atomic))

(defun ctrie-size (ctrie &aux (accum 0))
  (ctrie-map-keys ctrie
    (lambda (x) (declare (ignore x))
      (incf accum)))
  accum)

(defun ctrie-empty-p (ctrie)
  (check-type ctrie ctrie)
  (with-ctrie ctrie
    (= 0 (cnode-bitmap (inode-read (root-node-access ctrie))))))

(defun ctrie-ensure-get (ctrie key default)
  "Like CTRIE-GET, but if KEY is not found in CTRIE, automatically adds 
   the entry (KEY. DEFAULT). Secondary return value is true if key was
   already in the table."
  (multiple-value-bind (val ok) (ctrie-get ctrie key)
      (if ok
        (values val t)
        (values (ctrie-put ctrie key default) nil))))

(defun ctrie-to-alist (ctrie &key atomic)
  (ctrie-map ctrie #'collect2 :atomic atomic))

(defun ctrie-pprint (ctrie &optional (stream t))
  (pprint-tabular stream (ctrie-to-alist ctrie)))

(defun ctrie-to-hashtable (ctrie &key atomic)
  (alexandria:alist-hash-table
    (ctrie-map ctrie #'collect2 :atomic atomic)))

(defun ctrie-from-alist (alist)
  (let ((ctrie (make-ctrie)))
    (prog1 ctrie
      (mapc (lambda (pair) (ctrie-put ctrie (car pair) (cdr pair)))
        alist))))

(defun ctrie-from-hashtable (hashtable)
  "create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE"
  (let ((ctrie (make-ctrie :test (hash-table-test hashtable))))
    (prog1 ctrie (maphash (lambda (k v) (ctrie-put ctrie k v))
                   hashtable))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic CTRIE I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (find-package :cl-store)
    (pushnew :cl-store *features*)))

(defgeneric ctrie-save (ctrie place &key &allow-other-keys)
  (:documentation ""))

#+cl-store
(defmethod  ctrie-save ((ctrie ctrie) (place pathname) &key)
  (prog1 place (cl-store:store (ctrie-snapshot ctrie) place)))


(defgeneric ctrie-load (place &key &allow-other-keys))

#+cl-store
(defmethod  ctrie-load ((place pathname) &key)
  (cl-store:restore place))

(defgeneric ctrie-export (ctrie place &key &allow-other-keys)
  (:documentation ""))

#+cl-store 
(defmethod  ctrie-export ((ctrie ctrie) (place pathname) &key)
  (prog1 place (cl-store:store (ctrie-to-alist ctrie) place)))

(defmethod  ctrie-export ((ctrie ctrie) (place hash-table) &key)
  (ctrie-map ctrie (lambda (k v) (setf (gethash place k) v))))

(defgeneric ctrie-import (place &key &allow-other-keys)
  (:documentation ""))

#+cl-store
(defmethod  ctrie-import ((place pathname) &key)
  (ctrie-from-alist (cl-store:restore place)))

(defmethod  ctrie-import ((place hash-table) &key)
  (ctrie-from-hashtable place))
