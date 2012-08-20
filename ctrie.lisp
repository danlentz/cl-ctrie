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
  (:documentation "Condition for when an operational failure or
  inconsistency has occurred."))


(define-condition ctrie-modification-failed  (ctrie-operational-error)
  ((place   :initarg :place  :reader place-of)
    (reason :initarg :reason :reader reason-of))
  (:report
    (lambda (condition stream)
      (with-slots (ctrie op place reason) condition
        (format stream "CTRIE MODIFICATION FAILED~%")
        (format stream "-------------------------~%~%")
        (format stream "FAILURE: Operation ~S failed to modify ~S in CTRIE at #x~X~%~%"
          op place (if ctrie (sb-kernel:get-lisp-obj-address ctrie) 0))
        (format stream "CAUSE:   ~A~%~%" reason)
        (format stream "ABOUT:   ~A~%~%"
          (documentation (type-of condition) 'type)))))
  (:documentation
        "This condition indicates an unhandled failure of an attempt to
         perform stateful modification to CTRIE.  The most common case in
         which this might occur is when such an attempt is mode on a CTRIE
         designated as READONLY-P.  In any case, this condition represents an
         exception from which processing cannot continue and requires
         interactive user intervention in order to recover."))


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


(defmacro ctrie-error (condition &rest args)
  "Signal a CTRIE related condition."
  `(error ',condition :ctrie *ctrie* ,@args))


(defun ctrie-modification-failed (reason &key op place)
  "Signal a modification failure with the appropriate attendant metadata."
  (ctrie-error ctrie-modification-failed :op op :place place :reason reason))


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


(defun ctstamp ()
  (when t ;;*debug*
    (if *ctrie*
      (funcall (ctrie-stamp *ctrie*))
      (local-time:now))))


(defun flag (key level)
  "For a given depth, LEVEL, within a CTRIE, extract the correspondant
  sequence of bits from the computed hash of KEY that indicate the
  logical index of the arc on the path to which that key may be found.
  Note that the logical index of the arc is most likely not the same
  as the physical index where it is actually located -- for that see
  `FLAG-ARC-POSITION`"
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
  `#*10010000000000000000000000000000` one can easily see that the first
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
  "Atomically Stamped Reference structure _[Herlithy, TAOMP]_ that
  encapsulates the mutable slots within an inode. Any specific `REF`
  structure is, itself, never mutated.  Using the `REF` structure
  as basis of inode implementation provides the capability to 'bundle'
  additional metadata in an inode while still providing atomic compare
  and swap using a single comparison of the aggregate `REF` instance.
   - `STAMP` defines a slot containing implementation-specific metadata
     that may be maintained internally as a means of tracking inode
     modification and update behavior.  It should not be referenced by
     user code, and the format of its contents should not be relied apon.
   - `VALUE` defines a slot that contains a reference to the MAIN-NODE
     that the enclosing inode should be interpreted as 'pointing to'
   - `PREV` defines a slot which, during the `INODE-COMMIT` phase of the
     _GCAS INODE PROTOCOL_ maintains a reference to the last valid
     inode state, which may be restored, if necessary, during the
     course of the `INODE-READ` / `INODE-COMMIT` arbitration process"
  (stamp (ctstamp)        :read-only t)
  (value t   :type t      :read-only t)
  (prev  nil :type t))


(defstruct (failed-ref
             (:include ref)
             (:copier nil))
  "A `FAILED-REF` is a structure that is used to preserve the linkage to
  prior inode state following a failed GCAS.  Any inode access that
  detects a `FAILED-REF` will immediately invoke a commit to restore the
  inode to the state recorded in `FAILED-REF-PREV`")


(defstruct (inode
             (:constructor %make-inode)
             (:copier nil))
  "An INODE, or 'Indirection Node' is the mutable structure
  representing the link between other 'main-node' structures found in
  a ctrie.  An inode is the only type of node that may change the
  value of its content during its lifetime.  In this implementation,
  all such values as may change are encapsulated within a `REF`
  substructure.  Each inode also contains a generational descriptor
  object, comparible by identity only, which is used to identify the
  state of synchronization between the inode and the current
  'generation' indicated by the root inode at the base of the
  CTRIE. As an inode may not change its 'gen identity' during its
  lifetime, this disparity with the generation of the root node will
  immediately result in the replacement of the inode with a new one
  properly synchronized with the root's `GEN` object. In this
  implementation, `GEN` objects are implemented by GENSYMS -- unique,
  uninterned symbols which inherently provide very similar symantics
  to those required of the generational descriptor.
  - `GEN` defines a slot containing a generational descriptor object
  - `REF` defines a slot containing a `REF` struct that encapsulates
    the mutable content within an INODE"
  (gen nil :read-only t)
  (ref nil))


(defun make-inode (link-to &optional gen stamp prev)
  "Construct a new INODE that represents a reference to the value
  provided by argument LINK-TO, optionally augmented with a specified
  generational descriptor, timestamp, and/or previous state"
  (%make-inode
    :gen (or gen (gensym "ctrie"))
    :ref (make-ref :value link-to :stamp (or stamp (ctstamp)) :prev prev)))


(defmethod print-object ((o inode) stream)
  "Print a helpful representation of an INODE that can be easily
  understood by quick visual inspection. This should only be used
  during development, however, as it is not compliant with the
  standard common-lisp specification for *PRINT-READABLY*. A
  trivial example of the printed representation might appear as
  follows:
  ;;;
  ;;;    #S(CTRIE
  ;;;       :READONLY-P NIL
  ;;;       :TEST EQUAL
  ;;;       :HASH SXHASH
  ;;;       :ROOT #<INODE ctrie654349 @2012-08-09T19:31:01.686562-04:00
  ;;;               (HAS-PREV? NIL)
  ;;;               :=> #S(CNODE
  ;;;                      :BITMAP 67108864
  ;;;                      :FLAGS #*00000000000000000000000000100000
  ;;;                      :ARCS #(#S(SNODE :KEY 1 :VALUE 2)))>)
  ;;;"
  ;; (if (ref-p (inode-ref o))
  ;;   (progn
  ;;   (print-unreadable-object (o stream)
  ;;     (format stream
  ;;       "~a ~a ~s~%           ~C~A ~A~C~%           ~s ~s" 
  ;;       :inode (inode-gen o) (ref-stamp (inode-ref o))        
  ;;       #\( :has-prev? (not (null (ref-prev (inode-ref o)))) #\)
  ;;       :=> (ref-value (inode-ref o))))
  ;;     (terpri stream))
  (call-next-method))
;;)


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
       (if *debug* (local-time:timestamp= (ref-stamp ref) ,expected-stamp) t)
       (eql (ref-value ref) ,expected)
       (eq  (sb-ext:compare-and-swap (inode-ref ,obj) ref
              (make-ref :value ,new :stamp ,new-stamp :prev ,prev))
         ref))))


(defun/inline INODE-READ (inode)
  "INODE-READ provides the top-level interface to the inode _GCAS ACCESS_
  api, which is the mechanism which must be used to gain access to the
  content of any NON-ROOT inode. For access to the root inode, refer
  to the RDCSS inode api `ROOT-NODE-ACCESS`. Returns as four values,
  the MAIN-NODE, the STAMP, the PREVIOUS STATE (if any), and the REF
  structure encapsulated by the inode."
  (let (ref)
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
                                    result)))))))


(defun/inline INODE-MUTATE (inode old-value new-value)
  "INODE-MUTATE provides the top-level interface to the inode _GCAS
  MODIFICATION_ api, which is the mechanism which must be used to
  effect any change in a NON-ROOT inode.  For modification of the
  root-inode, refer to the `ROOT-NODE-REPLACE` _RDCSS ROOT NODE
  PROTOCOL_ Returns a boolean value which indicates the success or
  failure of the modification attempt."
  (when (ctrie-readonly-p *ctrie*)
    (ctrie-modification-failed "This CTRIE is READ-ONLY"
      :op 'inode-mutate :place (describe-object inode nil)))
  (multiple-value-bind (val stamp prev ref) (inode-read inode)
    (declare (ignore val prev ref))
    (if (gcas-compare-and-set inode
          old-value new-value stamp (ctstamp) old-value)
      (return-from inode-mutate
        (null (ref-prev (inode-commit inode (inode-ref inode)))))
      (return-from inode-mutate
        nil))))


(defun INODE-COMMIT (inode ref)
  "INODE-COMMIT implements the _GCAS COMMIT_ protocol which is invoked
  as necessary by the `INODE-READ` and `INODE-MUTATE` entry-points.  It is
  not meant to be invoked directly, as this would most likely result
  in corruption. Returns the `REF` structure representing the content of
  whatever root inode wound up successfully committed -- either the
  one requested, or one represented by a previous valid state.  In order
  to coexist with the _RDCSS ROOT NODE PROTOCOL_ this GCAS COMMIT
  implementation is augmented with RDCSS ABORTABLE READ semantics
  by a forward reference to a RDCSS-aware `ROOT-NODE-ACCESS` in order
  to safely compare INODE's generational descriptor with the one found
  in the root inode of the subject CTRIE."
  (when (ctrie-readonly-p *ctrie*)
    (ctrie-modification-failed "This CTRIE is READ-ONLY"
      :op 'inode-commit :place (describe-object inode nil)))
  (flet ((ABORTABLE-READ (ctrie)
           (root-node-access ctrie t)))
    (let1 prev (ref-prev ref)
      (typecase prev
        (null        (return-from inode-commit ref))
        (failed-ref  (if (cas (inode-ref inode) ref (failed-ref-prev prev))
                       (return-from inode-commit
                         (failed-ref-prev prev))
                       (return-from inode-commit
                         (inode-commit inode (inode-ref inode)))))
        (t            (if (and (not (ctrie-readonly-p *ctrie*))
                            (eql (inode-gen (ABORTABLE-READ *ctrie*))
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
                            (inode-commit inode (inode-ref inode))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (snode
             (:copier nil))
  "SNODE, i.e., 'Storage Node', is the LEAF-NODE structure ultimately
  used for the storage of each key/value pair contained in the CTRIE.
  An SNODE is considered to be immutable during its lifetime.
   - `KEY` defines the slot containing an element of the map's domain.
   - `VALUE` defines the slot containing the range-element mapped to `KEY`"
  (key   nil  :read-only t)
  (value nil  :read-only t))


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
  unique by the `CTRIE-TEST` function defined for that ctrie.  An
  LNODE (and therefore a chain of LNODEs) is considered to be
  immutable during its lifetime.  The order of the list is
  implemented (arbitrarily) as most recently added first, analogous to
  `CL:PUSH`
   - `ELT` defines the slot containing an enclosed SNODE
   - `NEXT` defines a slot referencing the next LNODE in the chain, or
     `NIL` if no further LNODES remain."
  (elt  nil :read-only t) 
  (next nil :read-only t))


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
  with ORIG-LNODE, but ensured to contain an LNODE enclosing an SNODE mapping
  KEY to VALUE.  If the given KEY equal to a key already present somewhere
  in the chain (as compared with equality predicate TEST) it will be
  replaced.  Otherwise a new LNODE will be added. In either case, the LNODE
  containing `(SNODE KEY VAlUE)` will be the first node in the resulting
  list"
  (let1 elts (loop for lnode = orig-lnode then (lnode-next lnode) while lnode
               unless (funcall test key (leaf-node-key (lnode-elt lnode)))
               collect (lnode-elt lnode))
    (apply #'enlist (push (snode key value) elts))))


(defun lnode-search (lnode key test)
  "Within the list of lnodes beginning with LNODE, return the range value
  mapped by the first SNODE containing a key equal to KEY as determined
  by equality predicate TEST, or `NIL` if no such key is found.  As a
  second value, in order to support storage of `NIL` as a key, return `T` to
  indicate that the KEY was indeed found during search, or `NIL` to indicate
  that no such key was present in the list"
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
  "A TNODE, or 'Tomb Node', is a special node structure used to preserve
  ordering during `CTRIE-DROP` (`%remove`) operations.
  Any time a TNODE is encountered during the course of a `CTRIE-GET` (`%lookup`)
  operation, the operative thread is required to invoke a `CLEAN` operation
  on the TNODE it has encountered and throw to `:RESTART` its lookup activity
  over again.  A TNODE is considered to be immutable and may not change its
  value during its lifetime.
   - `CELL` defines a slot which contains the entombed node structure.
      Only LNODE and SNODE type nodes are ever entombed"
  (cell nil :type (or snode lnode) :read-only t))


(defgeneric entomb (node)
  (:documentation "Return a newly constructed TNODE enclosing the argument
  LEAF-NODE structure `NODE`"))

(defmethod entomb (node)
  "Unless the provided argument is of a type for which an entombment
    specialization has been defined, signal an error, as we have arrived
    at an undefined state and cannot continue processing."
  (error "Entombment of ~A (type ~S) is undefined." node (type-of node)))

(defmethod entomb ((lnode lnode))
  "Entomb an LNODE in a newly created TNODE"
  (make-tnode :cell lnode))

(defmethod entomb ((snode snode))
  "Entomb an SNODE in a newly created TNODE"
  (make-tnode :cell snode))


(defgeneric resurrect (node)
  (:documentation "Return the 'resurection' of the NODE argument.  The
  resurrection of an INODE that references a TNODE is the
  LEAF-NODE structure entombed by that TNODE.  The resurrection of
  any other node is simply itself.")
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
  "A CNODE, or 'Ctrie Node' is a MAIN-NODE containing a vector of up
  to `2^W` 'arcs' -- i.e., references to either an SNODE or INODE
  structure, collectively referred to as `BRANCH-NODES.` Each CNODE
  also contains a (fixnum) bitmap that describes the layout of which
  logical indices within the total capacity of `2^W` arcs are actually
  allocated within that node with BRANCH-NODES physically present.
  For more specific details on these BITMAP and ARC-VECTOR
  constituents, refer to the following related functions: `FLAG`
  `FLAG-PRESENT-P` `FLAG-VECTOR` and `FLAG-ARC-POSITION. The CNODE
  structure is considered to be immutable and arcs may not be added or
  removed during its lifetime.  The storage allocated within a CNODE
  is fixed and specified at the time of its creation based on the
  value of BITMAP during initialization
   - `BITMAP`
   - `ARCS` "
  (bitmap 0           :read-only t)
  (arcs   %empty-map% :read-only t))


(defun make-cnode (&optional (bitmap 0))
  "Construct a CNODE with internal storage allocated for the number of
  arcs equal to the Hamming-Weight of the supplied BITMAP parameter.
  If no BITMAP is provided, the CNODE created will be empty -- a state
  which is only valid for the level 0 node referenced by the root of
  the CTRIE.  This constructor is otherwise never called directly, but
  is invoked during the course of higher-level operations such as
  `CNODE-EXTENDED` `CNODE-UPDATED` `CNODE-TRUNCATED` and `MAP-CNODE`"
  (%make-cnode
    :bitmap bitmap
    :arcs  (make-array (logcount bitmap))))


(defun cnode-extended (cnode flag position new-arc)
  "Construct a new cnode structure that is exactly like CNODE, but
  additionally contains the BRANCH-NODE specified by parameter NEW-ARC
  and logical index FLAG at the physical index POSITION within its
  vector of allocated arcs.  The BITMAP of this new CNODE will be
  calculated as an adjustment of the prior CNODE's BITMAP to reflect
  the presence of this additional arc.  In addition, the physical
  index within the extended storage vector for the other arcs present
  may also change with respect to where they were located in the prior
  CNODE.  In other words, the physical index of a given arc within the
  compressed CNODE storage vector should never be relied upon
  directly, it should always be accessed by calculation based on its
  LOGICAL index and the current CNODE BITMAP as described in more
  detail by the documentation for the functions `FLAG` `FLAG-VECTOR` and
  `FLAG-ARC-POSITION`"
  (let* ((new-bitmap (logior flag (cnode-bitmap cnode)))
          (new-cnode  (make-cnode new-bitmap)))
    (prog1 new-cnode
      (map-into (cnode-arcs new-cnode) #'identity (cnode-arcs cnode))
      (setf (svref (cnode-arcs new-cnode) position) new-arc)
      (unless (> position (length (cnode-arcs cnode)))
        (replace (cnode-arcs new-cnode) (cnode-arcs cnode)
          :start1 (+ position 1) :start2 position)))))


(defun cnode-updated (cnode position replacement-arc)
  "Construct a new cnode structure identical to CNODE, but having the
  BRANCH-NODE physically located at POSITION within the storage
  vector replaced by the one specified by REPLACEMENT-ARC.  Unlike
  `CNODE-EXTENDED` and `CNODE-TRUNCATED` the allocated storage and
  existing BITMAP of this CNODE will remain unchanged (as this is
  simply a one-for-one replacement) and correspondingly, no reordering
  of other nodes within the storage vector will occur"
  (let ((new-cnode (make-cnode (cnode-bitmap cnode))))
    (prog1 new-cnode
      (replace (cnode-arcs new-cnode) (cnode-arcs cnode))
      (setf (svref (cnode-arcs new-cnode) position) replacement-arc))))


(defun cnode-truncated (cnode flag pos)
  "Construct a new cnode structure that is exactly like CNODE, but
 with the arc at logical index FLAG and physical storage vector
 location POS removed.  The new CNODE will have an updated bitmap
 value that is adusted to reflect the removal of this arc, and the
 position of other arcs within the storage vector of the new CNODE
 will be adjusted in a manner analogous to that of `CNODE-EXTENDED`
 More details on this process may be found by referring to the
 documentation for the functions `FLAG` `FLAG-VECTOR` and
 `FLAG-ARC-POSITION`"
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
  "Construct a new cnode structure that is exactly like CNODE, but
  with each arc (BRANCH-NODE) present in CNODE replaced by the result
  of applying FN to that arc.  I.e., a simple functional mapping from
  the old CNODE by FN.  As with `CNODE-UPDATED` the allocated storage and
  BITMAP of the resulting CNODE will remain unchanged from the
  original, and no physical reordering of nodes within the storage
  vector will occur"
  (check-type fn function)
  (check-type cnode cnode)
  (aprog1 (make-cnode (cnode-bitmap cnode))
    (map-into (cnode-arcs it) fn (cnode-arcs cnode))))


(defgeneric refresh (place gen)
  (:documentation "Reconcile the node specified by PLACE with an
  updated generational descriptor object, GEN. The actions required
  for this reconciliation vary according to the node type and 
  specializations are defined on a casewise basis."))


(defmethod refresh ((cnode cnode) gen)
  "Return a new cnode structure identical to CNODE, but with any
    arcs that are INODES refreshed to generational descriptor GEN"
  (map-cnode (lambda (arc) (refresh arc gen)) cnode))


(defmethod refresh ((inode inode) gen)
  "Generate a replacement for inode that continues to reference the
    same MAIN-NODE as before, but otherwise contains the new generational
    descriptor GEN, and a new REF substructure initialized
    with freshly generated metadata, unconditionally discarding the old.
    Note that the refresh of an inode is not transitive to the nodes contained
    in the portion of the CTRIE that it references.  I.e., the process does
    not eagerly descend and propagate until needed, eliminating the
    overhead incurred by full traversals which, in many situations, turn out
    to be not even necessary."    
  (multiple-value-bind (val stamp) (inode-read inode)
    (declare (ignore stamp))
    (make-inode val gen (ctstamp))))


(defmethod refresh ((snode snode) gen)
  "An SNODE represents a LEAF storage cell and does not require
    any coordination with generational descriptors, and so is simply
    returned as-is. "
  snode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arc Retraction Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun/inline cnode-contracted (cnode level)
  "The _CONTRACTION_ of a CNODE is an ajustment performed when a CNODE
  at depth other than level 0 contains only a single SNODE arc.  In
  such a case, that SNODE is entombed in a new TNODE, which is
  returned as the result of the CNODE contraction. In all other cases
  the CNODE is simply returned as-is.  A CONTRACTION represents the
  first of the two-step _ARC RETRACTION PROTOCOL_ that effects the reclaimation
  of allocated storage no longer used and the optimization of of lookup
  efficiency by compacting CTRIE depth and thereby the number of levels
  which must be traversed.  For further information, refer to the function
  `CNODE-COMPRESSED` which implements the second stage of this protocol,
  completing the process."
  (let1 arcs (cnode-arcs cnode)
    (if (and (plusp level) (eql 1 (logcount (cnode-bitmap cnode))))
      (atypecase (svref arcs 0)
        (snode (entomb it))
        (t     cnode))
      cnode)))


(defun/inline cnode-compressed (cnode level)
  "The _COMPRESSION_ of a CNODE is the second step of the _ARC
  RETRACTION PROTOCOL_ completing a retraction that has been initiated
  by `CNODE-CONTRACTED`.  The CNODE compression is computed by
  generating a replacement cnode structure that is similar to CNODE,
  but with any entombed inode arcs created during contraction simply
  replaced by the SNODE that had been entombed. This is called the
  _RESURRECTION_ of that SNODE. After all entombed inode arcs of a
  cnode have been collapsed into simple SNODE leaves, if the resulting
  CNODE has been compressed so far as to contain only a single SNODE
  leaf, it is subjected to another CONTRACTION before it is returned
  as the result of the compression. Otherwise it is simply returned
  and represents a complete iteration of the _ARC RETRACTION PROTOCOL_"
  (check-type cnode cnode)
  (cnode-contracted (map-cnode #'resurrect cnode) level))


(defun/inline clean (inode level)
  "CLEAN is the basic entry-point into the arc retraction protocol. Given an
  arbitrary, non-root inode referencing a CNODE that can be compressed,
  update that inode to reference the result of that compression.  Otherwise
  INODE will remain unaffected."
  (check-type inode inode)
  (check-type level fixnum)
  (loop until
    (let1 node (inode-read inode)
      (if (cnode-p node)
        (inode-mutate inode node (cnode-compressed node level))
        t))))


(defun clean-parent (parent-inode target-inode key level)
  "During a `CTRIE-DROP` (`%remove`) operation, if the result of a KEY/VALUE
  removal is an arc consisting of an `ENTOMBED` inode (one referencing a TNODE), then,
  if that arc remains accessible from the parent of a CNODE containing it, generate
  the compression of that CNODE and update its parent INODE with the result."
  (check-type parent-inode (or null inode))
  (check-type target-inode inode)
  (check-type level        fixnum)
  (let1 target-gen (inode-gen target-inode)
    (if (not (eq target-gen (inode-gen (root-node-access *ctrie*))))
      (throw :restart target-gen)
      (loop while
        (let ((parent-ref  (inode-read parent-inode))
               (target-ref (inode-read target-inode)))
          (when (cnode-p parent-ref)
            (let* ((bmp   (cnode-bitmap parent-ref))
                    (flag (flag key level))
                    (pos  (flag-arc-position flag bmp)))
              (when (and (flag-present-p flag bmp)
                      (tnode-p target-ref)
                      (eq target-inode (svref (cnode-arcs parent-ref) pos)))
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

(defun leaf-node-p (thing)
  (subtypep (type-of thing) 'leaf-node))
    
(deftype branch-node ()
  "a BRANCH-NODE represents a single arc typically contained
  within A CNODE."
  `(or inode snode))

(defun branch-node-p (thing)
  (subtypep (type-of thing) 'branch-node))

(deftype main-node ()
  "A MAIN-NODE is a node that typically represents a specific
  depth or level within the ctrie and is referenced
  by its own unique inode."
  `(or cnode lnode tnode))

(defun main-node-p (thing)
  (subtypep (type-of thing) 'main-node))


(defgeneric leaf-node-key (resource)
  (:documentation "Return the KEY contained in a node that may be
  either an SNODE or entombed SNODE, regardless of which kind it
  happens to be")
  (:method ((snode snode))
    (snode-key snode))
  (:method ((tnode tnode))
    (leaf-node-key (tnode-cell tnode))))


(defgeneric leaf-node-value (resource)
  (:documentation "Return the VALUE contained in a node that may be
  either an SNODE or entombed SNODE, regardless of which kind it
  happens to be")
  (:method ((snode snode))
    (snode-value snode))
  (:method ((tnode tnode))
    (leaf-node-value (tnode-cell tnode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE Root Container
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (ctrie (:constructor %make-ctrie))
  "A CTRIE structure is the root container that uniquely identifies a CTRIE
  instance, and  contains the following perameters which specify the
  definable aspects of each CTRIE:
   - `READONLY-P` if not `NIL` prohibits any future modification or
  cloning of this instance.
   - `TEST` is a designator for an equality predicate that will be
  applied to disambiguate and determine the equality of any two
  keys. It is recommened that this value be a symbol that is fboundp,
  to retain capability of externalization (save/restore). At present,
  though, this is not enforced and a function object or lambda
  expression will also be accepted, albeit without the ability of
  save/restore.
   - `HASH` is a designator for a hash function, which may be
  desirable to customize when one has specific knowledge about the set
  of keys which will populate the table.  At this time, a 32-bit hash
  is recommended as this is what has been used for development and
  testing and has been shown to provide good performance in
  practice. As with `TEST` it is recommended that `HASH` be specified
  by a symbol that is fboundp.
   - `ROOT` is the slot used internally for storage of the root inode
  structure that maintains the reference to the contents of the ctrie
  proper.  The ctrie-root must only be accessed using the _RDCSS ROOT
  NODE PROTOCOL_ defined by the top-level entry-points `ROOT-NODE-ACCESS`
  and `ROOT-NODE-REPLACE`"
  (readonly-p  nil)
  (test       'equal  :read-only t)
  (hash       'sxhash :read-only t)
  (stamp      (if *debug* #'local-time:now (constantly nil)))
  (root       (make-inode (make-cnode) (gensym "ctrie"))))


(defun make-ctrie (&rest args &key name root (readonly-p nil)
                    (test 'equal) (hash 'sxhash) &allow-other-keys)
  "CREATE a new CTRIE instance. This is the entry-point constructor 
  intended for use by the end-user."
  (declare (ignorable name readonly-p test hash root))
  (apply #'%make-ctrie args))


(defmacro/once with-ctrie (&once ctrie &body body)
  "Configure the dynamic environment with the appropriate condition
  handlers, control fixtures, and instrumentation necessary to execute
  the operations in BODY on the specified CTRIE. Unless specifically
  documented, the particular configuration of this dynamic environment
  should be considered an implementation detail and not relied upon. A
  particular exception, however, is that within the dynamic extent of
  a WITH-CTRIE form, the code implementing a CTRIE operation may
  expect that the special variable `*CTRIE*` will be bound to the root
  container of subject CTRIE.  See also the documentation for
  `*CTRIE*`"
  `(let* ((*ctrie* (if (typep ,ctrie 'function)
                     (funcall ,ctrie #'identity)
                     ,ctrie)))
     ,@body))


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
  store. See also `(cas cl-ctrie::find-ctrie-root)`")
  (:method ((ctrie ctrie))
    (ctrie-root ctrie))
  (:method ((ctrie function))
    (find-ctrie-root (funcall ctrie #'identity))))


;; (defgeneric (cas find-ctrie-root) (ctrie-designator old new)
;;   (:documentation "(CAS FIND-CTRIE-ROOT) implements the atomic
;;   compare-and swap subprimitive that is the operational analogue to
;;   `FIND-CTRIE-ROOT`. It should not be referenced by the higher-level
;;   implementation or end-user code. The purpose of
;;   `(CAS FIND-CTRIE-ROOT)` is to incorporate a level of indirection
;;   specialized on the class of the root container to facilitate future
;;   extension with alternate storage models, e.g., an external
;;   persistent disk-based store.")
;;   (:method ((ctrie ctrie) old new)
;;     (cas (ctrie-root ctrie) old new)))


(defstruct (rdcss-descriptor
             (:copier nil))
  "An RDCSS-DESCRIPTOR object represents a 'plan' for a proposed RDCSS
  (restricted double compare single swap) operation. The use of this
  descriptor object provides the means to effect an atomic RDCSS in
  software, requiring only hardware support for single-word CAS, which is
  preferable because it is commonly available on curent consumer hardware.
   - `OV`        designates a slot containing the OLD (current) root inode.
                 If the swap is unsuccessful, the resulting ctrie will revert
                 to this structure as the root inode. 
   - `OVMAIN`    designates a slot containing the CNODE that is referenced
                 by the OLD (current) root inode.
   - `NV`        designates a slot containing a fully assembled replacement
                 root inode referencing a valid CNODE. This pair will become
                 the root inode and level 0 MAIN-NODE of the ctrie if the
                 swap is successful.
   - `COMMITTED` designates a flag which, when not NIL, indicates that the
                 RDCSS plan defined by this descriptor has completed
                 successfully"
  (ov        nil :read-only t)
  (ovmain    nil :read-only t)
  (nv        nil :read-only t)
  (committed nil))


(defun root-node-access (ctrie &optional abort)
  "ROOT-NODE-ACCESS extends `FIND-CTRIE-ROOT`,
  implementing the _RDCSS ROOT NODE PROTOCOL_ for access to root inode of
  CTRIE.  In particular, it ensures that if, instead of an inode, the
  root of CTRIE contains an RDCSS descriptor of a proposed root-node
  update, that it will immediately invoke `ROOT-NODE-COMMIT` to act
  on that descriptor and return an INODE struct that is the result of
  the completed commit process. `ROOT-NODE-ACCESS` only provides access
  to the root inode _STRUCTURE_ and in particular it does not provide
  safe access to the _CONTENT_ of that inode. In order to access those
  contents, the root inode returned by `ROOT-NODE-ACCESS` must be further
  processed by `INODE-READ` in order to still properly comply with the
  underlying GCAS protocol implementation requirements common to all
  inodes"
  (atypecase (find-ctrie-root ctrie)
    (inode it)
    (t (root-node-commit ctrie abort))))


(defun root-node-replace (ctrie ov ovmain nv)
  "ROOT-NODE-REPLACE implements the _RDCSS ROOT NODE PROTOCOL_ for
  replacement of the ROOT INODE of a CTRIE structure with another one
  that contains new or alternative values, achieving the end-result
  effectively the same as if by mutation.  The replacement of the root
  inode is accomplished in two, basic conceptual stages. First, an
  `RDCSS-DESCRIPTOR` object which specifies in full the current state
  and all desired changes in the proposed resulting state.  Thus,
  whether any individual replacement attempt succeeds or fails, either
  result is guarenteed to represent a valid state. An attempt is then
  made to atomically swap this RDCSS-DESCRIPTOR with the current CTRIE
  root inode.  Note that, although it contains all of the information
  representing two, distinct, root inode states, the RDCSS-DESCRIPTOR
  is not, itself, a valid root inode.  That is the reason why all
  access to the root inode must be accomplished using this specialized
  _RDCSS ROOT NODE PROTOCOL_ rather than just the _GCAS INODE
  PROTOCOL_ alone, as is done with all other non-root inodes.  Once an
  atomic compare-and-swap of an RDCSS-DESCRIPTOR object with the root
  inode completes successfully, `ROOT-NODE-COMMIT` is invoked which
  will attempt to complete the second step of this protocol.  The
  result of that commit will be one or the other of the two valid
  states defined in the RDCSS-DESCRIPTOR object.  If another thread
  concurrently attempts access to a root node holding an
  RDCSS-DESCRIPTOR object rather than an INODE, it will invoke
  `ROOT-NODE-COMMIT` itself, possibly prempting our own attempt, but
  guaranteeing nonblocking access to a valid root node by any concurrent
  thread"
  (when (ctrie-readonly-p ctrie)
    (ctrie-modification-failed "This CTRIE is READ-ONLY"
      :op 'root-node-replace :place (list :ov ov :nv nv)))
  (let1 desc (make-rdcss-descriptor :ov ov :ovmain ovmain :nv nv)
    (if (cas (ctrie-root ctrie) ov desc)
      (prog2 (root-node-commit ctrie nil)
        (rdcss-descriptor-committed desc))
      nil)))


(defun root-node-commit (ctrie &optional abort)
  "rdcss api to complete a root-node transaction"
  (atypecase (ctrie-root ctrie)
    (inode it)
    (rdcss-descriptor (if abort
                        (if (cas (ctrie-root ctrie) it (rdcss-descriptor-ov it))
                          (rdcss-descriptor-ov it)
                          (root-node-commit ctrie abort))
                        (let1 oldmain (inode-read (rdcss-descriptor-ov it))
                          (if (eq oldmain (rdcss-descriptor-ovmain it))
                            (if (cas (ctrie-root ctrie) it (rdcss-descriptor-nv it))
                              (prog1 (rdcss-descriptor-nv it)
                                (setf (rdcss-descriptor-committed it) t))
                              (root-node-commit ctrie abort))
                            (if (cas (ctrie-root ctrie) it (rdcss-descriptor-ov it))
                              (rdcss-descriptor-ov it)
                              (root-node-commit ctrie abort))))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feature impl. using RDCSS root-node protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ctrie-snapshot (ctrie &key read-only)
  ""
  (loop until
    (let* ((root (root-node-access ctrie))
            (main (inode-read root)))
      (when (root-node-replace ctrie root main (make-inode main (gensym "ctrie")))
        (return-from ctrie-snapshot
          (if read-only
            (make-ctrie :readonly-p t :root root)
            (make-ctrie :root (make-inode main (gensym "ctrie")))))))))


(defun ctrie-clear (ctrie)
  ""
  (loop until
    (let* ((root (root-node-access ctrie))
            (main (inode-read root)))
      (when (root-node-replace ctrie root main (make-inode (make-cnode) (gensym "ctrie")))
        (return-from ctrie-clear ctrie)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cas-ctrie-root / This Operation Appears to have changed in recently published
;; descriptions of the ctrie which change the protocol to eliminate this
;; possibility by starting with a fully initialized level 0 cnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                         ;;;;
;;;;              THE BIG THREE:  INSERT / LOOKUP / REMOVE                   ;;;;
;;;;                                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT/INSERT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %insert (inode key value level parent startgen)
  "   -  The detailed specifics required to perform an insertion into a
  CTRIE map are defined by and contained within the `%INSERT` function,
  which is not part of the USER API and should never be invoked
  directly.  The procedures required for interaction with `%INSERT` are
  managed entirely by the upper layer entry-points and should be
  entirely invisible to the developer using CL-CTRIE.  The alogorithm is
  intricate and requires quite some effort to figure out based on the
  papers and documentation. For that reason this attempt is made to
  properly document the process -- not to encourage anyone to fiddle
  with it...

  > 1.  A given call to %insert carries with it no guarantee that it will
  actually succeed.  Further, there is no guarantee it will do anything
  at all -- including ever returning to the caller having invoked it.
  This is because there are a number of circumstances that may possibly
  interfere with insertion in a lock-free concurrent data-structure and
  in order to minimize the cost incurred by aborted attempts and
  restarts, there is no effort wasted on careful condition handling or
  recovery.  Instead, as soon as it is recognised that a particular
  attempt will not succeed, Control is thrown via non-local exit
  unceremoneously the entire call stack and restarting each time from
  the very beginning. There are quite a few places within the process
  where you will find these nonlocal exits. Originally I had in mind to
  incorporate some additional insrumentation to track and gather
  statistics the log the specifics of all this, and there are a number
  of extension points provided.  For more details refer to the
  `MULTI-CATCH` and `CATCH-CASE` control structure documentation.

  > 2.  %insert ALWAYS begins at an INODE.  This is not surprising, of
  course, since the root of the tree (where all inserts begin) is an
  INODE, and because INODES are the only nodes that provide
  mutability. When we which to effect change to the CTRIE regardless of
  where or what type, It may only be accomplished by mutating the parent
  INODE immediately above it to reference content that must be freshly
  regenerated each time changes are required.  Once it is established
  that the %insert always begins at an inode, we can reason further
  about the sequence of events that follow by considering some of the
  node oriented invariants for CTRIEs specified variously in the
  academic literature.  First, it has been clearly defined that an INODE
  structure may only reference three kinds of structure that we
  collectively refer to as MAIN-NODES.  These are the three potential
  cases which we will consider next.

  > 3.  Consider first the TNODE. If indeed we follow an INODE and
  discover it directly leads us to a TNODE, or 'Tomb Node'. This tells
  us, first, that we have arrived at a dead-end, second that we must
  assist with the 'compression' of this arc by invoking the `CLEAN`
  operation on the tombed INODE's parent.  Finally, there is nothing
  further we can do so we THROW to :RESTART.

  > 4.  If traverse the INODE and arrive at an LNODE, we are also at the
  end of the ARC, but if it is due to hash collision then the algorithm
  then indeed it may be correct.  In this case we attempt to 'insert'
  ourself in te LNODE chain and then invoke INODE mutate to atomic
  commit and then THROW to :RESTART

  > 5.  As a simple instance of the general case, we may arrive at a CNODE
  with vacant arc that represents the index specified by the bits of our
  KEY's hash code that are active for this level within the ctrie.  When
  this is the case, we construct a replacement CNODE augmented with our
  key/value pair as an SNODE LEAF at the physical position within the
  CNODES storage vector appropriately translated from the logical arc
  index as described by the documentation of the functions
  `FLAG-ARC-POSITION` and `FLAG-VECTOR` If we successfully mutate the
  parent inode by completing an atomic replacement of the old cnode with
  the one we constructed, then our insertion has succeeded and we return
  the range value now successfully mapped by KEY in order to indicate
  our success.  Otherwise we THROW to :RESTART.

  > 6.  If we find the logical index of our 'arc' in this CNODE is not
  empty and available as we did above, there are exactly two other
  possibly find there; we know this because it is required to be a
  BRANCH-NODE -- either an snode leaf storage or an inode referencing a
  MAIN-NODE that represents the next layer of the CTRIE.  We describe
  various posible cases and the define a procedure specified for each
  below.

  > 7.  If we find that the node PRESENT at this index is an INODE, then
  this is the simplest of the possible cases.  Conceptually, what we
  intend to do is continue to follow our arc, descending to the next
  level of the CTRIE structure that is referenced by that inode.  In
  practice, however, we are required to consider the possibility that
  the generational descriptor object the inode contains may not be
  consistent with STARTGEN, which is the one current in the root INODE
  of this CTRIE.  This may be the case, for example, as the result of
  some past cloning/snapshot operation if we are the first since then to
  traverse this inode. (Remember that the refresh of generational
  descriptor occurs lazily on an as-needed basis in order to avoid
  overhead incurred by eager traversals which often turn out to have
  been unnecessary).  In consideration of this we proceeed as follows: -
  If the inode generational descriptor is consistent with STARTGEN, we
  simply continue along our arc by recursively invoking `%insert` on
  that INODE.  If that function call returns successfully with a VALUE,
  then the insertion was successful, and we also then return, passing
  along that value.  Thus the result is communicated back through the
  caller chain, eventually arriving back as the result of the original
  CTRIE-PUT entry-point.  - Otherwise if the generational descriptor is
  not consistent with STARTGEN we attempt an atomic `INODE-MUTATE` on
  the PARENT INODE of this CNODE that effects the replacement of that
  inode within it by one FRESHLY CREATED by `REFRESH` and ensured to be
  consistent with STARTGEN.  If this succeeds, we invoke %insert
  recusively and proceed in the same manner, since, effectively, we are
  now in a state equivalent to the one described above.  - If the
  INODE-MUTATE of the prior step did NOT succeed, then we are out of
  options and throw to :RESTART the insertion process from the beginning
  all over again.

  > 8.  If we find that the node PRESENT at this index is an SNODE,
  then our situation becomes a little bit more complex and
  there are a few more contingencies we must be prepared to
  address.

  > 9.  Once again, looking at the simplest first, when an insert
  operation encounters a leaf-node somewhere along the descent of it's
  'own' arc, one potential case is that it found the node it was
  looking for -- one that contains a key that satisfies the test
  predicate defined for the dynamic extent of the current operation,
  `CTEQUAL,` when compared to the `KEY` currently being `%INSERTED.`
  If the equality test is satisfied then the VALUE that node maps
  should be updated with the one of the present insertion.  The steps
  to effect the update are very similar to those of step 5, however We
  construct a replacement CNODE augmented with our key/value pair as a
  replacement SNODE in the SAME physical position as the one we have
  found -- refer to the documentation for the function `CNODE-UPDATED`
  for additional specifics on the internal details that describe this
  operation.  If we successfully mutate the parent inode by completing
  an atomic replacement of the old cnode with the one we constructed,
  then our update has succeeded and we return the range value now
  successfully mapped by KEY in order to indicate our success.
  Otherwise we THROW to :RESTART.

  > 10.  In some circumstances, we encounter a node on our arc whose
  hash code bits have matched that of the current key of this
  insertion for all of the lower order bits that have been consumed so
  far, up to the current depth, but that (as opposed to step 9) does
  not satisfy `CTEQUAL.` and so is NOT a candidiate for update
  replacement.  Except in very vare circumstances, there will be some
  depth at which the active bits of its hash code will indeed be
  distinct from our own, and at that point a CNODE can be constructed
  that will proprerly contain both it and an snode mapping the
  key/value of the current insertion.  This means we must ENTEND the
  ctrie as many layers as needed to get to that depth, inserting
  CNODES and INODES at each step along the way.  Now, we will first
  describe the 'edge' case where we have encounted the 'rare
  circumstance.' If we perform this process and arrive at a depth
  where all 32 hash code bits have been consumed and, indeed, these
  two unequal keys are the result of a 'hash code collision' In order
  that we preserve correct operation, we respond in this case by
  chaining these key/value SNODES into a linked list of LNODES.
  Therefore, they can share the same arc index and when we encounter
  such a thing during future traversals, we can accomodate the
  collision using simple linear search and a few basic LNODE utility
  functions such as `LNODE-INSERTED` `LNODE-REMOVED` `LNODE-SEARCH`
  `LNODE-LENGTH` and the list constructor `ENLIST.` Once we have
  `ENLIST`ed the colliding SNODES, we create a new INODE pointing to
  that list, and then attempt atomic replacement of the CNODE above
  with one we extend to contain that INODE.  If we do successfully
  mutate the prior CNODES parent INODE resulting in its replacement
  with the CNODE we constructed, then our insert has succeeded and we
  return the range value now successfully mapped by KEY in order to
  indicate our success.  Otherwise we THROW to :RESTART.

  > 11.  Finally, let us return to those intermediate steps, mentioned
  above, to specify the means by which we perform the level-by-level
  extension of a given arc to accommodate both the above case of hash
  collision as well as the more common one when the reason for
  extension is simply to accomodate normal growth capacity and
  allocation.  In both cases, though, the extensions are performed for
  the same initiating cause -- to accomodate the collision of leaf
  node keys resident at lower levels of the structure.  Depending on
  the similarity of two colliding hash keys, the extension process may
  not be resolved with a single iteration.  In the case of full
  collisiion, described above, the extension process will recur, up to
  a maximum depth of `(32/W)` levels, at which point an L-NODE chain
  will be created.  At each iteration, a new INODE is created,
  pointing to a new CNODE containing one of our conflictung pairs.
  Then, `%INSERT` is attempted on that INODE and this process recurs.
  Once this cycle of insert/extend completes, each INODE/CNODE pair is
  returned to the parent -- the entire newly created structure
  eventually returning to the point of original conflicts whre the
  extension cycle began.  If we successfully mutate the parent inode
  by completing an atomic replacement of the old cnode with the one
  that begins this newly built structue, then our update has succeeded
  and we return the range value now successfully mapped by KEY in
  order to indicate our success.  Otherwise we THROW to :RESTART"
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 0.  INODE (start)  ;;   
  ;;;;;;;;;;;;;;;;;;;;;;;;

  (atypecase (inode-read inode)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 3.  INODE -> TNODE ;;   
    ;;;;;;;;;;;;;;;;;;;;;;;;
    
    (TNODE (throw :restart (clean parent (- level 5))))

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 4.  INODE -> LNODE ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;

    (LNODE (unless (inode-mutate inode it
                     (lnode-inserted it key value #'ctequal))
             (throw :restart it)))

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5.  INODE -> CNODE ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;
    
    (CNODE (let* ((cnode it)
                   (bmp  (cnode-bitmap cnode))
                   (flag (flag key level))
                   (pos  (flag-arc-position flag bmp)))
             
             ;;;;;;;;;;;;;;;;;;;;;;;;
             ;; 5.  CNODE -> empty ;;
             ;;;;;;;;;;;;;;;;;;;;;;;;
             
             (if (not (flag-present-p flag bmp))
               (let ((new-cnode (cnode-extended cnode flag pos (snode key value))))
                 (if (inode-mutate inode cnode new-cnode)
                   (return-from %insert value)
                   (throw :restart cnode)))
               
               (let ((present (svref (cnode-arcs cnode) pos)))    
                 (etypecase present

                   ;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; 6.  CNODE -> INODE ;;
                   ;;;;;;;;;;;;;;;;;;;;;;;;
                   
                   (INODE (if (eq startgen (inode-gen present))
                            (return-from %insert
                              (%insert present key value (+ level 5) inode startgen))
                            (if (inode-mutate inode it (refresh it startgen))
                              (%insert inode key value level parent startgen)
                              (throw :restart startgen))))

                   ;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; 8.  CNODE -> SNODE ;; 
                   ;;;;;;;;;;;;;;;;;;;;;;;;
                   
                   (SNODE (if (ctequal key (snode-key present))
                            (let ((new-cnode (cnode-updated cnode pos (snode key value))))

                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ;; 9.  SNODE -> SNODE / REPLACE (updated range value) ;;
                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                              (if (inode-mutate inode cnode new-cnode)
                                (return-from %insert value)
                                (throw :restart present)))                          
                            
                            (if (>= level 30)
                              (let* ((new-snode (snode key value))
                                      (lnode-chain (enlist new-snode present))
                                      (new-inode (make-inode lnode-chain startgen))
                                      (new-cnode (cnode-updated cnode pos new-inode)))

                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;; 10.  SNODE -> SNODE / new INODE->LNODE (hash collision) ;;
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                (if (inode-mutate inode cnode new-cnode)
                                  (return-from %insert value)
                                  (throw :restart lnode-chain)))
                              
                              (let* ((new-flag-other (flag (snode-key present) (+ level 5)))
                                      (new-bitmap (logior new-flag-other))
                                      (new-cnode (make-cnode new-bitmap))
                                      (new-inode (make-inode new-cnode startgen)))

                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;; 11.  SNODE-> SNODE / new INODE->CNODE (ctrie growth) ;;
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                (setf (svref (cnode-arcs new-cnode) (flag-arc-position
                                                                      new-flag-other
                                                                      new-bitmap))
                                  present)
                                
                                (%insert new-inode key value (+ level 5) inode startgen)
                                
                                (if (inode-mutate inode cnode (cnode-updated cnode pos new-inode))
                                  (return-from %insert value)
                                  (throw :restart present)))))))))))))


(defun ctrie-put (ctrie key value)
  "Insert a new entry into CTRIE mapping KEY to VALUE.  If an entry
  with key equal to KEY aleady exists in CTRIE, according to the
  equality predicate defined by `CTRIE-TEST` then the priorbmapping
  will be replaced by VALUE. Returns `VALUE` representing the
  mapping in the resulting CTRIE"
  (with-ctrie ctrie 
    (loop  with d = 0 and p = nil and result
      for  root =  (root-node-access *ctrie*)
      when (catch-case (%insert root key value d p (inode-gen root))
             (:restart (prog1 nil
                         (when *debug*
                           (format *TRACE-OUTPUT* "~8A  timeout insert (~A . ~A)~%"
                             it key value))))
             (t        (prog1 (setf result it)
                         (when *debug*
                           (format *TRACE-OUTPUT* "~8S  done .~%~S~%" it *ctrie*)))))
      return result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET/LOOKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun %lookup (inode key level parent startgen)
  "The general concept of the procedure for finding a given key within
  a simplified SEQUENTIAL model of a CTRIE can be summarized as
  follows: If the internal node is at level `L` then the W bits of the
  hashcode starting from position `W * L` are used as a logical index
  into the vector of `2^W` arcs that can possibly be represented
  within that node (see `FLAG` and `FLAG-VECTOR`). This logical index
  is then transformed into a physical index that denotes a specific
  position locating this arc relative to all other arcs currently
  present in the node (see `FLAG-ARC-POSITION`.  In this way, storage
  within the node need not be allocated for representation of empty
  arc positions. At all times the invariant is maintained that the
  number of arcs allocated within a given CNODE is equal to the
  Hamming-Weight of its BITMAP -- i.e., the number of nonzero bits
  present (see `CL:LOGCOUNT`). The arc at this calculated relative
  position is then followed, and the process repeated until arrival at
  a leaf-node or empty arc position.  Locating a given key becomes
  substantially more complicated in the actual lock-free concurrent
  ctrie algorithm"
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
  (with-ctrie ctrie
    (flet ((attempt-get (root key level parent gen try)
             (declare (ignorable try))
             (catch-case (%lookup root key level parent gen)
               (:notfound  (return-from ctrie-get (values nil nil)))
               (:restart   (multiple-value-prog1 (values it nil)
                             #+()(when *debug* (format t  "~8s  restarting after try ~d.~%"
                                                 it try))))
               (t          (multiple-value-prog1 (values it t)
                             #+()(when *debug* (format t  "~8s  done on try ~d.~%"
                                                 it try)))))))
      (loop with vals with d = 0 and p = nil
        for  r = (root-node-access *ctrie*) for try from 1
        do   (setf vals (multiple-value-list (attempt-get r key d p (inode-gen r) try)))
        when (> try 1000)  do (error "try ~d" try)
        when (second vals) do (return-from ctrie-get (values (first vals) t))))))


(defun (setf ctrie-get) (value ctrie key)
  (with-ctrie ctrie
    (prog1 value
      (ctrie-put *ctrie* key value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DROP/REMOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
(defun %remove (inode key level parent startgen)
  (atypecase (inode-read inode)
    (tnode (values (clean parent (- level 5)) :restart))
    (lnode (let* ((new-lnode      (lnode-removed it key #'ctequal))
                   (groomed-lnode (if (null (lnode-next new-lnode))
                                    (entomb (lnode-elt new-lnode))
                                    new-lnode))
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
    (with-ctrie ctrie
      (let1 r (root-node-access *ctrie*)
        (multiple-value-bind (val kind) (%remove r key 0 nil (inode-gen r))
          (case kind
            (:restart  (return-from ctrie-drop (ctrie-drop *ctrie* key)))
            (:notfound (values val nil))
            (t         (values val t)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sketch of Alternative Approach using FSM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; something like this could be a lot easier (with a little macrology to
;; eliminate the boilerplate) but probably hard to do without introdcing
;; some inefficiencies. at the moment i'm not inspired to start redoing
;; code working already anyway

;; (defun make-start-state (&optional at &aux (edge-types '(tnode lnode cnode)))
;;   (alet (started-at looking-at edge)
;;     (funcall this :reset at)
;;     (dlambda
;;       (:?      () (describe this))
;;       (:reset (&optional (start at))
;;         (setf started-at start)
;;         (setf edge nil looking-at nil)
;;         this)
;;       (:check-preconditions ()
;;         (inode-p started-at))
;;       (:initialize ()
;;         (and
;;           (funcall this :check-preconditions)
;;           (setf looking-at started-at)
;;           (setf edge (inode-read looking-at))
;;           t))
;;       (:check-invariants ()
;;         (and
;;           (inode-p started-at)
;;           (inode-p looking-at)
;;           (find (type-of edge) edge-types)))
;;       (:edge-type ()
;;         (type-of edge))
;;       (:look-at-next ()
;;         edge)
;;       (t () (funcall this :?)))))
;;
;; (defun make-main-node-state ()
;;   (alet (started-at looking-at edge-type edge)
;;     (dlambda
;;       (:?      () (describe this))
;;       (:initialize (start look)
;;         (setf started-at start)
;;         (setf looking-at look))
;;       (t () (funcall this :?)))))
;;
;; (defun make-insert-fsm ()
;;   (alet (start-state looking-at edge-type edge)
;;     (alet-fsm
;;       (start (&rest args)
;;         (or start-state (apply #'make-start-state args))
;;         (funcall start-state :check-preconditions)
;;         (funcall start-state :initialize)
;;         (funcall start-state :check-invariants)
;;         (multiple-value-prog1 (values
;;                                 (setf edge (funcall start-state :edge))
;;                                 (setf edge-type (funcall start-state :edge-type)))
;;           (state (fdefinition (funcall start-state :edge-type)))))
;;       (cnode (&rest args)
;;         (declare (ignore args))
;;         :cnode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diagnostic and Status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ctrie-max-depth (thing)
  "Compute the maximum length of any arc. Useful as a diagnostic"
  (typecase thing
    (snode 0)
    (tnode 0)
    (lnode 0)
    (ctrie (ctrie-max-depth (inode-read (root-node-access thing))))
    (inode (ctrie-max-depth (inode-read thing)))
    (cnode (1+ (loop for arc across (cnode-arcs thing)
                 maximizing (ctrie-max-depth arc))))))


(defun ctrie-min-depth (thing)
  "Compute the minimum length of any arc. Useful as a diagnostic"
  (typecase thing
    (snode 0)
    (tnode 0)
    (lnode 0)
    (ctrie (ctrie-min-depth (inode-read (root-node-access thing))))
    (inode (ctrie-min-depth (inode-read thing)))
    (cnode (1+ (loop for arc across (cnode-arcs thing)
                 minimizing (ctrie-min-depth arc))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-node (node fn))


(defun ctrie-map (ctrie fn &key atomic
                   &aux accum);; (ctrie (with-ctrie *ctrie*)))
  (declare (special accum))
  (with-ctrie ctrie
    (with-ctrie (if atomic (ctrie-snapshot *ctrie* :read-only t) *ctrie*)
      (let ((root (root-node-access *ctrie*)))
        (map-node root fn))
      accum)))


(defmacro ctrie-do ((key value ctrie &key atomic) &body body)
  "Iterate over (key . value) in ctrie in the manner of dolist.
  ;;;  EXAMPLE: (ctrie-do (k v ctrie)
  ;;;             (format t \"~&~8S => ~10S~%\" k v))"
  `(ctrie-map ,ctrie #'(lambda (,key ,value) ,@body)
     :atomic ,atomic))

(defmethod map-node ((node null) fn)
  (values))

(defmethod map-node ((node inode) fn)
  (map-node (inode-read node) fn))


(defmethod map-node ((node snode) fn)
  (funcall fn (snode-key node) (snode-value node)))


(defmethod map-node ((node tnode) fn)
  (map-node (tnode-cell node) fn))


(defmethod map-node ((node lnode) fn)
 ;; (log:sexp node)
  (map-node (lnode-elt node) fn)
  (map-node (lnode-next node) fn))


(defmethod map-node ((node cnode) fn)
  (loop for arc across (cnode-arcs node)
    do (map-node arc fn)))


(defun ctrie-map-keys (ctrie fn &key atomic)
  (with-ctrie ctrie
    (ctrie-map *ctrie*
      (lambda (k v) (declare (ignore v))
        (funcall fn k)) :atomic atomic)))


(defun ctrie-map-values (ctrie fn &key atomic)
  (with-ctrie ctrie 
    (ctrie-map *ctrie*
      (lambda (k v) (declare (ignore k))
        (funcall fn v)) :atomic atomic)))


;; (defgeneric ctrie-map-into (ctrie place &optional (fn #'identity))
;;   (:method ((ctrie ctrie) (place ctrie) fn)
;;       (ctrie-map ctrie
;;         (lambda (&rest args) 
;;           (apply #'ctrie-put place (apply fn k v)))))
;;   (:method (ctrie  place fn &aux
;;              (place (with-ctrie place *ctrie*))
;;              (ctrie (with-ctrie ctrie *ctrie*)))
;;     (ctrie-map ctrie
;;       (lambda (k v) 
;;         (apply #'ctrie-put place (funcall fn k v))))))


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
  (with-ctrie ctrie
    (ctrie-map *ctrie*
      #'collect-keys :atomic atomic)))


(defun ctrie-values (ctrie &key atomic)
  (with-ctrie ctrie
    (ctrie-map *ctrie*
      #'collect-values :atomic atomic)))


(defun ctrie-size (ctrie &aux (accum 0))
  (with-ctrie ctrie
    (ctrie-map-keys *ctrie*
      (lambda (x) (declare (ignore x))
        (incf accum)))
    accum))


(defun ctrie-empty-p (ctrie)
  (with-ctrie ctrie
    (= 0 (cnode-bitmap
           (inode-read
             (root-node-access *ctrie*))))))


(defun ctrie-to-alist (ctrie &key atomic)
  (with-ctrie  ctrie
    (ctrie-map *ctrie*
      #'collect2 :atomic atomic)))


(defun ctrie-pprint (ctrie &optional (stream t))
  (with-ctrie ctrie
    (pprint-tabular stream (ctrie-to-alist *ctrie*))))


(defun ctrie-to-hashtable (ctrie &key atomic)
  (with-ctrie ctrie
    (alexandria:alist-hash-table
      (ctrie-map *ctrie*
        #'collect2 :atomic atomic))))


(defun ctrie-from-alist (alist &key ctrie)
  (with-ctrie (or ctrie (make-ctrie))
    (prog1 *ctrie*
      (mapc (lambda (pair)
              (ctrie-put *ctrie* (car pair) (cdr pair)))
        alist))))

(defun ctrie-from-hashtable (hashtable &key ctrie)
  "create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE"
    (with-ctrie (or ctrie (make-ctrie :test (hash-table-test hashtable)))
      (prog1 *ctrie* (maphash (lambda (k v) (ctrie-put *ctrie* k v))
                       hashtable))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic CTRIE I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (find-package :cl-store)
    (pushnew :cl-store *features*)))


(defgeneric ctrie-save (ctrie place &key &allow-other-keys)
  (:documentation ""))

(defmethod  ctrie-save :around (ctrie place &key)
  (log:info "Saving ctrie to ~A" place)
  (call-next-method))


#+cl-store
(defmethod  ctrie-save ((ctrie ctrie) (place pathname) &key)
  (with-ctrie ctrie
    (prog1 place
      (cl-store:store (ctrie-snapshot *ctrie*) place))))

(defmethod  ctrie-save ((ctrie function) (place pathname) &key)
  (with-ctrie ctrie
    (prog1 place
      (cl-store:store (ctrie-snapshot *ctrie*) place))))


(defgeneric ctrie-load (place &key &allow-other-keys))


#+cl-store
(defmethod  ctrie-load ((place pathname) &key)
  (cl-store:restore place))


(defgeneric ctrie-export (ctrie place &key &allow-other-keys)
  (:documentation ""))


#+cl-store 
(defmethod  ctrie-export ((ctrie ctrie) (place pathname) &key)
  (with-ctrie ctrie
    (prog1 place
      (cl-store:store (ctrie-to-alist *ctrie*) place))))

(defmethod  ctrie-export ((ctrie function) (place pathname) &key)
  (with-ctrie ctrie
    (prog1 place
      (cl-store:store (ctrie-to-alist *ctrie*) place))))

(defmethod  ctrie-export ((ctrie ctrie) (place hash-table) &key)
  (with-ctrie ctrie
    (ctrie-map *ctrie*
      (lambda (k v)
        (setf (gethash place k) v)))))

(defmethod  ctrie-export ((ctrie function) (place hash-table) &key)
  (with-ctrie ctrie
    (ctrie-map *ctrie*
      (lambda (k v)
        (setf (gethash place k) v)))))


(defgeneric ctrie-import (place &key &allow-other-keys)
  (:documentation ""))


#+cl-store
(defmethod  ctrie-import ((place pathname) &key)
  (ctrie-from-alist (cl-store:restore place)))


(defmethod  ctrie-import ((place hash-table) &key)
  (ctrie-from-hashtable place))



