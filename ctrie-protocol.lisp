;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The CTRIE Protocol
;;
;; Explicate 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ctrie ()
  "A CTRIE structure is the root container that uniquely identifies a CTRIE
  instance, and  contains the following perameters which specify the
  definable aspects of each CTRIE
  ------------------------------------------------------------------------------
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
   and `ROOT-NODE-REPLACE`
  ------------------------------------------------------------------------------"
  '(satisfies ctrie-p))

(defgeneric ctrie-p (thing))

(defgeneric ctrie-root (ctrie))

(defgeneric ctrie-name (ctrie))

(defgeneric ctrie-readonly-p (ctrie))

(defgeneric ctrie-test (ctrie))

(defgeneric ctrie-hash (ctrie))

(defgeneric ctrie-stamp (ctrie))

(defgeneric ctrie-context (ctrie))

(defgeneric ctrie-env (thing &optional default-context))

(defgeneric ctrie-next-index (sequence-designator))

(defgeneric make-ctrie (place &key context readonly-p test hash stamp &allow-other-keys))

(defgeneric funcall-with-ctrie-context (thing thunk)
  (:documentation  "Configure the dynamic environment with the appropriate condition
  handlers, control fixtures, and instrumentation necessary to execute
  the operations in BODY on the specified CTRIE. Unless specifically
  documented, the particular configuration of this dynamic environment
  should be considered an implementation detail and not relied upon. A
  particular exception, however, is that within the dynamic extent of
  a WITH-CTRIE form, the code implementing a CTRIE operation may
  expect that the special variable `*CTRIE*` will be bound to the root
  container of subject CTRIE.  See also the documentation for
  `*CTRIE*`"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Supporting Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric node-eq (node1 node2))

(defgeneric index-incf (thing &optional amount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ref ()
  "Atomically Stamped Reference structure _[Herlithy, TAOMP]_ that
  encapsulates the mutable slots within an inode. Any specific `REF`
  structure is, itself, never mutated.  Using the `REF` structure
  as basis of inode implementation provides the capability to 'bundle'
  additional metadata in an inode while still providing atomic compare
  and swap using a single comparison of the aggregate `REF` instance.
  ------------------------------------------------------------------------------
   - `STAMP` defines a slot containing implementation-specific metadata
    that may be maintained internally as a means of tracking inode
    modification and update behavior.  It should not be referenced by
    user code, and the format of its contents should not be relied apon.
   - `VALUE` defines a slot that contains a reference to the MAIN-NODE
    that the enclosing inode should be interpreted as 'pointing to'
   - `PREV` defines a slot which, during the `INODE-COMMIT` phase of the
    _GCAS INODE PROTOCOL_ maintains a reference to the last valid
    inode state, which may be restored, if necessary, during the
    course of the `INODE-READ` / `INODE-COMMIT` arbitration process
  ------------------------------------------------------------------------------"
  '(satisfies ref-p))

(defgeneric ref-p (thing))

(defgeneric ref-prev (ref))

(defgeneric ref-stamp (ref))

(defgeneric ref-value (ref))

(define-layered-function make-ref (&rest args &key stamp value prev &allow-other-keys))

(deftype failed-ref ()
  "A `FAILED-REF` is a structure that is used to preserve the linkage to
  prior inode state following a failed GCAS.  Any inode access that
  detects a `FAILED-REF` will immediately invoke a commit to restore the
  inode to the state recorded in `FAILED-REF-PREV`"
  '(satisfies failed-ref-p))

(defgeneric failed-ref-p (thing))

(define-layered-function make-failed-ref (&key stamp value prev &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype inode ()
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
  ------------------------------------------------------------------------------
  - `GEN` defines a slot containing a generational descriptor object
  - `REF` defines a slot containing a `REF` struct that encapsulates
    the mutable content within an INODE
  ------------------------------------------------------------------------------"
  '(satisfies inode-p))

(defgeneric inode-p (thing))

(defgeneric inode-ref (inode))

(defgeneric inode-gen (inode))

(define-layered-function make-inode (link-to &optional gen stamp prev)
  (:documentation "Construct a new INODE that represents a reference
  to the value provided by argument LINK-TO, optionally augmented with
  a specified generational descriptor, timestamp, and/or previous
  state"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GCAS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric cas-ctrie-root (ctrie old new)
  (:documentation "")
  (:method :around (ctrie old new)
    (check-type ctrie ctrie)
    (check-type old   (or inode rdcss-descriptor))
    (check-type new   (or inode rdcss-descriptor))
    (call-next-method)))

(defgeneric gcas-compare-and-set (obj expected new expected-stamp new-stamp prev)
  (:documentation "A thin  abstraction over the basic compare-and-swap
  primitive which provides a consistent interface to the underlying
  inode structure and manages additional metadata, providing
  reasonable defaults when they are not specified.")
  (:method :around (obj expected new expected-stamp new-stamp prev)
    (declare (ignorable expected-stamp new-stamp prev))
    (check-type obj       inode)
    (check-type expected  (or lnode snode tnode cnode))
    (check-type new       (or lnode snode tnode cnode))
    (call-next-method)))

(defgeneric cas-failed-ref (inode ref)
  (:documentation "")
  (:method :around (inode ref)
    (check-type inode inode)
    (check-type ref   ref)
    (call-next-method)))

(defgeneric cas-ref-prev (ref prev new-generator)
  (:documentation "")
  (:method :around (ref prev new-generator)
    (check-type ref  ref)
;;    (check-type prev (or lnode snode tnode cnode))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-function maybe-box (thing))

(define-layered-function maybe-unbox (thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype snode ()
  "SNODE, i.e., 'Storage Node', is the LEAF-NODE structure ultimately
  used for the storage of each key/value pair contained in the CTRIE.
  An SNODE is considered to be immutable during its lifetime.
  ------------------------------------------------------------------------------
   - `KEY` defines the slot containing an element of the map's domain.
   - `VALUE` defines the slot containing the range-element mapped to `KEY`
  ------------------------------------------------------------------------------"
  '(satisfies snode-p))

(defgeneric snode-p (thing))

(define-layered-function snode (key value &key &allow-other-keys)
  (:documentation  "Construct a new SNODE which represents the mapping from
  domain-element KEY to range-element VALUE."))

(defgeneric snode-key (thing))

(defgeneric snode-value (thing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype lnode ()
  "LNODE, i.e., 'List Node', is a special structure used to enclose
  SNODES in a singly-linked chain when the hash-codes of the
  respective SNODE-KEYS collide, but those keys are determined to be
  unique by the `CTRIE-TEST` function defined for that ctrie.  An
  LNODE (and therefore a chain of LNODEs) is considered to be
  immutable during its lifetime.  The order of the list is
  implemented (arbitrarily) as most recently added first, analogous to
  `CL:PUSH`
  ------------------------------------------------------------------------------
   - `ELT` defines the slot containing an enclosed SNODE
   - `NEXT` defines a slot referencing the next LNODE in the chain, or
     `NIL` if no further LNODES remain.
  ------------------------------------------------------------------------------"
  '(satisfies lnode-p))

(defgeneric lnode-p (thing))

(defgeneric lnode-elt (lnode))

(defgeneric lnode-next (lnode))

(define-layered-function make-lnode (&key elt next &allow-other-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TNODE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tnode ()
  "A TNODE, or 'Tomb Node', is a special node structure used to preserve
  ordering during `CTRIE-DROP` (`%remove`) operations.
  Any time a TNODE is encountered during the course of a `CTRIE-GET` (`%lookup`)
  operation, the operative thread is required to invoke a `CLEAN` operation
  on the TNODE it has encountered and throw to `:RESTART` its lookup activity
  over again.  A TNODE is considered to be immutable and may not change its
  value during its lifetime.
  ------------------------------------------------------------------------------
   - `CELL` is the property of a TNODE that represents the node that it entombs.
      Only LNODE and SNODE type nodes are ever entombed
  ------------------------------------------------------------------------------"
  '(satisfies tnode-p))

(defgeneric tnode-p (thing)
  (:documentation "TNODE type-predicate"))

(defgeneric tnode-cell (tnode))

(define-layered-function make-tnode (&key cell &allow-other-keys))

(defgeneric entomb (node)
  (:documentation "Return a newly constructed TNODE enclosing the
  argument LEAF-NODE structure `NODE`. Unless the provided argument is
  of a type for which an entombment specialization has been defined,
  signal an error, as we have arrived at an undefined state and cannot
  continue processing."))

(defgeneric resurrect (node)
  (:documentation "Return the 'resurection' of the NODE argument.  The
  resurrection of an INODE that references a TNODE is the
  LEAF-NODE structure entombed by that TNODE.  The resurrection of
  any other node is simply itself."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype cnode ()
  "A CNODE, or 'ctrie node' is a MAIN-NODE containing a vector of up
  to `2^W` 'arcs' -- i.e., references to either an SNODE or INODE,
  collectively referred to as `BRANCH-NODES.` Each CNODE
  also contains a (fixnum) bitmap that describes the layout of which
  logical indices within the total capacity of `2^W` arcs are actually
  allocated within that node with BRANCH-NODES physically present.
  For more specific details on these BITMAP and ARC-VECTOR
  constituents, refer to the following related functions: `FLAG`
  `FLAG-PRESENT-P` `FLAG-VECTOR` and `FLAG-ARC-POSITION.  CNODEs
  are considered to be immutable and arcs may not be added or
  removed during its lifetime.  The storage allocated within a CNODE
  is fixed and specified at the time of its creation based on the
  value of the BITMAP specified at time of its initialization
  ------------------------------------------------------------------------------
   - `BITMAP`
   - `ARCS`
  ------------------------------------------------------------------------------"
  '(satisfies cnode-p))

(defgeneric cnode-p (thing))

(defgeneric cnode-bitmap (cnode))

(defgeneric cnode-arcs (cnode))

(define-layered-function make-cnode (&key bitmap initial-contents)
  (:documentation "Construct a CNODE with internal storage allocated
  for the number of arcs equal to the Hamming-Weight of the supplied
  BITMAP parameter.  If no BITMAP is provided, the CNODE created will
  be empty -- a state which is only valid for the level 0 node
  referenced by the root of the CTRIE.  This constructor is otherwise
  never called directly, but is invoked during the course of
  higher-level operations such as `CNODE-EXTENDED` `CNODE-UPDATED`
  `CNODE-TRUNCATED` and `MAP-CNODE`"))

(defgeneric arc-aref (place index))

(defgeneric (setf arc-aref) (value place index))

(defgeneric arcs-len (place))

(defgeneric refresh (place gen)
  (:documentation "Reconcile the node specified by PLACE with an
  updated generational descriptor object, GEN. The actions required
  for this reconciliation vary according to the node type and 
  specializations are defined on a casewise basis.
  * INODE:
   generate a replacement for that inode that continues to reference the
   same MAIN-NODE as before, but otherwise contains the new generational
   descriptor GEN, and a new REF substructure initialized
   with freshly generated metadata, unconditionally discarding the old.
   Note that the refresh of an inode is not transitive to the nodes contained
   in the portion of the CTRIE that it references.  I.e., the process does
   not eagerly descend and propagate until needed, eliminating the
   overhead incurred by full traversals which, in many situations, turn out
   to be not even necessary.
  * SNODE:
   represents a LEAF storage cell and does not require
   any coordination with generational descriptors, and so is simply
   returned as-is."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node type abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype leaf-node ()
  "A LEAF-NODE represents a terminal value in a CTRIE arc.
  LEAF-NODEs always contain a unit-payload of CTRIE data
  storage; For example, an SNODE contains a key/value pair."
  `(or snode tnode))
    
(deftype branch-node ()
  "a BRANCH-NODE represents a single arc typically contained
  within A CNODE."
  `(or inode snode))

(deftype main-node ()
  "A MAIN-NODE is a node that typically represents a specific
  depth or level within the ctrie and is referenced
  by its own unique inode."
  `(or cnode lnode tnode))

(defgeneric leaf-node-key (resource)
  (:documentation "Return the KEY contained in a node that may be
  either an SNODE or entombed SNODE, regardless of which kind it
  happens to be"))

(defgeneric leaf-node-value (resource)
  (:documentation "Return the VALUE contained in a node that may be
  either an SNODE or entombed SNODE, regardless of which kind it
  happens to be"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDCSS root-inode protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype rdcss-descriptor ()
  "An RDCSS-DESCRIPTOR object represents a 'plan' for a proposed RDCSS
  (restricted double compare single swap) operation. The use of this
  descriptor object provides the means to effect an atomic RDCSS in
  software, requiring only hardware support for single-word CAS, which is
  preferable because it is commonly available on curent consumer hardware.
  ------------------------------------------------------------------------------
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
                 successfully
   ------------------------------------------------------------------------------"
  '(satisfies rdcss-descriptor-p))

(defgeneric rdcss-descriptor-p (thing))

(defgeneric rdcss-descriptor-ov (rdcss-descriptor))

(defgeneric rdcss-descriptor-ovmain (rdcss-descriptor))

(defgeneric rdcss-descriptor-nv (rdcss-descriptor))

(defgeneric rdcss-descriptor-committed (rdcss-descriptor))

(define-layered-function make-rdcss-descriptor (&key ov ovmain nv committed
                                                 &allow-other-keys)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic CTRIE Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-node (node fn)
  (:documentation "Applies a function two arguments, FN, to all (key . value)
  pairs contained in and below NODE.  This function is expected to be specialized
  with appropriate implementations for each specific type of possible NODE"))


(defgeneric ctrie-save (ctrie place &key &allow-other-keys)
  (:documentation "Save a representation of CTRIE to PLACE such that an
  identical ctrie may be restored at a later time using `CTRIE-LOAD` on
  that PLACE.  CTRIE-SAVE guarantees consistency under all circumstances
  using an atomic, point-in-time snapshot of CTRIE."))

(defgeneric ctrie-load (place &key &allow-other-keys)
  (:documentation "Restore a ctrie that has been saved to PLACE using
  `CTRIE-SAVE`"))

(defgeneric ctrie-export (ctrie place &key &allow-other-keys)
  (:documentation "Save all (key . value) pairs found in CTRIE to PLACE,
  such that they may be restored later using `CTRIE-IMPORT`.  Properties
  of CTRIE (other than the entries contained) are NOT preserved.  Atomicity
  is not guaranteed.  `CTRIE-EXPORT` provides an alternative to `CTRIE-SAVE`
  that supports a more compact, space-efficient stored representation, at
  the expense of losing the consistency and identicality guarantees
  of `CTRIE-SAVE`"))

(defgeneric ctrie-import (place &key ctrie &allow-other-keys)
  (:documentation "Restore all (key . value) pairs saved to PLACE into
  either the specified CTRIE if provided, or one newly created with
  default properties."))
