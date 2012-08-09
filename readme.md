CL-CTRIE
========

**CL-CTRIE** is a common-lisp implementation of the CTrie unordered map
data-structure described in the paper '*Concurrent Tries with
Efficient Non-Blocking Snapshots*, (c) ACM 2-25-2012' by Prokopec,
Bronson, Bagwell, and Odersky.

### Overview ###

A brief overview of general ctrie
concepts and existing implementations is available through its 
[page on wikipedia](http://en.wikipedia.org/wiki/Ctrie), of which the
following is a brief excerpt:

> The Ctrie data structure is a non-blocking
> concurrent hash array mapped trie based on
> single-word compare-and-swap instructions in a
> shared-memory system. It supports concurrent
> LOOKUP, INSERT and REMOVE operations. Just like
> the hash array mapped trie, it uses the entire
> 32-bit space for hash values thus having low
> risk of hashcode collisions... Ctries have been
> shown to be comparable in performance with
> concurrent skip lists, concurrent hash tables
> and similar data structures in terms of the
> lookup operation...  However, they are far more
> scalable than most concurrent hash tables where
> the insertions are concerned. Most concurrent
> hash tables are bad at conserving memory - when
> the keys are removed from the hash table, the
> underlying array is not [reduced in size]. Ctries
> have the property that the allocated memory is
> always a function of only the current number of
> keys in the data-structure.  Ctries have logarithmic
> complexity bounds of the basic operations...
> with a low constant factor due to [large dispersal
> ratio, (32^n arcs at level n)]. Ctries support a
> lock-free, linearizable, constant-time SNAPSHOT
> operation... This is a breakthrough in concurrent
> data-structure design, since other existing concurrent
> data-structures do not support snapshots. [This provides
> the means to support features such as] lock-free,
> linearizable iterator, size and clear operations. [This
> is superior to other] existing concurrent data-structures
> [which require the use of global locks [for exclusive,
> blocking semantics for update access] permitting...
> [no concurrent readers or writers] during any [update,
> insert, remove or other modifications]. In particular,
> Ctries have an O(1) ITERATOR creation operation, O(1)
> CLEAR operation, O(1) DUPLICATE operation and an
> amortized O(log n) operation for SIZE-RETRIEVAL.

### Platform ###

Currently the lisp platform supported by cl-ctrie is SBCL version
1.0.55 or greater hosted on x86/x86-64 architecture. Support could
easily be entended to include other common-lisp implementations that
offer atomic compare-and-swap functionality, notably LispWorks
5.x/6.x, which is also well instrumented with lock-free, atomic
primitives, although this is not necessarily a high priority for the
initial development cycle.

### Status

TODO

### Ideosyncrasies

Perhaps most ideosyncrasies of this common-lisp ctrie implementation
as compared with the [original](http://github.com/axel22/Ctries),
written in Scala, result from the efforts I have taken to, where
feasible, adopt an approach emphasizing a more functional oriented
decomposition of the algortithm, written in a manner that is more
closely representative of ideomatic, common-lisp coding style.  For
example, rather than expose a general purpose GCAS and RDCSS api,
these protocols are incorporated into ctrie-specific abstractions. For
__GCAS__ the exposed api includes: `INODE-READ` `INODE-MUTATE` and
`INODE-COMMIT` and for __RDCSS:__ `ROOT-NODE-ACCESS`
`ROOT-NODE-REPLACE` and `ROOT-NODE-COMMIT.` The liberties I have taken
have the intended benefit of providing an interface that is much
easier to digest, understand, remember, and work with (at least for
me) than direct exposure in imperative style of the intricate
mechanations that underlie the ctrie algorithm.  On the other hand,
the further one strays from a direct translation of the original
(verified) ctrie implementation, the greater the likelihood of
introducing bugs into an environment (lock-free concurrent data
structure development) in which bugs can be extremely subtle and
notoriously difficult to detect.  I have attempted to strike an
appropriate balance between these conflicting concerns, and I intend
to mitigate the risk, at least in part, through continued development
of an extensive arsenal of regression tests and benchmarking
facilities.

In addition, there are a few differences in the feature set that
is provided -- such as a suite of mapping operators in leiu of a
Java-style Iterator.  For the most part I expect that these
changes will be preferable to developers accustomed to a more
'lispy' coding style.

For additional insight into the specifics unique to this ctrie
implementation, an abbreviated reference to a number of internal
details may be found in the section [Internal Reference](Internal
Reference) of this document, or, of course, by referring to
the [comprehensive documentation](doc/api/index.html) that is
provided as part of this distribution.


### Source Files

The following outline provides a general description of the
constituent source files of the cl-ctrie repository and their
respective purpose.

#### Required

The following files are the core sources currently necessary
for correct ctrie operation. 

- __`ctrie-package.lisp:`__  Package definition
- __`ctrie.lisp:`__          Ctrie implementation
- __`ctrie-util.lisp:`__     Supporting Utilities

#### Supplemental

The following files define extended functionality, management,
and analysis facilities supporting the development of CL-CTRIE.

- `ctrie-cas.lisp:`   SBCL CAS extensions
- `ctrie-doc.lisp:`   Automated Documentation Support
- `ctrie-test.lisp:`  Test and Performance Measurement

### Documentation

Comprehensive, HTML based [documentation](doc/api/index.html) may be
found at the project relative pathname `doc/api/index.html`.  In order
to enable support for supplemental documentation related features, such
as the ability to dynamically regenerate the provided documentation
files incorporating all updates and changes as may be present in the
source code, a (lightly) enhanced distribution of CLDOC is required
and may be obtained on [github](http://github.com/danlentz/cldoc).

The following sections provide a compact overview of the user api
and a reference to some internal definitions of interest.

#### User API Reference

The user api of cl-ctrie should be largely familiar to the average
common-lisp programmer.  Nearly all exported symbols of the CL-CTRIE 
package begin with the prefix "ctrie" and thus can be convenientely
incorporated via USE-PACKAGE or equivalent package definition.  The
following definitions comprise a quick reference to the the public
user api:

* * * * * *


_[structure]_        `CTRIE ()`

> A CTRIE structure is the root container that uniquely identifies a CTRIE
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
  and `ROOT-NODE-REPLACE`


_[function]_         `MAKE-CTRIE  (&REST ARGS &KEY NAME ROOT (READONLY-P NIL)
                                   (TEST 'EQUAL) (HASH 'SXHASH))`

> CREATE a new CTRIE instance. This is the entry-point constructor 
  intended for use by the end-user.


_[function]_         `CTRIE-P  (OBJECT)`

> Returns T if the specified object is of type ctrie.


_[function]_         `CTRIE-TEST  (CTRIE)`

> Returns the test of the specified ctrie



_[function]_         `CTRIE-HASH  (CTRIE)`

> Returns the hash of the specified ctrie



_[function]_         `CTRIE-READONLY-P  (CTRIE)`

> Returns and (with setf) changes the readonly-p of the specified ctrie



_[function]_         `CTRIE-PUT  (CTRIE KEY VALUE)`

_[function]_         `CTRIE-GET  (CTRIE KEY)`

_[function]_         `CTRIE-DROP  (CTRIE KEY)`

> Remove KEY and it's value from the CTRIE.


_[macro]_            `CTRIE-DO  ((KEY VALUE CTRIE &KEY ATOMIC) &BODY BODY)`

> Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t "~&~8S => ~10S~%" k v))


_[function]_         `CTRIE-MAP  (CTRIE FN &KEY ATOMIC &AUX ACCUM)`

_[function]_         `CTRIE-MAP-KEYS  (CTRIE FN &KEY ATOMIC)`

_[function]_         `CTRIE-MAP-VALUES  (CTRIE FN &KEY ATOMIC)`

_[generic-function]_ `CTRIE-MAP-INTO  (CTRIE PLACE FN)`

_[function]_         `CTRIE-KEYS  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-VALUES  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-SIZE  (CTRIE &AUX (ACCUM 0))`

_[function]_         `CTRIE-CLEAR  (CTRIE)`

_[function]_         `CTRIE-PPRINT  (CTRIE &OPTIONAL (STREAM T))`

_[function]_         `CTRIE-TO-ALIST  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-TO-HASHTABLE  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-FROM-HASHTABLE  (HASHTABLE)`

> create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE


_[function]_         `CTRIE-FROM-ALIST  (ALIST)`

_[function]_         `CTRIE-EMPTY-P  (CTRIE)`

_[method]_           `CTRIE-SAVE  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-SAVE  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-LOAD  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-LOAD  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-EXPORT  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-IMPORT  ((PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-IMPORT  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-IMPORT  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[function]_         `CTRIE-SNAPSHOT  (CTRIE &KEY READ-ONLY)`

_[macro]_            `CTRIE-ERROR  (CTRIE CONDITION &REST ARGS)`

> Signal a CTRIE related condition.


_[condition]_        `CTRIE-ERROR (ERROR)`

> Abstract superclass of CTRIE related conditions.


_[condition]_        `CTRIE-STRUCTURAL-ERROR (CTRIE-ERROR)`

> Condition designating that the CTRIE data structure
   has been determined to be invalid.


_[condition]_        `CTRIE-OPERATIONAL-ERROR (CTRIE-ERROR)`

> Conditixon for when an operational failure or
  inconsistency has occurred.


_[condition]_        `CTRIE-OPERATION-RETRIES-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*


_[condition]_        `CTRIE-NOT-IMPLEMENTED (CTRIE-ERROR)`

> Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded.


_[condition]_        `CTRIE-NOT-SUPPORTED (CTRIE-ERROR)`

> Condition designating functionality that is
  deliberately not supported.


_[condition]_        `CTRIE-INVALID-DYNAMIC-CONTEXT (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form


_[condition]_        `CTRIE-GENERATIONAL-MISMATCH (CTRIE-STRUCTURAL-ERROR)`

> Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal


* * * * * *

#### Internal Reference (abridged)

The following reference describes some selected internal
implementation details of interest.  Under normal circumstances it
should not be necessary to interact with these unexported symbols
unless developing an extension to cl-ctrie, but are presented here for
the sake of convenience, in order to provide better insight into the ctrie
structure in general, and to help illuminate significant aspects of this
implementation in particular.  As mentioned above, [comprehensive
documentation](doc/api/index.html) of all symbols is also provided,
and should be considered the authoratative reference to the CL-CTRIE
implementation.

* * * * * * *


_[special-variable]_ `*CTRIE*  (NIL)`

> Within the dynamic extent of a CTRIE operation this variable will
  be bound to the root-container CTRIE operand.  It is an error if an
  operation is defined that attempts access to a CTRIE without this
  binding, which is properly established by wrapping the operation in
  an appropriate WITH-CTRIE form.


_[special-variable]_ `*RETRIES*  (16)`

> Establishes the number of restarts permitted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-RETRIES-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.


_[special-variable]_ `*TIMEOUT*  (2)`

> Establishes the duration (in seconds) allotted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-TIMEOUT-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.


_[macro]_            `MULTI-CATCH  (TAG-LIST &BODY FORMS)`

> Macro allowing catch of multiple tags at once and
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
    ;;;                 (t ...)))


_[macro]_            `CATCH-CASE  (FORM &REST CASES)`

> User api encapsulating the MULTI-CATCH control-structure in a
    syntactic format that is identical to that of the familiar CASE
    statement, with the addition that within the scope of each CASE
    clause, a lexical binding is established between the symbol IT and
    the value caught from the throw form.


_[structure]_        `CTRIE ()`

> A CTRIE structure is the root container that uniquely identifies a CTRIE
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
  and `ROOT-NODE-REPLACE`


_[function]_         `CTRIE-P  (OBJECT)`

> Returns T if the specified object is of type ctrie.


_[function]_         `CTRIE-HASH  (CTRIE)`

> Returns the hash of the specified ctrie



_[function]_         `CTRIE-TEST  (CTRIE)`

> Returns the test of the specified ctrie



_[function]_         `CTRIE-READONLY-P  (CTRIE)`

> Returns and (with setf) changes the readonly-p of the specified ctrie



_[function]_         `CTHASH  (KEY)`

> Compute the hash value of KEY using the hash function defined by
  the CTRIE designated by the innermost enclosing WITH-CTRIE form.


_[function]_         `CTEQUAL  (X Y)`

> Test the equality of X and Y using the equality predicate defined
  by the CTRIE designated by the innermost enclosing WITH-CTRIE form.


_[macro]_            `WITH-CTRIE  (&ONCE CTRIE &BODY BODY)`

> Configure the dynamic environment with the appropriate condition handlers,
  control fixtures, and instrumentation necessary to perform the
  operations in BODY on the specified CTRIE. Unless specifically
  documented, the particular configuration of this dynamic environment
  should be considered an implementation detail and not relied upon. A
  particular exception, however, is that within the dynamic extent of
  a WITH-CTRIE form, the code implementing a CTRIE operation may
  expect that the special variable `*CTRIE*` will be bound to the root
  container of CTRIE operated upon.  See also the documentation for
  `*CTRIE*`


_[function]_         `FLAG  (KEY LEVEL)`

> For a given depth, LEVEL, within a CTRIE, extract the correspondant
  sequence of bits from the computed hash of KEY that indicate the
  logical index of the arc on the path to which that key may be found.
  Note that the logical index of the arc is most likely not the same
  as the physical index where it is actually located -- for that see
  `FLAG-ARC-POSITION`


_[function]_         `FLAG-PRESENT-P  (FLAG BITMAP)`

> Tests the (fixnum) BITMAP representing the logical index of all
  arcs present in a CNODE for the presence of a particular arc whose
  logical index is represented by FLAG.


_[function]_         `FLAG-ARC-POSITION  (FLAG BITMAP)`

> Given FLAG representing the logical index of an arc, and BITMAP
  representing all arcs present, compute a physical index for FLAG in
  such a manner as to always ensure all arcs map uniquely and
  contiguously to the smallest vector that can contain the given
  arcs.


_[function]_         `FLAG-VECTOR  (&OPTIONAL (CONTENT 0))`

> FLAG-VECTOR is a bit-vector representation of the (fixnum)
  BITMAP. It is currently not used for any calculation, however it is
  included within each CNODE as a convenience because it makes it
  immediately clear from visual inspection which logical arc indexes
  are represented in the node. For example, from the bit-vector
  `#*10010000000000000000000000000000` one can easily see that the first
  and fourth positions are occupied, and the rest empty.


_[structure]_        `REF ()`

> Atomically Stamped Reference structure _[Herlithy, TAOMP]_ that
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
     course of the `INODE-READ` / `INODE-COMMIT` arbitration process


_[function]_         `REF-P  (OBJECT)`

> Returns T if the specified object is of type ref.


_[function]_         `REF-STAMP  (REF)`

> Returns the stamp of the specified ref



_[function]_         `REF-VALUE  (REF)`

> Returns the value of the specified ref



_[function]_         `REF-PREV  (REF)`

> Returns and (with setf) changes the prev of the specified ref



_[structure]_        `FAILED-REF (REF)`

> A `FAILED-REF` is a structure that is used to preserve the linkage to
  prior inode state following a failed GCAS.  Any inode access that
  detects a `FAILED-REF` will immediately invoke a commit to restore the
  inode to the state recorded in `FAILED-REF-PREV`


_[function]_         `FAILED-REF-P  (OBJECT)`

> Returns T if the specified object is of type failed-ref.


_[structure]_        `INODE ()`

> An INODE, or 'Indirection Node' is the mutable structure
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
    the mutable content within an INODE


_[function]_         `INODE-P  (OBJECT)`

> Returns T if the specified object is of type inode.


_[function]_         `INODE-GEN  (INODE)`

> Returns the gen of the specified inode



_[function]_         `INODE-REF  (INODE)`

> Returns and (with setf) changes the ref of the specified inode



_[function]_         `MAKE-INODE  (LINK-TO &OPTIONAL GEN STAMP PREV)`

> Construct a new INODE that represents a reference to the value
  provided by argument LINK-TO, optionally augmented with a specified
  generational descriptor, timestamp, and/or previous state


_[macro]_            `GCAS-COMPARE-AND-SET  (OBJ EXPECTED NEW EXPECTED-STAMP
                                             NEW-STAMP PREV)`

> A thin, macro layer abstraction over the basic compare-and-swap
  primitive which provides a consistent interface to the underlying
  inode structure and manages additional metadata, providing
  reasonable defaults when they are not specified.


_[function]_         `INODE-READ  (INODE)`

> INODE-READ provides the top-level interface to the inode _GCAS ACCESS_
  api, which is the mechanism which must be used to gain access to the
  content of any NON-ROOT inode. For access to the root inode, refer
  to the RDCSS inode api `ROOT-NODE-ACCESS`. Returns as four values,
  the MAIN-NODE, the STAMP, the PREVIOUS STATE (if any), and the REF
  structure encapsulated by the inode.


_[function]_         `INODE-MUTATE  (INODE OLD-VALUE NEW-VALUE)`

> INODE-MUTATE provides the top-level interface to the inode _GCAS
  MODIFICATION_ api, which is the mechanism which must be used to
  effect any change in a NON-ROOT inode.  For modification of the
  root-inode, refer to the `ROOT-NODE-REPLACE` _RDCSS ROOT NODE
  PROTOCOL_ Returns a boolean value which indicates the success or
  failure of the modification attempt.


_[function]_         `INODE-COMMIT  (INODE REF)`

> INODE-COMMIT implements the _GCAS COMMIT_ protocol which is invoked
  as necessary by the `INODE-READ` and `INODE-MUTATE` entry-points.  It is
  not meant to be invoked directly, as this would most likely result
  in corruption. Returns the `REF` structure representing the content of
  whatever root inode wound up successfully committed -- either the
  one requested, or one represented by a previous valid state


_[function]_         `SNODE  (KEY VALUE)`

> Construct a new SNODE which represents the mapping from
  domain-element KEY to range-element VALUE.


_[structure]_        `SNODE ()`

> SNODE, i.e., 'Storage Node', is the LEAF-NODE structure ultimately
  used for the storage of each key/value pair contained in the CTRIE.
  An SNODE is considered to be immutable during its lifetime.
   - `KEY` defines the slot containing an element of the map's domain.
   - `VALUE` defines the slot containing the range-element mapped to `KEY`


_[function]_         `SNODE-P  (OBJECT)`

> Returns T if the specified object is of type snode.


_[function]_         `SNODE-KEY  (SNODE)`

> Returns the key of the specified snode



_[function]_         `SNODE-VALUE  (SNODE)`

> Returns the value of the specified snode



_[structure]_        `LNODE ()`

> LNODE, i.e., 'List Node', is a special structure used to enclose
  SNODES in a singly-linked chain when the hash-codes of the
  respective SNODE-KEYS collide, but those keys are determined to be
  unique by the `CTRIE-TEST` function defined for that ctrie.  An
  LNODE (and therefore a chain of LNODEs) is considered to be
  immutable during its lifetime.  The order of the list is
  implemented (arbitrarily) as most recently added first, analogous to
  `CL:PUSH`
   - `ELT` defines the slot containing an enclosed SNODE
   - `NEXT` defines a slot referencing the next LNODE in the chain, or
     `NIL` if no further LNODES remain.


_[function]_         `LNODE-P  (OBJECT)`

> Returns T if the specified object is of type lnode.


_[function]_         `LNODE-ELT  (LNODE)`

> Returns the elt of the specified lnode



_[function]_         `LNODE-NEXT  (LNODE)`

> Returns the next of the specified lnode



_[function]_         `ENLIST  (&REST REST)`

> Construct a chain of LNODE structures enclosing the values supplied.
  It is assumed (elsewhere) that each of these values is a valid SNODE
  structure.


_[function]_         `LNODE-REMOVED  (ORIG-LNODE KEY TEST)`

> Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but with any LNODE containing an SNODE equal to KEY
  removed.  Equality is tested as by the predicate function passed as
  the argument TEST. The order of nodes in the resulting list will
  remain unchanged.


_[function]_         `LNODE-INSERTED  (ORIG-LNODE KEY VALUE TEST)`

> Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but ensured to contain an LNODE enclosing an SNODE mapping
  KEY to VALUE.  If the given KEY equal to a key already present somewhere
  in the chain (as compared with equality predicate TEST) it will be
  replaced.  Otherwise a new LNODE will be added. In either case, the LNODE
  containing `(SNODE KEY VAlUE)` will be the first node in the resulting
  list


_[function]_         `LNODE-SEARCH  (LNODE KEY TEST)`

> Within the list of lnodes beginning with LNODE, return the range value
  mapped by the first SNODE containing a key equal to KEY as determined
  by equality predicate TEST, or `NIL` if no such key is found.  As a
  second value, in order to support storage of `NIL` as a key, return `T` to
  indicate that the KEY was indeed found during search, or `NIL` to indicate
  that no such key was present in the list


_[function]_         `LNODE-LENGTH  (LNODE)`

> Return the number of LNODES present in the chain beginning at LNODE


_[structure]_        `TNODE ()`

> A TNODE, or 'Tomb Node', is a special node structure used to preserve
  ordering during `CTRIE-DROP` (`%remove`) operations.
  Any time a TNODE is encountered during the course of a `CTRIE-GET` (`%lookup`)
  operation, the operative thread is required to invoke a `CLEAN` operation
  on the TNODE it has encountered and throw to `:RESTART` its lookup activity
  over again.  A TNODE is considered to be immutable and may not change its
  value during its lifetime.
   - `CELL` defines a slot which contains the entombed node structure.
      Only LNODE and SNODE type nodes are ever entombed


_[function]_         `TNODE-P  (OBJECT)`

> Returns T if the specified object is of type tnode.


_[function]_         `TNODE-CELL  (TNODE)`

> Returns the cell of the specified tnode



_[method]_           `ENTOMB  ((SNODE SNODE))`

> Entomb an SNODE in a newly created TNODE


_[method]_           `ENTOMB  ((LNODE LNODE))`

> Entomb an LNODE in a newly created TNODE


_[method]_           `ENTOMB  (NODE)`

> Unless the provided argument is of a type for which an entombment
    specialization has been defined, signal an error, as we have arrived
    at an undefined state and cannot continue processing.


_[generic-function]_ `ENTOMB  (NODE)`

> Return a newly constructed TNODE enclosing the argument
  LEAF-NODE structure `NODE`


_[generic-function]_ `RESURRECT  (NODE)`

> Return the 'resurection' of the NODE argument.  The
  resurrection of an INODE that references a TNODE is the
  LEAF-NODE structure entombed by that TNODE.  The resurrection of
  any other node is simply itself.


_[structure]_        `CNODE ()`

> A CNODE, or 'Ctrie Node' is a MAIN-NODE containing a vector of up
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
   - `FLAGS`
   - `ARCS` 


_[function]_         `CNODE-P  (OBJECT)`

> Returns T if the specified object is of type cnode.


_[function]_         `MAKE-CNODE  (&OPTIONAL (BITMAP 0))`

> Construct a CNODE with internal storage allocated for the number of
  arcs equal to the Hamming-Weight of the supplied BITMAP parameter.
  If no BITMAP is provided, the CNODE created will be empty -- a state
  which is only valid for the level 0 node referenced by the root of
  the CTRIE.  This constructor is otherwise never called directly, but
  is invoked during the course of higher-level operations such as
  `CNODE-EXTENDED` `CNODE-UPDATED` `CNODE-TRUNCATED` and `MAP-CNODE`


_[function]_         `CNODE-EXTENDED  (CNODE FLAG POSITION NEW-ARC)`

> Construct a new cnode structure that is exactly like CNODE, but
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
  `FLAG-ARC-POSITION`


_[function]_         `CNODE-UPDATED  (CNODE POSITION REPLACEMENT-ARC)`

> Construct a new cnode structure identical to CNODE, but having the
  BRANCH-NODE physically located at POSITION within the storage
  vector replaced by the one specified by REPLACEMENT-ARC.  Unlike
  `CNODE-EXTENDED` and `CNODE-TRUNCATED` the allocated storage and
  existing BITMAP of this CNODE will remain unchanged (as this is
  simply a one-for-one replacement) and correspondingly, no reordering
  of other nodes within the storage vector will occur


_[function]_         `CNODE-TRUNCATED  (CNODE FLAG POS)`

> Construct a new cnode structure that is exactly like CNODE, but
 with the arc at logical index FLAG and physical storage vector
 location POS removed.  The new CNODE will have an updated bitmap
 value that is adusted to reflect the removal of this arc, and the
 position of other arcs within the storage vector of the new CNODE
 will be adjusted in a manner analogous to that of `CNODE-EXTENDED`
 More details on this process may be found by referring to the
 documentation for the functions `FLAG` `FLAG-VECTOR` and
 `FLAG-ARC-POSITION`


_[function]_         `MAP-CNODE  (FN CNODE)`

> Construct a new cnode structure that is exactly like CNODE, but
  with each arc (BRANCH-NODE) present in CNODE replaced by the result
  of applying FN to that arc.  I.e., a simple functional mapping from
  the old CNODE by FN.  As with `CNODE-UPDATED` the allocated storage and
  BITMAP of the resulting CNODE will remain unchanged from the
  original, and no physical reordering of nodes within the storage
  vector will occur


_[method]_           `REFRESH  ((SNODE SNODE) GEN)`

> An SNODE represents a LEAF storage cell and does not require
    any coordination with generational descriptors, and so is simply
    returned as-is. 


_[method]_           `REFRESH  ((INODE INODE) GEN)`

> Generate a replacement for inode that continues to reference the
    same MAIN-NODE as before, but otherwise contains the new generational
    descriptor GEN, and a new REF substructure initialized
    with freshly generated metadata, unconditionally discarding the old.
    Note that the refresh of an inode is not transitive to the nodes contained
    in the portion of the CTRIE that it references.  I.e., the process does
    not eagerly descend and propagate until needed, eliminating the
    overhead incurred by full traversals which, in many situations, turn out
    to be not even necessary.


_[method]_           `REFRESH  ((CNODE CNODE) GEN)`

> Return a new cnode structure identical to CNODE, but with any
    arcs that are INODES refreshed to generational descriptor GEN


_[generic-function]_ `REFRESH  (PLACE GEN)`

> Reconcile the node specified by PLACE with an
  updated generational descriptor object, GEN. The actions required
  for this reconciliation vary according to the node type and 
  specializations are defined on a casewise basis.


_[function]_         `CNODE-CONTRACTED  (CNODE LEVEL)`

> The _CONTRACTION_ of a CNODE is an ajustment performed when a CNODE
  at depth other than level 0 contains only a single SNODE arc.  In
  such a case, that SNODE is entombed in a new TNODE, which is
  returned as the result of the CNODE contraction. In all other cases
  the CNODE is simply returned as-is.  A CONTRACTION represents the
  first of the two-step _ARC RETRACTION PROTOCOL_ that effects the reclaimation
  of allocated storage no longer used and the optimization of of lookup
  efficiency by compacting CTRIE depth and thereby the number of levels
  which must be traversed.  For further information, refer to the function
  `CNODE-COMPRESSED` which implements the second stage of this protocol,
  completing the process.


_[function]_         `CNODE-COMPRESSED  (CNODE LEVEL)`

> The _COMPRESSION_ of a CNODE is the second step of the _ARC
  RETRACTION PROTOCOL_ completing a retraction that has been initiated
  by `CNODE-CONTRACTED`.  The CNODE compression is computed by
  generating a replacement cnode structure that is similar to CNODE,
  but with any entombed inode arcs created during contraction simply
  replaced by the SNODE that had been entombed. This is called the
  _RESURRECTION_ of that SNODE. After all entombed inode arcs of a
  cnode have been collapsed into simple SNODE leaves, if the resulting
  CNODE has been compressed down to only a single SNODE leaf, it is
  subjected to another CONTRACTION before it is returned as the result
  of the compression and completes the _ARC RETRACTION PROTOCOL_


_[function]_         `CLEAN  (INODE LEVEL)`

> CLEAN is the basic entry-point into the arc retraction protocol. Given an
  arbitrary, non-root inode referencing a CNODE that can be compressed,
  update that inode to reference the result of that compression.  Otherwise
  INODE will remain unaffected.


_[function]_         `CLEAN-PARENT  (PARENT-INODE TARGET-INODE KEY LEVEL)`

_[generic-function]_ `LEAF-NODE-KEY  (RESOURCE)`

_[generic-function]_ `LEAF-NODE-VALUE  (RESOURCE)`

_[generic-function]_ `FIND-CTRIE-ROOT  (CTRIE-DESIGNATOR)`

> FIND-CTRIE-ROOT is a subprimitive used by the
  internal CTRIE implementation to access the root inode of a given
  ctrie root-container. It does not provide safe access to the
  contents of that inode and should not be referenced by the
  higher-level implementation or end-user code.  The purpose of
  FIND-CTRIE-ROOT is to incorporate a level of indirection specialized
  on the class of the root container to facilitate future extension
  with alternate storage models, e.g., an external persistent disk-based
  store. See {defgeneric (cas cl-ctrie::find-ctrie-root)}


_[structure]_        `RDCSS-DESCRIPTOR ()`

> An RDCSS-DESCRIPTOR object represents a 'plan' for a proposed RDCSS
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
                successfully.


_[function]_         `RDCSS-DESCRIPTOR-P  (OBJECT)`

> Returns T if the specified object is of type rdcss-descriptor.


_[function]_         `RDCSS-DESCRIPTOR-OV  (RDCSS-DESCRIPTOR)`

> Returns the ov of the specified rdcss-descriptor



_[function]_         `RDCSS-DESCRIPTOR-OVMAIN  (RDCSS-DESCRIPTOR)`

> Returns the ovmain of the specified rdcss-descriptor



_[function]_         `RDCSS-DESCRIPTOR-NV  (RDCSS-DESCRIPTOR)`

> Returns the nv of the specified rdcss-descriptor



_[function]_         `RDCSS-DESCRIPTOR-COMMITTED  (RDCSS-DESCRIPTOR)`

> Returns and (with setf) changes the committed of the specified rdcss-descriptor



_[function]_         `ROOT-NODE-ACCESS  (CTRIE &OPTIONAL ABORT)`

> ROOT-NODE-ACCESS extends {defgeneric cl-ctrie::FIND-CTRIE-ROOT},
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
  undrlying GCAS protocol implementation for inodes.


_[function]_         `ROOT-NODE-REPLACE  (CTRIE OV OVMAIN NV)`

> rdcss api for replacement of root ctrie inode


_[function]_         `ROOT-NODE-COMMIT  (CTRIE &OPTIONAL ABORT)`

> rdcss api to complete a root-node transaction


_[function]_         `CTRIE-SNAPSHOT  (CTRIE &KEY READ-ONLY)`

_[function]_         `CTRIE-CLEAR  (CTRIE)`

_[function]_         `CTRIE-PUT  (CTRIE KEY VALUE)`

_[function]_         `%INSERT  (INODE KEY VALUE LEVEL PARENT STARTGEN)`

_[function]_         `CTRIE-GET  (CTRIE KEY)`

_[function]_         `%LOOKUP  (INODE KEY LEVEL PARENT STARTGEN)`

> The general concept of the procedure for finding a given key within
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
  a leaf-node or empty arc position.

    Locating a given key becomes substantially more complicated in
  the actual lock-free concurrent ctrie algorithm.  


_[function]_         `CTRIE-DROP  (CTRIE KEY)`

> Remove KEY and it's value from the CTRIE.


_[function]_         `%REMOVE  (INODE KEY LEVEL PARENT STARTGEN)`

_[function]_         `CTRIE-MAP  (CTRIE FN &KEY ATOMIC &AUX ACCUM)`

_[macro]_            `CTRIE-DO  ((KEY VALUE CTRIE &KEY ATOMIC) &BODY BODY)`

> Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t "~&~8S => ~10S~%" k v))


_[method]_           `MAP-NODE  ((NODE CNODE) FN)`

_[method]_           `MAP-NODE  ((NODE LNODE) FN)`

_[method]_           `MAP-NODE  ((NODE TNODE) FN)`

_[method]_           `MAP-NODE  ((NODE SNODE) FN)`

_[method]_           `MAP-NODE  ((NODE INODE) FN)`

_[generic-function]_ `MAP-NODE  (NODE FN)`

_[function]_         `CTRIE-MAP-KEYS  (CTRIE FN &KEY ATOMIC)`

_[function]_         `CTRIE-MAP-VALUES  (CTRIE FN &KEY ATOMIC)`

_[generic-function]_ `CTRIE-MAP-INTO  (CTRIE PLACE FN)`

_[function]_         `CTRIE-KEYS  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-VALUES  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-SIZE  (CTRIE &AUX (ACCUM 0))`

_[function]_         `CTRIE-EMPTY-P  (CTRIE)`

_[function]_         `CTRIE-TO-ALIST  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-TO-HASHTABLE  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-PPRINT  (CTRIE &OPTIONAL (STREAM T))`

_[function]_         `CTRIE-FROM-ALIST  (ALIST)`

_[function]_         `CTRIE-FROM-HASHTABLE  (HASHTABLE)`

> create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE


_[method]_           `CTRIE-SAVE  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-SAVE  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-LOAD  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-LOAD  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-EXPORT  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[method]_           `CTRIE-IMPORT  ((PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-IMPORT  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-IMPORT  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[macro]_            `CTRIE-ERROR  (CTRIE CONDITION &REST ARGS)`

> Signal a CTRIE related condition.


_[condition]_        `CTRIE-ERROR (ERROR)`

> Abstract superclass of CTRIE related conditions.


_[condition]_        `CTRIE-STRUCTURAL-ERROR (CTRIE-ERROR)`

> Condition designating that the CTRIE data structure
   has been determined to be invalid.


_[condition]_        `CTRIE-OPERATIONAL-ERROR (CTRIE-ERROR)`

> Conditixon for when an operational failure or
  inconsistency has occurred.


_[condition]_        `CTRIE-OPERATION-RETRIES-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*


_[condition]_        `CTRIE-NOT-IMPLEMENTED (CTRIE-ERROR)`

> Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded.


_[condition]_        `CTRIE-NOT-SUPPORTED (CTRIE-ERROR)`

> Condition designating functionality that is
  deliberately not supported.


_[condition]_        `CTRIE-INVALID-DYNAMIC-CONTEXT (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form


_[condition]_        `CTRIE-GENERATIONAL-MISMATCH (CTRIE-STRUCTURAL-ERROR)`

> Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal


_[function]_         `README  (&OPTIONAL (STREAM *STANDARD-OUTPUT*))`

> Update documentation sections of the README file. When an output stream
  is specified, the results are also echoed to that stream. To inhibit
  output, invoke as `(readme (make-broadcast-stream))` or use `README-QUIETLY`


_[function]_         `README-QUIETLY  ()`

> Update documentation sections of the README file, inhibiting any other
  printed output.


_[function]_         `APIDOC  (&OPTIONAL (SCOPE :EXTERNAL))`

> Collect a list of strings representing the documentation for
  CL-CTRIE rendered in a compact format suitable for inclusion in a
  lightweight text-markup format document.  If SCOPE is specified it
  must be either :EXTERNAL. corresponding to those symbols exported as
  the public API, or :HOME, which designates all symbols defined
  locally in package.


_[function]_         `PRINC-APIDOC  (&OPTIONAL (SCOPE :EXTERNAL))`

> Print to `*STANDARD-OUTPUT*` the documentation for CL-CTRIE rendered
  in a compact format.  This is intended primarily as a convenience to
  the interactive user seeking quick reference at the REPL.  If SCOPE
  is specified it must be either :EXTERNAL. corresponding to those
  symbols exported as the public API, or :HOME, which designates all
  symbols defined locally in package.


_[function]_         `COLLECT-DOCS  (&OPTIONAL (SCOPE :EXTERNAL)
                                     (SORT #'STRING<))`

> Regenerate on-disk html documentation and collect the cached
  in-memory descriptors for further processing. If SCOPE is specified
  it must be either :EXTERNAL. corresponding to those symbols exported
  as the public API, or :HOME, which designates all symbols defined
  locally in package.  Output order may be customized by an optionally
  specified SORT function.


_[macro]_            `DEFINE-DIAGRAM  (TYPE (&OPTIONAL CONTEXT) &BODY BODY)`

> Define a diagrammatic representation of TYPE, optionally specialized
  for a specific CONTEXT. See {defgeneric cl-ctrie::make-diagram}.


* * * * * * *

generic cl-ctrie::make-diagram}.


* * * * * * *

