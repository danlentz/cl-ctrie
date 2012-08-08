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

### Supported Lisp Implementations ###

Currently the lisp platform supported by cl-ctrie is SBCL version
1.0.55 or greater, although support could easily be entended to
other common-lisp implementations that offer support for atomic
compare-and-swap functionality, notably LispWorks 5.x/6.x, which is
also well instrumented with lock-free, atomic primitives, although
this is not necessarily a high priority for the initial development
cycle.

### Current Status


### Ctrie Structure

```common-lisp

```

### Comprehensive Documentation

Complete, HTML based [documentation](./doc/api/index.html) may be
found at the project relative pathname doc/api/index.html


### API Reference

The user api of cl-ctrie is should be familiar to the average
common-lisp programmer.  All exported symbols of the CL-CTRIE package
begin with the prefix "ctrie" and thus can be convenientely
incorporated via USE-PACKAGE or equivalent package definition.  The
following definitions comprise the public user api:

* * * * *

_[function]_ `CTRIE-CLEAR           (CTRIE)`

>  

_[macro]_ `CTRIE-DO              ((KEY VALUE CTRIE &KEY ATOMIC) &BODY BODY)`

>  Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t "~&~8S => ~10S~%" k v))

_[function]_ `CTRIE-DROP            (CTRIE KEY)`

>  Remove KEY and it's value from the CTRIE.

_[function]_ `CTRIE-EMPTY-P         (CTRIE)`

>  NIL

_[function]_ `CTRIE-ENSURE-GET      (CTRIE KEY DEFAULT)`

>  Like CTRIE-GET, but if KEY is not found in CTRIE, automatically adds 
   the entry (KEY. DEFAULT). Secondary return value is true if key was
   already in the table.

_[macro]_ `CTRIE-ERROR           (CTRIE CONDITION &REST ARGS)`

>  Signal a CTRIE related condition.

_[condition]_ `CTRIE-ERROR          (ERROR)`

>  Abstract superclass of CTRIE related conditions.

_[method]_ `CTRIE-EXPORT          ((CTRIE CTRIE) (PLACE HASH-TABLE) &KEY)`

>  NIL

_[method]_ `CTRIE-EXPORT          ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-EXPORT          (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[function]_ `CTRIE-FROM-ALIST      (ALIST)`

>  NIL

_[function]_ `CTRIE-FROM-HASHTABLE  (HASHTABLE)`

>  create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE

_[condition]_ `CTRIE-GENERATIONAL-MISMATCH (CTRIE-STRUCTURAL-ERROR)`

>  Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal

_[function]_ `CTRIE-GET             (CTRIE KEY)`

>  NIL

_[function]_ `CTRIE-HASH            (CTRIE)`

>  Returns and (with setf) changes the hash of the specified ctrie


_[method]_ `CTRIE-IMPORT          ((PLACE HASH-TABLE) &KEY)`

>  NIL

_[method]_ `CTRIE-IMPORT          ((PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-IMPORT          (PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[condition]_ `CTRIE-INVALID-DYNAMIC-CONTEXT (CTRIE-OPERATIONAL-ERROR)`

>  Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form

_[function]_ `CTRIE-KEYS            (CTRIE &KEY ATOMIC)`

>  NIL

_[method]_ `CTRIE-LOAD            ((PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-LOAD            (PLACE &KEY &ALLOW-OTHER-KEYS)`

>  NIL

_[function]_ `CTRIE-MAP             (CTRIE FN &KEY ATOMIC &AUX ACCUM)`

>  NIL

_[generic-function]_ `CTRIE-MAP-INTO        (CTRIE PLACE FN)`

>  NIL

_[function]_ `CTRIE-MAP-KEYS        (CTRIE FN &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-MAP-VALUES      (CTRIE FN &KEY ATOMIC)`

>  NIL

_[condition]_ `CTRIE-NOT-IMPLEMENTED (CTRIE-ERROR)`

>  Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded.

_[condition]_ `CTRIE-NOT-SUPPORTED  (CTRIE-ERROR)`

>  Condition designating functionality that is
  deliberately not supported.

_[condition]_ `CTRIE-OPERATION-RETRIES-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

>  Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*

_[condition]_ `CTRIE-OPERATIONAL-ERROR (CTRIE-ERROR)`

>  Conditixon for when an operational failure or
  inconsistency has occurred.

_[function]_ `CTRIE-PPRINT          (CTRIE &OPTIONAL (STREAM T))`

>  NIL

_[function]_ `CTRIE-PUT             (CTRIE KEY VALUE)`

>  NIL

_[function]_ `CTRIE-READONLY-P      (CTRIE)`

>  Returns and (with setf) changes the readonly-p of the specified ctrie


_[method]_ `CTRIE-SAVE            ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-SAVE            (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[function]_ `CTRIE-SIZE            (CTRIE &AUX (ACCUM 0))`

>  NIL

_[function]_ `CTRIE-SNAPSHOT        (CTRIE &KEY READ-ONLY)`

>  

_[condition]_ `CTRIE-STRUCTURAL-ERROR (CTRIE-ERROR)`

>  Condition designating that the CTRIE data structure
   has been determined to be invalid.

_[function]_ `CTRIE-TEST            (CTRIE)`

>  Returns and (with setf) changes the test of the specified ctrie


_[function]_ `CTRIE-TO-ALIST        (CTRIE &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-TO-HASHTABLE    (CTRIE &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-VALUES          (CTRIE &KEY ATOMIC)`

>  NIL

_[function]_ `MAKE-CTRIE            (&REST ARGS &KEY NAME ROOT (READONLY-P NIL)
                                     (TEST 'EQUAL) (HASH 'SXHASH))`

>  CREATE a new CTRIE instance. This is the entry-point constructor api
  intended for use by the end-user.
  

### Internal Reference

The following reference describes the internal implementation specific
definitions which comprise cl-ctrie.  Under normal circumstances it 
should no be necessary to interact with these unexported symbols unless
developing an extension to cl-ctrie, but are presented here for the sake
of completeness, and in order to provide better insight into the ctrie
structure in general, and this implementation in particular.

* * * * *

_[special-variable]_ `%EMPTY-MAP%           ((VECTOR))`

>  Defines the initial value of an empty CNODE arc-vector.

_[function]_ `%INSERT               (INODE KEY VALUE LEVEL PARENT STARTGEN)`

>  NIL

_[function]_ `%LOOKUP               (INODE KEY LEVEL PARENT STARTGEN)`

>  NIL

_[function]_ `%MAKE-CNODE           (&KEY (BITMAP 0) (FLAGS %NO-FLAGS%)
                                     (ARCS %EMPTY-MAP%))`

>  Returns a newly created cnode.

_[function]_ `%MAKE-CTRIE           (&KEY READONLY-P (TEST 'EQUAL)
                                     (HASH 'SXHASH)
                                     (ROOT
                                      (MAKE-INODE (MAKE-CNODE)
                                                  (GENSYM "ctrie"))))`

>  Returns a newly created ctrie.

_[function]_ `%MAKE-INODE           (&KEY GEN REF)`

>  Returns a newly created inode.

_[special-variable]_ `%NO-FLAGS%            ((FLAG-VECTOR))`

>  Defines the initial value of a flag-vector representing a
  cnode containing an empty arc-vector.

_[function]_ `%REMOVE               (INODE KEY LEVEL PARENT STARTGEN)`

>  NIL

_[special-variable]_ `*BREAK*               (T)`

>  special variable used for dynamic control of break loops see {defun :break}

_[special-variable]_ `*CONTEXT*             (NIL)`

>  Diagnostic value, not used in production, used to maintain
  additional information tracking the current dynamic state.

_[special-variable]_ `*CTRIE*               (NIL)`

>  Within the dynamic extent of a CTRIE operation this variable will
  be bound to the root-container CTRIE operand.  It is an error if an
  operation is defined that attempts access to a CTRIE without this
  binding, which is properly established by wrapping the operation in
  an appropriate WITH-CTRIE form.

_[special-variable]_ `*DEBUG*               (NIL)`

>  Debugging flag, not used in production, to enable generation of
  additional diagnostic and reporting information.

_[special-variable]_ `*RETRIES*             (16)`

>  Establishes the number of restarts permitted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-RETRIES-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.

_[special-variable]_ `*TIMEOUT*             (2)`

>  Establishes the duration (in seconds) allotted to a CTRIE operation
  established by a WITH-CTRIE form before a condition of type
  CTRIE-OPERATION-TIMEOUT-EXCEEDED will be signaled, aborting the
  operatation, and requiring operator intervention to resume
  processing.

_[function]_ `^                     (THING &OPTIONAL WAIT)`

>  inspect THING in the emacs SLIME inspector, optionally waiting
  for the inspector to be dismissed before continuing if WAIT is
  not null

_[function]_ `ALL-DESCS             (DOCS)`

>  Collect all 'descriptors' from an alist of the form
  '((symbol . descriptor-list) ... ) such as one generated by
  {defun:collect-docs}

_[macro]_ `ANAPHORIC             (OP TEST &BODY BODY)`

>  higher-order anaphoric operator creation macro.

_[function]_ `APIDOC                (&OPTIONAL (SCOPE :EXTERNAL))`

>  Collect a list of strings representing the documentation for
  CL-CTRIE rendered in a compact format suitable for inclusion in a
  lightweight text-markup format document.  If SCOPE is specified it
  must be either :EXTERNAL. corresponding to those symbols exported as
  the public API, or :HOME, which designates all symbols defined
  locally in package.

_[macro]_ `APROG1                (FIRST &BODY REST)`

>  Binds IT to the first form so that it can be used in the rest of the
  forms. The whole thing returns IT.

_[macro]_ `ATOMIC-UPDATE         (PLACE UPDATE-FN &REST ARGUMENTS &ENVIRONMENT
                                  ENV)`

>  Updates PLACE atomically to the value returned by calling function
  designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.
  PLACE may be read and UPDATE-FN evaluated and called multiple times before the
  update succeeds: atomicity in this context means that value of place did not
  change between the time it was read, and the time it was replaced with the
  computed value. PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.
  EXAMPLE: Conses T to the head of FOO-LIST:
  ;;;   (defstruct foo list)
  ;;;   (defvar *foo* (make-foo))
  ;;;   (atomic-update (foo-list *foo*) #'cons t)

_[macro]_ `ATYPECASE             (KEYFORM &BODY CASES)`

>  Like TYPECASE, except binds the result of the keyform to IT (via LET) for
  the scope of the cases.

_[macro]_ `AWHEN                 (TEST &BODY BODY)`

>  Like WHEN, except binds the result of the test to IT (via LET) for the scope
  of the body.

NIL_[function]_ `BYTE-VECTOR-TO-HEX-STRING  (VECTOR)`

>  Return a 32 character string that maps uniquely to the given byte vector.

_[function]_ `CAS-BYTE-SAP          (SAP OLD NEW)`

>  NIL

_[function]_ `CAS-WORD-SAP          (SAP OLD NEW)`

>  NIL

_[macro]_ `CATCH-CASE            (FORM &REST CASES)`

>  User api encapsulating the MULTI-CATCH control-structure in a
    syntactic format that is identical to that of the familiar CASE
    statement, with the addition that within the scope of each CASE
    clause, a lexical binding is established between the symbol IT and
    the value caught from the throw form.

_[function]_ `CLEAN                 (INODE LEVEL)`

>  NIL

_[function]_ `CLEAN-PARENT          (PARENT-INODE TARGET-INODE KEY LEVEL)`

>  NIL

NIL_[function]_ `CNODE-ARCS            (CNODE)`

>  Returns and (with setf) changes the arcs of the specified cnode


_[function]_ `CNODE-BITMAP          (CNODE)`

>  Returns and (with setf) changes the bitmap of the specified cnode


_[function]_ `CNODE-COMPRESSED      (CNODE LEVEL)`

>  NIL

_[function]_ `CNODE-CONTRACTED      (CNODE LEVEL)`

>  NIL

_[function]_ `CNODE-EXTENDED        (CNODE FLAG POSITION VALUE)`

>  NIL

_[function]_ `CNODE-FLAGS           (CNODE)`

>  Returns and (with setf) changes the flags of the specified cnode


_[function]_ `CNODE-P               (OBJECT)`

>  Returns T if the specified object is of type cnode.

_[function]_ `CNODE-TRUNCATED       (CNODE FLAG POS)`

>  NIL

_[function]_ `CNODE-UPDATED         (CNODE POSITION VALUE)`

>  NIL

_[function]_ `COLLECT-DOCS          (&OPTIONAL (SCOPE :EXTERNAL)
                                     (SORT #'STRING<))`

>  Regenerate on-disk html documentation and collect the cached
  in-memory descriptors for further processing. If SCOPE is specified
  it must be either :EXTERNAL. corresponding to those symbols exported
  as the public API, or :HOME, which designates all symbols defined
  locally in package.  Output order may be customized by an optionally
  specified SORT function.

_[function]_ `COLLECT-KEYS          (X Y)`

>  NIL

_[function]_ `COLLECT-VALUES        (X Y)`

>  NIL

_[function]_ `COLLECT2              (X Y)`

>  NIL

_[function]_ `COPY-CTRIE            (CTRIE)`

>  Returns a copy of the specified ctrie.

_[function]_ `COPY-RDCSS-DESCRIPTOR  (RDCSS-DESCRIPTOR)`

>  Returns a copy of the specified rdcss-descriptor.

_[function]_ `CREATE-NULL-ID-BYTE-VECTOR  NIL`

>  Generate a 16-byte vector representing the NULL uuid.

_[function]_ `CREATE-UNIQUE-ID-BYTE-VECTOR  NIL`

>  Create a universally unique 16-byte vector using unicly or uuid
  libraries if available, or else fall back to random generation.

_[function]_ `CTEQUAL               (X Y)`

>  Test the equality of X and Y using the equality predicate defined
  by the CTRIE designated by the innermost enclosing WITH-CTRIE form.

_[function]_ `CTHASH                (KEY)`

>  Compute the hash value of KEY using the hash function defined by
  the CTRIE designated by the innermost enclosing WITH-CTRIE form.

NIL_[function]_ `CTRIE-CLEAR           (CTRIE)`

>  

_[generic-function]_ `CTRIE-CONTEXT         (CTRIE-INVALID-DYNAMIC-CONTEXT)`

>  Returns the ctrie-context of the specified ctrie-invalid-dynamic-context


_[macro]_ `CTRIE-DO              ((KEY VALUE CTRIE &KEY ATOMIC) &BODY BODY)`

>  Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t "~&~8S => ~10S~%" k v))

_[function]_ `CTRIE-DROP            (CTRIE KEY)`

>  Remove KEY and it's value from the CTRIE.

_[function]_ `CTRIE-EMPTY-P         (CTRIE)`

>  NIL

_[function]_ `CTRIE-ENSURE-GET      (CTRIE KEY DEFAULT)`

>  Like CTRIE-GET, but if KEY is not found in CTRIE, automatically adds 
   the entry (KEY. DEFAULT). Secondary return value is true if key was
   already in the table.

_[macro]_ `CTRIE-ERROR           (CTRIE CONDITION &REST ARGS)`

>  Signal a CTRIE related condition.

_[condition]_ `CTRIE-ERROR          (ERROR)`

>  Abstract superclass of CTRIE related conditions.

_[method]_ `CTRIE-EXPORT          ((CTRIE CTRIE) (PLACE HASH-TABLE) &KEY)`

>  NIL

_[method]_ `CTRIE-EXPORT          ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-EXPORT          (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[function]_ `CTRIE-FROM-ALIST      (ALIST)`

>  NIL

_[function]_ `CTRIE-FROM-HASHTABLE  (HASHTABLE)`

>  create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE

_[condition]_ `CTRIE-GENERATIONAL-MISMATCH (CTRIE-STRUCTURAL-ERROR)`

>  Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal

_[function]_ `CTRIE-GET             (CTRIE KEY)`

>  NIL

_[function]_ `CTRIE-HASH            (CTRIE)`

>  Returns and (with setf) changes the hash of the specified ctrie


_[method]_ `CTRIE-IMPORT          ((PLACE HASH-TABLE) &KEY)`

>  NIL

_[method]_ `CTRIE-IMPORT          ((PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-IMPORT          (PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[condition]_ `CTRIE-INVALID-DYNAMIC-CONTEXT (CTRIE-OPERATIONAL-ERROR)`

>  Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form

_[function]_ `CTRIE-KEYS            (CTRIE &KEY ATOMIC)`

>  NIL

_[method]_ `CTRIE-LOAD            ((PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-LOAD            (PLACE &KEY &ALLOW-OTHER-KEYS)`

>  NIL

_[function]_ `CTRIE-MAP             (CTRIE FN &KEY ATOMIC &AUX ACCUM)`

>  NIL

_[generic-function]_ `CTRIE-MAP-INTO        (CTRIE PLACE FN)`

>  NIL

_[function]_ `CTRIE-MAP-KEYS        (CTRIE FN &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-MAP-VALUES      (CTRIE FN &KEY ATOMIC)`

>  NIL

_[condition]_ `CTRIE-NOT-IMPLEMENTED (CTRIE-ERROR)`

>  Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded.

_[condition]_ `CTRIE-NOT-SUPPORTED  (CTRIE-ERROR)`

>  Condition designating functionality that is
  deliberately not supported.

_[generic-function]_ `CTRIE-OF              (CTRIE-ERROR)`

>  Returns the ctrie of the specified ctrie-error


_[condition]_ `CTRIE-OPERATION-RETRIES-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

>  Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*

_[condition]_ `CTRIE-OPERATION-TIMEOUT-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

>  Condition indicating an operation has failed the
   maximum number of times specified by the s-variable *retries*

_[condition]_ `CTRIE-OPERATIONAL-ERROR (CTRIE-ERROR)`

>  Conditixon for when an operational failure or
  inconsistency has occurred.

_[function]_ `CTRIE-P               (OBJECT)`

>  Returns T if the specified object is of type ctrie.

_[function]_ `CTRIE-PPRINT          (CTRIE &OPTIONAL (STREAM T))`

>  NIL

_[function]_ `CTRIE-PUT             (CTRIE KEY VALUE)`

>  NIL

_[function]_ `CTRIE-READONLY-P      (CTRIE)`

>  Returns and (with setf) changes the readonly-p of the specified ctrie


_[function]_ `CTRIE-ROOT            (CTRIE)`

>  Returns and (with setf) changes the root of the specified ctrie


_[method]_ `CTRIE-SAVE            ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

>  NIL

_[generic-function]_ `CTRIE-SAVE            (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

>  

_[function]_ `CTRIE-SIZE            (CTRIE &AUX (ACCUM 0))`

>  NIL

_[function]_ `CTRIE-SNAPSHOT        (CTRIE &KEY READ-ONLY)`

>  

_[condition]_ `CTRIE-STRUCTURAL-ERROR (CTRIE-ERROR)`

>  Condition designating that the CTRIE data structure
   has been determined to be invalid.

_[function]_ `CTRIE-TEST            (CTRIE)`

>  Returns and (with setf) changes the test of the specified ctrie


_[function]_ `CTRIE-TO-ALIST        (CTRIE &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-TO-HASHTABLE    (CTRIE &KEY ATOMIC)`

>  NIL

_[function]_ `CTRIE-VALUES          (CTRIE &KEY ATOMIC)`

>  NIL

_[macro]_ `DEFINE-DIAGRAM        (TYPE (&OPTIONAL CONTEXT) &BODY BODY)`

>  define a diagrammatic representation of TYPE, optionally specialized
  for a specific CONTEXT. See {defgeneric cl-ctrie::make-diagram}.

_[macro]_ `DEFLEX                (VAR VAL &OPTIONAL (DOC NIL DOCP))`

>  Defines a top level (global) lexical VAR with initial value VAL,
  which is assigned unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the name |VAR| and the
  name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
  kind 'VARIABLE. The new VAR will have lexical scope and thus may
  be shadowed by LET bindings without affecting its global value.

_[macro]_ `DEFMACRO/ONCE         (NAME PARAMS &BODY BODY)`

>  Like `defmacro' except that params which are immediately preceded
   by `&once' are passed to a `once-only' call which surrounds `body'.

_[macro]_ `DEFUN/INLINE          (NAME ARGS &BODY BODY)`

>  define a function automatically declared to be INLINE

_[function]_ `DIAGRAM               (THING &OPTIONAL (CONTEXT *CONTEXT*))`

>  Generate a DONUTS diagram for THING, optionally specialized for
  a specific CONTEXT. See {defgeneric cl-ctrie::make-diagram}.

_[function]_ `ENLIST                (&REST REST)`

>  Construct a chain of LNODE structures enclosing the values supplied.
  It is assumed (elsewhere) that each of these values is a valid SNODE
  structure.

_[generic-function]_ `ENTOMB                (NODE)`

>  NIL

_[generic-function]_ `EXPECTED              (CTRIE-GENERATIONAL-MISMATCH)`

>  Returns the expected of the specified ctrie-generational-mismatch


NIL_[function]_ `FAILED-REF-P          (OBJECT)`

>  Returns T if the specified object is of type failed-ref.

_[generic-function]_ `FIND-CTRIE-ROOT       (CTRIE-DESIGNATOR)`

>  FIND-CTRIE-ROOT is a subprimitive used by the
  internal CTRIE implementation to access the root inode of a given
  ctrie root-container. It does not provide safe access to the
  contents of that inode and should not be referenced by the
  higher-level implementation or end-user code.  The purpose of
  FIND-CTRIE-ROOT is to incorporate a level of indirection specialized
  on the class of the root container to facilitate future extension
  with alternate storage models, e.g., an external persistent disk-based
  store. See {defgeneric (cas cl-ctrie::find-ctrie-root)}

_[function]_ `FLAG                  (KEY LEVEL)`

>  For a given depth, LEVEL, within a CTRIE, extract the correspondant
  sequence of bits from the computed hash of KEY that indicate the
  logical index of the arc on the path to which that key may be found.
  Note that the logical index of the arc is most likely not the same
  as the physical index where it is actually located -- for that see
  'flag-arc-position'

_[function]_ `FLAG-ARC-POSITION     (FLAG BITMAP)`

>  Given FLAG representing the logical index of an arc, and BITMAP
  representing all arcs present, compute a physical index for FLAG in
  such a manner as to always ensure all arcs map uniquely and
  contiguously to the smallest vector that can contain the given
  arcs.

_[function]_ `FLAG-PRESENT-P        (FLAG BITMAP)`

>  Tests the (fixnum) BITMAP representing the logical index of all
  arcs present in a CNODE for the presence of a particular arc whose
  logical index is represented by FLAG.

_[function]_ `FLAG-VECTOR           (&OPTIONAL (CONTENT 0))`

>  FLAG-VECTOR is a bit-vector representation of the (fixnum)
  BITMAP. It is currently not used for any calculation, however it is
  included within each CNODE as a convenience because it makes it
  immediately clear from visual inspection which logical arc indexes
  are represented in the node. For example, from the bit-vector
  #*10010000000000000000000000000000 one can easily see that the first
  and fourth positions are occupied, and the rest empty.

_[generic-function]_ `FOUND                 (CTRIE-GENERATIONAL-MISMATCH)`

>  Returns the found of the specified ctrie-generational-mismatch


_[macro]_ `GCAS-COMPARE-AND-SET  (OBJ EXPECTED NEW EXPECTED-STAMP NEW-STAMP
                                  PREV)`

>  A thin, macro layer abstraction over the basic compare-and-swap
  primitive which provides a consistent interface to the underlying
  inode structure and manages additional metadata, providing
  reasonable defaults when they are not specified.

_[generic-function]_ `GEN-CONTEXT           (CTRIE-INVALID-DYNAMIC-CONTEXT)`

>  Returns the gen-context of the specified ctrie-invalid-dynamic-context


_[macro]_ `GENSYM-LET            ((&REST SYMBOLS) &BODY BODY)`

>  NIL

_[function]_ `GENSYM-LIST           (LENGTH)`

>  generate a list of LENGTH uninterned symbols

_[macro]_ `GENSYM-VALUES         (NUM)`

>  NIL

_[function]_ `GET-VECTOR-ADDR       (VECTOR)`

>  NIL

_[function]_ `HAS-DECLARE-P         (BODY)`

>  NIL

_[function]_ `HAS-DOCSTRING-P       (BODY)`

>  NIL

_[function]_ `HEX-STRING-TO-BYTE-VECTOR  (STRING)`

>  Return the byte vector represented by the (hex) STRING, which is assumed
   to be valid, as by 'byte-vector-to-hex-string'

NIL_[function]_ `INODE-COMMIT          (INODE REF)`

>  INODE-COMMIT implements the GCAS COMMIT protocol which is invoked
  as necessary by the INODE-READ and INODE-MUTATE entry-points.  It is
  not meant to be invoked directly, as this would most likely result
  in corruption. Returns the REF structure representing the content of
  whatever root inode wound up successfully committed -- either the
  one requested, or one represented by a previous state.

_[function]_ `INODE-GEN             (INODE)`

>  Returns and (with setf) changes the gen of the specified inode


_[function]_ `INODE-MUTATE          (INODE OLD-VALUE NEW-VALUE)`

>  INODE-MUTATE provides the top-level interface to the inode GCAS
  MODIFICATION api, which is the mechanism which must be used to
  effect any change in a NON-ROOT inode.  For modification of the
  root-inode, refer to the RDCSS inode api
  'ROOT-NODE-REPLACE'. Returns a boolean value which indicates the
  success or failure of the modification attempt.

_[function]_ `INODE-P               (OBJECT)`

>  Returns T if the specified object is of type inode.

_[function]_ `INODE-READ            (INODE)`

>  INODE-READ provides the top-level interface to the inode GCAS ACCESS api,
  which is the mechanism which must be used to gain access to the
  content of any NON-ROOT inode. For access to the root inode, refer
  to the RDCSS inode api 'ROOT-NODE-ACCESS'. Returns as four values,
  the MAIN-NODE, the STAMP, the PREVIOUS STATE (if any), and the REF
  structure encapsulated by the inode.

_[function]_ `INODE-REF             (INODE)`

>  Returns and (with setf) changes the ref of the specified inode


NIL_[generic-function]_ `LEAF-NODE-KEY         (RESOURCE)`

>  NIL

_[generic-function]_ `LEAF-NODE-VALUE       (RESOURCE)`

>  NIL

_[macro]_ `LET1                  (VAR VALUE &BODY BODY)`

>  Make a single `let' binding, heroically saving three columns.

NIL_[function]_ `LNODE-ELT             (LNODE)`

>  Returns and (with setf) changes the elt of the specified lnode


_[function]_ `LNODE-INSERTED        (ORIG-LNODE KEY VALUE TEST)`

>  Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but ensured to contain an LNODE enclosing an SNODE of
  KEY and VALUE.  If the given KEY equal to a key already present somewhere
  in the chain (as compared with equality predicate TEST) it will be
  replaced.  Otherwise a new LNODE will be added. In either case, the LNODE
  containing (SNODE KEY VAlUE) will be the first node in the resulting
  list.

_[function]_ `LNODE-LENGTH          (LNODE)`

>  Return the number of LNODES present in the chain beginning at LNODE

_[function]_ `LNODE-NEXT            (LNODE)`

>  Returns and (with setf) changes the next of the specified lnode


_[function]_ `LNODE-P               (OBJECT)`

>  Returns T if the specified object is of type lnode.

_[function]_ `LNODE-REMOVED         (ORIG-LNODE KEY TEST)`

>  Construct a chain of LNODE structures identical to the chain starting
  with ORIG-LNODE, but with any LNODE containing an SNODE equal to KEY
  removed.  Equality is tested as by the predicate function passed as
  the argument TEST. The order of nodes in the resulting list will
  remain unchanged.

_[function]_ `LNODE-SEARCH          (LNODE KEY TEST)`

>  Within the list of lnodes beginning with LNODE, return the range value
  mapped to by the first SNODE containing a key equal to KEY as determined
  by equality predicate TEST, or NIL if no such key is found.  As a
  second value, in order to support storage of NIL as a key, return T to
  indicate that the KEY was indeed found during search, or NIL to indicate
  that no such key was present in the list.

NIL_[function]_ `MAKE-CNODE            (&OPTIONAL (BITMAP 0))`

>  NIL

_[function]_ `MAKE-CTRIE            (&REST ARGS &KEY NAME ROOT (READONLY-P NIL)
                                     (TEST 'EQUAL) (HASH 'SXHASH))`

>  CREATE a new CTRIE instance. This is the entry-point constructor api
  intended for use by the end-user.

_[method]_ `MAKE-DIAGRAM          (THING CONTEXT &KEY)`

>  By default, attempt to generate a NODE representing THING.

_[generic-function]_ `MAKE-DIAGRAM          (THING CONTEXT &KEY
                                             &ALLOW-OTHER-KEYS)`

>  Define a specific digram generation procedure specialized
 on the class of THING and optionally for a specific context, represented
 by an abitrary symbol naming that context.

_[function]_ `MAKE-FAILED-REF       (&KEY)`

>  Returns a newly created failed-ref.

_[function]_ `MAKE-INODE            (LINK-TO &OPTIONAL GEN STAMP PREV)`

>  Construct a new INODE that represents a link to LINK-TO, optionally
  augmented with a specified generational descriptor, timestamp, and/or
  previous state.

_[function]_ `MAKE-LNODE            (&KEY ELT NEXT)`

>  Returns a newly created lnode.

_[function]_ `MAKE-RDCSS-DESCRIPTOR  (&KEY OV OVMAIN NV COMMITTED)`

>  Returns a newly created rdcss-descriptor.

_[function]_ `MAKE-REF              (&KEY (STAMP (LOCAL-TIME:NOW)) (VALUE T)
                                     PREV)`

>  Returns a newly created ref.

_[function]_ `MAKE-SNODE            (&KEY KEY VALUE)`

>  Returns a newly created snode.

_[function]_ `MAKE-TNODE            (&KEY CELL)`

>  Returns a newly created tnode.

_[function]_ `MAP-CNODE             (FN CNODE)`

>  NIL

_[method]_ `MAP-NODE              ((NODE CNODE) FN)`

>  NIL

_[method]_ `MAP-NODE              ((NODE LNODE) FN)`

>  NIL

_[method]_ `MAP-NODE              ((NODE TNODE) FN)`

>  NIL

_[method]_ `MAP-NODE              ((NODE SNODE) FN)`

>  NIL

_[method]_ `MAP-NODE              ((NODE INODE) FN)`

>  NIL

_[generic-function]_ `MAP-NODE              (NODE FN)`

>  NIL

_[function]_ `MAPAPPEND             (FUN &REST ARGS)`

>  NIL

_[macro]_ `MULTI-CATCH           (TAG-LIST &BODY FORMS)`

>  Macro allowing catch of multiple tags at once and
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

_[function]_ `MULTI-CATCH-1         (BLOCK-NAME TAG-LIST FORMS)`

>  Helper for multi-catch macro

_[generic-function]_ `NODE-OF               (CTRIE-STRUCTURAL-ERROR)`

>  Returns the node of the specified ctrie-structural-error


_[macro]_ `ONCE-ONLY             (VARS &BODY BODY)`

>  NIL

_[macro]_ `ONCE-ONLY-1           (VAR &BODY BODY)`

>  NIL

_[generic-function]_ `OP-OF                 (CTRIE-OPERATIONAL-ERROR)`

>  Returns the op of the specified ctrie-operational-error


_[macro]_ `PLACE-FN              (PLACE-FORM)`

>  This creates a closure which can write to and read from the 'place'
   designated by PLACE-FORM.

_[macro]_ `POST-INCF             (PLACE &OPTIONAL (DELTA 1))`

>  place++ ala C

_[macro]_ `PPMX                  (FORM)`

>  Pretty prints the macro expansion of FORM.

_[function]_ `PRINC-APIDOC          (&OPTIONAL (SCOPE :EXTERNAL))`

>  Print to *standard-output* the documentation for CL-CTRIE rendered
  in a compact format.  This is intended primarily as a convenience to
  the interactive user seeking quick reference at the REPL.  If SCOPE
  is specified it must be either :EXTERNAL. corresponding to those
  symbols exported as the public API, or :HOME, which designates all
  symbols defined locally in package.

_[function]_ `PRINT2                (X Y &OPTIONAL (STREAM T))`

>  NIL

_[function]_ `RANDOM-STRING         (&KEY (LENGTH 16))`

>  Returns a random alphabetic string.

NIL_[function]_ `RDCSS-DESCRIPTOR-COMMITTED  (RDCSS-DESCRIPTOR)`

>  Returns and (with setf) changes the committed of the specified rdcss-descriptor


_[function]_ `RDCSS-DESCRIPTOR-NV   (RDCSS-DESCRIPTOR)`

>  Returns and (with setf) changes the nv of the specified rdcss-descriptor


_[function]_ `RDCSS-DESCRIPTOR-OV   (RDCSS-DESCRIPTOR)`

>  Returns and (with setf) changes the ov of the specified rdcss-descriptor


_[function]_ `RDCSS-DESCRIPTOR-OVMAIN  (RDCSS-DESCRIPTOR)`

>  Returns and (with setf) changes the ovmain of the specified rdcss-descriptor


_[function]_ `RDCSS-DESCRIPTOR-P    (OBJECT)`

>  Returns T if the specified object is of type rdcss-descriptor.

NIL_[function]_ `REF-P                 (OBJECT)`

>  Returns T if the specified object is of type ref.

_[function]_ `REF-PREV              (REF)`

>  Returns and (with setf) changes the prev of the specified ref


_[function]_ `REF-STAMP             (REF)`

>  Returns and (with setf) changes the stamp of the specified ref


_[function]_ `REF-VALUE             (REF)`

>  Returns and (with setf) changes the value of the specified ref


_[generic-function]_ `REFRESH               (PLACE GEN)`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFCONSTANT-DESCRIPTOR)
                                   &OPTIONAL (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFPARAMETER-DESCRIPTOR)
                                   &OPTIONAL (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFVAR-DESCRIPTOR) &OPTIONAL
                                   (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFMACRO-DESCRIPTOR) &OPTIONAL
                                   (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFMETHOD-DESCRIPTOR) &OPTIONAL
                                   (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFGENERIC-DESCRIPTOR)
                                   &OPTIONAL (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFUN-DESCRIPTOR) &OPTIONAL
                                   (STREAM NIL))`

>  NIL

_[method]_ `RENDER                ((DESC CLDOC::DEFINE-CONDITION-DESCRIPTOR)
                                   &OPTIONAL (STREAM NIL))`

>  NIL

_[method]_ `RENDER                (DESC &OPTIONAL STREAM)`

>  NIL

_[generic-function]_ `RENDER                (DESC &OPTIONAL STREAM)`

>  Output to STREAM a compact rendering of a
   documentation-descriptor suitable for inclusion in lightweight
   text-markup.

_[generic-function]_ `RESURRECT             (NODE)`

>  NIL

_[generic-function]_ `RETRIES-OF            (CTRIE-OPERATION-RETRIES-EXCEEDED)`

>  Returns the retries of the specified ctrie-operation-retries-exceeded


_[function]_ `ROOT-NODE-ACCESS      (CTRIE &OPTIONAL ABORT)`

>  ROOT-NODE-ACCESS extends {defgeneric cl-ctrie::FIND-CTRIE-ROOT},
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

_[function]_ `ROOT-NODE-COMMIT      (CTRIE &OPTIONAL ABORT)`

>  rdcss api to complete a root-node transaction

_[function]_ `ROOT-NODE-REPLACE     (CTRIE OV OVMAIN NV)`

>  rdcss api for replacement of root ctrie inode

_[generic-function]_ `SECONDS-OF            (CTRIE-OPERATION-TIMEOUT-EXCEEDED)`

>  Returns the seconds of the specified ctrie-operation-timeout-exceeded


_[function]_ `SNODE                 (KEY VALUE)`

>  Construct a new SNODE which represents the mapping from
  domain-element KEY to range-element VALUE.

NIL_[function]_ `SNODE-KEY             (SNODE)`

>  Returns and (with setf) changes the key of the specified snode


_[function]_ `SNODE-P               (OBJECT)`

>  Returns T if the specified object is of type snode.

_[function]_ `SNODE-VALUE           (SNODE)`

>  Returns and (with setf) changes the value of the specified snode


_[function]_ `SPIN-HINT             NIL`

>  NIL

_[function]_ `TEST-BYTE-VECTOR-HEX-STRING-ROUNDRIP  NIL`

>  NIL

NIL_[function]_ `TNODE-CELL            (TNODE)`

>  Returns and (with setf) changes the cell of the specified tnode


_[function]_ `TNODE-P               (OBJECT)`

>  Returns T if the specified object is of type tnode.

_[function]_ `UNSPLICE              (FORM)`

>  NIL

_[macro]_ `WITH-CTRIE            (&ONCE CTRIE &BODY BODY)`

>  Configure the dynamic environment with the appropriate condition handlers,
  control fixtures, and instrumentation necessary to perform the
  operations in BODY on the specified CTRIE. Unless specifically
  documented, the particular configuration of this dynamic environment
  should be considered an implementation detail and not relied upon. A
  particular exception, however, is that within the dynamic extent of
  a WITH-CTRIE form, the code implementing a CTRIE operation may
  expect that the special variable *CTRIE* will be bound to the root
  container of CTRIE operated upon.  See also the documentation for
  '*CTRIE*'

_[macro]_ `WITH-PREAMBLE         ((PREAMBLE BODY-VAR) &BODY BODY)`

>  Pop docstring and declarations off `body-var' and assign them to `preamble'.

_[function]_ `XADD-WORD-SAP         (SAP WORD)`

>  NIL
