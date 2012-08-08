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

Perhaps most ideosyncrasies of this ctrie implementation as compared
with the [original](http://github.com/axel22/Ctries), written in
Scala, result from the efforts I have taken to, where feasible,
attempt to adopt a more functional oriented approach that is more
representative of ideomatic, common-lisp coding style.  For example,
rather than expose a general purpose GCAS and RDCSS api, these
protocols are incorporated into ctrie-specific abstractions:
`INODE-READ` `INODE-MUTATE` and `INODE-COMMIT` for GCAS, and for
RDCSS, `ROOT-NODE-ACCESS` `ROOT-NODE-REPLACE` and `ROOT-NODE-COMMIT.`
These liberties I have taken have the benefit of providing an
interface that is much easier to digest, understand, remember, and
work with (at least for me) than direct exposure in imperative style
of the intricate mechanations that underlie the ctrie algorithm.  On
the other hand, the further one strays from a direct translation of
the original (verified) ctrie implementation, the greater the
likelihood of introducing bugs into an environment (lock-free
concurrent data structure development) in which bugs can be extremely
subtle and are notoriously difficult to detect.  I have attempted to
strike an appropriate balance between these conflicting concerns, and
I intend to mitigate the risk, at least in part, through development
of a more extensive arsenal of regression tests and benchmarking
facilities.

In addition, there are a few differences in the feature set that
is provided, such as a suite of mapping operators in leiu of a
Java style iterator.  For the most part I expect that these
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

The following sections provide a compact overview of the User API
and a reference to some internal definitions of interest.

#### User API Reference

The user api of cl-ctrie should be largely familiar to the average
common-lisp programmer.  Nearly all exported symbols of the CL-CTRIE 
package begin with the prefix "ctrie" and thus can be convenientely
incorporated via USE-PACKAGE or equivalent package definition.  The
following definitions comprise a quick reference to the the public
user api:

* * * * *

_[structure]_        `CTRIE ()`

> A CTRIE root container uniquely identifies a CTRIE instance, and
  contains the following perameters which specify the customizable
  aspects of each CTRIE:
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
  and `ROOT-NODE-COMMIT`


_[function]_         `CTRIE-CLEAR  (CTRIE)`

_[macro]_            `CTRIE-DO  ((KEY VALUE CTRIE &KEY ATOMIC) &BODY BODY)`

> Iterate over (key . value) in ctrie in the manner of dolist.
   EXAMPLE: (ctrie-do (k v ctrie)
              (format t "~&~8S => ~10S~%" k v))


_[function]_         `CTRIE-DROP  (CTRIE KEY)`

> Remove KEY and it's value from the CTRIE.


_[function]_         `CTRIE-EMPTY-P  (CTRIE)`

_[function]_         `CTRIE-ENSURE-GET  (CTRIE KEY DEFAULT)`

> Like CTRIE-GET, but if KEY is not found in CTRIE, automatically adds 
   the entry (KEY. DEFAULT). Secondary return value is true if key was
   already in the table.


_[macro]_            `CTRIE-ERROR  (CTRIE CONDITION &REST ARGS)`

> Signal a CTRIE related condition.


_[condition]_        `CTRIE-ERROR (ERROR)`

> Abstract superclass of CTRIE related conditions.


_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-EXPORT  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-EXPORT  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[function]_         `CTRIE-FROM-ALIST  (ALIST)`

_[function]_         `CTRIE-FROM-HASHTABLE  (HASHTABLE)`

> create a new ctrie containing the same (k . v) pairs and equivalent
  test function as HASHTABLE


_[condition]_        `CTRIE-GENERATIONAL-MISMATCH (CTRIE-STRUCTURAL-ERROR)`

> Condition indicating an operation encountered an
   outdated or inconsistent node during its attempted traversal


_[function]_         `CTRIE-GET  (CTRIE KEY)`

_[function]_         `CTRIE-HASH  (CTRIE)`

> Returns and (with setf) changes the hash of the specified ctrie



_[method]_           `CTRIE-IMPORT  ((PLACE HASH-TABLE) &KEY)`

_[method]_           `CTRIE-IMPORT  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-IMPORT  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[condition]_        `CTRIE-INVALID-DYNAMIC-CONTEXT (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation was attempted
   outside the dynamic extent of a valid enclosing WITH-CTRIE form


_[function]_         `CTRIE-KEYS  (CTRIE &KEY ATOMIC)`

_[method]_           `CTRIE-LOAD  ((PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-LOAD  (PLACE &KEY &ALLOW-OTHER-KEYS)`

_[function]_         `CTRIE-MAP  (CTRIE FN &KEY ATOMIC &AUX ACCUM)`

_[generic-function]_ `CTRIE-MAP-INTO  (CTRIE PLACE FN)`

_[function]_         `CTRIE-MAP-KEYS  (CTRIE FN &KEY ATOMIC)`

_[function]_         `CTRIE-MAP-VALUES  (CTRIE FN &KEY ATOMIC)`

_[condition]_        `CTRIE-NOT-IMPLEMENTED (CTRIE-ERROR)`

> Condition designating functionality for which the
   implementation has not been written, but has not been deliberately
   excluded.


_[condition]_        `CTRIE-NOT-SUPPORTED (CTRIE-ERROR)`

> Condition designating functionality that is
  deliberately not supported.


_[condition]_        `CTRIE-OPERATION-RETRIES-EXCEEDED (CTRIE-OPERATIONAL-ERROR)`

> Condition indicating an operation has failed the
   maximum number of times specified by the special-variable
   *retries*


_[condition]_        `CTRIE-OPERATIONAL-ERROR (CTRIE-ERROR)`

> Conditixon for when an operational failure or
  inconsistency has occurred.


_[function]_         `CTRIE-PPRINT  (CTRIE &OPTIONAL (STREAM T))`

_[function]_         `CTRIE-PUT  (CTRIE KEY VALUE)`

_[function]_         `CTRIE-READONLY-P  (CTRIE)`

> Returns and (with setf) changes the readonly-p of the specified ctrie



_[method]_           `CTRIE-SAVE  ((CTRIE CTRIE) (PLACE PATHNAME) &KEY)`

_[generic-function]_ `CTRIE-SAVE  (CTRIE PLACE &KEY &ALLOW-OTHER-KEYS)`

_[function]_         `CTRIE-SIZE  (CTRIE &AUX (ACCUM 0))`

_[function]_         `CTRIE-SNAPSHOT  (CTRIE &KEY READ-ONLY)`

_[condition]_        `CTRIE-STRUCTURAL-ERROR (CTRIE-ERROR)`

> Condition designating that the CTRIE data structure
   has been determined to be invalid.


_[function]_         `CTRIE-TEST  (CTRIE)`

> Returns and (with setf) changes the test of the specified ctrie



_[function]_         `CTRIE-TO-ALIST  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-TO-HASHTABLE  (CTRIE &KEY ATOMIC)`

_[function]_         `CTRIE-VALUES  (CTRIE &KEY ATOMIC)`

_[function]_         `MAKE-CTRIE  (&REST ARGS &KEY NAME ROOT (READONLY-P NIL)
                                   (TEST 'EQUAL) (HASH 'SXHASH))`

> CREATE a new CTRIE instance. This is the entry-point constructor api
  intended for use by the end-user.



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

* * * * *
