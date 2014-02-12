

# Towards the best collection API

## Abstract

Most programming languages support collections, represented by an
in-memory data structure, a file, a database, or a generating
function. A programming language system gives us typically one of the
two interfaces to systematically access elements of a collection. One
traversal API is based on enumerators -- e.g., for-each, map, filter
higher-order procedures -- of which the most general is fold. The
second approach relies on streams, a.k.a. cursors, lazy
lists. Generators such as the ones in Icon, Ruby and Python are a
hybrid approach.

This article presents a design of the overall optimal collection
traversal interface, which is based on a left-fold-like combinator
with premature termination. We argue from the practical standpoint
that such an interface is superior: in efficiency; in ease of
programming; in more predictable resource utilization and avoidance of
resource leaks.

We also demonstrate a procedure that converts _any_ left-fold-like
enumerator for any collection into the corresponding
cursor. Therefore, the stream-based interface, when it is truly needed
or preferred, can be automatically and generically derived from the
proposed interface.

We present two variants of the left-fold enumerator: for languages
with and without first-class continuations. The proposed traversal
interface has been implemented in a Scheme database access API used in
a production environment. The generic-left-fold-based interface
underlies the Collection SRFI (Scheme Request for Implementation) and
is being considered for an Oracle database interface in Haskell.


## Introduction and terminology

Access to collections cuts through many programming languages. By a
collection we mean a hash table; a file as a collection of lines,
words, or bytes; a file system directory; an XML document; a suffix
tree; or a "dynamic" collection produced by a database query or any
other generating function.

There are two basic approaches to systematically access collection
elements. One approach relies on enumerators such as 'map', 'for-each'
and 'filter'. The most general of those is fold, which lets us
maintain our state during the traversal. The other traversal API is
based on lazy lists (a.k.a. cursors, or streams).

A word about terminology is in order, as it is rather confusing.  We
will call a higher-order for-each-type procedure an enumerator. It is
this procedure that takes a collection and a handler, and applies the
handler to each element of the collection in turn. The handler itself
will be called an iteratee, because it is being applied -- by an
active entity, the enumeratOR.  An enumerator is presumably privy to
the internals of a collection; it knows how to fetch elements in some
(specified) order, and knows when to stop. A handler does not have
this knowledge, nor is it supposed to. Throughout the article we
eschew the commonly used term 'iterator': in C++ it means an accessor,
which does not iterate over a collection. Rather, it is _being_
iterated. On the other hand, languages like OCaml provide a procedure
'iter' that actively traverses a collection, applying a user-defined
function to elements.

The next section argues from the practical standpoint against cursors
and for enumerators. We contend that an enumerator such as left fold
should be the primary means of traversing and obtaining all values of
a collection. Enumerators are superior to cursors:

- in efficiency
- in ease of programming
- in more predictable resource utilization and avoidance of resource leaks.

The latter is of special importance for collections backed by external
resources such as file handles and database connections.

Section 3 proposes the overall optimal enumeration interface, which
adds premature termination and explicit multiple state variables to
the left fold. Section 4 discusses generators.

Given a stream, we can always construct the corresponding
enumerator. It is perhaps less appreciated that given a left fold
enumerator, we can always construct the corresponding stream: we can
invert an enumerator inside out. Section 5 describes the inversion
procedure, which is fully generic and works for any
collection. Because the enumerator interface subsumes the cursor-based
one and because the former is more efficient and less error-prone, a
collection API should offer enumerators as the primary, native
traversal interface.

The automatical enumerator inversion procedure in Section 5 relies on
first-class (delimited) continuations. However, we can write a similar
enumerator inversion even in a language that lacks first-class
continuations.  Section 6 demonstrates a non-recursive left-fold-like
combinator that can be instantiated into the ordinary left fold or into
a stream. Both instantiation procedures are generic and do not depend
on the type of the collection.

In conclusion, we recap our recommendations for the optimal collection
traversal API and point out to the existing implementations of the
API.

## Why an enumerator is better than a cursor

There are many fundamental reasons why enumerators such as fold are
profoundly better than cursors. One such reason is the ease of program
analysis and optimizations [Sheard, Bananas, Hutton]. This
article will not discuss them. Instead, we concentrate on utterly
practical considerations: performance, ease of programming, and
resource utilization.

Writing a procedure that traverses, e.g., an AVL tree is far easier
than writing a cursor that remembers the current position and can
advance to the next one. The latter is especially tricky if the tree
in question does not have parent pointers (which waste space and lead
to sharing and resource leakage problems). One look at the C++ STL
should convince one how wretched programming of iterators could be.

The "current element" of a cursor is a state, which can be altered by
advancing the cursor. This state introduces an implicit dependency
among all expressions that use the cursor, similar to the dependency
imposed by global mutable variables. Therefore, programming with
cursors is error-prone. In contrast, an enumerator does not expose its
traversal state to an iteratee and does not permit any alteration to
that state by an iteratee. The difference between the leaky and the
perfect encapsulations of the traversal state is well explained in
[ULLMAN] when comparing a cursor-based network database query language
DBTG with SQL. Incidentally, streams, another cursor-based interface,
expose the current element and hence eliminate the implicit state
dependency.

Another problem of cursors is telling the user that there are no more
values to access. When a collection is exhausted, cursor operations
typically return an "out-of-band" value such as EOF (cf.  getc() in C)
or raise an exception (cf. iterator.next() in Python). None of these
approaches appear entirely satisfactory. Unlike cursors, an enumerator
does not need to do anything special to tell the iteratee that the
traversal is finished: the enumerator will simply return. There is no
longer a problem with out-of-band values and user's disregarding such
values.

Cursors are simply less efficient. A cursor must maintain the state of
the traversal. That state may be invalid. Each operation on a cursor,
therefore, must first verify that the cursor is in the valid
state. The cost of the checks quickly adds up.

This issue of the inefficiency of cursors is well known. For example,
it is highly advisable to use "native" operations to move large
sections of an array (ArrayCopy in Java) rather than copy
element-by-element. The cost of an array bound check on each access to
an element is one of the considerations.

Enumerators can do more than just eliminate redundant bound
checks. Enumerators, if implemented as staged functions (e.g., C++
templates) can tile or unroll loops and thus partially or completely
remove the traversal iteration overhead [Veldhuizen]. The perfect
encapsulation of the traversal state makes enumerators particularly
adaptable for multi-stage programming.

Databases give another example of the efficiency of enumerators: "The
performance of cursors is horrible in almost all systems. One of us
(Shasha) once had an experience of re-writing an eight-hour query
having nested cursors into a cursor-free query that took 15 seconds."
[Shasha]

It is often said that the key to the best performance is to 
do more on the server. To find the maximum value of a database table
column, it's far faster to evaluate
       `select MAX(col) from collection`
than to open a cursor on the collection and retrieve all values from
'col' searching for the maximum. Stored procedure languages (some of
which are quite sophisticated) were introduced to make the server-side
processing easier and powerful.  We should stress that 
	`select MAX(col) from collection`
is precisely equivalent to
	`foldl max lower_bound collection`


Traversing a collection often requires resources, e.g., a stack space,
a connection to a database, a DIR handle, a DOM tree of an XML
document, etc. An enumerator takes care of allocating these resources.
When iteratee explicitly tells the enumerator to stop -- or when the
collection is exhausted -- the enumerator can orderly free the
resources. An enumerator can be programmed as

```scheme
	   (define (enum proc collection)
	       (let ((database-connection #f))
		(dynamic-wind
		   (lambda () 
		      (set! database-connection
			    (open-connection collection)))
		   (lambda () (iterate proc))
		   (lambda ()
		      (set! database-connection
		            (close-connection database-connection))))))
```

If 'proc' does not capture its continuation [Footnote-1] -- as it is
often the case -- the database connection is opened (taken from the
pool) at the beginning of the iteration and is returned to the pool at
the end.  If we were to provide access to our collection in the form of
a cursor, we would have to place the variable 'database-connection'
into that cursor. We cannot close the database until the cursor is
alive. But how do we know when there are no alive cursors and
therefore it is safe to recycle the database connection? We must rely
either on the programmer's explicitly closing the connection, or on a
finalizer. The sheer number of internet security advisories concerning
memory allocation problems indicates that manual management of
resources is greatly error-prone. The finalizer solution is not
satisfactory either: finalizers are rarely supported and when they
are, they are an unreliable tool to manage precious resources. The
execution of finalizers is unpredictable and is generally beyond
programmer's control.

[Footnote-1]
An iteratee may call a continuation captured before the enumerator was
entered. For that reason, an enumerator should employ a dynamic-wind
to detect such an attempt to escape, shed excessive resources and
switch to a "low-power" mode while the iteratee is on the run.


## The most general enumeration interface

We propose the following general enumeration interface. It is based on
a left-fold enumerator and explicitly supports multiple state
variables (i.e., seeds) and a premature termination of the
iteration. In this Section we describe the interface in pseudo-code,
which can be instantiated in Scheme [SRFI-44], Haskell ([Daume],
Section 6) or other concrete language.

    The enumeration procedure
       coll-fold-left COLL PROC SEED SEED ... -> [SEED ... ]

traverses the collection denoted by COLL. The procedure takes one or
more state arguments denoted by SEED, and returns just as many. PROC
is an iteratee procedure:

   PROC VAL SEED SEED ... -> [INDIC SEED SEED ...]

It takes n+1 arguments and returns n+1 values. The first argument is
the current value retrieved from the collection. The other n arguments
are the seeds. The first return value is a boolean indicator, whose
false value causes the premature termination of the iteration.  The
other return values from PROC are the new values of the seeds. The
procedure coll-fold-left enumerates the collection in some order and
invokes PROC passing it the current value of the collection and the
current seeds. The first invocation of PROC receives SEEDs that were
the arguments of coll-fold-left. The further invocations of PROC
receive SEEDs that were produced by the previous invocation of
PROC. When the collection is exhausted or when the PROC procedure
returns the indicator value of false, coll-fold-left terminates the
iterations, disposes of allocated resources, and returns the current
SEEDs. If the collection COLL is empty, coll-fold-left does not invoke
PROC and returns its argument SEEDs.



## Enumerators and generators

A programming language Icon popularized generators as a way to
traverse actual and virtual collections. Generators are also supported
in Ruby and Python. The documentation of Icon defines a generator as
an expression that can produce several values, on demand.
Multiple-valued expressions can also be implemented with shift and
reset [SHIFT]. The paper [SHIFT] gives many examples of their use.

Generators occupy an intermediate place between enumerators and
cursors. A generator is just as easy to write as an enumerator. It
traverses a collection, fetches the current element and _yields_ it,
by passing the element to a dedicated procedure or syntax form. When
the latter returns, the traversal continues. On the other hand,
generators are trivially related to streams [ENUM-CC]. The latter
article discusses generators and enumerators in more
detail. Incidentally, the article demonstrates that a generator-based
code in Python can be translated into Scheme almost
verbatim. Generators give the first hint that enumerators and cursors
are related via first-class continuations.

Like cursors and streams, generators are demand-driven. A user must
explicitly request a new value to advance the traversal. Therefore,
like cursors generators leak resources: it is not clear when the
iteration should be assumed terminated and the associated resources
can be safely disposed of.

## How to invert an enumerator in a language with first-class continuations

Sometimes we indeed need to traverse a collection via a cursor.
Reasons may include moving data from one collection to another, or
interfacing legacy code. If a collection API provides enumerators, we
obtain cursors for free. We pass an enumerator to a generic
translation procedure, which inverts the enumerator "inside out" and
returns a cursor.

The following code illustrates the conversion in Scheme, a language
with first-class continuations. The procedure lfold->lazy-list is a
fully generic translation procedure: it takes a left-fold enumerator
for _any_ collection, and converts the enumerator to a stream (lazy
list). The latter is a realization of a cursor.

```scheme
(define (lfold->lazy-list lfold collection)
  (delay
    (call-with-current-continuation
      (lambda (k-main)
	(lfold collection
	  (lambda (val seed)
	    (values
	      (call-with-current-continuation
		(lambda (k-reenter)
		  (k-main
		    (cons val
		      (delay
			(call-with-current-continuation
			  (lambda (k-new-main)
			    (set! k-main k-new-main)
			    (k-reenter #t))))))))
	      seed))
	  '()) 				; Initial seed
	(k-main '())))))

```

The present article is an abstract. Code in a functional language is
the best abstract. The discussion is delegated to the talk and the
full paper. The article [ENUM-CC] discusses the inversion procedure in
more detail and points out to the complete code and the test cases.


## How to invert an enumerator in a language without first-class continuations

If a programming language lacks first-class continuations, the
conversion from an enumerator to a cursor is still possible. However,
we need to generalize the enumerator interface and make it
non-recursive. For concreteness, this section demonstrates our
approach for one particular language without first-class
continuations: Haskell. Applications to other languages are
straightforward.

In Haskell, the general left-fold enumerator has the following
interface [HINV]:

> type CFoldLeft coll val m seed = coll -> CollEnumerator val m seed
> type CollEnumerator val m seed =
>        Iteratee val seed
>        -> seed      -- the initial seed
>        -> m seed
> type Iteratee val seed = seed -> val -> Either seed seed

where 'coll' is the type of a collection with elements of the type
'val', and 'm' is an arbitrary monad. The type 'seed' is the type of a
state variable or variables (if the seed is a tuple). If an iteratee
returns Right seed', the iteration continues with seed' as the new
seed. If the iteratee returns Left seed'', the enumerator immediately
stops further iterations, frees all the resources, and returns seed''
as the final result.

Incidentally, Hal Daume III mentioned [DAUME] that such left-fold
enumerator is indeed useful in practice. It is his preferred method of
iterating over a file considered as a collection of characters, lines,
or words.

To make the enumerator non-recursive, we need to add an additional
argument -- self:

> type CFoldLeft' val m seed = 
>        Self (Iteratee val seed) m seed
>        -> CollEnumerator val m seed
> type Self iter m seed = iter -> seed -> m seed
> type CFoldLeft1Maker coll val m seed = coll -> m (CFoldLeft' val m seed)

A function of the type CFoldLeft' is also an enumerator. However, that
enumerator does not recurse to advance the traversal. It invokes Self
instead. Given CFoldLeft1Maker, we can obtain either the CFoldLeft
enumerator, or a stream.  The former translation procedure amounts to
taking a fixpoint:

> hfold_nonrec_to_rec:: (Monad m) => 
>        coll -> (CFoldLeft1Maker coll val m seed)
>             -> m (CollEnumerator val m seed)
> hfold_nonrec_to_rec coll hfold1_maker = do
>    hfold_left' <- hfold1_maker coll
>    return $ fix hfold_left'
> fix f = f g where g = f g

Converting CFoldLeft' into a stream is equally simple:

> data MyStream m a = MyNil (Maybe a) | MyCons a (m (MyStream m a))

> hfold_nonrec_to_stream:: 
>    (Monad m) => CFoldLeft' val m (MyStream m val) 
>                 -> m (MyStream m val)
> hfold_nonrec_to_stream hfold_left' = do
>   let k fn (MyNil Nothing)  = return $ MyNil Nothing
>       k fn (MyNil (Just c)) 
>         = return $ MyCons c (hfold_left' k fn (MyNil Nothing))
>   hfold_left' k (\_ c -> Right $ MyNil $ Just c) (MyNil Nothing)

The polymorphic types of both conversion procedures indicate that the
procedures are generic and apply to any collection and any traversal.


The article [HINV] demonstrates both translations of CFoldLeft' on a
concrete example of a file taken as a collection of
characters. Haskell provides a cursor interface to that collection:
hGetChar. We implement a left fold enumerator CFoldLeft'. We then show
how to turn that enumerator back to a stream: how to express functions
myhgetchar and myhiseof only in terms of the left fold enumerator. The
derivation of these functions is independent of the precise nature of
the enumerator. Incidentally, if we turn two enumerators into streams,
we can safely interleave these streams.


## Conclusions

In a language with first-class continuations, we propose
coll-fold-left as the overall optimal interface to systematically
access values of a collection (Section 3):

   coll-fold-left COLL PROC SEED SEED ... -> [SEED ... ]
   PROC VAL SEED SEED ... -> [INDIC SEED SEED ...]

In a language without first-class continuations, we propose
CFoldLeft1Maker (Section 6) as such optimal interface. 

The enumerator-based interface is optimal because enumerators: are
easier to write; are less error-prone to use; are more efficient;
provide a better encapsulation of the state of the traversal; avoid
resource leaks.

We have presented generic conversion procedures that turn enumerators
into cursors. The existence of these procedures demonstrates that
enumerator- and cursor-based interfaces are inter-convertible. We have
argued however that the enumerator interface should be considered
primary and offered natively in a collection API. It is far more
efficient and easy for a programmer to implement cursors via
enumerators, than the other way around.

The coll-fold-left interface has indeed been implemented and tested in
practice. We have written a relational database interface for Scheme
[DBINTF], which we have been using in the production environment. We
have also implemented coll-fold-left to enumerate entries in a TIFF
image tag directory. The enumerator coll-fold-left has been chosen to
be the primary traversal interface in Scheme Collections SRFI
[SRFI-44]. A similar interface is being considered for an Oracle RDBMS
binding in Haskell.


## References

[SHIFT] Olivier Danvy and Andrzej Filinski. Abstracting Control.
Proc. 1990 ACM Conf. on LISP and Functional Programming, pp. 151-160,
Nice, France, June 1990.

[Daume] Hal Daume III.  Re: From enumerators to cursors: turning the 
left fold inside out.
A message posted on the Haskell mailing list on
24 Sep 2003 07:47:23 -0700.

[Hutton] Graham Hutton. A tutorial on the universality and 
expressiveness of fold.
Journal of Functional Programming, 9(4):355-372, July 1999.

[ENUM-CC] Oleg Kiselyov. General ways to traverse collections.
January 1, 2004.
http://pobox.com/~oleg/ftp/Scheme/enumerators-callcc.html

[DBINTF] Oleg Kiselyov. Scheme database access tools. May 10, 2003.
http://pobox.com/~oleg/ftp/Scheme/lib/db-util1.scm
http://pobox.com/~oleg/ftp/Scheme/tests/vdbaccess.scm 

[HINV] Oleg Kiselyov. From enumerators to cursors: turning the left
fold inside out. January 1, 2004.
http://pobox.com/~oleg/ftp/Haskell/misc.html#fold-stream
The first draft was posted on the Haskell mailing list on
23 Sep 2003 23:59:45 -0700.

[Bananas] Erik Meijer, Maarten M. Fokkinga, and Ross Paterson. 
Functional programming with bananas, lenses, envelopes, and barbed wire. 
In J. Hughes, editor, FPCA'91: Functional Programming Languages and
Computer Architecture, volume 523 of LNCS, pp. 124-144. 
Springer-Verlag, 1991.                              

[SRFI-44] Scott G. Miller. Collections.
Scheme Request for Implementation SRFI-44. October 2003.
http://srfi.schemers.org/srfi-44/srfi-44.html

[Shasha] Dennis E. Shasha and Philippe Bonnet. Smooth Talking Your Databases.
Dr.Dobbs Journal, July 2002, pp. 46-54.

[Sheard] Tim Sheard and Leonidas Fegaras. A fold for all seasons. 
Proc. Conf. on Functional Programming and Computer Architecture
(FPCA'93), pp. 233-242, Copenhagen, Denmark, June 1993.

[ULLMAN] Jeffrey Ullman. Principles of Database Systems.
Second Edition, 484 pp. Computer Science Press, 1982.

[Veldhuizen]
T. Veldhuizen. Expression Templates.
C++ Report, Vol. 7 No. 5 June 1995, pp. 26-31
http://osl.iu.edu/~tveldhui/papers/Expression-Templates/exprtmpl.html
