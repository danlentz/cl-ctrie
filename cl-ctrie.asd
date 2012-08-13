;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(asdf:defsystem :cl-ctrie
  :serial t
  :description      "CTrie: a lock-free, concurrent, key/value map"
  :author           "Dan Lentz <danlentz@gmail.com>"
  :license          "MIT"
  :version          "0.0.8"
  
  :long-description "This is a common-lisp implementation of the CTrie unordered map
                     data-structure described in the paper 'Concurrent Tries with
                     Efficient Non-Blocking Snapshots, (c) ACM 2-25-2012' by Prokopec,
                     Bronson, Bagwell, and Odersky.  A brief overview of general ctrie
                     concepts and existing implementations is available at
                     <http://en.wikipedia.org/wiki/Ctrie>, whose introduction is briefly
                     excerpted here as follows:

                            The Ctrie data structure is a non-blocking
                          concurrent hash array mapped trie based on
                          single-word compare-and-swap instructions in a
                          shared-memory system. It supports concurrent
                          LOOKUP, INSERT and REMOVE operations. Just like
                          the hash array mapped trie, it uses the entire
                          32-bit space for hash values thus having low
                          risk of hashcode collisions... Ctries have been
                          shown to be comparable in performance with
                          concurrent skip lists, concurrent hash tables
                          and similar data structures in terms of the
                          lookup operation...  However, they are far more
                          scalable than most concurrent hash tables where
                          the insertions are concerned. Most concurrent
                          hash tables are bad at conserving memory - when
                          the keys are removed from the hash table, the
                          underlying array is not [reduced in size]. Ctries
                          have the property that the allocated memory is
                          always a function of only the current number of
                          keys in the data-structure.  Ctries have logarithmic
                          complexity bounds of the basic operations...
                          with a low constant factor due to [large dispersal
                          ratio, (32^n arcs at level n)]. Ctries support a
                          lock-free, linearizable, constant-time SNAPSHOT
                          operation... This is a breakthrough in concurrent
                          data-structure design, since other existing concurrent
                          data-structures do not support snapshots. [This provides
                          the means to support features such as] lock-free,
                          linearizable iterator, size and clear operations. [This
                          is superior to other] existing concurrent data-structures
                          [which require the use of global locks [for exclusive,
                          blocking semantics for update access] permitting...
                          [no concurrent readers or writers] during any [update,
                          insert, remove or other modifications]. In particular,
                          Ctries have an O(1) ITERATOR creation operation, O(1)
                          CLEAR operation, O(1) DUPLICATE operation and an
                          amortized O(log n) operation for SIZE-RETRIEVAL.

                     Currently the lisp platform supported by cl-ctrie is SBCL version
                     1.0.55 or greater, although support could easily be entended to
                     other common-lisp implementations that offer support for atomic
                     compare-and-swap functionality, notably LispWorks 5.x/6.x, which is
                     also well instrumented with lock-free, atomic primitives, although
                     this is not necessarily a high priority for the initial development
                     cycle."
  
  :weakly-depends-on (:cl-store :donuts :cldoc :cl-ppcre)
  :depends-on (:alexandria :lisp-unit :local-time)
  :components ((:static-file  "cl-ctrie.asd")
                (:static-file "readme.md")
                (:file "ctrie-package")
                (:file "ctrie-cas")
                (:file "ctrie-util")
                (:file "ctrie")
                (:file "ctrie-lambda")
        #+cldoc (:file "ctrie-doc")))



(asdf:defsystem :cl-ctrie-test
  :serial t
  :depends-on (:cl-ctrie :lparallel :cl-skip-list)
  :components ((:file "ctrie-test")))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie))))
  (asdf:load-system :cl-ctrie-test)
  (funcall (intern (symbol-name :run-ctrie-tests) (find-package :cl-ctrie-test))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie-test))))
  (asdf:load-system :cl-ctrie-test)
  (funcall (intern (symbol-name :run-ctrie-tests) (find-package :cl-ctrie-test))))

(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie))))
  nil)

(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :cl-ctrie-test))))
  nil)

#+cldoc
(defmethod asdf:perform ((o asdf::doc-op) (c (eql (asdf:find-system :cl-ctrie))))
  (asdf:load-system :cl-ctrie)
  (funcall (intern (symbol-name :readme-quietly) (find-package :cl-ctrie))))

#+cldoc
(defmethod asdf:operation-done-p ((o asdf::doc-op) (c (eql (asdf:find-system :cl-ctrie))))
  nil)
