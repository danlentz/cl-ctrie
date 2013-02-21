;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/common/pointer :in cl-ctrie/common))


(deftest check-deref-atom/simple ()
  (is (eq (pointer:deref :atom) :atom)))


(deftest check-deref-function/simple ()
  (is (equal (pointer:deref #'list 0 1 2 3) '(0 1 2 3))))


(deftest check-deref-pathname/string-octets ()
  (is (equalp
        (pointer:deref #P"~/.sbclrc" 'string)
        (io:octets-to-string (pointer:deref #P"~/.sbclrc" 'vector))))
  (is (equalp 
        (io:string-to-octets (pointer:deref #P"~/.sbclrc" 'string))
        (pointer:deref #P"~/.sbclrc" 'vector))))


(deftest check-deref-url/string-octets ()
  (is (equalp
        (pointer:deref (ql-http:url "http://www.cliki.net/") 'string)
        (io:octets-to-string (pointer:deref (ql-http:url "http://www.cliki.net/") 'vector))))
  (is (equalp
        (io:string-to-octets (pointer:deref (puri:uri "http://www.cliki.net/") 'string))
        (pointer:deref (puri:uri "http://www.cliki.net/") 'vector)))
  (is (equalp
        (io:string-to-octets (pointer:deref (ql-http:url "http://www.cliki.net/") 'string))
        (pointer:deref (ql-http:url "http://www.cliki.net/") 'vector))))


(deftest check-deref-url/string-file ()
  (io:with-temporary-file (f fname)
    (is (equalp
          (setf (pointer:deref fname)
            (pointer:deref (puri:uri "http://paste.lisp.org/list-full.rss") 'string))
          (pointer:deref fname))))
  (io:with-temporary-file (f fname)
    (is (equalp
          (setf (pointer:deref fname)
            (pointer:deref (ql-http:url "http://paste.lisp.org/list-full.rss") 'string))
          (pointer:deref fname)))))


(deftest check-deref-file/roundtrip-setf ()
  (io:with-temporary-file (f fname)
    (is (equalp
          (setf (pointer:deref fname) (princ-to-string (uuid:make-v4-uuid)))
          (pointer:deref fname))))
  (io:with-temporary-file (f fname)
    (is (equalp
          (setf (pointer:deref fname) *features*)
          (read-from-string  (pointer:deref fname))))))

(deftest check-deref-uri/roundtrip-encodings ()  
  (flet ((ok (return-type uri-string)
           (equalp
             (pointer:deref (puri:uri    uri-string) return-type)
             (pointer:deref (ql-http:url uri-string) return-type))))
    (is (ok 'vector "http://cliki.net/site/recent-changes"))
    (is (ok 'string "http://cliki.net/site/recent-changes"))
    (is (ok 'vector "http://cliki.net/"))
    (is (ok 'string "http://cliki.net/"))  
    (is (ok 'vector "http://paste.lisp.org/list.rss"))
    (is (ok 'string "http://paste.lisp.org/list.rss"))))


