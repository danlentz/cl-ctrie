;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :pointer
  (:nicknames :&)
  (:use :closer-common-lisp :io)
  (:export :deref :@))

(in-package :pointer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Reference/Dereference 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric deref (thing &optional type &rest args))

(defgeneric (setf deref) (value location &optional type &rest args))

(setf (fdefinition '@) (fdefinition 'deref))

(setf (fdefinition '(setf @)) (fdefinition '(setf deref)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmethod deref :before (thing &optional type &rest args)
;;   (declare (ignorable thing type args))
;;   (log:debug "&(~S ~A~{ ~S~})" thing type args))


(defmethod deref ((thing t) &optional (type t) &rest args)
  "if thing is not known to be a reference, return it."
  (declare (ignore args))
  (values thing type))


(defmethod deref ((thing cons) &optional (type 'car) &rest args)
  "if thing is a cons cell, dereference as normal"
  (declare (ignore args))
  (ecase type
    (car (car thing))
    (cdr (cdr thing))))


(defmethod deref  ((thing function) &optional type &rest args)
    "return the result of applying function to args"
  (apply thing type args))


(defmethod deref ((thing pathname) &optional (type 'string) &rest args)
  "Returns the entire contents of the specified file."
  (declare (ignore args))
  (case type
    (string     (io:read-file-to-string thing))
    (strings    (io:read-file-to-string-list thing))
    (cons       (io:read-file-to-list thing))
    (list       (io:read-file-to-list thing))
    (vector     (io:read-file-to-byte-vector thing))
    (t          (cl-store:restore thing))))


(defmethod deref ((stream stream) &optional (type 'strings) &rest args)
  (declare (ignore args))
  (case type
    (strings (io:read-stream-to-string-list stream))
    (string  (io:read-stream-to-string      stream))
    (t       (cl-store:restore              stream))))


(defmethod deref ((url ql-http:url) &optional (type 'vector) &rest args)
  (declare (ignore args))
  (let ((pathname (merge-pathnames (princ-to-string (unicly:make-v4-uuid)) #p"/tmp/")))
    (unwind-protect (and
                      (ql-http:fetch url pathname)
                      (deref pathname type))
      (delete-file pathname))))


;; this may or may not be reasonable behavior, but the idea is if one has puri loaded
;; they may likely prefer drakma as a default http client.  Otherwise quicklisp is
;; assumed to be universally available
#+chunga
(defmethod deref ((uri puri:uri) &optional (type 'vector) &rest args)
  (declare (ignore args))
  (multiple-value-bind
    (return-content return-code) (drakma:http-request uri :force-binary t :want-stream t)
    (format *trace-output* "http-request [~A]: ~D" uri return-code)
    (values
      (case type
        (stream return-content)
        (vector (io:read-stream-to-byte-vector return-content))
        (string (io:octets-to-string (io:read-stream-to-byte-vector return-content))))
      return-code)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setf-able assignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (setf deref) ((thing t) (location pathname) &optional type &rest args)
  (declare (ignore type args))
  (prog1 thing
    (cl-store:store thing location)))
  
  
(defmethod (setf deref) ((string string) (location pathname) &optional type &rest args)
  (declare (ignore args))
  (prog1 string
    (with-open-file (out location :direction :output
                      :if-exists :overwrite :if-does-not-exist :create
                      :external-format (or type :default))
      (write-sequence string out))))


(defmethod (setf deref) ((cons cons) (location pathname) &optional type &rest args)
  (declare (ignore args))
  (prog1 cons
    (with-open-file (out location :direction :output
                      :if-exists :overwrite :if-does-not-exist :create
                      :external-format (or type :default))
      (pprint cons out))))


(defmethod (setf deref) ((byte-vector vector) (location pathname) &optional (file-position 0)
                          &rest args)
  (declare (ignorable args))
  (prog1 byte-vector
    (with-open-file (out location :direction :output :element-type '(unsigned-byte 8)
                      :if-exists :overwrite :if-does-not-exist :create
                      :external-format (getf args :external-format :default))
      (file-position out (or file-position :end))
      (write-sequence byte-vector out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(assert (eq (deref :atom) :atom))
(assert (equal (deref #'list 0 1 2 3) '(0 1 2 3)))

(assert (equalp
          (deref #P"~/.sbclrc" 'string)
          (io:octets-to-string (deref #P"~/.sbclrc" 'vector))))

(assert (equalp
          (io:string-to-octets (deref #P"~/.sbclrc" 'string))
          (deref #P"~/.sbclrc" 'vector)))

(assert (equalp
          (deref (ql-http:url "http://www.cliki.net/") 'string)
          (sb-ext:octets-to-string (deref (ql-http:url "http://www.cliki.net/") 'vector))))

(assert (equalp
          (io:string-to-octets (deref (puri:uri "http://www.cliki.net/") 'string))
          (deref (puri:uri "http://www.cliki.net/") 'vector)))

(assert (equalp
          (io:string-to-octets (deref (ql-http:url "http://www.cliki.net/") 'string))
          (deref (ql-http:url "http://www.cliki.net/") 'vector)))

(assert (equalp
          (setf (deref #p"/tmp/note.rss")
            (deref (puri:uri "http://paste.lisp.org/list-full.rss") 'string))
          (deref #p"/tmp/note.rss")))

(assert (equalp
          (setf (deref #p"/tmp/note.rss")
            (deref (ql-http:url "http://paste.lisp.org/list-full.rss") 'string))
          (deref #p"/tmp/note.rss")))

(assert (equalp
          (setf (deref #p"/tmp/dan.txt") (princ-to-string (unicly:make-v4-uuid)))
          (deref #p"/tmp/dan.txt")))

(assert (equalp
          (setf (deref #p"sexp.tmp") *features*)
          (read-from-string  (deref #p"sexp.tmp"))))

(assert (typep
          (deref (puri:uri "http://paste.lisp.org/list.rss") 'stream)
          'flex:flexi-io-stream))
 
(flet ((ok (return-type uri-string)
         (equalp
           (deref (puri:uri    uri-string) return-type)
           (deref (ql-http:url uri-string) return-type))))
  (assert (ok 'vector "http://cliki.net/recent-changes.rdf"))
  (assert (ok 'string "http://cliki.net/recent-changes.rdf"))
  (assert (ok 'vector "http://cliki.net/"))
  (assert (ok 'string "http://cliki.net/"))  
  (assert (ok 'vector "http://paste.lisp.org/list.rss"))
  (assert (ok 'string "http://paste.lisp.org/list.rss")))




;;; extras

(deref #P"~/.sbclrc" 'string)
(deref #P"~/.sbclrc" 'strings)
(deref #P"~/.sbclrc" 'cons)
(deref #P"~/.sbclrc" 'vector)
(deref (ql-http:url "http://www.cliki.net/recent-changes.rdf"))
(deref (ql-http:url "http://www.cliki.net/recent-changes.rdf") 'vector)
(deref (ql-http:url "http://www.cliki.net/recent-changes.rdf") 'string)
(deref (ql-http:url "http://www.cliki.net/") 'strings)
(deref (ql-http:url "http://www.cliki.net/") 'string)
(deref (ql-http:url "http://www.cliki.net/") 'vector)
(dclx:printv (deref (puri:uri "http://paste.lisp.org/") 'string))
(dclx:printv (deref (puri:uri "http://paste.lisp.org/list.rss") 'string))
(dclx:printv (deref (ql-http:url  "http://paste.lisp.org/list.rss") 'string))
(deref (ql-http:url "http://cliki.net/recent-changes.rdf") 'string)
(equalp
  (deref (puri:uri "http://cliki.net/recent-changes.rdf") 'vector)
  (deref (ql-http:url  "http://cliki.net/recent-changes.rdf") 'vector))

|#


