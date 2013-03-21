;; regex.lisp



(defpackage :readtable
  (:use :closer-common-lisp :c2mop :alexandria :named-readtables)
  (:export
    #:with-macro-character
    #:with-macro-characters
    #:|#~-reader|))
  
(in-package :readtable)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexically Scoped macro-chars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-macro-character ((ch func) &body body)
  "Bind the reader macro func to ch and execute the body in this
  environment.  Restore the original reader-macros when this form is
  done.
  ;;;
  ;;; (with-macro-character (ch func) body)
  ;;;"
  (let ((c (gensym))
        (f (gensym))
        (o (gensym)))
    `(let ((,c ,ch)
           (,f ,func))
       (let ((,o (get-macro-character ,c)))
         (set-macro-character ,c ,f)
         (unwind-protect
              (progn ,@body)
           (set-macro-character ,c ,o))))))


(defmacro with-macro-characters (pairs &body body)
  "Bind the reader macro func1 to ch1, and so on, and execute the body
   in this environment. Restore the original reader-macros when this
   form is done.
   ;;;
   ;;; (with-macro-characters ((ch1 func1) (ch1 func2) ...) body)
   ;;;"
  (if (null pairs)
      `(progn ,@body)
      `(if (oddp (length ',(car pairs)))
           (error "with-macro-characters: ~A must be a pair of a
           character and a reader-macro-function" ',(car pairs))
           (with-macro-character ,(car pairs)
             (with-macro-characters ,(cdr pairs)
               ,@body)))))


;; (defun try-it ()
;;   (with-macro-characters
;;       ((#\! #'reader-iota-0)
;;        (#\@ #'reader-iota-1))
;;     (concatenate 'list
;;                  '(first)
;;                  (read-from-string "!10")
;;                  '(second)
;;                  (read-from-string "@10"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doug Hoytes PPCRE Regular Expression Reader from "Let Over Lambda"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun segment-reader (stream ch n)
  (when (> n 0)
    (let ((chars))
      (do ((curr (read-char stream) (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro subst-mode-ppcre-lambda-form (args)
  (once-only (args)
    (with-gensyms (str)
      ``(lambda (,',str)
          (cl-ppcre:regex-replace-all
           ,(car ,args)
           ,',str
           ,(cadr ,args))))))

#+cl-ppcre
(defmacro match-mode-ppcre-lambda-form (args mods fn)
  (once-only (args mods)
    (with-gensyms (str)
      ``(lambda (,',str)
          (,,fn
           ,(if (zerop (length ,mods))
              (car ,args)
              (format nil "(?~a)~a" ,mods (car ,args)))
           ,',str)))))

#+cl-ppcre
(defun scan-to-strings-values (regex target-string)
  (multiple-value-bind (all subs)
      (cl-ppcre:scan-to-strings regex target-string)
    (values-list (nconc (list all) (when subs (coerce subs 'list))))))

#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((or
         (char= mode-char #\m)
         (char= mode-char #\c)) (match-mode-ppcre-lambda-form
                                  (segment-reader stream (read-char stream) 1)
                                  (coerce (loop for c = (read-char stream)
                                            while (alpha-char-p c) collect c
                                            finally (unread-char c stream))
                                    'string)
                                  (if (char= mode-char #\c)
                                    'cl-ppcre:scan
                                    'scan-to-strings-values)))
      ((char= mode-char #\s)   (subst-mode-ppcre-lambda-form
                                 (segment-reader stream (read-char stream) 2)))
      (t                       (error "Unknown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)


#|

(funcall #~c/.*/ "abcde")
0
5
#()
#()

(funcall #~m/.*/ "abcde")
"abcde"

(funcall #~m/abc(.*)e/ "abcde")
"abcde"
"d"

(funcall #~m/c(..)/ "abcde")
"cde"
"de"

(funcall #~m/\w*/ "abcde")
"abcde"

|#
