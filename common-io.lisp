;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :io
  (:use :closer-common-lisp)
  (:export
    :string-to-octets
    :octets-to-string
    :writing-nicely
    :read-stream-to-string-list
    :read-stream-to-string
    :read-stream-to-byte-vector
    :write-string-list-to-stream
    :read-file-to-list
    :write-list-to-file
    :read-file-to-string-list
    :read-file-to-string
    :write-string-to-file
    :write-string-list-to-file
    :read-file-to-byte-vector
    :write-byte-vector-to-file
    :copy-file
    :file-equal
    :copy-stream
    :copy-binary-stream
    :relative-pathname
    #+cl-ppcre :regex-replace-in-file
    #+cl-ppcre :regex-search-in-file
    #+cl-ppcre :regex-search-in-string
    :random-string
    :make-temporary-filename
    :with-temporary-file
    :-filename-
    :-file-
    #+sbcl :*figlet-program*
    #+sbcl :figlet
    #+cl-ppcre :grep    
    :tee
    :cat
    :head
    :tail))

(in-package :io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Idioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-list (thing)
  (if (atom thing)
    (list thing)
    thing))

(defun string-to-octets (string)
  #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
  #-sbcl (babel:string-to-octets string))

(defun octets-to-string (octets &key (start 0) end)
  #+sbcl (sb-ext:octets-to-string octets :external-format :utf-8 :start start :end end)
  #-sbcl (babel:octets-to-string octets :start start :end end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental I/O Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro writing-nicely (&rest forms)
  `(let ((*print-escape*  t)
          (*print-level*  nil)
          (*print-length* nil)
          (*print-array*  t)
          (*print-pretty* t)
          (*print-circle* t)
          (*print-case*  :downcase)
          (*package*     (find-package :common-lisp)))     
     ,@forms))


(defun read-stream-to-string-list (in)
  (let ((lines '())
        (end-of-file (gensym)))
    (do ((line (read-line in nil end-of-file)
               (read-line in nil end-of-file)))
        ((eq line end-of-file))
      (push line lines))
    (nreverse lines)))


(defun write-string-list-to-stream (string-or-string-list stream &aux
                                 (string-list (ensure-list string-or-string-list)))
  (loop for string in string-list
    do (write-line string stream)))


(defun read-stream-to-string (in)
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
            (buffer (make-string buffer-size)))
      (loop for size = (read-sequence buffer in)
        do (write-string buffer contents :start 0 :end size)
        while (= size buffer-size)))))


(defun read-stream-to-byte-vector (in)
  (let* ((contents (make-array 0 :element-type '(unsigned-byte 8)))
          (buffer-size 4096)
          (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop
      for size = (read-sequence buffer in)
      for len = (length contents)
      do (progn 
           (setf contents (adjust-array contents (+ len size)))
           (replace contents buffer :start1 len :end1 (+ len size) :start2 0 :end2 size))
      while (= size buffer-size)
      finally (return contents))))


(defun read-file-to-list (pathname)
  (with-open-file (in pathname :direction :input)
    (loop for form = (read in nil :eof-marker)
      until (eq form :eof-marker)
      collect form)))


(defun write-list-to-file (list pathname &key (if-exists :overwrite)
                            (if-does-not-exist :create))
  (with-open-file (out pathname :direction :output :external-format :default
                    :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (writing-nicely
      (loop for form in list do (pprint form out)))))
 

(defun read-file-to-string-list (pathname)
  (with-open-file (in pathname :direction :input)
    (let ((lines '())
           (end-of-file (gensym)))
      (do ((line (read-line in nil end-of-file)
             (read-line in nil end-of-file)))
        ((eq line end-of-file))
        (push line lines))
      (nreverse lines))))


(defun read-file-to-string (pathname)
  (with-output-to-string (contents)
    (with-open-file (in pathname :direction :input)
      (let* ((buffer-size 4096)
              (buffer (make-string buffer-size)))
        (loop for size = (read-sequence buffer in)
          do (write-string buffer contents :start 0 :end size)
          while (= size buffer-size))))))


(defun write-string-to-file (string pathname &key (if-exists :overwrite)
                              (if-does-not-exist :create) (external-format :default))
  (with-open-file (out pathname :direction :output  :if-exists if-exists
                         :if-does-not-exist if-does-not-exist
                         :external-format external-format)
    (write-sequence string out)))


(defun write-string-list-to-file (string-list pathname &key (if-exists :overwrite)
                              (if-does-not-exist :create) (external-format :default))
  (with-open-file (out pathname :direction :output  :if-exists if-exists
                         :if-does-not-exist if-does-not-exist
                    :external-format external-format)
    (dolist (string string-list)
      (write-line string out))))


(defun read-file-to-byte-vector (pathname &key (start 0) end)
  (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
    (let* ((end (or end (file-length in)))
            (length (- end start)))
      (file-position in start)
      (let ((byte-vector (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence byte-vector in)
        byte-vector))))


(defun write-byte-vector-to-file (byte-vector pathname &key (file-position 0)
                                   (if-exists :overwrite) (if-does-not-exist :create))
  (with-open-file (out pathname :direction :output :element-type '(unsigned-byte 8)
                    :if-exists if-exists :if-does-not-exist if-does-not-exist)            
    (file-position out file-position)
    (write-sequence byte-vector out)))


(defun copy-stream (in out &key (element-type '(unsigned-byte 8)) finish-output)
  (let* ((buffer-size 4096)
          (buffer (make-array buffer-size :element-type element-type)))
    (labels ((read-chunks ()
               (let ((size (read-sequence buffer in)))
                 (if (< size buffer-size)
                   (write-sequence buffer out :start 0 :end size)
                   (progn
                     (write-sequence buffer out)
                     (read-chunks))))))
      (read-chunks))
    (when finish-output (finish-output out))))


(defun copy-binary-stream (in out &key (chunk-size 16384))
  (do*
    ((buf (make-array chunk-size :element-type '(unsigned-byte 8)))
      (pos (read-sequence buf in) (read-sequence buf in)))
    ((zerop pos))
    (write-sequence buf out :end pos)))


(defun copy-file (from to &key (if-to-exists :supersede))
  (prog1 to
    (with-open-file (input from :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (output to :direction :output :element-type '(unsigned-byte 8)
                        :if-exists if-to-exists)
        (copy-binary-stream input output)))))


(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same contents
 (viewed as binary files)."
  (with-open-file (stream1 file1 :element-type '(unsigned-byte 8))
    (with-open-file (stream2 file2 :element-type '(unsigned-byte 8))
      (and (= (file-length stream1) (file-length stream2))
        (loop :for byte1 := (read-byte stream1 nil nil)
          :for byte2 := (read-byte stream2 nil nil)
          :while (and byte1 byte2)
          :always (= byte1 byte2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun relative-pathname (relative-to pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
      (make-pathname :name (or name (pathname-name pathname))
        :type (or type (pathname-type pathname))
        :directory directory)
      relative-to)))


(defun random-string (&optional (len 36))
  (funcall #'concatenate 'string
    (loop repeat len collect (code-char (+ (char-code #\A) (random 26))))))


(defun make-temporary-filename (&key (directory #p"/tmp/") (type "tmp"))
  (make-pathname
    :directory (pathname-directory directory)
    :name (concatenate 'string (princ-to-string (get-universal-time)) "-" (random-string))
    :type type))


(defmacro with-temporary-file ((&optional (file-var '-file-) (pathname-var '-filename-))
                                &body body)
  `(let ((,pathname-var (make-temporary-filename)))
     (unwind-protect (with-open-file (,file-var ,pathname-var 
                                       :direction :io
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                       ,@body)
       (ignore-errors (delete-file ,pathname-var)))))

(assert (equal "hi" (with-temporary-file (x)
                      (format x "hi")
                      (force-output x)
                      (read-file-to-string -filename-))))

(assert (equal "hi" (with-temporary-file (x fn)
                      (format x "hi")
                      (force-output x)
                      (read-file-to-string fn))))

(assert (equal "hi" (with-temporary-file ()
                      (format -file- "hi")
                      (force-output -file-)
                      (read-file-to-string -filename-))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Regular Expression Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+cl-ppcre
(defun regex-replace-in-file (pattern replacement pathname)
  (with-open-file (stream pathname :direction :io :if-exists :overwrite)
    (loop with regexp = (ppcre:create-scanner pattern)
      with line and missing-newline-p with position = 0
      do (setf (values line missing-newline-p) (read-line stream nil))
      while line do (file-position stream position)
      (write-string (ppcre:regex-replace regexp line replacement) stream)
      (unless missing-newline-p (terpri))  (finish-output stream)
      (setf position (file-position stream)))))


#+cl-ppcre
(defun regex-search-in-file (pattern pathname)
  (remove-if-not #'identity
    (with-open-file (stream pathname :direction :input :if-does-not-exist :error)
      (loop with regexp = (ppcre:create-scanner pattern)
        with line and missing-newline-p 
        do (setf (values line missing-newline-p) (read-line stream nil))
        while line collect (ppcre:scan-to-strings regexp line)))))


#+cl-ppcre
(defun regex-search-in-string (pattern string &optional (start 0) end)
  (remove-if-not #'identity
    (with-input-from-string (stream string :start start :end end)
      (loop with regexp = (ppcre:create-scanner pattern)
        with line and missing-newline-p 
        do (setf (values line missing-newline-p) (read-line stream nil))
        while line collect (ppcre:scan-to-strings regexp line)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIXish Convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cl-ppcre
(defgeneric grep (pattern where &key &allow-other-keys)
  (:method  (pattern (string string) &key (start 0) end)
    (regex-search-in-string pattern string start end))
  (:method  (pattern (pathname pathname) &key replacement)
    (if replacement
      (regex-replace-in-file pattern replacement pathname)
      (regex-search-in-file pattern pathname))))


(defun cat (pathname)
  "return contents of file PATHNAME as a string"
  (read-file-to-string pathname))


(defmacro tee (pathname &body body)
  "simultateously append output directed to *standard-output* to the file at PATHNAME"
  (let ((out (gensym "TEE"))
         (path (gensym "PATHNAME")))
    `(let ((,path ,pathname))
       (format *trace-output* "~&;;;; *standard-output* appending to ~A~%" ,path)
       (unwind-protect
         (progn
           (with-open-file (,out ,path
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
             (let ((*standard-output* (make-broadcast-stream *standard-output* ,out)))
               ,@body)))
         (format *trace-output* "~&;;;; *standard-output* no longer appending to ~A~%" ,path)))))


(defun head (pathname &optional (lines 5))
  (unless (and (integerp lines) (plusp lines))
    (error "lines must be a positive integer, but was specified as ~A" lines))
  (with-open-file (in pathname :direction :input :if-does-not-exist :error)
    (loop with line
      for current-line from 0
      while (< current-line lines)
      do (setf line (read-line in nil))
      while line collect line)))


(defun tail (pathname &optional (lines 5))
  (unless (and (integerp lines) (plusp lines))
    (error "lines must be a positive integer, but was specified as ~A" lines))
  (last (read-file-to-string-list pathname) lines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figlet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+sbcl
(defvar *figlet-program* "/usr/local/bin/figlet")


#+sbcl
(defun figlet (message &key output (executable *figlet-program*)
                (font :small) (justification :left) (width 80) (smush t) (right-to-left nil))
  (check-type width integer)
  (check-type message string)
  (check-type font (or string keyword))
  (check-type executable (or string pathname))
  (check-type justification (member :left :right :center))
  (let ((result (with-output-to-string (s)
                  (terpri s)                 
                  (sb-ext:run-program executable
                    (list
                      "-f" (string-downcase (princ-to-string font))
                      "-w" (princ-to-string width)
                      "-p"
                      (format nil "-~A" (subseq (string-downcase (string justification)) 0 1))
                      (if smush "-s" "-k")
                      (if right-to-left "-R")
                      message)
                    :output s))))
    (typecase output
      (null     result)
      (stream   (princ result output))
      (pathname (write-string-to-file result output :if-exists :append))
      (t        (princ result) (values)))))



#|
(figlet "figlet" :output t) =>
   __ _      _     _   
  / _(_)__ _| |___| |_ 
 |  _| / _` | / -_)  _|
 |_| |_\__, |_\___|\__|
       |___/           


(figlet "hi" :output t :justification :center) =>
                                     _    _ 
                                    | |_ (_)
                                    | ' \| |
                                    |_||_|_|


(figlet "package" :output t :font :big :justification :right) =>
                                                          _                    
                                                         | |                   
                                         _ __   __ _  ___| | ____ _  __ _  ___ 
                                        | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \
                                        | |_) | (_| | (__|   < (_| | (_| |  __/
                                        | .__/ \__,_|\___|_|\_\__,_|\__, |\___|
                                        | |                          __/ |     
                                        |_|                         |___/      
|#

