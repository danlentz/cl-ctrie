;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :tty
  (:use :common-lisp)
  (:export
    :write-wrap
    :fformat
    :render-layout
    :with-histogram-output
    :ascii-layout
    :test-ascii-layout
    :render-ascii-layout
    :render-heading
    :repeat
    :text-progress-bar))

(in-package :tty)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FFORMAT Compiler Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fformat (&rest all)
  (apply #'format all))

(define-compiler-macro fformat (&whole form stream fmt &rest args)
  (if (constantp fmt)
    (if stream
      `(funcall (formatter ,fmt)
         ,stream ,@args)
      (let ((g!stream (gensym "stream")))
        `(with-output-to-string (,g!stream)
           (funcall (formatter ,fmt)
             ,g!stream ,@args))))
    form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress Bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun text-progress-bar (stream n &key
                           (character #\*) (length 80)
                           (deciles? t) (before "~&[") (after "]~%"))
  "Return a closure that displays a progress bar when called with
increments (defaults to 1).  When the second argument is T, index will be set
to the given value (instead of a relative change).

LENGTH determines the number of CHARACTERs to display (not including AFTER and
BEFORE, which are displayed when the closure is first called and after the
index reaches N, respectively).  When DECILES?, characters at every decile
will be replaced by 0,...,9.

When STREAM is NIL, nothing is displayed."
  (unless stream (return-from text-progress-bar (lambda ())))
  (let* ((it          (make-string length :initial-element character))
          (characters (prog1 it
                        (when deciles?
                          (loop for index :below 10 do
                            (replace it (format nil "~d" index)
                              :start1 (floor (* index length) 10))))))
          (index      0)
          (position   0))
    (lambda (&optional (increment 1) absolute?)
      (when before
        (format stream before)
        (setf before nil))
      (if absolute?
        (progn
          (assert (<= index increment) () "Progress bar can't rewind.")
          (setf index increment))
        (incf index increment))
      (assert (<= index n) () "Index ran above total (~A > ~A)." index n)
      (let ((target-position (floor (* index length) n)))
        (loop while (< position target-position) do
          (princ (aref characters position) stream)
          (incf position)))
      (when (and (= index n) after)
        (format stream after)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORMAT Control Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (format nil "~v/repeat/" 5 "xyz")
;;; --> "xyzxyzxyzxyzxyz"

(defun cl-user::repeat (stream argument colon at &rest parameters)
  (declare (ignore at))
  (let ((repeat (or (first parameters) 1)))
    (check-type repeat integer)
    (if colon
        (loop :repeat repeat :do (prin1 argument stream))
        (loop :repeat repeat :do (princ argument stream)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Histogram -- adapted from source released by Scott Fahlman, Carnegie-Mellon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-histogram-output ((low high &key stream (bucket-size 1) (limit 60))
                                  &body body)
  "Create a histogram with buckets of the specified size (defaults to 1),
  spanning the range from Low (inclusive) to High (exclusive), with
  two additional buckets to catch values below and above this range.
  LIMIT specifies lhe maximum number of #'s that are to be printed.
  The body is executed as a progn, and every call to COLLECT within
  the body provides a value for the histogram to count.  When Body
  exits, the histogram is printed to stream or returned a a string"
  `(let* ((*hist-lower-limit* ,low)
           (*hist-upper-limit* ,high)
           (*hist-bucket-size* ,bucket-size)
           (*hist-nbuckets*
             (+ 2 (ceiling (- *hist-upper-limit* *hist-lower-limit*)
                    *hist-bucket-size*)))
           (*hist-array* (make-array *hist-nbuckets* :initial-element 0)))
     (flet ((collect (value)
              (cond
                ((< value *hist-lower-limit*)
                  (incf (svref *hist-array* (1- *hist-nbuckets*))))
                ((>= value *hist-upper-limit*)
                  (incf (svref *hist-array* (- *hist-nbuckets* 2))))
                (t (incf (svref *hist-array*
                           (floor (- value *hist-lower-limit*)
                             *hist-bucket-size*)))))))
       (progn ,@body))
     
     (format ,stream "~%~A~%"
       (with-output-to-string (out)
         (let ((biggest 0) (scale 1) (legend ""))
           (dotimes (b (- *hist-nbuckets* 2))
             (when (> (svref *hist-array* b) biggest)
               (setq biggest (svref *hist-array* b))))           
           (when (> biggest ,limit)
             (setq scale (ceiling biggest ,limit))
             (setf legend (format nil "~%SCALE:    # = ~S ~%" scale)))           
           (format out "~&< ~S: ~12,8T~S~%" *hist-lower-limit*
             (svref *hist-array* (1- *hist-nbuckets*)))           
           (do ((b 0 (1+ b)) (bval *hist-lower-limit* (+ bval *hist-bucket-size*))
                 (bcount 0)) ((= b (- *hist-nbuckets* 2)))
             (setq bcount (svref *hist-array* b))
             (multiple-value-bind (q r) (truncate bcount scale)
               (format out "~S: ~12,8T~S~20,8T~V,1,0,'#@A~%"
                 bval bcount (1+ q) (if (zerop r) #\  #\.))))
           (format out "> ~S: ~12,8T~S~%~A~%" *hist-upper-limit*
             (svref *hist-array* (- *hist-nbuckets* 2)) legend))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested ASCII Rendered Panel Layout  -- http://paste.lisp.org/display/39459
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enumerate (start end)
  (if (> start end) '()
      (cons start (enumerate (1+ start) end))))

(defun repeat (item times)
  (loop repeat times collect item))

(defun join (output-type-spec things delimeter)
  (concatenate output-type-spec delimeter
    (reduce
      (lambda (x y)
        (concatenate output-type-spec x delimeter y))
      things)
    delimeter))

(defun ascii-rows (graphic)
  (length graphic))

(defun ascii-cols (graphic)
  (length (car graphic)))

(defun ascii-size (graphic)
  (list (ascii-rows graphic) (ascii-cols graphic)))

;; (string-replace-section "abcdefghijklmnopqrstuvwxyz" "------" 18)
;; => "abcdefghijklmnopqr------yz"

(defun string-replace-section (string replace from-index)
  (concatenate 'string (subseq string 0 from-index)
    replace (subseq string (+ from-index (length replace)))))

(defun ascii-composite (canvas graphic location)
  "paste graphic on top of canvas at location"
  (concatenate 'list
    (subseq canvas 0 (car location))
    (mapcar (lambda (canvas-line graphic-line)
              (string-replace-section canvas-line graphic-line (cadr location)))
      (nthcdr (car location) canvas) graphic)
    (subseq canvas (+ (car location) (ascii-rows graphic)))))

(defun ascii-layout (graphics)
  "((graphic graphic graphic)
    (graphic graphic graphic))"
  (let* ((table-cell-sizes
           (mapcar (lambda (table-row) (mapcar #'ascii-size table-row))
             graphics))
          ;; across each row find maximum row size
          (row-sizes
            (mapcar (lambda (table-row) (apply #'max (mapcar #'car table-row)))
              table-cell-sizes))
          ;; do the same for columns
          (column-sizes
            (mapcar (lambda (column)
                      (apply #'max
                        (mapcar #'cadr
                          (mapcar (lambda (row)
                                    (nth column row)) table-cell-sizes))))
              (enumerate 0 (length graphics))))
          ;;    + <colsize1> + <colsize2> + ... +
          (table-frame-row
            (join 'string
              (mapcar (lambda (size)
                        (make-string size :initial-element #\-)) column-sizes) "+"))
          (table-row
            (join 'string
              (mapcar (lambda (size)
                        (make-string size :initial-element #\Space)) column-sizes) "|"))
          (ascii-layout
            (join 'list
              (mapcar (lambda (cols-for-cell) (repeat table-row cols-for-cell))
                row-sizes)
              (list table-frame-row))))
    (loop for row-size in (cons 0 row-sizes)
      for graphic-row in graphics
      summing (1+ row-size) into row
      do (loop for col-size in (cons 0 column-sizes)
           for graphic in graphic-row
           summing (1+ col-size) into col
           do (setq ascii-layout (ascii-composite ascii-layout
                                   graphic (list row col)))))
    ascii-layout))

(defvar *example*
  (ascii-layout
    (list (list (ascii-layout '((("x") ("y") ("z")) (("1") ("2") ("3"))))
            (ascii-layout '((("  /\\  "
                              " /  \\ "
                              "/____\\")
                              
                              (" _____ "
                               "|     |"
                               "|     |"
                               "|_____|")
                              
                              ("\\\\  //"
                               " \\\\// "
                               " //\\\\ "
                               "//  \\\\"))
                             
                             (("1567")  ("##########"
                                         "         #"
                                         "#######  #"
                                         "#     #  #"
                                         "#        #"
                                          "##########")
                               ("hello :O"
                                 "'world'?"))))))))
(defun test-ascii-layout ()
  (assert
    (equalp
      (format nil "~{~a~%~}" *example*)
"+-------+----------------------------+
|+-+-+-+|+------+----------+--------+|
||x|y|z|||  /\\  | _____    |\\\\  //  ||
|+-+-+-+|| /  \\ ||     |   | \\\\//   ||
||1|2|3|||/____\\||     |   | //\\\\   ||
|+-+-+-+||      ||_____|   |//  \\\\  ||
|       |+------+----------+--------+|
|       ||1567  |##########|hello :O||
|       ||      |         #|'world'?||
|       ||      |#######  #|        ||
|       ||      |#     #  #|        ||
|       ||      |#        #|        ||
|       ||      |##########|        ||
|       |+------+----------+--------+|
+-------+----------------------------+
")))       

(defun render-ascii-layout (stream spec)
   (format stream "~{~a~%~}" spec))

;; (render-ascii-layout t *example*) ==>
;; +-------+----------------------------+
;; |+-+-+-+|+------+----------+--------+|
;; ||x|y|z|||  /\  | _____    |\\  //  ||
;; |+-+-+-+|| /  \ ||     |   | \\//   ||
;; ||1|2|3|||/____\||     |   | //\\   ||
;; |+-+-+-+||      ||_____|   |//  \\  ||
;; |       |+------+----------+--------+|
;; |       ||1567  |##########|hello :O||
;; |       ||      |         #|'world'?||
;; |       ||      |#######  #|        ||
;; |       ||      |#     #  #|        ||
;; |       ||      |#        #|        ||
;; |       ||      |##########|        ||
;; |       |+------+----------+--------+|
;; +-------+----------------------------+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEREDOC Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let ((pattern (nreverse chars))
          output)
      (labels ((match (pos chars)
        (if (null chars)
          pos
          (if (char= (nth pos pattern) (car chars))
              (match (1+ pos) (cdr chars))
              (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
        (do (curr
             (pos 0))
            ((= pos (length pattern)))
          (setf curr (read-char stream)
                pos (match pos (list curr)))
          (push curr output))
        (coerce
          (nreverse
            (nthcdr (length pattern) output))
          'string)))))

(set-dispatch-macro-character  #\# #\> #'|#>-reader|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #` Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg collect (symb 'a i))
       ,(funcall (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character  #\# #\` #'|#`-reader|))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGEX Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun segment-reader (stream ch n)
  (when (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
             (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))


(defmacro subst-mode-ppcre-lambda-form (args)
  (alexandria:once-only (args)
    (alexandria:with-gensyms (str)
      ``(lambda (,',str)
          (cl-ppcre:regex-replace-all
           ,(car ,args)
           ,',str
           ,(cadr ,args))))))


(defmacro match-mode-ppcre-lambda-form (args mods fn)
  (alexandria:once-only (args mods)
    (alexandria:with-gensyms (str)
      ``(lambda (,',str)
          (,,fn
           ,(if (zerop (length ,mods))
              (car ,args)
              (format nil "(?~a)~a" ,mods (car ,args)))
           ,',str)))))


(defun scan-to-strings-values (regex target-string)
  (multiple-value-bind (all subs)
      (cl-ppcre:scan-to-strings regex target-string)
    (values-list (nconc (list all) (when subs (coerce subs 'list))))))


(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((or (char= mode-char #\m) (char= mode-char #\c))
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        1)
        (coerce (loop for c = (read-char stream)
                      while (alpha-char-p c) collect c
                      finally (unread-char c stream))
                'string)
        (if (char= mode-char #\m) 'cl-ppcre:scan 'scan-to-strings-values)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)))
      (t (error "Unknown #~~ mode character")))))

(set-dispatch-macro-character #\# #\~ #'|#~-reader|)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reisbeck / Write Wrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-wrap (stream strng width &optional indent (first-indent indent))
  " Writes string to stream, split into width-size lengths, breaking
   at returns and spaces in the string, if possible, indenting every
   line indent spaces (default = 0), except the first line which is
   indented first-indent spaces (default = indent).  Note: to generate
   a string simply use with-output-to-string
   ;;;
   ;;;   (WITH-OUTPUT-TO-STRING (s) (WRITE-WRAP s ...))
   ;;;
   ;;;   (WRITE-WRAP stream string width &optional indent first-indent)
   ;;;"
(let ((*print-pretty* nil))
    (do* ((end (length strng))
          (indent-string (when (and indent (> indent 0))
                           (make-string indent
                                        :initial-element #\space)))
          (first-indent-string (when (and first-indent (> first-indent 0))
                                 (make-string first-indent
                                              :initial-element #\space)))
          (start 0 (1+ next))
          (next (break-pos strng start end width)
                (break-pos strng start end width))
          (margin first-indent-string indent-string))
         ((null next))
      (when margin (write-string margin stream))
      (write-string strng stream :start start :end next)
      (terpri stream))))


(defun whitespace-p (ch)
  "(whitespace-p char) is true if ch is whitespace."
  (member ch '(#\linefeed #\newline #\return #\space #\tab)))


(defun break-pos (strng start end width)
  ";; `(break-pos string start end width)1 returns the position to break string
   ;;   at, guaranteed to be no more than width characters.  If there's a`
   ;;   return, its position is used, else the last space before the width
   ;;   cutoff, else width.  If the end comes before width, then end is 
   ;;   returned."
(unless (or (null start) (>= start end))
   (let ((limit (min (+ start width) end)))
     (or (position #\newline strng :start start :end limit)
	 (and (= end limit) end)
	 (position #\space strng :start start :end limit :from-end t)
	 limit))))		


(defun non-whitespace-pos (strng &optional (start 0))
  "returns the position of the first non-whitespace character in string,
   after start, if any.
     ;;
     ;; (non-whitespace-pos string &optional start)
     ;;"
  (position-if-not #'whitespace-p strng :start start))



#|
Copyright (c) 2003 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#
