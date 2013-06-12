;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbology
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *alternative-package-exports*
  '(:get :put :put-if :put-if-not :put-ensure :put-replace :put-replace-if
     :put-update :put-update-if :drop :drop-if :drop-if-not :make :new ;; :def
     :do :keys :values :size :test :hash :next-id
     :readonly-p :map :map-keys :map-values :clear :pprint :error :to-alist :to-hashtable
     :from-hashtable :from-alist :empty-p :save :load :export :import :snapshot :fork
     :lambda :lambda-ctrie :lambda-class :lambda-object :lambda-spawn :define 
     :max-depth :min-depth :enable-pooling :disable-pooling :pooling-enabled-p
     :pool-status :index :all :names :find :name)
  "defines the symbols exported by the 'alternative' symbol naming styled package")

;; from :hu.dwim.util

(defun fully-qualified-symbol-name (symbol &optional separator)
  (flet ((string+ (&rest args) (apply #'concatenate 'string args)))
    (let* ((symbol-name (symbol-name symbol))
            (package (symbol-package symbol))
            (keyword-package (load-time-value (find-package :keyword))))
      (if package
        (string+
          (unless (eq package keyword-package) (package-name package))
          (or separator (if (or (eq package keyword-package)
                              (eq (nth-value 1 (find-symbol symbol-name package)) :external))
                          ":" "::"))
          symbol-name)
        (string+ "#:" symbol-name)))))

(defun find-fully-qualified-symbol (name &key (otherwise nil))
  "The inverse of FULLY-QUALIFIED-SYMBOL-NAME. Does not INTERN but it
  does instantiate package-less symbols."
  (check-type name string)
  (if (starts-with-subseq "#:" name)
      (make-symbol (subseq name 2))
    (hu.dwim.util:find-symbol* name :packages '() :otherwise otherwise)))

(defun if-symbol-exists (package name)
  "Can be used to conditionalize at read-time like this:
   #+#.(hu.dwim.util:if-symbol-exists \"PKG\" \"FOO\")(pkg::foo ...)"
  (if (and (find-package (string package))
           (find-symbol (string name) (string package)))
      '(:and)
      '(:or)))

(defun gensym-list (length)
  "generate a list of LENGTH uninterned symbols"
  (loop repeat length collect (gensym)))
  
(defmacro gensym-values (num)
  `(values ,@(loop REPEAT num COLLECT '(gensym))))

(defmacro gensym-let ((&rest symbols) &body body)
  (let ((n (length symbols)))
    `(multiple-value-bind ,symbols (gensyms-values ,n)
       ,@body)))

(defun internal-symbols (package &optional (return-type 'list))
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0))
        (used (package-use-list package)))
    (do-symbols (symbol (find-package package))
      (unless (find (symbol-package symbol) used)
        (vector-push-extend symbol acc)))
    (coerce acc return-type)))

(defun external-symbols (package &optional (return-type 'list))
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol (find-package package))
      (vector-push-extend symbol acc))
    (coerce acc return-type)))

(defun home-symbols (package &optional (return-type 'list))
  (coerce (loop for sym being the present-symbols in package
            when (eq (find-package package) (symbol-package sym))
            collect sym)
    return-type))
    
(defun home-functions (package)
  (remove-if-not #'fboundp (home-symbols package)))


(defun generate-alternative-package ()
  "Generate package :CTRIE which supports an alternative package/naming style
  in which the contents are meant to be used in a fully-qualified package:symbol
  manner, rather than used or imported.  For example, `#'CL-CTRIE:CTRIE-GET` would
  be equivalent to `#'CTRIE:GET,` `#'CL-CTRIE:MAKE-CTRIE` to `#'CTRIE:MAKE` and
  so forth."
  (let* ((banner (format nil "Mapping symbols to alternative package: \"CTRIE\""))
          (uscores (make-string (length banner) :initial-element #\-))
          (*package* (find-package :cl-ctrie))
          (*print-length* 20)
          (*print-lines* 1))
    (format t "~%~%~A~%~A~%~%" banner uscores)
    (delete-package :ctrie)
    (make-package :ctrie :use nil)
    (mapc (lambda (sym) (export (intern (string sym) :ctrie) :ctrie))
      *alternative-package-exports*)
    (loop for alt in (sort (external-symbols :ctrie) #'string<)
      for sym = (symbolicate "CTRIE-" alt)
      for xsym = (symbolicate alt "-CTRIE")
      for xsympl = (symbolicate alt "-CTRIES")
      do (format t "~&~23S -> " alt)
      when #3=(find-class sym nil) do (format t "C: ~A " (setf (find-class alt) #3#))
      do (if #1=(macro-function sym)
           (format t "M: ~A " (setf (macro-function alt) #1#))
           (if #2=(macro-function xsym)
             (format t "M: ~A " (setf (macro-function alt) #2#))
             (if (fboundp xsympl)
               (format t "F: ~A " (setf (fdefinition alt) (fdefinition xsympl)))
               (if (fboundp xsym)
                 (format t "F: ~A " (setf (fdefinition alt) (fdefinition xsym)))
                 (when (fboundp sym)
                   (format t "F: ~A " (setf (fdefinition alt) (fdefinition sym))))))))
;;      (setf (fdefinition 'ctrie:all) (fdefinition 'cl-ctrie:all-ctries))
      (terpri)
      (values))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Helpful Utility Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota* (n &optional (start 0))
  "Creates a list of n elements starting with element START.
START can be a number, a character, a string or a symbol."
  (etypecase start
    (number (loop for i from start
                  repeat n
                  collect i))
    (character (loop with c = (char-code (or (and (characterp start) start) #\a))
                     for i from 0
                     repeat n
                     collect (code-char (+ c i))))
    (string (loop with c = (char-code (or (and (stringp start) (aref start 0)) #\a))
                     for i from 0
                     repeat n
                     collect (string (code-char (+ c i)))))
    (symbol (loop with c = (char-code (or (and (symbolp start) (aref (string start) 0)) #\a))
                     for i from 0
                     repeat n
                     collect (intern (string (code-char (+ c i))) (symbol-package start))))))

(defun repeat (it n)
  "Returns a list of n elements of IT."
  (loop repeat n collect it))

#+swank 
(defun ^ (thing &optional wait)
  "inspect THING in the emacs SLIME inspector, optionally waiting
  for the inspector to be dismissed before continuing if WAIT is
  not null"
  (swank:inspect-in-emacs thing :wait wait))

#+swank
(defun edit (&optional what)
  "Edit `WHAT` in Emacs.
  ```
  ;;; WHAT can be:
  ;;; -   A pathname or a string,
  ;;; -   A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  ;;; -   A function name (symbol or cons),
  ;;; -   NIL.```"
  (swank:ed-in-emacs what))


(define-symbol-macro ?  (prog1 * (describe *)))

#+swank
(define-symbol-macro ?^ (prog1 * (^ *)))

(defvar *break* t
  "special variable used for dynamic control of break loops see {defun :break}")

(fmakunbound :break)
;;; nikodemus/lisppaste 
(defun :break (name &rest values)
  "Demark an instrumented break-point that includes a STOP-BREAKING
   restart.  Subsequently calling (:break t) will re-enable :break
   breakpoints."
  (if *break* (restart-case (break "~A = ~{~S~^, ~}" name values)
                (stop-breaking ()
                  :report "Stop breaking"
                  (setf *break* nil)))
    (when (and (eq name t)(not values))
      (setf *break* t)))
  (values-list values))

;; Steve Goss's experimental "threading" macro
;; http://blog.thezerobit.com/2012/07/21/immutable-persistent-data-structures-in-common-lisp.html

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  " * EXAMPLE
  ```;;; (-> (empty-map)
     ;;;   (with :a 100)
     ;;;   (with :b 200)
     ;;;   (less :a))
     ;;;
     ;;; #{| (:B 200) |}
 ```"
  (if form-supplied-p
    (if more
      `(-> (-> ,x ,form) ,@more)
      (if (listp form)
        `(,(car form) ,x ,@(cdr form))
        (list form x)))
    x))

#|
(defmacro -> (value &body body)
  `(progn
     ,(loop for form in (cons nil body)
	    for ->form = value
	      then (list* (first form) ->form (rest form))
	    finally (return ->form))))

(defmacro ->> (value &body body)
  `(progn
     ,(loop for form in (cons nil body)
	    for ->>form = value
	      then (append form (list ->>form))
	    finally (return ->>form))))
|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map-update 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-update (fun src &rest lists)
  (funcall
    (reduce (lambda (cont list-elms)
              (lambda ()
                (apply fun (funcall cont) list-elms) ))
            (apply #'mapcar #'list lists)
            :initial-value (constantly src) )))


(defun subst-all (to-list from-list src)
  "```
   ;;; (subst-all '(1 2 3) '(a b c) '(a b c d c b a))
   ;;;  => '(1 2 3 d 3 2 1)
   ```"
  (map-update
    (lambda (src from to) (subst to from src))
    src from-list to-list))





;; (defun curry (fn &rest pref-args)
;;   (lambda (&rest suf-args)
;;     (apply fn (append pref-args suf-args);; )))

;; (defun rcurry (fn &rest suf-args)
;;   (lambda (&rest pref-args)
;;     (apply fn (append pref-args suf-args))))

;; (defun collect-if (predicate proseq &rest rest)
;;  (apply 'remove-if-not predicate proseq rest))

;; (declaim (inline ensure-function))	; to propagate return type.
;; (declaim (ftype (function (t) (values function &optional)) ensure-function))

;; (defun ensure-function (function-designator)
;;   "Returns the function designated by FUNCTION-DESIGNATOR:
;; if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
;; it must be a function name and its FDEFINITION is returned."
;;   (if (functionp function-designator)
;;     function-designator
;;     (fdefinition function-
;;      designator)))


;; (defun compose (function &rest more-functions)
;;   "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
;; arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
;; and then calling the next one with the primary value of the last."
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (reduce (lambda (f g)
;; 	    (let ((f (ensure-function f))
;;                    (g (ensure-function g)))
;; 	      (lambda (&rest arguments)
;; 		(declare (dynamic-extent arguments))
;; 		(funcall f (apply g arguments)))))
;;     more-functions :initial-value function))



;; (defun multiple-value-compose (function &rest more-functions)
;;   "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
;; its arguments to to each in turn, starting from the rightmost of
;; MORE-FUNCTIONS, and then calling the next one with all the return values of
;; the last."
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (reduce (lambda (f g)
;; 	    (let ((f (ensure-function f))
;;                    (g (ensure-function g)))
;; 	      (lambda (&rest arguments)
;; 		(declare (dynamic-extent arguments))
;; 		(multiple-value-call f (apply g arguments)))))
;;     more-functions :initial-value function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'Unique Value' Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun random-string (&key (length 16))
  "Returns a random alphabetic string."
  (let ((id (make-string length)))
    (do ((x 0 (incf x)))
      (( = x length))
      (setf (aref id x) (code-char (+ 97 (random 26)))))
    id))

(defun random-uuid ()
  #+:unicly          (unicly:make-v4-uuid)
  #+:uuid            (uuid:make-v4-uuid)
  #-(or uuid unicly) (random-string :length 32))
  
(defun create-unique-id-byte-vector ()
  "Create a universally unique 16-byte vector using unicly or uuid
  libraries if available, or else fall back to random generation."
  (or
    #+:unicly (unicly:uuid-bit-vector-to-byte-array
               (unicly:uuid-to-bit-vector (unicly:make-v4-uuid)))
    #+:uuid   (uuid:uuid-to-byte-array (uuid:make-v4-uuid))
    (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for i from 0 to 15 do (setf (aref bytes i) (random 255)))
      bytes)))

(defgeneric uuid-to-integer (uuid-designator))

#+unicly
(defmethod uuid-to-integer ((uuid unicly:unique-universal-identifier))
  "Create a universally unique 128-bit integer using unicly when available"
   (unicly::uuid-bit-vector-to-integer 
     (unicly:uuid-to-bit-vector uuid)))
    
(defun create-null-id-byte-vector ()
  "Generate a 16-byte vector representing the NULL uuid."
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

(defun byte-vector-to-hex-string (vector)
  "Return a 32 character string that maps uniquely to the given byte vector."
  (with-output-to-string (out)
    (loop for byte across vector do (format out "~2,'0x" byte))))

(defun hex-string-to-byte-vector (string)
  "Return the byte vector represented by the (hex) STRING, which is assumed
   to be valid, as by 'byte-vector-to-hex-string'"
  (let ((len (length string))
         (*read-base* 16))
    (loop 
      with bytes = (make-array (ceiling (/ len 2)) :element-type '(unsigned-byte 8))
      for i from 0 by 2 for j from 0 to (ceiling (/ len 2)) while (< i (1- len))
      do (setf (aref bytes j) (read-from-string string nil 0 :start i :end (+ 2 i)))
      finally (return bytes))))

(defun test-byte-vector-hex-string-roundrip ()
  (let* ((bv0 (create-unique-id-byte-vector))
          (bv1 (hex-string-to-byte-vector (byte-vector-to-hex-string bv0))))
    (assert (equalp bv0 bv1)) 
    (values bv0 bv1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pjb

(defstruct cache
  alist)

(defun assoc-cache* (cache key &optional default-value-thunk)
  (let ((entry (assoc key (cache-alist cache))))
    (unless entry
      (setf entry (cons key (funcall default-value-thunk)))
      (push entry (cache-alist cache)))
    (cdr entry) ))

(defmacro assoc-cachef (cache key &optional default-value)
  `(assoc-cache* ,cache ,key (lambda () ,default-value)))



;; (defun reuse-cons (x y x-y)
;;   "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
;;   (if (and (eql x (car x-y)) (eql y (cdr x-y)))
;;       x-y
;;     (cons x y)))

(defvar *cons-table*  (make-hash-table :test #'equal))
(defvar *a-cons-cell* (list nil)
  "holding area for spare cons.")

(defun hcons (x y)                      
  (setf (car *a-cons-cell*) x (cdr *a-cons-cell*) y)
  (let ((z (gethash *a-cons-cell* *cons-table*)))
    (or z
      (prog1 (setf (gethash *a-cons-cell* *cons-table*) *a-cons-cell*)
        (setq *a-cons-cell* (list nil))))))

(defun hlist* (first rest)
  (cond
    ((atom rest)  (hcons first rest))
    ((null rest)  (hcons first nil))
    ((consp rest) (hcons first (hlist* (car rest) (cdr rest))))))
;;    (t (error "invalid list"))

(defun hlist (&rest args)
  (when args
    (hlist* (car args) (cdr args))))

(defvar *an-mcons-pair* nil) ;;  (mm:mcons nil nil))

;; (mm:mptr-to-lisp-object  (mm::lisp-object-to-mptr (hcons nil nil)))

;;   defun mcons (x y)
  
;; (eq (hlist (iota 17)) (hlist (iota 17)))
  
;;   (defun hh (seq)
;;   (loop for cell on (reverse seq)
;;     for hlist = (print (hcons (car cell) nil)) then (print (hcons (car cell) hlist))
;;     collect (print (hcons (car cell) (cdr cell)))))

;; (hlist* :z (list :a :b :c :d))

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))


(defmacro multiple-setf (value &rest places)
  "The multiple-setf macro was written by Mario Castel‚àö¬∞n. It is a
 beautiful form to support multiple places in zerof and nilf."
  (alexandria:once-only (value)
    `(setf ,@(loop for place in places
               append `(,place ,value)))))

(defmacro nilf (&rest places)
  "Set PLACES to nil"
  `(multiple-setf nil ,@places))

(defmacro aconsf (place key value &environment env)
  "CONS is to PUSH as ACONS is to ACONSF; it pushes (cons KEY VALUE) to the PLACE."
  (multiple-value-bind (temps vals stores set-value get-value)
      (get-setf-expansion place env)
    (unless (null (cdr stores))
      (error "ACONSF can't store to this form: ~:_~S" place))
    (alexandria:once-only (key value)
      `(let* (,@(mapcar 'list temps vals)
              (,(car stores)
               (acons ,key ,value ,get-value)))
         ,set-value
         ,value))))

;; (define-modify-macro nconcf (&rest lists)
;;   nconc
;;   "Modify-macro for NCONC. Sets place designated by the first argument to
;; the result of calling NCONC with the place and the LISTS.")



(defmacro fill-hash (hash-table &body key-vals)
  "Macro for filling a hash-table.  returns the filled hash-table."
  (let ((table (gensym "hash-table")))
    `(let ((,table ,hash-table))
       ,@(loop for (key value) in key-vals
            collect `(setf (gethash ,key ,table) ,value))
       ,table)))

(defparameter *codon-table*
  (fill-hash (make-hash-table :test #'equal)
    ("UUU" "F") ("UUC" "F") ("UUA" "L") ("UUG" "L")
    ("UCU" "S") ("UCC" "S") ("UCA" "S") ("UCG" "S")
    ("UAU" "Y") ("UAC" "Y") ("UAA" "*") ("UAG" "*")
    ("UGU" "C") ("UGC" "C") ("UGA" "*") ("UGG" "W")
    ("CUU" "L") ("CUC" "L") ("CUA" "L") ("CUG" "L")
    ("CCU" "P") ("CCC" "P") ("CCA" "P") ("CCG" "P")
    ("CAU" "H") ("CAC" "H") ("CAA" "Q") ("CAG" "Q")
    ("CGU" "R") ("CGC" "R") ("CGA" "R") ("CGG" "R")
    ("AUU" "I") ("AUC" "I") ("AUA" "I") ("AUG" "M")
    ("ACU" "T") ("ACC" "T") ("ACA" "T") ("ACG" "T")
    ("AAU" "N") ("AAC" "N") ("AAA" "K") ("AAG" "K")
    ("AGU" "S") ("AGC" "S") ("AGA" "R") ("AGG" "R")
    ("GUU" "V") ("GUC" "V") ("GUA" "V") ("GUG" "V")
    ("GCU" "A") ("GCC" "A") ("GCA" "A") ("GCG" "A")
    ("GAU" "D") ("GAC" "D") ("GAA" "E") ("GAG" "E")
    ("GGU" "G") ("GGC" "G") ("GGA" "G") ("GGG" "G")))


;;; Clozure Common Lisp (??)
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
           (exp (macroexpand exp1))
           (*print-circle* nil))
     (format *trace-output* "~%;; Form: ~W"  (quote ,form))
     #+() (pprint (quote ,form) *trace-output*)
     (cond ((equal exp exp1)
             (format *trace-output* "~%;;~%;; Macro expansion:~%")
             (pprint exp *trace-output*))
       (t (format *trace-output* "~&;; First step of expansion:~%")
         (pprint exp1 *trace-output*)
         (format *trace-output* "~%;;~%;; Final expansion:~%")
         (pprint exp *trace-output*)))
     (format *trace-output* "~%;;~%;; ")
     (values)))



(defmacro post-incf (place &optional (delta 1))
  "place++ ala C"
  `(prog1 ,place (incf ,place ,delta)))




(defmacro deflex (var val &optional (doc nil docp))    
  "__DEFLEX__ -- Define a top level (global) lexical VAR with initial
   value VAL, which is assigned unconditionally as with
   DEFPARAMETER. If a DOC string is provided, it is attached to both
   the name |VAR| and the name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a
   documentation string of kind 'VARIABLE. The new VAR will have
   lexical scope and thus may be shadowed by LET bindings without
   affecting its dynamic (global) value.

  > Define 'global lexical variables', that is, top-level
  variables (convenient when debugging) that are lexical in scope, and
  thus don't pollute either the special or lexical variable spaces
  [except for the names of the 'shadow' variables (c.f.), which are
  hopefully non-conflicting in most cases]. Thanks to the denizens of
  the 'comp.lang.lisp' newsgroup for many useful discussions (and
  flames!) on this topic, and for the suggestion for the simple and
  efficient (albeit inelegant) 'shadow' variable approach used here.
  [Note: Like several others, I had previously used a single global
  adjustable vector of shadow values, with complicated compile-time
  allocation of indices so that symbol-macro FOO expanded into something
  like this: (AREF *LEXICAL-STORE* (LOAD-TIME-VALUE {index-for-FOO})).
  But the following approach is much simpler and more maintainable.]
  -- 2005-06-12 -- Package bugfix thanks to Adam Warner
  <adam@consulting.net.nz>"
  
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))      ; BUGFIX [see above]
         (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
      `(progn
         (defparameter ,backing-var ,val ,doc)
         (setf (documentation ',var 'variable) ,doc)
         (define-symbol-macro ,var ,backing-var))
      `(progn
         (defparameter ,backing-var ,val)
         (define-symbol-macro ,var ,backing-var)))))

;;; File downloaded from http://rpw3.org/hacks/lisp/deflex.lisp


;;; emacs?
(defun mapappend (fun &rest args)
   (if (some 'null args)
       '()
       (append (apply fun (mapcar 'car args))
         (mapappend fun (mapcar 'cdr args)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLISTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Erik Naggum in comp.lang.lisp.
;; http://groups.google.fr/group/comp.lang.lisp/msg/ac10b819b1117c4f

(defmacro drop (object place &rest keys &key key test test-not &environment environment)
  "Drop a particular OBJECT from list in PLACE.  (Intended as counterpart to PUSH/-NEW.)
Copyright 1999 by Erik Naggum.  Verbatim inclusion and redistribution permitted.
For any other use, contact Erik Naggum."
  (declare (ignore key test test-not))
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place environment)
    (let ((evaled-value (gensym))
          (store-var (first store-vars)))
      (if (cdr store-vars)
        `(let* ((,evaled-value ,object)
                ,@(mapcar #'list vars vals))
           (multiple-value-bind ,store-vars ,reader
             (setq ,store-var (delete ,evaled-value ,store-var :count 1 ,@keys))
             ,writer))
        `(let* ((,evaled-value ,object)
                ,@(mapcar #'list vars vals)
                (,store-var (delete ,evaled-value ,reader :count 1 ,@keys)))
           ,writer)))))





(defun ?? (string-designator &optional package external-only)
  (apply #'apropos string-designator package external-only nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

