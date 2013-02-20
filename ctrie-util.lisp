;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor for experimental alternative package interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *alternative-package-exports*
  '(:get :put :put-if :put-if-not :put-ensure :put-replace :put-update
     :drop :make :new :do :keys :values :size :test :hash :readonly-p
     :map :map-keys :map-values :clear :pprint :error :to-alist :to-hashtable
     :from-hashtable :from-alist :empty-p :save :load :export :import :snapshot :fork
     :lambda :lambda-ctrie :lambda-class :lambda-object :lambda-spawn :define 
     :max-depth :min-depth :enable-pooling :disable-pooling :pooling-enabled-p
     :pool-status :ps :index :all :names :find :name)
  "defines the symbols exported by the 'alternative' symbol naming styled package")


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

(defun gensym-list (length)
  "generate a list of LENGTH uninterned symbols"
  (loop repeat length collect (gensym)))
  
(defmacro gensym-values (num)
  `(values ,@(loop REPEAT num COLLECT '(gensym))))

(defmacro gensym-let ((&rest symbols) &body body)
  (let ((n (length symbols)))
    `(multiple-value-bind ,symbols (gensyms-values ,n)
       ,@body)))

(defun random-string (&key (length 16))
  "Returns a random alphabetic string."
  (let ((id (make-string length)))
    (do ((x 0 (incf x)))
      (( = x length))
      (setf (aref id x) (code-char (+ 97 (random 26)))))
    id))

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

;; (test-byte-vector-hex-string-roundrip)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;   #(210 216 162 217 188 189 78 162 150 249 163 170 175 143 56 10)
;;
;; (test-byte-vector-hex-string-roundrip)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185)
;;   #(18 84 222 74 74 46 68 53 134 219 105 134 17 177 38 185))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macrology originating from LMJ's excellent LPARALLEL http://www.lparallel.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro let1 (var value &body body)
    "Make a single `let' binding, heroically saving three columns."
    `(let ((,var ,value))
       ,@body))

  (defmacro defun/inline (name args &body body)
    "define a function automatically declared to be INLINE"
    `(progn
       (declaim (inline ,name))
       (defun ,name ,args ,@body)))

  (defmacro once-only-1 (var &body body)
    (let ((tmp (gensym (symbol-name var))))
      ``(let ((,',tmp ,,var))
          ,(let ((,var ',tmp))
             ,@body))))

  (defmacro once-only (vars &body body)
    (if vars
      `(once-only-1 ,(car vars)
         (once-only ,(cdr vars)
           ,@body))
      `(progn ,@body)))

  (defun unsplice (form)
    (if form (list form) nil))

  (defun has-docstring-p (body)
    (and (stringp (car body)) (cdr body)))

  (defun has-declare-p (body)
    (and (consp (car body)) (eq (caar body) 'declare)))

  (defmacro with-preamble ((preamble body-var) &body body)
    "Pop docstring and declarations off `body-var' and assign them to `preamble'."
    `(let ((,preamble (loop
                        :while (or (has-docstring-p ,body-var)
                                 (has-declare-p ,body-var))
                        :collect (pop ,body-var))))
       ,@body))

  (defmacro defmacro/once (name params &body body)
    "Like `defmacro' except that params which are immediately preceded
   by `&once' are passed to a `once-only' call which surrounds `body'."
    (labels ((once-keyword-p (obj)
               (and (symbolp obj) (equalp (symbol-name obj) "&once")))
              (remove-once-keywords (params)
                (mapcar (lambda (x) (if (consp x) (remove-once-keywords x) x))
                  (remove-if #'once-keyword-p params)))
              (find-once-params (params)
                (mapcon (lambda (x)
                          (cond ((consp (first x))
                                  (find-once-params (first x)))
                            ((once-keyword-p (first x))
                              (unless (and (cdr x) (atom (cadr x)))
                                (error "`&once' without parameter in ~a" name))
                              (list (second x)))
                            (t
                              nil)))
                  params)))
      (with-preamble (preamble body)
        `(defmacro ,name ,(remove-once-keywords params)
           ,@preamble
           (once-only ,(find-once-params params)
             ,@body)))))
  )



(defmacro/once build-list (&once n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop :repeat ,n :collect (progn ,@body)))


(defmacro/once build-vector (&once n &body body)
  "Execute `body' `n' times, collecting the results into a vector."
  (with-gensyms (result index)
    `(let1 ,result (make-array ,n)
       (dotimes (,index ,n ,result)
         (setf (aref ,result ,index) (progn ,@body))))))


(defmacro with-thread ((&key bindings name) &body body)
  `(let1 bt:*default-special-bindings* ,bindings
     (bt:make-thread (lambda () ,@body)
       :name ,name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anaphora
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro anaphoric (op test &body body)
  "higher-order anaphoric operator creation macro."
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro aprog1 (first &body rest)
  "Binds IT to the first form so that it can be used in the rest of the
  forms. The whole thing returns IT."
  `(anaphoric prog1 ,first ,@rest))

(defmacro awhen (test &body body)
  "Like WHEN, except binds the result of the test to IT (via LET) for the scope
  of the body."
  `(anaphoric when ,test ,@body))

(defmacro atypecase (keyform &body cases)
  "Like TYPECASE, except binds the result of the keyform to IT (via LET) for
  the scope of the cases."
  `(anaphoric typecase ,keyform ,@cases))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Atomic Update (sbcl src copied over until i update to a more recent release)
;; TODO: unused?

(defmacro atomic-update (place update-fn &rest arguments &environment env) 
  "Updates PLACE atomically to the value returned by calling function
  designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.
  PLACE may be read and UPDATE-FN evaluated and called multiple times before the
  update succeeds: atomicity in this context means that value of place did not
  change between the time it was read, and the time it was replaced with the
  computed value. PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.
  EXAMPLE: Conses T to the head of FOO-LIST:
  ;;;   (defstruct foo list)
  ;;;   (defvar *foo* (make-foo))
  ;;;   (atomic-update (foo-list *foo*) #'cons t)"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))


(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))


(defmacro multiple-setf (value &rest places)
  "The multiple-setf macro was written by Mario Castel‚àö¬∞n. It is a
 beautiful form to support multiple places in zerof and nilf."
  (once-only (value)
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
    (once-only (key value)
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



(defmacro get-place (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((writer `(let (,@(mapcar #'list vars vals))
                     (lambda (,@store-vars)
                       ,writer-form)))
          (reader `(let (,@(mapcar #'list vars vals))
                     (lambda () ,reader-form))))
      `(values ,writer ,reader))))

#|
(defparameter *x* '(1 2 3))
(defparameter *write-x* (get-place (car *x*)))
(funcall *write-x* 4)
(print *x*)
(defun no-really (set-place)
  (let ((*x* 42))
   (funcall set-place 7)))
(no-really *write-x*)
|#

;;; place utils (from ??)
(defmacro place-fn (place-form)
  "This creates a closure which can write to and read from the 'place'
   designated by PLACE-FORM."
  (with-gensyms (value value-supplied-p)
    `(sb-int:named-lambda place (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
         ,place-form))))


(defmacro map-fn (place-form)
  "This creates a closure which can write to and read from 'maps'"
  (with-gensyms (key value value-supplied-p)
    `(sb-int:named-lambda place (,key &optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
         (setf (,place-form ,key) ,value)
         (,place-form ,key)))))


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
;; CLOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *nuke-existing-packages*   nil)
(defvar *nuke-existing-classes*    nil)
(defvar *store-class-superclasses* nil)

(defun fc (class-designator)
  (typecase class-designator
    (class    class-designator)  
    (keyword (fc (string class-designator)))
    (string  (fc (read-from-string class-designator)))
    (symbol  (find-class class-designator))
    (t       (find-class class-designator))))


(defun proto (thing)
  (flet ((get-proto (c)
           (let ((cc (find-class c)))
             (c2mop:finalize-inheritance cc)
             (c2mop:class-prototype cc))))
    (etypecase thing
      (class  (get-proto thing))
      (standard-object (get-proto (class-of thing)))
      (symbol (get-proto  thing)))))


(defun finalize (class-designator)
  (finalize-inheritance (fc class-designator))
  (fc class-designator))


(defun new (&rest args)
  (apply #'make-instance args))


(defun slot-value-safe (obj slot &optional (unbound-return :unbound))
  (handler-case (values (slot-value obj slot) t)
    (unbound-slot (c) (values unbound-return c))))


(defun required-arg (name)
  (error "~S is a required argument" name))

(defgeneric get-slot-details (slot-definition)
  (declare (optimize speed))
  (:documentation 
    "Return a list of slot details which can be used 
    as an argument to ensure-class")
  (:method ((slot-definition #+(or ecl abcl (and clisp (not mop))) t 
              #-(or ecl abcl (and clisp (not mop))) slot-definition))
    (list :name (slot-definition-name slot-definition)
      :allocation (slot-definition-allocation slot-definition)
      :initargs (slot-definition-initargs slot-definition)
      :readers (slot-definition-readers slot-definition)
      :type (slot-definition-type slot-definition)
      :writers (slot-definition-writers slot-definition))))

;; (mapcar #'get-slot-details (sb-mop:class-slots (fc 'cl-user::test-file)))
;;
;; ((:NAME ASDF::NAME :ALLOCATION :INSTANCE :INITARGS (:NAME) :READERS NIL
;;    :TYPE  STRING :WRITERS NIL)
;;   (:NAME ASDF:VERSION :ALLOCATION :INSTANCE :INITARGS (:VERSION) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::DESCRIPTION :ALLOCATION :INSTANCE :INITARGS (:DESCRIPTION)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::LONG-DESCRIPTION :ALLOCATION :INSTANCE :INITARGS
;;     (:LONG-DESCRIPTION) :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::LOAD-DEPENDENCIES :ALLOCATION :INSTANCE :INITARGS NIL :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::IN-ORDER-TO :ALLOCATION :INSTANCE :INITARGS (:IN-ORDER-TO)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::DO-FIRST :ALLOCATION :INSTANCE :INITARGS (:DO-FIRST) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::INLINE-METHODS :ALLOCATION :INSTANCE :INITARGS NIL :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::PARENT :ALLOCATION :INSTANCE :INITARGS (:PARENT) :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::RELATIVE-PATHNAME :ALLOCATION :INSTANCE :INITARGS (:PATHNAME)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::ABSOLUTE-PATHNAME :ALLOCATION :INSTANCE :INITARGS NIL :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::OPERATION-TIMES :ALLOCATION :INSTANCE :INITARGS NIL :READERS NIL
;;     :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::AROUND-COMPILE :ALLOCATION :INSTANCE :INITARGS (:AROUND-COMPILE)
;;     :READERS NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::%ENCODING :ALLOCATION :INSTANCE :INITARGS (:ENCODING) :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME ASDF::PROPERTIES :ALLOCATION :INSTANCE :INITARGS (:PROPERTIES) :READERS
;;     NIL :TYPE T :WRITERS NIL)
;;   (:NAME TYPE :ALLOCATION :INSTANCE :INITARGS (:TYPE) :READERS NIL :TYPE T
;;     :WRITERS NIL))


;; Structure definition storing
(defun get-layout (obj)
  (slot-value obj 'sb-pcl::wrapper))

(defun get-info (obj)
  (declare (type sb-kernel:layout obj))
  (slot-value obj 'sb-int:info))

(defun dd-name (dd)
  (slot-value dd 'sb-kernel::name))

(defvar *sbcl-struct-inherits*
  `(,(get-layout (find-class t))
    ,@(when-let (class (find-class 'sb-kernel:instance nil))
        (list (get-layout class)))
    ,(get-layout (find-class 'cl:structure-object))))

(defstruct (struct-def (:conc-name sdef-))
  (supers (required-arg :supers) :type list)
  (info (required-arg :info) :type sb-kernel:defstruct-description))

(defun info-or-die (obj)
  (let ((wrapper (get-layout obj)))
    (if wrapper
        (or (get-info wrapper) 
            (error "No defstruct-definition for ~A." obj))
        (error "No wrapper for ~A." obj))))

(defun save-able-supers (obj)
  (set-difference (coerce (slot-value (get-layout obj) 'sb-kernel::inherits)
                          'list)
                  *sbcl-struct-inherits*))

(defun get-supers (obj)
  (loop for x in (save-able-supers obj) 
     collect (let ((name (dd-name (get-info x))))
               (if *store-class-superclasses* 
                   (find-class name)
                   name))))


;; Restoring 
(defun sbcl-struct-defs (info)
  (append (sb-kernel::constructor-definitions info)
          (sb-kernel::class-method-definitions info)))

(defun create-make-foo (dd)
  (declare (optimize speed))
  (funcall (compile nil `(lambda () ,@(sbcl-struct-defs dd))))
  (find-class (dd-name dd)))


(defun sb-kernel-defstruct (dd supers source)
  (declare (ignorable source))
 ;; #+defstruct-has-source-location 
  (sb-kernel::%defstruct dd supers source))

  ;; #-defstruct-has-source-location
  ;; (sb-kernel::%defstruct dd supers))

(defun sbcl-define-structure (dd supers)
  (cond ((or *nuke-existing-classes*  
             (not (find-class (dd-name dd) nil)))
         ;; create-struct
         (sb-kernel-defstruct dd supers nil)
         ;; compiler stuff
         (sb-kernel::%compiler-defstruct dd supers) 
         ;; create make-?
         (create-make-foo dd))
        (t (find-class (dd-name dd)))))
         
(defun super-layout (super)
  (etypecase super
    (symbol (get-layout (find-class super)))
    (structure-class 
     (super-layout (dd-name (info-or-die super))))))

(defun super-layouts (supers)
  (loop for super in supers 
    collect (super-layout super)))


(defgeneric serializable-slots (object)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot-definitions to serialize. The default
    is to call serializable-slots-using-class with the object 
    and the objects class")
  (:method ((object standard-object))
   (serializable-slots-using-class object (class-of object)))
#+(or sbcl cmu openmcl allegro)
  (:method ((object structure-object))
   (serializable-slots-using-class object (class-of object)))
  (:method ((object condition))
   (serializable-slots-using-class object (class-of object))))

; unfortunately the metaclass of conditions in sbcl and cmu 
; are not standard-class

(defgeneric serializable-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class standard-class))
   (class-slots class))
#+(or sbcl cmu openmcl allegro) 
  (:method ((object t) (class structure-class))
   (class-slots class))
#+sbcl
  (:method ((object t) (class sb-pcl::condition-class))
   (class-slots class))
#+cmu
  (:method ((object t) (class pcl::condition-class))
   (class-slots class)))



(defvar *object->sexp-visited-objects* nil)

(defun object->sexp (obj &key suppress-types suppress-properties)
  "Converts arbitrary CLOS objects into s-expressions that can easily be used in tests."
 (if (and (subtypep (type-of obj) 'standard-object)
        (find obj *object->sexp-visited-objects*))
    :recursive-reference
    (let ((*object->sexp-visited-objects* (cons obj *object->sexp-visited-objects*)))
      (cond ((find (type-of obj) suppress-types) :suppressed)
        ((subtypep (type-of obj) 'standard-object)
          (multiple-value-bind (instance slots)
            (make-load-form-saving-slots obj)
            (let ((class (cadr (cadadr instance)))
                   (bound-slots (mapcar #'(lambda (s)
                                            (list (second (first (last (second s))))
                                              (object->sexp (second (third s))
                                                :suppress-types suppress-types
                                                :suppress-properties suppress-properties)))
                                  (remove 'slot-makunbound (rest slots) :key #'first))))
              (list* class
                (sort (remove-if #'(lambda (slot)
                                     (find (first slot) suppress-properties))
                        bound-slots)
                  #'string< :key (compose #'symbol-name #'first))))))
        ((null obj) nil)
        ((listp obj)
          (mapcar #'(lambda (obj) (object->sexp obj
                                    :suppress-types suppress-types
                                    :suppress-properties suppress-properties)) obj))
        (t obj)))))



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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printv - extended edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;; Licensed under Apache License 2.0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *printv-output* (make-broadcast-stream))

(defun printv-enable ()
  (setf *printv-output* *trace-output*))

(defun printv-disable ()
  (setf *printv-output* (make-broadcast-stream)))


(defun printv-minor-separator ()
  (format *printv-output* "~&;;; ~72,,,'-<-~>~%")
  (force-output *printv-output*))

(defun printv-major-separator ()
  (format *printv-output* "~&;;;~%")
  (princ
    (concatenate 'string ";;;"
      (make-string (- *print-right-margin* 5)
        :initial-element #\=)) *printv-output*)
  (force-output *printv-output*))

(defun printv-form-printer (form)
  (typecase form
    ;; String (label):
    (string (format *printv-output* "~&;;; ~a~%" form))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *printv-output* "~&;;;   ~w =>" form))
    (vector (format *printv-output* "~&;;   ~s~%" form)) 
    ;; Self-evaluating form:
    (t (format *printv-output* "~&;;;   ~s~%" form)))
  (force-output *printv-output*))

(defun printv-values-printer (values-list)
  (format *printv-output* "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%"  values-list)
  (force-output *printv-output*))


(defmacro vlet* (bind-forms &body body)
  `(let* ,(mapcar
            #'(lambda (form)
                (if (listp form)
                  `(,(car form)
                     (let ((v ,(cadr form)))
                       (printv (list ',(car form) v))
                       v))
                  form))
            bind-forms)
     ,@body))

(defmacro vlet (bind-forms &body body)
  `(let ,(mapcar #'(lambda (form)
		     (if (listp form)
                       `(,(car form) (let ((v ,(cadr form)))
                                       (printv (list ',(car form) v))
                                       v))
                       form))
           bind-forms)
     ,@body))


(defmacro vcond (&body clauses)
  `(cond ,@(mapcar #'(lambda (clause)
		      `((let ((x ,(car clause)))
			  (printv (list ',(car clause) '=> x))
			  x)
			,@(cdr clause)))
             clauses)))


(defun printv-expander (forms &optional values-trans-fn) ;; Allow for customized printv'ers:
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil) ,result-sym)
       ,@(loop for form in forms nconcing
           (cond
             ;; Markup form:
             ((eq form ':ff) (list '(printv-major-separator)))
             ((eq form ':hr) (list '(printv-minor-separator)))
             
             ;; Binding form:
             ((and (consp form) (or (eq (car form) 'let) (eq (car form) 'let*)))
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym
                      ,(if values-trans-fn
                         `(funcall ,values-trans-fn
                            (multiple-value-list
                              (case (car form)
                                (let (vlet `,(rest form)))
                                (let* (vlet* `,(rest form))))))
                         `(multiple-value-list
                            (case (car form)
                              (let (vlet `,(rest form)))
                              (let* (vlet* `,(rest form))))))))))
             
             ;; COND form:
             ((and (consp form) (eq (car form) 'cond)) 
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list (vcond `,(rest form))))
                                         `(multiple-value-list (vcond `,(rest form))))))))
             
             ;; Evaluated form:
             ((or (consp form) (and (symbolp form) (not (keywordp form))))
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list ,form))
                                         `(multiple-value-list ,form))))))
             
             ;; Self-evaluating form:
             (t `((printv-form-printer 
                    (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))


(defmacro printv (&rest forms)
  (printv-expander forms))

(defmacro v (&rest forms)
  (printv-expander forms))

(setf (symbol-function :printv) #'printv-expander)

(define-symbol-macro v/   (printv /))
(define-symbol-macro v//  (printv //))
(define-symbol-macro v/// (printv ///))

(define-symbol-macro v+   (printv +))
(define-symbol-macro v++  (printv ++))
(define-symbol-macro v+++ (printv +++))


(defun ?? (string-designator &optional package external-only)
  (apply #'apropos string-designator package external-only nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

