;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specifications for automated README (re)generation  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-symbol-macro $readme-pathname
  (asdf:system-relative-pathname (asdf:find-system :cl-ctrie) "readme.md"))


(setf (get 'readme :user-marker) "* * * * * *")
(setf (get 'readme :user-api)
  '(ctrie make-ctrie ctrie-p ctrie-test
     ctrie-hash ctrie-readonly-p  ctrie-put ctrie-get ctrie-drop ctrie-do
     ctrie-map ctrie-map-keys ctrie-map-values ctrie-map-into ctrie-keys
     ctrie-values ctrie-size ctrie-clear ctrie-pprint 
     ctrie-to-alist ctrie-to-hashtable ctrie-from-hashtable
     ctrie-from-alist ctrie-empty-p ctrie-save ctrie-load ctrie-export
     ctrie-import
     ctrie-max-depth
     ctrie-min-depth
     ctrie-snapshot
     ctrie-fork 
     ctrie-lambda
     ctrie-lambda-ctrie
     ctrie-lambda-spawn
     ctrie-lambda-class
     ctrie-lambda-object
     define-ctrie
     new-ctrie
     ctrie-enable-pooling
     ctrie-disable-pooling
     ctrie-pooling-enabled-p
     ctrie-error ctrie-structural-error
     ctrie-operational-error ctrie-operation-retries-exceeded
     ctrie-not-implemented ctrie-not-supported
     ctrie-invalid-dynamic-context ctrie-generational-mismatch
     ctrie-modification-failed
     ))


(setf (get 'readme :internal-marker) "* * * * * * *")
(setf (get 'readme :internal-ref)  
  '(*ctrie* *retries* *timeout* *hash-code*
     multi-catch catch-case
     ctrie ctrie-p ctrie-hash ctrie-test ctrie-readonly-p cthash ctequal
     with-ctrie flag flag-present-p flag-arc-position flag-vector
     ref ref-p ref-stamp ref-value ref-prev
     failed-ref failed-ref-p failed-ref-prev
     leaf-node branch-node main-node
     inode inode-p inode-gen inode-ref make-inode gcas-compare-and-set
     inode-read inode-mutate inode-commit
     snode snode-p snode-key snode-value
     lnode lnode-p lnode-elt lnode-next enlist lnode-removed lnode-inserted
     lnode-search lnode-length tnode tnode-p tnode-cell entomb resurrect
     cnode cnode-p make-cnode cnode-extended cnode-updated
     cnode-truncated map-cnode refresh cnode-contracted cnode-compressed
     clean clean-parent leaf-node-key leaf-node-value find-ctrie-root   
     rdcss-descriptor rdcss-descriptor-p rdcss-descriptor-ov
     rdcss-descriptor-ovmain rdcss-descriptor-nv
     rdcss-descriptor-committed root-node-access root-node-replace
     root-node-commit
     ctrie-snapshot ctrie-clear ctrie-put %insert ctrie-get %lookup
     ctrie-drop %remove
     ctrie-map ctrie-do map-node ctrie-map-keys ctrie-map-values
     ctrie-map-into ctrie-keys ctrie-values ctrie-size ctrie-empty-p
     ctrie-to-alist ctrie-to-hashtable ctrie-pprint ctrie-from-alist
     ctrie-from-hashtable
     ctrie-save ctrie-load ctrie-export ctrie-import
     ctrie-max-depth
     ctrie-min-depth
     ctrie-snapshot
     ctrie-fork 
     ctrie-lambda
     ctrie-lambda-ctrie
     ctrie-lambda-spawn
     ctrie-lambda-class
     ctrie-lambda-object
     new-ctrie
     define-ctrie
     ctrie-enable-pooling
     ctrie-disable-pooling
     ctrie-pooling-enabled-p
     *pool-high-water*
     cnode-pool
     pool-queue
     pool-worker
     allocate-cnode     
     fill-pool
     fill-all-pools     
     ctrie-error ctrie-structural-error ctrie-operational-error
     ctrie-modification-failed
     ctrie-operation-retries-exceeded ctrie-not-implemented
     ctrie-not-supported ctrie-invalid-dynamic-context
     ctrie-generational-mismatch readme readme-quietly apidoc princ-apidoc
     collect-docs define-diagram generate-alternative-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extended Descriptor Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cldoc:define-descriptor-handler DEFUN (form)
  "definition of a function that may possibly be named by keyword"
  (make-instance 'cldoc::defun-descriptor
    :type (format nil "~s" (first form))
    :name (if (keywordp (second form))
            (format nil "keyword::~a" (second form))
            (format nil "~s" (second form)))
    :lambda-list (third form)
    :doc (cldoc::extract-doc (cdddr form))))


(cldoc:define-descriptor-handler DEFINE-PANDORIC-FUNCTION (form)
  "Function with lexical enviornment defined by Pandoric Closure "
  (make-instance 'cldoc::defun-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list '(self)
    :doc (documentation (second form) 'function)))


(cldoc:define-descriptor-handler DEFUN/INLINE (form)
  "inline function definition"
  (make-instance 'cldoc::defun-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list (third form)
    :doc (cldoc::extract-doc (cdddr form))))


(cldoc:define-descriptor-handler DEFMACRO/ONCE (form)
  "macro with support for &ONCE 'once-only' argument extension to
  the standard destructuring macro lambda list keywords"
  (make-instance 'cldoc::defmacro-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list (third form)
    :doc (cldoc::extract-doc (cdddr form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptor Processing 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-docs  (&optional (scope :external) (sort #'string<))
  "Regenerate on-disk html documentation and collect the cached
  in-memory descriptors for further processing. If SCOPE is specified
  it must be either :EXTERNAL. corresponding to those symbols exported
  as the public API, or :HOME, which designates all symbols defined
  locally in package.  Output order may be customized by an optionally
  specified SORT function."
  (let1 dir (namestring (asdf:system-relative-pathname
                          (asdf:find-system :cl-ctrie)
                          "doc/api/"))
    (cldoc:extract-documentation 'cldoc:html dir (asdf:find-system :cl-ctrie)
      :table-of-contents-title "CL-CTRIE"))
  (remove-if #'null
    (sort
      (ecase scope
        (:external (loop for item in
                     (let (syms)
                       (do-external-symbols (s (find-package :cl-ctrie))
                         (push s syms))
                       (mapcar #'string-downcase (mapcar #'symbol-name syms)))
                     collect (cons item (mapcar #'cldoc::meta-descriptor-desc
                                          (gethash item cldoc::*name->meta-decriptors*)))))
        (:home (loop for item in
                 (let (syms)
                   (do-symbols (s #1=(find-package :cl-ctrie))
                     (when (eq (symbol-package s) #1#)
                       (push s syms)))
                   (mapcar #'string-downcase (mapcar #'symbol-name syms)))
                 collect (cons item (mapcar #'cldoc::meta-descriptor-desc
                                      (gethash item cldoc::*name->meta-decriptors*))))))
      sort :key #'car)
    :key #'cdr))


(defun all-descs (docs)
  "Collect all 'descriptors' from an alist of the form
  '((symbol . descriptor-list) ... ) such as one generated by
  {defun:collect-docs}"
  (loop for c in docs appending (cdr c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lightweight Text-markup Rendering
;;
;; rendering attractive representations in html, text, and
;; 'github-flavoured' markdown from the same docstring markup syntax
;; seems to become a pretty ugly ordeal pretty fast.  I hope this
;; strikes a reasonable balance. Not a lot of effort has been spent
;; eliminating duplicate code or innovating sophisticated format
;; control strings at present, since this stuff gets repeatedly
;; revised/replaced/deleted quite a bit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric render (desc &optional stream)
  (:documentation "Output to STREAM a compact rendering of a
   documentation-descriptor suitable for inclusion in lightweight
   text-markup."))


(defmethod render (desc &optional stream)
  (declare (ignore desc stream))
  (values))


(defmethod render ((desc cldoc::defstruct-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::inheritence cldoc::slots cldoc::doc) desc
    (let ((slot-names (mapcar (lambda (sd) (slot-value sd 'cldoc::name)) cldoc::slots)))
      (declare (ignorable slot-names))
      (if (< (length cldoc::doc) 1)
        (format stream "~20A `~A (~{~S~^ ~^-~^>~})`~%~%"
          "_[structure]_" (string-upcase cldoc::name) cldoc::inheritence)      
        (format stream "~20A `~A (~{~S~^ ~^-~^>~})`~%~%> ~A~%~%~%"
          "_[structure]_" (string-upcase cldoc::name) cldoc::inheritence cldoc::doc)))))


(defmethod render ((desc cldoc::define-condition-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::inheritence cldoc::slots cldoc::doc) desc
    (let ((slot-names (mapcar (lambda (sd) (slot-value sd 'cldoc::name)) cldoc::slots)))
      (declare (ignorable slot-names))
      (if (< (length cldoc::doc) 1)
        (format stream "~20A `~A (~{~S~^ ~^-~^>~})`~%~%"
          "_[condition]_" (string-upcase cldoc::name) cldoc::inheritence)      
        (format stream "~20A `~A (~{~S~^ ~^-~^>~})`~%~%> ~A~%~%~%"
          "_[condition]_" (string-upcase cldoc::name) cldoc::inheritence cldoc::doc)))))


(defmethod render ((desc cldoc::defun-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::lambda-list cldoc::doc) desc
    (if (< (length cldoc::doc) 1)
      (format stream "~20A `~A  ~:S`~%~%"
        "_[function]_" (string-upcase cldoc::name) cldoc::lambda-list)
      (format stream "~20A `~A  ~:S`~%~%> ~A~%~%~%"
        "_[function]_" (string-upcase cldoc::name) cldoc::lambda-list cldoc::doc))))


(defmethod render ((desc cldoc::defgeneric-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::lambda-list cldoc::qualifiers  cldoc::doc) desc
    (declare (ignorable cldoc::qualifiers))
    (if (< (length cldoc::doc) 1)
      (format stream "~20A `~A  ~S`~%~%"
        "_[generic-function]_" (string-upcase cldoc::name)  cldoc::lambda-list)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[generic-function]_" (string-upcase cldoc::name)  cldoc::lambda-list cldoc::doc))))


(defmethod render ((desc cldoc::defmethod-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::lambda-list cldoc::qualifiers cldoc::doc) desc
    (declare (ignorable cldoc::qualifiers))
    (if (< (length cldoc::doc) 1)
      (format stream "~20A `~A  ~S`~%~%"
        "_[method]_" (string-upcase cldoc::name) cldoc::lambda-list)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[method]_" (string-upcase cldoc::name) cldoc::lambda-list cldoc::doc))))


(defmethod render ((desc cldoc::defmacro-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::lambda-list cldoc::doc) desc
    (if (< (length cldoc::doc) 1)    
      (format stream "~20A `~A  ~S`~%~%"
        "_[macro]_" (string-upcase cldoc::name) cldoc::lambda-list)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[macro]_" (string-upcase cldoc::name) cldoc::lambda-list cldoc::doc))))


(defmethod render ((desc cldoc::deftype-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::args cldoc::doc) desc
    (if (< (length cldoc::doc) 1)    
      (format stream "~20A `~A  ~S`~%~%"
        "_[type]_" (string-upcase cldoc::name) cldoc::args)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[type]_" (string-upcase cldoc::name) cldoc::args cldoc::doc))))


(defmethod render ((desc cldoc::defvar-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::value cldoc::doc) desc
    (if (< (length cldoc::doc) 1)    
      (format stream "~20A `~A  ~S`~%~%"
        "_[special-variable]_" (string-upcase cldoc::name) cldoc::value)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[special-variable]_" (string-upcase cldoc::name) cldoc::value cldoc::doc))))


(defmethod render ((desc cldoc::defparameter-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::value cldoc::doc) desc
    (if (< (length cldoc::doc) 1)    
      (format stream "~20A `~A  ~S`~%~%"
        "_[special-variable]_" (string-upcase cldoc::name) cldoc::value)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[special-variable]_" (string-upcase cldoc::name) cldoc::value cldoc::doc))))


(defmethod render ((desc cldoc::defconstant-descriptor) &optional stream)
  (with-slots (cldoc::name cldoc::value cldoc::doc) desc
    (if (< (length cldoc::doc) 1)    
      (format stream "~20A `~A  ~S`~%~%"
        "_[constant-variable]_" (string-upcase cldoc::name) cldoc::value)
      (format stream "~20A `~A  ~S`~%~%> ~A~%~%~%"
        "_[constant-variable]_" (string-upcase cldoc::name) cldoc::value cldoc::doc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-Level Documentation Generation Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apidoc (&optional (scope :external))
  "Collect a list of strings representing the documentation for
  CL-CTRIE rendered in a compact format suitable for inclusion in a
  lightweight text-markup format document.  If SCOPE is specified it
  must be either :EXTERNAL. corresponding to those symbols exported as
  the public API, or :HOME, which designates all symbols defined
  locally in package."
  (remove-if #'null (mapcar #'render (all-descs (collect-docs scope)))))


(defun princ-apidoc (&optional (scope :external))
  "Print to `*STANDARD-OUTPUT*` the documentation for CL-CTRIE rendered
  in a compact format.  This is intended primarily as a convenience to
  the interactive user seeking quick reference at the REPL.  If SCOPE
  is specified it must be either :EXTERNAL. corresponding to those
  symbols exported as the public API, or :HOME, which designates all
  symbols defined locally in package."
  (mapc #'princ (apidoc scope))
  (values))


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


(defun readmedoc (docs symbol-list)  
  (apply #'concatenate 'string
    (append
      (list (format nil "~%~%"))
      (remove-if #'null
        (mapcar #'render
          (loop for desc in
            (loop for sym in symbol-list
              collecting (assoc (symbol-name sym) docs :test #'equalp))
            appending (cdr desc)))))))


(defun readme (&optional (stream *standard-output*))
  "Update documentation sections of the README file. When an output stream
  is specified, the results are also echoed to that stream. To inhibit
  output, invoke as `(readme (make-broadcast-stream))` or use `README-QUIETLY`"
  ;; todo: rewrite when requirements stabilize
  (princ (let1 docs (collect-docs :home)
           (flet ((gen (syms-prop marker-prop)  
                    (apply #'concatenate 'string 
                      (loop
                        with in-region   = nil
                        with region-line = -1
                        with syms        = (get 'readme syms-prop)
                        with marker      = (get 'readme marker-prop)
                        for line in (read-file-to-string-list $README-PATHNAME)
                        when (not in-region) collect (progn
                                                       (when (equalp line marker)
                                                         (setf in-region t))
                                                       (concatenate 'string line
                                                         (format nil "~%")))
                        when in-region do (incf region-line)
                        when (and in-region (zerop region-line)) collect (readmedoc docs syms)
                        when (and in-region (plusp region-line) (equalp line marker))
                        collect (progn
                                  (setf in-region nil)
                                  (concatenate 'string line (format nil "~%")))))))
             (write-string-to-file (gen :user-api     :user-marker)     $README-PATHNAME)
             (write-string-to-file (gen :internal-ref :internal-marker) $README-PATHNAME)))
    stream)
  (values))


(defun readme-quietly ()
  "Update documentation sections of the README file, inhibiting any other
  printed output."
  (readme (make-broadcast-stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figlet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(figlet "C L - C T R I E" :output t :font :small :justification :center)
(figlet "cl-ctrie" :output t :font :small :justification :right)

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vivisection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(defgeneric make-diagram (thing context &key &allow-other-keys)
  (:documentation "Define a specific digram generation procedure specialized
 on the class of THING and optionally for a specific context, represented
 by an abitrary symbol naming that context."))


#+()
(defmethod make-diagram (thing context &key)
  "By default, attempt to generate a NODE representing THING."
  (donuts::<> thing))

#+()
(defun diagram (thing &optional (context *context*))
  "Generate a DONUTS diagram for THING, optionally specialized for
  a specific CONTEXT. See {defgeneric cl-ctrie::make-diagram}."
  (donuts::& ()
    (make-diagram thing context)))


#+()
(defmacro define-diagram (type (&optional context) &body body)
  "Define a diagrammatic representation of TYPE, optionally specialized
  for a specific CONTEXT. See {defgeneric cl-ctrie::make-diagram}."
  (let ((specializer (if context `(list 'eql ,context) 't)))
    (with-gensyms (spc)
      `(let ((,spc ,specializer))
         (declare (ignorable ,spc))
         (defmethod make-diagram ((,type ,type) (context ,specializer) &key)
           ,@body)))))

;; +donuts
;; (define-diagram snode ()
;;   (donuts:[] (format nil "~A | ~A" (snode-key snode) (snode-value snode))))

;; #+donuts
;; (define-diagram symbol ()
;;   (cl:prin1-to-string symbol))

;; #+donuts
;; (define-diagram inode ()
;;   (donuts:<> (concatenate 'string "inode " (string (inode-gen inode)) "\\n"
;;                (local-time:format-rfc1123-timestring nil (ref-stamp (inode-ref inode))))))


;; #+donuts
;; (define-diagram cnode ()
;;   (donuts:[]   "cnode"))


;; #+donuts
;; (define-diagram tnode ()
;;   (donuts:[&] (donuts:<> "tnode")))


;; (defmethod make-diagram ((thing snode) context &key)
;;   (donuts:[] (format nil "~A | ~A" (snode-key thing) (snode-value thing))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alternate (unused) integration for use with Edi Weitz's documentation-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(defun collect-docs (&optional (scope :external))
  (sort (ecase scope
    (:external (loop for sym being the external-symbols of (find-package :cl-ctrie)
                 nconc (documentation-template::doc-entries sym)))
    (:home     (loop for sym being the symbols  of (find-package :cl-ctrie)
                 when (eq (symbol-package sym) (find-package :cl-ctrie))
                 nconc (documentation-template::doc-entries sym))))
    #'documentation-template::doc-entry<))

#+()
(defmethod render ((type (eql :class)) entry &optional (stream nil))
  (let* ((object (first entry))
          (class (string (funcall #'type-of (find-class object nil))))
          (cpl (mapcar #'class-name (sb-mop:class-precedence-list
                                      (find-class object nil)))))
    (format stream "~%[~A] ~A~%" class object)
    (print cpl)))


