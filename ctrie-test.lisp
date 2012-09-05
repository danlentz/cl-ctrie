;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet ((define-package (name docs &rest packages)
               `(defpackage ,name
                  (:documentation ,docs)
                  (:use :closer-common-lisp :closer-mop :lisp-unit #+():lparallel ,@packages)
                  (:export :run-ctrie-tests)
                  ,@(loop :for pkg :in packages :collect
                      `(:import-from ,pkg ,@(loop :for sym :being :the :symbols :in pkg
                                              :collect (make-symbol (string sym))))))))
    (define-package #:cl-ctrie-test
      "This is a testing sandbox for the system :cl-ctrie"
      :cl-ctrie))) 

(in-package :cl-ctrie-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUN-CTRIE-TESTS entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-ctrie-tests ()
  (let* ((banner (format nil "Starting test run on ~A" (sb-int:format-universal-time
                                                         nil (get-universal-time))))
          (uscores (make-string (length banner) :initial-element #\-)))
    (format t "~%~%~A~%~A~%~%" banner uscores)
    (run-all-tests :cl-ctrie-test)
    (format t "~%~%")
    (finish-output))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table Abstraction Scaffolding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-skiplist-tables (&body body)
  `(flet ((make-table (&key (test #'eql))
            (cl-skip-list:make-skip-list :key-equal test :value-equal test))
           (table-clear (tbl)
             (error "not supported"))
           (table-get (tbl k)
             (cl-skip-list:skip-list-lookup tbl k))
           (table-put (tbl k v)
             (cl-skip-list:skip-list-add tbl k v))
           (table-drop (tbl k)
             (cl-skip-list:skip-list-delete tbl k))
           (table-map (fn tbl)
             (cl-skip-list:map-skip-list fn tbl)))
     ,@body))


(defmacro with-hash-tables (&body body)
  `(flet ((make-table (&key (test #'eql))
            (make-hash-table :synchronized t :test test))
           (table-clear (tbl)
             (sb-ext:with-locked-hash-table (tbl)
               (clrhash tbl)))
           (table-get (tbl k)
             (sb-ext:with-locked-hash-table (tbl)
               (gethash k tbl)))
           (table-put (tbl k v)
             (sb-ext:with-locked-hash-table (tbl)             
               (setf (gethash k tbl) v)))
           (table-drop (tbl k)
             (sb-ext:with-locked-hash-table (tbl)
               (remhash k tbl)))
           (table-map (fn tbl)
             (sb-ext:with-locked-hash-table (tbl)
               (maphash fn tbl))))
     ,@body))


(defmacro with-ctrie-tables (&body body)
  `(flet ((make-table (&key (test #'eql))
            (make-ctrie :test test))
           (table-get (tbl k)
             (ctrie-get tbl k))
           (table-put (tbl k v)
             (setf (ctrie-get tbl k) v))
           (table-drop (tbl k)
             (ctrie-drop tbl k))
           (table-map (fn tbl)
             (ctrie-map tbl fn)))
     ,@body))


(define-test check-table-abstraction-fixtures
  (assert-equalp (values "1" t)
    (with-hash-tables
      (let1 ht (make-table) 
        (table-put ht 1 "1")
        (table-get ht 1))))
  (assert-equalp (values "1")
    (with-skiplist-tables
      (let1 ht (make-table) 
        (table-put ht 1 "1")
        (table-get ht 1))))
  (assert-equalp (values "1" t)
    (with-ctrie-tables
      (let1 ct (make-table) 
        (table-put ct 1 "1")
        (table-get ct 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance Measurement and Instrumentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-timing (thunk)
  "Executes THUNK and returns a timing-info object specifying how long
  execution took and how much memory was used. Implementation of
  collect-timing for SBCL. This code is a cut and paste adoption from
  from sbcl/src/code/time.lisp"
  (declare (type function thunk)) 
  (let (old-run-utime       new-run-utime       old-run-stime
         new-run-stime       old-real-time       new-real-time
         old-page-faults     new-page-faults     real-time-overhead
         run-utime-overhead  run-stime-overhead  page-faults-overhead
         old-bytes-consed    new-bytes-consed    cons-overhead)    

    (multiple-value-setq                ;; Calculate the overhead...
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    (multiple-value-setq                ;; Do it a second time
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))    
    (multiple-value-setq
      (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (sb-impl::time-get-sys-info))    

    (setq run-utime-overhead   (- new-run-utime old-run-utime))
    (setq run-stime-overhead   (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time        (get-internal-real-time))
    (setq old-real-time        (get-internal-real-time))
    (setq new-real-time        (get-internal-real-time))
    (setq real-time-overhead   (- new-real-time old-real-time))
    (setq cons-overhead        (- new-bytes-consed old-bytes-consed))

    (multiple-value-setq                ;; Now get the initial times.
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))    

    (setq old-real-time (get-internal-real-time))    
    (let ((start-gc-run-time sb-impl::*gc-run-time*) result timing)
      (progn
        (setq result (multiple-value-list (funcall thunk)))
        (multiple-value-setq
          (new-run-utime new-run-stime new-page-faults new-bytes-consed)
          (sb-impl::time-get-sys-info))        
        (setq new-real-time (- (get-internal-real-time) real-time-overhead))
        
        (let ((gc-run-time (max (- sb-impl::*gc-run-time* start-gc-run-time) 0)))          
          (setq timing
            (list
              :real-time     (max (- new-real-time old-real-time) 0.0)
              :user-time     (max (/ (- new-run-utime old-run-utime) 1000.0) 0.0)
              :system-time   (max (/ (- new-run-stime old-run-stime) 1000.0) 0.0)
              :gc-time       (float gc-run-time)
              :page-faults   (max (- new-page-faults old-page-faults) 0)
              :bytes-consed  (max (- new-bytes-consed old-bytes-consed) 0))))))))


(defmacro with-timing-collected-runs ((num) &body body)
  "automate agregation of metrics collected over NUM iterations of the
   execution of code in BODY via 'collect-timing'"
  (with-gensyms (iter)
    `(let ((,iter ,num))
       (loop repeat ,iter collect (collect-timing (lambda () ,@body))))))


(defun collate-timing (result-list)
  "Process and aggregate a collection of timing run statistics"
  (loop
    with iter = (length result-list)
    for run in result-list
    summing (getf run :real-time) into total-real-time
    summing (getf run :user-time) into total-user-time
    summing (getf run :system-time) into total-system-time
    summing (getf run :gc-time) into total-gc-time
    summing (getf run :page-faults) into total-page-faults
    summing (getf run :bytes-consed) into total-bytes-consed
    maximizing (getf run :real-time) into max-real-time
    maximizing (getf run :user-time) into max-user-time
    maximizing (getf run :system-time) into max-system-time
    maximizing (getf run :gc-time) into max-gc-time
    maximizing (getf run :page-faults) into max-page-faults
    maximizing (getf run :bytes-consed) into max-bytes-consed
    minimizing (getf run :real-time) into min-real-time
    minimizing (getf run :user-time) into min-user-time
    minimizing (getf run :system-time) into min-system-time
    minimizing (getf run :gc-time) into min-gc-time
    minimizing (getf run :page-faults) into min-page-faults
    minimizing (getf run :bytes-consed) into min-bytes-consed
    finally (return
              `(:real-time     (:max ,max-real-time
                                 :min ,min-real-time
                                 :avg ,(/ total-real-time iter))
                 :user-time    (:max ,max-user-time
                                 :min ,min-user-time
                                 :avg ,(/ total-user-time iter))
                 :system-time  (:max ,max-system-time
                                 :min ,min-system-time
                                 :avg ,(/ total-system-time iter))
                 :gc-time      (:max ,max-gc-time
                                 :min ,min-gc-time
                                 :avg ,(/ total-gc-time iter))
                 :page-faults  (:max ,max-page-faults
                                 :min ,min-page-faults
                                 :avg ,(/ total-page-faults iter))
                 :bytes-consed (:max ,max-bytes-consed
                                 :min ,min-bytes-consed
                                 :avg ,(/ total-bytes-consed iter))))))


(define-test check-timing-collection-fixtures
  (let1 stats (collate-timing (with-timing-collected-runs (5) t))
    (loop for major in '(:REAL-TIME :USER-TIME :SYSTEM-TIME :GC-TIME
                          :PAGE-FAULTS :BYTES-CONSED) 
      do (loop for minor in '(:MAX :MIN :AVG) 
           do (assert-true (numberp (getf (getf stats major) minor)))))))


(define-test check-byte-vector-hex-string-roundrip
  (loop repeat 10 do
    (let* ((bv0 (create-unique-id-byte-vector))
            (bv1 (hex-string-to-byte-vector (byte-vector-to-hex-string bv0))))
      (assert-equalp bv0 bv1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CATCH-CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test check-catch-case
  (flet ((test-catch-case (&optional (iterations 10))
           (let ((a 0) (b 0) (c 0) (d 0) (count 0)
                  (start (get-internal-real-time)))
             (flet ((report (ts case-name case-count)
                      (incf count)
                      (when *debug*
                        (sleep .01)
                        (format t "Test ~5,'0D [tick ~7,'0D] Tag ~S now ~D~%" count
                          (- ts start) case-name case-count))))
               (loop repeat iterations do
                 (catch-case (let ((key (alexandria:whichever :a :b :c :d)))
                               (when key (throw key (get-internal-real-time))))
                   (:a (report it :A (incf a)))
                   (:b (report it :B (incf b)))
                   (:c (report it :C (incf c)))
                   (:d (report it :D (incf d)))
                   (t  "T")))
               (assert-eql iterations (+ a b c d)))
               (when *debug* (list :a a :b b :c c :d d)))))
    (loop repeat 128 do (test-catch-case 256))))


;; (loop repeat 5 collect (test-catch-case 20))
;;  => ((:A 4 :B 6 :C 6 :D 4)
;;      (:A 5 :B 4 :C 5 :D 6)
;;      (:A 5 :B 3 :C 7 :D 5)
;;      (:A 7 :B 2 :C 6 :D 5)
;;      (:A 4 :B 7 :C 4 :D 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARCS, FLAGS, HASHING and BITMAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test check-flag-computation
  (with-ctrie (make-ctrie)
    (assert-eql 2048     (flag nil 0))
    (assert-eql 1        (flag nil 5))
    (assert-eql 1        (flag nil 10))
    (assert-eql 65536    (flag nil 15))
    (assert-eql 1        (flag nil 20))
    (assert-eql 256      (flag nil 25))
    (assert-eql 4        (flag t 0))
    (assert-eql 1024     (flag 0 0))
    (assert-eql 67108864 (flag 1 0))
    (assert-eql 262144   (flag 1 5))
    (assert-eql 2048     (flag 1 10))
    (assert-eql 8388608  (flag 1 15))))


(define-test check-flag-arc-position
  (loop for power from 0 to 32 do
    (assert-eql 0 (flag-arc-position (expt 2 power) 0))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 1)) #x10000000) 1))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 2))
    (assert-eql 2 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 3))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 3)) #x10000000) 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-atomic-update
  ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
  ;; atomic update with (INCF (CDR X)), the result becomes
  ;; unpredictable.
  (let ((x (cons :count 0)))
    (mapc #'sb-thread:join-thread
      (loop repeat 512
        collect (sb-thread:make-thread
                  (lambda ()
                    (loop repeat 512
                      do (atomic-update (cdr x) #'1+)
                      (sleep 0.00001))))))
    (assert-eql (* 512 512) (cdr x))))

#+()
(define-test check-atomic-inode-mutation
  (with-ctrie (make-ctrie)
    (dotimes (rep 8)
      (let ((x (make-inode 0)))
        (mapc #'sb-thread:join-thread
          (loop repeat 256
            collect (sb-thread:make-thread
                      (lambda ()
                        (loop repeat 1024
                          do (loop until (inode-mutate x (inode-read x) (1+ (inode-read x))))
                          (sleep 0.00001))))))
        (multiple-value-bind (val stamp) (inode-read x)
          (assert-eql (* 256 1024) val)
          (assert-eql val stamp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-lnode-search
  (let1 ln (enlist (snode 1 :one) (snode 2 :two) (snode 3 :three))
    (assert-eq (values :one t)   (lnode-search ln 1 'eql))
    (assert-eq (values :two t)   (lnode-search ln 2 'eql))
    (assert-eq (values :three t) (lnode-search ln 3 'eql))
    (assert-eq (values nil nil)  (lnode-search ln 4 'eql))))


(define-test check-lnode-length/enlist
  (dotimes (rep 8)
    (let1 len (random 32)
      (assert-eql len (lnode-length (apply #'enlist (alexandria:iota len)))))))


(define-test check-lnode-inserted/removed
  (dotimes (rep 8)
    (let* ((len (random 32))
            (ints (alexandria:iota len))
            (ln  (loop for int in ints
                   for ln = (lnode-inserted nil int int 'eql)
                   then (lnode-inserted ln int int 'eql)
                   finally (return ln))))
      (assert-eql len (lnode-length ln))
      (loop for i in ints do (assert-eql (values i t) (lnode-search ln i 'eql)))
      (loop for i in ints for remain from (1- len) downto 0
        for rest = (lnode-removed ln i 'eql) then (lnode-removed rest i 'eql)
        do (assert-eql remain (lnode-length rest))))))


(defmacro with-worlds-worst-hash-functions (&body body)
  `(flet ((mod2 (n)   (mod n 2))
           (mod3 (n)  (mod n 3))
           (mod4 (n)  (mod n 4))
           (self (n)  (logand n #xFFFFFFFF))
           (fixed (n) (declare (ignore n)) 1))
     ,@body))


(define-test check-depth-and-simple-extension ()
  (let1 c (make-ctrie)
    (assert-eql 1 (ctrie-max-depth c))
    (assert-eql 1 (ctrie-min-depth c))
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (assert-eql 1 (ctrie-max-depth c))
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (ctrie-put c "whats" "up")                                      ;; split
    (assert-eql 2 (ctrie-max-depth c))
    (assert-eql 1 (ctrie-min-depth c))
    (ctrie-put c "cool" "kitten")                                   ;; replace at new depth
    (assert-equalp (ctrie-get c "whats")  (values "up" t))
    (assert-equalp (ctrie-get c "cool")   (values "kitten" t))      ;; L2 Entries found
    (ctrie-put c "bibble" "babble")                                 ;; Second L2 Split
    (assert-eql 2 (ctrie-max-depth c))))
    

    
(define-test check-extension/retraction/lnode-chaining ()
  ;; this might be one of hardest tests.  The odds in real life
  ;; of needing to extend / contract SIX levels for the insertion
  ;; removal of one key/value are quite low.  It also, in the course
  ;; of events, tests a number of entombment/revival cycles, plus, of
  ;; course, the lnode chaining 
  (with-worlds-worst-hash-functions
    (let ((c4 (make-ctrie :hash #'mod4)))
      (ctrie-put c4 0 0)
      (ctrie-put c4 1 1)
      (ctrie-put c4 2 2)
      (ctrie-put c4 3 3)
      (assert-eql 1 (ctrie-max-depth c4))
      (ctrie-put c4 4 4)
      (assert-eql 7 (ctrie-max-depth c4))
      (loop for i from 0 to 4 do
        (assert-eql (values i t) (ctrie-get c4 i)))
      (ctrie-drop c4 4)
      (assert-eql 7 (ctrie-max-depth c4))
      (assert-eql (values 0 t) (ctrie-get c4 0))
      (assert-eql 1 (ctrie-max-depth c4))
      (loop for i from 0 to 3 do
        (assert-eql (values i t) (ctrie-get c4 i))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Insertion and Retrieval Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-ctrie-smoke-test
  (let1 c (make-ctrie)
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (assert-equalp (ctrie-get c "key")    (values "value" t))        ;; lookup present
    (assert-false  (ctrie-get c "floobadooba"))                      ;; lookup absent
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (assert-equalp (ctrie-get c "yow")    (values "wee"   t))
    (assert-equalp (ctrie-get c "whiz")   (values "bang"  t))
    (assert-equalp (ctrie-get c "cool")   (values "cat"   t))
    (assert-equalp (ctrie-get c "key")    (values "value" t))
    (assert-equalp (ctrie-get c "snip")   (values "snap"  t))
    (assert-equalp (ctrie-get c "zip")    (values "pop"   t))
    (assert-equalp (ctrie-get c "foo")    (values "bar"   t))        ;; All L1 entries Roundtripped
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (ctrie-put c "whats" "up")                                      ;; split
    (ctrie-put c "cool" "kitten")                                   ;; replace at new depth
    (assert-equalp (ctrie-get c "whats")  (values "up" t))
    (assert-equalp (ctrie-get c "cool")   (values "kitten" t))      ;; L2 Entries found
    (ctrie-put c "bibble" "babble")                                 ;; Second L2 Split
    (assert-equalp (ctrie-get c "doo")    (values "wilma" t))
    (assert-equalp (ctrie-get c "bibble") (values "babble" t))      ;; Second L2 Entries found
    (assert-false (ctrie-get c "headache"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "foo") "bar"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "zip") "pop"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "bam") "bam"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "snip") "snap"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "doo") "wilma"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "bibble") "babble"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "yabba") "fred"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "dabba") "barney"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "bad") "ass"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "key") "value"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "cool") "kitten"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "whats") "up"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "hey") "dude"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "whiz") "bang"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "wow") "man"))
    (assert-true (funcall (ctrie-test c) (ctrie-drop c "yow") "wee"))
    (assert-true (ctrie-empty-p c))
    (values :pass c)))


(define-test check-simple-insert/lookup ()
  (let ((c (make-ctrie))
         (list (copy-list (remove-if-not #'keywordp *features*))))
    (loop for key in list
      do (assert-equalp (princ-to-string key) (ctrie-put c key (princ-to-string key))))
    (let ((result (loop for key in list
                    for val = (ctrie-get c key) 
                    do (assert-equalp (values (princ-to-string key) t) (ctrie-get c key))
                    collect val)))
      (assert-eql  (length (remove-if #'null result)) (length list))
      (assert-true (null (remove-if #'null
                      (set-difference result (mapcar #'princ-to-string list)
                        :test #'equalp)))))))
   

(define-test check-simple-insert/lookup/drop ()
  (let ((c (make-ctrie))
         (list (copy-list (remove-if-not #'keywordp *features*))))
    (loop for key in list
      do (assert-equalp (princ-to-string key) (ctrie-put c key (princ-to-string key))))
    (let ((result (loop for key in list
                    for val = (ctrie-get c key) 
                    do (assert-equalp (values (princ-to-string key) t) (ctrie-get c key))
                    collect val)))
      (assert-eql  (length (remove-if #'null result)) (length list))
      (assert-true (null (remove-if #'null
                           (set-difference result (mapcar #'princ-to-string list)
                             :test #'equalp)))))
    (dolist (symbol list)
      (assert-equalp (values (princ-to-string symbol) t) (ctrie-drop c symbol)))
    (assert-true (ctrie-empty-p c))
    c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bulk Operation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-bulk-insert/lookup ()
  (let ((c (make-ctrie))
         (num (* 1024 1024)))
    (loop for i from 1 to num do (ctrie-put c i i))
    (assert-eql num (ctrie-size c))
    (loop for i from 1 to num
      do (assert-eql (values i t) (ctrie-get c i)))
    (assert-eql (values nil nil) (ctrie-get c 0))))


(define-test check-bulk-insert/drop ()
  (let ((c (make-ctrie))
         (num (* 1024 1024)))
    (loop for i from 1 to num do (ctrie-put c i i))
    (assert-eql num (ctrie-size c))
    (assert-eql (values nil nil) (ctrie-get c 0))
    (loop for i from 1 to num
      do (assert-eql (values i t) (ctrie-drop c i)))
    (assert-true (ctrie-empty-p c))
    c))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parallel Operation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-ctrie* (make-ctrie))
(defvar *input*      (coerce (iota (expt 2 18)) 'vector))
(defvar *kernels*    (loop for i from 0 to 3
                          collect (lparallel:make-kernel (expt 2 i))))


(define-test check-parallel-insert-parallel-lookup ()
  (loop for kernel in *kernels* do
    (let1 lparallel:*kernel* kernel
      (ctrie-clear *test-ctrie*)
      (assert-true (ctrie-empty-p *test-ctrie*))
      (lparallel:pmap nil #'(lambda (i) (ctrie-put *test-ctrie* i i)) *input*)
      (assert-eql (length *input*) (ctrie-size *test-ctrie*))
      (lparallel:pmap nil #'(lambda (i) (assert-eql i (ctrie-get *test-ctrie* i))) *input*)
      (loop for i across *input*
        do (assert-eql (values i t) (ctrie-get *test-ctrie* i)))
      )))


(define-test check-parallel-insert-parallel-drop ()
  (loop for kernel in *kernels* do
    (let1 lparallel:*kernel* kernel
      (ctrie-clear *test-ctrie*)
      (assert-true (ctrie-empty-p *test-ctrie*))
      (lparallel:pmap nil #'(lambda (i) (ctrie-put *test-ctrie* i i)) *input*)
      (assert-eql (length *input*) (ctrie-size *test-ctrie*))
      (loop for i across *input*
        do (assert-eql (values i t) (ctrie-get *test-ctrie* i)))
      (lparallel:pmap nil #'(lambda (i) (ctrie-drop *test-ctrie* i)) *input*)
      (assert-true (ctrie-empty-p *test-ctrie*))
      )))




;; (defun put-parallel (&optional (round 2) (total (expt 2 10)))
;;   (assert (= 0 (mod total (expt 2 round))))
;;   (ctrie-clear *test-ctrie*)
;;   (sb-thread:barrier (:memory)
;;     (let* ((num-threads (expt 2 round))
;;             (per-thread (/ total num-threads)))
;;       (mapc #'sb-thread:join-thread
;;         (loop repeat num-threads
;;           for start from 0 by per-thread
;;           collect (sb-thread:make-thread
;;                     (lambda ()
;;                       (loop for i from start
;;                         repeat per-thread
;;                         do (ctrie-put *test-ctrie* i i))))))
;;       (assert-eql total (ctrie-size *test-ctrie*))
;;       (loop for j from 0 to (1- total)
;;         do (assert-eql (values j t) (ctrie-get *test-ctrie* j)))
;;       (print (ctrie-size *test-ctrie*))
;;       #+() (ctrie-pprint *test-ctrie*))))
  

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snapshot Related Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test check-atomic-clear ()
  (let ((c (make-ctrie)))
    (assert-true (ctrie-empty-p c))
    (assert-eql 0 (ctrie-size c))
    (ctrie-put c "foo" "bar")
    (assert-false (ctrie-empty-p c))
    (assert-eql 1 (ctrie-size c))
    (ctrie-clear c)
    (assert-true (ctrie-empty-p c))
    (assert-eql 0 (ctrie-size c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor and Supporting Facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test check-fbind ()
  (fbind (foo (lambda (x) (list 'foo x)))
    (assert-equalp (foo 1)  '(foo 1))
    (assert-equalp (foo :x) '(foo :x))
    (assert-equalp (foo (foo t)) '(foo (foo t))))) 


(define-test check-alet-fsm ()
  (flet ((make-test-fsm ()
           (alet ((acc 0))
             (alet-fsm
               (going-up (n)
                 (if (eq n 'invert)
                   (state going-down)
                   (incf acc n)))
               (going-down (n)
                 (if (eq n 'invert)
                   (state going-up)
                   (decf acc n)))))))
    (fbind (fsm (make-test-fsm))
      (assert-eql  0 (fsm 0))
      (assert-eql  5 (fsm 5))
      (assert-eql  5 (fsm 0))
      (assert-eql  6 (fsm 1))
      (fsm 'invert)
      (assert-eql  0 (fsm 6))
      (assert-eql -5 (fsm 5))
      (fsm 'invert)
      (assert-eql  0 (fsm 5)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRIE-LAMBDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(define-test check-ctrie-lambda-spawn ()
  (flet ((doit (&optional read-only)
           (let* ((c0 (make-ctrie))
                   (c1 (ctrie-lambda c0))
                   (c2 (ctrie-lambda-spawn c1 :read-only read-only)))
             (mapcar (compose
                       #'inode-gen
                       #'root-node-access
                       #'ctrie-lambda-ctrie)
               (list c0 c1 c2)))))
    (doit)))
