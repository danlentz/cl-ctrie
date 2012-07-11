;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :ctrie)

(defun run-ctrie-tests ()
  (let* ((banner (format nil "Starting test run on ~A" (sb-int:format-universal-time
                                                         nil (get-universal-time))))
          (uscores (make-string (length banner) :initial-element #\-)))
    (format t "~%~%~A~%~A~%~%" banner uscores)
    (run-all-tests :ctrie)
    (format t "~%~%")
    (finish-output))
  (values))

(define-test check-atomic-update
  ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
  ;; atomic update with (INCF (CDR X)) above, the result becomes
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

(define-test check-flag-computation
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
  (assert-eql 8388608  (flag 1 15)))

(define-test check-flag-arc-position
  (loop for power from 0 to 32 do
    (assert-eql 0 (flag-arc-position (expt 2 power) 0))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 1)) #x10000000) 1))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 2))
    (assert-eql 2 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 3))
    (assert-eql 1 (flag-arc-position (mod (expt 2 (+ power 3)) #x10000000) 4))))

(define-test check-atomic-inode-mutation
  (dotimes (rep 8)
    (let ((x (make-inode 0)))
      (mapc #'sb-thread:join-thread
        (loop repeat 256
          collect (sb-thread:make-thread
                    (lambda ()
                      (loop repeat 1024
                        do (loop until (inode-mutate x (deref x) (1+ (deref x))))
                        (sleep 0.00001))))))
      (multiple-value-bind (val stamp) (deref x)
        (assert-eql (* 256 1024) val)
        (assert-eql val stamp)))))

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
  

(defun test-cas-ctrie-root (&optional (iterations 10))
  (assert (notany #'null (loop for ct in
                           (list (make-ctrie) (make-ctrie :root (make-inode 0))
                             (make-ctrie
                               :root (make-inode
                                       :ref (%make-cnode
                                              :bitmap (random 32)
                                              :arcs (iota (random 32))))))
                           do (cas-ctrie-root ct (make-inode :ref 42))
                           collect (inode-ref (ctrie-root ct)))))
  (loop repeat iterations
    with value-cases = (list
                         nil
                         (make-inode :ref (gensym))
                         (make-inode :ref (make-cnode))
                         (make-inode :ref (%make-cnode :arcs (make-gensym-list (random 32)))))
    with ctrie-cases = (list
                        (make-ctrie)
                        (make-ctrie :root (make-inode 0))
                        (make-ctrie :root (make-inode :ref (random 255)))
                        (make-ctrie :root (make-inode :ref (%make-cnode :arcs (iota (random 32))))))
    for ct = (random-elt ctrie-cases) then (random-elt ctrie-cases)
    for vt = (random-elt value-cases) then (random-elt value-cases)
    collect ct into test-cases
    do (cas-ctrie-root ct vt)
    collect (ctrie-root ct) into test-results
    finally (assert (length= test-cases test-results))  
    (return (values test-cases test-results))))


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

;;;;;
;;
;; (time (test-ctrie-simple-smoke-check))
;;
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000239 seconds of total run time (0.000238 user, 0.000001 system)
;;   100.00% CPU
;;   660,296 processor cycles
;;   32,768 bytes consed
;;
;; :PASS
;; #S(CTRIE
;;    :TEST EQUAL
;;    :ROOT #<INODE :STAMP 33 :VALUE #S(CNODE
;;                                      :BITMAP 0
;;                                      :FLAGS #*00000000000000000000000000000000
;;                                      :ARCS #())>)
;;;;;
;;
;; ctrie as it should have appeared before drops in above test:
;;
;; #S(CTRIE
;;    :TEST EQUAL
;;    :ROOT #<INODE :STAMP 17 :VALUE #S(CNODE
;;                                      :BITMAP 571150067
;;                                      :FLAGS #*11001111011100001101000001000100
;;                                      :ARCS #(#S(SNODE :KEY "yow"  :VALUE "wee")
;;                                              #S(SNODE :KEY "wow"  :VALUE "man")
;;                                              #S(SNODE :KEY "whiz" :VALUE "bang")
;;                                              #S(SNODE :KEY "hey"  :VALUE "dude")
;;                                              #<INODE :STAMP 1
;;                                                      :VALUE #S(CNODE :BITMAP 9
;;                                                                      :FLAGS #*10010000000000000000000000000000
;;                                                                      :ARCS #(#S(SNODE :KEY "whats" :VALUE "up")
;;                                                                              #S(SNODE :KEY "cool" :VALUE "kitten")))>
;;                                              #S(SNODE :KEY "key"   :VALUE "value")
;;                                              #S(SNODE :KEY "bad"   :VALUE "ass")
;;                                              #S(SNODE :KEY "dabba" :VALUE "barney")
;;                                              #S(SNODE :KEY "yabba" :VALUE "fred")
;;                                              #<INODE :STAMP 0
;;                                                      :VALUE #S(CNODE :BITMAP 1610612736
;;                                                                      :FLAGS #*00000000000000000000000000000110
;;                                                                      :ARCS #(#S(SNODE :KEY "bibble" :VALUE "babble")
;;                                                                              #S(SNODE :KEY "doo" :VALUE "wilma")))>
;;                                              #S(SNODE :KEY "snip" :VALUE "snap")
;;                                              #S(SNODE :KEY "bam"  :VALUE "bam")
;;                                              #S(SNODE :KEY "zip"  :VALUE "pop")
;;                                              #S(SNODE :KEY "foo"  :VALUE "bar")))>)




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
