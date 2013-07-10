;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/ctrie/smoke :in cl-ctrie/ctrie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make-Instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-make-instance-and-typep/fundamental ()
  (is (typep (make-instance 'fundamental-ctrie) 'ctrie))
  (is (typep (make-instance 'fundamental-ctrie) 'fundamental-ctrie)))

(deftest check-make-instance-and-typep/transient ()
  (is (typep (make-instance 'transient-ctrie) 'ctrie))
  (is (typep (make-instance 'transient-ctrie) 'transient-ctrie)))

(deftest check-make-instance-and-typep/persistent ()
  (is (typep (make-instance 'persistent-ctrie) 'ctrie))
  (is (typep (make-instance 'persistent-ctrie) 'persistent-ctrie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple PUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-simple-put/fundamental ()
  (let ((c (make-instance 'fundamental-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 1 1) 1))
    (is (null (ctrie-put c 2 nil)))))

(deftest check-simple-put/transient ()
  (let ((c (make-instance 'transient-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 1 1) 1)) 
    (is (null (ctrie-put c 2 nil)))))

(deftest check-simple-put/persistent ()
  (let ((c (make-instance 'persistent-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 1 1) 1)) 
    (is (null (ctrie-put c 2 nil)))))

(deftest check-simple-put-and-replace/fundamental ()
  (let ((c (make-instance 'fundamental-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 0 1) 1))
    (is (null (ctrie-put c 0 nil)))))

(deftest check-simple-put-and-replace/transient ()
  (let ((c (make-instance 'transient-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 0 1) 1))
    (is (null (ctrie-put c 0 nil)))))

(deftest check-simple-put-and-replace/persistent ()
  (let ((c (make-instance 'persistent-ctrie)))
    (is (eq (ctrie-put c 0 :zero) :zero))
    (is (= (ctrie-put c 0 1) 1)) 
    (is (null (ctrie-put c 0 nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Depth after PUT and PUT with extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-put-depth-and-simple-extension/for-class :auto-call nil) (class-name)
  (let1 c (make-instance class-name)
    (is (= 1 (ctrie-max-depth c)))
    (is (= 1 (ctrie-min-depth c)))
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (is (= 1 (ctrie-max-depth c)))
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (ctrie-put c "whats" "up")                                      ;; split
    (is (= 2 (ctrie-max-depth c)))
    (is (= 1 (ctrie-min-depth c)))
    (ctrie-put c "cool" "kitten")                                   ;; replace at new depth
    (ctrie-put c "bibble" "babble")                                 ;; Second L2 Split
    (is (= 2 (ctrie-max-depth c)))))

(deftest check-put-depth-and-simple-extension/fundamental ()
  (check-put-depth-and-simple-extension/for-class 'fundamental-ctrie))

(deftest check-put-depth-and-simple-extension/transient ()
  (check-put-depth-and-simple-extension/for-class 'transient-ctrie))

(deftest check-put-depth-and-simple-extension/persistent ()
  (check-put-depth-and-simple-extension/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT then GET at Level 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-put-then-get-level-one/for-class :auto-call nil) (class-name)
  (let1 c (make-instance class-name)
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (is (eql (ctrie-get c "trickery") nil))
    (is (eql (nth-value 1 (ctrie-get c "trickery")) nil))
    (is (equal (ctrie-get c "key") "value"))
    (is (eql (nth-value 1 (ctrie-get c "key")) t))
    (is (equal (ctrie-get c "foo") "bar"))
    (is (equal (ctrie-get c "zip") "pop"))
    (is (equal (ctrie-get c "whiz") "bang"))
    (is (equal (ctrie-get c "yow") "wee"))
    (is (equal (ctrie-get c "snip") "snap"))
    (is (equal (ctrie-get c "cool") "cat"))                          ;; check replacement
    (is (equal (ctrie-get c "wow") "man"))
    (is (equal (ctrie-get c "hey") "dude"))
    (is (equal (ctrie-get c "bad") "ass"))
    (is (equal (ctrie-get c "yabba") "fred"))
    (is (equal (ctrie-get c "dabba") "barney"))
    (is (equal (ctrie-get c "doo") "wilma"))
    (is (equal (ctrie-get c "bam") "bam"))))

(deftest check-put-then-get-level-one/fundamental ()
  (check-put-then-get-level-one/for-class 'fundamental-ctrie))

(deftest check-put-then-get-level-one/transient ()
  (check-put-then-get-level-one/for-class 'transient-ctrie))

(deftest check-put-then-get-level-one/persistent ()
  (check-put-then-get-level-one/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT then GET multi-level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-put-then-get-multi-level/for-class :auto-call nil) (class-name)
  (let1 c (make-instance class-name)
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (ctrie-put c "whats" "up")                                      ;; split
    (ctrie-put c "cool" "kitten")                                   ;; replace at new depth
    (ctrie-put c "bibble" "babble")                                 ;; Second L2 Split
    (is (eql (ctrie-get c "trickery") nil))
    (is (eql (nth-value 1 (ctrie-get c "trickery")) nil))
    (is (equal (ctrie-get c "key") "value"))
    (is (eql (nth-value 1 (ctrie-get c "key")) t))
    (is (equal (ctrie-get c "foo") "bar"))
    (is (equal (ctrie-get c "zip") "pop"))
    (is (equal (ctrie-get c "whiz") "bang"))
    (is (equal (ctrie-get c "yow") "wee"))
    (is (equal (ctrie-get c "whats") "up"))                         ;; check L2 GET
    (is (equal (ctrie-get c "snip") "snap"))
    (is (equal (ctrie-get c "cool") "kitten"))                      ;; check l2 replacement
    (is (equal (ctrie-get c "wow") "man"))
    (is (equal (ctrie-get c "hey") "dude"))
    (is (equal (ctrie-get c "bad") "ass"))
    (is (equal (ctrie-get c "yabba") "fred"))
    (is (equal (ctrie-get c "dabba") "barney"))
    (is (equal (ctrie-get c "doo") "wilma"))
    (is (equal (ctrie-get c "bibble") "babble"))                    ;; check other L2 GET
    (is (equal (ctrie-get c "bam") "bam"))))

(deftest check-put-then-get-multi-level/fundamental ()
  (check-put-then-get-multi-level/for-class 'fundamental-ctrie))

(deftest check-put-then-get-multi-level/transient ()
  (check-put-then-get-multi-level/for-class 'transient-ctrie))

(deftest check-put-then-get-multi-level/persistent ()
  (check-put-then-get-multi-level/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extension, Retraction, and LNODE Chaining
;; this might be the most difficult smoke check.  The odds in real life
;; of needing to extend / contract SIX levels for the insertion
;; removal of one key/value are quite low.  It also, in the course
;; of events, tests a number of entombment/revival cycles, plus, of
;; course, the lnode chaining 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mod2 (n)
  (mod n 2))

(defun mod3 (n)
  (mod n 3))  

(defun mod4 (n)
  (mod n 4))

(defun self (n)
  (logand n #xFFFFFFFF))

(defun fixed (n)
  (declare (ignore n)) 1)


(deftest (check-extension-retraction-and-lnode-chaining/for-class :auto-call nil) (class-name)
  (let1 c4 (make-instance class-name :hash 'mod4)
        (ctrie-put c4 0 0)
    (ctrie-put c4 1 1)
    (ctrie-put c4 2 2)
    (ctrie-put c4 3 3)
    (is (eql 1 (ctrie-max-depth c4)))
    (ctrie-put c4 4 4)
    (is (eql 7 (ctrie-max-depth c4)))
    (loop for i from 0 to 4 do
      (is (eql (values i t) (ctrie-get c4 i))))
    (ctrie-drop c4 4)
    (is (eql 7 (ctrie-max-depth c4)))
    (is (eql (values 0 t) (ctrie-get c4 0)))
    (is (eql 1 (ctrie-max-depth c4)))
    (loop for i from 0 to 3 do
      (is (eql (values i t) (ctrie-get c4 i))))))

(deftest check-extension-retraction-and-lnode-chaining/fundamental ()
  (let ((c4 (make-fundamental-ctrie :hash 'mod4)))
    (ctrie-put c4 0 0)
    (ctrie-put c4 1 1)
    (ctrie-put c4 2 2)
    (ctrie-put c4 3 3)
    (is (eql 1 (ctrie-max-depth c4)))
    (ctrie-put c4 4 4)
    (is (eql 7 (ctrie-max-depth c4)))
    (loop for i from 0 to 4 do
      (is (eql (values i t) (ctrie-get c4 i))))
    (ctrie-drop c4 4)
    (is (eql 7 (ctrie-max-depth c4)))
    (is (eql (values 0 t) (ctrie-get c4 0)))
    (is (eql 1 (ctrie-max-depth c4)))
    (loop for i from 0 to 3 do
      (is (eql (values i t) (ctrie-get c4 i))))))

(deftest check-extension-retraction-and-lnode-chaining/transient ()
  (check-extension-retraction-and-lnode-chaining/for-class 'transient-ctrie))

(deftest check-extension-retraction-and-lnode-chaining/persistent ()
  (check-extension-retraction-and-lnode-chaining/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertion Drop and Retrieval 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-insertion-drop-and-get/for-class :auto-call nil) (class-name)
  (let1 c (make-instance class-name)
    (ctrie-put c "key" "value")
    (ctrie-put c "foo" "bar")
    (ctrie-put c "zip" "pop")
    (is (equalp (ctrie-get c "key")    (values "value" t)))          ;; lookup present
    (is (eql t (nth-value 1 (ctrie-get c "key"))))
    (is (null (ctrie-get c "floobadooba")))                          ;; lookup absent
    (is (null (nth-value 1 (ctrie-get c "floobadooba"))))
    (ctrie-put c "whiz" "bang")
    (ctrie-put c "yow" "wee")
    (ctrie-put c "snip" "snap")
    (ctrie-put c "cool" "daddyo")
    (ctrie-put c "cool" "cat")                                       ;; replace
    (is (equalp (multiple-value-list (ctrie-get c "yow"))    (list "wee"   t)))
    (is (equalp (multiple-value-list (ctrie-get c "whiz"))   (list "bang"  t)))
    (is (equalp (multiple-value-list (ctrie-get c "cool"))   (list "cat"   t)))
    (is (equalp (multiple-value-list (ctrie-get c "key"))    (list "value" t)))
    (is (equalp (multiple-value-list (ctrie-get c "snip"))   (list "snap"  t)))
    (is (equalp (multiple-value-list (ctrie-get c "zip"))    (list "pop"   t)))
    (is (equalp (multiple-value-list (ctrie-get c "foo"))    (list "bar"   t))) ;; All L1 RTrip
    (ctrie-put c "wow" "man")
    (ctrie-put c "hey" "dude")
    (ctrie-put c "bad" "ass")
    (ctrie-put c "yabba" "fred")
    (ctrie-put c "dabba" "barney")
    (ctrie-put c "doo" "wilma")
    (ctrie-put c "bam" "bam")
    (ctrie-put c "whats" "up")                                      ;; split
    (ctrie-put c "cool" "kitten")                                   ;; replace at new depth
    (is (equalp (multiple-value-list (ctrie-get c "whats"))
          (list "up" t)))
    (is (equalp (multiple-value-list (ctrie-get c "cool"))
          (list "kitten" t)))                                     ;; L2 Entries found
    (ctrie-put c "bibble" "babble")                                 ;; Second L2 Split
    (is (equalp (multiple-value-list (ctrie-get c "doo"))
          (list "wilma" t)))
    (is (equalp (multiple-value-list (ctrie-get c "bibble"))
          (list "babble" t)))                                     ;; Second L2 Entries found
    (is (null (ctrie-get c "headache")))
    (is (funcall (ctrie-test c) (ctrie-drop c "foo") "foo"))
    (is (funcall (ctrie-test c) (ctrie-drop c "zip") "zip"))
    (is (funcall (ctrie-test c) (ctrie-drop c "bam") "bam"))
    (is (funcall (ctrie-test c) (ctrie-drop c "snip") "snip"))
    (is (funcall (ctrie-test c) (ctrie-drop c "doo") "doo"))
    (is (funcall (ctrie-test c) (ctrie-drop c "bibble") "bibble"))
    (is (funcall (ctrie-test c) (ctrie-drop c "yabba") "yabba"))
    (is (funcall (ctrie-test c) (ctrie-drop c "dabba") "dabba"))
    (is (funcall (ctrie-test c) (ctrie-drop c "bad") "bad"))
    (is (funcall (ctrie-test c) (ctrie-drop c "key") "key"))
    (is (funcall (ctrie-test c) (ctrie-drop c "cool") "cool"))
    (is (funcall (ctrie-test c) (ctrie-drop c "whats") "whats"))
    (is (funcall (ctrie-test c) (ctrie-drop c "hey") "hey"))
    (is (funcall (ctrie-test c) (ctrie-drop c "whiz") "whiz"))
    (is (funcall (ctrie-test c) (ctrie-drop c "wow") "wow"))
    (is (funcall (ctrie-test c) (ctrie-drop c "yow") "yow"))
    (is (ctrie-empty-p c))))

(deftest check-insertion-drop-and-get/fundamental ()
  (check-insertion-drop-and-get/for-class 'fundamental-ctrie))

(deftest check-insertion-drop-and-get/transient ()
  (check-insertion-drop-and-get/for-class 'transient-ctrie))

(deftest check-insertion-drop-and-get/persistent ()
  (check-insertion-drop-and-get/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Medium Size Put Get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-medium-put-get/for-class :auto-call nil) (class-name)
  (let ((c (make-instance class-name))
         (list (copy-list (remove-if-not #'keywordp *features*))))
    (loop for key in list
      do (is (equalp (princ-to-string key) (ctrie-put c key (princ-to-string key)))))
    (let ((result (loop for key in list
                    for val = (ctrie-get c key) 
                    do (is (equalp (list (princ-to-string key) t)
                             (multiple-value-list (ctrie-get c key))))
                    collect val)))
      (is (eql  (length (remove-if #'null result)) (length list)))
      (is (null (remove-if #'null
                  (set-difference result (mapcar #'princ-to-string list)
                    :test #'equalp)))))))

(deftest check-medium-put-get/fundamental ()
  (check-medium-put-get/for-class 'fundamental-ctrie))

(deftest check-medium-put-get/transient ()
  (check-medium-put-get/for-class 'transient-ctrie))

(deftest check-medium-put-get/persistent ()
  (check-medium-put-get/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Medium Size Put Get Drop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-medium-put-get-drop/for-class :auto-call nil) (class-name)
  (let ((c (make-instance class-name))
         (list (copy-list (remove-if-not #'keywordp *features*))))
    (loop for key in list
      do (is (equalp (princ-to-string key) (ctrie-put c key (princ-to-string key)))))
    (let ((result (loop for key in list
                    for val = (ctrie-get c key) 
                    do (is (equalp (list (princ-to-string key) t)
                             (multiple-value-list (ctrie-get c key))))
                    collect val)))
      (is (eql  (length (remove-if #'null result)) (length list)))
      (is (null (remove-if #'null
                  (set-difference result (mapcar #'princ-to-string list)
                    :test #'equalp))))
      (loop for key in list
        do (is equal (princ-to-string symbol) (ctrie-drop c symbol)))
      (is (ctrie-empty-p c)))))

(deftest check-medium-put-get-drop/fundamental ()
  (check-medium-put-get-drop/for-class 'fundamental-ctrie))

(deftest check-medium-put-get-drop/transient ()
  (check-medium-put-get-drop/for-class 'transient-ctrie))

(deftest check-medium-put-get-drop/persistent ()
  (check-medium-put-get-drop/for-class 'persistent-ctrie))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bulk Put Get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-bulk-put-get/for-class :auto-call nil) (class-name)
  (let ((c (make-instance class-name))
         (num (* 1024 1024)))
    (loop for i from 1 to num do (ctrie-put c i i))
    (is (eql num (ctrie-size c)))
    (loop for i from 1 to num
      do (is (equalp (list i t) (multiple-value-list (ctrie-get c i)))))
    (is (equalp (list nil nil) (multiple-value-list (ctrie-get c 0))))))

(deftest check-bulk-put-get/fundamental ()
  (check-bulk-put-get/for-class 'fundamental-ctrie))

(deftest check-bulk-put-get/transient ()
  (check-bulk-put-get/for-class 'transient-ctrie))

(deftest check-bulk-put-get/persistent ()
  (check-bulk-put-get/for-class 'persistent-ctrie))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bulk Put Get Drop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-bulk-put-get-drop/for-class :auto-call nil) (class-name)
  (let ((c (make-instance class-name))
         (num (* 1024 1024)))
    (loop for i from 1 to num do (ctrie-put c i i))
    (is (eql num (ctrie-size c)))
    (loop for i from 1 to num
      do (is (equalp (list i t) (multiple-value-list (ctrie-get c i)))))
    (is (equalp (list nil nil) (multiple-value-list (ctrie-get c 0))))
    (loop for i from 1 to num
      do (is (eql i (ctrie-drop c i))))
    (is (ctrie-empty-p c))))

(deftest check-bulk-put-get-drop/fundamental ()
  (check-bulk-put-get-drop/for-class 'fundamental-ctrie))

(deftest check-bulk-put-get-drop/transient ()
  (check-bulk-put-get-drop/for-class 'transient-ctrie))

(deftest check-bulk-put-get-drop/persistent ()
  (check-bulk-put-get-drop/for-class 'persistent-ctrie))
    
