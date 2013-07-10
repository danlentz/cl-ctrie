;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/ctrie/primatives :in cl-ctrie/ctrie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique Identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-byte-vector-hex-string/roundrip ()
  (loop repeat 10 do
    (let* ((bv0 (create-unique-id-byte-vector))
            (bv1 (hex-string-to-byte-vector (byte-vector-to-hex-string bv0))))
      (is (equalp bv0 bv1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CATCH-CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-catch-case/repeatedly ()
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
               (is (= iterations (+ a b c d)))
             ;;(when *debug* (list :a a :b b :c c :d d))
             ))))
    (loop repeat 128 do (test-catch-case 256))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARCS, FLAGS, HASHING and BITMAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-flag-computation/by-example ()
  (with-ctrie (make-fundamental-ctrie)
    (is (= 2048     (flag nil 0)))
    (is (= 1        (flag nil 5)))
    (is (= 1        (flag nil 10)))
    (is (= 65536    (flag nil 15)))
    (is (= 1        (flag nil 20)))
    (is (= 256      (flag nil 25)))
    (is (= 4        (flag t 0)))
    (is (= 1024     (flag 0 0)))
    (is (= 67108864 (flag 1 0)))
    (is (= 262144   (flag 1 5)))
    (is (= 2048     (flag 1 10)))
    (is (= 8388608  (flag 1 15)))))

(deftest check-flag-arc-position/by-example ()
  (loop for power from 0 to 32 do
    (is (= 0 (flag-arc-position (expt 2 power) 0)))
    (is (= 1 (flag-arc-position (mod (expt 2 (+ power 1)) #x10000000) 1)))
    (is (= 1 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 2)))
    (is (= 2 (flag-arc-position (mod (expt 2 (+ power 2)) #x10000000) 3)))
    (is (= 1 (flag-arc-position (mod (expt 2 (+ power 3)) #x10000000) 4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATOMICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest check-atomic-update/1-million ()
  ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
  ;; atomic update with (INCF (CDR X)), the result becomes
  ;; unpredictable.
  (let ((x (cons :count 0)))
    (mapc #'sb-thread:join-thread
      (loop repeat 512
        collect (sb-thread:make-thread
                  (lambda ()
                    (loop repeat 512
                      do (atom::atomic-update (cdr x) #'1+)
                      (sleep 0.00001))))))
    (is (= (* 512 512) (cdr x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; SNODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-snode-basics/in-layer :auto-call nil) (layer-name)
  (funcall-with-layer-context (adjoin-layer layer-name contextl::*root-context*)
    (lambda ()
      (let ((one (snode 1 :one))
             (two (snode 2 nil))
             (three (snode "three" 3)))
        (is (= (snode-key one) 1))
        (is (eq (snode-value one) :one))
        (is (= (snode-key two) 2))
        (is (null (snode-value two)))
        (is (equal (snode-key three) "three"))
        (is (= (snode-value three) 3))))))

(deftest check-snode-basics/fundamental ()
  (check-snode-basics/in-layer 'fundamental))

(deftest check-snode-basics/transient ()
  (check-snode-basics/in-layer 'transient))

(deftest check-snode-basics/persistent ()
  (check-snode-basics/in-layer 'persistent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LNODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest (check-lnode-search/in-layer :auto-call nil) (layer-name)
  (funcall-with-layer-context (adjoin-layer layer-name contextl::*root-context*)
    (lambda ()
      (let1 ln (enlist (snode 1 :one) (snode 2 :two) (snode 3 :three))
        (is (eq (values :one t)   (lnode-search ln 1 'eql)))
        (is (eq (values :two t)   (lnode-search ln 2 'eql)))
        (is (eq (values :three t) (lnode-search ln 3 'eql)))
        (is (eq (values nil nil)  (lnode-search ln 4 'eql)))))))

(deftest check-lnode-search/fundamental ()
  (check-lnode-search/in-layer 'fundamental))

(deftest check-lnode-search/transient ()
  (check-lnode-search/in-layer 'transient))

(deftest check-lnode-search/persistent ()
  (check-lnode-search/in-layer 'persistent))

(deftest (check-lnode-length/in-layer :auto-call nil) (layer-name)
 (funcall-with-layer-context (adjoin-layer layer-name contextl::*root-context*)
   (lambda ()
     (dotimes (rep 8)
       (let1 len (random 32)
         (is (= len (lnode-length (apply #'enlist (alexandria:iota len))))))))))

(deftest check-lnode-length/fundamental ()
  (check-lnode-length/in-layer 'fundamental))

(deftest check-lnode-length/transient ()
  (check-lnode-length/in-layer 'transient))

(deftest check-lnode-length/persistent ()
  (check-lnode-length/in-layer 'persistent))

(deftest (check-lnode-inserted-and-removed/in-layer :auto-call nil) (layer-name)
  (funcall-with-layer-context (adjoin-layer layer-name contextl::*root-context*)
    (lambda ()
      (dotimes (rep 8)
        (let* ((len (random 32))
                (ints (alexandria:iota len))
                (ln  (loop for int in ints
                       for ln = (lnode-inserted nil int int 'eql)
                       then (lnode-inserted ln int int 'eql)
                       finally (return ln))))
          (is (= len (lnode-length ln)))
          (loop for i in ints do (is (eql (values i t) (lnode-search ln i 'eql))))
          (loop for i in ints for remain from (1- len) downto 0
            for rest = (lnode-removed ln i 'eql) then (lnode-removed rest i 'eql)
            do (is (eql remain (lnode-length rest)))))))))

(deftest check-lnode-inserted-and-removed/fundamental ()
  (check-lnode-inserted-and-removed/in-layer 'fundamental))

(deftest check-lnode-inserted-and-removed/transient ()
  (check-lnode-inserted-and-removed/in-layer 'transient))

(deftest check-lnode-inserted-and-removed/persistent ()
  (check-lnode-inserted-and-removed/in-layer 'persistent))
