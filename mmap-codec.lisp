;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;;;
;;; For the moment this is implemented via  an indirection in order to evaluate the various
;;; backend alternatives as well as to permit selection of backend on a class by
;;; class basis, since there is considerable difference represented among these options
;;; with regards to depth, completeness, versatility, and speed.  For example,
;;; for serialization of  PACKAGE, Rucksack signals an error, DWIM serializes the
;;; string (package-name PACKAGE) and cl-store serializes a complete representation
;;; including exports, imports, and the whole nine yards. The way I see it at the moment,
;;; the basic functionality matrix out-of-the box, without further customizations or
;;; enhancement is:
;;;
;;;           BACKEND  | DEPTH | COMPLETENESS     | SPEED
;;;          --------------------------------------------
;;;           cl-store | :full | 10 (everything)  | :slow
;;;           hu.dwim  | :full | 8  (most things) | :med
;;;           rucksack |   1   | 6  (many things) | :med-fast
;;;           userial  |   0   | 3  (few things)  | :fast
;;;
;;; I think the ideal solution is to spend some time with userial and put together exactly
;;; the right thing, but that will have to wait until other priorities are addressed, as
;;; the capabilities among the other options seem to offer a reasonable 90% solution, and
;;; the ideal hand-tooled userial solution can be snapped in at any time with 2 lines of
;;; code and altogether invisibly to the higher-level code depending on the uniform
;;; serialize/deserialize API provided.
;;;


(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to various encoding/decoding backends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun serialize-dwim (thing &rest args)
  (apply #'hu.dwim.serializer:serialize thing args))

(defun deserialize-dwim (vector &rest args)
  (apply #'hu.dwim.serializer:deserialize vector args))

(defun serialize-rucksack (thing)
  (flex:with-output-to-sequence (out)
    (rs::serialize thing (make-instance 'rs::serializer :stream out))))

(defun deserialize-rucksack (vector)
  (flex:with-input-from-sequence (in vector)
    (rs::deserialize (make-instance 'rs::serializer :stream in))))

(defun serialize-clstore (thing)
  (flex:with-output-to-sequence (out)
    (cl-store:store thing out)))

(defun deserialize-clstore (vector)
  (flex:with-input-from-sequence (in vector)
    (cl-store:restore in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend Registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((count 0)
       (serializers (make-hash-table))
       (deserializers (make-hash-table))
       (sentinels   nil))   
  (defun register-serializer (keyword serialize-fn deserialize-fn &optional (id count))
    (incf count)
    (setf (gethash keyword serializers) serialize-fn)
    (setf (gethash id deserializers)    deserialize-fn)
    (aconsf sentinels keyword id))
  (defun get-serializer (key)
    (gethash key serializers))
  (defun get-deserializer (int)
    (gethash int deserializers))
  (defun get-keyword-for-id (int)
    (alexandria:rassoc-value sentinels int))
  (defun get-id-for-keyword (key)
    (alexandria:assoc-value sentinels key)))

(register-serializer :dwim     #'serialize-dwim     #'deserialize-dwim)
(register-serializer :rucksack #'serialize-rucksack #'deserialize-rucksack)
(register-serializer :clstore  #'serialize-clstore  #'deserialize-clstore)


(defun serialize-using (backend-key thing)
  (serialize-dwim
    (cons
      (get-id-for-keyword backend-key)
      (funcall (get-serializer backend-key) thing))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniform API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (thing)
  (:method (thing)
    (serialize-using :dwim thing))
  (:method ((thing condition))
    (serialize-using :clstore thing))
  (:method ((thing package))
    (serialize-using :clstore thing)))
  
(defgeneric deserialize (thing)
  (:method ((thing vector))
    (let* ((prelim (deserialize-dwim thing)))
      (funcall (get-deserializer (car prelim)) (cdr prelim)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-output-to-string (s)
  (defun set-hu-serializer-debug-logger (&optional (new-value s))
    (prog1 new-value
      (warn "manually setting serializer debug log; New value is ~A"
        (setf hu.dwim.serializer::*DEBUG-LOG-ENABLED* new-value))))
  (defun get-hu-serializer-debug-stream ()
    s)
  (defun get-hu-serializer-logs ()
    (prog1 (get-output-stream-string s)
      (with-output-to-string (new-s)
        (setf s new-s)
        (set-hu-serializer-debug-logger new-s)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-serialization-roundtrip (value &key (test-fn #'equalp) (key-fn #'identity))
  (let ((restored-value (deserialize (serialize value)))) 
    (assert (funcall test-fn (funcall key-fn value) (funcall key-fn restored-value)))
    (values value restored-value)))


;;;
;; (test-serialization-roundtrip (asdf:find-system :asdf)
;;    :test-fn #'equalp :key-fn #'asdf:system-definition-pathname)
;;
;; #<ASDF:SYSTEM "asdf">
;; #<ASDF:SYSTEM "asdf">
;;


;;;
;; (test-serialization-roundtrip (find-class 'asdf:system))
;;
;;  #<STANDARD-CLASS ASDF:SYSTEM>
;;  #<STANDARD-CLASS ASDF:SYSTEM>
;;


;;;
;; (test-serialization-roundtrip (subseq *features* 0 10))
;;
;; (:COLLEX :RUNE-IS-CHARACTER :CSTM CL-IRREGSEXP::BIG-CHARACTERS-IN-STRINGS
;;  :OSICAT-FD-STREAMS :CHUNGA :FLEXI-STREAMS :STARTED :UP-3548623994 :LOADED)
;; (:COLLEX :RUNE-IS-CHARACTER :CSTM CL-IRREGSEXP::BIG-CHARACTERS-IN-STRINGS
;;  :OSICAT-FD-STREAMS :CHUNGA :FLEXI-STREAMS :STARTED :UP-3548623994 :LOADED)
;;


;;;
;; (test-serialization-roundtrip (find-package :cl)
;;    :test-fn #'equalp :key-fn #'package-used-by-list)
;;
;; #<PACKAGE "COMMON-LISP">
;; #<PACKAGE "COMMON-LISP">
;;


;;;
;; (test-serialization-roundtrip (make-condition 'file-error :pathname (user-homedir-pathname))
;;    :test-fn #'equalp :key-fn #'file-error-pathname)
;;
;; #<FILE-ERROR {1009F59C03}>
;; #<FILE-ERROR {1009F5E093}>


;;;
;; (let (syms (pkgs (list-all-packages)))
;;         (dolist (p pkgs)
;;           (do-external-symbols (s p)
;;             (push s syms)))
;;         (time (mapc #'test-serialization-roundtrip syms))
;;   (length syms))
;;
;; 26268
;;
;; Evaluation took:
;;   0.858 seconds of real time
;;   0.864487 seconds of total run time (0.851599 user, 0.012888 system)
;;   [ Run times consist of 0.118 seconds GC time, and 0.747 seconds non-GC time. ]
;;   100.70% CPU
;;   2,396,516,087 processor cycles
;;   219,560,640 bytes consed


;;;
;; (let ((pkgs (list-all-packages)))
;;         (time (mapc #'test-serialization-roundtrip pkgs))
;;         (length pkgs))
;;
;; 289
;;
;; Evaluation took:
;;   4.132 seconds of real time
;;   4.146531 seconds of total run time (4.127212 user, 0.019319 system)
;;   [ Run times consist of 0.071 seconds GC time, and 4.076 seconds non-GC time. ]
;;   100.36% CPU
;;   11,541,191,949 processor cycles
;;   135,036,608 bytes consed





#|
(defun time-serialize/deserialize (thing)
  (let* ((prior-debug hu.dwim.serializer::*debug-log-enabled*)
         (result (list
                   (time
                     
                    (ignore-errors (warn "testing rucksack")
                      (flex:with-input-from-sequence (in (flex:with-output-to-sequence (s)
                                                           (rs::serialize thing
                                                             (make-instance 'rs::serializer
                                                               :stream s))))
                        (rs::deserialize (make-instance 'rs::serializer :stream in)))))
                   (time
                    
                     (ignore-errors  (warn "testing hu.dwim")
                       (unwind-protect (progn
                                         (setf hu.dwim.serializer::*debug-log-enabled* nil)
                                         (hu.dwim.serializer:deserialize
                                           (hu.dwim.serializer:serialize thing)))
                         (setf hu.dwim.serializer::*debug-log-enabled* prior-debug))))
                   (time
                    
                    (ignore-errors  (warn "testing cl-store")
                      (flex:with-input-from-sequence (in (flex:with-output-to-sequence (s)
                                                           (cl-store:store thing s)))
                        (cl-store:restore in)))))))
    (values result (mapcar (lambda (x) (equalp x thing)) result))
    ))

(defun describe-all (things)
  (mapc #'describe (alexandria:ensure-list things)))

|#
