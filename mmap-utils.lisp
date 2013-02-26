;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-mmap)


(defconstant +mptr-bits+   64)
(defconstant +mtag-bits+   8)
(defconstant +mtags+       (ash 1 +mtag-bits+))
(defconstant +mindex-bits+ (- +mptr-bits+ +mtag-bits+))
(defconstant +word-length+ 8)

(deftype word ()
  `(unsigned-byte 64))


(defmacro defun-speedy (name lambda-list &body body &environment env)
  (declare (ignorable env))
  `(progn
     #+(or) (declaim (inline ,name)) 
     #+lispworks ,@(when env `((declaim (notinline ,name)))) 
     (defun ,name ,lambda-list
       #+(or) (declare (optimize speed))
       ,@body)))


(defun fc (class-designator)
  (typecase class-designator
    (class    class-designator)  
    (keyword (fc (string class-designator)))
    (string  (fc (read-from-string class-designator)))
    (symbol  (find-class class-designator))
    (t       (find-class class-designator))))

;; (fc 'standard-class)
;; (fc :standard-class)
;; (fc 'cl:standard-class)
;; (fc "standard-class")
;; (fc ":standard-class")
;; (fc "cl:standard-class")
;; (fc (fc 'standard-class))


(defun force-class (class-specifier)
  (fc class-specifier))


(defmacro cassert (test-form &optional places string &rest args)
  (declare (ignore places))
  `(unless ,test-form
     (cerror "Ignore the assertion"
       ,(or string (format nil "Assertion ~S failed" test-form)) ,@args)))


(define-symbol-macro ? (describe *))


#+swank 
(defun ^ (thing &optional wait)
  (swank:inspect-in-emacs thing :wait wait))

#+swank
(define-symbol-macro ^* (^ *))

#+swank
(define-symbol-macro ^** (^ **))

#+swank
(define-symbol-macro ^*** (^ ***))


(defun finalize-class (class-designator)
  (finalize-inheritance (fc class-designator))
  (fc class-designator))

(defun new (&rest args)
  (apply #'make-instance args))




(defun pp-line (ptr start width count &optional (stream t))
  (let ((bytes (loop for i below count
                     collect (cffi:mem-ref ptr :uint8 (+ start i)))))
    (format stream "~&~8,'0X ~{~2,'0X ~}~vt| ~{~C~}"
            start
            bytes
            (+ 10 (* width 3))
            (loop for byte in bytes
                  for char = (code-char byte)
                  collect (if (graphic-char-p char)
                              char
                              #\.)))))

(defun pp-mem (ptr count &key (width 16) (stream t))
  (loop for i below count by width
     do (pp-line ptr i width (min width (- count i)) stream))
  (terpri stream))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PP-MEM example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (pp-mem (sb-vm::vector-sap
;;           (io:string-to-octets (princ-to-string *features*))) #xe0)

;; 00000000 28 46 53 45 54 2D 45 58 54 2D 53 54 52 49 4E 47  | (FSET-EXT-STRING
;; 00000010 53 20 43 4C 2D 53 54 4F 52 45 20 42 49 47 2D 43  | S CL-STORE BIG-C
;; 00000020 48 41 52 41 43 54 45 52 53 2D 49 4E 2D 53 54 52  | HARACTERS-IN-STR
;; 00000030 49 4E 47 53 20 45 4C 45 50 48 41 4E 54 20 53 54  | INGS ELEPHANT ST
;; 00000040 41 52 54 45 44 0A 20 55 50 2D 33 35 37 30 36 33  | ARTED. UP-357063
;; 00000050 39 32 32 34 20 4C 4F 41 44 45 44 20 48 55 2E 44  | 9224 LOADED HU.D
;; 00000060 57 49 4D 20 43 4C 2D 54 59 50 45 53 45 54 54 49  | WIM CL-TYPESETTI
;; 00000070 4E 47 20 43 4C 2D 50 44 46 20 50 44 46 2D 42 49  | NG CL-PDF PDF-BI
;; 00000080 4E 41 52 59 20 55 53 45 2D 4E 4F 2D 5A 4C 49 42  | NARY USE-NO-ZLIB
;; 00000090 0A 20 4B 4D 52 2D 4D 4F 50 20 48 41 56 45 2D 4D  | . KMR-MOP HAVE-M
;; 000000A0 4F 50 20 52 54 20 4C 49 53 50 2D 55 4E 49 54 20  | OP RT LISP-UNIT 
;; 000000B0 55 4E 49 43 4C 59 20 43 4C 44 4F 43 20 53 43 52  | UNICLY CLDOC SCR
;; 000000C0 45 41 4D 45 52 20 43 4C 2D 50 50 43 52 45 2D 55  | EAMER CL-PPCRE-U
;; 000000D0 4E 49 43 4F 44 45 0A 20 43 4C 2D 55 4E 49 43 4F  | NICODE. CL-UNICO

