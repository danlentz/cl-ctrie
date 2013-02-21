;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/common/io :in cl-ctrie/common))

(deftest check-string-octet-vector/roundtrip ()
  (let* ((original-string (machine-instance))
          (octets (io:string-to-octets original-string))
          (new-string (io:octets-to-string octets)))
    (is (stringp original-string))
    (is (every #'numberp octets))
    (is (every #'characterp new-string))
    (is (string-equal original-string new-string))))


(deftest check-temporary-file/roundtrip-string ()
  (is (equal "hi" (io:with-temporary-file (x)
                    (format x "hi")
                    (force-output x)
                    (io:read-file-to-string io:-filename-))))
  (is (equal "hi" (io:with-temporary-file (x fn)
                    (format x "hi")
                    (force-output x)
                    (io:read-file-to-string fn))))
  (is (equal "hi" (io:with-temporary-file ()
                    (format io:-file- "hi")
                    (force-output io:-file-)
                    (io:read-file-to-string io:-filename-)))))


(deftest check-temporary-file/roundtrip-sexp ()
  (let* ((existing-file (asdf:system-definition-pathname :cl-ctrie))
          (sexp-from-existing-file (io:read-file-to-list existing-file)))
    (is (listp sexp-from-existing-file))
    (is (eq (caar sexp-from-existing-file) 'in-package))
    (is (equalp '(:a (:b :c) :d) (io:with-temporary-file (x)
                                   (io:write-list-to-file '(:a (:b :c) :d) x)
                                   (io:read-file-to-list x))))))
    

(deftest check-temporary-file/roundtrip-string-list ()
  (let* ((existing-file (asdf:system-definition-pathname :cl-ctrie))
          (string-list-from-existing-file (io:read-file-to-string-list existing-file)))
    (is (listp string-list-from-existing-file))
    (is (every #'stringp string-list-from-existing-file))
    (is (equalp ";;;" (subseq (first string-list-from-existing-file) 0 3)))
    (is (equalp '("1" "2" "3") (io:with-temporary-file (x)
                                 (io:write-string-list-to-file '("1" "2" "3") x)
                                 (io:read-file-to-string-list x))))))


(deftest check-temporary-file/roundtrip-byte-vector ()
  (let* ((existing-file (asdf:system-definition-pathname :cl-ctrie))
          (byte-vector-from-existing-file (io:read-file-to-byte-vector existing-file)))
    (is (vectorp byte-vector-from-existing-file))
    (is (every #'numberp byte-vector-from-existing-file))
    (is (equalp #(59 59 59) (subseq  byte-vector-from-existing-file 0 3)))
    (is (equalp #(65 66 67) (io:with-temporary-file (x)
                              (io:write-byte-vector-to-file #(65 66 67) x)
                              (io:read-file-to-byte-vector x))))
    (is (equalp "ABC" (io:with-temporary-file (x)
                        (io:write-byte-vector-to-file #(65 66 67) x)
                        (io:read-file-to-string x))))))

(deftest check-binary-file-copy/roundtrip-file-equal ()
  (is (equalp #(65 66 67) (io:with-temporary-file (x xn)
                            (io:write-byte-vector-to-file #(65 66 67) x)
                            (force-output x)
                            (io:with-temporary-file (y yn)
                              (io:copy-file xn yn)
                              (io:read-file-to-byte-vector y)))))
  (io:with-temporary-file (x xn)
    (io:write-byte-vector-to-file #(65 66 67) x)
    (force-output x)
    (io:with-temporary-file (y yn)
      (io:copy-file xn yn)
      (is (io:file-equal xn yn)))))
  

