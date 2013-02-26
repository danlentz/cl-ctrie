;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mm)

(sb-ext:defglobal *big-lock* (vector nil))
(sb-ext:defglobal *mptr-locks* (make-hash-table :test 'eql))

(defmacro with-locked-world (&body body)
  `(sb-thread::with-cas-lock ((svref *big-lock* 0))
     ,@body))

;; (with-world-lock 
;;   (print *big-lock*))

(defun find-mptr-lock (mptr)
  (with-locked-world 
    (or (gethash mptr *mptr-locks*)
      (setf (gethash mptr *mptr-locks*) (bt:make-lock (format nil "mptr~D" mptr))))))


(defmacro with-locked-mptr ((mptr) &body body)
  (alexandria:with-unique-names (-mptr-)
    `(let* ((,-mptr- ,mptr)
             (-mptr- ,-mptr-))
       (bt:with-lock-held ((find-mptr-lock -mptr-))
         ,@body))))

;; (cl-ctrie::ppmx (with-locked-mptr (999)
;;                   (describe (find-mptr-lock -mptr-))))
  
(defmmclass register ()
  ((content-value
     :initform nil
     :initarg  :content
     :accessor %register-content-value)
    (content-type
     :initform 't
     :initarg  :type
      :accessor %register-content-type)))

(defmmclass register-bank (marray)
  ((name
     :initarg :name
     :accessor register-bank-name
     :type string
     :initform (symbol-name (gensym)))
    (registers
      :accessor %register-bank-registers)))

(defmethod initialize-instance :after ((self register-bank) &key)
  nil)

   
(defun ensure-register-bank (name &optional (size 8))
  (with-world-lock
    (or
      (find name (retrieve-all-instances 'register-bank)
        :key #'register-bank-name :test #'equalp)
      (let ((it (make-marray size
                  :marray-class 'register-bank
                  :initial-contents (loop repeat size
                                      collect (make-instance 'register)))))
        (prog1 it
          (setf (register-bank-name it) name)
          (setf (%register-bank-registers it)
            (make-array (marray-length it) :initial-contents (marray-to-list it))))))))

(defun register-bank (designator)
  (%register-bank-registers 
    (etypecase designator
      (string        (ensure-register-bank designator))
      (keyword       (ensure-register-bank (symbol-name designator)))
      (register-bank designator))))

;; (every #'meq (register-bank "xyz") (register-bank "xyz"))
;; (every #'meq (register-bank :home) (register-bank :home))


(defun register (bank index)
)




;; (defparameter b0 (make-register-bank "test" 8))

;; (describe b0)
;; #<REGISTER-BANK  M@26649(25:104)>
;;   [standard-object]

;; Slots with :MEMORY allocation:
;;   LENGTH     = 8
;;   BASE       = 2214909953
;;   NAME       = "test"
;;   REGISTERS  = #(#<REGISTER  M@100378(26:392)> #<REGISTER  M@104474(26:408)>..
;; Slots with :INSTANCE allocation:
;;   %PTR       = 26649
;;                   (%register-bank-registers b0)
;; #(#<REGISTER  M@100378(26:392)> #<REGISTER  M@104474(26:408)>
;;   #<REGISTER  M@108570(26:424)> #<REGISTER  M@112666(26:440)>
;;   #<REGISTER  M@116762(26:456)> #<REGISTER  M@120858(26:472)>
;;   #<REGISTER  M@124954(26:488)> #<REGISTER  M@129050(26:504)>)

                  
;; #<REGISTER-BANK  M@26649(25:104)>
;; ?

;; (retrieve-all-instances 'register-bank)
;; (#<REGISTER-BANK  M@18457(25:72)> #<REGISTER-BANK  M@10265(25:40)>
;;   #<REGISTER-BANK  M@2073(25:8)>)


;; b0
;; #(#<REGISTER  M@67610(26:264)> #<REGISTER  M@71706(26:280)>
;;   #<REGISTER  M@75802(26:296)> #<REGISTER  M@79898(26:312)>
;;   #<REGISTER  M@83994(26:328)> #<REGISTER  M@88090(26:344)>
;;    #<REGISTER  M@92186(26:360)> #<REGISTER  M@96282(26:376)>)
;; b0
;; #(#<REGISTER  M@67610(26:264)> #<REGISTER  M@71706(26:280)>
;;   #<REGISTER  M@75802(26:296)> #<REGISTER  M@79898(26:312)>
;;   #<REGISTER  M@83994(26:328)> #<REGISTER  M@88090(26:344)>
;;   #<REGISTER  M@92186(26:360)> #<REGISTER  M@96282(26:376)>)
