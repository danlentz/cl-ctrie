;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package "SB-VM")

;;
;; This cas extension is entirely the genius of Paul Khuong.
;;


(handler-bind ((simple-error
                 (lambda (condition) (declare (ignore condition))
                   (invoke-restart 'continue))))
  (sb-c:defknown cl-ctrie::spin-hint
    ()
    (values))
  (sb-c:defknown cl-ctrie::xadd-word-sap
    (system-area-pointer word)
    word)
  (sb-c:defknown cl-ctrie::cas-word-sap
    (system-area-pointer word word)
    word)   
  (sb-c:defknown cl-ctrie::cas-byte-sap
    (system-area-pointer (unsigned-byte 8) (unsigned-byte 8))
    (unsigned-byte 8)))

  
(sb-c:define-vop (cl-ctrie::spin-hint)
  (:translate cl-ctrie::spin-hint)
  (:policy :fast-safe)
  (:generator 5
    (sb-assem:inst sb-vm::pause)))


(sb-c:define-vop (cl-ctrie::cas-byte-sap)
  (:translate cl-ctrie::cas-byte-sap)
  (:policy :fast-safe)
  (:args (sap :scs (sb-vm::sap-reg) :to :eval)
         (old :scs (sb-vm::unsigned-reg) :target rax)
         (new :scs (sb-vm::unsigned-reg)))
  (:temporary (:sc descriptor-reg :offset rax-offset
                   :from (:argument 1) :to :result :target result) rax)
  (:results (result :scs (sb-vm::unsigned-reg)))
  (:arg-types system-area-pointer sb-vm::unsigned-num sb-vm::unsigned-num)
  (:result-types sb-vm::unsigned-num)
  (:generator 5
     (sb-vm::move rax old)
     (sb-vm::inst sb-vm::cmpxchg (sb-vm::make-ea :byte :base sap)
           (make-byte-tn new) :lock)
     (sb-vm::inst sb-vm::movzx result al-tn)))


(sb-c:define-vop (cl-ctrie::cas-word-sap)
  (:translate cl-ctrie::cas-word-sap)
  (:policy :fast-safe)
  (:args (sap :scs (sb-vm::sap-reg) :to :eval)
         (old :scs (sb-vm::unsigned-reg) :target eax)
         (new :scs (sb-vm::unsigned-reg)))
  (:arg-types system-area-pointer sb-vm::unsigned-num sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rax-offset
                :from (:argument 1) :to :result :target r) eax)
  (:results (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (sb-vm::move eax old)
    (sb-vm::inst sb-vm::cmpxchg (sb-vm::make-ea :qword :base sap) new :lock)
    (sb-vm::move r eax)))


(define-vop (cl-ctrie::xadd-word-sap)
  (:translate cl-ctrie::xadd-word-sap)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (inc :scs (unsigned-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:arg-types system-area-pointer unsigned-num)
  (:result-types unsigned-num)
  (:generator 5
     (inst xadd (make-ea :qword :base sap)
           inc :lock)
    (move result inc)))


(defun cl-ctrie::spin-hint ()
  (cl-ctrie::spin-hint))


(defun cl-ctrie::cas-word-sap (sap old new)
  (declare (type system-area-pointer sap)
           (word old new))
  (cl-ctrie::cas-word-sap sap old new))


(defun cl-ctrie::cas-byte-sap (sap old new)
  (declare (type system-area-pointer sap)
    (type (unsigned-byte 8) old new))
  (cl-ctrie::cas-byte-sap sap old new))


(defun cl-ctrie::xadd-word-sap (sap word)
  (declare (type system-area-pointer sap)
           (type sb-vm:word word))
  (cl-ctrie::xadd-word-sap sap word))


(defun cl-ctrie::get-vector-addr (vector)
  (logandc2 (sb-kernel:get-lisp-obj-address vector) sb-vm:lowtag-mask))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLS VOPs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; see cell.lisp:symbol-value
(eval-when (:compile-toplevel)
  (define-vop (tls-ref)
    (:args (index :scs (descriptor-reg)))
    (:results (value :scs (descriptor-reg)))
    #+x86-64
    (:generator 5
                (inst mov value (make-ea :qword
                                         :base thread-base-tn
                                         :index index :scale 1)))
    #+x86
    (:generator 5
                (inst fs-segment-prefix)
                (inst mov value (make-ea :dword :base index))))
  
  (define-vop (tls-set)
    (:args (value :scs (descriptor-reg))
           (index :scs (descriptor-reg)))
    (:results)
    #+x86-64
    (:generator 5
                (inst mov (make-ea :qword
                                   :base thread-base-tn
                                   :index index :scale 1)
                      value))
    #+x86
    (:generator 5
                (inst fs-segment-prefix)
                (inst mov (make-ea :dword :base index) value)))
  
  (define-vop (%set-symbol-global-value)
    (:args (value  :scs (descriptor-reg))
           (symbol :scs (descriptor-reg)))
    (:results)
    #+(or x86-64 x86)
    (:generator 5
                (storew value symbol symbol-value-slot other-pointer-lowtag))))
