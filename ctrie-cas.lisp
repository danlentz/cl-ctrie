;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package "SB-VM")

;;
;; This cas extension is entirely the genius of Paul Khuong.
;;


(handler-bind ((simple-error
                 (lambda (condition) (declare (ignore condition))
                   (invoke-restart 'continue))))
  (sb-c:defknown ctrie::spin-hint
    ()
    (values))
  (sb-c:defknown ctrie::xadd-word-sap
    (system-area-pointer word)
    word)
  (sb-c:defknown ctrie::cas-word-sap
    (system-area-pointer word word)
    word)   
  (sb-c:defknown ctrie::cas-byte-sap
    (system-area-pointer (unsigned-byte 8) (unsigned-byte 8))
    (unsigned-byte 8)))

  
(sb-c:define-vop (ctrie::spin-hint)
  (:translate ctrie::spin-hint)
  (:policy :fast-safe)
  (:generator 5
    (sb-assem:inst sb-vm::pause)))


(sb-c:define-vop (ctrie::cas-byte-sap)
  (:translate ctrie::cas-byte-sap)
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


(sb-c:define-vop (ctrie::cas-word-sap)
  (:translate ctrie::cas-word-sap)
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


(define-vop (ctrie::xadd-word-sap)
  (:translate ctrie::xadd-word-sap)
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


(defun ctrie::spin-hint ()
  (ctrie::spin-hint))


(defun ctrie::cas-word-sap (sap old new)
  (declare (type system-area-pointer sap)
           (word old new))
  (ctrie::cas-word-sap sap old new))


(defun ctrie::cas-byte-sap (sap old new)
  (declare (type system-area-pointer sap)
    (type (unsigned-byte 8) old new))
  (ctrie::cas-byte-sap sap old new))


(defun ctrie::xadd-word-sap (sap word)
  (declare (type system-area-pointer sap)
           (type sb-vm:word word))
  (ctrie::xadd-word-sap sap word))


(defun ctrie::get-vector-addr (vector)
  (logandc2 (sb-kernel:get-lisp-obj-address vector) sb-vm:lowtag-mask))


