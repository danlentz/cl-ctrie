;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-ctrie-test)

(defsuite*  (cl-ctrie/ctrie/layers :in cl-ctrie/ctrie))

(deftest (check-configuration-context/validity) ()
  (is (typep 'set 'valid-configuration-layer))
  (is (not (valid-configuration-context-p '(set))))
  (is (typep '(set unordered transient) 'valid-configuration-context))
  (is (not (typep '(unordered set) 'valid-configuration-context)))
  (is (typep '(persistent weight-balanced map) 'valid-configuration-context))
  (is (not (typep '(allocation unordered set) 'valid-configuration-context))))


(deftest (check-grouped-layer-metaclass/activation) ()
  (flet ((active-layer-names ()
           (mapcar #'layer-name (active-layers))))
    (with-active-layers (unordered)
      (is (equalp (active-layer-names) '(unordered t)))
      (with-active-layers (ordered)
        (is (equalp (active-layer-names) '(ordered t)))
        (with-active-layers (weight-balanced)
          (is (equalp (active-layer-names) '(weight-balanced t)))        
          (with-active-layers (height-balanced)
            (is (equalp (active-layer-names) '(height-balanced t)))))))))


(deftest (check-grouped-layer-metaclass/configurations) ()
  (flet ((active-layer-names ()
           (remove 't (mapcar #'layer-name (active-layers)))))
    (with-active-layers (transient unordered map)
      (is (typep (active-layer-names) 'valid-configuration-context))
      (is (equalp (active-layer-names) '(map unordered transient)))
      (with-active-layers (set)
        (is (typep (active-layer-names) 'valid-configuration-context))
        (is (equalp (active-layer-names) '(set unordered transient)))
        (with-active-layers (persistent weight-balanced seq)
          (is (typep (active-layer-names) 'valid-configuration-context))
          (is (equalp (active-layer-names) '(seq weight-balanced persistent)))
          (with-active-layers (height-balanced)
            (is (typep (active-layer-names) 'valid-configuration-context))
            (is (equalp (active-layer-names) '(height-balanced seq persistent)))))))))            


(deftest (check-configuration-context :auto-call nil) (&rest layers)
  (flet ((active-layer-names ()
           (remove 't (mapcar #'layer-name (active-layers))))
          (configuration-context ()
            (apply #'combined-layer-context contextl::*active-context* layers)))
    (let ((layer-names (funcall-with-layer-context
                         (configuration-context) #'active-layer-names)))
      (is (equalp (reverse layers) layer-names))
      (is (typep layer-names 'valid-configuration-context)))))


(deftest (check-configuration-context/combined) ()
  (let ((ctxs '((unordered transient set)
                 (weight-balanced map persistent)
                 (persistent/cache height-balanced seq)
                 (map unordered transient)
                 (unordered set persistent))))
    (loop for ctx in ctxs do (apply #'check-configuration-context ctx))))

