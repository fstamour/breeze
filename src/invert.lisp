
(defpackage #:breeze.invert
  (:documentation "Command to invert the form at point.")
  (:use #:cl))

(in-package #:breeze.invert)


(unless (donep $node)
  (cond
    ((match (sym :wild t) $node)
     ;; replace by nil
     )
    ((match (sym :wild nil) $node)
     ;; replace by t
     )))
