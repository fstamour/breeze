;; https://github.com/ksluckow/awesome-symbolic-execution
;; https://en.wikipedia.org/wiki/Symbolic_execution

#|

use cases:
- inferring a function's type
- inferring a macro's type
  -end goal: smart completion, maybe suggest adding `check-type' s to the macro

- "propagate" a default value of a function, see if it's equivalent to another
- propagate the "actual used values"...
  - if a function supports many types, but is always called with the same value or type.

|#

(defpackage #:breeze.abstract-symbolic
  (:documentation "Abstract interpretation and symbolic execution.")
  (:use #:cl))

(in-package #:breeze.abstract-symbolic)

(defun evall (form)
  (cond
    ((constantp form) form)
    ((symbolp form)
     ())))

(let ((env ()))
  (loop :for form :in `((+ 1 x))))


(defun interpret-call (function-form argforms env)
  (apply (interpret function-form env) (evlis argforms env)))
