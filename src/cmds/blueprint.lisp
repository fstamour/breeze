
(defpackage #:breeze.blueprint
  (:documentation "Snippets, but better™")
  (:use #:cl))

(in-package #:breeze.blueprint)

;; TODO make "breeze-quickinsert" choose to insert a defun when there's a "defun" at point...
#|

defun


defmethod insert-pattern

patterns need docstring/documentation/prompt

insert-symbol
ensure-sym (convert symbol to sym)
|#

#++ ;; TODO snippet draft:
`(define-command (:the symbol ?name) () \n
   (fmt "\"~@(~a~).\"" ?name))

;; DSL:
#++
((sym "make-array" "cl")
 '(0)
 :element-type <>
 :adjustable t
 :fill-pointer t)
