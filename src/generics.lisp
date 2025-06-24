
(defpackage #:breeze.generics
  (:documentation "Declare some generic interfaces.")
  (:use #:cl)
  (:export #:eqv))

(in-package #:breeze.generics)


(defgeneric eqv (a b)
  (:documentation "Test whether A and B are equivalent."))

(defmethod eqv (a b)
  (equalp a b))

(defmethod eqv ((a sequence) (b sequence))
  (every #'eqv a b))

(defmethod eqv ((a cons) (b cons))
  (loop :for x :on a
        :for y :on b
        :always (eqv (car x) (car y))))

#++
(eqv
 '(?var . a)
 '(?var . a))

#|

start
end
name
eqv
source
emptyp
donep

|#
