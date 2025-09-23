
(defpackage #:breeze.generics
  (:documentation "Declare some generic interfaces.")
  (:use #:cl)
  (:export #:eqv #:name))

(in-package #:breeze.generics)


(defgeneric eqv (a b)
  (:documentation "Test whether A and B are equivalent."))

(defmethod eqv ((a (eql :_)) b)
  "Don't care."
  t)

(defmethod eqv (a (b (eql :_)))
  "Don't care."
  t)

(defmethod eqv (a b)
  (equalp a b))

(defmethod eqv ((a sequence) (b sequence))
  (or (eq a b)
      (every #'eqv a b)))

(defmethod eqv ((a cons) (b cons))
  (loop :for x :on a
        :for y :on b
        :always (eqv (car x) (car y))))

#++
(progn
  (eqv '(?var . a) '(?var . a))
  (eqv '(?var . :_) '(?var . a))
  (eqv '(?var . a) '(?var . :_)))

#|

start
end
source
emptyp
donep

|#

(defgeneric name (thing)
  (:documentation "Get the name of THING."))

;; TODO create a mix-in class for "objec with a name"
