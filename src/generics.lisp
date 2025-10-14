
(defpackage #:breeze.generics
  (:documentation "Declare some generic interfaces.")
  (:use #:cl)
  (:export #:eqv #:name #:source))

(in-package #:breeze.generics)

(defgeneric source (x)
  (:documentation "Get the source (usually a string) of x."))

(defgeneric eqv (a b)
  (:documentation "Test whether A and B are equivalent."))

(defmethod eqv ((a (eql :_)) b)
  "Don't care."
  t)

(defmethod eqv (a (b (eql :_)))
  "Don't care."
  t)

(defmethod eqv ((a symbol) b)
  (eq a b))

(defmethod eqv (a (b symbol))
  (eq a b))

(defmethod eqv (a b)
  (equalp a b))

(defmethod eqv ((a sequence) (b sequence))
  (if (eq nil b)
      (eq a b)
      (or (eq a b)
          (every #'eqv a b))))

(defmethod eqv ((a cons) (b cons))
  (loop :for x :on a
        :for y :on b
        :always (eqv (car x) (car y))))

;; TODO this is utterly untested (not even in the repl)
(defmethod eqv ((a hash-table) (b hash-table))
  (and
   (= (hash-table-count a) (hash-table-count b))
   (loop :for key :being :the :hash-key :of a :using (hash-value value-a)
         :always (multiple-value-bind (value-b value-b-p)
                     (gethash key b)
                   (and value-b-p
                        (eqv value-a value-b))))))

(defmethod eqv ((a string) (b string))
  (string= a b))

#++
(progn
  (eqv '(?var . a) '(?var . a))
  (eqv '(?var . :_) '(?var . a))
  (eqv '(?var . a) '(?var . :_)))

#|

start
end
emptyp
donep

source: TODO use in "reader" and "buffer" (buffer -> node-iterator -> parser-state -> source)

|#

(defgeneric name (thing)
  (:documentation "Get the name of THING."))

;; TODO create a mix-in class for "objec with a name"
