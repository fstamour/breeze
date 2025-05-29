(defpackage #:breeze.test.iterator
  (:documentation "Tests for the package breeze.iterator")
  (:use #:cl #:breeze.iterator)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail)
  ;; importing unexported symbols
  (:import-from #:breeze.iterator))

(in-package #:breeze.test.iterator)

(define-test+run vector-iterator
  (let* ((vector #(1 2 3))
         (iterator (make-vector-iterator vector)))
    (is eq vector (slot-value iterator 'vector))
    (is = 0 (current-position iterator))
    (progn
      (true (firstp iterator))
      (false (before-last-p iterator))
      (false (lastp iterator)))
    (progn
      (next iterator)
      (false (firstp iterator))
      (true (before-last-p iterator))
      (false (lastp iterator)))
    (progn
      (next iterator)
      (false (firstp iterator))
      (false (before-last-p iterator))
      (true (lastp iterator))))
  (let* ((vector #(1 2 3))
         (iterator (make-vector-iterator vector :position 2)))
    (is eq vector (slot-value iterator 'vector))
    (is = 2 (current-position iterator)))
  (is equal (list 2 3)
      (collect (make-vector-iterator #(1 2 3) :position 1)))
  (false
   (collect (make-vector-iterator #(1 2 3) :position 10))))

(define-test+run selector
  (is equal (list 0 2 4 6)
      (collect (make-selector
                (make-vector-iterator #(0 1 2 3 4 5 6))
                #'evenp))
      "Should have removed the odd values")
  (is equal '(b d f)
      (collect (make-selector
                (make-vector-iterator #(a b c d e f))
                (lambda (iterator)
                  (oddp (slot-value iterator 'position)))
                :apply-filter-to-iterator-p t))
      "Should have removed the even positions"))

(define-test+run nested-vector-iterator
  (let* ((vector #(1 2 3))
         (iterator (make-nested-vector-iterator vector)))
    (is eq vector (vec iterator))
    (is = 0 (pos iterator)))
    ;; Testing push-vector and pop-vector
  (is equal '(1 2 3 4)
      (let ((iterator (make-nested-vector-iterator #(1 #(2 3) 4))))
        (list (prog1 (value iterator) (next iterator))
              (progn (push-vector iterator (value iterator))
                     (value iterator))
              (progn (next iterator)
                     (value iterator))
              (progn (pop-vector iterator) (next iterator)
                     (value iterator))))))

(defun make-leaf (vector)
  (make-leaf-iterator vector #'vectorp))

(define-test+run leaf-iterator
  (true (donep (make-leaf #())))
  (true (donep (make-leaf #(#()))))
  (true (donep (make-leaf #(#(#())))))
  (false (donep (make-leaf #(#() 1 #()))))
  (false (collect (make-leaf #())))
  (false (collect (make-leaf #(#()))))
  (is equal '(1)
      (collect (make-leaf #(1 #()))))
  (is equal '(1)
      (collect (make-leaf #(#() 1))))
  (is equal '(1 2)
      (collect (make-leaf #(1 #() 2))))
  (is equal '(1 2 3 4)
      (collect (make-leaf #(1 #(2 3) 4)))
      "It should have flattened the nested vectors.")
  (is equal '(1 2 3 4)
      (collect (make-leaf #(1 #() #(2 3) 4)))
      "It should have flattened the nested vectors.")
  (is equal '(a b c d e f g)
      (collect (make-leaf #(a b #(c d #(e f) g))))
      "It should have flattened the nested vectors.")
  (is equal '(1 2 3 4 5 6 7 8 9)
      (collect
          (make-selector
           (make-leaf #(1 2 #(3 4 #(5 6) 7) 8 9))
           (constantly t)))
      "Selector and recursive-iterator should interact correctly...")
  (is equal '(b d f)
      (let ((i 0))
        (collect
            (make-selector
             (make-leaf #(a b #(c d #(e f) g)))
             (lambda (iterator)
               (declare (ignore iterator))
               (prog1 (oddp i) (incf i)))
             :apply-filter-to-iterator-p t)))
      "Should flatten the nested vectors and keep only the odd positions."))

(defun make-pre-order (vector)
  (make-pre-order-iterator vector #'vectorp))

;; (defparameter *it* (make-pre-order #(#())))
;; (next *it*)

(define-test+run pre-order-iterator
  (true (donep (make-pre-order #())))
  (false (donep (make-pre-order #(#()))))
  (false (donep (make-pre-order #(#() 1 #()))))
  (is equalp nil (collect (make-pre-order #())))
  (is equalp '(#()) (collect (make-pre-order #(#()))))
  (is equalp '(1 #())
      (collect (make-pre-order #(1 #()))))
  (is equalp '(#() 1)
      (collect (make-pre-order #(#() 1))))
  (is equalp '(1 #() 2)
      (collect (make-pre-order #(1 #() 2))))
  (is equalp '(1 #(2) 2 3)
      (collect (make-pre-order #(1 #(2) 3))))
  (is equalp '(1 #(2 3) 2 3 4)
      (collect
          (make-pre-order
           #(1 #(2 3) 4)))
      "It should have iterated on both the vectors and their elements.")
  (is equalp '(a b #(c d #(e f) g) c d #(e f) e f g)
      (collect
          (make-pre-order #(a b #(c d #(e f) g))))
      "It should have iterated on both the vectors and their elements.")
  (is equalp '(a #() b c)
      (collect
          (make-pre-order #(a #() b c)))
      "It should have iterated on both the vectors and their elements.")
  (is equalp '(1 2 #(3 4 #(5 6) 7) 3 4 #(5 6) 5 6 7 8 9)
      (collect
          (make-selector
           (make-pre-order #(1 2 #(3 4 #(5 6) 7) 8 9))
           (constantly t)))
      "Selector and recursive-iterator (pre-order traversal) should interact correctly...")
  (is equalp '(b c #(e f) f)
      (let ((i 0))
        (collect
            (make-selector
             (make-pre-order #(a b #(c d #(e f) g)))
             (lambda (iterator)
               (declare (ignore iterator))
               (prog1 (oddp i) (incf i)))
             :apply-filter-to-iterator-p t)))
      "Should iterate on both the vectors and their elements and keep only the odd positions."))


#++
(collect
    (make-concat-iterator
     (vector
      (make-vector-iterator #(1 2 3))
      (make-vector-iterator #(a b c))))
  :limit 10)

#++
(let ((it (make-nested-vector-iterator
          #(a b #(c) #(d #(e))))))
  (collect it :limit 2))
