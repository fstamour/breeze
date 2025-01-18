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
    (is eq vector (vec iterator))
    (is = 0 (pos iterator)))
  (let* ((vector #(1 2 3))
         (iterator (make-vector-iterator vector :position 2)))
    (is eq vector (vec iterator))
    (is = 2 (pos iterator)))
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
                  (oddp (pos iterator)))
                :apply-filter-to-iterator-p t))
      "Should have removed the even positions"))

(define-test+run nested-vector-iterator
  (let* ((vector #(1 2 3))
         (iterator (make-nested-vector-iterator vector)))
    (is eq vector (vec iterator))
    (is = 0 (pos iterator)))
  (let* ((vector #(1 2 3))
         (iterator (make-nested-vector-iterator vector :position 2)))
    (is eq vector (vec iterator))
    (is = 2 (pos iterator)))
  (is equal (list 2 3)
      (collect (make-nested-vector-iterator #(1 2 3) :position 1)))
  (false
   (collect (make-nested-vector-iterator #(1 2 3) :position 10)))
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

#++
(defun dig-into-even-length-vector (iterator)
  (let ((x (value iterator)))
    (and (vectorp x)
         (evenp (length x)))))

(define-test+run flattener
  (is equal '(1 2 3 4)
      (collect
          (make-flattener
           (make-nested-vector-iterator #(1 #(2 3) 4))
           #'vectorp))
      "It should have flattened the nested vectors.")
  (is equal '(a b c d e f g)
      (collect
          (make-flattener
           (make-nested-vector-iterator #(a b #(c d #(e f) g)))
           #'vectorp))
      "It should have flattened the nested vectors.")
  (is equal '(1 2 3 4 5 6 7 8 9)
      (collect
          (make-selector
           (make-flattener
            (make-nested-vector-iterator #(1 2 #(3 4 #(5 6) 7) 8 9))
            #'vectorp)
           (constantly t)))
      "Selector and flattener should interact correctly...")
  (is equal '(b d f)
      (let ((i 0))
        (collect
            (make-selector
             (make-flattener
              (make-nested-vector-iterator #(a b #(c d #(e f) g)))
              #'vectorp)
             (lambda (iterator)
               (prog1 (oddp i) (incf i)))
             :apply-filter-to-iterator-p t)))
      "Should flatten the nested vectors and keep only the odd positions."))


#++
(collect
    (make-concat-iterator
     (vector
      (make-vector-iterator #(1 2 3))
      (make-vector-iterator #(a b c))))
  :limit 10)
