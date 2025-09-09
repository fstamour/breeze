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
                #:fail))

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

(define-test+run tree-iterator
  (let* ((root #(1 2 3))
         (iterator (make-tree-iterator root)))
    (is eq root (subtree iterator))
    (is = 0 (pos iterator))
    (is equal '(1 2 3) (collect iterator)))
  (let* ((iterator (make-tree-iterator #())))
    (true (donep iterator)))
    ;; Testing push-subtree and pop-subtree
  (is equal '(1 2 3 4)
      (let ((iterator (make-tree-iterator #(1 #(2 3) 4))))
        (list (prog1 (value iterator) (next iterator))
              (progn (push-subtree iterator (value iterator))
                     (value iterator))
              (progn (next iterator)
                     (value iterator))
              (progn (pop-subtree iterator) (next iterator)
                     (value iterator))))))

#++
(collect
    (make-concat-iterator
     (vector
      (make-vector-iterator #(1 2 3))
      (make-vector-iterator #(a b c))))
  :limit 10)

#++
(let ((it (make-tree-iterator
          #(a b #(c) #(d #(e))))))
  (collect it :limit 2))
