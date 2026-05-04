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
