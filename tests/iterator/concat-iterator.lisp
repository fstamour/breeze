(in-package #:breeze.test.iterator)

(define-test+run concat-iterator
  ;; Two non-empty children — values are yielded in order.
  (is equal '(1 2 3 a b c)
      (collect
       (make-concat-iterator
        (vector
         (make-vector-iterator #(1 2 3))
         (make-vector-iterator #(a b c))))))
  ;; A leading empty child is skipped.
  (is equal '(a b c)
      (collect
       (make-concat-iterator
        (vector
         (make-vector-iterator #())
         (make-vector-iterator #(a b c))))))
  ;; An intermediate empty child is skipped.
  (is equal '(1 2 a b)
      (collect
       (make-concat-iterator
        (vector
         (make-vector-iterator #(1 2))
         (make-vector-iterator #())
         (make-vector-iterator #(a b))))))
  ;; All children empty — iterator is done immediately.
  (true (donep
         (make-concat-iterator
          (vector
           (make-vector-iterator #())
           (make-vector-iterator #())))))
  ;; Empty list of iterators — done immediately.
  (true (donep (make-concat-iterator #()))))
