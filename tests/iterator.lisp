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
  (:import-from #:breeze.iterator
                #:dig
                #:maybe-dig
                #:skip-value-p
                #:dig-value-p))

(in-package #:breeze.test.iterator)

(defun test-iterator (iterator vector
                      &key (pos 0) donep value)
  (is eq vector (vec iterator)
      "The iterator was not intialized with the right vector.")
  (is = pos (pos iterator)
      "The iterator's position was not correctly initialized to ~s." pos)
  (when donep
    (true (donep iterator)))
  (when value
    (is equalp value (value iterator))))

(define-test make-iterator
  (let* ((vector #(1 2 3)))
    (test-iterator (make-vector-iterator vector) vector)
    (test-iterator (make-depth-first-iterator vector) vector)))

(define-test+run donep
  (progn
    (true (donep (make-vector-iterator #())))
    (true (donep (make-vector-iterator #() :position -1)))
    (true (donep (make-vector-iterator #() :position 1)))
    (false (donep (make-vector-iterator #(1))))
    (false (donep (make-vector-iterator #(1 2 3))))
    (true (donep (make-vector-iterator #(1 2 3) :position 10))))
  (progn
    (true (donep (make-depth-first-iterator #())))
    (true (donep (make-depth-first-iterator #() :position -1)))
    (true (donep (make-depth-first-iterator #() :position 1)))
    (false (donep (make-depth-first-iterator #(1))))
    (false (donep (make-depth-first-iterator #(1 2 3))))
    (true (donep (make-depth-first-iterator #(1 2 3) :position 10)))))

(defun skip-odd-values (iterator)
  (oddp (value iterator)))

(define-test+run skipp
  (progn
    (false (skipp (make-vector-iterator #() :skip-value-p #'skip-odd-values)))
    (false (skipp (make-vector-iterator #(2) :skip-value-p #'skip-odd-values)))
    (false (skipp (make-vector-iterator #(1) :skip-value-p #'skip-odd-values)))
    (false (skipp (make-vector-iterator #(1)
                                        :skip-value-p #'skip-odd-values :dont-skip-p nil)))
    (is equal '(2 4) (collect (make-vector-iterator #(1 2 3 4)
                                                    :skip-value-p #'skip-odd-values))))
  #++ ;; TODO
  (progn
    (true (donep (make-depth-first-iterator #())))
    (true (donep (make-depth-first-iterator #() :position -1)))
    (true (donep (make-depth-first-iterator #() :position 1)))
    (false (donep (make-depth-first-iterator #(1))))
    (false (donep (make-depth-first-iterator #(1 2 3))))
    (true (donep (make-depth-first-iterator #(1 2 3) :position 10)))))

(define-test+run dig
  (let* ((vector1 #(1 2 3))
         (vector2 #(a b c d e f))
         (iterator (dig
                    (make-depth-first-iterator vector1)
                    vector2)))
    (test-iterator iterator vector2)))

(defun dig-into-even-length-vector (iterator)
  (let ((x (value iterator)))
    (and (vectorp x)
         (evenp (length x)))))

(define-test+run nested-iteration
  ;; empty case, so the iterator is donep from the start
  (let* ((vector #())
         (iterator (make-depth-first-iterator vector)))
    (test-iterator iterator vector :pos 0 :donep t)
    (parachute:fail (value iterator))
    (next iterator)
    (test-iterator iterator vector :pos 1 :donep t)
    (fail (value iterator)))
  ;; non-empty, no recursive iteration
  (let* ((vector #(1 2 3))
         (iterator (maybe-dig (make-depth-first-iterator vector))))
    (test-iterator iterator vector))
  ;; non-empty, no nested iteration
  (let* ((vector #(1 2 3))
         (iterator (make-depth-first-iterator vector)))
    (test-iterator iterator vector :pos 0 :value 1)
    (next iterator)
    (test-iterator iterator vector :pos 1 :value 2))
  ;; recurse on the first element
  (let* ((vector1 #(1 2))
         (vector2 `#(,vector1))
         (iterator0 (make-depth-first-iterator
                     vector2
                     :dig-value-p #'dig-into-even-length-vector))
         (iterator (next iterator0))
         (root-iterator (parent iterator)))
    (is eq iterator0 iterator)
    (test-iterator iterator vector1 :pos 1)
    (test-iterator root-iterator vector2)
    (is eq (skip-value-p root-iterator) (skip-value-p iterator))
    (is eq (dig-value-p root-iterator) (dig-value-p iterator)))
  ;; recurse on the second element
  (let* ((vector1 #(1 2))
         (vector2 `#(a ,vector1))
         (root-iterator (make-depth-first-iterator
                         vector2
                         :dig-value-p #'dig-into-even-length-vector))
         (iterator (next root-iterator)))
    (isnt eq root-iterator iterator)
    (test-iterator iterator vector1)
    (test-iterator root-iterator vector2 :pos 1)
    (is eq root-iterator (parent iterator))
    (is eq (skip-value-p root-iterator) (skip-value-p iterator))
    (is eq (dig-value-p root-iterator) (dig-value-p iterator))))

(defun flatten (vector &optional skipp)
  #++
  (collect (make-depth-first-iterator
            vector
            :dig-value-p (lambda (iterator)
                           (vectorp (value iterator)))
            :skip-value-p skipp))
  (loop
    :for i :from 0
    :for iterator := (make-depth-first-iterator
                      vector
                      :dig-value-p (lambda (iterator)
                                     (vectorp (value iterator)))
                      :skip-value-p skipp)
      :then (next iterator)
    :until (prog1 (donep iterator)
             ;; (format *debug-io* "~%~%~d: ~S" i iterator)
             )
    :for value = (value iterator)
    ;; :do (format *debug-io* "~&~d: ~S~%~%" i value)
    :collect value))

(define-test+run flatten
  (is equalp '(a b c d e f g)
      (flatten #(a b #(c d #(e f) g))))
  ;; This is a neat example: it flattens the nested vectors _and_ it's
  ;; able to skip one entry out of two.
  (is equalp '(b d f g)
      (flatten #(a b #(c d #(e f) g))
               (lambda (iterator)
                 (labels ((global-position (iterator)
                            (if iterator
                                (+ (pos iterator)
                                   (global-position (parent iterator)))
                                0)))
                   (evenp (global-position iterator)))))))
