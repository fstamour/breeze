
(defpackage #:breeze.fsa
  (:documentation "")
  (:use #:cl))

(in-package #:breeze.fsa)

;; field = commutativity, associativity and distribution of the 2 operations...

(let* ((classes #(+ a b))
       (initial-states #(0))
       (occurences '((a . 2) (b . 1)))
       (input #(+ a b a))
       (transitions '((0 . (1 2))
                      (1 . (1 2))
                      (2 . (1 2)))))
  (loop
    :with counts = (alexandria:alist-hash-table occurences)
    :for state = (aref initial-states 0)
      :then (cdr (assoc class transitions))
    :for guard :below 100
    :for item :across input
    :for class = (position item classes)
    :do
       (unless (if (listp state)
                   (find class state)
                   (= class state))
         (return nil))
       (alexandria:when-let ((count (gethash item counts)))
         (when (zerop count)
           (return (values nil (format nil "Too many ~s's" item))))
         (decf (gethash item counts)))
    :do (print (list item state (alexandria:hash-table-alist counts)))
    :finally (progn
               (maphash (lambda (k v)
                          (when (plusp v)
                            (return (values nil (format nil "Not enough ~s's" k)))))
                        counts)
               (return t))))


(flet ((h (x)
         (loop :for item :across x
               :sum (sxhash item))))
  (apply #'=
         (mapcar #'h
                 '(#(+ a b a)
                   #(+ a a b)
                   #(+ b a a)))))
