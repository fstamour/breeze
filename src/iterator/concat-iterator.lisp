(in-package #:breeze.iterator)

;;; Concat iterator
;;
;; TODO maybe find a better name
;; TODO add documentation
;; TODO add tests
;;
;; Walks a vector of iterators, yielding each child's values in
;; sequence. Empty (already-done) children are skipped automatically.

(defclass concat-iterator (vector-iterator) ()
  (:documentation "Concatenates the values of a vector of iterators."))

(defun %skip-done-children (iterator)
  (with-slots (vector position) iterator
    (loop :while (and (< position (length vector))
                      (donep (aref vector position)))
          :do (incf position))))

(defun make-concat-iterator (iterators)
  (let ((it (make-instance 'concat-iterator
                           :vector (coerce iterators 'vector))))
    (%skip-done-children it)
    it))

(defmethod child-iterator ((iterator concat-iterator))
  (with-slots (vector position) iterator
    (aref vector position)))

(defmethod next ((iterator concat-iterator))
  (next (child-iterator iterator))
  (%skip-done-children iterator))

(defmethod value ((iterator concat-iterator))
  (value (child-iterator iterator)))
