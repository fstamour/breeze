
(defpackage #:breeze.utils
  (:use :cl)
  (:documentation "Utilities")
  (:export
   #:package-apropos
   #:optimal-string-alignment-distance
   #:walk
   #:walk-car
   #:walk-list
   #:indent-string
   #:print-comparison))

(in-package #:breeze.utils)

(defun walk (tree fn  &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on every elements"
  (dolist (node tree)
    (if (listp node)
        (when (funcall recurse-p)
          (walk node fn recurse-p))
        (funcall fn node))))

(defun walk-list (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each list parts"
  (when (listp tree)
    (funcall fn tree)
    (dolist (node tree)
      (when (funcall recurse-p node)
        (walk-list node fn recurse-p)))))

(defun walk-car (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each first elements (cars)"
  (walk-list tree
             #'(lambda (node)
                 (funcall fn (car node)))
             recurse-p))

(defun package-apropos (search-string)
  "Compute a list of package that contains the search-string."
  (remove-if-not #'(lambda (package)
                     (search search-string (package-name package)
                             :test #'string-equal))
                 (list-all-packages)))


(defun optimal-string-alignment-distance (vec-a vec-b)
  "Compute an edit distance between two vector."
  (let* ((m (length vec-a))
         (n (length vec-b))
         (diff-0 (make-array (list (1+ n)) :element-type 'integer))
         (diff-1 (make-array (list (1+ n)) :element-type 'integer))
         (diff-2 (make-array (list (1+ n)) :element-type 'integer)))

    (loop :for i :upto n :do
      (setf (aref diff-1 i) i))
    (setf (aref diff-0 0) 1)

    (flet ((a (index) (aref vec-a (1- index)))
           (b (index) (aref vec-b (1- index)))
           (diff-0 (index) (aref diff-0 index))
           (diff-1 (index) (aref diff-1 index))
           (diff-2 (index) (aref diff-2 index)))
      (loop :for i :from 1 :upto m :do
        (loop :for j :from 1 :upto n
              :for cost = (if (eq (a i) (b j)) 0 1) ;; aka substitution-cost
              :do
                 (setf (aref diff-0 j) (min
                                        (1+ (diff-1 j))          ;; deletion
                                        (1+ (diff-0 (1- j)))     ;; insertion
                                        (+ cost (diff-1 (1- j))) ;; substitution
                                        ))
                 ;; transposition
                 (when (and (< 1 i) (< 1 j)
                            (eq (a i) (b (1- j)))
                            (eq (a (1- i)) (b j)))
                   (setf (aref diff-0 j) (min (diff-0 j)
                                              (+ cost (diff-2 (- j 2)))))))
        (when (/= m i)
          (let ((tmp diff-2))
            (setf diff-2 diff-1
                  diff-1 diff-0
                  diff-0 tmp
                  (aref diff-0 0) (1+ i)))))
      (diff-0 n))))

(defun indent-string (indentation string)
  "Prepend INDENTATION spaces at the beginning of each line in STRING."
  (check-type indentation (integer 0))
  (with-input-from-string (input string)
    (with-output-to-string (output)
      (loop :for line = (read-line input nil nil)
	 :while line
	 :do (format output "~a~a~%" (str:repeat indentation " ") line)))))

#|
(indent-string 4 (format nil "a~%b"))
"    a
    b
"
|#

(defun print-comparison (stream string1 string2)
  "Print two (close) string in a way that the difference are easier to see."
  (let* ((mismatch (mismatch string1 string2)))
    (format stream "~&~a~%~a|~%~a"
	    string1
	    (str:repeat mismatch "=")
	    string2)))

#|
(print-comparison nil "abc" "adc")

(print-comparison nil "abce" "abcd")

(print-comparison nil
		  (string-downcase 'system-files)
		  (string-downcase 'sytsem-files))
"system-files
==|
sytsem-files"
|#
