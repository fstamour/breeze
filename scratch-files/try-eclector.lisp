;; 13 april 2020 - Trying out eclector: a portable and extensible
;; common lisp reader https://github.com/s-expressionists/Eclector

(in-package :breeze.user)

(ql:quickload '(#:eclector))

(defun read-forms (stream)
  "Read all forms from a stream."
  (let ((end-of-file (gensym "eof")))
      (loop :for form = (read stream nil end-of-file)
	 :while (not (eq form end-of-file))
	 :collect form)))

(defun read-file (pathname )
  "Read a file, form by form"
  (with-open-file (stream pathname)
    (read-forms stream)))

#+nil
(with-open-file (stream "examples/function-redifinition.lisp")
  (let ((end-of-file (gensym "eof")))
    (loop :for form = (eclector.reader:read stream nil end-of-file)
       :while (not (eq form end-of-file))
	 :collect form)))

(defun relative-pathname (pathname)
  (if (cl-fad:pathname-relative-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
      pathname))

(defun form-in-package-p (form)
  "Is the form an \"in-package\" form?
If it is return the package-designator"
  (and (listp form)
       (eq 'cl:in-package (first form))
       (second form)))

(defun form-function-p (form)
  "Is the form a function?"
  (and (listp form)
       (eq 'cl:defun (first form))))


(defparameter *examples* (make-hash-table :test 'equal))

;; load examples
(loop :for (_ name . rest) :in
     (read-file (relative-pathname "examples/function-redefinition.lisp"))
   :do (setf (gethash name *examples*) rest))

(alexandria:hash-table-plist *examples*)

("Add a function"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Doubles x"
      (+ X X))
    (DEFUN |3X|
        (X)
      "Multiply x by 3"
      (* 2 X)))))
 "Change implementation and documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Multiply x by 2"
      (* 2 X)))))
 "Change documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Adds x to itself"
      (+ X X)))))
 "Add documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Doubles x"
      (+ X X)))))
 "Change implementation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      (* 2 X))))))
