;; 13 april 2020 - Trying out eclector: a portable and extensible
;; common lisp reader https://github.com/s-expressionists/Eclector

(in-package :breeze.user)

(ql:quickload '(#:eclector))

;; Read a file, form by form
(defun read-file (pathname )
  (with-open-file (stream pathname)
    (let ((end-of-file (gensym "eof")))
      (loop :for form = (read stream nil end-of-file)
	 :while (not (eq form end-of-file))
	 :collect form))))

#+nil
(with-open-file (stream "examples/function-redifinition.lisp")
  (let ((end-of-file (gensym "eof")))
    (loop :for form = (eclector.reader:read stream nil end-of-file)
       :while (not (eq form end-of-file))
	 :collect form)))

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
(loop :for (nil name nil before nil after) :in
     (read-file "examples/function-redifinition.lisp")
   :do (setf (gethash name *examples*) (list before after)))

(alexandria:hash-table-plist *examples*)
