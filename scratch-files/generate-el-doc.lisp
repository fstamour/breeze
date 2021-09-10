(in-package #:common-lisp-user)

(defpackage #:generate-el-doc
  (:use :cl))

(in-package #:generate-el-doc)

(defun read-breeze.el ()
  (let ((eof (gensym)))
    (alexandria:with-input-from-file
     (input
      (merge-pathnames "src/breeze.el"
		       (breeze.asdf:system-directory 'breeze)))
     (loop for form = (read input nil eof)
	   until (eq form eof)
	   collect form))))

(let ((forms ))
  (loop for form in (read-breeze.el)
	when (and (listp form)
		  (eq 'defun (car form)))
	collect (second form)))
