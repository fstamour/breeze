;;; Common lisp side of breeze.el

(in-package #:common-lisp-user)

(defpackage #:breeze/el
  (:use :cl)
  (:export
   #:make-project
   #:get-ql-local-project-directories ))

(in-package #:breeze/el)

(defun insert-at (current-text text-to-insert position)
  (with-output-to-string (stream)
    (princ (subseq current-text 0 position) stream)
    (format stream text-to-insert)
    (princ (subseq current-text position) stream)))


(defun test-insert-at (pre post insert)
  (insert-at (concatenate 'string pre post)
	     insert
	     (length pre)))

(equal
 (test-insert-at "(defun f1 ()" ")"
		 "~&(let (()))")
 (test-insert-at 
  "(defun f1 ()
" ")"
  "~&(let (()))"))


(defun insert-let (string position)
  (insert-at string
	     "~&(let (()))"
	     position))




(ql:quickload "quickproject")

(defun make-project (&rest args)
  ;; make-project pathname &key depends-on author include-copyright license name template-directory template-parameters => project-name 
  (apply #'quickproject:make-project args))

(defun get-ql-local-project-directories ()
  (mapcar #'namestring
	  ql:*local-project-directories*))

