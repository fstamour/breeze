
(defpackage #:breeze.definition
  (:use :cl)
  (:shadow cl:defun cl:fmakunbound)
  (:export
   #:*function*
   #:*function-change-hooks*
   #:defun
   #:fmakunbound
   #:function-body))

(in-package #:breeze.definition)

(defvar *function* (make-hash-table)
  "Set of all functions defined with breeze.definition:defun")

(defvar *function-change-hooks* ()
  "List of functions to call when a function is redefined")

(cl:defun flag-funtion-change (name)
  (loop :for hook :in *function-change-hooks*
        :do (funcall hook name)))

(defmacro defun (&whole whole name lambda-list &body body)
  `(progn (cl:defun ,name ,lambda-list
            ,@body)
          (setf (gethash ',name *function*) ',whole)
          (flag-funtion-change ',name)
          ',name))

(cl:defun fmakunbound (name)
  "Make NAME have no global function definition."
  (cl:fmakunbound name)
  (remhash name *function*))

(cl:defun function-body (name)
  "Get the body of a function by name"
  (fourth (gethash name *function*)))


