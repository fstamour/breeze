;;;; Goal: experimenting with macroexpand-hook to see if it could be
;;;; of use (e.g. to detect a `defun`).

(cl:in-package #:common-lisp-user)

(defpackage #:macroexpand
  (:use :cl))

(in-package #:macroexpand)

(describe '*macroexpand-hook*)
#|
Documentation:
The value of this variable must be a designator for a function that can
take three arguments,
- a macro expander function,
- the macro form to be expanded, and
- the lexical environment to expand in.

The function should return the expanded form.
This function is called by MACROEXPAND-1 whenever a *runtime* expansion
is needed.

Initially this is set to FUNCALL.
|#


(defmacro defun* (name (&rest lambda-list) &body body)
  `(progn
     (format t "~&defun* ~a" name)
     (defun ,name ,lambda-list ,@body)))

(defun macroexpand-hook (expander macro-form environment)
  (let ((macroexpanded-form
	  (funcall expander macro-form environment)))
    (format t "~&The macro-form ~%~a~%~%expanded to ~%~a"
	    macro-form
	    macroexpanded-form)))

(setf *macroexpand-hook* #'macroexpand-hook)


(defun* x2 (x)
  (* x 2))

#|
The macro-form
(DEFUN* X2
    (X)
  (* X 2))

expanded to
(PROGN (FORMAT T ~&defun* ~a NAME) (DEFUN X2 (X) (* X 2)))NIL
|#

;;; So I would need a kind of code-walker to find "defuns" in the
;;; macro-expanded code.
;;;
;;; TODO Take a look at cl-walker

(ql:quickload 'cl-walker)

;;; Note: intercepting and analyzing _every_ forms might slow down
;;; stuff...
