
(defpackage #:breeze.definition
  (:documentation "Provides replacements for \"definition forms\" (such as defun and defmacro).
The goal is to (portably) make sure we keep the definitions and not just their [compiled] results.")
  (:use :cl)
  (:shadow cl:defun cl:fmakunbound)
  (:export
   #:*function*
   #:*function-redifinition-hooks*
   #:defun
   #:fmakunbound
   #:function-body))

(in-package #:breeze.definition)

(defvar *function* (make-hash-table)
  "Set of all functions defined with breeze.definition:defun")

(defvar *function-redifinition-hooks* ()
  "List of functions to call when a function is redefined")

(cl:defun flag-funtion-redifinition (name)
  "Calls each function in *funtion-redifinition-hooks*."
  (loop :for hook :in *function-redifinition-hooks*
        :do (funcall hook name)))

(defmacro defun (&whole whole name lambda-list &body body)
  "Define a functions and saves its definition in memory, flag a function redifinition."
  `(progn (cl:defun ,name ,lambda-list
            ,@body)
          (setf (gethash ',name *function*) ',whole)
          (flag-funtion-redifinition ',name)
          ',name))

(cl:defun fmakunbound (name)
  "Make NAME have no global function definition."
  (cl:fmakunbound name)
  (remhash name *function*))

(cl:defun function-body (name)
  "Get the body of a function by name"
  (fourth (gethash name *function*)))

;; TODO defmacro
;; TODO defgeneric
;; TODO defmethod
;; TODO defclass
;; TODO what about closures?

#|
(loop :for symbol :being :the :external-symbol :of :cl
:when (alexandria:starts-with-subseq "DEF" (symbol-name symbol))
:collect symbol)

(DEFINE-SYMBOL-MACRO
DEFCLASS
DEFUN
DEFSETF
DEFMETHOD
DEFCONSTANT
DEFINE-METHOD-COMBINATION
DEFSTRUCT
DEFVAR
DEFGENERIC
DEFTYPE
DEFMACRO
DEFPARAMETER
DEFPACKAGE
DEFINE-COMPILER-MACRO
DEFINE-MODIFY-MACRO
DEFINE-CONDITION
DEFINE-SETF-EXPANDER)
|#
