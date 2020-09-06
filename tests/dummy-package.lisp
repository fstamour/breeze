(in-package #:common-lisp-user)

(defpackage #:breeze.dummy.test
  (:use :cl)
  (:nicknames :dum)
  (:export
   ;; Documented symbols
   #:*bound-variable*
   #:*unbound-variable*
   #:a-class
   #:a-function
   #:a-generic-function
   #:a-macro
   #:slot

   ;; Undocumented symbols
   #:*bound-variable-undocumented*
   #:*unbound-variable-undocumented*
   #:class-undocumented
   #:function-undocumented
   #:generic-function-undocumented
   #:macro-undocumented
   #:slot-undocumented

   #:another-generic-function
   ))

(in-package #:breeze.dummy.test)


;;; Documented symbols

(defvar *unbound-variable* ()
  "A documented unbound symbol.")

(defvar *bound-variable* t
  "A documented bound symbol.")

(defun a-function ()
  "A documented function."
  t)

(defgeneric a-generic-function ()
  (:documentation "A documented generic function."))

(defmethod a-generic-function ()
  "A method."
  t)

(defmacro a-macro ()
  "A documented macro."
  t)

(defclass a-class ()
  ((slot
    :accessor slot
    :documentation "A documented slot."))
  (:documentation "A documented class."))


;;; Undocumented symbols

(defvar *unbound-variable-undocumented* ())
(defvar *bound-variable-undocumented* t)
(defun function-undocumented ())
(defgeneric generic-function-undocumented ())
(defmethod generic-function-undocumented ())
(defmacro macro-documented ())
(defclass class-undocumented ()
  ((slot-undocumented
    :accessor slot-undocumented)))


;;; Other cases

(defgeneric another-generic-function (x))
(defmethod another-generic-function ((x (eql '1))))
(defmethod another-generic-function ((x (eql '2)))
  "documented" t)
