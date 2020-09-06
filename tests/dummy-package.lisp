(in-package #:common-lisp-user)

(defpackage #:breeze.dummy.test
  (:use :cl)
  (:nicknames :dum)
  (:export
   ;; Documented symbols
   #:*bound-variable-documented*
   #:*unbound-variable-documented*
   #:class-documented
   #:function-documented
   #:macro-documented
   #:slot-documented

   ;; Undocumented symbols
   #:*bound-variable-undocumented*
   #:*unbound-variable-undocumented*
   #:class-undocumented
   #:function-undocumented
   #:macro-undocumented
   #:slot-undocumented
   ))

(in-package #:breeze.dummy.test)


;;; Documented symbols

(defvar *unbound-variable-documented* ()
  "A documented unbound symbol.")
(defvar *bound-variable-documented* t
  "A documented bound symbol.")

(defun function-documented ()
  "A documented function.")

(defgeneric generic-function-documented ()
  (:documentation "A documented generic function."))

(defmacro macro-documented ()
  "A documented macro.")

(defclass class-documented ()
  ((slot-documented
    :accessor slot-documented
    :documentation "A documented slot."))
  (:documentation "A documented class."))


;;; Undocumented symbols

(defvar *unbound-variable-undocumented* ())
(defvar *bound-variable-undocumented* t)
(defun function-undocumented ())
(defgeneric generic-function-undocumented ())
(defmacro macro-documented ())
(defclass class-undocumented ()
  ((slot-undocumented
    :accessor slot-undocumented)))
