(in-package #:common-lisp-user)

(defpackage #:breeze.dummy.test
  (:use :cl)
  (:nicknames :dum)
  (:export
   ;; Documented symbols
   #:function-documented
   #:*unbound-variable-documented*
   #:*bound-variable-documented*
   #:class-documented
   #:slot-documented

   ;; Undocumented symbols
   #:function-undocumented
   #:*unbound-variable-undocumented*
   #:*bound-variable-undocumented*
   #:class-undocumented
   #:slot-undocumented
   ))

(in-package #:breeze.dummy.test)

(defvar *unbound-variable-documented* ()
  "A documented unbound symbol.")
(defvar *bound-variable-documented* t
  "A documented bound symbol")

(defun function-documented ())
(defgeneric generic-function-documented ())

(defclass class-documented ()
  ((slot-documented
    :accessor slot-documented)))

(defvar *unbound-variable-undocumented* ())
(defvar *bound-variable-undocumented* t)
(defun function-undocumented ())
(defgeneric generic-function-undocumented ())

(defclass class-undocumented ()
  ((slot-undocumented
    :accessor slot-undocumented)))
