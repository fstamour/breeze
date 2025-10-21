(defpackage #:breeze.range
  (:documentation "A simple class that holds a start and end points,")
  (:use #:cl #:breeze.generics)
  (:export #:range #:range= #:start #:end))

(in-package #:breeze.range)

(defclass range ()
  ((start
    :initarg :start
    :initform 0
    :accessor start)
   (end
    :initarg :end
    :initform nil
    :accessor end)))

(defun range (start end)
  (make-instance 'range :start start :end end))

(defmethod print-object ((range range) stream)
  (print-unreadable-object
      (range stream :type t :identity nil)
    (format stream "~s-~s" (start range) (end range))))

(declaim (inline range=))
(defun range= (a b)
  (and (= (start a) (start b))
       (= (end a) (end b))))

(defmethod eqv ((a range) (b range))
  (range= a b))
