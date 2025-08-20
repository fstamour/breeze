#|

Playing around with the idea of replacing spinneret because it's pretty
slow to load.

|#

(defpackage #:breeze.html
  (:documentation "")
  (:use #:cl))

(in-package #:breeze.html)

(defclass tag ()
  ((name :initform nil
         :initarg :name
         :accessor name)
   (attributes :initform nil
               :initarg :attributes
               :accessor attributes)
   (body :initform nil
         :initarg :body
         :accessor body)))

(dolist (sym
         '(br
           a
           p
           div span
           pre
           html
           link
           ol li
           h1 h2 h3 h4 h5 h6))
  (setf (symbol-value sym) sym)
  (export sym))

(defmethod make-tag (name attributes body)
  (values name attributes body))

(defun tag (name &rest attributes-and-body)
  (let ((attributes (butlast attributes-and-body))
        (body (car (last attributes-and-body))))
    (multiple-value-bind (name attributes body)
        (make-tag name attributes body))
    (make-instance 'tag
                   :name name
                   :attributes attributes
                   :body body)))

(tag br)
