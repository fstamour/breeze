;;; TODO This should come with its own system, so that people can
;;; configure breeze in their init file, without loading the whole
;;; breeze system.

(defpackage #:breeze.configuration
  (:documentation "Breeze's configuration")
  (:use #:cl))

(in-package #:breeze.configuration)

;;; Configurations

(defparameter *breeze-default-author* ""
  "The default author when generating asdf system.")

(defparameter *breeze-default-system-author* "Public"
  "The default licence when generating asdf system.")

(defparameter *breeze-capture-folder* "~/breeze-capture"
  "The folder where to save scratch files.")


;; TODO there are some configuration in refactor.lisp
