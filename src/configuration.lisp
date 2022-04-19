;;; TODO This should come with its own system, so that people can
;;; configure breeze in their init file, without loading the whole
;;; breeze system.

(defpackage #:breeze.configuration
  (:documentation "Breeze's configuration")
  (:nicknames #:breeze.config)
  (:use #:cl)
  (:export
   #:*default-author*
   #:*default-system-licence*
   #:*capture-folder*))

(in-package #:breeze.configuration)

;;; Configurations

(defparameter *default-author* ""
  "The default author when generating asdf system.")

(defparameter *default-system-licence* "Public"
  "The default licence when generating asdf system.")

(defparameter *capture-folder* "~/breeze-capture"
  "The folder where to save scratch files.")
