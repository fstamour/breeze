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
   #:*capture-folder*
   #:*capture-template*))

(in-package #:breeze.configuration)

;;; Configurations

(defvar *default-author* ""
  "The default author when generating asdf system.")

(defvar *default-system-licence* "Public"
  "The default licence when generating asdf system.")

(defvar *capture-folder* "~/breeze-capture"
  "The folder where to save capture files.")

;; TODO Load from <breeze>/data/default-capture-template.lisp
(defvar *capture-template*

  "(ql:quickload '(alexandria))

  ;; make it easier to debug
  (declaim (optimize (speed 0) (safety 3) (debug 3)))

  #|

  Goal:

  Motivation:

  What am I going to try first:

  |#

  "
  "The format string used to populate a capture file when first creating it.")
