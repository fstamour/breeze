
(defpackage #:breeze.configuration
  (:documentation "Breeze's configuration")
  (:nicknames #:breeze.config)
  (:use #:cl)
  (:export
   #:*default-author*
   #:*default-system-licence*
   #:*capture-folder*
   #:*capture-template*
   #:config-file-root
   #:config-file-path
   #:load-config-file))

(in-package #:breeze.configuration)

;;; Configurations

(defvar *default-author* ""
  "The default author when generating asdf system.")

(defvar *default-system-licence* "Public"
  "The default licence when generating asdf system.")

(defvar *capture-folder*
  (merge-pathnames "breeze-capture/" (user-homedir-pathname))
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

(defun config-file-root ()
  "Get the path to the directory that contains breeze's configuration file."
  (uiop:xdg-config-home "breeze/"))

(defun config-file-path ()
  "Get the path to breeze's configuration file."
  (uiop:xdg-config-home "breeze/config.lisp"))

(defun load-config-file ()
  "Load breeze's config file."
  (let ((path (config-file-path)))
    (when (probe-file path)
      ;; TODO perhaps make sure to use the "standard" reader??
      (let ((*package* (find-package '#:cl-user)))
        (load path)
        ;; return T when it loaded correctly
        t))))
