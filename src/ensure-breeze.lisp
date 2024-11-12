#|

This file is used to load breeze's system.

It is used, for example, by emacs in breeze.el.

TODO "Checkpoints"
TODO Unload (e.g. delete-package) if it fails to load!
TODO _maybe_ add a variable *breeze-loaded-correctly-p*

|#

(cl:in-package #:cl-user)

(defpackage #:breeze-loader
  (:use #:cl))

(in-package #:breeze-loader)

(defun or-die (error callback)
  (let (success)
    (multiple-value-bind (result condition)
        (ignore-errors
         (prog1 (funcall callback)
           (setq success t)))
      (unless (and success (not condition))
        (error (format nil "~A: ~A" error condition)))
      result)))

(or-die "Failed to load asdf."
        (lambda () (require 'asdf)))

(or-die "Failed to set the path to the system definition."
  (lambda ()
    (defparameter *asd*
      (merge-pathnames "../breeze.asd" *load-truename*)))) ; this line is dynamically replaced in breeze.el's breeze-%loader

(or-die "Failed to load the system definition"
  (lambda () (asdf:load-asd *asd*)))

;; TODO error handling
;;
;; TODO if quicklisp is not available, check if all dependencies are
;; available before trying to load the whole system
;;
;; TODO _maybe_ fallback to vendored dependency systems if they can't
;; be found
;;
;; TODO some dependencies and subsystems could be made optional, maybe
;; this script could take care of setting up some *features*?


(if (asdf:component-loaded-p "breeze")
    "Already loaded"
    (or-die "Failed to load breeze's system."
            (lambda ()
              (prog1
                  #+quicklisp
                (prog1 "Loaded using quicklisp"
                  (ql:quickload "breeze"))
                #-quicklisp
                (prog1
                    "Loaded using asdf:load-system"
                  (asdf:load-system '#:breeze))
                (format t "~&Breeze loaded!~%")))))
