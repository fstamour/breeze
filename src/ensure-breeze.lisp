#|

This file is used to load breeze's system.

It is used, for example, by emacs in breeze.el.

|#

(cl:in-package #:cl-user)


;; TODO "Checkpoints"
;; TODO Unload (e.g. delete-package) if it fails to load!
;; TODO _maybe_ add a variable *breeze-loaded-correctly-p*

(asdf:load-asd
 (merge-pathnames "../breeze.asd" *load-truename*))

;; TODO error handling
;;
;; TODO if quicklisp is not available, check if all dependencies are
;; available before trying to load the whole system
;;
;; TODO _maybe_ fallback to vendored dependency systems if they can't
;; be found
;;
;; TODO some dependencies and subsystems could be make optional, maybe
;; this script could take care of setting up some *features*?

(unless (asdf:component-loaded-p "breeze")
  #+quicklisp
  (ql:quickload "breeze")
  #-quicklisp
  (asdf:load-system '#:breeze)
  (format t "~&Breeze loaded!~%"))
