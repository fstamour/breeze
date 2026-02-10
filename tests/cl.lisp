
(defpackage #:breeze.test.cl
  (:documentation "Tests for breeze.cl")
  (:use #:cl)
  (:import-from #:breeze.cl
                #:*groups*)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.cl)

(defun collect-symbols (tree)
  "Collect all symbols from a tree."
  (let (symbols)
    (labels ((walk (node)
               (cond
                 ((symbolp node)
                  (pushnew node symbols))
                 ((listp node)
                  (mapc #'walk node)))))
      (mapc #'walk tree))
    symbols))

(defvar *all-symbols* (remove-duplicates (collect-symbols *groups*)))

(define-test+run *groups*
  ;; check for missing functions or macros
  (let (missing)
    (do-external-symbols (s :cl)
      (when (ignore-errors (symbol-function s))
        (unless (member s *all-symbols*)
          (push s missing))))
    (is equal '() missing
        "~d CL symbols missing from *groups*:~%~{ - ~s~%~}"
        (length missing) missing))
  ;; make sure we only include symbols from the cl package
  (let ((non-cl (remove-if (lambda (s)
                             (eq (symbol-package s)
                                 (find-package :cl)))
                           *all-symbols*)))
    (is equal '() non-cl
        "~d symbols not in CL package:~%~{ - ~s~%~}"
        (length non-cl) non-cl)))
