(defpackage #:breeze.command-utils
  (:documentation "Utilities to write commands")
  (:use #:cl #:breeze.command #:breeze.analysis))

(in-package #:breeze.command-utils)

(defun node-at-point ())

;; TODO This should go in a file for "commands that uses parse trees"
(defun pulse-node (node)
  (pulse (start node) (end node)))

(defun current-node (&key pulsep)
  (alexandria:when-let*
      ((buffer (current-buffer))
       (node-iterator (node-iterator buffer)))
    (when pulsep (pulse-node node-iterator))
    node-iterator))
