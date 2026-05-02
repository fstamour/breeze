(defpackage #:breeze.test-helpers
  (:documentation "Utilities for testing breeze.")
  (:use #:cl)
  (:import-from #:breeze.parser
                #:goto-position
                #:make-node-iterator)
  (:export
   #:iterator-at))

(in-package #:breeze.test-helpers)

(defun iterator-at (string &key at)
  "Return a node-iterator for STRING positioned at the token matching
SEARCH-STRING.  Useful for simulating 'cursor inside a form'."
  (let ((iterator (make-node-iterator string)))
    (etypecase at
      (string (goto-position iterator (search at string)))
      (integer (goto-position iterator at))
      (null iterator))))
