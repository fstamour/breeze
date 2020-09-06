(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use :cl))

(in-package #:breeze.refactor)

(defun insert-at (current-text text-to-insert position)
  (with-output-to-string (stream)
    (princ (subseq current-text 0 position) stream)
    (format stream text-to-insert)
    (princ (subseq current-text position) stream)))

(defun test-insert-at (pre post insert)
  (insert-at (concatenate 'string pre post)
	     insert
	     (length pre)))

#+nil
(equal
 (test-insert-at "(defun f1 ()" ")"
		 "~&(let (()))")
 (test-insert-at
  "(defun f1 ()
" ")"
  "~&(let (()))"))

(defun insert-let (string position)
  (insert-at string
	     "~&(let (()))"
	     position))

;; TODO invert: (not X) <=> X
;; TODO invert: when <=> unless

