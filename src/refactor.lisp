(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use :cl)
  (:import-from
   #:breeze.utils
   #:whitespacep)
  (:export
   #:form-at-point))

(in-package #:breeze.refactor)


;;; Low-level utility functions


(defun insert-at (current-text text-to-insert position)
  "Insert TEXT-TO-INSERT in CURRENT-TEXT at POSITION. Return a new string."
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
  "Insert a let form at POSITION in STRING."
  (insert-at string
	     "~&(let (()))"
	     position))

;; TODO invert: (not X) <=> X
;; TODO invert: when <=> unless

(defun next-non-whitespace (string &optional (point 0))
  (loop :for i :from point :upto (length string)
	:for c = (aref string i)
	:while (whitespacep c)
	:finally (return i)))

;; (next-non-whitespace "  3") => 2

(defun form-at-point (string point)
  (let ((eof (gensym "eof"))
	(start (next-non-whitespace string point)))
    ;; (multiple-value-bind )
;;; TODO <============================
    (read-from-string string t eof :start start :preserve-whitespace t)))

#+(or)
(form-at-point
 "

(let ((x 2))
  (evenp x)) ; comment1

;; comment2
(trepri)
"

 14)

;; TODO Comment form
;; TODO Wrap with multiple-value-bind
;;      TODO [HARD] Infer how many values are returned (e.g. from standard functions)


;;; Higher-level interface

(defun refactor (code filename point action &rest action-args)
  )
