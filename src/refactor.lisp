;;;; Ironically, this file is a mess, it's all Work-in-progress™
;;;;

(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use :cl)
  (:import-from
   #:breeze.utils
   #:whitespacep)
  (:export
   #:form-at-point))

(in-package #:breeze.refactor)

#|

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

;; (defun refactor (code filename point action &rest action-args))
|#



;; WIP
(defun breeze-buffer-has-defpackage ()
  "Check if a buffer already contains a defpackage form.")

;; WIP
(defun breeze-in-loop ()
  "Check if it's a valid place to add a loop clause.")

;; TODO A "skipped node" can also be a form hidden behing a feature (#+/-)
(defun emptyp (nodes)
  "Whether a list of node contains no code."
  (every #'(lambda (node)
	     (typep node 'breeze.reader:skipped-node))
	 nodes))

(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
	   (symbol-package-qualified-name command))
  (:method ((command (eql 'insert-defpackage)))
	   "Insert a defpackage form."))


(defun quickfix (&rest all
		       &key
		       buffer-string
		       buffer-name
		       buffer-file-name
		       point
		       point-min
		       point-max)
  (declare (ignorable all buffer-string buffer-name buffer-file-name
		      point point-min point-max))
  (let* (;; Parse the buffer
	 (nodes (parse-string buffer-string))
	 ;; Find the top-level form "at point"
	 (current-top-level-node
	  (find-if #'(lambda (node)
		       (destructuring-bind (start . end)
			   (node-source node)
			 (and
			  (<= start point end)
			  (< point end))))
		   nodes))
	 ;; Accumulate a list of commands that make sense to run in
	 ;; the current context
	 (commands
	  (uiop:while-collecting
	   (push-command)
	   ;; Suggest to insert a "defpackage" when the buffer is "empty".
	   (when (emptyp nodes) (push-command 'insert-defpackage))
	   ;; Suggest
	   (when (or (null current-top-level-node)
		     (typep current-top-level-node
			    'breeze.reader:skipped-node))
	     (push-command 'insert-defun))
	   )))
    ;; (format t "~&Current node: ~a" current-top-level-node)
    ;; (format t "~&Is current node a defpackage ~a" (defpackage-node-p current-top-level-node))
    ;; (format t "~&positions ~a" (mapcar #'node-source nodes))
    ;; (format t "~&nodes ~a" nodes)
    (mapcar #'(lambda (command-name)
		(describe-command command-name))
	    (remove-duplicates commands))))

#+nil
(quickfix :buffer-string "   " :point 3)
					;
