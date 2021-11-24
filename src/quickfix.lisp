
(in-package #:common-lisp-user)

(defpackage #:breeze.quickfix
  (:use :cl)
  (:export
   #:quickfix
   #:describe-command)
  (:import-from #:breeze.utils
		#:positivep
		#:symbol-package-qualified-name)
  (:import-from #:breeze.refactor
		#:form-at-point)
  (:import-from #:breeze.reader
		#:parse-string
		#:node-source
		#:defpackage-node-p))

(in-package #:breeze.quickfix)

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
