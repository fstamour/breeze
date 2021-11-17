
(in-package #:common-lisp-user)

(defpackage #:breeze.quickfix
  (:use :cl)
  (:export #:quickfix)
  (:import-from #:breeze.refactor
		#:form-at-point)
  (:import-from #:breeze.reader
		#:parse-string
		#:node-source
		#:defpackage-node-p))

(in-package #:breeze.quickfix)

;; TODO A "skipped node" can also be a form hidden behing a feature (#+/-)
(defun emptyp (nodes)
  "Whether a list of node contains no code."
  (every #'(lambda (node)
	     (typep node 'breeze.reader:skipped-node))
	 nodes))


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
  (let* ((nodes (parse-string buffer-string))
	 (current-top-level-node
	  (find-if #'(lambda (node)
		       (destructuring-bind (start . end)
			   (node-source node)
			 (< start point end)))
		   nodes))
	 (commands
	  (uiop:while-collecting
	   (push-command)
	   ;; Suggest to insert a "defpackage" when the buffer is "empty".
	   (when (emptyp nodes) (push-command 'breeze-insert-defpackage))

	   )))
    (format t "~&Current node: ~a" current-top-level-node)
    (format t "~&Is current node a defpackage ~a" (defpackage-node-p current-top-level-node))

    ;; (format t "~&positions ~a" (mapcar #'node-source nodes))
    ;; (format t "~&nodes ~a" nodes)
    (mapcar #'symbol-name commands)))
