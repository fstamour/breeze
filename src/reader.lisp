(in-package #:common-lisp-user)

(defpackage #:breeze.reader
  (:use :cl)
  (:import-from #:breeze.utils
		#:read-stream-range
		#:stream-size
		#:positivep)
  (:export
   ;; Syntax tree
   #:node
   #:skipped-node
   #:symbol-node
   #:read-eval-node
   #:character-node
   #:list-node
   #:function-node

   #:node-content
   #:node-prefix
   #:node-source
   #:node-raw

   #:parse-stream
   #:parse-string
   #:unparse-to-stream
   #:unparse-to-string

   #:defpackage-node-p)
  (:shadow #:read-from-string))

(in-package #:breeze.reader)

;; TODO Use log4cl...
(defparameter *debug-print* nil)
;; (setf *debug-print* t)

(defun dbg (control-string &rest args)
  (when *debug-print*
    (apply #' format *debug-io* control-string args)))


;;; Syntax tree data structure

(defclass node ()
  ((content
    :initform nil
    :initarg :content
    :accessor node-content)
   (prefix
    :initform nil
    :initarg :prefix
    :accessor node-prefix)
   (source
    :initform nil
    :initarg :source
    :accessor node-source)
   (raw
    :initform nil
    :initarg :raw
    :accessor node-raw))
  (:documentation "Base class for the parse-results (syntax tree)."))

(defclass skipped-node (node)
  ()
  (:documentation "Syntax node for skipped content."))

(defclass symbol-node (node)
  ()
  (:documentation "Syntax node for symbols."))

(defclass read-eval-node (node)
  ()
  (:documentation "Syntax node for #. (read-eval)."))

(defclass character-node (node)
  ((char
    :initform nil
    :initarg :char
    :accessor node-char))
  (:documentation "Syntax node for #\\ (character literals)."))

(defclass list-node (node)
  ()
  (:documentation "Syntax node for lists."))

(defclass function-node (node)
  ()
  (:documentation "Syntax node for #'expression."))

(defgeneric non-terminal-p (node)
  (:documentation "Can a node contain other nodes.")
  (:method ((node node)) (listp (node-content node)))
  (:method ((node skipped-node)) nil)
  (:method ((node symbol-node)) nil)
  (:method ((node character-node)) nil))

(defgeneric terminalp (node)
  (:documentation "Can a node contain other nodes.") (:method ((node node)) (not (non-terminal-p node))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (if (node-prefix node)
	(format stream "~s ~s"
		(node-prefix node)
		(node-content node)))
    (format stream "~s"
	    (node-content node))))

#+nil
(defmethod print-object ((node reader-macro-node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (if (node-prefix node)
	(format stream "~s ~c ~s"
		(node-prefix node)
		(node-char node)
		(node-content node)))
    (format stream "~c ~s"
	    (node-char node)
	    (node-content node))))

(defmethod print-object ((node symbol-node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (if (node-prefix node)
	(format stream "~s ~s"
		(node-prefix node)
		(or (node-raw node) (node-content node)))
	(format stream "~s"
		(or (node-raw node) (node-content node))))))

(defmethod print-object ((node skipped-node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (format stream "~s"
	    (node-raw node))))


;;; Parser "client"

;; Define a class representing the "parse result client"
(defclass breeze-client (eclector.parse-result:parse-result-client)
  ()
  (:documentation
   "Controls how the reader construct the parse-results."))


(defmethod eclector.parse-result:make-expression-result
    ((client breeze-client) (result t) (children t) (source t))
  "Create an expression result"
  (dbg "~&result: ~s source: ~s" result source)
  (cond
    (;; If result is a node, populate it's source and content.
     (typep result 'node)
     (setf (node-source result) source)
     (when children
       (setf (node-content result) children))
     result)
    (;; If result is a symbol, make a symbol-node
     (symbolp result)
     (make-instance 'symbol-node
		    :content result
		    :source source))
    #+nil (;; TODO Might not need this
	   (and (listp result)
		(member (car result) +mapping+ :key #'car))
	   (make-instance (cdar (member (car result) +mapping+
					:key #'car))
			  :content children ;; result
			  :source source))
    (;; Else, make a generic node
     t
     (make-instance 'node
		    :content (or children result)
		    :source source))))

;; Create a "make skipped input result" method for our custom client
(defmethod eclector.parse-result:make-skipped-input-result
    ((client breeze-client) (stream t) (reason t) (source t))
  "Create a skipped-node parse result."
  (make-instance 'skipped-node
		 :content (read-stream-range stream
					     (car source)
					     (cdr source))
		 :source source))


(defmethod eclector.reader:evaluate-expression ((client breeze-client)
						expression)
  "Create a syntax node for #. ."
  (make-instance 'read-eval-node
		 :content expression))


(defmethod eclector.reader:find-character ((client breeze-client)
					   designator)
  "Create a syntac node for #\\ ."
  (make-instance 'character-node
		 :content (format nil "#\\~a" designator)
		 :char
		 (call-next-method)))

(defmethod eclector.reader:call-reader-macro ((client breeze-client)
					      input-stream char
					      readtable)
  "Create a syntax node for lists."
  (case char
    (#\(
     (make-instance 'list-node
		    :content (call-next-method)))
    (t
     (call-next-method))))

(defmethod eclector.reader:wrap-in-function ((client breeze-client)
					     name)
  "Create a syntax node for #' ."
  (make-instance 'function-node :content name))

(defun maybe-source (form eof)
  (and
   form
   (not (eq eof form))
   (node-source form)))

(defun read-from-string (string &optional (eof-error-p t)
				eof-value
				&key
				(start 0)
				end
				preserve-whitespace)
  (eclector.parse-result:read-from-string
   (make-instance 'breeze-client)
   string
   eof-error-p
   eof-value
   :start start
   :end end
   :preserve-whitespace preserve-whitespace))


(defun read-all-forms (stream)
  (let ((eof (gensym "eof")))
    (loop for form =
	  (eclector.parse-result:read-preserving-whitespace
	   (make-instance 'breeze-client)
	   stream
	   nil
	   eof)
	  until (eq eof form)
	  collect form)))

;; end-at is not used, its purpose is to help find trailing characters
;; but I haven't implemented that because I'm not sure it's the way to go.
(defun post-process-nodes! (stream forms &optional (start-at 0) end-at)
  "Update each nodes in FORMS to include their prefix and raw (extracted from STREAM)."
  (declare (ignore end-at))
  (let ((previous nil))
    (loop
      for form in forms
      for start = start-at then (cdr (node-source previous))
      for end = (car (node-source form))
      for prefix = (unless (zerop (- end start))
		     (read-stream-range stream start end))
      for raw = (destructuring-bind (from . to)
		    (node-source form)
		  (read-stream-range stream from to))
      do
	 (dbg "~&node: ~a raw: ~s" form raw)
	 ;; update FORM
	 (setf
	  ;; Add the prefix
	  (node-prefix form) prefix
	  ;; Add the raw
	  (node-raw form) raw)
      #+nil
       (cond
	 ((string= "#'" )))
       ;; recurse
       (unless (terminalp form)
	 (post-process-nodes! stream (node-content form)
			      (car (node-source form))
			      (cdr (node-source form))))
       ;; update loop variables
       (setf previous form))))

(defun get-tail (stream forms &optional (end (stream-size stream)))
  "Given a list of forms, extract any trailing characters that were ignored."
  (let* ((tail (alexandria:lastcar forms))
	 (tail-end (if tail
		       (cdr (node-source tail))
		       0)))
    (when (positivep (- end tail-end))
      (make-instance 'skipped-node
		     :content (read-stream-range stream tail-end end)
		     :source (cons tail-end end)))))

(defun parse (stream)
  "Read STREAM entirely using breeze's reader."
  (let ((forms (read-all-forms stream)))
    (post-process-nodes! stream forms)
    `(,@forms
      ,@(alexandria:if-let ((tail (get-tail stream forms)))
	  (list tail)))))

(defun parse-string (string)
  "Read STRING entirely using breeze's reader."
  (with-input-from-string
      (stream string)
    (parse stream)))


(defun unparse-to-stream (stream nodes)
  "Print a list of NODES into STREAM."
  (let ((*print-case* :downcase)
	(*print-circle* t))
    (unparse-to-stream% stream nodes)))

(defun unparse-to-stream% (stream nodes)
  "Print a list of NODES into STREAM (implementation)."
  (dolist (node nodes)
    (alexandria:if-let
	((prefix (node-prefix node)))
      (unless (string= "(" prefix)
	(write-string prefix stream)))
    (unparse-node stream node)))

(defun unparse-to-string (nodes)
  "Print a list of NODES as a STRING."
  (with-output-to-string
      (stream)
    (unparse-to-stream stream nodes)))

(defgeneric unparse-node (stream node)
  (:documentation "Print a NODE into STREAM.")
  (:method (stream (node node))
    (let ((content (node-content node)))
      (cond
	((not (terminalp node))
	 (unparse-to-stream% stream (node-content node))
	 #+nil (when (listp content)
		 (write-char #\) stream)))
	((typep content 'character-node)
	 (princ (node-raw node) stream))
	(t
	 (format stream "~a" content)))))
  (:method (stream (node list-node))
    (write-char #\( stream)
    (unparse-to-stream% stream (node-content node))
    (write-char #\) stream)))



(defun defpackage-node-p (node)
  (and
   (typep node 'list-node)
   (let ((car (car (node-content node))))
     (and (typep car 'symbol-node)
	  (string-equal "defpackage" (node-content car))))))
