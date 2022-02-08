(in-package #:common-lisp-user)

(defpackage #:breeze.reader
  (:use :cl)
  (:import-from #:breeze.utils
		#:read-stream-range
		#:stream-size
		#:positivep)
  (:local-nicknames (#:a #:alexandria)
		    (#:tpln #:trivial-package-local-nicknames))
  (:export
   ;; Syntax tree types
   #:node
   #:skipped-node
   #:symbol-node
   #:read-eval-node
   #:character-node
   #:list-node
   #:function-node

   ;; Type predicates
   #:skipped-node-p
   #:symbol-node-p
   #:read-eval-node-p
   #:character-node-p
   #:list-node-p
   #:function-node-p

   ;; Node accessors
   #:node-content
   #:node-prefix
   #:node-source
   #:node-raw
   #:node-start
   #:node-end

   ;; Parse and unparse list of forms
   #:parse
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

(defun node-start (node)
  "Return the position where NODE starts."
  (car (node-source node)))

(defun node-end (node)
  "Return the positiion where NODE ends."
  (cdr (node-source node)))

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

(defclass feature-expression-node (node)
  ((feature-expression
    :initform nil
    :initarg :feature-expression
    :accessor node-feature-expression))
  (:documentation "Syntax node for a feature expression."))

(defclass function-node (node)
  ()
  (:documentation "Syntax node for #'expression."))


(defmacro define-node-type-predicates (types)
  `(progn
     ,@(loop :for type :in types
	     :collect
	     `(defun ,(alexandria:symbolicate
		       type
		       (if (position #\- (symbol-name type))
			   '-p
			   'p))
		  (node)
		(typep node ',type)))))

(define-node-type-predicates
    (node
     skipped-node
     symbol-node
     read-eval-node
     character-node
     list-node
     function-node))

(defgeneric non-terminal-p (node)
  (:documentation "Can a node contain other nodes.")
  (:method ((node node)) (listp (node-content node)))
  (:method ((node skipped-node)) nil)
  (:method ((node symbol-node)) nil)
  (:method ((node character-node)) nil))

(defgeneric terminalp (node)
  (:documentation "Can a node contain other nodes.")
  (:method ((node node)) (not (non-terminal-p node))))

(defmethod print-object ((node node) stream)
  (let ((*print-circle* t))
    (print-unreadable-object
	(node stream :type t :identity nil)
      (when (node-prefix node)
	(format stream "~s "
		(node-prefix node)))
      (format stream "~s :raw ~s"
	      (node-content node)
	      (node-raw node)))))

(defmethod print-object ((node symbol-node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (format stream "~s " (symbol-package (node-content node)))
    (when (node-prefix node)
      (format stream "~s "
	      (node-prefix node)))
    (format stream "~s"
	    (or (node-raw node) (node-content node)))))

(defmethod print-object ((node skipped-node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (format stream "~s"
	    (node-content node))))

(defmethod print-object ((node feature-expression-node) stream)
  (let ((*print-circle* t))
    (print-unreadable-object
	(node stream :type t :identity nil)
      (when (node-prefix node)
	(format stream "~s "
		(node-prefix node)))
      (format stream "~s ~s :raw ~s"
	      (node-feature-expression node)
	      (node-content node)
	      (node-raw node)))))


;;; Parser "client"

;; Define a class representing the "parse result client"
(defclass breeze-client (eclector.parse-result:parse-result-client)
  ((source
    :initarg :source
    :initform nil
    :type (or null string stream)
    :accessor source))
  (:documentation
   "Controls how the reader construct the parse-results."))

(defun raw (breeze-client start end)
  (alexandria:if-let ((source (source breeze-client)))
    (etypecase source
      (string (subseq source start end))
      (stream (read-stream-range source start end)))))

;; WIP another version of "raw" that doens't support streams, but can
;; use displaced arrays
#+ (or)
(defun raw (breeze-client start end)
  (alexandria:if-let ((source (source breeze-client)))
    (let* ((end (min end (length source)))
	   (size (- end start)))
      (make-array size
		  :element-type (array-element-type source)
		  :displaced-to source
		  :displaced-index-offset start))))

(defmethod eclector.parse-result:make-expression-result
    ((client breeze-client) (result t) (children t) (source t))
  "Create an expression result"
  (let* ((raw (raw client (car source) (cdr source)))
	 (node
	   (progn
	     (dbg "~&make-expression result: ~s children: ~s source: ~s raw: ~s" result children source raw)
	     (cond
	       ((or (alexandria:starts-with-subseq "#+" raw)
		    (alexandria:starts-with-subseq "#-" raw))
		;; (let ((content (node-content))))
		(make-instance 'feature-expression-node
			       :feature-expression (first children)
			       :content
			       (if (nodep result)
				   result ;;(node-content result)
				   (cdr children))))
	       (;; If result is a node, populate it
		(typep result 'node)
		(when children
		  (setf (node-content result) children))
		result)
	       (;; If result is a symbol, make a symbol-node
		(symbolp result)
		(make-instance 'symbol-node
			       :content result))
	       (;; Else, make a generic node
		t
		(make-instance 'node
			       :content (or children result)))))))
    (setf (node-source node) source
	  (node-raw node) raw)
    (dbg "~&new-node: ~s" node)
    node))

;; Create a "make skipped input result" method for our custom client
(defmethod eclector.parse-result:make-skipped-input-result
    ((client breeze-client) (stream t) (reason t) (source t))
  "Create a skipped-node parse result."
  (make-instance 'skipped-node
		 :content
		 (read-stream-range stream
				    (car source)
				    (cdr source))
		 ;; WIP using string only, instead of stream
		 #+ (or)
		 (raw client
		      (car source)
		      (cdr source))
		 :source source))

;; Support package-local-nicknames
(defmethod eclector.reader:interpret-symbol
    ((client breeze-client) (stream t)
     package-indicator symbol-name internp)
  (unless (case package-indicator
	    (:current *package*)
	    (:keyword (find-package "KEYWORD"))
	    (t (find-package package-indicator)))
    (a:if-let (actual-package
	       (cdr (assoc "TPLN"
			   (tpln:package-local-nicknames *package*)
			   :test #'string=)))
      (setf package-indicator actual-package)))
  (call-next-method))


(defmethod eclector.reader:evaluate-expression ((client breeze-client)
						expression)
  "Create a syntax node for #. ."
  (make-instance 'read-eval-node
		 :content expression))

(defmethod eclector.reader:evaluate-feature-expression
    ((client breeze-client) feature-expression)
  (if (typep feature-expression 'node)
      ;; This is very hacky ¯\_(ツ)_/¯
      (eclector.reader:call-with-current-package
       client
       #'(lambda ()
	   (eclector.reader:evaluate-feature-expression
	    client
	    (cl:read-from-string (node-raw feature-expression))))
       :keyword)
      (call-next-method)))

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

(defun read-from-string (string &optional (eof-error-p t)
				  eof-value
			 &key
			   (start 0)
			   end
			   preserve-whitespace)
  (eclector.parse-result:read-from-string
   (make-instance 'breeze-client
		  :source string)
   string
   eof-error-p
   eof-value
   :start start
   :end end
   :preserve-whitespace preserve-whitespace))


(defun read-all-forms (stream)
  (let ((eof (gensym "eof"))
	(client (make-instance
		 'breeze-client
		 :source
		 stream
		 #+ (or)
		 (prog1 (alexandria:read-stream-content-into-string stream)
		   (file-position stream 0)))))
    (loop
      for form =
	       (eclector.parse-result:read-preserving-whitespace
		client
		stream
		nil
		eof)
      until (eq eof form)
      collect form)))

;; end-at is not used, its purpose is to help find trailing characters
;; but I haven't implemented that because I'm not sure it's the way to go.
(defun post-process-nodes! (stream forms &optional (start-at 0) end-at)
  "Update each nodes in FORMS to include their prefix (extracted from STREAM)."
  (declare (ignore end-at))
  (let ((previous nil))
    (loop
      for form in forms
      for start = start-at then (cdr (node-source previous))
      for end = (car (node-source form))
      for prefix = (unless (zerop (- end start))
		     (read-stream-range stream start end))
      do
	 ;; update FORM
	 (setf
	  ;; Add the prefix
	  (node-prefix form) prefix)
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

(defun unparse-to-string (nodes)
  "Print a list of NODES as a STRING."
  (with-output-to-string
      (stream)
    (etypecase nodes
      (list
       (unparse-to-stream stream nodes))
      (node
       (unparse-to-stream stream (list nodes))))))


#+ (or)
(sb-profile:profile
 parse
 read-all-forms
 read-from-string
 make-instance
 eclector.parse-result:make-expression-result
 eclector.parse-result:read-preserving-whitespace
 post-process-nodes!
 raw)

;; #+ (or) (sb-profile:report)
