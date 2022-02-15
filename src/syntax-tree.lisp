(defpackage #:breeze.syntax-tree
  (:documentation "Syntax tree data structure")
  (:use #:cl)
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
   #:node-end))

(in-package #:breeze.syntax-tree)


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


;;; Printing

(defun cropped (string &optional (length 25))
  (str:replace-all #.(coerce (list #\Newline) 'string) "\\n"
		   (str:shorten length string)))

(defun print-node-type (node stream)
  (format stream "~@(~a~) "
	  (if (eq 'node (type-of node))
	      "Node"
	      (let ((type (symbol-name (type-of node))))
		(subseq type 0 (- (length type) #. (length "-node")))
		;; (subseq type 0 3)
		))))

(defun print-node-prefix (node stream)
  (a:if-let (prefix (node-prefix node))
    (format stream "~s "
	    (if (every #'breeze.utils:whitespacep prefix)
		(length prefix)
		(node-prefix node)))))

(defmacro print-node (&body body)
  `(let ((*print-circle* t)
	 (*print-right-margin* nil))
     (print-unreadable-object
	 (node stream)
       ,@body)))

(defmethod print-object ((node node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    (format stream "<~a>~%~:t ~s"
	    (cropped
	     (node-raw node))
	    (node-content node))))

(defmethod print-object ((node symbol-node) stream)
  (print-node
    (print-node-type node stream)
    (format stream "~s " (symbol-package (node-content node)))
    (print-node-prefix node stream)
    (format stream "~s"
	    (or (node-raw node) (node-content node)))))

(defmethod print-object ((node skipped-node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    ;; TODO Only print the first 15 characters
    (format stream "~s"
	    (node-content node))))

(defmethod print-object ((node feature-expression-node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    (format stream "~s ~s :raw ~s"
	    (node-feature-expression node)
	    (node-content node)
	    (node-raw node))))


;;; Type predicates

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
