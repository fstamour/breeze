;; 13 april 2020 - Trying out eclector: a portable and extensible
;; common lisp reader https://github.com/s-expressionists/Eclector

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :breeze.user)

;; (ql:system-apropos-list "eclector")

(ql:quickload '(#:eclector
		#:eclector-concrete-syntax-tree))

(defun read-forms (stream)
  "Read all forms from a stream."
  (let ((end-of-file (gensym "eof")))
      (loop :for form = (read stream nil end-of-file)
	 :while (not (eq form end-of-file))
	 :collect form)))

(defun read-file (pathname )
  "Read a file, form by form"
  (with-open-file (stream pathname)
    (read-forms stream)))

(defun relative-pathname (pathname)
  (if (cl-fad:pathname-relative-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
    pathname))

#+nil
(with-open-file (stream (relative-pathname "examples/function-redefinition.lisp"))
  (let ((end-of-file (gensym "eof")))
    (loop :for form = (eclector.reader:read stream nil end-of-file)
       :while (not (eq form end-of-file))
	 :collect form)))

(defun form-in-package-p (form)
  "Is the form an \"in-package\" form?
If it is return the package-designator"
  (and (listp form)
       (eq 'cl:in-package (first form))
       (second form)))

(defun form-function-p (form)
  "Is the form a function?"
  (and (listp form)
       (eq 'cl:defun (first form))))


(defparameter *examples* (make-hash-table :test 'equal))

;; load examples
(loop :for (_ name . rest) :in
     (read-file (relative-pathname "examples/function-redefinition.lisp"))
   :do (setf (gethash name *examples*) rest))

(alexandria:hash-table-plist *examples*)

("Add a function"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Doubles x"
      (+ X X))
    (DEFUN |3X|
        (X)
      "Multiply x by 3"
      (* 2 X)))))
 "Change implementation and documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Multiply x by 2"
      (* 2 X)))))
 "Change documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     "Doubles x"
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Adds x to itself"
      (+ X X)))))
 "Add documentation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      "Doubles x"
      (+ X X)))))
 "Change implementation"
 (:BEFORE
  ((IN-PACKAGE 'EXAMPLES)
   (DEFUN |2X|
       (X)
     (+ X X)))
  (:AFTER
   ((IN-PACKAGE 'EXAMPLES)
    (DEFUN |2X|
        (X)
      (* 2 X))))))


;;; Eclector + Concrete Syntax Tree

(defparameter *cst*
  (eclector.concrete-syntax-tree:read-from-string "
   (1 2 3)
   (4 5)"))

(defparameter *src*
  (alexandria:read-file-into-string
   (relative-pathname "examples/min.lisp")))

(defparameter *csts*
  (with-input-from-string
   (stream *src*)
   (let ((end-of-file (gensym "eof")))
     (loop :for form = (eclector.concrete-syntax-tree:read-preserving-whitespace
			stream nil end-of-file)
	   :while (not (eq form end-of-file))
	   :collect form))))


(let ((previous nil))
  (loop for guard from 0 below 100
	for source in (mapcar #'concrete-syntax-tree:source *csts*)
	for from = 0 then (cdr previous)
	for to =  (car source)
	do (setf previous source)
	collect (unless (zerop (- to from))
		  `((,from . ,to)
		    ,(subseq *src* from to)))))
(((0 . 69) ";;; This is just an example used to test the parsing of source code

")
 ((100 . 102) "

")
 ((133 . 135) "

")
 ((153 . 179) "

;; A less powerful min!
"))



;; Conclusion of the previous "page": CST are nice because they allow
;; you to manipulate syntax tree without loosing too much source
;; information, but they do not provide anything to print back the
;; modified AST. So it's not exactly what I need.


;;; Eclector + Custom parse result

(defun read-stream-range (stream from to)
  (let ((current-position (file-position stream)))
    (unwind-protect
	(let ((sequence (make-string (- to from))))
	  (file-position stream from)
	  (read-sequence sequence stream)
	  sequence)
      (file-position stream current-position))))

(with-input-from-string
 (stream "(1 #|comment|# \"string\")")
 (values
  (read-stream-range stream 3 (+ 3 11))
  (file-position stream)))
;; => "#|comment|#", 0

(defun stream-size (stream)
  (let ((current-position (file-position stream)))
    (when current-position
      (unwind-protect
	  (progn
	    (file-position stream :end) ;; TODO This might fail
	    (file-position stream))
	(file-position stream current-position)))))

(with-input-from-string
 (stream "(1 #|comment|# \"string\")")
 (stream-size stream))
;; => 24

(defun positivep (x)
  (> x 0))

;; (positivep 1)
;; (positivep 0)
;; (positivep -1)

;; Define a class representing the "parse result client"
(defclass my-client (eclector.parse-result:parse-result-client)
  ())

(defclass node ()
  ((content
    :initform nil
    :initarg :content
    :accessor content)
   (prefix
    :initform nil
    :initarg :prefix
    :accessor prefix)
   (source
    :initform nil
    :initarg :source
    :accessor source)
   (raw
    :initform nil
    :initarg :raw
    :accessor raw)))

(defclass ignored-node (node) ())
(defclass read-eval-node (node) ())
(defclass symbol-node (node) ())

(defgeneric terminalp (node)
  (:documentation "Whethere a node cannot contain other nodes.")
  (:method ((node node)) nil)
  (:method ((node ignored-node)) t)
  (:method ((node symbol-node)) t))


(defmethod print-object ((node node) stream)
  (print-unreadable-object
   (node stream :type t :identity nil)
   (if (prefix node)
       (format stream "~s ~s @ ~s"
	       (prefix node)
	       (content node)
	       (source node)))
   (format stream "~s @ ~s"
	   (content node)
	   (source node))))

(defmethod print-object ((node symbol-node) stream)
  (print-unreadable-object
   (node stream :type t :identity nil)
   (if (prefix node)
       (format stream "~s ~s @ ~s"
	       (prefix node)
	       (or (raw node) (content node))
	       (source node))
     (format stream "~s @ ~s"
	     (or (raw node) (content node))
	     (source node)))))

(defmethod print-object ((node ignored-node) stream)
  (print-unreadable-object
   (node stream :type t :identity nil)
   (format stream "~s @ ~s"
	   (raw node)
	   (source node))))

;; Create a "make expression result" method for our custom client
(defmethod eclector.parse-result:make-expression-result
  ((client my-client) (result t) (children t) (source t))
  (format t "~&result: ~s source: ~s" result source) ;; ========================================
  (typecase result
    ;; #.
    (read-eval-node
     (setf (source result) source)
     (when children
       (setf (content result) children))
     result)
    (symbol
     (make-instance 'symbol-node
		    :content result
		    :source source))
    (t
     (make-instance 'node
		    :content (or children result)
		    :source source))))

;; Create a "make skipped input result" method for our custom client
(defmethod eclector.parse-result:make-skipped-input-result
  ((client my-client) (stream t) (reason t) (source t))
  (make-instance 'ignored-node
		 :content (read-stream-range stream (car source) (cdr source))
		 :source source))

;; Not useful
#+nil
(defmethod eclector.reader:note-skipped-input ((client my-client) (stream t) (reason t))
  (format t "~&eclector.reader:note-skipped-input reason: ~a" reason))

#+nil
(remove-method #'eclector.reader:note-skipped-input
	       (find-method #'eclector.reader:note-skipped-input nil '(my-client t t)))

(defmethod eclector.reader:evaluate-expression ((client my-client) expression)
  (make-instance 'read-eval-node
		 :content expression))

(defun maybe-source (form eof)
  (and
   form
   (not (eq eof form))
   (source form)))

(defun read-all-forms (stream)
  (let ((eof (gensym "eof")))
    (loop for form =
	  (eclector.parse-result:read-preserving-whitespace
	   (make-instance 'my-client)
	   stream
	   nil
	   eof)
	  until (eq eof form)
	  collect form)))

(defun add-prefix! (stream forms &optional (start-at 0))
  "Update each nodes in FORMS to include their prefix (extracted from STREAM)."
  (let ((previous nil))
    (loop
     for form in forms
     for start = start-at then (cdr (source previous))
     for end = (car (source form))
     for prefix = (unless (zerop (- end start))
		    (read-stream-range stream start end))
     for raw = (destructuring-bind (from . to)
		   (source form)
		 (read-stream-range stream from to))
     do
     (format t "~&node: ~a raw: ~s" form raw)
     ;; update FORM
     (setf (prefix form) prefix
	   (raw form) raw)
     ;; recurse
     (unless (terminalp form)
       (add-prefix! stream (content form)
		    (car (source form))))
     ;; update loop variables
     (setf previous form))))

(defun get-tail (stream forms)
  "Given a list of forms, extract any trailing characters that were ignored."
  (let* ((tail (alexandria:lastcar forms))
	 (tail-end (if tail
		       (cdr (source tail))
		     0))
	 (stream-size (stream-size stream)))

    (when (positivep (- stream-size tail-end))
      (make-instance 'ignored-node
		     :content (read-stream-range stream tail-end stream-size)
		     :source (cons tail-end stream-size)))))

(defun parse (stream)
  (let ((forms (read-all-forms stream)))
	   (add-prefix! stream forms)
	   `(,@forms
	     ;; ,@ (source (alexandria:lastcar forms))
	     ,@(alexandria:if-let ((tail (get-tail stream forms)))
				  (list tail)))))

(defun parse-string (string)
  (with-input-from-string
   (stream string)
   (parse stream)))

;; TODO circular structures
;; TODO #S(struct-name field1 field2)
;; TODO #C(real complex)
(defparameter *simple-examples*
  '("1"
    " 1 "
    "()"
    " ( 2 )"
    "\"hi\""
    ";; hello"
    " #| hello |#"
    "a ;; hello"
    "b  #| hello |#"
    "(1 #|comment|# \"string\")"
    "`(,a ,b)"
    ;; "(oups" TODO
    "(1 . 2)"
    "#.(+ 1 2)"
    "#+nil ingored"))

(defmacro for-each-string-as-stream ((stream-var string-var string-list)
				     &body body)
  (check-type stream-var symbol)
  (check-type string-var symbol)
  `(loop for ,string-var in ,string-list
	do
	(with-input-from-string (,stream-var ,string-var)
				,@body)))

(for-each-string-as-stream
 (stream string *simple-examples*)
 (format t "~&===~%~s => ~s~%"
	 string
	 (parse stream)))

(parse-string "`(,a)")
(parse-string "()")
(parse-string "#.()")
(parse-string "#.(+ 1 2)")
(parse-string "#'print")

(defun unparse-to-stream (stream nodes)
  (let ((*print-case* :downcase)
	(*print-circle* t))
    (unparse-to-stream% stream nodes)))

(defun unparse-to-stream% (stream nodes)
  (dolist (node nodes)
    (alexandria:if-let
     ((prefix (prefix node)))
     (format stream "~a" prefix))
    (unparse-node stream node)))

(defun unparse-to-string (nodes)
  (with-output-to-string
    (stream)
    (unparse-to-stream stream nodes)))

(defmethod unparse-node (stream (node node))
  (cond
   ((and (listp (content node))
	 (content node))
    (unparse-to-stream% stream (content node))
    (write-char #\) stream))
   (t
    (format stream "~a" (content node)))))

(defmethod unparse-node (stream (node ignored-node))
  (princ (content node) stream))

(defmethod unparse-node (stream (node symbol-node))
  (princ (raw node) stream))

(unparse-to-string (parse-string " nil "))
(unparse-to-string (parse-string " NiL "))
(unparse-to-string (parse-string " () "))
(unparse-to-string (parse-string " '() "))
(parse-string "'()")
(parse-string "'a")
(unparse-to-string (parse-string "(quote a b c)"))
