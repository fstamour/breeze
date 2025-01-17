;;; Snippets
;;; N.B. They are more like templates/skeletons.
;;;
;;; - The macro "define-snippet" generate a function and register the
;;;   snippet
;;; - Snippets always takes a stream-designator as a first argument
;;;
;;; Major things that could be improved:
;;;   - The snippet's lambda-list
;;;   - The snippet's name (currently "snippet/the-name")
;;;   - Ability to ask the user for input


(in-package #:common-lisp-user)

(defpackage breeze.snippets
  (:use :cl)
  (:import-from #:alexandria
		#:ensure-car
		#:symbolicate)
  (:export #:find-snippet
	   ;; #:snippet-inputs
	   #:snippet-lambda-list
	   #:snippet-function))

(in-package #:breeze.snippets)

(ql:quickload '(str with-output-to-stream))

(defvar *snippet-stream* nil
  "The stream use to print out snippets.")

(defparameter *snippets* (make-hash-table)
  "Hash table to keep track of the snippets.")

(defparameter *indentation* 0
  "Variable to manage indentation level when printing snippets.")

(defun find-snippet (name)
  (gethash name *snippets*))

(defclass snippet ()
  ((name
    :reader snippet-name
    :initarg :name
    :type 'symbol)
   (definition
    :reader snippet-definition
    :initarg :definition)
   (function
    :reader snippet-function
    :initarg :function)))

(defmethod print-object ((snippet snippet) stream)
  (print-unreadable-object (snippet stream :type t :identity t)
    (format stream "~s" (snippet-name snippet))))

(defmethod snippet-lambda-list ((snippet snippet))
  (third (snippet-definition snippet)))

(defun indent (&optional (stream *standard-output*))
  "Print a number of space based on the value of *indentation*."
  (princ (breeze.string:repeat-string *indentation* "  ") stream))

(defun transform-snippet (body)
  "Take a skeleton and generate the code to execute it."
  (loop :for form :in body
	:collect (cond
		   ;; Shorthand for newline
		   ((eq '\n form) '(progn
				    (terpri *snippet-stream*)
				    (indent *snippet-stream*)))
		   ;; Increment indentation
		   ((eq '> form) '(incf *indentation*))
		   ;; Decrement indentation
		   ((eq '< form) '(decf *indentation*))
		   ;; ignored for now, used to position the cursor
		   ((eq '_ form) nil)
		   ((stringp form)
		    `(princ ,form *snippet-stream*))
		   ;; Variables must be quoted
		   ((and
		     (listp form)
		     (eq 'quote (first form))
		     (symbolp (second form)))
		    `(princ ,(second form) *snippet-stream*))
		   ;; Recurse, except for printing forms
		   ((listp form)
		    (cond
		      ;; FIXME the list is incomplete
		      ((member (first form) '(format princ prin1))
		       form)
		      (t
		       (transform-snippet form))))
		   ;; The rest is unmodified
		   (t form))))

(defun make-snippet-function (lambda-list docstring body)
  `(lambda
       (stream
	&optional
	  ,@(mapcar #'ensure-car lambda-list))
     ,docstring
     ;; Save the current indentation level
     (let ((*indentation* *indentation*))
       (with-output-to-stream:with-output-to-stream
	   (*snippet-stream* stream)
	 ,@(transform-snippet body)))))

#+nil
(defun make-snippet-funtion-with-keyword-argument
    ()
  ;; Second definition, where the lambda-list are keywords
  (defun ,(symbolicate 'snippet/ name '*)
      (stream
       &key
	 ,@(mapcar #'ensure-car lambda-list))
    ,docstring
    (,(symbolicate 'snippet/ name)
     stream
     ,@(mapcar #'ensure-car lambda-list))))

(defmacro define-snippet (&whole whole
			    name (&rest lambda-list)
			    docstring
			  &body body)
  "Create a function and register a snippet."
  (check-type docstring string)		; Docstrings are required
  `(setf (gethash ',name *snippets*)
	 (make-instance 'snippet
			:name ',name
			:definition ',whole
			:function ,(make-snippet-function
				    lambda-list
				    docstring
				    body))))

(defun snippet-inputs (snippet)
  (snippet-lambda-list
   (gethash snippet *snippets*)))

;; (snippet-inputs 'defmacro)


(define-snippet defpackage
    ((name :placeholder "Name of the package")
     (nicknames :type (list string)
		:placeholder "Nickname for the package"))
    "Define a package."
  "(in-package #:common-lisp-user)" \n \n
  "(defpackage #:" 'name > \n
  (when nicknames
    "(:nicknames "
    (loop :for nickname :in nicknames
	  :do (format t "~(#:~s~)" nickname)) ")" \n)
  "(:use :cl))" < \n \n
  "(in-package #:" 'name ")" \n \n)

#+nil
(snippet/defpackage nil "my-awesome-package" '(map))


(define-snippet defun ((name :placeholder "Name of the function")
		       (lambda-list :placeholder "Argument list"))
    "Insert a defun form."
  (indent)
  "(defun " 'name
  " (" (when lambda-list
	 (format *snippet-stream* "~(~{~a~^ ~}~)"
		 (alexandria:ensure-list lambda-list)))
  ")" > \n
  _ ")")

(trace snippet/defun)

#+nil
(snippet/defun nil "my-first-function")
#+nil
(snippet/defun nil "my-first-function" "x")
#+nil
(snippet/defun nil "my-first-function" '("x" "&optional" "y"))
#+nil
(snippet/defun nil "my-first-function" "x &optional y")

(define-snippet defmacro ((name :placeholder "Name of the macro")
			  (lambda-list :placeholder "Argument list"))
    "Insert a defmacro form."
  (indent)
  "(defmacro " 'name
  " (" (when lambda-list
	 (format *snippet-stream* "~(~a~)" lambda-list)) ")" > \n
  _ ")")

#+nil
(snippet/defmacro nil "my-macro")
