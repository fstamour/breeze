;;;; WARN: THIS WAS PROTOTYPED USING MYELIN


;;; Snippets
;;; N.B. They are more like templates/skeletons.

(in-package #:common-lisp-user)

(defpackage #:breeze.snippets
  (:use :cl))

(in-package #:breeze.snippets)

;; (ql:quickload 'with-output-to-stream)

(defvar *snippet-stream* nil
  "The stream use to print out snippets.")

(defparameter *snippets* (make-hash-table)
  "Hash table to keep track of the definition of the snippets.")

(defparameter *indentation* 0
  "Variable to manage indentation level when printing snippets.")

(defun indent (&optional (stream *standard-output*))
  "Print a number of space based on the value of *indentation*."
  (princ (str:repeat *indentation* "  ") stream))

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
		   ((member (first form) '(format princ prin1)) form)
		   (t
		    (transform-snippet form))))
		;; The rest is unmodified
		(t form))))

(defmacro define-snippet (&whole whole
			    name (&rest lambda-list)
			    docstring
			  &body body)
  "Create a function and register a snippet."
  (check-type docstring string)
  `(progn
     (defun ,(alexandria:symbolicate 'snippet/ name)
	 (stream
	  &optional
	    ,@(mapcar #'ensure-car lambda-list))
       ,docstring
       ;; Save the current indentation level
       (let ((*indentation* *indentation*))
	 (with-output-to-stream:with-output-to-stream (*snippet-stream* stream)
	   ,@(transform-snippet body))))
     (defun ,(symbolicate 'snippet/ name '*)
	 (stream
	  &key
	    ,@(mapcar #'ensure-car lambda-list))
       ,docstring
       (,(symbolicate 'snippet/ name)
	stream
	,@(mapcar #'ensure-car lambda-list)))
     ;; Register the snippet's definition.
     (setf (gethash ',name *snippets*) ',whole)
     ',name))

(define-snippet defpackage ((name :placeholder "Name of the package")
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

(defun snippet-inputs (snippet)
  (third
   (gethash snippet *snippets*)))

(define-snippet defun ((name :placeholder "Name of the function")
		       (lambda-list :placeholder "Argument list"))
    "Insert a defun form."
  (indent)
  "(defun " 'name
  " (" (when lambda-list
	 (format *snippet-stream* "~(~{~a~^ ~}~)"
		 (ensure-list lambda-list)))
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
(snip/petdefmacro "my-macro")

(defun snippet-function-symbol (snippet)
  (let ((*package* #.*package*))
    (symbolicate 'snippet/ snippet)))

(defun snippet-function-symbol* (snippet)
  (let ((*package* #.*package*))
    (symbolicate 'snippet/ snippet '*)))



(defvar *code-id-counter* 0)
(defparameter *code* (make-hash-table))

;; (clrhash *code*)

(define-easy-handler (code :uri "/code")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (let ((id (or id (incf *code-id-counter*))))
    (with-output-to-string (*html*)
      (with-html ("Code")
	(:ul
	 (loop
	    :for snippet :being :the :hash-key :of *snippets*
	    :for fn = (snippet-function-symbol snippet)
	    :do (:li
		 (:form :method "post"
			:action "/code/add-snippet-form"
			(:input :type "hidden"
				:value id
				:name "id")
			(:input :type "hidden"
				:name "snippet"
				:value snippet)
			(:input :type "submit"
				:value (format nil "Insert ~(~a~)" snippet))))))
	(unless (gethash id *code*)
	  (setf (gethash id *code*) (list (format nil ";;; Code ~a" id))))
	(loop
	   :for block :in (gethash id *code*)
	   :for block-id :from 0
	   :do
	     (:article (:pre block))
	     (:form :method "post"
			:action "/code/edit-block"
			(:input :type "hidden"
				:value id
				:name "id")
			(:input :type "hidden"
				:name "block-id"
				:value block-id)
			(:input :type "submit"
				:value "Edit"))
	     (:form :method "post"
			:action "/code/delete-block"
			(:input :type "hidden"
				:value id
				:name "id")
			(:input :type "hidden"
				:name "block-id"
				:value block-id)
			(:input :type "submit"
				:value "Delete")))
	))))

(define-easy-handler (add-snippet-form :uri "/code/add-snippet-form")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (with-output-to-string (*html*)
    (with-html ("Add snippet to code")
      (:h2 (format nil "Snippet \"~(~a~)\"" snippet))
      (:form :method "post"
	     :action "/code/add-snippet"
	     (:input :type "hidden"
		     :value id
		     :name "id")
	     (:input :type "hidden"
		     :name "snippet"
		     :value snippet)
	     (loop
		:for spec :in (snippet-inputs
			       (intern snippet #.*package*))
		:do (render-input spec) (:br))
	     (:input :type "submit")))))

(define-easy-handler (add-snippet :uri "/code/add-snippet")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (setf (gethash id *code*)
	;; Append the current code with the result of calling the snippet.
	(append
	 (gethash id *code*)
	 (list
	  ;; Call the snippet function
	  (apply (snippet-function-symbol* snippet)
		 nil
		 ;; Transform the parameters to keywords
		 (loop :for (key value) :on
		      (hash-table-plist
		       (cl-hash-util:collecting-hash-table ()
			 (loop
			    :for (key . value) :in (post-parameters*)
			    :do
			      (unless (member key '("id" "snippet")
					      :test 'string=)
				(when (and value
					   (> (length value) 0))
				  (cl-hash-util:collect
				      (make-keyword (string-upcase key))
				    value))))))
		    :by #'cddr
		    :append (list key
				  (if (cdr value)
				      value
				      (car value))))))))
  (redirect (format nil "/code?id=~a" id)))

(cl-hash-util:collecting-hash-table
    (:mode (list
	    #'(lambda (current new)
		(append (ensure-list current)
			(list new)))
	    #'(lambda (new)
		new)))
  (cl-hash-util:collect :a 1)
  (cl-hash-util:collect :a 2)
  (cl-hash-util:collect :b 1)
  )

(loop
   :for snippet :being :the :hash-key :of *snippets*
   :for fn = (snippet-function-symbol snippet)
   :collect (list fn (documentation fn 'function)))

(define-easy-handler (snippet :uri "/code/snippet")
    ((snippet :parameter-type 'string))
  (with-output-to-string (*html*)
    (with-html ("Snippet")
      (:p
       (format nil snippet)
       ;; Warning: internalizing strings from the internet _might_ not be a good idea
       (let ((snippet (intern (string-upcase snippet))))
	 (if (gethash snippet *snippets*)
	     (:ul
	      (multiple-value-bind (required optional)
		  (parse-ordinary-lambda-list
		   (third
		    (gethash snippet *snippets*)))
		(loop :for arg :in required
		   :do (:li (format nil "~a" arg)))
		(loop :for (arg default _) :in optional
		   :do (:li (format nil "~a" arg)))))
	     "Not found"))))))



(define-easy-handler (listing :uri "/code/listing")
    ()
  (with-output-to-string (*html*)
    (with-html ("Listing")
      (:h2 "Defpackage")
      (:article
       (:pre
	  (snippet/defpackage *html* "dummy")))
      (:article
       (:pre
	"(defun x2 (x) (* 2 x))")))))
