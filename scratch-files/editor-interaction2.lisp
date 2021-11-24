(in-package #:common-lisp-user)

(defpackage #:editor-interaction2
  (:use :cl)
  (:export
   #:dummy-command
   #:continue-command-processing))

(in-package #:editor-interaction2)

(defparameter *state* ())
(defparameter *next* ())

(defun process-command (args command)
  (setf *next* command
	;; Assuming args is a plist...
	*state* (alexandria:plist-alist args))
  (process-command% command nil))

(defun process-command% (command args)
  (multiple-value-bind (request next-command)
      (when command
	(apply command args))
    (if (functionp request)
	(process-command% request nil)
	(prog1
	    ;; Return the request
	    request
	  ;; Update *next*
	  (cond			  ; using "cond" for future expansions
	    ((eq t next-command)
	     ;; Don't update *next*, we'll just loop
	     )
	    (t (setf *next* next-command)))))))

(defun continue-command-processing (&rest response)
  (process-command% *next* response))

(trace process-command)
(trace process-command%)
(trace continue-command-processing)



(defun choose (prompt collection &optional callback)
  "Send a message to the editor to ask the user to choose one element in the collection."
  (lambda ()
    (values
     `("choose" ,prompt ,collection)
     callback)))

(defun read-string (prompt &optional callback)
  "Send a message to the editor to ask the user to enter a string."
  (lambda ()
    (values
     `("read-string" ,prompt)
     callback)))

(defun insert-at (position string save-excursion-p &optional callback)
  "Send a message to the editor telling it to insert STRING at POSITION.
Set SAVE-EXCURSION-P to non-nil to keep the current position."
  (lambda ()
    (values (list
	     (if save-excursion-p
		 "insert-at-saving-excursion"
		 "insert-at")
	     position
	     string)
	    callback)))

(defun insert (string-spec &optional callback)
  "Send a message to the editor telling it to insert STRING at POSITION.
Set SAVE-EXCURSION-P to non-nil to keep the current position."
  (lambda ()
    (values (list "insert"
		  (if (listp string-spec)
		      (apply #'format nil string-spec)
		      string-spec))
	    callback)))

(defun read-string-then-insert (prompt control-string
				&optional callback)
  (read-string prompt
	       (lambda (string)
		 (insert (list control-string string) callback))))

(defun replace-region (position-from position-to
		       replacement-string
		       save-excursion-p
		       &optional callback)
  "Send a message to the editor telling it to replace the text between POSITION-FROM POSITION-TO by REPLACEMENT-STRING.
Set SAVE-EXCURSION-P to non-nil to keep the current position."
  (lambda ()
    (values (list
	     (if save-excursion-p
		 "replace-saving-excursion"
		 "replace")
	     position-from
	     position-to
	     replacement-string)
	    callback)))

(defun backward-char (&optional n callback)
  (lambda ()
    (values (list "backward-char" n)
	    callback)))



(defun point ()
  (alexandria:assoc-value *state* :point))

(defun simple-command ()
  (insert (point) "hi" nil))

(defun command-with-2-actions (&key point &allow-other-keys)
  (insert (point) "bob" nil
	  (lambda ()
	    (insert (point) "hi " nil))))

(defun command-with-2-actions* (&key point &allow-other-keys)
  (values (insert (point) "bob" nil)
	  (lambda ()
	    (insert (point) "hi " nil))))


(process-command '(:point 42) #'simple-command)
;; => ("insert" 42 "hi")
;; *next* is nil
;; *state* => ((:POINT . 42))

(process-command '(:point 42) #'command-with-2-actions)
;; => ("insert" 42 "bob")
;; *next* contains a lambda
(continue-command-processing)
;; => ("insert" 42 "hi ")
;; *next* is nil
(continue-command-processing)
;; => nil


(defun test-command (command args responses)
  (loop
    ;; guards against infinite loop
    for i below 10
    for request = (process-command args command)
      then (if send-response-p
	       (funcall #'continue-command-processing response)
	       (funcall #'continue-command-processing))
    for send-response-p =
			(member (car request)
				'("read-string"
				  "choose")
				:test #'string=)
    for response in responses
    collect (list request response)))

(test-command (insert nil "hi" nil)
	      ()
	      '(nil))

(test-command
 (insert nil "hi" nil
	 (read-string "Name? "
		      (lambda (name)
			(insert nil (format nil " ~a" name) nil))))
 ;; no args
 ()
 (list
  nil
  "bob"
  nil))
;; => ((("insert" NIL "hi") NIL) (("read-string" "Name? ") "bob")
;; 			   (("insert" NIL " bob") NIL))

(defun dummy-command (&rest all
		      &key
			buffer-string
			buffer-name
			buffer-file-name
			point
			point-min
			point-max)
  (declare (ignore buffer-string
		   buffer-name
		   buffer-file-name
		   point
		   point-min
		   point-max))
  (process-command
   all
   (choose "Which command?" '("hi" "greetings")
	   (lambda (choice)
	     (cond
	       ((string= choice "hi")
		(insert nil "hi" nil
			(read-string "Name? "
				     (lambda (name)
				       (insert nil (format nil " ~a" name) nil)))))
	       ((string= choice "greetings")
		(insert nil "Generic greetings to you!" nil)))))))

;;; Much better interface :)
;;;
;;; Next: utility function to help with looping, chaining, and common
;;; usage.  BUT, to get an idea of a "common usage", I would need to
;;; actually start using this... So first add move this to src/ (maybe
;;; with better names) and add tests.
;;;
;;; Hint: use reduce for chaining. (values command t) for simple loops


(defun insert-defun-shaped (form-name all)
  (process-command
   all
   (chain
     (insert (list "(~a " form-name))
     (read-string-then-insert "Name: " "~a (")
     (read-string-then-insert
      ;; Who needs to loop...?
      "Enter the arguments: " "~a)~%)")
     (backward-char))))

(defun insert-defun (&rest all)
  (insert-defun-shaped "defun" all))

(defun insert-defmacro (&rest all)
  (insert-defun-shaped "defmacro" all))

(defmacro defcommand (name &body commands)
  "Macro to define _simple_ commands."
  `(defun ,name (&rest all
		 &key
		   buffer-string
		   buffer-name
		   buffer-file-name
		   point
		   point-min
		   point-max)
     (declare (ignorable buffer-string
			 buffer-name
			 buffer-file-name
			 point
			 point-min
			 point-max))
     (process-command
      all
      (chain
	,@commands))))

(defcommand insert-loop-clause-for-on-list
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :on ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(defcommand insert-loop-clause-for-in-list
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(defcommand insert-loop-clause-for-in-list
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(defcommand insert-loop-clause-for-hash
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the key: "
   "~a :being :the :hash-key :of ")
  (read-string-then-insert
   "Enter the variable name for the hash-table: "
   "~a :using (hash-value ")
  (read-string-then-insert
   "Enter the variable name for the value: "
   "~a)"))


;; TODO insert-let (need to loop probably)

(define-skeleton breeze-insert-defvar
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored.
  > "(defvar *"
  (skeleton-read "Name: ") "* "
  (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(defun insert-defvar-shaped (form-name all)
  (process-command
   all
   (chain
     (insert (list "(~a " form-name))
     ;; TODO Check if name is surrounded by "*"
     (read-string-then-insert "Name: " "*~a* ")
     (read-string-then-insert "Initial value: " "~a~%")
     (read-string-then-insert "Documentation string " "\"~a\")"))))

(defun insert-defvar (&rest all)
  (insert-defvar-shaped "defvar" all))

(defun insert-defparameter (&rest all)
  (insert-defvar-shaped "defparameter" all))

(defun insert-defconstant (&rest all)
  (insert-defvar-shaped "defconstant" all))

;; TODO Add "alexandria" when the symbol is not interned
(defun insert-define-constant (&rest all)
  (insert-defvar-shaped "define-constant" all))


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
  (let ((commands
	  '((insert-loop-clause-for-in-list
	     "Insert a loop clause to iterate in a list.")
	    (insert-loop-clause-for-on-list
	     "Insert a loop clause to iterate on a list.")
	    (insert-loop-clause-for-hash
	     "Insert a loop clause to iterate on a hash-table.")
	    (insert-defun
	     "Insert a defun form.")
	    (insert-macro
	     "Insert a defmacro form."))))
    (process-command
     all
     (choose "Choose a command: "
	     (mapcar #'second commands)
	     (lambda (choice)
	       (list "run-command"
		     (let ((*print-escape* t)
			   (*package* (find-package "KEYWORD"))
			   (function (car (find choice commands
						:key #'second
						:test #'string=))))
		       (prin1-to-string function)
		       #+nil
		       (format nil "(~s)" function))))))))

(defun dummy-loop)
