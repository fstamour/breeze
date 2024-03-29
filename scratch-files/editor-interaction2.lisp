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



(defun dummy-loop)
