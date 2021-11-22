(in-package #:breeze.quickfix)

(defparameter *response-counter* 0)
(defparameter *state* ())
(defparameter *next* ())

(defun request-done ()
  "Send a message to the editor telling it we're done."
  nil)

(defun request-choose (prompt collection)
  "Send a message to the editor to ask the user to choose one element in the collection."
  `("choose" ,prompt ,collection))

(defun request-read-string (prompt)
  "Send a message to the editor to ask the user to enter a string."
  `("read-string" ,prompt))

;; TODO We should probably specify the buffer name
(defun request-insert (position string &optional save-excursion-p)
  "Send a message to the editor telling it to insert STRING at POSITION.
Set SAVE-EXCURSION-P to non-nil to keep the current position."
  (list
   (if save-excursion-p
       "insert-saving-excursion"
       "insert")
   position
   string))

;; TODO We should probably specify the buffer name
(defun request-replace (position-from position-to
			replacement-string
			&ptional save-excursion-p)
  "Send a message to the editor telling it to replace the text between POSITION-FROM POSITION-TO by REPLACEMENT-STRING.
Set SAVE-EXCURSION-P to non-nil to keep the current position."
  (list
   (if save-excursion-p
       "replace-saving-excursion"
       "replace")
   position
   string))

(defun prototyping-stuff (&rest all
			  &key
			    buffer-string
			    buffer-name
			    buffer-file-name
			    point
			    point-min
			    point-max)
  (declare (ignorable all buffer-string buffer-name buffer-file-name
		      point point-min point-max))
  (setf *state* all)
  ;; (prin1 `("insert-at" ,point "hello"))
  #+nil
  (request-choose
   "Prompt? "
   '("1" "2" "3"))
  `("insert" ,point "hi")
  )

(defun prototyping-stuff2 (&rest response)
  (prog1
      (progn
	(setf (getf *state* *response-counter*) response)
	(incf *response-counter*)
	;; (prin1 *state*)
	;; Done
	(request-done))
    #+nil
    (setf *state* nil)))

(loop :for (k v) on *state* :by #'cddr
      :when (numberp k)
	:collect (list k v))


;; I think to make this manageable, I should try to put a lambda
;; into *next* and prototyping-stuff2 should "just" call that.
;;
;; I don't I'll need *response-counter* nor would I need to have a
;; *state*, although it's very useful for debugging.
;;
;; But, I could keep a *log* or something for debugging.


(send
 (request-choose
  "Prompt? "
  '("1" "2" "3"))
 (receive (choice-a)
     (send
      (request-choose
       "Prompt? "
       '("1" "2" "3"))
      (receive (choice-b)
	  (request-insert 0 (format nil "hi ~a ~a" choice-a choice-b))))))

=>

(prog1
    (request-choose
     "Prompt? "
     '("1" "2" "3"))
  (setf *next*
	(lambda (choice-a)
	  (prog1
	      (request-insert 0 "hi")
	    (setf *next*
		  (lambda (choice-b)
		    (prog1
			(request-insert 0 (format nil "hi ~a ~a" choice-a choice-b))
		      (setf *next*
			    (lambda ()
			      (request-done))))))))))

(funcall *next* "allo")
(funcall *next* "foo")
(funcall *next*)

(defmacro with-send-and-receive (&body body)
  `(macrolet ((send (&body send-body)
		(let ((request-body (butlast send-body))
		      (receive-form (alexandria:lastcar send-body)))
		  `(prog1
		       (progn ,@request-body)
		     ;; TODO check receive-form is ok (e.g. starts with "receive", has a lambda-list)
		     (setf *next* (lambda ,(second receive-form)
				    (with-send-and-receive
				      ,@(cddr receive-form)))))))
	      (send-done (&body body)
		`(prog1 (progn ,@body)
		   (setf *next*
			 (lambda (&rest _)
			   (declare (ignore _))
			   (request-done))))))
     ,@body))

(with-send-and-receive)
(with-send-and-receive
  (+ 1 1))

(trivial-macroexpand-all:macroexpand-all
 `(with-send-and-receive
    (send-done)))

(with-send-and-receive
  (values
   (send
    (request-choose
     "Prompt? "
     '("1" "2" "3"))
    (receive (choice-a)
	(send
	 (request-choose
	  "Prompt? "
	  '("a" "b" "c"))
	 (receive (choice-b)
	     (send-done
	      (request-insert 0 (format nil "hi ~a ~a" choice-a choice-b)))))))
   (funcall *next* "allo")
   (funcall *next* "foo")
   (funcall *next*)
   (funcall *next*)))

("choose" "Prompt? " ("1" "2" "3"))
("choose" "Prompt? " ("a" "b" "c"))
("insert" 0 "hi allo foo")
NIL
NIL




(defun prototyping-stuff (&rest all
			  &key
			    buffer-string
			    buffer-name
			    buffer-file-name
			    point
			    point-min
			    point-max)
  (declare (ignorable all buffer-string buffer-name buffer-file-name
		      point point-min point-max))
  (setf *next* nil)
  (with-send-and-receive
    (send
     (request-choose
      "Prompt? "
      '("1" "2" "3"))
     (receive (choice-a)
	 (send
	  (request-choose
	   "Prompt? "
	   '("a" "b" "c"))
	  (receive (choice-b)
	      (send-done
	       (request-insert point (format nil "hi ~a ~a" choice-a choice-b)))))))))

(defun prototyping-stuff2 (&rest response)
  (apply *next* response))

hi 2 b
hi 2 b

(prototyping-stuff :point 42)
;; => ("choose" "Prompt? " ("1" "2" "3"))
(prototyping-stuff2 "1")
;; => ("choose" "Prompt? " ("a" "b" "c"))
(prototyping-stuff2 "b")
;; => ("insert" 42 "hi 1 b")
(prototyping-stuff2 nil)
;; =>  NIL


;;; Now, how do I integrate this with snippets.lisp?
;;;
;;; - The snippets are currently written to a stream
;;; - They are oblivious to the current context (which is fine, the
;;;    caller can take care of that)
;;; - The snippets are written at once, after all the arguments are
;;;   collected. It would be nicer if it behave like emacs's skeletons
;;;   or it they would insert a template that can be filled.
;;;
;;; The simplest for now is just to ask the user for all parameters
;;; insert the full snippets.

;;; Before I do that though, I would like to try to ask things in a
;;; loop (e.g. to get a list of names).
;;;
;;; In order to do that, I could _not_ update *next* and add some
;;; lexical variables to accumulate the answers.


(defun prototyping-stuff (&rest all
			  &key
			    buffer-string
			    buffer-name
			    buffer-file-name
			    point
			    point-min
			    point-max)
  (declare (ignorable all buffer-string buffer-name buffer-file-name
		      point point-min point-max))
  (setf *next* nil)
  (prog1
      (request-read-string "Enter a new word: ")
    (let ((answers nil))
      (setf *next* (lambda (answer)
		     (if (and answer
			      (stringp answer)
			      (positivep (length answer)))
			 ;; More
			 (prog1
			     (request-read-string "Enter a new word: ")
			   (push answer answers))
			 ;; Done
			 (progn
			   (setf answers (nreverse answers))
			   (setf *next*
				 (lambda (&rest _)
				   (declare (ignore _))
				   (request-done)))
			   (request-insert point
					   (format nil "~{~a~^, ~}" answers)))))))))


(prototyping-stuff :point 42)
;; => ("read-string" "Enter a new word: ")
(prototyping-stuff2 "a")
;; => ("read-string" "Enter a new word: ")
(prototyping-stuff2 "b")
;; => ("read-string" "Enter a new word: ")
(prototyping-stuff2 "")
;; => ("insert" 42 "a, b")
(prototyping-stuff2 nil)
;; => NIL

;;; That works, how do I make this manageable?
;;; First of all, I don't think we need to use "macrolet

(defmacro send (&body body)
  (let ((request-body (butlast body))
	(receive-form (alexandria:lastcar body)))
    `(prog1
	 (progn ,@request-body)
       ;; TODO check receive-form is ok (e.g. starts with "receive", has a lambda-list)
       (setf *next* (lambda ,(second receive-form)
		      (with-send-and-receive
			,@(cddr receive-form)))))))

(defmacro send-done (&body body)
  `(prog1 (progn ,@body)
     (setf *next*
	   (lambda (&rest _)
	     (declare (ignore _))
	     (request-done)))))


(defmacro send-while  ((var-accumulator var while-predicate request) &body body)
  (check-type var symbol)
  `(prog1
       ,request
     (let ((,var-accumulator nil))
       (setf *next* (lambda (,var)
		      (if ,while-predicate
			  ;; More
			  ,request
			  ;; Done
			  (progn
			    (setf ,var-accumulator (nreverse ,var-accumulator))
			    ,@body)))))))

(let ((point 42))
  (send-while (answers
	       answer
	       (and answer
		    (stringp answer)
		    (positivep (length answer)))
	       (request-read-string "Enter a new word: "))
    (send-done
      (request-insert point
		      (format nil "~{~a~^, ~}" answers)))))
(prototyping-stuff2 "a")
("read-string" "Enter a new word: ")
(prototyping-stuff2 "")
("insert" 42 "")
(prototyping-stuff2 nil)
NIL

;;; prototyping-stuff is specific to a kind of interaction or edit
;;; prototyping-stuff2 is not
;;; What could be a good name for prototyping-stuff2?
;;;  - next
;;;  - continue
;;;  - ?
;;;
;;; What could be a good name for all these kind of concepts?
;;;  - editor-interaction
;;;    - maybe there's no editor, only a REPL, but readline could be
;;;      considered a poor-man's editor...
;;;  - edits
;;;    - Not sure if all "interactions" are going to be edits
;;;
;;;  Maybe it would be easier to find a name if we split the concepts
;;;  of "edits" and "user-interaction". Maybe the "edit backend" is
;;;  the lisp process doing modifications directly in the files (or
;;;  in-memory) and the "user-interaction backend" is McClim?
;;;
;;; The functions request-* could be method, dispatched on different
;;;  "clients"


(defun prototyping-stuff (&rest all
			  &key
			    buffer-string
			    buffer-name
			    buffer-file-name
			    point
			    point-min
			    point-max)
  (let* ((snippet (breeze.snippets:find-snippet
		   'breeze.snippets::defmacro))
	 (inputs (breeze.snippets:snippet-lambda-list snippet))
	 (fn (breeze.snippets:snippet-function snippet)))
    (with-send-and-receive
      (send-done
	(request-insert point (funcall fn))))))


(let* ((snippet (breeze.snippets:find-snippet
		 'breeze.snippets::defmacro))
       (inputs (breeze.snippets:snippet-lambda-list snippet))
       (fn (breeze.snippets:snippet-function snippet)))
  fn)


;;; The macros I made are very hard to use and debug, how could I make
;;; this simpler?
;;; Having a list of lambdas to call one after the other would be much
;;; simpler, but it would be hard to support control-flow (ifs, loops,
;;; etc.).
;;; I think a good approach would be that to return a new lambda along
;;; with the "request" and to use values instead of prog1.
;;; It should make things easier to debug.
;;; Also, I currently take advantage of the fact that the steps are
;;; defined as lambdas _inside_ the previous's step lambdas, closing
;;; over the newly sent parameters (response). I think I could just
;;; accumulate them in a list... and pass the whole list to the next
;;; lambda... we'll see


;;;; To make it easier to load [the right thing], I decided to
;;;; continue to try stuff in editor-interaction2.lisp.
