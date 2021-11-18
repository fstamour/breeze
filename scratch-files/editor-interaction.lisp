(in-package #:breeze.quickfix)

(defparameter *response-counter* 0)
(defparameter *state* ())
(defparameter *next* ())

(defun request-done ()
  "Send a message to the editor telling it we're done."
  nil)

(defun request-completing-read (prompt collection)
  "Send a message to the editor to ask the user to choose one element in the collection."
  `("completing-read"
    ,prompt
    ,collection))

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
  (request-completing-read
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
