;;; Common lisp side of breeze.el

(in-package #:common-lisp-user)

(defpackage #:breeze/el
  (:use :cl)
  (:export
   #:make-project
   #:get-ql-local-project-directories
   #:advise-swank-interactive-eval
   #:restore-swank-interactive-eval
   #:get-recent-interactively-evaluated-forms))

(in-package #:breeze/el)

(defun insert-at (current-text text-to-insert position)
  (with-output-to-string (stream)
    (princ (subseq current-text 0 position) stream)
    (format stream text-to-insert)
    (princ (subseq current-text position) stream)))

(defun test-insert-at (pre post insert)
  (insert-at (concatenate 'string pre post)
	     insert
	     (length pre)))

#+nil
(equal
 (test-insert-at "(defun f1 ()" ")"
		 "~&(let (()))")
 (test-insert-at 
  "(defun f1 ()
" ")"
  "~&(let (()))"))

(defun insert-let (string position)
  (insert-at string
	     "~&(let (()))"
	     position))



;;; project scaffolding

(defun make-project (&rest args)
  ;; make-project pathname &key depends-on author include-copyright license name template-directory template-parameters => project-name 
  (apply #'quickproject:make-project args))

(defun get-ql-local-project-directories ()
  (mapcar #'namestring
	  ql:*local-project-directories*))



;;; advising swank

(defvar *original-swank-interactive-eval* #'swank:interactive-eval
  "The original swank:interactive-eval function.")

(defparameter *recent-forms* ()
  "A list of recently-evaluated forms (as strings).")

(defun interactive-eval (string)
  "Breeze's interactive-eval."
  (pushnew string *recent-forms* :test #'string=)
  ;; (format t "~&Interactive-eval: ~A~%" string)
  (funcall *original-swank-interactive-eval* string))

(defun %interactive-eval (string)
  (funcall 'interactive-eval string))

(defun advise-swank-interactive-eval ()
  (setf (symbol-function 'swank:interactive-eval) #'%interactive-eval))

(defun restore-swank-interactive-eval ()
  (setf (symbol-function 'swank:interactive-eval)
	*original-swank-interactive-eval*))

;; (advise-swank-interactive-eval)
;; (restore-swank-interactive-eval)

;; TODO cleanup *recent-forms* from time to time.
;; TODO maybe uses a hash-table instead of a list for *recent-forms* to keep a
;;  kind of frequency-table

(defun get-recent-interactively-evaluated-forms ()
  *recent-forms*)
