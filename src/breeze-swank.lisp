;;; Common lisp side of breeze.el

(in-package #:common-lisp-user)

(defpackage #:breeze.swank
  (:use :cl #:alexandria)
  (:documentation "Backend side of integration with swank")
  (:export
   #:make-project
   #:get-ql-local-project-directories
   #:advise-swank-interactive-eval
   #:restore-swank-interactive-eval
   #:get-recent-interactively-evaluated-forms))

(in-package #:breeze.swank)


;;; project scaffolding

(defun make-project (&rest args
		     &key depends-on author include-copyright license name template-directory template-parameters)
  (declare (ignore depends-on author include-copyright license name template-directory template-parameters))
  "Scaffold a project. Currently it's just a wrapper on quickproject's make-project."
  (apply #'quickproject:make-project args))

(defun get-ql-local-project-directories ()
  "Get the list of quicklisp local-projects directories (as strings)."
  (mapcar #'namestring
	  ql:*local-project-directories*))



;;; advising swank

(defvar *original-swank-interactive-eval* #'swank:interactive-eval
  "The original swank:interactive-eval function.")

(defparameter *recent-forms* ()
  "A list of recently-evaluated forms (as strings).")

(defun call-with-correction-suggestion (function)
  "Funcall FUNCTION wrapped in a handler-bind form that suggest corrections."
  (handler-bind
      ((undefined-function #'(lambda (condition)
			       (let ((input (string-downcase (cell-error-name condition)))
				     (candidate nil)
				     (candidate-distance 0))
				 (do-symbols (sym)
				   (when (fboundp sym)
				     (let ((distance (breeze.utils:optimal-string-alignment-distance
						      input
						      (string-downcase sym))))
				       (when (or (not candidate)
						 (< distance candidate-distance))
					 (setf candidate sym
					       candidate-distance distance)))))
				 (warn "Did you mean \"~a\"?~%~a"
				       candidate
				       (breeze.utils:indent-string
					2 (breeze.utils:print-comparison nil
									 (string-downcase candidate)
									 input)))))))
    (funcall function)))


;; (call-with-correction-suggestion (lambda () (eval '(prin))))

(defparameter *interactive-eval-hooks* '())

(defun interactive-eval (string)
  "Breeze's interactive-eval."
  (pushnew string *recent-forms* :test #'string=)
  ;; (format t "~&Interactive-eval: ~A~%" string)
  (call-with-correction-suggestion
   (lambda ()
     ;; (swank::with-buffer-syntax () (eval string))
     (prog1
	 (funcall *original-swank-interactive-eval* string)
       (loop
	  :for (name . hook) :in *interactive-eval-hooks*
	  :do
	    (handler-case
		(funcall hook string)
	      (error (condition)
		(format *error-output*
			"~&Error signaled while running \"~a\" interactive-eval hook: ~a~%  "
			name
			condition))))))))

(defun %interactive-eval (string)
  (funcall 'interactive-eval string))

(defun advise-swank-interactive-eval ()
  "Advise swank:interactive-eval."
  (unless (eq (symbol-function 'swank:interactive-eval) #'%interactive-eval)
    (setf (symbol-function 'swank:interactive-eval) #'%interactive-eval)))

(defun restore-swank-interactive-eval ()
  "Unadvise swank:interactive-eval."
  (setf (symbol-function 'swank:interactive-eval)
	*original-swank-interactive-eval*))

;; (advise-swank-interactive-eval)
;; (restore-swank-interactive-eval)

;; TODO cleanup *recent-forms* from time to time.
;; TODO maybe uses a hash-table instead of a list for *recent-forms* to keep a
;;  kind of frequency-table

(defun get-recent-interactively-evaluated-forms ()
  "Get the 50 most recently evaluated forms"
  (loop :for form :in *recent-forms*
     :for i :below 50
     :do (format t "~&~a~%"
		 (remove #\newline form))))
