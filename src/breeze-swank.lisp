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
   #:get-recent-interactively-evaluated-forms)
  (:import-from #:breeze.xref
		#:classp))

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


;;; introspection

(defun get-symbol-package (symbol)
  (check-type symbol symbol)
  (format t "\"~(~a ~a~)\""
	  (package-name (symbol-package symbol))
	  (symbol-name symbol)))



;;; advising swank

(defvar *original-swank-interactive-eval* #'swank:interactive-eval
  "The original swank:interactive-eval function.")

(defparameter *recent-forms* ()
  "A list of recently-evaluated forms (as strings).")


(defmacro minimizing ((var) &body body)
  (check-type var symbol)
  (with-gensyms (score)
    `(let  ((,var nil)
	    (,score))
       (flet ((,var (new-candidate new-score)
		(when (or (not ,var)
			  (< new-score ,score))
		  (setf ,var new-candidate
			,score new-score))))
	 ,@body
	 ,var))))

#+nil
(minimizing (x)
  (x 'a 10)
  (x 'b 5))
;; => B

(defun find-most-similar-symbol (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (fboundp sym)
	(candidate sym
		   (breeze.utils:optimal-string-alignment-distance
		    input
		    (string-downcase sym)))))))

;; (find-most-similar-symbol "prin") ;; => princ

(defun find-most-similar-package (input)
  (minimizing (candidate)
    (loop :for package in (list-all-packages)
	  :for package-name = (package-name package) :do
	    (loop :for name in `(,package-name ,@(package-nicknames package)) :do
	      (candidate name (breeze.utils:optimal-string-alignment-distance
			       input
			       (string-downcase name)))))))

;; (find-most-similar-package "breeze.util") ;; => breeze.utils

(defun find-most-similar-class (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (classp sym)
	(candidate sym
		   (breeze.utils:optimal-string-alignment-distance
		    input
		    (string-downcase sym)))))))

(defun resignal-with-suggestion-restart (input candidate condition)
  ;; Ok, this is messy as hell, but it works
  (unless
      ;; We install a new restart
      (with-simple-restart (use-suggestion "Use \"~a\" instead of \"~a\"." candidate input)
	;; with-simple-restart returns the _last evaluated_ form
	t
	;; Then we signal the condition again
	(error condition))
    ;; with-simple-restart will return nil and t if the restart was invoked
    (let ((use-value (find-restart 'use-value condition)))
      (invoke-restart use-value candidate))))

(defun suggest (input candidate condition)
  ;; WARNING: Using a non-exported symbol from swank
  (swank::background-message "Did you mean \"~a\"?" candidate)
  (when candidate
    (let ((restart (find-restart 'use-value condition)))
      (or
       (and restart (resignal-with-suggestion-restart input candidate condition))
       (warn "Did you mean \"~a\"?~%~a"
	     candidate
	     (breeze.utils:indent-string
	      2 (breeze.utils:print-comparison nil
					       (string-downcase candidate)
					       input)))))))

(defun call-with-correction-suggestion (function)
  "Funcall FUNCTION wrapped in a handler-bind form that suggest corrections."
  (handler-bind
      ((undefined-function
	 #'(lambda (condition)
	     (let* ((input (string-downcase (cell-error-name condition)))
		    (candidate (find-most-similar-symbol input)))
	       (suggest input candidate condition))))
       (package-error
	 #'(lambda (condition)
	     (let* ((input (string-downcase
			    (package-name (package-error-package condition))))
		    (candidate (find-most-similar-package input)))
	       (suggest input candidate condition))))
       #+sbcl
       (sb-pcl:class-not-found-error
	 #'(lambda (condition)
	     (let* ((input (string-downcase (sb-kernel::cell-error-name condition)))
		    (candidate (find-most-similar-class input)))
	       (suggest input candidate condition)))))
    (funcall function)))

;; (prin)
;; (cl-suer:print :oups)
;; (call-with-correction-suggestion (lambda () (eval '(prin))))
;; (make-instance 'typos)

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
