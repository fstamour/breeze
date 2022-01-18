;;; Common lisp side of breeze.el

(in-package #:common-lisp-user)

(defpackage #:breeze.listener
  (:use :cl #:alexandria)
  (:documentation "Backend side of integration with swank")
  (:export
   #:make-project
   #:get-ql-local-project-directories
   #:advise-swank-interactive-eval
   #:restore-swank-interactive-eval
   #:get-recent-interactively-evaluated-forms)
  (:import-from #:breeze.xref
		#:classp)
  (:import-from #:breeze.utils
		#:optimal-string-alignment-distance*))

(in-package #:breeze.listener)


;;; project scaffolding

(defun make-project (&rest args
		       ;; &key depends-on author include-copyright license
		       ;; name template-directory template-parameters
		       ;; &allow-other-keys
		       )
  ;; (declare (ignore depends-on author include-copyright license name
  ;; template-directory template-parameters))
  "Scaffold a projec using quickproject's make-project."
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

;; TODO Use a heap to get the N smallest values!
;; TODO Put that into utils?
(defmacro minimizing ((var
		       &key
			 (score-var (gensym "score"))
			 tracep)
		      &body body)
  "Creates both a variable (let) and a function (flet) to keep track
of the instance of that had the smallest score."
  (check-type var symbol)
  `(let  ((,var nil)
	  (,score-var))
     (flet ((,var (new-candidate new-score)
	      ,@(when tracep
		  `((format *debug-io* "~&new-candidate: ~s new-score: ~s"
			    new-candidate new-score)))
	      (when (and new-score
			 (or
			  ;; if it wasn't intialized already
			  (null ,var)
			  ;; it is initialized, but score is better
			  (< new-score ,score-var)))
		(setf ,var new-candidate
		      ,score-var new-score))))
       ,@body
       (values ,var ,score-var))))


(defun find-most-similar-symbol (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (fboundp sym)
	(candidate sym
		   (breeze.utils:optimal-string-alignment-distance*
		    input
		    (string-downcase sym)
		    3))))))

;; (find-most-similar-symbol "prin") ;; => princ, 1

(defun find-most-similar-package (input)
  (minimizing (candidate)
    (loop :for package in (list-all-packages)
	  :for package-name = (package-name package) :do
	    (loop :for name in `(,package-name ,@(package-nicknames package)) :do
	      (candidate name
			 (breeze.utils:optimal-string-alignment-distance*
			  input
			  (string-downcase name)
			  3))))))

#+ (or)
(progn
  (find-most-similar-package "breeze.util")
  ;; => breeze.utils, 1

  (find-most-similar-package "commmon-lisp")
  ;; => "COMMON-LISP", 1
  )

(defun find-most-similar-class (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (classp sym)
	(candidate sym
		   (breeze.utils:optimal-string-alignment-distance*
		    input
		    (string-downcase sym)
		    3))))))

(defvar *last-invoked-restart* nil
  "For debugging purposes only")

(defun resignal-with-suggestion-restart (input candidate condition)
  ;; Ok, this is messy as hell, but it works
  (unless
      ;; We install a new restart
      (with-simple-restart (use-suggestion
			    "Use \"~a\" instead of \"~a\"."
			    candidate input)
	;; with-simple-restart returns the _last evaluated_ form
	t
	;; Then we signal the condition again
	(error condition))
    ;; with-simple-restart will return nil and t if the restart was
    ;; invoked
    (let ((use-value (find-restart 'use-value condition)))
      (setf *last-invoked-restart* (list candidate))
      (format *debug-io* "~&About to invoke the restart ~s with the value ~s."
	      use-value
	      candidate)
      ;; (describe use-value)
      ;; (inspect use-value)
      (invoke-restart use-value candidate))))

(defun suggest (input candidate condition)
  ;; WARNING: Using a non-exported symbol from swank
  (swank::background-message "Did you mean \"~a\"?" candidate)
  (when candidate
    (let ((restart (find-restart 'use-value condition)))
      (or
       (and restart (resignal-with-suggestion-restart
		     input candidate condition))
       (warn "Did you mean \"~a\"?~%~a"
	     candidate
	     (breeze.utils:indent-string
	      2
	      (breeze.utils:print-comparison
	       nil
	       (string-downcase candidate)
	       input)))))))

(defgeneric condition-suggestion-input (condition)
  (:documentation "Get input for \"find-most-similar-*\" functions from a condition")
  ;; Default implementation
  (:method (condition)
    (cell-error-name condition))
  (:method ((condition undefined-function))
    (format *debug-io* "~&1")
    (cell-error-name condition))
  (:method ((condition package-error))
    (let ((package-designator
	    (package-error-package condition)))
      (if (stringp package-designator)
	  package-designator
	  #+sbcl ;; only tested on sbcl
	  (car
	   (slot-value condition
		       'sb-kernel::format-arguments)))))
  #+sbcl
  (:method ((condition sb-ext:package-does-not-exist))
    (package-error-package condition))
  #+sbcl
  (:method ((condition sb-pcl:class-not-found-error))
    (sb-kernel::cell-error-name condition)))

;; (trace condition-suggestion-input)

(defmacro defun-suggest (types)
  `(progn
     ,@(loop
	 :for type :in types
	 :collect
	 `(defun ,(symbolicate 'suggest- type) (condition)
	    (let* ((input (string-downcase (condition-suggestion-input condition)))
		   (candidate (,(symbolicate 'find-most-similar- type) input)))
	      #+ (or)
	      (format *debug-io*
		      ,(format nil
			       "~~&candidate ~(~a~): ~~s"
			       type)
		      candidate)
	      (if candidate
		  (suggest input candidate condition)
		  (error condition)))))))

(defun-suggest
    (symbol
     package
     class))

#+ (or)
(trace suggest-symbol
       suggest-package
       suggest-class)

#+ (or)
(progn
  ;; List the slot of a condition
  (sb-kernel::condition-assigned-slots *condition*)

  ;; Get the first element of a condition's format arguments
  (car
   (slot-value *condition*
	       'sb-kernel::format-arguments)) )

(defvar *last-condition* nil
  "For debugging purposose only.")

#+ (or)
(defparameter *condition* *last-condition*
  "Just a quick way to save the last-condition.")

#+ (or)
(type-of *condition*)
;; => SB-PCL::MISSING-SLOT

(defun call-with-correction-suggestion (function)
  "Funcall FUNCTION wrapped in a handler-bind form that suggest corrections."
  (handler-bind
      ((error #'(lambda (condition)
		  (setf *last-condition* condition)
		  (error condition))))
    (handler-bind
	;; The order is important!!!
	((undefined-function #'suggest-symbol)
	 #+sbcl (sb-ext:package-does-not-exist #'suggest-package)
	 #+sbcl (sb-int:simple-reader-package-error #'suggest-symbol)
	 #+ (or)
	 (package-error #'suggest-package)
	 #+sbcl
	 (sb-pcl:class-not-found-error #'suggest-class))
      (funcall function))))

;; (prin t)
;; (commmon-lisp:print :oups)
;; (cl:prin :oups)
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

(defun find-worker-threads ()
  (let ((current-thread (bt:current-thread)))
    (remove-if-not #'(lambda (thread)
		       (and (not (eq current-thread thread))
			    (string= "worker" (bt:thread-name thread))))
		   (sb-thread:list-all-threads))))

(defun kill-worker-threads ()
  (let ((threads (find-worker-threads)))
    (when threads
      (mapcar #'bordeaux-threads:destroy-thread threads))
    (format t "Killed ~d threads." (length threads))))

#|
(kill-worker-threads)
|#
