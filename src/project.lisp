;;; TODO Look into other existing solutions than quickproject
;;; - https://github.com/fukamachi/cl-project
;;; - https://github.com/40ants/cl-project-with-docs
;;; - https://github.com/vindarel/cl-cookieproject

(defpackage #:breeze.project
  (:documentation "Project scaffolding utilities")
  (:use #:cl)
  (:export #:scaffold-project))

(in-package #:breeze.project)

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


;; TODO defun choose-local-project-directories

;; TODO
#+ (or)
(defun scaffold-project ()
  "Create a project named NAME using quickproject."
  (interactive)
  (let ((name (read-string "Name of the project: "))
	;; TODO let the user choose a directory outside of quicklisp's local
	;; project directories.  see (read-directory-name "directory: ").
	(directory (breeze-choose-local-project-directories))
	;; TODO let the user choose
	(author user-full-name)
	(licence "Public domain")
	;; TODO depends-on
	;; TODO include-copyright
	;; TODO template-directory
	;; TODO template-parameters
	)
    (slime-interactive-eval
     (concat
      "(breeze.listener:make-project \"" directory name "\""
      " :author \"" author "\""
      " :license \"" licence "\""
      ")"))
    (message "\"%s\" created" (concat directory name "/"))
    (find-file (concat directory name "/README.md"))))
