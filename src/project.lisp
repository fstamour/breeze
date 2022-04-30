;;; TODO Look into other existing solutions than quickproject
;;; - https://github.com/fukamachi/cl-project
;;; - https://github.com/40ants/cl-project-with-docs
;;; - https://github.com/vindarel/cl-cookieproject

(defpackage #:breeze.project
  (:documentation "Project scaffolding utilities")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:length>1?)
  (:import-from #:breeze.command
                #:define-command
                #:read-string
                #:choose
                #:message
                #:find-file)
  (:import-from #:breeze.config
                #:*default-author*
                #:*default-system-licence*)
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

(defun ql-local-project-directories ()
  "Get the list of quicklisp local-projects directories (as strings)."
  (mapcar #'namestring
          ql:*local-project-directories*))

(defun choose-local-project-directories ()
  (let ((directories (ql-local-project-directories)))
    (cond
      ((null directories)
       (read-string
        "Please enter the directory where to create the project: "))
      ((length>1? directories)
       (choose "Please choose where to create the project: "
               directories))
      (t
       (first directories)))))

(define-command scaffold-project (project-name directory)
  "Create a project interactively using quickproject."
  (let* ((project-name
           (or project-name
               (read-string "Name of the project: ")))
         (directory
           ;; TODO What if the directory already exists?
           (uiop:ensure-directory-pathname
            (or directory
                (merge-pathnames  project-name
                                  (choose-local-project-directories)))))
         (author (read-string "Author of the project: "
                              *default-author*))
         (license (read-string "Licence of the project: "
                               *default-system-licence*)))
    ;; TODO depends-on
    ;; TODO include-copyright
    ;; TODO template-directory
    ;; TODO template-parameters
    ;; TODO add validations
    (quickproject:make-project
     directory
     :name project-name
     :author author
     :license license)
    (message "Project \"~a\" created." directory)
    (find-file directory)))
