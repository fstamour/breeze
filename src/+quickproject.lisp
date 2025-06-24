(defpackage #:breeze+quickproject
  (:documentation "Integration with quickproject.")
  (:use #:cl)
  (:import-from #:breeze.command
                #:define-command
                #:read-string
                #:find-file
                #:message)
  (:import-from #:breeze.project
                #:choose-local-project-directories
                #:confirm-scaffold-directory
                #:read-string)
  (:import-from #:breeze.config
                #:*default-author*
                #:*default-system-licence*)
  (:export #:breeze-quickproject))

(in-package #:breeze+quickproject)

(define-command breeze-quickproject (project-name directory)
  "Create a project interactively using quickproject."
  (let* (;; TODO Currently the user is able to enter an empty string
         (project-name
           (or project-name
               (read-string "Name of the project: ")))
         (directory
           (confirm-scaffold-directory
            (uiop:ensure-directory-pathname
             (or directory
                 (merge-pathnames project-name
                                  (choose-local-project-directories))))))
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
