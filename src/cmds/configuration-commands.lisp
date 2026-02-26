(defpackage #:breeze.configuration-commands
  (:documentation "Commands to help configure breeze.")
  (:use #:cl #:breeze.command
        #:breeze.command-utils)
  (:export #:open-config-file
           #:update-config-file
           #:reload-config))

(in-package #:breeze.configuration-commands)

;; TODO perhaps this could be more interactive?
(defun generate-config-file (config-file &key (if-exists :error))
  (with-open-file (out config-file
                             :direction :output
                             :if-exists if-exists
                             :if-does-not-exist :create)
          (format out "(in-package #:breeze)")
          (do-external-symbols (sym 'breeze.configuration)
            (when (breeze.xref:specialp sym)
              (format out "~%~%;; ~a~%(setf ~(~a~) ~s)"
                      (documentation sym 'variable)
                      sym
                      (symbol-value sym))))))

(defun ensure-config-directory ()
  "If the config directory does not exist, ask the user if they want to
create it, if they say no, stop the current command right away."
  (create-directory-or-abort (config-file-root)))

(define-command open-config-file ()
  "Open breeze's config file.
Generates the config file if it does not already exist."
  (ensure-config-directory)
  (let ((path (config-file-path)))
    ;; If it doesn't exists, fill it up!
    (unless (probe-file path)
      (generate-config-file path))
    (find-file path)))

(define-command update-config-file ()
  "Create or update breeze's config file."
  (ensure-config-directory)
  (let ((path (merge-pathnames "config.lisp" config-root)))
    (when (probe-file path)
      (unless (ask-y-or-n-p "This is going to overwrite the file ~s, are you sure?"
                            path)
        (message "Aborted by user")
        (return-from-command)))
    (generate-config-file path :if-exists :supersede)
    (find-file path)))

(define-command reload-config ()
  "Reload breeze's configuration file."
  (let ((path (config-file-path)))
    (if (probe-file path)
        (if (load-config-file)
            (message "Breeze's configuration file (re)loaded successfully.")
            (if (ask-y-or-n-p
                 "Breeze's configuration file ~s was not found, do you want to create it now?"
                 path)
                (open-config-file)
                (message "Aborted by user"))))))
