(defpackage #:breeze.command-utils
  (:documentation "Utilities to write commands")
  (:use #:cl #:breeze.command #:breeze.analysis)
  (:export #:create-directory-or-abort
           #:pulse-node
           #:current-node))

(in-package #:breeze.command-utils)

(defun create-directory-or-abort (directory)
  (unless (probe-file directory)
    (cond
      ((ask-y-or-n-p "The directory ~s doesn't exist, do you want to create it?")
       (ensure-directories-exist directory))
      (t (message "Aborted by user")
         (return-from-command)))))

(defun node-at-point ())

;; TODO This should go in a file for "commands that uses parse trees"
(defun pulse-node (node)
  (pulse (start node) (end node)))

(defun current-node (&key pulsep)
  (alexandria:when-let*
      ((buffer (current-buffer))
       (node-iterator (node-iterator buffer)))
    (when pulsep (pulse-node node-iterator))
    node-iterator))

;; TODO Maybe make a command "replace-form" or "replace-car"

#++ ;; TODO this is work in progress
(defun ensure-in-package (package-name)
  "Insert an ~cl:in-package~ form, if necessary."
  (let (($package (current-package))))
  (insert "(cl:in-package #:asdf-user)~%~%"))


;; TODO method "ask licence" and "ask author"
;; (methods, not defun, for better testability and customization)
