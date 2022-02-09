
(defvar breeze/status
  "OK"
  "TBD Status of the current project.")


;;; Mode-line indicator

(defun breeze/set-status (new-status)
  "Update the status variable with NEW-STATUS and update the mode-line."
  (setf breeze/status new-status)
  (force-mode-line-update))

(defun breeze/configure-mode-line ()
  "Add breeze's status to the mode-line-format, if not already there."
  (interactive)
  (unless
      (cl-remove-if-not
       #'(lambda (el)
	   (and (listp el)
		(eq 'breeze/status (car el))))
       mode-line-format)
    (setf mode-line-format
	  (append mode-line-format
		  '((breeze/status  ("--> " breeze/status " <--")))))))

;; (breeze/configure-mode-line)
