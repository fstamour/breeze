(defvar breeze-temporary-buffers '())

(defun breeze-make-temporary-buffer ()
  "Make a buffer name, add it to the list."
  (let ((buffer-name (make-temp-name (concat "tmp-" (format-time-string "%Y-%m.%dT%H.%M.%S-")))))
    (push buffer-name breeze-temporary-buffers)
    buffer-name))

(defun breeze-kill-all-temporary-buffer ()
  "Kill all buffers."
  (interactive)
  (dolist (buffer-name breeze-temporary-buffers)
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name)))
  (setf breeze-temporary-buffers '()))
