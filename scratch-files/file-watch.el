;;; File watching

(require 'filenotify)

(defun breeze-on-dir-change (event)
  ;; (cl-destructuring-bind (descriptor action file &optional file1))
  (message "breeze-on-dir-change: %S" event))


;; List all sub-directories using fd
;; TODO this should include the "root"
(dolist (dir (split-string
              (shell-command-to-string "fd . \"/home/fstamour/quicklisp/local-projects/breeze/\" -t d -0")
              (string ?\0) 'omit-nulls))
  (file-notify-add-watch dir '(change) 'breeze-on-dir-change))

;; $ touch src/boop
;; => breeze-on-dir-change: ((2 . 3) created "/home/fstamour/quicklisp/local-projects/breeze/src/boop")
