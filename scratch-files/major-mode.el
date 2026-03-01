
;;; major mode

;;;###autoload
(define-derived-mode breeze-major-mode prog-mode
  "BRZ")

(keymap-set breeze-major-mode-map "M-RET" #'breeze-quickinsert)
(keymap-set breeze-major-mode-map "C-." #'breeze-quickfix)
(keymap-set breeze-major-mode-map "C-c C-c" #'breeze-eval-defun)
(keymap-set breeze-major-mode-map "C-c o" #'breeze-other-file)
(keymap-set breeze-major-mode-map "C-c C-o" #'breeze-other-file-other-window)

;; TODO use <remap> (keymap-set breeze-major-mode-map "<remap> <kill-line>" 'breeze-kill-line)
