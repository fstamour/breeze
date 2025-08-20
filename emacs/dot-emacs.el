;;;; ~/.emacs file for use in automated tests

(custom-set-variables
 '(custom-enabled-themes '(leuven-dark)))

(add-to-list 'load-path
             (expand-file-name "~/breeze/emacs/"))

(require 'breeze-autoloads)

(with-eval-after-load 'slime
  ;; Ensure breeze is loaded when a slime connection is opened
  (add-hook 'slime-connected-hook
            'breeze-connected-hook-function))

;; Enable breeze-minor-mode in lisp-mode
(add-hook 'lisp-mode-hook #'breeze-minor-mode)

(with-eval-after-load 'breeze
  ;; Enable flymake-mode in breeze-minor-mode
  (breeze-minor-mode-enable-flymake-mode))
