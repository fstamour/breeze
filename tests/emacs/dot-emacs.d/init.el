;;;; -*- lexical-binding: t -*-
;;;; ~/.emacs file for use in automated tests


;;; Configuration to load breeze

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


;;; Custom variables

(custom-set-variables
 ;;; Theme
 '(custom-enabled-themes '(leuven-dark))
 ;; This tells sly to go ahead and replace slime without asking
 ;; TODO it looks like this doesn't work in my current version of sly given by my current version of guix
 ;; it was added in ba40c8f054ec in may 2024
 '(sly-replace-slime t))


;;; Font size (height) - because running emacs in a docker using ssh's
;;; X forwarding doesn't care about high DPI

(cl-defun -set-face-height (height &optional frame)
  "Change the frame's default font height."
  (set-face-attribute 'default
                      (or frame (selected-frame))
                      :height height))

;; Change the font size
(let* ((height 150)
       (hook (lambda ()
               (-set-face-height height))))
  ;; Change the current frame's default font height
  (-set-face-height 135)
  ;; Change future frames' default font height
  (add-hook 'after-make-frame-functions hook)
  (add-hook 'server-switch-hook hook))



;; Might want to configure these:
;; `process-query-on-exit-flag'
;; `confirm-kill-processes'


;;; Integration with other modes

(which-key-mode 1)


;;; Making this super ez to use

(require 'button)

(define-derived-mode breeze-startup-manual-test-mode fundamental-mode
  "Breeze's startup buffer for manual test's mode"
  "A mode to help manually testing breeze")

(keymap-set breeze-startup-manual-test-mode-map "<tab>" 'forward-button)
(keymap-set breeze-startup-manual-test-mode-map "<backtab>" 'backward-button)


(push (expand-file-name "~/breeze/") safe-local-variable-directories)

(defun insert-command-button-item (command &optional description)
  (insert " - ")
  (insert-button
   (if (listp command)
       (format "M-: %S" command)
     (format "M-x %s" command))
   'action (lambda (x)
             (if (listp command)
                 (apply (car command) (cdr command))
               (call-interactively command t))))
  (when description
    (insert " " description))
  (insert "\n"))

(defun breeze-run-demo ()
  (interactive)
  (load-file "~/breeze/scripts/demo/demo.el"))

;; TODO org-roam
;; TODO tags.org - validate the tasks's tags
;; TODO workbench.el - udpate breeze.el
;; TODO update autoloads
;; TODO run the tests (e.g. using makefile)
;; TODO run ert tests
;; TODO integration tests?
;; TODO maybe even other editors?!? (i.e. use emacs as a command runner/shell)

(setf initial-buffer-choice
      (lambda ()
        (let ((buffer (get-buffer-create "*breeze-startup-manual-testing*"))
              (initital-point))
          (with-current-buffer buffer
            (insert "Welcome, here are some things to get you started: \n")
            (setf initital-point (+ 3 (point)))
            (insert-command-button-item 'breeze-init)
            (insert-command-button-item 'slime)
            (insert-command-button-item 'sly)
            (insert-command-button-item
             'breeze-run-demo
             "Load the demo!")
            (insert-command-button-item
             '(find-file "~/breeze/scripts/demo/demo.el")
             "Open demo.el!")
            (insert-command-button-item
             '(find-file-other-window "~/breeze/")
             "Open breeze's directory")
            (insert-command-button-item
             '(find-file-other-window "~/breeze/tests/emacs/")
             "Open breeze's emacs tests directory")
            (insert-command-button-item
             '(find-file-other-window "~/.emacs.d/init.el")
             "Open emacs' init file.")
            ;; Keep the "restar-emacs" last
            (insert-command-button-item
             'restart-emacs
             "Restart emacs - useful for reloading emacs' init files")
            (insert "\n\n")
            (insert "Notes:\n")
            (insert " - M-x breeze-init and M-x slime or M-x sly should do pretty much the same thing, except that breeze-init will ask to choose between sly and slime.\n")
            (insert " - The M-x command history is populated with some commands.")
            (insert "\n")
            (goto-char initital-point)
            (breeze-startup-manual-test-mode)
            (read-only-mode))
          buffer)))

;; Add some commands to M-x's history
(push "slime" extended-command-history)
(push "sly" extended-command-history)
(push "breeze-init" extended-command-history)
(push "breeze-capture" extended-command-history)
(push "breeze-quickproject" extended-command-history)
(push "toggle-debug-on-error" extended-command-history)
(push "restart-emacs" extended-command-history)
