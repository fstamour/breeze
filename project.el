(require 'ido)

(defun pick-localproject-directories (choices)
  "Prompt user to pick a choice from a list."
  (interactive)
  (message "%s" (ido-completing-read "Choose " choices)))

(setq slime-enable-evaluate-in-emacs t)


(completing-read "")

(slime-eval `(cl-user:list-local-project-directories))

