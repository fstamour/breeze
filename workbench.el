
(setf debug-on-error t)
(setf debug-on-error nil)

;; Useful for debugging whether slime or sly is running
(process-list)




(breeze-list-loaded-listeners)

(breeze-choose-listener)

(breeze-check-if-listener-connected)

(completing-read "Choose a lisp listener to start: "
                 '(sly slime) nil t)




(breeze-eval "1")
(breeze-eval "'(a b c)")
(breeze-eval "t")
(breeze-eval "(not nil)")




(breeze-validate-if-package-exists "cl")

(breeze-validate-if-package-exists "breeze")

(breeze-validate-if-breeze-package-exists)



;; I don't remember why I needed this? maybe I had redefined the
;; defcommand macro.
(defun breeze-reload ()
  (breeze-eval "(asdf:load-system '#:breeze :force t)"))

(load breeze-breeze.el)


;;;

(breeze-translate-command-lambda-list '(a b c))

(breeze-translate-command-lambda-list '(a::1 b:2 c::3))

(symbol-function 'breeze-scaffold-project)


;;; Trying to make breeze system load automatically... and
;;; asynchronously if it make sense.

(breeze-add-hooks 'slime)
(breeze-add-hooks 'sly) ; not implemented yet


;;; Other listener hooks

slime-connected-hook
slime-inferior-process-start-hook
slime-net-process-close-hooks
slime-cycle-connections-hook
slime-connected-hook
slime-event-hooks

;; contribs (like slime-repl) defines even more hooks
