
(setf debug-on-error t)
(setf debug-on-error nil)

;; Useful for debugging whether slime or sly is running
(process-list)


;;; Reloading

(breeze-eval "(asdf:load-system '#:breeze :force t)")
(load breeze-breeze.el)


;;; Listener

(breeze-list-loaded-listeners)

(breeze-choose-listener)

(breeze-check-if-listener-connected)

(completing-read "Choose a lisp listener to start: "
                 '(sly slime) nil t)


;;; Eval

(breeze-eval "1")
(breeze-eval "'(a b c)")
(breeze-eval "t")
(breeze-eval "(not nil)")



;;; Initialization

(breeze-validate-if-package-exists "cl")

(breeze-validate-if-package-exists "breeze")

(breeze-validate-if-breeze-package-exists)


;;; "Dynamic" emacs commands

(breeze-translate-command-lambda-list '(a b c))

(breeze-translate-command-lambda-list '(a::1 b:2 c::3))

(breeze-refresh-commands)

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
