
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


;;; Flymake

flymake-diagnostic-functions

(flymake-disabled-backends)

;; - invoking flymake-start with a prefix argument is a way to reset the
;; disabled backend list
;; - so that they will be tried again in the next check.
;; - Manually toggling flymake-mode off and on again also works.



;;; flymake example of "recent changes"

;; writing "asdf"
;; here we see that the first entry is the most recent.
(:recent-changes ((16191 16192 #("f" 0 1 (fontified t)))
                  (16190 16191 #("d" 0 1 (fontified t)))
                  (16189 16190 #("s" 0 1 (fontified t)))
                  (16188 16189 #("a" 0 1 (fontified t))))
                 :changes-start 16188 :changes-end 16192)

;; (insert "asdf")
;; one edit = 1 change
(:recent-changes ((16188 16192 "asdf")) :changes-start 16188 :changes-end 16192)

;; deleting (backward) 5 chars in a row
(:recent-changes ((16186 16186 "")
                  (16187 16187 "")
                  (16188 16188 "")
                  (16189 16189 "")
                  (16190 16190 ""))
                 :changes-start 16186 :changes-end 16190)

;; deleting (forward)
(:recent-changes ((17460 17460 "")
                  (17460 17460 "")
                  (17460 17460 "")
                  (17460 17460 "")) :changes-start 17460 :changes-end 17460)

;; C-k to delete a bunch of chars
(:recent-changes ((16181 16181 "")) :changes-start 16181 :changes-end 16181)

;; here's a big issue: with what flymakes provides, we cannot know
;; when multiple characters are deleted at once :/

;; flymake uses a hook:
;; after-change-functions

;; Three arguments are passed to each function:
;; 1. beginning of the range of changed text
;; 2. end of the range of changed text
;; 3. length in chars of the pre-change text replaced by that range.

;; (zerop length) => insertion
;; (if (= start end)) => deletion, (plusp length)
