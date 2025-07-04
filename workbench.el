
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

(symbol-function 'breeze-quickfix)


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




(defun breeze/sldb-show-all-details (&optional on)
  (interactive)
  (cl-assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (cl-loop for i below 200
             do
             (sldb-forward-frame)
             (sldb-show-frame-details))))



;;; TODO show number of workers in header-line

(breeze-%eval `(cl:eval '(cl:length (breeze.thread:find-worker-threads))))
(breeze-eval "(cl:length (breeze.thread:find-worker-threads))")


;;; WIP Lisp listener state

(defun breeze-find-slime-connections ()
  (cl-remove-duplicates
   (cl-loop for buffer the buffers
            when (and (equal (buffer-local-value 'major-mode buffer)
                             'slime-repl-mode))
            collect (buffer-local-value 'slime-buffer-connection buffer))))

(breeze-find-slime-connections)

(defun breeze-find-slime-repl-buffers ()
  (cl-loop for buffer the buffers
           when (and (equal (buffer-local-value 'major-mode buffer) 'slime-repl-mode))
           collect buffer))

(breeze-find-slime-repl-buffers)

(defun breeze-slime-repl-input-history (buffer)
  (mapcar (lambda (entry)
            (substring-no-properties entry))
          (buffer-local-value 'slime-repl-input-history buffer)))

(breeze-slime-repl-input-history
 (cl-first (breeze-find-slime-repl-buffers)))



;;; TODO detect when sbcl entered LDB

(with-current-buffer "*inferior-lisp*"
  (save-excursion
    (goto-char (point-max))
    (search-backward "Welcome to LDB, a low-level debugger for the Lisp runtime environment.")))

(breeze-eval
 "(labels ((boom (x)
               (boom x)))
        (boom #\\💀))")

(process-filter
 (breeze-listener-connected-p))
slime-net-filter

(comint-check-proc)

(let* ((buffer (get-buffer "*inferior-lisp*"))
       (process (get-buffer-process buffer)))
  (process-command process))
("sbcl" "--noinform" "--dynamic-space-size" "16000")

(let* ((buffer (get-buffer "*inferior-lisp*"))
       (process (get-buffer-process buffer)))
  process)


slime-net-processes



;;; Enabling/disabling breeze

(breeze-disabled-p)

?\
?\


;;; generating stubs

(defun breeze--update-command-stubs ()
  (interactive)
  (breeze-refresh-commands)
  (save-excursion
    (with-current-buffer (find-file-noselect breeze-breeze.el)
      ;; TODO maybe "unnarrow"
      (goto-char (point-max))
      (let* ((end (search-backward "" nil t))
             (start (progn
                      (search-backward "" nil t)
                      ;; Skip the C-l and C-m chararcters
                      (forward-line)
                      ;; inspecting, just to make sure
                      ;; (thing-at-point 'line t)
                      ;; Skip the ";;; This page ...."
                      (forward-line)
                      (point))))

        (buffer-substring-no-properties start end)

        (replace-region-contents
         start end
         (lambda ()
           "hello\n"))

        ;; TODO replace, not just insert
        ;; (insert (format "%S" (breeze-list-commands)))
        ))))


(save-excursion
  (with-current-buffer (find-file-noselect breeze-breeze.el)
    ;; TODO maybe "unnarrow"
    (goto-char (point-max))
    (let* ((end (search-backward "\n\n" nil t))
           (start (progn
                    (search-backward "" nil t)
                    ;; Skip the C-l and C-m chararcters
                    (forward-line)
                    ;; inspecting, just to make sure
                    ;; (thing-at-point 'line t)
                    ;; Skip the ";;; This page ...." and leave an
                    ;; empty line
                    (forward-line 2)
                    (point))))

      (replace-region-contents
       start end
       (lambda ()
         (with-temp-buffer
           (dolist (command (breeze-list-commands))
             (insert (format "%S\n" `(breeze--defstub ,command ,(documentation command t)))))
           (buffer-substring-no-properties (point-min) (point-max))))))))




(documentation 'breeze-quickfix t)
(symbol-plist )
