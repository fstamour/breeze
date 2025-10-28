;; -*- lexical-binding: t -*-
;;; package -- breeze integration with emacs

;;; Commentary:
;;

;;; Code:


;;; Requires

(require 'cl-lib)


;;; Logging

(defun breeze-debug (string &rest objects)
  "Log a meesage in the \" *breeze-debug*\" buffer."
  (save-current-buffer
    (set-buffer (get-buffer-create " *breeze-debug*"))
    (setf buffer-read-only nil)
    (goto-char (point-max))
    (insert
     "\n"
     (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] ")
     (let ((s (apply #'format string objects)))
       (if (< (length s) 1000)
           s
         (cl-subseq s 0 1000))))
    (setf buffer-read-only t)))

(defun breeze-message (string &rest objects)
  "Log a meesage in both \" *breeze-debug*\" and *Messages* buffers."
  (apply #'message string objects)
  (apply #'breeze-debug string objects))


;;; Utilities for integrating with other packages that might or might
;;; not be loaded.

(defun breeze-fbound-p (symbol)
  "Like `fboundp' but returns SYMBOL instead of `t' when true."
  (and (fboundp symbol) symbol))

(defun breeze-keep-fbound (symbols)
  (remove 'nil (mapcar 'breeze-fbound-p symbols)))

(cl-defun breeze-remove-nil (&rest args)
  (remove 'nil args))

(defun breeze-symbol-value (symbol)
  (when (boundp symbol)
    (symbol-value symbol)))

(cl-defun breeze-funcall (symbol &rest args)
  (when (fboundp symbol)
    (apply symbol args)))

(defun breeze-add-hook (hook function &optional depth local)
  (when (boundp hook)
    (add-hook hook function depth local)))

(defun breeze-remove-hook (hook function &optional local)
  (when (boundp hook)
    (remove-hook hook function local)))


;;; Lisp listener state

(defun breeze-sly-connected-p ()
  "Check if sly loaded and connected."
  (or
   (breeze-symbol-value 'sly-dispatching-connection)
   (breeze-symbol-value 'sly-default-connection)))

(defun breeze-slime-connected-p ()
  "If slime is loaded and connected."
  (or
   (breeze-symbol-value 'slime-dispatching-connection)
   (breeze-symbol-value 'slime-default-connection)))

(cl-defun breeze-listener-connected-p (&optional (errorp t))
  "Check if either sly or slime is loaded and connected."
  (or (breeze-sly-connected-p)
      (breeze-slime-connected-p)
      (and errorp
           (error "Please start either slime or sly."))))

(cl-defun breeze-list-loaded-listeners (&optional (errorp t))
  "Returns a list of loaded listeners (sly or slime)."
  (or (breeze-keep-fbound '(slime sly))
      (and errorp
           (error "Please load either slime or sly."))))

(cl-defun breeze-list-connected-listeners ()
  "Returns a list of connected listeners (sly or slime)."
  (breeze-remove-nil (and (breeze-sly-connected-p) 'sly)
                     (and (breeze-slime-connected-p) 'slime)))

(cl-defun breeze-%symbolicate2 (listener &optional suffix)
  "Build up a symbol. Used to refer to sly or slime's functions
without using the symbols, as they might not exists if they are
not loaded."
  (cond
   (suffix (intern (format "%s-%s" listener suffix)))
   ((symbolp listener) listener)
   (t (intern listener))))

(defun breeze-single (list)
  (when (and list (= (length list) 1))
    (cl-first list)))

(cl-defun breeze-choose-listener (&optional (errorp t))
  "Interactively ask the user to choose a listener (e.g. sly or
slime) if multiple listeners are available."
  (let ((listeners (breeze-list-loaded-listeners errorp)))
    (or (breeze-single listeners)
        (let ((connected-listeners (breeze-list-connected-listeners)))
          (or (breeze-single connected-listeners)
              ;; If both OR neither sly and slime are connected.
              (intern (completing-read "Choose a lisp listener to start: "
                                       listeners nil t)))))))

(cl-defun breeze-%listener-symbolicate (&optional suffix)
  "Build up a symbol. Used to refer to sly or slime's functions
without using the symbols, as they might not exists if they are
not loaded."
  (breeze-%symbolicate2 (breeze-choose-listener) suffix))

(cl-defun breeze-%listener-apply (suffix args)
  "Apply ARGS to a function SUFFIX. e.g. (breeze-%listener-apply
'-eval 42) might execute (apply 'sly-eval 42) or (apply
'slime-eval 42), depending on which listener was chosen."
  (apply (breeze-%listener-symbolicate suffix) args))

(cl-defun breeze-%listener-funcall (suffix &rest args)
  "Funcall a function SUFFIX with
ARGS. e.g. (breeze-%listener-funcall
'-eval 42) might execute (apply 'sly-eval 42) or (apply
'slime-eval 42), depending on which listener was chosen."
  (breeze-%listener-apply suffix args))

(defun breeze-start-listener ()
  "Start a listener (e.g. calls \"(sly)\" or \"(slime)\")."
  (interactive)
  (let ((listener (breeze-choose-listener)))
    (funcall listener)))

(defun breeze-ensure-listener ()
  "Start a listener (e.g. sly or slime) if none is connected. Will
signal an error if no listeners are loaded."
  (or (breeze-listener-connected-p nil)
      (breeze-start-listener)))


;;; Evaluation

(defun breeze-%eval (form &optional package)
  "Evaluate FROM using sly-eval or slime-eval"
  (breeze-%listener-funcall "eval" form package))

(defun breeze-%eval-async (form &optional cont package)
  "Asynchronously evaluate FORM using sly-eval-async or
slime-eval-async, calls the continuation CONT with the resulting
value."
  (breeze-%listener-funcall "eval-async" form cont package))

(defun breeze-eval (string)
  "Evaluate STRING using the CL function breeze.listener:rpc-eval"
  (let ((value (breeze-%eval `(breeze.listener:rpc-eval ,string))))
    (breeze-debug "Breeze: got the value: %S" value)
    value))

(defun breeze-eval-async (string &optional cont package)
  "Asynchronously evaluate STRING using the CL function breeze.listener:rpc-eval,
calls the continuation CONT with the resulting value."
  (breeze-%eval-async
   `(breeze.listener:rpc-eval ,string)
   cont
   package))


;;; Generic utilities

(defun breeze-ensure-mode (mode)
  (if (derived-mode-p mode)
      (breeze-debug "already in %s (%s)" major-mode mode)
    (progn
      (breeze-debug "switching to %s" mode)
      (funcall mode))))

(defun breeze-show-temp-buffer (buffer-name mode callback)
  (with-current-buffer (get-buffer-create buffer-name)
    (fundamental-mode)
    (setq buffer-read-only t
          buffer-file-name nil)
    (buffer-disable-undo)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (delete-all-overlays)
      (insert "- hi\n+ world\n")
      (with-temp-buffer-window buffer-name
          'display-buffer-pop-up-window
          nil
        (breeze-ensure-mode mode)
        (funcall callback)))))

;; Example:
;; (breeze-show-temp-buffer "*breeze-show*"
;;                          'diff-mode
;;                          (lambda ()
;;                            (insert " hola\n \n- hi\n+ world\n")))

;; (defun %%%breeze-fake-diff (file-before file-after hunks)
;;   (with-output-to-string
;;     (princ (format "--- %s\n+++ %s\n" file-before file-after))
;;     (cl-loop for (range-before range-after diff) in hunks
;;              do (princ (format "@@ -%s,%s +%s,%s @@\n%s\n"
;;                                (car range-before)
;;                                (cdr range-before)
;;                                (car range-after)
;;                                (cdr range-after)
;;                                diff)))))

;; (breeze-show-temp-buffer "*breeze-show*"
;;                          'diff-mode
;;                          (lambda ()
;;                            (insert (%%%breeze-fake-diff "a.lisp" "a.lisp"
;;                                                         `(((3 . 4) (3 . 4)
;;                                                            " previously...\n- holla\n+ holla world\n"))))
;;                            (diff-fixup-modifs (point-min) (point-max))))
;;
;; ...Or use the faces `diff-removed', `diff-added', etc (instead of
;; trying to use diff-mode)

(defun breeze-with-face (string &rest face-plist)
  "Add the 'face property to STRING."
  (propertize string 'face face-plist))


;;; Common lisp driven interactive commands

(cl-defun breeze-compute-buffer-args (&key (include-buffer-content-p t))
  "Compute the list of buffer-related arguments to send to breeze
when starting a command."
  (format
   (concat
    ":buffer-name %S "
    ":buffer-file-name %S "
    ":buffer-string %S "
    ":point %S "
    ":point-min %S "
    ":point-max %S "
    ":major-mode %S ")
   (buffer-name)
   (buffer-file-name)
   (when include-buffer-content-p
     (buffer-substring-no-properties (point-min) (point-max)))
   (1- (point))
   (1- (point-min))
   (1- (point-max))
   major-mode
   default-directory))

(defun breeze-command-start (name &optional extra-args)
  "Start a command by evaluating the CL function breeze.command:start-command.
Returns an integer id that can be used to interact with the
running command."
  (breeze-debug "Breeze: starting command: %s." name)
  (let ((id (breeze-eval
             (format "(breeze.command:start-command '%s '(%s) '%S)"
                     name
                     (breeze-compute-buffer-args :include-buffer-content-p nil)
                     extra-args
                     nil))))
    (breeze-debug "Breeze: start-command %S returned %s" name id)
    id))

(defun breeze-command-cancel (id reason)
  "Cancel the command ID, with REASON (a string, useful for
diagnostics)."
  (breeze-eval
   (format "(breeze.command:cancel-command %s %S)" id reason))
  (breeze-debug "Breeze: command %s canceled." id))

(defun breeze-command-continue (id response send-response-p)
  "Send RESPONSE to the command ID so it can continues after
receiving the data it requested."
  (let ((request
         (breeze-eval
          (if send-response-p
              (format
               "(breeze.command:continue-command %s %S)" id response)
            (format "(breeze.command:continue-command %s)" id)))))
    (breeze-debug "Breeze: (#%s) request received: %s" id request)
    request))

(defun breeze-deregister-command (id)
  (breeze-eval (format "(breeze.command:deregister %s)" id)))

;; TODO maybe add a "narrow" request type?
;; TODO use "inhibit-modification-hooks" where necessary...
(defun breeze-command-process-request (id request)
  "Dispatch REQUESTs from a command."
  (pcase (car request)
    ("choose"
     (completing-read (cl-second request)
                      (cl-third request)))
    ("read-string"
     (apply #'read-string (cl-rest request)))
    ("insert-at"
     (cl-destructuring-bind (_ position string)
         request
       (when (numberp position) (goto-char (1+ position)))
       (insert string)))
    ("insert-at-saving-excursion"
     (cl-destructuring-bind (_ position string)
         request
       (save-excursion
         (when (numberp position) (goto-char (1+ position)))
         (insert string))))
    ("insert"
     (cl-destructuring-bind (_ string) request (insert string)))
    ("replace"
     (cl-destructuring-bind
         (point-from point-to replacement-string)
         (cdr request)
       (delete-region (1+ point-from) (1+ point-to))
       (save-excursion (goto-char (1+ point-from))
                       (insert replacement-string))))
    ("message"
     (breeze-message "%s" (cl-second request)))
    ("find-file"
     (cl-destructuring-bind (file other-window-p)
         (cdr request)
       (if other-window-p
           (find-file-other-window file)
         (find-file file))))
    ("return"
     (breeze-deregister-command id)
     (throw 'breeze-run-command (cl-second request)))
    ("buffer-string"
     (buffer-substring-no-properties (point-min) (point-max)))
    ("goto-char"
     (cl-destructuring-bind (_ position)
         request
       (goto-char (1+ position))))
    ("pulse"
     (cl-destructuring-bind (_ start end)
         request
       (pulse-momentary-highlight-region (1+ start) (1+ end))))
    (_ (breeze-debug "Unknown request: %S" request))))

(defun breeze-run-command (name &rest extra-args)
  "Runs a \"breeze command\". TODO Improve this docstring."
  ;; (interactive)
  (breeze-debug "breeze-run-command")
  (catch 'breeze-run-command
    (let ((id (breeze-command-start name extra-args)))
      (condition-case condition
          (progn
            (cl-loop
             ;; guards against infinite loop
             for i below 1000
             for
             ;; Get the first request from the command
             request = (breeze-command-continue id nil nil)
             ;; Continue the command
             then (breeze-command-continue id response send-response-p)
             ;; "Request" might be nil, if it is, we're done
             while (and request
                        (not (string= "done" (car request))))
             ;; Whether or not we need to send arguments to the next callback.
             for send-response-p = (member (car request)
                                           '("choose" "read-string" "buffer-string"))
             ;; Process the command's request
             for response = (breeze-command-process-request id request)
             ;; Log request and response (for debugging)
             do (breeze-debug "Breeze: request received: %S response to send %S"
                              request
                              response))
            (breeze-deregister-command id))
        (quit
         (breeze-debug "Breeze run command: (%S) " id condition)
         (breeze-command-cancel id "User-cancelled"))
        (t
         (breeze-debug "Breeze run command: (%S) got the condition %s" id condition)
         (breeze-command-cancel id "Elisp condition"))))))


;;; Dynamically define interactive (cl-driven) commands in emacs

;; TODO This creates new commands, but what happens if a command was removed?
(defun breeze-refresh-commands ()
  "Ask the inferior lisp which commands it has and define corresponding
commands in emacs."
  (interactive)
  (with-temp-buffer
    (dolist (command-plist (breeze-eval "(breeze.command:list-all-commands-for-editor)"))
      ;; this can be confusing:
      ;;  - name is a symbol
      ;;  - symbol is a string of the cl symbol
      ;;  - lambda-list is a list of symbols that only contains the name
      ;;    of the arguments (no &optional, no default values, etc.)
      (cl-destructuring-bind (&key name symbol lambda-list documentation)
          command-plist
        (let* ((defun `(cl-defun ,name (&optional ,@lambda-list)
                         ,documentation
                         ;; (interactive "" 'lisp-mode 'breeze-minor-mode 'breeze-major-mode)
                         (interactive)
                         (unless (breeze-disabled-p)
                           (if (and (breeze-list-loaded-listeners)
                                    (breeze-listener-connected-p)
                                    (breeze-validate-if-breeze-package-exists))
                               (breeze-run-command ,symbol ,@lambda-list)
                             (breeze--stub ,symbol))))))
          (insert (format "%S\n" defun))
          (insert (format "%S\n" `(setf (get ',name 'breeze-command-p) t))))))
    ;; (debug)
    (eval-buffer)))


;;; Enabling/disabling breeze

(defvar breeze-disabled-p (make-hash-table)
  "Internal hash-table used to disable breeze’s features on a
per-connection basis. It is managed by the `breeze-enable' and
`breeze-disable' commands.

When breeze is disabled for a connection, all breeze commands
become no-ops (for that connection), while hooks remain intact.

Disabling is useful when breeze cannot be loaded or is currently
broken (e.g. during development of breeze itself).")

(defun breeze-disabled-p ()
  (let ((connection (breeze-listener-connected-p)))
    (gethash connection breeze-disabled-p)))

(defun breeze-disable ()
  "Disable breeze's features for the current connection.
This doesn't remove any hooks, instead, each breeze's command becomes no-ops."
  (interactive)
  (let ((connection (breeze-listener-connected-p)))
    (setf (gethash connection breeze-disabled-p) t)))

(defun breeze-enable ()
  "Enable breeze's features for the current connection."
  (interactive)
  (let ((connection (breeze-listener-connected-p)))
    (setf (gethash connection breeze-disabled-p) nil)))

;; TODO breeze-not-initialized-hook

;; TODO restore the stubs if the inferior lisp is
;; closed.


;;; "Autoload"

(defun breeze--stub (name)
  "A dummy command that is used to ensure breeze is loaded the first
time a command is invoked. breeze-refresh-commands is called,
which will redefine the dummy command."
  (if (breeze-disabled-p)
      nil ;; (warn "Breeze is disabled")
    (and
     (breeze-list-loaded-listeners)
     (breeze-listener-connected-p)
     (breeze-validate-if-breeze-package-exists)
     (breeze-refresh-commands))))

(defun breeze-header-line ()
  "Compute the content of the header-line for the current buffer"
  (interactive)
  (let ((connection (breeze-listener-connected-p nil)))
    (if connection
        (format "BRZ: %s %S (%s)"
                (process-name connection)
                (process-contact connection)
                (if (process-live-p (breeze-listener-connected-p nil))
                    "live" "!!!dead!!!"))
      (format "BRZ: not connected"))))

;; calling the common lisp side in the header line can really slow things down:
;;
;; backtrace()
;; breeze-validate-if-breeze-package-exists()
;; (and (breeze-list-loaded-listeners) (breeze-listener-connected-p) (breeze-validate-if-breeze-package-exists) (breeze-refresh-commands))
;; (if (breeze-disabled-p) nil (and (breeze-list-loaded-listeners) (breeze-listener-connected-p) (breeze-validate-if-breeze-package-exists) (breeze-refresh-commands)))
;; breeze--stub("breeze-header-line")
;; breeze-header-line()
;; eval((breeze-header-line))
;; redisplay_internal\ \(C\ function\)()


;;; Completion at point -- NOT IMPLEMENTED, this is just a stub for now

;; TODO breeze-capfs-mode or breeze-completion-mode
;; TODO update the documentatiob (e.g. "getting started"

;; 1. `completion-at-point' uses the variable `completion-at-point-functions'
;; 2. it finds and call `breeze-completion-at-point' (no "S")
;; 3. `breeze-completion-at-point' returns a list (START END COLLECTION)
;;    where COLLECTION is a function generated by `completion-table-dynamic'
;; 4. `breeze--complete-at-point' gets called with a STRING
;; 5. delegate the generation of candidates to the command
;;    `breeze-completions-at-point' (with an "S")

(defun breeze--complete-at-point (string)
  ;; (message "string: %S" string)
  ;; '("prin1")
  (breeze-completions-at-point))

(defun breeze-completion-at-point ()
  (when breeze-minor-mode
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        ;; (message "bounds: %S" bounds)
        (list (car bounds)
              (cdr bounds)
              ;; completion-table-with-cache wraps
              ;; completion-table-dynamic
              (completion-table-with-cache 'breeze--complete-at-point)
              :exclusive 'no)))))

(define-minor-mode breeze-capfs-mode
  "Toggle Breeze's completion at point"
  :lighter nil
  (cond
   (breeze-capfs-mode
    (add-hook 'completion-at-point-functions 'breeze-completion-at-point))
   (t
    (remove-hook 'completion-at-point-functions 'breeze-completion-at-point))))



;;; Initializations

(defun breeze-validate-if-package-exists (package)
  "Returns true if the package PACKAGE exists in the inferior lisp."
  (breeze-debug "breeze-validate-if-package-exists %S" package)
  (breeze-%eval
   `(cl:eval
     (cl:and (cl:or (cl:find-package ,(downcase package))
                    (cl:find-package ,(upcase package)))
             t))))

;; (breeze-validate-if-package-exists "ASDF")

(defun breeze-validate-if-breeze-package-exists ()
  "Returns true if the package \"breeze.utils\" exists in the
inferior lisp."
  ;; (backtrace)
  (breeze-validate-if-package-exists "breeze.utils"))

(defvar breeze-breeze.el load-file-name
  "Path to \"breeze.el\".")

(defun breeze-relative-path (&rest components)
  "Compute a path relative to the root of the project.
Uses the variable breeze-breeze.el to find the root."
  (expand-file-name
   (apply 'file-name-concat
          (file-name-directory breeze-breeze.el)
          ".."
          components)))

;; TODO this doesn't work well on a "remote systems"
(defun breeze-%loader ()
  "Read src/ensure-breeze.lisp as a string and tweak it for use by a
listener."
  (with-temp-buffer
    (insert-file-contents (breeze-relative-path "src/ensure-breeze.lisp"))
    (beginning-of-buffer)

    (insert "(cl:multiple-value-bind (#1=#.(gensym \"result\") #2=#.(gensym \"condition\")) (cl:ignore-errors \n")

    (end-of-buffer)
    (insert "\n)
(list #1# (when #2# (format nil \"~A\" #2#)))
)")
    (buffer-string)))

;; (breeze-%loader)

(cl-defun breeze-load (&optional cont)
  "Asynchronously load breeze into the inferior lisp."
  (interactive)
  ;; require asdf
  (breeze-%eval '(cl:progn (cl:require "asdf") t))
  ;; define package
  (breeze-%eval '(cl:progn (cl:defpackage breeze.loader (:use :cl)) t))
  ;; load!
  (breeze-%eval `(cl:progn (cl:defparameter breeze.loader::*asd*
                                            ,(breeze-relative-path "breeze.asd")) t))
  (breeze-%eval-async
   `(cl:let ((cl:*package* (cl:find-package '#:breeze.loader)))
            (cl:eval (cl:read-from-string ,(breeze-%loader))))
   (lambda (result)
     (cl-destructuring-bind (success condition)
         result
       (breeze-message (if (and success (not condition))
                           "Breeze loaded successfully. (%s)"
                         condition)
                       success)
       (when (and (and success (not condition)) cont)
         (funcall cont))))
   '#:breeze.loader))

(cl-defun breeze-ensure (&optional callback)
  "Make sure that breeze is loaded in the inferior lisp."
  (unless (breeze-disabled-p)
    (if (breeze-validate-if-breeze-package-exists)
        (when callback (funcall callback))
      (breeze-message "Loading breeze's system asynchronously...")
      (breeze-load
       (lambda (&rest _)
         (breeze-message "Breeze loaded in inferior-lisp.")
         (breeze-refresh-commands)
         (when callback (funcall callback)))))))


;; See slime--setup-contribs, I named this breeze-init so it _could_
;; be added to slime-contrib,
;;;###autoload
(cl-defun breeze-init ()
  "Initialize breeze."
  (interactive)
  (breeze-ensure-listener)
  (breeze-ensure)
  (breeze-debug "Breeze initialized (might still be loading in the inferior lisp)."))


;;; Listener Hooks

;; TODO This is experimental! I mean... more than the rest xD
(defun breeze-%%%setup-hooks (listener)
  "Hook into every hooks in slime and log when it's called along
with which arguments."
  (when (eq 'slime listener)
    (cl-loop for hook in '(slime-connected-hook
                           slime-inferior-process-start-hook
                           slime-net-process-close-hooks
                           slime-cycle-connections-hook
                           slime-connected-hook
                           slime-event-hooks)
             do (add-hook hook (lambda (&rest args) (breeze-debug "%S: %S" hook args) nil)))))

(defun breeze-connected-hook-function ()
  "Hook to be called when a listener is connected."
  (breeze-ensure))

(defun breeze-enable-connected-hook ()
  "Configure a hook to initialize breeze when connecting to sly or slime."
  (interactive)
  (breeze-add-hook 'slime-connected-hook
                   'breeze-connected-hook-function)
  (breeze-add-hook 'sly-connected-hook
                   'breeze-connected-hook-function))

(defun breeze-disable-connected-hook ()
  "Remove 'breeze-connected-hook-function from sly or slime's
\"connected hook\"."
  "Remove the hook to initialize breeze when connecting to sly or slime."
  (interactive)
  (breeze-remove-hook 'slime-connected-hook
                      'breeze-connected-hook-function)
  (breeze-remove-hook 'sly-connected-hook
                      'breeze-connected-hook-function))


;;; Incremental parsing

(defun breeze-after-change-function (start stop length)
  nil)

(defun breeze-after-change-function (start stop length)
  (unless (breeze-disabled-p)
    (breeze-ensure
     (lambda ()
       (breeze-eval-async
        (prin1-to-string
         (let ((base (list 'breeze.incremental-reader:after-change-function
                           start stop length
                           :buffer-name (buffer-name)
                           :buffer-file-name (buffer-file-name))))
           (if (zerop length)
               (append base (list :insertion (buffer-substring-no-properties start stop)))
             base))))))))

(add-hook 'breeze-minor-mode-hook
          (lambda ()
            (if breeze-minor-mode
                ;; When enabling breeze-minor-mode
                (add-hook 'after-change-functions 'breeze-after-change-function nil t)
              ;; When disabling breeze-minor-mode
              (remove-hook 'after-change-functions 'breeze-after-change-function t))))


;;; Hooks for flymake

(defun breeze-lint (args callback)
  "Asynchronously calls the function breeze.lint:lint."
  ;; TODO use ARGS to be able to INCREMENTALLY parse and analyze the
  ;; buffer
  ;;
  ;; (null args) => first call ever
  ;;
  ;; :recent-changes
  ;; :changes-start
  ;; :changes-end
  (breeze-ensure
   (lambda ()
     ;; (cl-loop for change in (cl-getf args :recent-changes) do (breeze-debug "  change: %S" change))
     (funcall callback (breeze-run-command 'breeze.lint:lint))))
  nil)

;; TODO maybe delete?
(defun breeze-lint-interactive ()
  "Call breeze-lint, but print the diagnostics as messages. Mainly
for debugging breeze itself."
  (interactive)
  (breeze-lint nil (lambda (diag)
                     (message "breeze-lint: %s" diag))))

(defun breeze-flymake (report-fn &rest args)
  "A flymake diagnostic function to integrate breeze.analysis:lint with flymake."
  ;; this logs way too much (probably slowing down everything too):
  ;; (breeze-debug "flymake: %S" args)
  (if (and
       (not (breeze-disabled-p))
       (breeze-listener-connected-p)
       ;; TODO breeze-ready-p
       )
      (let ((buffer (current-buffer)))
        (breeze-lint args (lambda (cl-diagnostics)
                            (funcall report-fn
                                     (cl-loop for (beg end type text) in cl-diagnostics
                                              collect (flymake-make-diagnostic
                                                       ;; Locus
                                                       buffer
                                                       (1+ beg) (if end
                                                                    (1+ end)
                                                                  (point-max))
                                                       type
                                                       text))))))
    ;; Not connected, so we can't call breeze's linter.
    (funcall report-fn nil)))

;; TODO breeze-flymake-mode
;; TODO 1. ensure-breeze-minor-mode
;; TODO 2. add backend
;; TODO 3. enable flymake
;; TODO clean up the existing flymake-related functions
;; TODO update the "getting started" documentation

(defun breeze-enable-flymake-backend ()
  "Add breeze-flymake to the list of flymake-diagnostic-functions."
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'breeze-flymake nil t))

(defun breeze-disable-flymake-backend ()
  "Remove breeze-flymake from the list of flymake-diagnostic-functions."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions 'breeze-flymake nil))

;; TODO assumes slime
;; TODO this doesn't work perfectly
;; TODO if there's no slime/sly notes, nor flymake errors,
;;      go to the next "TODO"
;; TODO investigate using (goto-char (next-overlay-change (point)))
;; TODO investigate `next-error' and `compile-next-error'
(defun breeze-next-note ()
  "Go to either the next note from the listener or to the next
flymake error."
  (interactive)
  (let ((slime-note (save-excursion (slime-find-next-note))))
    (if slime-note
        (slime-next-note)
      (flymake-goto-next-error))))

;; TODO assumes slime
;; TODO this doesn't work perfectly
(defun breeze-previous-note ()
  "Go to either the previous note from the listener or to the
previous flymake error."
  (interactive)
  (let ((slime-note (save-excursion (slime-find-previous-note))))
    (if slime-note
        (slime-previous-note)
      (flymake-goto-prev-error))))


;;; minor mode

(defvar breeze-minor-mode-map
  (make-sparse-keymap)
  "Keymap for breeze-minor-mode")

;;;###autoload
(define-minor-mode breeze-minor-mode
  "Toggle Breeze minor mode on or off

Breeze minor mode is an Emacs minor mode that complements lisp-mode."
  :lighter " brz"
  :keymap breeze-minor-mode-map
  :interactive (lisp-mode)
  (cond
   (breeze-minor-mode
    ;; TODO What if dabbrev-abbrev-skip-leading-regexp is already customized?
    (setf dabbrev-abbrev-skip-leading-regexp "\\(#?:\\)\\|+")
    (setq header-line-format '(:eval (breeze-header-line))))
   (t
    (setq header-line-format nil))))

;; Analogous to org-insert-structure-template
;; (define-key breeze-minor-mode-map (kbd "C-c C-,") 'breeze-insert)
(keymap-set breeze-minor-mode-map "M-RET" #'breeze-quickinsert)

;; Analogous to org-goto
(keymap-set breeze-minor-mode-map "C-c C-j" #'imenu)

;; Analogous to Visual Studio Code's "quickfix"
(keymap-set breeze-minor-mode-map "C-." #'breeze-quickfix)

;; TODO M-n M-p https://www.gnu.org/software/emacs/manual/html_node/flymake/Finding-diagnostics.html

(keymap-set breeze-minor-mode-map "M-p" #'breeze-previous-note)
(keymap-set breeze-minor-mode-map "M-n" #'breeze-next-note)

(keymap-set breeze-minor-mode-map "C-c C-o" #'breeze-other-file-other-window)
(keymap-set breeze-minor-mode-map "C-c o" #'breeze-other-file)
(keymap-set breeze-minor-mode-map "M-c" #'breeze-interactive-eval)

(defvar breeze-test-map (make-sparse-keymap)
  "Keymap for the test-related commands.")

;; TODO none of these commands are implemented
(keymap-set breeze-test-map "r" 'breeze-test-run)
(keymap-set breeze-test-map "u" 'breeze-test-undefine)
(keymap-set breeze-test-map "a" 'breeze-test-run-all)
(keymap-set breeze-test-map "." 'breeze-test-quickfix)
(keymap-set breeze-test-map "g" 'breeze-test-goto)

(keymap-set breeze-minor-mode-map "C-c t" breeze-test-map)

;; TODO bind M-a and M-e to smth (forward/backward sexp)

;; TODO define keymap breeze-insert-keymap
;; TODO maybe a "transient" could work?
;; TODO e.g. C-c i l => breeze-insert-lambda
;; (keymap-set breeze-minor-mode-map "C-c i" #'breeze-insert)


;; Disabled for now
;; eval keymap - because we might want to keep an history
;; (defvar breeze-eval-map (make-sparse-keymap))
;; eval last expression
;; (define-key breeze-minor-mode-map (kbd "C-c e") breeze-eval-map)
;; choose an expression from history to evaluate
;; (define-key breeze-eval-map (kbd "e") 'breeze-reevaluate-form)

;;;###autoload
(defun enable-breeze-minor-mode ()
  "Enable breeze-minor-mode."
  (interactive)
  (unless breeze-minor-mode
    (breeze-minor-mode 1)))

;;;###autoload
(defun disable-breeze-minor-mode ()
  "Disable breeze-minor-mode."
  (interactive)
  (when breeze-minor-mode
    (breeze-minor-mode -1)))

(defun breeze-minor-mode-enable-flymake-mode ()
  "Configure a hook to enable flymake-mode when breeze-minor mode is enabled"
  (interactive)
  ;; TODO actually enable flymake
  (add-hook 'breeze-minor-mode-hook 'flymake-mode)
  (add-hook 'breeze-minor-mode-hook 'breeze-enable-flymake-backend))

(defun breeze-minor-mode-disable-flymake-mode ()
  "Configure a hook to enable flymake-mode when breeze-minor mode is enabled"
  (interactive)
  ;; TODO actually disable flymake (what if it was already enabled by
  ;; something else?)
  (remove-hook 'breeze-minor-mode-hook 'flymake-mode)
  (remove-hook 'breeze-minor-mode-hook 'breeze-enable-flymake-backend)
  (breeze-disable-flymake-backend))

(defun breeze-enable-minor-mode-hook ()
  "Configure lisp-mode-hook to automatically enable
breeze-minor-mode in lisp-mode."
  (interactive)
  (add-hook 'lisp-mode-hook #'breeze-minor-mode))

(defun breeze-disable-minor-mode-hook ()
  "Remove breeze-minor-mode from lisp-mode-hook, to stop
automatically enabling breeze-minor-mode in lisp-mode."
  (interactive)
  (remove-hook 'lisp-mode-hook #'breeze-minor-mode))

;; TODO use the hook "change-major-mode-hook" (or
;; "after-change-major-mode-hook"??) to disable/enable
;; breeze-minor-mode


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


;;; Generating stubs

(defun breeze-list-commands ()
  "List the \"breeze commands\" that are currently defined in emacs."
  (let ((x))
    (mapatoms (lambda (s)
                (when (get s 'breeze-command-p)
                  (push s x))))
    x))

;;;###autoload
(defmacro breeze--defstub (name docstring)
  "Macro to define a stub function for the command NAME."
  `(defun ,name ()
     ,docstring
     (interactive)
     (breeze--stub ,(symbol-name name))))


;;; This page is auto-generated (see ../workbench.el), don't edit it

;;;###autoload
(breeze--defstub breeze-completions-at-point "completion-at-point")
;;;###autoload
(breeze--defstub breeze-insert-defvar "Insert a defvar form.")
;;;###autoload
(breeze--defstub breeze-insert-decoded-time-multiple-value-bind "Insert a cl:multiple-value-bind form to bind the output of cl:get-decoded-time")
;;;###autoload
(breeze--defstub breeze-insert-loop-clause-for-on-list "Insert a loop clause to iterate on a list.")
;;;###autoload
(breeze--defstub breeze-insert-handler-case-form "Insert handler case form.")
;;;###autoload
(breeze--defstub breeze-insert-setf-defun "Insert a setf function form e.g. (defun (setf ...) ...)")
;;;###autoload
(breeze--defstub breeze-insert-handler-bind-form "Insert handler bind form.")
;;;###autoload
(breeze--defstub breeze-insert-loop-clause-for-hash "Insert a loop clause to iterate on a hash-table.")
;;;###autoload
(breeze--defstub breeze-declaim-inline "Declaim inline the current top-level function.")
;;;###autoload
(breeze--defstub breeze-insert-defgeneric "Insert a defgeneric form.")
;;;###autoload
(breeze--defstub breeze-capture "Quickly create a lisp file in a pre-determined directory.")
;;;###autoload
(breeze--defstub breeze-insert-print-unreadable-object-boilerplate "Insert a print-object method form.")
;;;###autoload
(breeze--defstub breeze-kill-worker-threads "Find threads named \"worker\", then destroy them.")
;;;###autoload
(breeze--defstub breeze-insert-local-nicknames "Insert local nicknames.")
;;;###autoload
(breeze--defstub breeze-insert-make-load-form-boilerplate "Insert a make-load-form method form.")
;;;###autoload
(breeze--defstub breeze-quickload "Choose a system to load with quickload.")
;;;###autoload
(breeze--defstub breeze-insert-fancy-sbcl-shebang "Insert fancy sbcl shebang at the start of the buffer.")
;;;###autoload
(breeze--defstub breeze-insert-defpackage "Insert a defpackage form.")
;;;###autoload
(breeze--defstub breeze-other-file "Find the alternative file for the current file.")
;;;###autoload
(breeze--defstub breeze-quickfix "Given the context, suggest some applicable commands.")
;;;###autoload
(breeze--defstub breeze-other-file-other-window "Find the alternative file for the current file.")
;;;###autoload
(breeze--defstub breeze-insert-loop-clause-for-in-list "Insert a loop clause to iterate in a list.")
;;;###autoload
(breeze--defstub breeze-insert-defclass "Insert a defclass form.")
;;;###autoload
(breeze--defstub breeze-insert-defparameter "Insert a defparameter form.")
;;;###autoload
(breeze--defstub breeze-insert-breeze-define-command "Insert a breeze:define-command form.")
;;;###autoload
(breeze--defstub breeze-insert-fancy-emacs-propline "Insert a fancy emacs propline.")
;;;###autoload
(breeze--defstub breeze-interactive-eval-command "A command to interactively evaluate code.")
;;;###autoload
(breeze--defstub breeze-insert-asdf "Insert an asdf system definition form.")
;;;###autoload
(breeze--defstub breeze-insert-initialize-instance~method "Insert a ~(defmethod initialize-instance ...)~ form.")
;;;###autoload
(breeze--defstub breeze-insert-in-package-cl-user "Insert (cl:in-package #:cl-user)")
;;;###autoload
(breeze--defstub breeze-insert-lambda "Insert a lambda form.")
;;;###autoload
(breeze--defstub breeze-insert-make-array "Insert a make-array form.")
;;;###autoload
(breeze--defstub breeze-insert-defun "Insert a defun form.")
;;;###autoload
(breeze--defstub breeze-kill-sexp "Kill the expression following point.")
;;;###autoload
(breeze--defstub breeze-insert-defconstant "Insert a defconstant form.")
;;;###autoload
(breeze--defstub breeze-insert-class-slot "Insert a defclass slot form.")
;;;###autoload
(breeze--defstub breeze-insert-define-constant "Insert a alexandria:define-constant form.")
;;;###autoload
(breeze--defstub breeze-quickinsert "Given the context, suggest applicable snippets.")
;;;###autoload
(breeze--defstub breeze-insert-defmethod "Insert a defmethod form.")
;;;###autoload
(breeze--defstub breeze-insert-defmacro "Insert a defmacro form.")




(provide 'breeze)
;;; breeze.el ends here
