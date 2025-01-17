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
     (apply #'format string objects))
    (setf buffer-read-only t)))

(defun breeze-message (string &rest objects)
  "Log a meesage in both \" *breeze-debug*\" and *Messages* buffers."
  (apply #'message string objects)
  (apply #'breeze-debug string objects))



;;; Lisp listener state

(defun breeze-sly-connected-p ()
  "Check if sly loaded and connected."
  (and (fboundp 'sly-connected-p)
       (sly-connected-p)))

(defun breeze-slime-connected-p ()
  "If slime is loaded and connected."
  (and (fboundp 'slime-connected-p)
       (slime-connected-p)))

(cl-defun breeze-listener-connected-p (&optional (errorp t))
  "Check if either sly or slime is loaded and connected."
  (or (breeze-sly-connected-p)
      (breeze-slime-connected-p)
      (and errorp
           (error "Please start either slime or sly."))))

(cl-defun breeze-list-loaded-listeners (&optional (errorp t))
  "Returns a list of loaded listeners (sly or slime)."
  (or (remove 'nil (list (and (fboundp 'slime) 'slime)
                         (and (fboundp 'sly) 'sly)))
      (and errorp
           (error "Please load either slime or sly."))))

(cl-defun breeze-%symbolicate2 (listener &optional suffix)
  "Build up a symbol. Used to refer to sly or slime's functions
without using the symbols, as they might not exists if they are
not loaded."
  (cond
   (suffix (intern (format "%s-%s" listener suffix)))
   ((symbolp listener) listener)
   (t (intern listener))))

;; TODO errorp is not used
(cl-defun breeze-choose-listener (&optional (errorp t))
  "Interactively ask the user to choose a listener (e.g. sly or
slime) if multiple listeners are available."
  (let ((listeners (breeze-list-loaded-listeners errorp)))
    (when listeners
      (if (= (length listeners) 1)
          (cl-first listeners)
        (breeze-%symbolicate2
         (completing-read "Choose a lisp listener to start: "
                          listeners nil t))))))

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

(defun breeze-%eval (form)
  "Evaluate FROM using sly-eval or slime-eval"
  (breeze-%listener-funcall "eval" form))

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
    ":point-max %S")
   (buffer-name)
   (buffer-file-name)
   (when include-buffer-content-p
     (buffer-substring-no-properties (point-min) (point-max)))
   (1- (point))
   (1- (point-min))
   (1- (point-max))))

(defun breeze-command-start (name &optional extra-args)
  "Start a command by evaluating the CL function breeze.command:start-command.
Returns an integer id that can be used to interact with the
running command."
  (breeze-debug "Breeze: starting command: %s." name)
  (let ((id (breeze-eval
             (format "(breeze.command:start-command '%s '(%s) '%S)"
                     name
                     (breeze-compute-buffer-args)
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
;; (breeze-command-cancel)

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


;; TODO maybe add a "narrow" request type?
(defun breeze-command-process-request (request)
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
       (let ((point (point)))
         (kill-region (1+ point-from) (1+ point-to))
         (goto-char (1+ point-from))
         (insert replacement-string)
         (goto-char point))))
    ("message"
     (breeze-message "%s" (cl-second request)))
    ("find-file"
     (find-file (cl-second request)))
    (_ (breeze-debug "Unknown request: %S" request) )))


(defun breeze-run-command (name &rest extra-args)
  "Runs a \"breeze command\". TODO Improve this docstring."
  (interactive)
  (breeze-debug "breeze-run-command")
  ;; TODO Do I really want to initialize breeze here?
  ;; (breeze-ensure)
  ;; TODO extra-args
  (let ((id (breeze-command-start name extra-args)))
    (condition-case condition
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
                                       '("choose" "read-string"))
         ;; Process the command's request
         for response = (breeze-command-process-request request)
         ;; Log request and response (for debugging)
         do (breeze-debug "Breeze: request received: %S response to send %S"
                          request
                          response))
      (quit
       (breeze-debug "Breeze run command: (%S) " id condition)
       (breeze-command-cancel id "User-cancelled"))
      (t
       (breeze-debug "Breeze run command: (%S) got the condition %s" id condition)
       (breeze-command-cancel id "Elisp condition")))))


;;; Dynamically define interactive (cl-driven) commands in emacs

(defun breeze--remove-suffix (suffix string)
  "String utility to remove the SUFFIX from STRING if it's present."
  (if (string-suffix-p suffix string)
      (cl-subseq string 0 (- (length string)
                             (length suffix)))
    string))

(defun breeze-translate-command-symbol (symbol)
  "Translate \"common lisp\" symbols to \"emacs lisp\" symbols. Used
to dynamically generate emacs commands for each \"breeze
commands\"."
  (let ((name (symbol-name symbol)))
    (cl-destructuring-bind (package command)
        (split-string name ":")
      (list symbol
            (if (string-prefix-p "breeze" name)
                (intern (format "breeze-%s"
                                (breeze--remove-suffix "-command" command)))
              ;; TODO maybe add some way to customize this
              symbol)))))

;; TODO this handles only very simplistic cases and it's aleady complex...
;; maybe I should do this translation on the CL side and return something easier to handle???
(defun breeze-translate-command-lambda-list (lambda-list)
  "Translate a \"breeze command lambda list\" to an \"emacs lisp\"
lambda list. Used to dynamically generate emacs commands for each
\"breeze commands\"."
  (cl-loop for symbol in lambda-list
           for sanitized-symbol = (intern (car (last (split-string (symbol-name symbol) ":"))))
           collect sanitized-symbol))

;; TODO This creates new commands, but what happens if a command was removed?
(defun breeze-refresh-commands ()
  "Ask the inferior lisp which commands it has and define
corresponding commands in emacs."
  (interactive)
  (cl-loop for (symbol cl-lambda-list docstring) in (breeze-eval "(breeze.command:list-all-commands t)")
           for (cl-symbol el-symbol) = (breeze-translate-command-symbol symbol)
           for el-lambda-list = (breeze-translate-command-lambda-list cl-lambda-list)
           for defun = `(cl-defun ,el-symbol (&optional ,@el-lambda-list)
                          ,docstring
                          ;; (interactive "" 'lisp-mode 'breeze-minor-mode 'breeze-major-mode)
                          (interactive)
                          (breeze-run-command ,(symbol-name cl-symbol) ,@el-lambda-list))
           do
           ;; (breeze-debug "%S" defun)
           (eval defun)))


;;; "Autoload"

(defun breeze-disabled-p ()
  nil)

;; TODO breeze-not-initialized-hook

(defun breeze--stub (name)
  "A dummy command that is used to load breeze the first time a
command is invoked. breeze-refresh-commands is called, which will
redefined the dummy command (there's only 1 at the moment:
breeze-quickfix)."
  (warn "Breeze is not loaded")
  (if (breeze-disabled-p)
      (warn "Breeze is disabled")
    (and
     (breeze-list-loaded-listeners)
     (breeze-listener-connected-p)
     (breeze-validate-if-breeze-package-exists)
     (breeze-refresh-commands))))

(defun breeze-quickfix ()
  "A stub for the breeze command \"quickfix\", calling it the first
time will initialize breeze and redefine this command."
  (interactive)
  (breeze--stub "quickfix"))


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

    ;; (search-forward "(or-die ") (previous-line)

    (insert "(cl:multiple-value-bind (#1=#.(gensym \"result\") #2=#.(gensym \"condition\")) (cl:ignore-errors \n")

    (search-forward "*asd*")
    (forward-line) (back-to-indentation)
    (kill-sexp)
    (insert (format "%S" (breeze-relative-path "breeze.asd")))

    (end-of-buffer)
    (insert "\n)
(list #1# (when #2# (format nil \"~A\" #2#)))
)")
    (buffer-string)))

;; (breeze-%loader)

(cl-defun breeze-load (&optional cont)
  "Asynchronously load breeze into the inferior lisp."
  (interactive)
  (breeze-%eval-async
   `(cl:let ((cl:*package* (cl:find-package :cl-user)))
            (cl:eval (cl:read-from-string ,(breeze-%loader))))
   (lambda (result)
     (cl-destructuring-bind (success condition)
         result
       (breeze-message (if (and success (not condition))
                           "Breeze loaded successfully. (%s)"
                         condition)
                       success)
       (when (and (and success (not condition)) cont)
         (funcall cont))))))

(cl-defun breeze-ensure (&optional callback)
  "Make sure that breeze is loaded in the inferior lisp."
  (if (breeze-validate-if-breeze-package-exists)
      (when callback (funcall callback))
    (breeze-message "Loading breeze's system asynchronously...")
    (breeze-load
     (lambda (&rest _)
       (breeze-message "Breeze loaded in inferior-lisp.")
       (breeze-refresh-commands)
       (when callback (funcall callback))))))


;; See slime--setup-contribs, I named this breeze-init so it _could_
;; be added to slime-contrib,
(cl-defun breeze-init ()
  "Initialize breeze."
  (interactive)
  (breeze-ensure-listener)
  (breeze-ensure)
  (breeze-debug "Breeze initialized (might still be loading in the inferior lisp."))


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
             do (add-hook hook (lambda (&rest args) (breeze-debug "%S: %S" hook args) nil)) )))

(defun breeze-connected-hook-function ()
  "Hook to be called when a listener is connected."
  (breeze-ensure))

(defun breeze-enable-connected-hook ()
  "Configure a hook to initialize breeze when connecting to sly or slime."
  (interactive)
  (add-hook (breeze-%listener-symbolicate "connected-hook")
            'breeze-connected-hook-function))

(defun breeze-disable-connected-hook ()
  "Remove 'breeze-connected-hook-function from sly or slime's
\"connected hook\"."
  (interactive)
  "Remove the hook to initialize breeze when connecting to sly or slime."
  (remove-hook (breeze-%listener-symbolicate "connected-hook")
               'breeze-connected-hook-function))


;;; Incremental parsing

(defun breeze-after-change-function (start stop length)
  (breeze-ensure
   (lambda ()
     (breeze-eval-async
      (prin1-to-string
       (let ((base (list 'breeze.analysis:after-change-function
                         start stop length
                         :buffer-name (buffer-name)
                         :buffer-file-name (buffer-file-name)
                         )))
         (if (zerop length)
             (append base (list :insertion (buffer-substring-no-properties start stop)))
           base)))))))

(add-hook 'breeze-minor-mode-hook
          (lambda ()
            (if breeze-minor-mode
                ;; When enabling breeze-minor-mode
                (add-hook 'after-change-functions 'breeze-after-change-function nil t)
              ;; When disabling breeze-minor-mode
              (remove-hook 'after-change-functions 'breeze-after-change-function t))))


;;; Hooks for flymake

(defun breeze-lint (args callback)
  "Asynchronously calls the function breeze.analysis:lint."
  ;; TODO use ARGS to be able to INCREMENTALLY parse and analyzed the
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
     (breeze-eval-async
      (format "(breeze.analysis:lint %s)"
              (breeze-compute-buffer-args
               ;; :include-buffer-content-p (null args)
               ))
      callback)))
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
                                                       (1+ beg) (1+ end)
                                                       type
                                                       text))))))
    ;; Not connected, so we can't call breeze's linter.
    (funcall report-fn nil)))

(defun breeze-enable-flymake-backend ()
  "Add breeze-flymake to the list of flymake-diagnostic-functions."
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'breeze-flymake nil t))

(defun breeze-disable-flymake-backend ()
  "Remove breeze-flymake from the list of flymake-diagnostic-functions."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions 'breeze-flymake nil))

;; TODO assumes slime
;; TODO this doesn't work well at all
;; TODO if there's no slime/sly notes, nor flymake errors,
;;      go to the next "TODO"
(defun breeze-next-note ()
  "Go to either the next note from the listener or to the next
flymake error."
  (interactive)
  (let ((slime-note (save-excursion (slime-find-next-note))))
    (if slime-note
        (slime-next-note)
      (flymake-goto-next-error))))

;; TODO assumes slime
;; TODO this doesn't work well at all
(defun breeze-previous-note ()
  "Go to either the previous note from the listener or to the
previous flymake error."
  (interactive)
  (let ((slime-note (save-excursion (slime-find-previous-note))))
    (if slime-note
        (slime-previous-note)
      (flymake-goto-prev-error))))


;;; WIP Alternate files (this is currently very brittle, but it should
;;; work for most of my projects).
;;;
;;; TODO Better docstrings
;;; TODO move this logic to the inferior lisp!

(defun breeze--candidate-aternate-directories ()
  "Generate a list of existing alternate directories."
  (let ((root (vc-root-dir)))
    (cl-remove-if-not
     (lambda (fullpath)
       (and
        (file-exists-p fullpath)
        (file-directory-p fullpath)))
     (mapcar
      (lambda (dirname) (expand-file-name
                         (file-name-concat root dirname)))
      '("src" "t" "test" "tests")))))

;; (breeze--candidate-aternate-directories)
;; => '("/home/fstamour/dev/breeze/src" "/home/fstamour/dev/breeze/tests")

(defun breeze--split-file-name (file-name)
  (cl-loop
   for altdir in (breeze--candidate-aternate-directories)
   for relative-path = (file-relative-name file-name altdir)
   when (string-prefix-p altdir (expand-file-name relative-path))
   do (cl-return (list altdir relative-path))))

;; (breeze--split-file-name (buffer-file-name))
;; => '("/home/fstamour/dev/breeze/src" "breeze.el")

(defun breeze--alternate-files (file-name)
  (cl-loop
   with (dir path) = (breeze--split-file-name file-name)
   for altdir in (breeze--candidate-aternate-directories)
   for altpath = (file-name-concat altdir path)
   when (and (not (string= dir altdir))
             (file-exists-p altpath))
   collect altpath))

;; TODO I should really check how to unit-tests emacs lisp...
;; (equal
;;  (breeze--alternate-files
;;   (file-name-concat (vc-root-dir) "src/pattern.lisp"))
;;  (list (expand-file-name
;;         (file-name-concat (vc-root-dir) "tests/pattern.lisp"))))


;;; The names breeze-other-file and breeze-other-file-other-window are
;;; inspired by projectile's equivalent commands.

;; TODO would be nice if it suggested to create the alternate files if
;; it didn't exist.
(defun breeze-other-file ()
  "Open other file."
  (interactive)
  (let ((other-file (car (breeze--alternate-files (buffer-file-name)))))
    (when other-file
      (find-file other-file))))

(defun breeze-other-file-other-window ()
  "Open other file in other window."
  (interactive)
  (let ((other-file (car (breeze--alternate-files (buffer-file-name)))))
    (when other-file
      (find-file-other-window other-file))))


;;; managing threads

;; TODO as of 2022-02-08, there's code for that in listener.lisp
;; breeze-kill-worker-thread


;;; minor mode

(defvar breeze-minor-mode-map
  (make-sparse-keymap)
  "Keymap for breeze-minor-mode")

(define-minor-mode breeze-minor-mode
  "Toggle Breeze minor mode on or off

Breeze minor mode is an Emacs minor mode that complements lisp-mode."
  :lighter " brz"
  :keymap breeze-minor-mode-map
  :interactive (lisp-mode)
  (when breeze-minor-mode
    ;; TODO What if dabbrev-abbrev-skip-leading-regexp is already customized?
    (setf dabbrev-abbrev-skip-leading-regexp "\\(#?:\\)\\|+")))

;; Analogous to org-insert-structure-template
;; (define-key breeze-minor-mode-map (kbd "C-c C-,") 'breeze-insert)

;; Analogous to org-goto
(keymap-set breeze-minor-mode-map "C-c C-j" #'imenu)

;; Analogous to Visual Studio Code's "quickfix"
(keymap-set breeze-minor-mode-map "C-." #'breeze-quickfix)

;; TODO M-n M-p https://www.gnu.org/software/emacs/manual/html_node/flymake/Finding-diagnostics.html

(keymap-set breeze-minor-mode-map "M-p" #'breeze-previous-note)
(keymap-set breeze-minor-mode-map "M-n" #'breeze-next-note)

(keymap-set breeze-minor-mode-map "C-c o" #'breeze-other-file-other-window)

;; Disabled for now
;; eval keymap - because we might want to keep an history
;; (defvar breeze-eval-map (make-sparse-keymap))
;; eval last expression
;; (define-key breeze-minor-mode-map (kbd "C-c e") breeze-eval-map)
;; choose an expression from history to evaluate
;; (define-key breeze-eval-map (kbd "e") 'breeze-reevaluate-form)

(defun enable-breeze-minor-mode ()
  "Enable breeze-minor-mode."
  (interactive)
  (unless breeze-minor-mode
    (breeze-minor-mode 1)))

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
  ;; TODO actually disable flymake
  (remove-hook 'breeze-minor-mode-hook 'flymake-mode)
  (remove-hook 'breeze-minor-mode-hook 'breeze-enable-flymake-backend))

(defun breeze-enable-minor-mode-hook ()
  "Configure a hook to enable breeze-minor-mode in lisp-mode."
  (interactive)
  (add-hook 'lisp-mode-hook #'breeze-minor-mode))

(defun breeze-disable-minor-mode-hook ()
  "Configure a hook to enable breeze-minor-mode in lisp-mode."
  (interactive)
  (remove-hook 'lisp-mode-hook #'breeze-minor-mode))

;; TODO use the hook "change-major-mode-hook" (or
;; "after-change-major-mode-hook"??) to disable/enable
;; breeze-minor-mode


;;; major mode

(define-derived-mode breeze-major-mode prog-mode
  "BRZ")

(keymap-set breeze-major-mode-map "C-." #'breeze-quickfix)
(keymap-set breeze-major-mode-map "C-c C-c" #'breeze-eval-defun)
(keymap-set breeze-major-mode-map "C-c o" #'breeze-other-file-other-window)



(provide 'breeze)
;;; breeze.el ends here
