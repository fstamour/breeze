;;; package -- breeze integration with emacs
;; -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Features:
;; - snippets with abbrevs
;; - capture
;; - interactive make-project

;;; Code:


;;; Scratch section
;; (setf debug-on-error t)
;; (setf debug-on-error nil)


;;; Requires
(require 'cl-lib)


;;; Logging

(defun breeze-debug (string &rest objects)
  "Log a meesage in *breeze-debug* buffer."
  (save-current-buffer
    (set-buffer (get-buffer-create "*breeze-debug*"))
    (setf buffer-read-only nil)
    (goto-char (point-max))
    (insert
     "\n"
     (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] ")
     (apply #'format string objects))
    (setf buffer-read-only t)))

(defun breeze-message (string &rest objects)
  "Log a meesage in *breeze-debug* and *Messages* buffer."
  (apply #'message string objects)
  (apply #'breeze-debug string objects))



;;; Variables

(defvar breeze-minor-mode-map
  (make-sparse-keymap)
  "Keymap for breeze-minor-mode")


;;; Integration with lisp listener

;; Useful for debugging whether slime or sly is running
;; (process-list)

(defun breeze-sly-connection ()
  "If sly loaded, get the list of connections."
  (and (fboundp 'sly-current-connection)
       (sly-current-connection)))

(defun breeze-slime-connection ()
  "If slime is loaded, get the list of connections."
  (and (fboundp 'slime-current-connection)
       (slime-current-connection)))

(defun breeze-check-if-listener-loaded ()
  (or (fboundp 'sly)
      (fboundp 'slime)
      (error "Please load either slime or sly.")))

(defun breeze-choose-listener ()
  (cond
   ((and (fboundp 'sly)
         (fboundp 'slime))
    (completing-read "Choose a lisp listener to start: "
                     '("Sly" "SLIME") nil t))
   ((fboundp 'sly) "Sly")
   ((fboundp 'slime) "SLIME")))

(defun breeze-start-listener ()
  (let ((listener (breeze-choose-listener)))
    (cond
     ((string= listener "Sly") (sly))
     ((string= listener "SLIME") (slime))
     (t (error "Unknown listener: %S" listener)))))

(defun breeze-check-if-listener-connected (&optional errorp)
  (or (breeze-sly-connection)
      (breeze-slime-connection)
      (breeze-start-listener)
      (and errorp
           (error "Please start either slime or sly."))))

;; This is used by breeze-eval
(defun breeze-sly-or-not-slime ()
  "Tries to determine which listener to use, sly or slime?"
  ;; This errors if none is loaded
  (breeze-check-if-listener-loaded)
  ;; This errors if none is connected
  (breeze-check-if-listener-connected)
  (cond
   ((breeze-sly-connection) t)
   ((breeze-slime-connection) nil)))

(defun breeze-%eval (form)
  (if (breeze-sly-or-not-slime) (sly-eval form) (slime-eval form)))

(defun breeze-eval (string)
  (let ((value (breeze-%eval `(breeze.listener:rpc-eval ,string))))
    (breeze-debug "Breeze: got the value: %S" value)
    value))

;; (breeze-interactive-eval "1")
;; (breeze-interactive-eval "'(a b c)")
;; (breeze-interactive-eval "t")
;; (breeze-interactive-eval "(not nil)")

(defun breeze-eval-predicate (string)
  (breeze-eval string))

(defun breeze-eval-list (string)
  (breeze-eval string))


;;; Prerequisites checks

(cl-defun breeze-check-if-connected-to-listener (&optional verbosep)
  "Make sure that slime or sly is connected, signals an error otherwise."
  (if (or
       (breeze-sly-connection)
       (breeze-slime-connection))
      (progn
        (when verbosep
          (breeze-message "Slime or Sly already started."))
        t)
    ;; TODO Pretty sure we don't want this
    (when nil
      (progn
        (when verbosep
          (breeze-message "Starting slime..."))
        (slime)))))

(defun breeze-validate-if-package-exists (package)
  "Returns true if the package PACKAGE exists in the inferior-lisp."
  (breeze-debug "breeze-validate-if-package-exists %S" package)
  (breeze-%eval
   `(cl:eval
     (cl:and (cl:or (cl:find-package ,(downcase package))
                    (cl:find-package ,(upcase package)))
             t))))

(defun breeze-validate-if-breeze-package-exists ()
  "Returns true if the package \"breeze.utils\" exists in the inferior-lisp."
  (breeze-validate-if-package-exists "breeze.utils"))

;; (breeze-validate-if-breeze-package-exists)

(defvar breeze-breeze.el load-file-name
  "Path to \"breeze.el\".")

;; I don't remember why I needed this? maybe I had redefined the
;; defcommand macro.
(defun breeze-reload ()
  (breeze-eval "(asdf:load-system '#:breeze :force t)"))

(cl-defun breeze-ensure-breeze ()
  "Make sure that breeze is loaded in swank or slynk."
  (unless (breeze-validate-if-breeze-package-exists)
    (breeze-message "Loading breeze's system...")
    (breeze-%eval
     `(cl:load ,(expand-file-name
                 (concat
                  (file-name-directory
                   breeze-breeze.el)
                  "/ensure-breeze.lisp")))))
  (breeze-debug "Breeze loaded in inferior-lisp."))

;; (breeze-ensure-breeze)

;; See slime--setup-contribs, I named this breeze-init so it _could_
;; be added to slime-contrib,
;; I haven't tested it yet though.
(cl-defun breeze-init (&optional verbosep)
  "Ensure that breeze is initialized correctly on swank's side."
  (interactive)
  (breeze-check-if-connected-to-listener)
  (breeze-ensure-breeze)
  (when verbosep
    (breeze-message "Breeze initialized.")))


(defmacro breeze-with-listener (&rest body)
  "Make sure breeze is loaded on the common lisp side, then run BODY."
  `(progn
     (breeze-init)
     ,@body))

;; (macroexpand-1 '(breeze-with-listener (print 42)))

(defun breeze-cl-to-el-list (list)
  "Convert NIL to nil and T to t.
Common lisp often returns the symbols capitalized, but emacs
lisp's reader doesn't convert them."
  (if (eq list 'NIL)
      nil
    (mapcar #'(lambda (el)
                (cl-case el
                  (NIL nil)
                  (T t)
                  (t el)))
            list)))


;;; Common lisp driven interactive commands

(defun breeze-compute-buffer-args ()
  (apply 'format
         ":buffer-name %s
          :buffer-file-name %s
          :buffer-string %s
          :point %s
          :point-min %s
          :point-max %s"
         (mapcar #'prin1-to-string
                 (list
                  (buffer-name)
                  (buffer-file-name)
                  (buffer-substring-no-properties (point-min) (point-max))
                  (1- (point))
                  (1- (point-min))
                  (1- (point-max))))))

;; TODO extra-args
(defun breeze-command-start (name)
  "Returns an id"
  (breeze-debug "Breeze: starting command: %s." name)
  (let ((id (breeze-eval
             (format "(breeze.command:start-command '%s '(%s) %s)"
                     name
                     (breeze-compute-buffer-args)
                     ;; TODO extra-args
                     nil))))
    (breeze-debug "Breeze: start-command %S returned %s" name id)
    id))

(defun breeze-command-cancel (id reason)
  (breeze-eval
   (format "(breeze.command:cancel-command %s %S)" id reason))
  (breeze-debug "Breeze: command %s canceled." id))
;; (breeze-command-cancel)

(defun breeze-command-continue (id response send-response-p)
  (let ((request
         (breeze-eval
          (if send-response-p
              (format
               "(breeze.command:continue-command %s %S)" id response)
            (format "(breeze.command:continue-command %s)" id)))))
    (breeze-debug "Breeze: (#%s) request received: %s" id request)
    request))

(defun breeze-command-process-request (request)
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
       (kill-region (1+ point-from) (1+ point-to))
       (goto-char (1+ point-from))
       (insert replacement-string)))
    ("backward-char"
     (backward-char (cl-second request))
     ;; Had to do this hack so the cursor is positioned
     ;; correctly... probably because of aggressive-indent
     (funcall indent-line-function))
    ("message"
     (message "%s" (cl-second request)))
    ("find-file"
     (find-file (cl-second request)))
    (_ (error "Invalid request: %S" request) )))


;; TODO extra-args
(defun breeze-run-command (name)
  "Runs a \"breeze command\". TODO Improve this docstring."
  (interactive)
  (breeze-debug "breeze-run-command")
  (breeze-ensure-breeze)
  ;; TODO extra-args
  (let ((id (breeze-command-start name)))
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


;;; quickfix (similar to code actions in visual studio code)

(defun breeze-quickfix ()
  "Choose from a list of commands applicable to the current context."
  (interactive)
  (breeze-run-command "breeze.refactor:quickfix"))

(defun breeze-insert-defpackage ()
  "Choose a command from a list of applicable to the current context."
  (interactive)
  (breeze-run-command "breeze.refactor:insert-defpackage"))

(defun breeze-eval-defun ()
  "Evaluate current top-level form."
  (interactive)
  (breeze-run-command "breeze.listener:interactive-eval-command"))

;; TODO narrow-to-defun (maybe clone the buffer too?) -- "focus to defun"...


;;; code evaluation
;;
;; TODO This will not work for a while, I want to wrap both sly and
;; slime on emacs side and call a "breeze-eval" on lisp side. Then I'm
;; not sure I want this exact feature...
;;

(when nil
  (defun breeze-get-recently-evaluated-forms ()
    "Get recently evaluated forms from the server."
    (cl-destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output
                      "(breeze.listener:get-recent-interactively-evaluated-forms)"))
      (split-string output "\n"))))

(when nil
  (defun breeze-reevaluate-form ()
    (interactive)
    (let ((form (completing-read  "Choose recently evaluated form: "
                                  (breeze-get-recently-evaluated-forms))))
      (when form
        (slime-interactive-eval form)))))


;;; project scaffolding

(defun breeze-scaffold-project ()
  "Create a project using quickproject."
  (interactive)
  (breeze-run-command "breeze.project:scaffold-project"))


;;; capture

(defun breeze-capture ()
  "Create a file ready to code in."
  (interactive)
  (breeze-run-command "breeze.capture:capture"))


;;; WIP Alternate files (this is currently very brittle, but it should
;;; work for most of my projects).
;;;
;;; TODO Better docstrings

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

(define-minor-mode breeze-minor-mode
  "Breeze mimor mode."
  :lighter " brz"
  :keymap breeze-minor-mode-map)

;; Analogous to org-insert-structure-template
;; (define-key breeze-minor-mode-map (kbd "C-c C-,") 'breeze-insert)

;; Analogous to org-goto
(define-key breeze-minor-mode-map (kbd "C-c C-j") #'imenu)

;; Analogous to Visual Studio Code's "quickfix"
(define-key breeze-minor-mode-map (kbd "C-.") #'breeze-quickfix)

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
  (breeze-minor-mode 1))

(defun disable-breeze-minor-mode ()
  "Disable breeze-minor-mode."
  (interactive)
  (breeze-minor-mode -1))

;; (add-hook 'breeze-minor-mode-hook 'breeze-init)
;; TODO This should be in the users' config
;; (add-hook 'slime-lisp-minor-mode-hook 'breeze-init)
;; TODO This should be in the users' config


;;; major mode

(define-derived-mode breeze-major-mode prog-mode
  "BRZ")

(define-key breeze-major-mode-map (kbd "C-.") #'breeze-quickfix)

(define-key breeze-major-mode-map (kbd "C-c C-c") #'breeze-eval-defun)



;; TODO define-key: This is a legacy function; see ‘keymap-set’ for
;; the recommended function to use instead.


(defun breeze ()
  "Initialize breeze."
  (interactive)
  (breeze-init t))

(provide 'breeze)
;;; breeze.el ends here
