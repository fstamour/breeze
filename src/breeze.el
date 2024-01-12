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
  "Check if sly loaded, get the list of connections."
  (and (fboundp 'sly-connected-p)
       (sly-connected-p)))

(defun breeze-slime-connected-p ()
  "If slime is loaded, get the list of connections."
  (and (fboundp 'slime-connected-p)
       (slime-connected-p)))

(cl-defun breeze-listener-connected-p (&optional (errorp t))
  (or (breeze-sly-connected-p)
      (breeze-slime-connected-p)
      (and errorp
           (error "Please start either slime or sly."))))

(cl-defun breeze-list-loaded-listeners (&optional (errorp t))
  "Returns a list of loaded listneres (sly or slime)."
  (or (remove 'nil (list (and (fboundp 'sly) 'sly)
                         (and (fboundp 'slime) 'slime)))
      (and errorp
           (error "Please load either slime or sly."))))

(cl-defun breeze-choose-listener (&optional (errorp t))
  (let ((listeners (breeze-list-loaded-listeners errorp)))
    (when listeners
      (if (= (length listeners) 1)
          (first listeners)
        (intern
         (completing-read "Choose a lisp listener to start: "
                          listeners nil t))))))

(defun breeze-start-listener ()
  "Start a listener (e.g. calls \"(sly)\" or \"(slime)\")."
  (interactive)
  (let ((listener (breeze-choose-listener)))
    (funcall listener)))

(defun breeze-ensure-listener ()
  (or (breeze-listener-connected-p nil)
      (breeze-start-listener)))


;;; Evaluation

(defun breeze-%eval (form)
  (funcall (intern (format "%s-eval" (breeze-choose-listener)))
           form))

(defun breeze-%eval-async (form &optional cont package)
  (funcall (intern (format "%s-eval-async" (breeze-choose-listener)))
           form cont package))

(defun breeze-eval (string)
  (let ((value (breeze-%eval `(breeze.listener:rpc-eval ,string))))
    (breeze-debug "Breeze: got the value: %S" value)
    value))

;; TODO async eval!


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

(defun breeze-command-start (name &optional extra-args)
  "Returns an id"
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


;; TODO maybe add a "narrow" request type?
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
    ("message"
     (message "%s" (cl-second request)))
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
  (if (string-suffix-p suffix string)
      (subseq string 0 (- (length string)
                          (length suffix)))
    string))

(defun breeze-translate-command-symbol (symbol)
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
  (loop for symbol in lambda-list
        for sanitized-symbol = (intern (car (last (split-string (symbol-name symbol) ":"))))
        collect sanitized-symbol))

(defun breeze-refresh-commands ()
  "Ask the inferior lisp which commands it has and define
corresponding commands in emacs."
  (interactive)
  (cl-loop for (symbol cl-lambda-list docstring) in (breeze-eval "(breeze.command:list-all-commands t)")
           for (cl-symbol el-symbol) = (breeze-translate-command-symbol symbol)
           for el-lambda-list = (breeze-translate-command-lambda-list cl-lambda-list)
           do (eval `(cl-defun ,el-symbol (&optional ,@el-lambda-list)
                       ,docstring
                       (interactive "" 'breeze-minor-mode 'breeze-major-mode)
                       (breeze-run-command ,(symbol-name cl-symbol) ,@el-lambda-list)))))




;;; Initializations

(defun breeze-validate-if-package-exists (package)
  "Returns true if the package PACKAGE exists in the inferior lisp."
  (breeze-debug "breeze-validate-if-package-exists %S" package)
  (breeze-%eval
   `(cl:eval
     (cl:and (cl:or (cl:find-package ,(downcase package))
                    (cl:find-package ,(upcase package)))
             t))))

(defun breeze-validate-if-breeze-package-exists ()
  "Returns true if the package \"breeze.utils\" exists in the
inferior lisp."
  (breeze-validate-if-package-exists "breeze.utils"))

(defvar breeze-breeze.el load-file-name
  "Path to \"breeze.el\".")

(defun breeze-relative-path (&rest components)
  (expand-file-name
   (apply 'file-name-concat
          (file-name-directory breeze-breeze.el)
          ".."
          components)))

(cl-defun breeze-load (&optional cont)
  "Load breeze into the inferior system."
  (breeze-%eval-async
   `(cl:load ,(breeze-relative-path "src/ensure-breeze.lisp"))
   cont))

(cl-defun breeze-ensure ()
  "Make sure that breeze is loaded in the inferior lisp."
  (unless (breeze-validate-if-breeze-package-exists)
    (breeze-message "Loading breeze's system asynchronously...")
    (breeze-load
     (lambda (&rest _)
       (breeze-message "Breeze loaded in inferior-lisp.")
       (breeze-refresh-commands)))))


;; See slime--setup-contribs, I named this breeze-init so it _could_
;; be added to slime-contrib,
(cl-defun breeze ()
  "Initialize breeze."
  (interactive)
  (breeze-ensure-listener)
  (breeze-ensure)
  (breeze-debug "Breeze initialized (might still be loading in the inferior lisp."))


;;; Hooks

;; TODO This is experimental! I mean... more than the rest xD
(defun breeze-%%%setup-hooks (listener)
  (when (eq 'slime listener)
    (cl-loop for hook in '(slime-connected-hook
                           slime-inferior-process-start-hook
                           slime-net-process-close-hooks
                           slime-cycle-connections-hook
                           slime-connected-hook
                           slime-event-hooks)
             do (add-hook hook (lambda (&rest args) (breeze-debug "%S: %S" hook args) nil)) )))

(defun breeze-connected-hook-function ()
  (breeze-ensure))

;; TODO this assumes slime
(defun breeze-add-hooks (listener)
  (add-hook 'slime-connected-hook 'breeze-connected-hook-function))

;; TODO this assumes slime
(defun breeze-remove-hooks (listener)
  (remove-hook 'slime-connected-hook 'breeze-connected-hook-function))



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

(provide 'breeze)
;;; breeze.el ends here
