;; -*- lexical-binding: t -*-

;; This is for debugging
(server-start)

;; Can use this to pass arguments to the script
;; (print argv)

(setf debug-on-error t)


;;; Variables

(defvar demo-listener-port
  (or (getenv "DEMO_LISTENER_PORT") 40050)
  "Which port common lisp listener (repl) is listening to.")

(defvar demo-root (concat
                   (vc-find-root
                    (or (buffer-file-name
                         (current-buffer))
                        (pwd))
                    ".git")
                   "./scripts/demo")
  "Directory where to get the other scripts and to save the outputs.")

(defvar demo-log-file (concat demo-root "/demo.log")
  "Location of emacs-director's log file.")

(defvar demo-window-config nil
  "The window configuration before the lisp listener client is stated.")

;; Also for debugging, this is to keep the outputs of all
;; shell-command (it's in the buffer shell-command-buffer-name).
;; See demo-finally
(setq shell-command-dont-erase-buffer t)

(setq
 ;; This variable was added in emacs 28
 shell-command-buffer-name "*Shell Command Output*"
 ;; Put the error in the same buffer as the errors
 shell-command-default-error-buffer shell-command-buffer-name)


;;; Helper functions

(defun demo-log (format-string &rest args)
  "Log a message."
  ;; Binding inhibit-message to nil will still log the messages to
  ;; *Messages*, but they won't be displayed, making sure this
  ;; script's logs won't appear in the screenshots.
  (let (inhibit-message)
    (apply #'message format-string args)))

(defun demo-in-container-p ()
  "Test if emacs is running in a container."
  ;; This is a bad implementation, but it'll do for now.
  ;; I _will_ break the day I don't use the root user in the container
  (= 0 (user-real-uid)))

(defun demo-load-listener-client ()
  "Load slime or slynk (spoiler: only slime is supported for now)"
  ;; Load slime
  (if (demo-in-container-p)
      (progn
        (add-to-list 'load-path "/swank")
        (require 'slime-autoloads)
        (slime-setup '(slime-fancy)))
    (load "~/quicklisp/slime-helper.el"))
  ;; Configure slime
  (setq
   ;; Autocomplete
   slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

(defun demo-open-logs ()
  "To be used with :on-error, :on-failure, etc."
  (interactive)
  (find-file demo-log-file))

(defun demo-shell-command (command)
  "Wraps shell-command, to makes sure all the outputs are sent to the right buffer."
  (shell-command command
                 ;; output-buffer
                 shell-command-buffer-name
                 ;; error-buffer
                 shell-command-buffer-name))

(defun demo-capture (name)
  "Take a screenshot (using scrot)"
  (interactive)
  (let ((command (format "scrot %s/%s.png" demo-root name)))
    (demo-log "About to run %S" command)
    (demo-shell-command command)))

(defun demo-call-capture (name)
  "Generate a definition of a step that calls demo-capture."
  (let ((name name))
    `(:call ,#'(lambda ()
                 (interactive)
                 (demo-capture name)))))

(defun demo-finally (success)
  "Called when the emacs-director run ends."
  (with-current-buffer (get-buffer shell-command-buffer-name)
    (write-file (concat demo-root "/shell-command-output.log")))
  (with-current-buffer (get-buffer "*Messages*")
    (write-file (concat demo-root "/messages.log")))
  (if success
      (kill-emacs 0)
    (progn
      ;; TODO For debugging, don't kill if an emacsclient is connected
      (kill-emacs 1))))

(defun demo-restore-window-configuration ()
  (interactive)
  (set-window-configuration demo-window-config))

(defun demo-before-start ()
  ;;(require 'ivy)
  ;; (selectrum-mode 1)
  (switch-to-buffer (get-buffer-create "example.lisp"))
  (menu-bar-mode -1)
  (setf demo-window-config (current-window-configuration))
  (demo-load-listener-client)
  ;; Load breeze
  (require 'breeze)
  ;; Connect to lisp
  (slime-connect "localhost" demo-listener-port))

(defun demo-generate-steps ()
  `(,(demo-call-capture "slime-initialized")
    ;; Initialize breeze (it already is though... I'll keep it
    ;; for when I do the demo that shows how to setup breeze.)
    (:type "\M-x")
    (:type "breeze")
    (:type [return])
    (:call demo-restore-window-configuration)
    ;; Breeze-mode
    (:call lisp-mode)
    (:call breeze-mode)
    ,(demo-call-capture "breeze-example-buffer")
    ;; Calling quickfix
    (:call breeze-quickfix)
    ,(demo-call-capture "breeze-quickfix-menu-1")
    ;; This wait is useuful to give some time to inspect to
    ;; state of affairs with emacsclient.
    ;; (:wait 600)
    ))


;;; Running emacs-director

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("src/"
              "scripts/emacs-director/"))

(let ((steps (demo-generate-steps)))
  (demo-log "Steps: %S" steps)
  (demo-log "command-line-args-left: %S" command-line-args-left)
  (director-run
   :version 1
   :before-start #'demo-before-start
   :steps steps
   :typing-style 'human
   :log-target (cons 'file demo-log-file)
   :delay-between-steps 0.5
   :after-end (lambda () (demo-finally t))
   :on-failure (lambda () (demo-finally nil))
   :on-error (lambda () (demo-finally nil))))


;; (listify-key-sequence ".")
;; (read-key-sequence-vector "")
