;; -*- lexical-binding: t -*-

;; This is for debugging
(server-start)

;; Can use this to pass arguments to the script
;; (print argv)

;; (setf debug-on-error t)


;;;; Variables

;; sly tries to "remove" slime whenever lisp-mode is activated, let's
;; start clean
(setf lisp-mode-hook nil)

(defvar demo-listener-port
  (or (getenv "DEMO_LISTENER_PORT") 40050)
  "Which port common lisp listener (repl) is listening to.")

(defvar breeze-root
  (vc-find-root
   (or load-file-name
       (buffer-file-name
        (current-buffer))
       (pwd))
   ".git")
  "Directory where breeze is cloned.")

(defvar demo-root
  (expand-file-name
   "./scripts/demo"
   breeze-root)
  "Directory where to find the other scripts.")

(defvar emacs-director-root
  (expand-file-name
   "./scripts/emacs-director"
   breeze-root)
  "Directory where emacs-director is cloned.")

(defvar demo-output-dir (or (getenv "DEMO_OUTPUT_DIR")
                            (expand-file-name "output/" demo-root))
  "Directory where to save the outputs.")

(defun demo-log-file ()
  "Location of emacs-director's log file."
  (concat demo-output-dir "/demo.log"))

(defvar demo-window-config nil
  "The window configuration before the lisp listener client is stated.")

;; Also for debugging, this is to keep the outputs of all
;; shell-command (it's in the buffer shell-command-buffer-name).
;; See demo-finally
(setq shell-command-dont-erase-buffer t)

(setq
 shell-command-buffer-name "*Shell Command Output*"
 ;; Put the error in the same buffer as the output
 shell-command-default-error-buffer shell-command-buffer-name)


;;; Helper functions

(defun demo-log (format-string &rest args)
  "Log a message."
  ;; Binding inhibit-message to t will still log the messages to
  ;; *Messages*, but they won't be displayed, making sure this
  ;; script's logs won't appear in the screenshots.
  (let ((inhibit-message t))
    (apply #'message format-string args)))

(defun demo-in-container-p ()
  "Test if emacs is running in a container."
  ;; This is a bad implementation, but it'll do for now.
  ;; I _will_ break the day I don't use the root user in the container
  (or (= 0 (user-real-uid))
      ;; if pid 1 is emacs,321 then there's a very good chance we're
      ;; currently running inside a container.
      (string= "emacs\0"
               (with-temp-buffer
                 (insert-file "/proc/1/cmdline")
                 (buffer-string)))))

(defun demo-load-listener-client ()
  "Load slime or slynk (spoiler: only slime is supported for now)"
  ;; Load slime
  (unless (fboundp 'slime)
    (if (demo-in-container-p)
        (progn
          (add-to-list 'load-path "/swank")
          (require 'slime-autoloads)
          (slime-setup '(slime-fancy)))
      (load "~/quicklisp/slime-helper.el")))
  ;; Configure slime
  (setq
   ;; Autocomplete
   slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

(defun demo-open-logs ()
  "To be used with :on-error, :on-failure, etc."
  (interactive)
  (find-file (demo-log-file)))

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
  (let ((command (format "scrot %s/%s.png" demo-output-dir name)))
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
    (write-file (concat demo-output-dir "/shell-command-output.log")))
  (with-current-buffer (get-buffer "*Messages*")
    (write-file (concat demo-output-dir "/messages.log")))
  ;; (if success
  ;;     (kill-emacs 0)
  ;;   (progn
  ;;     ;; TODO For debugging, don't kill if an emacsclient is connected
  ;;     (kill-emacs 1)))
  )

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
  (slime)
  ;; (slime-connect "localhost" demo-listener-port)
  )

(defun demo-generate-steps ()
  `(,(demo-call-capture "slime-initialized")
    ;; Initialize breeze (it already is though... I'll keep it
    ;; for when I do the demo that shows how to setup breeze.)
    (:type "\M-x")
    (:type "breeze-init")
    ;; (:type "slime")
    ;; (:type [return])
    (:call demo-restore-window-configuration)
    ;; Breeze-mode
    ;; (:call lisp-mode)
    ;; (:call breeze-minor-mode)
    ,(demo-call-capture "breeze-example-buffer")
    ;; Calling quickfix
    (:call breeze-quickfix)
    ,(demo-call-capture "breeze-quickfix-menu-1")
    ;; This wait is useuful to give some time to inspect to
    ;; state of affairs with emacsclient.
    ;; (:wait 600)
    ))


;;; Running emacs-director

;; TODO use demo-log?
(message "demo-root: %S" demo-root)

;; Ensure emacs-director is loaded
(unless (fboundp 'director-bootstrap)
  (let ((emacs-directory-path (expand-file-name "util/director-bootstrap.el" emacs-director-root)))
    (message "About to load %S" emacs-directory-path)
    (load emacs-directory-path)))

(when (demo-in-container-p)
  (setf demo-output-dir "/tmp/demo/output"))
;; TODO check if it already exist
(mkdir demo-output-dir t)

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path (list
             (expand-file-name "emacs" breeze-root)
             emacs-director-root))

(let ((steps (demo-generate-steps)))
  (demo-log "Steps: %S" steps)
  (demo-log "command-line-args-left: %S" command-line-args-left)
  (director-run
   :version 1
   :before-start #'demo-before-start
   :steps steps
   :typing-style 'human
   :log-target (cons 'file (demo-log-file))
   :delay-between-steps 0.5
   :after-end (lambda () (demo-finally t))
   :on-failure (lambda () (demo-finally nil))
   :on-error (lambda () (demo-finally nil))))


;; (listify-key-sequence ".")
;; (read-key-sequence-vector "")
