;; -*- lexical-binding: t -*-

;; This is for debugging
(server-start)

;; (switch-to-buffer "*Message*")

(defvar demo-listener-port (or (getenv "DEMO_LISTENER_PORT") 40050))

(setq shell-command-dont-erase-buffer t)

(defun demo-in-container-p ()
  "Test if emacs is running in a container."
  ;; This is a bad implementation, but it'll do for now.
  (= 0 (user-real-uid)))

(defun demo-load-listener ()
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

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("src/"
              "scripts/emacs-director/"))

(defvar *demo-root* (concat
                     (vc-find-root
                      (or (buffer-file-name
                           (current-buffer))
                          (pwd))
                      ".git")
                     "./scripts/demo"))

(defvar *demo-log-file* (concat *demo-root* "/demo.log"))


(defvar *demo-window-config* nil)

(defun demo-open-logs ()
  "To be used with :on-error, :on-failure, etc."
  (interactive)
  (find-file *demo-log-file*))

;; Can use this to pass arguments to the script
;; (print argv)

;; TODO Don't capture if (getenv "DISPLAY") is null
(defun demo-capture (name)
  (interactive)
  (let ((command
         ;; (format "tmux capture-pane -e -p > %s/%s.capture" *demo-root* name)
         (format "scrot %s/%s.png" *demo-root* name)
         ;; (format
         ;;  ;; "tmux capture-pane -e -p > %s/%s.capture" *demo-root* name
         ;;  "scrot %s/%s.png -n \"-t '%s'\" -x 10 -y 10 -c 255,0,0,255"
         ;;  *demo-root* name name)
         ))
    (director--log (format "About to run %S" command))
    (shell-command command)))

(defun demo-call-capture (name)
  (let ((name name))
    `(:call ,#'(lambda ()
                 (interactive)
                 (demo-capture name)))))

(defun demo-on-end (success)
  (if success
      (kill-emacs 0)
    (progn
      (save-excursion
        (with-current-buffer (get-buffer shell-command-buffer-name)
          (write-file (concat *demo-root* "/shell-command-output.log"))))
      ;; TODO For debugging, don't kill if an emacsclient is connected
      (kill-emacs 1))))

(director-run
 :version 1
 :before-start (lambda ()
                 ;;(require 'ivy)
                 ;; (selectrum-mode 1)
                 (switch-to-buffer (get-buffer-create "*example*"))
                 (menu-bar-mode -1)
                 (setf *demo-window-config* (current-window-configuration))
                 (demo-load-listener)
                 ;; Load breeze
                 (require 'breeze)
                 ;; Connect to lisp
                 (slime-connect "localhost" demo-listener-port))
 :steps `(,(demo-call-capture "slime-initialized")
          ;; (:wait 2)
          (:type "\M-x")
          (:type "breeze")
          (:type [return])
          ;; (:wait 2)
          ;; Switch to the other window
          (:call
           (lambda ()
             (interactive)
             (set-window-configuration *demo-window-config*)))

          ;; Breeze-mode
          (:call lisp-mode)
          (:call breeze-mode)
          ,(demo-call-capture "breeze-example-buffer")
          ;; Calling quickfix
          (:call breeze-quickfix)
          ,(demo-call-capture "breeze-quickfix-menu-1")
          ;; (:wait 300)
          )
 :typing-style 'human
 :log-target (cons 'file *demo-log-file*)
 :delay-between-steps 0.5
 :after-end (lambda () (demo-on-end t))
 :on-failure (lambda () (demo-on-end nil))
 :on-error (lambda () (demo-on-end nil)))


;; (listify-key-sequence ".")
;; (read-key-sequence-vector "")
