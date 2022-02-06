;; -*- lexical-binding: t -*-

(let ((libressl-pem (getenv "LIBRESSL_PEM")))
  (when libressl-pem
    (require 'gnutls)
    (add-to-list 'gnutls-trustfiles
                 ;; TODO Fix LIBRESSL_PEM in docker.nix, so we can use
                 ;; that instead of hard-coding this.
                 ;; P.S. I found that file using `find / -name '*.pem'`
                 "/nix/store/z9dmcm0b2n25nfgb3csypn7z250y6ihf-libressl-3.4.1/etc/ssl/cert.pem")))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install 'selectrum)


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
		     "/scripts/demo"))

(server-start)

(defvar *demo-window-config* nil)

;; Can use this to pass arguments to the script
;; (print argv)

(defun capture (name)
  (interactive)
  (shell-command
   (format
    ;; "tmux capture-pane -e -p > %s/%s.capture" *demo-root* name
    "scrot %s/%s.png -n '%s'" *demo-root* name name)))

(defun call-capture (name)
  (let ((name name))
    `(:call ,#'(lambda ()
		 (interactive)
		 (capture name)))))

(director-run
 :version 1
 :before-start (lambda ()
		 ;;(require 'ivy)
		 (selectrum-mode 1)
                 (switch-to-buffer (get-buffer-create "*example*"))
                 (menu-bar-mode -1)
		 (setf *demo-window-config*
		       (current-window-configuration))
		 ;; Load slime
		 (load "~/quicklisp/slime-helper.el")
		 ;; Configure slime
		 (setq slime-lisp-implementations
		       '((sbcl ("sbcl"
				"--noinform"
				"--dynamic-space-size" "16000"
                                "--load" "breeze.asd"
                                "--eval" "(ql:quickload :breeze)")
			       :coding-system utf-8-unix))
		       slime-default-lisp 'sbcl
		       ;; Autocomplete
		       slime-complete-symbol-function
		       'slime-fuzzy-complete-symbol)
		 ;; Load breeze
		 ;; (require 'breeze)
		 ;; Start slime
		 ;; (slime-connect "localhost" 40050)
                 (slime)
                 )
 :steps `(,(call-capture "slime-initialized")
	  (:type "\M-x")
	  (:type "breeze")
	  (:type [return])
	  (:wait 2)
	  ;; Switch to the other window
	  (:call
	   (lambda ()
	     (interactive)
	     (set-window-configuration *demo-window-config*)))

	  ;; Breeze-mode
	  (:call lisp-mode)
	  (:call breeze-mode)
	  ,(call-capture "breeze-example-buffer")
	  ;; Calling quickfix
	  (:call breeze-quickfix)
	  ,(call-capture "breeze-quickfix-menu-1"))
 :typing-style 'human
 :log-target (cons 'file (concat *demo-root* "/demo.log"))
 :delay-between-steps 0.5
 :after-end (lambda () (kill-emacs 0))
 :on-failure (lambda () (kill-emacs 1))
 :on-error (lambda () (kill-emacs 1)))


;; (listify-key-sequence ".")
;; (read-key-sequence-vector "")
