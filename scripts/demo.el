
(package-refresh-contents)
(package-install 'selectrum)
(package-initialize)

(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("src/"
	      "scripts/emacs-director/"))

(defvar *demo-window-config* nil)

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
				"--dynamic-space-size" "16000")
			       :coding-system utf-8-unix))
		       slime-default-lisp 'sbcl
		       ;; Autocomplete
		       slime-complete-symbol-function
		       'slime-fuzzy-complete-symbol)
		 ;; Load breeze
		 (require 'breeze)
		 ;; Start slime
		 ;; (slime)
		 )
 :steps '((:type "\M-x")
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
	  ;; Calling quickfix
	  (:call breeze-quickfix)
	  (:wait 5))
 :typing-style 'human
 :log-target '(file . "demo.log")
 :delay-between-steps 0.5
 :after-end (lambda () (kill-emacs 0))
 :on-failure (lambda () (kill-emacs 1))
 :on-error (lambda () (kill-emacs 1)))


;; (listify-key-sequence ".")
;; (read-key-sequence-vector "")
