(director-bootstrap
 :user-dir "/tmp/director-demo"
 :packages '()
 :load-path '("src/"
	      "scripts/emacs-director/"))

(director-run
 :version 1
 :before-start (lambda ()
                 (switch-to-buffer (get-buffer-create "*example*"))
                 (menu-bar-mode -1)
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
		       slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
		 ;; Load breeze
		 (require 'breeze)
		 ;; Start slime
		 ;; (slime)
		 )
 :steps '((:type "\M-x")
	  (:type "breeze-mode")
	  (:type [return])
	  ;; Switch to the other window
	  (:type "\C-x")
	  (:type "\C-o"))
 :typing-style 'human
 :delay-between-steps 1
 :after-end (lambda () (kill-emacs 0))
 :on-error (lambda () (kill-emacs 1)))
