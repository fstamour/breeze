

(defun breeze--insert-interactive-commands (prefix)
  "Insert list of commands that starts with PREFIX."
  (loop for x being the symbols
        if (and (commandp x)
                (or (string-prefix-p prefix (symbol-name x))))
        do (insert (symbol-name x) "\n")))

(defun breeze--insert-describe-keymap (keymap)
  "Insert the description of KEYMAP"
  (describe-keymap keymap)
  (insert
   (mapconcat
    (lambda (line)
      (concat ";; " line "\n"))
    (split-string
     (with-current-buffer "*Help*"
       (buffer-substring-no-properties (point-min) (point-max)))
     "\n"))))


;;; Paredit

(breeze--insert-interactive-commands "paredit-")

paredit-backward-up
paredit-backward-down
paredit-comment-dwim
paredit-add-to-next-list
paredit-add-to-previous-list
paredit-close-parenthesis-and-newline
paredit-wrap-square
paredit-split-sexp
paredit-RET
paredit-semicolon
paredit-C-j
paredit-wrap-sexp
paredit-open-square
paredit-hack-kill-region
paredit-meta-doublequote-and-newline
paredit-convolute-sexp
paredit-close-angled
paredit-close-round-and-newline
paredit-newline
paredit-meta-doublequote
paredit-close-bracket-and-newline
paredit-forward-down
paredit-backslash
paredit-close-round
paredit-kill-region
paredit-wrap-curly
paredit-copy-as-kill
paredit-recentre-on-sexp
paredit-join-with-next-list
paredit-close-square
paredit-open-curly
paredit-delete-region
paredit-join-with-previous-list
paredit-backward-kill-word
paredit-backward-delete
paredit-wrap-angled
paredit-reindent-defun
paredit-recenter-on-defun
paredit-open-angled
paredit-yank-pop
paredit-backward-slurp-sexp
paredit-splice-sexp
paredit-backward
paredit-forward-up
paredit-forward
paredit-close-bracket
paredit-focus-on-defun
paredit-close-parenthesis
paredit-open-parenthesis
paredit-insert-html-examples
paredit-open-bracket
paredit-wrap-round
paredit-close-angled-and-newline
paredit-open-round
paredit-delete-char
paredit-forward-barf-sexp
paredit-splice-sexp-killing-backward
paredit-join-sexps
paredit-close-curly
paredit-close-square-and-newline
paredit-forward-slurp-sexp
paredit-mode
paredit-splice-sexp-killing-forward
paredit-forward-delete
paredit-backward-barf-sexp
paredit-raise-sexp
paredit-kill
paredit-forward-kill-word
paredit-recenter-on-sexp
paredit-close-curly-and-newline
paredit-doublequote


(breeze--insert-describe-keymap paredit-mode-map)
;;
;; Key             Binding
;;
;; C-d		paredit-delete-char
;; C-j		paredit-C-j
;; C-k		paredit-kill
;; RET		paredit-RET
;; "		paredit-doublequote
;; (		paredit-open-round
;; )		paredit-close-round
;; ;		paredit-semicolon
;; [		paredit-open-square
;; \		paredit-backslash
;; ]		paredit-close-square
;; DEL		paredit-backward-delete
;; C-(		paredit-backward-slurp-sexp
;; C-)		paredit-forward-slurp-sexp
;; C-{		paredit-backward-barf-sexp
;; C-}		paredit-forward-barf-sexp
;; C-M-<left>	paredit-backward-slurp-sexp
;; C-M-<right>	paredit-backward-barf-sexp
;; C-<left>	paredit-forward-barf-sexp
;; C-<right>	paredit-forward-slurp-sexp
;; M-<down>	paredit-splice-sexp-killing-forward
;; M-<up>		paredit-splice-sexp-killing-backward
;; <delete>	paredit-forward-delete
;; <deletechar>	paredit-forward-delete
;;
;; C-M-b		paredit-backward
;; C-M-d		paredit-forward-down
;; C-M-f		paredit-forward
;; C-M-n		paredit-forward-up
;; C-M-p		paredit-backward-down
;; C-M-u		paredit-backward-up
;; M-"		paredit-meta-doublequote
;; M-(		paredit-wrap-round
;; M-)		paredit-close-round-and-newline
;; M-;		paredit-comment-dwim
;; M-?		paredit-convolute-sexp
;; M-J		paredit-join-sexps
;; M-S		paredit-split-sexp
;; M-d		paredit-forward-kill-word
;; M-q		paredit-reindent-defun
;; M-r		paredit-raise-sexp
;; M-s		paredit-splice-sexp
;; M-DEL		paredit-backward-kill-word
;; ESC C-<left>	paredit-backward-slurp-sexp
;; ESC C-<right>	paredit-backward-barf-sexp
;; ESC <down>	paredit-splice-sexp-killing-forward
;; ESC <up>	paredit-splice-sexp-killing-backward
;;
;; C-c C-M-l	paredit-recenter-on-sexp
;;
;; [back]
;;



;;; Lispy

(breeze--insert-interactive-commands "lispy-")

lispy-brackets-barf-to-point-or-jump-nostring
lispy-tab
lispy-left
lispy-parens-auto-wrap
lispy-underscore
lispy-join
lispy-build-semanticdb
lispy-backward
lispy-extract-block
lispy-x
lispy-mark-car
lispy-move-end-of-line
lispy-goto-def-down
lispy-eval
lispy-show-top-level
lispy-goto-local
lispy-move-outline-up
lispy-to-ifs
lispy-flatten
lispy-goto-elisp-commands
lispy-raise-sexp
lispy-newline-and-indent
lispy-goto-mode
lispy-expr-canonical-p
lispy-backward-slurp-sexp
lispy-mark
lispy-back
lispy-goto-projectile
lispy-barf
lispy-stringify-oneline
lispy-delete
lispy-arglist-inline
lispy-brackets-auto-wrap
lispy-ace-symbol
lispy-raise
lispy-comment
lispy-mode
lispy-mark-right
lispy-right
lispy-outline-right
lispy-slurp-or-barf-left
lispy-close-square
lispy-doublequote
lispy-fill
lispy-cursor-ace
lispy-parens-down
lispy-convolute-left
lispy-newline-and-indent-plain
lispy-close-curly
lispy-convolute-sexp
lispy-goto-recursive
lispy-brackets
lispy-down-slurp
lispy-eval-current-outline
lispy-shifttab
lispy-toggle-thread-last
lispy-move-up
lispy-reverse
lispy-out-forward-newline
lispy-splice-sexp-killing-forward
lispy-unstringify
lispy-ace-symbol-replace
lispy-cd
lispy-follow
lispy-cleanup
lispy-to-defun
lispy-forward-barf-sexp
lispy-stringify
lispy-ediff-regions
lispy-edebug
lispy-up
lispy-move-beginning-of-line
lispy-meta-return
lispy-widen
lispy-open-line
lispy-view-test
lispy-right-nostring
lispy-kill-word
lispy-unbind-variable-clojure
lispy-different
lispy-tick
lispy-outline-promote
lispy-ace-subword
lispy-oneline
lispy-left-maybe
lispy-narrow
lispy-forward-delete
lispy-braces-auto-wrap
lispy-outline-prev
lispy-outline-next
lispy-delete-backward-or-splice-or-slurp
lispy-mark-list
lispy-debug-step-in
lispy-edebug-stop
lispy-forward-slurp-sexp
lispy-store-region-and-buffer
lispy-quotes
lispy-tilde
lispy-wrap-round
lispy-eval-and-replace
lispy-wrap-brackets
lispy-delete-backward
lispy-iedit
lispy-braces-barf-to-point-or-jump-nostring
lispy-hash
lispy-ace-char
lispy-move-right
lispy-other-verb
lispy-mark-left
lispy-forward
lispy-alt-multiline
lispy-teleport
lispy-backward-barf-sexp
lispy-indent-adjust-parens
lispy-parens
lispy-outline-goto-child
lispy-alt-line
lispy-multiline
lispy-close-round-and-newline
lispy-other-space
lispy-quit
lispy-map-done
lispy-comment-region
lispy-down
lispy-cursor-down
lispy-knight-up
lispy-flow
lispy-describe-inline
lispy-colon
lispy-clone
lispy-splice-sexp-killing-backward
lispy-string-oneline
lispy-dedent-adjust-parens
lispy-hat
lispy-barf-to-point-nostring
lispy-backtick
lispy-open-curly
lispy-to-cond
lispy-goto-symbol
lispy-visit
lispy-open-square
lispy-at
lispy-backward-delete
lispy-x-more-verbosity
lispy-kill-sentence
lispy-insert-outline-below
lispy-bind-variable
lispy-buffer-kill-ring-save
lispy-outline-demote
lispy-meta-doublequote
lispy-outline-left
lispy-other-mode
lispy-to-lambda
lispy-new-copy
lispy-wrap-braces
lispy-slurp
lispy-let-flatten
lispy-goto
lispy-split
lispy-space
lispy-eval-expression
lispy-describe
lispy-goto-verb
lispy-view
lispy-kill
lispy-undo
lispy-delete-or-splice-or-slurp
lispy-yank
lispy-ace-paren
lispy-barf-to-point
lispy-convolute
lispy-repeat
lispy-slurp-or-barf-right
lispy-up-slurp
lispy-eval-and-insert
lispy-kill-at-point
lispy-braces
lispy-ert
lispy-move-down
lispy-backward-kill-word
lispy-paste
lispy-move-left
lispy-eval-other-window
lispy-knight-down
lispy-eval-and-comment
lispy-goto-def-ace
lispy-parens-barf-to-point-or-jump-nostring
lispy-extract-defun
lispy-insert-outline-left
lispy-mark-symbol
lispy-describe-bindings-C-4
lispy-unbind-variable
lispy-raise-some
lispy-splice
lispy--ediff-regions
lispy-beginning-of-defun


(breeze--insert-describe-keymap lispy-mode-map)
;;
;; Key             Binding
;;
;; C-a		lispy-move-beginning-of-line
;; C-d		lispy-delete
;; C-e		lispy-move-end-of-line
;; C-j		lispy-newline-and-indent
;; C-k		lispy-kill
;; RET		lispy-newline-and-indent-plain
;; C-y		lispy-yank
;; SPC		lispy-space
;; "		lispy-quotes
;; #		lispy-hash
;; '		lispy-tick
;; (		lispy-parens
;; )		lispy-right-nostring
;; +		special-lispy-join
;; -		special-lispy-ace-subword
;; .		special-lispy-repeat
;; /		special-lispy-splice
;; 0 .. 9		special-digit-argument
;; :		lispy-colon
;; ;		lispy-comment
;; <		special-lispy-barf
;; >		special-lispy-slurp
;; @		lispy-at
;; A		special-lispy-beginning-of-defun
;; B		special-lispy-ediff-regions
;; C		special-lispy-convolute
;; D		special-pop-tag-mark
;; E		special-lispy-eval-and-insert
;; F		special-lispy-follow
;; G		special-lispy-goto
;; H		special-lispy-ace-symbol-replace
;; I		special-lispy-shifttab
;; J		special-lispy-outline-next
;; K		special-lispy-outline-prev
;; L		special-lispy-outline-goto-child
;; M		special-lispy-alt-multiline
;; N		special-lispy-narrow
;; O		special-lispy-oneline
;; P		special-lispy-paste
;; Q		special-lispy-ace-char
;; R		special-lispy-raise-some
;; S		special-lispy-stringify
;; V		special-lispy-visit
;; W		special-lispy-widen
;; X		special-lispy-convolute-left
;; Z		special-lispy-edebug-stop
;; [		lispy-backward
;; ]		lispy-forward
;; ^		lispy-hat
;; _		special-lispy-underscore
;; `		lispy-backtick
;; a		special-lispy-ace-symbol
;; b		special-lispy-back
;; c		special-lispy-clone
;; d		special-lispy-different
;; e		special-lispy-eval
;; f		special-lispy-flow
;; g		special-lispy-goto-local
;; h		special-lispy-left
;; i		special-lispy-tab
;; j		special-lispy-down
;; k		special-lispy-up
;; l		special-lispy-right
;; m		special-lispy-mark-list
;; n		special-lispy-new-copy
;; o		special-lispy-other-mode
;; p		special-lispy-eval-other-window
;; q		special-lispy-ace-paren
;; r		special-lispy-raise
;; s		special-lispy-move-down
;; t		special-lispy-teleport
;; u		special-lispy-undo
;; v		special-lispy-view
;; w		special-lispy-move-up
;; x		special-lispy-x
;; y		special-lispy-occur
;; z		special-lh-knight/body
;; {		lispy-braces
;; }		lispy-brackets
;; ~		special-lispy-tilde
;; DEL		lispy-delete-backward
;; C-,		lispy-kill-at-point
;; C-1		lispy-describe-inline
;; C-2		lispy-arglist-inline
;; C-3		lispy-right
;; C-4		lispy-x
;; C-7		lispy-cursor-down
;; C-8		lispy-parens-down
;; C-9		lispy-out-forward-newline
;; C-<return>	lispy-open-line
;; M-<left>	lispy-outline-demote
;; M-<return>	lispy-meta-return
;; M-<right>	lispy-outline-promote
;; <backtab>	lispy-shifttab
;;
;; M-RET		lispy-meta-return
;; M-,		pop-tag-mark
;; M-.		lispy-goto-symbol
;; M-J		lispy-join
;; M-d		lispy-kill-word
;; M-i		lispy-iedit
;; M-j		lispy-split
;; M-k		lispy-kill-sentence
;; M-m		lispy-mark-symbol
;; M-o		lispy-left-maybe
;; M-q		lispy-fill
;; M-DEL		lispy-backward-kill-word
;; C-M-,		lispy-mark
;;
;; [back]
;;



;;; Smartparens

(use-package smartparens)

(breeze--insert-interactive-commands "smartparens-")

smartparens-mode
smartparens-global-mode
smartparens-global-strict-mode
smartparens-strict-mode

(breeze--insert-interactive-commands "sp-")

sp-convolute-sexp
sp-splice-sexp
sp-splice-sexp-killing-backward
sp-prefix-symbol-object
sp-use-paredit-bindings
sp-newline
sp-use-smartparens-bindings
sp-end-of-previous-sexp
sp-backward-whitespace
sp-select-next-thing
sp-kill-region
sp-delete-char
sp-absorb-sexp
sp-forward-sexp
sp-slurp-hybrid-sexp
sp-beginning-of-next-sexp
sp-select-next-thing-exchange
sp-beginning-of-previous-sexp
sp-delete-region
sp-backward-sexp
sp-backward-delete-symbol
sp-kill-whole-line
sp-up-sexp
sp-emit-sexp
sp-prefix-save-excursion
sp-end-of-next-sexp
sp-kill-word
sp-copy-sexp
sp-raise-sexp
sp-previous-sexp
sp-cheat-sheet
sp-join-sexp
sp-backward-barf-sexp
sp-transpose-hybrid-sexp
sp-forward-slurp-sexp
sp-clone-sexp
sp-select-previous-thing
sp-backward-delete-word
sp-backward-copy-sexp
sp-remove-active-pair-overlay
sp-kill-sexp
sp-indent-adjust-sexp
sp-splice-sexp-killing-forward
sp--kill-or-copy-region
sp-indent-defun
sp-wrap-cancel
sp-change-inner
sp-prefix-pair-object
sp-backward-slurp-sexp
sp-backward-kill-word
sp-prefix-tag-object
sp-delete-word
sp-comment
sp-backward-unwrap-sexp
sp-backward-delete-char
sp-show-enclosing-pair
sp-add-to-previous-sexp
sp-rewrap-sexp
sp-narrow-to-sexp
sp-backward-up-sexp
sp-extract-before-sexp
sp-select-previous-thing-exchange
sp-backward-parallel-sexp
sp-skip-forward-to-symbol
sp-backward-symbol
sp-add-to-next-sexp
sp-skip-backward-to-symbol
sp-forward-barf-sexp
sp-dedent-adjust-sexp
sp-delete-symbol
sp-describe-system
sp-splice-sexp-killing-around
sp-forward-whitespace
sp-down-sexp
sp-backward-kill-symbol
sp-push-hybrid-sexp
sp-extract-after-sexp
sp-transpose-sexp
sp-end-of-sexp
sp-unwrap-sexp
sp-backward-down-sexp
sp-kill-hybrid-sexp
sp-beginning-of-sexp
sp-forward-symbol
sp-forward-parallel-sexp
sp-kill-symbol
sp-next-sexp
sp-highlight-current-sexp
sp-split-sexp
sp-backward-kill-sexp
sp-swap-enclosing-sexp
sp-mark-sexp

(breeze--insert-describe-keymap smartparens-mode-map)
;; ok... smartparens is being a smartass about it's keymap, it is
;; defined dynamically (because it supports many languages, and
;; multiple "styles"), see:
;;
;; - sp-lisp-modes
;; - sp--populate-keymap
;; - sp-use-paredit-bindings
;; - sp-use-smartparens-bindings
;; - sp--set-base-key-bindings


;; See https://ebzzry.com/en/emacs-pairs/
;; and https://github.com/Fuco1/smartparens/wiki
