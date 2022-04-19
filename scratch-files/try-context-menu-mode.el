;; https://ruzkuku.com/texts/emacs-mouse.html

(when (fboundp 'context-menu-mode))

(defun breeze-context-menu (menu mouse-click-event)
  ;; (message "%S - %S" menu major-mode)
  (when (eq major-mode 'lisp-mode)
    (save-excursion
      (mouse-set-point mouse-click-event)
      (when
          ;; Add a separator at the end
          (define-key-after menu
            ;; Name of the separator
            [breeze-context-menu-separator]
            ;; Definition of the binding
            menu-bar-separator
            ;; After is nil, so the new binding goes at the end of the keymap
            ))
      (define-key-after menu [breeze-menu-test]
        '(menu-item "Breeze quickfix"
                    breeze-quickfix
                    :help "Call breeze quickfix at point"))))
  menu)

define-key-after

(add-hook 'context-menu-functions #'breeze-context-menu)


;; (context-menu-undo context-menu-region context-menu-middle-separator context-menu-local context-menu-minor)
