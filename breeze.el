
(define-minor-mode breeze-mode
  "Minor mode to work with the project \"assembly\"."
  :keymap (make-keymap))

(defun breeze-run-tests ()
  (interactive)
  (slime-repl-eval-string "(breeze.user:run-all-tests)"))

; (concat "(breeze.user:run-all-tests (find-package '"
;        (slime-current-package)
;        "))")

(define-key breeze-mode-map (kbd "<f5>")
  'breeze-run-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trying to get the values of ql:*local-project-directories*
;; to ask to choose in emacs' minibuffer

;; What are repl-variables?

;; This makes emacs hangs or slime crash
;; because slime doesn't recognize the "#" character in the response.
;; (slime-eval 'ql:*local-project-directories*)


;; Expect a string back..?
;; (swank-repl:listener-eval ql:*local-project-directories*)

(let ((local-project-directories))
  (slime-rex
      ()
      ('(cl:prin1-to-string (cl:mapcar
                             (cl:function cl:namestring)
                             ql:*local-project-directories*)))
    ((:ok result)
     (slime-repl-insert-result (list result))
     (setf local-project-directories result)))
  local-project-directories)
