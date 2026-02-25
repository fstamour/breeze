;;;; -*- lexical-binding: t; -*-
;;;; Goal: try to automatically diagnose breeze configuration issues
;;;;
;;;; Motivations: I currently have an issue where breeze-minor-mode is
;;;; seemingly not enabled, but I'm pretty sure it should be. Building
;;;; up some debug script will be useful for debugging other users'
;;;; issues (if they arise) and could be used in the tests as
;;;; assertions or for debugging the tests.

(require 'cl)

;; List all symbols that starts with "breeze-"
(let ((x))
  (mapatoms (lambda (s)
              (when (string-prefix-p "breeze-" (symbol-name s))
                (push s x)))))

;; TODO check for suffix "breeze" (which would be wrong!)
(cl-loop for path in load-path
         when (or (string-suffix-p "breeze/emacs" path)
                  (or (string-suffix-p "/slime" path)
                      (string-suffix-p "/slime/contrib" path))
                  (or (string-suffix-p "/sly" path)
                      (string-suffix-p "/sly/contrib" path))
                  (string-suffix-p "lib/hyperspec" path))
         collect path)
;; look for suffix "slime" and "slime/contrib

(memq 'breeze features)
;; => (breeze ...)
;; "breeze is loaded"

(memq 'slime features)
;; => (slime ...)

(memq 'sly features)
;; => nil


(fboundp 'slime)
;; => t

(fboundp 'sly)
;; => nil

(breeze-listener-connected-p nil)
;; => nil
;; not connected


lisp-mode-hook
;; => (breeze-minor-mode slime-lisp-mode-hook)
;;
;; maybe `breeze-minor-mode' should be _after_ `slime-lisp-mode-hook'?

slime-connected-hook
;; => (breeze-connected-hook-function)

sly-connected-hook
;; not bound (because sly is not loaded)

breeze-minor-mode-hook
;; => (breeze-enable-flymake-backend
;; flymake-mode
;; breeze-setup-after-change-function)

flymake-diagnostic-functions
;; => nil
;;
;; when evaluated in a buffer where breeze-minor-mode is disabled

;; Looking at flymake's internals...
flymake--state
;; => #s(hash-table data (breeze-flymake #s(flymake--state nil t (error "Please start either slime, sly or inferior-lisp.") nil #s(hash-table))))
;;; ^^^ TODO this means that `breeze-listener-connected-p' was called
;;; without `errorp' set to nil

(with-current-buffer (get-buffer "worker.lisp")
  (let ((debug-on-error t))
    ;; Fake C-u M-x flymake-start (which resets the diagnostic functions)
    (flymake-start nil '(4))))

;; I can start thinking about invariants:
;;
;; 1. if `lisp-mode-hook' contains `breeze-minor-mode', then all
;; "lisp-mode" buffers should have breeze-minor-mode enabled.
;;
;; 2. if `breeze-minor-mode-hook' contains
;; `breeze-enable-flymake-backend', it should also contain
;; `flymake-mode', and the "breeze-minor-mode" buffers should have
;; flymake-mode enabled _and_ should have `breeze-flymake' in their
;; `flymake-diagnostic-functions'
;;
;; 3. if `breeze-minor-mode-hook' contains
;; `breeze-setup-after-change-function', the "breeze-minor-mode"
;; buffers should have `breeze-after-change-function' in their
;; `after-change-functions'
;;
;; 4. if `breeze-capfs-mode' is enabled, `breeze-completion-at-point'
;; should be in `completion-at-point-functions'
;;
;; 5. `header-line-format'




;;; TODO Other checks:
;;; 1. content of variable `breeze-disabled-p'
;;; 2. buffer " *breeze-debug*"
;;; 3. (breeze-disabled-p)
;;; 4. (breeze-list-loaded-listeners)
;;; 5. (breeze-validate-if-breeze-package-exists)
;;; 6. (breeze-eval "(breeze.command:list-all-commands-for-editor)") vs (breeze-list-commands)
;;; 7. content of variable `breeze-breeze.el'
;;; 8. files relatives to `breeze-breeze.el' (using `breeze-relative-path')
;;; 9. content of variable `inferior-lisp-program'
;;; 10. content of variables `slime-lisp-implementations' & `sly-lisp-implementations'
;;; 11. content of variables `slime-default-lisp' & `sly-default-lisp'
;;; 12. content of variable `load-path'
;;;
;;; On common lips side:
;;; 1. does the package `asdf' exists
;;; 2. does the package `ql' exists
;;; 3. which implementation is it




;;; Listing buffers by major-mode

(defun breeze-list-buffers-by-local-value (variable local-value)
  (cl-loop for buf in (buffer-list)
           when (eq local-value (buffer-local-value variable buf))
           collect buf))

(cl-defun breeze-lisp-buffers ()
  "List all buffers with the `lisp-mode' major mode."
  (breeze-list-buffers-by-local-value 'major-mode 'lisp-mode))

(defun breeze-miner-mode-buffers ()
  "List all buffers with `breeze-minor-mode' enabled."
  (breeze-list-buffers-by-local-value 'breeze-minor-mode t))

(equal (mapcar 'buffer-name (breeze-lisp-buffers))
       (mapcar 'buffer-name (breeze-list-buffers-by-local-value 'breeze-minor-mode t)))

;; All the buffers with `lisp-mode' but not `breeze-minor-mode'
(cl-loop for buf in (buffer-list)
         when (and (eq 'lisp-mode (buffer-local-value 'major-mode buf))
                   (not (buffer-local-value 'breeze-minor-mode buf)))
         collect buf)
;; => (#<buffer recfile.lisp>)





;; loop breeze buffers
;; check flymake--state 'breeze-flymake
;; if error, run flyemake start with debugger




(defun breeze-check-lisp-mode-hook ()
  "Ensure that `lisp-mode-hook' contains the right command to enable
`breeze-minor-mode'."
  (when (memq #'breeze-minor-mode lisp-mode-hook)
    (warn "`lisp-mode-hook' contains the function `breeze-minor-mode', but it should use `enable-breeze-minor-mode' instead.")
    (remove-hook 'lisp-mode-hook #'breeze-minor-mode))
  (add-hook 'lisp-mode-hook #'enable-breeze-minor-mode))

(add-hook 'lisp-mode-hook #'breeze-minor-mode)
