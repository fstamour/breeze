;; -*- lexical-binding: t -*-

(require 'ert)


;;; Testing the test helpers 🤓

(ert-deftest test/breeze--xor ()
  (should (equal '(nil t t nil)
                 (mapcar (lambda (args)
                           (apply 'breeze--xor args))
                         '((nil nil)
                           (nil t)
                           (t nil)
                           (t t))))))



(ert-deftest test/breeze-%symbolicate ()
  (should (eq 'sly (breeze-%symbolicate2 "sly")))
  (should (eq 'sly (breeze-%symbolicate2 'sly)))
  (should (eq 'slime (breeze-%symbolicate2 "slime")))
  (should (eq 'slime (breeze-%symbolicate2 'slime)))
  (should (eq 'sly-eval (breeze-%symbolicate2 'sly "eval")))
  (should (eq 'slime-eval (breeze-%symbolicate2 'slime "eval")))
  (should (eq 'slime-connected-hook
              (breeze-%symbolicate2 'slime "connected-hook"))))

(ert-deftest test/breeze-relative-path ()
  (should (file-exists-p (breeze-relative-path)))
  (should (file-exists-p (breeze-relative-path "src/")))
  (should (file-exists-p (breeze-relative-path "emacs/breeze.el")))
  (should (file-exists-p (breeze-relative-path "src/ensure-breeze.lisp"))))



(ert-deftest test/breeze-fbound-p ()
  (should (eq 'car (breeze-fbound-p 'car)))
  (should-not (breeze-fbound-p 'this-symbol-does-not-exist-at-all)))

(ert-deftest test/breeze-keep-fbound ()
  (should (equal '(car cdr) (breeze-keep-fbound '(car cdr))))
  (should-not (breeze-keep-fbound '(nonexistent-xyz)))
  (should (equal '(car cdr) (breeze-keep-fbound '(car nonexistent-xyz cdr)))))

(ert-deftest test/breeze-remove-nil ()
  (should (equal '(1 2 3) (breeze-remove-nil 1 nil 2 nil 3)))
  (should-not (breeze-remove-nil nil nil))
  (should (equal '(a b) (breeze-remove-nil 'a 'b))))

(defvar breeze--test-var nil
  "Dynamic variable for testing `breeze-symbol-value'.")

(ert-deftest test/breeze-symbol-value ()
  (should-not (breeze-symbol-value 'breeze--test-var))
  (let ((breeze--test-var 42))
    (should (= 42 (breeze-symbol-value 'breeze--test-var))))
  (should-not (breeze-symbol-value 'breeze-unbound-symbol-xyz)))

(ert-deftest test/breeze-funcall ()
  (should (= 3 (breeze-funcall '+ 1 2)))
  (should-not (breeze-funcall 'nonexistent-function-xyz)))

(defvar breeze--test-hook nil
  "Dynamic variable for testing `breeze-add-hook' and `breeze-remove-hook'.")

(ert-deftest test/breeze-add-hook ()
  (let ((breeze--test-hook nil))
    (breeze-add-hook 'breeze--test-hook #'identity)
    (should (memq #'identity breeze--test-hook)))
  ;; When hook is not bound, it should not error
  (breeze-add-hook 'breeze-unbound-hook-xyz #'identity))

(ert-deftest test/breeze-remove-hook ()
  (let ((breeze--test-hook (list #'identity)))
    (breeze-remove-hook 'breeze--test-hook #'identity)
    (should-not breeze--test-hook))
  ;; When hook is not bound, it should not error
  (breeze-remove-hook 'breeze-unbound-hook-xyz #'identity))

(ert-deftest test/breeze-single ()
  (should-error (breeze-single 'a))
  (should (eq 'a (breeze-single '(a))))
  (should-not (breeze-single '(a b)))
  (should-not (breeze-single nil))
  (should-not (breeze-single '())))

(ert-deftest test/breeze-history-symbol ()
  (should (eq 'breeze-foo (breeze-history-symbol "breeze-foo")))
  (should-not (breeze-history-symbol nil)))

(ert-deftest test/breeze-with-face ()
  (let ((result (breeze-with-face "hello" :foreground "red")))
    (should (string= "hello" result))
    (should (get-text-property 0 'face result))))


;;; Logging

(ert-deftest test/breeze-debug ()
  ;; make sure the buffer doesn't exist
  (when-let ((buf (get-buffer " *breeze-debug*")))
    (kill-buffer buf))
  ;; log a message
  (breeze-debug "test message %d" 42)
  (let ((buf (get-buffer " *breeze-debug*")))
    ;; check that the buffer was created
    (should buf)
    ;; and that it contains the message
    (with-current-buffer buf
      (should (string-match-p
               "test message 42"
               (buffer-substring-no-properties (point-min) (point-max))))))
  ;; Test truncation of long messages
  (breeze-debug "%s" (make-string 2000 ?x))
  (with-current-buffer (get-buffer " *breeze-debug*")
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p (make-string 2000 ?x) content)))))


;;; Listener state (no connection)

(ert-deftest test/breeze-sly-connected-p ()
  (should-not (breeze-sly-connected-p)))

(ert-deftest test/breeze-slime-connected-p ()
  (should-not (breeze-slime-connected-p)))

(ert-deftest test/breeze-listener-connected-p ()
  (should-not (breeze-listener-connected-p nil))
  (should (equal '(error "Please start either slime, sly or inferior-lisp.")
                 (should-error (breeze-listener-connected-p)))))

(ert-deftest test/breeze-list-connected-listeners ()
  (should-not (breeze-list-connected-listeners)))

(ert-deftest test/breeze-header-line ()
  (should (string= "BRZ: not connected" (breeze-header-line))))


;;; Buffer-related functions

(ert-deftest test/breeze-compute-buffer-args ()
  (with-temp-buffer
    (insert "hello world 1")
    (goto-char 6)
    (cl-destructuring-bind (&key buffer-name buffer-file-name buffer-string
                                 point point-min point-max major-mode)
        ;; The "car" is because `read-from-string' returns
        ;; (OBJECT-READ . FINAL-STRING-INDEX) instead of just the
        ;; object...
        (car (read-from-string
              (concat "(" (breeze-compute-buffer-args) ")")))
      (should (string-match-p "^ \\*temp\\*" buffer-name))
      (should-not buffer-file-name)
      (should (string= "hello world 1" buffer-string))
      (should (= 5 point))
      (should (= 0 point-min))
      (should (= 13 point-max))
      (should (eq 'fundamental-mode major-mode)))
    (cl-destructuring-bind (&key buffer-string &allow-other-keys)
        (car (read-from-string
              (concat "("
                      (breeze-compute-buffer-args
                       :include-buffer-content-p nil)
                      ")")))
      (should-not buffer-string))))

(ert-deftest test/breeze-%loader ()
  (let* ((file-content (with-temp-buffer
                         (insert-file-contents
                          (breeze-relative-path "src/ensure-breeze.lisp"))
                         (buffer-string)))
         (prefix "(cl:multiple-value-bind (#1=#.(gensym \"result\") #2=#.(gensym \"condition\")) (cl:ignore-errors \n")
         (suffix "\n)\n(list #1# (when #2# (format nil \"~A\" #2#)))\n)"))
    (should (string= (breeze-%loader) (concat prefix file-content suffix)))))



;;; Command request processing

(ert-deftest test/breeze-command-process-request/insert-at ()
  (with-temp-buffer
    (insert "hello")
    ;; position is 0-indexed, insert-at does (goto-char (1+ position))
    (breeze-command-process-request nil '("insert-at" 3 " world 2 "))
    (should (string= "hel world 2 lo" (buffer-string)))))

(ert-deftest test/breeze-command-process-request/insert-at-saving-excursion ()
  (with-temp-buffer
    (insert "hello")
    (goto-char 3)
    (breeze-command-process-request nil '("insert-at-saving-excursion" 0 "X"))
    (should (string= "Xhello" (buffer-string)))
    (should (= 4 (point)))))

(ert-deftest test/breeze-command-process-request/insert ()
  (with-temp-buffer
    (insert "hello")
    (breeze-command-process-request nil '("insert" " world 3"))
    (should (string= "hello world 3" (buffer-string)))))

(ert-deftest test/breeze-command-process-request/insert-saving-excursion ()
  (with-temp-buffer
    (insert "hello")
    (goto-char 3)
    (breeze-command-process-request nil '("insert-saving-excursion" " world 4 "))
    (should (string= "he world 4 llo" (buffer-string)))
    (should (= 3 (point)))))

(ert-deftest test/breeze-command-process-request/replace ()
  (with-temp-buffer
    (insert "hello world")
    (breeze-command-process-request nil '("replace" 6 11 "WORLD 5"))
    (should (string= "hello WORLD 5" (buffer-string)))))

(ert-deftest test/breeze-command-process-request/goto-char ()
  (with-temp-buffer
    (insert "hello world")
    (breeze-command-process-request nil '("goto-char" 3))
    (should (= 3 (point)))))

(ert-deftest test/breeze-command-process-request/buffer-string ()
  (with-temp-buffer
    (insert "hello world")
    (should (string= "hello world"
                     (breeze-command-process-request nil '("buffer-string"))))))

(ert-deftest test/breeze-command-process-request/message ()
  (let ((msg (format "test-request-msg-%s" (random))))
    (breeze-command-process-request nil (list "message" msg))
    ;; Should appear in *Messages* (via breeze-message)
    (with-current-buffer "*Messages*"
      (should (string-match-p
               (regexp-quote msg)
               (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Should also appear in the debug buffer
    (with-current-buffer " *breeze-debug*"
      (should (string-match-p
               (regexp-quote msg)
               (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test/breeze-command-process-request/unknown ()
  ;; Should not error; the unknown request is logged via breeze-debug
  (let ((request-type (format "unknown-type-%s" (random))))
    (breeze-command-process-request nil (list request-type))
    (with-current-buffer " *breeze-debug*"
      (should (string-match-p
               (regexp-quote request-type)
               (buffer-substring-no-properties (point-min) (point-max)))))))


;; TODO (ert-deftest test/generate-bindings-documentation )

;; TODO (ert-deftest test/update-list-of-stubs)


;; Testing the "after-change-function"
(ert-deftest test/breeze-after-change-functions ()
  (let ((breeze-changes))
    (cl-letf (((symbol-function 'breeze-disabled-p) (lambda () nil)))
      (with-temp-buffer
        (rename-buffer "test1")
        (add-hook 'after-change-functions 'breeze-after-change-function nil t)
        (insert "abcd")
        (delete-char -1)
        (goto-char (point-min))
        (search-forward "bc")
        (replace-match "BC")
        (should (equal breeze-changes
                       '((:buffer-name "test1" :buffer-file-name nil :replace-at 1 :end 3 :text "BC")
                         (:buffer-name "test1" :buffer-file-name nil :delete-at 3 :length 1)
                         (:buffer-name "test1" :buffer-file-name nil :insert-at 0 :text "abcd"))))
        (should (equal "aBC" (buffer-substring-no-properties (point-min) (point-max))))))))
