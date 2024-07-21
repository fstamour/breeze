;;; Trying out emacs' "compile" command
;;;


;; Trying out with parachute's output


(cl-defun breeze--convert-compilation-error-to-buffer-or-file ()
  (message "breeze--convert-compilation-error-to-buffer-or-file: %S"
           (list (current-buffer)
                 (point)
                 (buffer-substring-no-properties
                  (match-beginning 1)
                  (match-end 1))))
  (list "/home/fstamour/quicklisp/local-projects/breeze/")
  ;; (list ".../breeze/src/lossless-reader.lisp")
  )

(cl-defun breeze--convert-compilation-error-to-buffer-or-file ()
  ;; (message "breeze--convert-compilation-error-to-buffer-or-file: %S" (list (current-buffer) (point)))
  (let ((symbol (buffer-substring-no-properties
                 (match-beginning 1)
                 (match-end 1))))
    ;; TODO handle multiple definitions
    (save-match-data
      (let* ((definition (first (slime-find-definitions symbol)))
             (location-alist (cdadr definition))
             (file (car (alist-get :file location-alist))))
        (message "breeze--: %S %S" symbol file)
        (unless file
          (let* ((parts (split-string symbol "::"))
                 (needle (downcase (or (cadr parts) (car parts)))))
            (deadgrep needle nil)))
        file))))

(push '("tests failed in \\(.*\\)$" breeze--convert-compilation-error-to-buffer-or-file)
      compilation-error-regexp-alist)

(trace-function-background 'breeze--convert-compilation-error-to-buffer-or-file)
(trace-function-background 'compilation-error-properties)
(trace-function-background 'compilation-internal-error-properties)

(save-excursion
  (with-current-buffer (get-buffer-create "*scratch-compile-mode-parachute*")
    (erase-buffer)
    (insert "
   0/   0 tests failed in BREEZE.TEST.LOSSLESS-READER::READ-PUNCTUATION
The test failed to evaluate properly:
  The function BREEZE.TEST.LOSSLESS-READER::REGISTER-TEST-STRING is undefined.
   0/   0 tests failed in BREEZE.TEST.LOSSLESS-READER::UNPARSE
The test failed to evaluate properly:
  The function BREEZE.TEST.LOSSLESS-READER::REGISTER-TEST-STRING is undefined.
")
    (compilation-parse-errors (point-min) (point-max))))

(cl-loop for symbol in '("BREEZE.TEST.LOSSLESS-READER::REGISTER-TEST-STRING"
                         "REGISTER-TEST-STRING")
         for splitted = (split-string symbol "::")
         collect (or (cadr splitted) (car splitted)))

(split-string
 "REGISTER-TEST-STRING"
 "::")


;; It's not straightforward to figure out _where_ a missing function is missing xD
(push '("The function \\(.*\\) is undefined." breeze--convert-compilation-error-to-buffer-or-file) compilation-error-regexp-alist)

(pop compilation-error-regexp-alist)
