;;;; See remote-loading.lisp

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(breeze-eval
 (concat "(progn"
         (get-string-from-file
          (breeze-relative-path "scratch-files/remote-loading.lisp"))
         "'loaded)"))
