
(defmacro breeze/demo/with-read-string (values &rest body)
  "Macro to help mock the function read-string"
  (let ((values-var (gensym)))
    `(let ((,values-var ,values))
       (cl-letf (((symbol-function 'read-string)
		              (lambda (prompt &optional initial-input default-value)
		                (pop ,values-var))))
	       ,@body))))

(defun breeze/demo/find-sldb-buffer ()
  (remove-if-not
   #'(lambda (buffer-name)
       (string-match-p "^\\*sldb" buffer-name))
   (mapcar #'buffer-name
	   (buffer-list))))

;; (mapcar #'kill-buffer (breeze/demo/find-sldb-buffer))

(use-package htmlize)

(with-current-buffer (get-buffer-create "demo.lisp")
  (erase-buffer)
  (lisp-mode)
  (slime-mode)
  (breeze-mode)
  ;; Use breeze's command to insert a package definition
  (breeze/demo/with-read-string
   '("demo" "")
   (breeze-insert-defpackage))
  ;; Use breeze's command to insert a function
  (breeze/demo/with-read-string
   '("foo" "")
   (breeze-insert-defun))
  ;; Write the function's body
  (insert "42")
  ;; Go "out" of the function's body
  (end-of-buffer)
  (insert "\n\n")

  ;; Evaluate the whole buffer
  (slime-eval-buffer)

  ;; Try to call "foo", but make a typo
  (insert "(fop)")
  ;; Take """screenshot""" of slime's debugger buffer.
  (run-at-time "0.5 sec" nil
	             ;; TODO Extract this in another function
	             #'(lambda ()
		               (with-temp-buffer
		                 ;; Copy SLDB's buffer into the temp buffer
		                 (insert
		                  (with-current-buffer
			                    (get-buffer
			                     ;; Open the first SLDB buffer
			                     (car (breeze/demo/find-sldb-buffer)))
			                  (prog1
			                      (buffer-string)
			                    ;; Abort the evaluation
			                    (sldb-invoke-restart-by-name "ABORT"))))
		                 ;; Export the temp buffer into a new "html" buffer
		                 (let ((html-buffer (htmlize-buffer)))
		                   (with-current-buffer html-buffer
			                   ;; Write the html result
			                   (write-file (breeze/demo/next-to-this-file "demo.html"))
			                   ;; Kill the html buffer
			                   (kill-buffer))))))
  (slime-eval-last-expression)
  (write-file (breeze/demo/next-to-this-file "demo.lisp"))
  ;; (buffer-substring-no-properties (point-min) (point-max))
  )


;; NEXT STEP: take the html buffer and extract the CSS and the PRE
;; element so we can embed the result into another webpage, like
;; breeze's documentation
