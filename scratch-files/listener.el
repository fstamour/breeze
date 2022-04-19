;; Trying to figure out how to communicate between slime and swank

(slime-eval `(swank:interactive-eval
	      "(print 'hi)"))
"=> HI"

(let ((string "(print 'hi)"))
  (slime-eval `(swank:interactive-eval
		,(format "(print %s)"
			 (prin1-to-string string)))))
"=> \"(print 'hi)\""


(let ((string "(print 'hi)"))
  (slime-eval `(swank:interactive-eval
		,(format "(print %s)"
			 (prin1-to-string (buffer-substring-no-properties
					   (point-min)
					   (point-max)))))))
"=> \";; Trying to figure out how to communicate between slime and swank

(slime-eval `(swank:interactive-eval
	      \\\"(print 'hi)\\\"))
\\\"=> HI\\\"

(let ((string \\\"(print 'hi)\\\"))
  (slime-eval `(swank:interactive-eval
		,(format \\\"(print %s)\\\"
			 (prin1-to-string string)))))
\\\"=> \\\\\\\"(print 'hi)\\\\\\\"\\\"


(let ((string \\\"(print 'hi)\\\"))
  (slime-eval `(swank:interactive-eval
		,(format \\\"(print %s)\\\"
			 (prin1-to-string (buffer-substring-no-properties
					   (point-min)
					   (point-max)))))))
\""




(slime-eval `(swank:eval-and-grab-output
	      ,(format "(read-from-string %s)"
		       (prin1-to-string (buffer-substring-no-properties
					 (point-min)
					 (point-max))))))
("" "(SLIME-EVAL `(SWANK:INTERACTIVE-EVAL \"(print 'hi)\"))
128")
;; It read only the first expression, and also returned the number of
;; characters read. Perfect.


(breeze-eval
 (format "(read-from-string %s)"
	 (prin1-to-string (buffer-substring-no-properties
			   (point-min)
			   (point-max)))))
("" "(SLIME-EVAL `(SWANK:INTERACTIVE-EVAL \"(print 'hi)\"))
128")
;; Same results, good.




;;; Other stuff to look at

;; (slime-goto-package-source-definition "breeze")
;; (slime-goto-xref)

;; (slime-rex (var ...) (sexp &optional package thread) clauses ...)

;; (slime-interactive-eval "(breeze.swank:)")

;; (global-set-key
;;  (kbd "<f5>")
;;  (lambda ()
;;    (interactive)
;;    (slime-interactive-eval
;;     (concat "(breeze.swank::insert-let "
;; 	    (replace-match "\\\""  "fixedcase" "literal")
;; 	    (slime-defun-at-point)
;; 	    "4"
;; 	    ")"))))
