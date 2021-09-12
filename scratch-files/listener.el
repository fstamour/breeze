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
