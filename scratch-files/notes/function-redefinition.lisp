
(example "Change implementation"
	 :before
	 ((in-package 'examples)
	  (defun 2x (x)
	    (+ x x)))
	 (:after
	  ((in-package 'examples)
	   (defun 2x (x)
		(* 2 x)))))

(example "Add documentation"
	 :before
	 ((in-package 'examples)
	  (defun 2x (x)
	       (+ x x)))
	 (:after
	  ((in-package 'examples)
	   (defun 2x (x)
		"Doubles x"
		(+ x x)))))

(example "Change documentation"
	 :before
	 ((in-package 'examples)
	  (defun 2x (x)
	       "Doubles x"
	       (+ x x)))
	 (:after
	  ((in-package 'examples)
	   (defun 2x (x)
		"Adds x to itself"
		(+ x x)))))

(example "Change implementation and documentation"
	 :before
	 ((in-package 'examples)
	  (defun 2x (x)
	       "Doubles x"
	       (+ x x)))
	 (:after
	  ((in-package 'examples)
	   (defun 2x (x)
		"Multiply x by 2"
		(* 2 x)))))

(example "Add a function"
	 :before
	 ((in-package 'examples)
	  (defun 2x (x)
	       "Doubles x"
	       (+ x x)))
	 (:after
	  ((in-package 'examples)
	   (defun 2x (x)
		"Doubles x"
		(+ x x))
	   (defun 3x (x)
	     "Multiply x by 3"
	     (* 2 x)))))
