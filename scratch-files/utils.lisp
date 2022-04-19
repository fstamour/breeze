
(defmacro chain (&body forms)
  (reduce (lambda (acc next)
	    (append acc (list next)))
	  (butlast forms)
	  :initial-value (alexandria:lastcar forms)
	  :from-end t))

(defmacro chain* (&body forms)
  (alexandria:with-gensyms (callback)
    `(lambda (,callback)
       (chain ,@forms ,callback))))

(defun reverse-parameter (fn)
  "Take a function of arity 2 and call return a function with the 2
 parameters inverted."
  #'(lambda (x y) (funcall fn y x)))
