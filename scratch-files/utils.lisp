
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
