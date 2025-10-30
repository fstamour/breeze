(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)


;;; Uninstall ultralisp
#+ (or)
(ql-dist:uninstall
 (find-if #'(lambda (dist)
	      (string= "ultralisp"
		       (ql-dist:name dist)))
	  (ql-dist:all-dists)))
