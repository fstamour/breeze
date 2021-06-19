
(in-package #:breeze.user)

(defun check-for-untested-functions ()
  (let ((missing-tests
	 (loop
	    :for package :in (current-packages)
	    :append (breeze.xref::function-without-test package))))
    (if missing-tests
	(progn
	  (princ "There are untested functions in current packages:")
	  (format t "~&~{ * ~A~%~}"
		  missing-tests))
	(format t "~&No untested function found. ~A" (cheers)))))

(loop
   :for package :in (current-packages)
   :append (breeze.xref::function-without-test package))
