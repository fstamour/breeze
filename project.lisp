
(in-package #:cl-user)

(ql:quickload '(quickproject))

(setf quickproject:*author* "Francis St-Amour"
      quickproject:*license* "GPLv3")

(defun choose-local-project-directory ()
  (if (= 1 (length ql:*local-project-directories*))
      (first ql:*local-project-directories*)
      (swank:eval-in-emacs
       `(pick-localproject-directories
         ',(mapcar #'namestring ql:*local-project-directories*)))))

(defun read-name ()
  (format t "~&Project name?~&> ")
  (read-line))

(quickproject:make-project
 (merge-pathnames "sees"
                  (first ql:*local-project-directories*))
 :depends-on '(alexandria esrap))



(defun list-local-project-directories ()
  (mapcar #'namestring ql:*local-project-directories*))

(swank::defslimefun list-localproject-directories ())

cl-user:l
