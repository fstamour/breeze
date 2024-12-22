
(cl:in-package #:cl-user)

(require '#:asdf)

(defvar *breeze-home*
  (asdf:system-source-directory "breeze")
  ;; (uiop:pathname-parent-directory-pathname *load-truename*)
  )

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(asdf:system-source-directory "breeze"))
   :inherit-configuration))

;; not on quicklisp as of 2024-08-22
(ql:quickload 'acute-terminal-control)




;; (setf SWANK:*LOG-EVENTS* t)

;; swank-repl::*saved-global-streams*
;; (setf swank:*globally-redirect-io* nil)

(format *debug-io* "fasd")
(format *terminal-io* "hi!~%")

(setf (swank::connection.env swank::*emacs-connection*)
      (remove '*terminal-io*
              (swank::connection.env swank::*emacs-connection*) :key #'car))


;; works only if the buffering is disabled
(acute-terminal-control:dimensions)

(acute-terminal-control:disable-system-echoing)
(acute-terminal-control:enable-system-echoing)


(acute-terminal-control:disable-system-buffering)
(acute-terminal-control:read-event)

(defun d ()
  (acute-terminal-control:disable-system-buffering)
  (setf (acute-terminal-control:cursor) (cons 0 0))
  (acute-terminal-control:erase)
  (format t "> ")
  (force-output)
  (unwind-protect
       (loop :repeat 3
             :do (progn
                   (format t "\\~c" (acute-terminal-control:read-event))
                   (force-output)))
    (acute-terminal-control:enable-system-echoing)))

;; in tmux :set -g mouse on

(defun read* ()
  (terpri)
  (loop
    for e = (acute-terminal-control:read-event)
    repeat 3
    until (eq e #\Return)
    ;; do (print e)
    collect e))

(defun repl ()
  (loop))
