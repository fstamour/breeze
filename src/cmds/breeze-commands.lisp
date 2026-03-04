(cl:in-package #:cl-user)

(in-package #:breeze.refactor)

;; TODO move into src/cmds/breeze-commands.lisp
;; TODO add commands to insert `defcheck`s
;; Dogfood'ing to the max!
(define-command insert-breeze-define-command ()
  "Insert a breeze:define-command form."
  (declare (context :top-level))
  (let ((name (read-string "Name of the command (symbol): ")))
    (insert
     "(define-command ~a ()~
    ~%  \"~@(~a~).\"~
    ~%  )"
     name
     (substitute #\Space #\- name))))
