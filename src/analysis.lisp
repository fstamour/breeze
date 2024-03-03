
(uiop:define-package #:breeze.analysis
    (:documentation "Linter, formatter, and maybe more.")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader)
  (:export #:lint))

(in-package #:breeze.analysis)

;;; Trying to figure out how to run the "formatting rules" without
;;; applying them...

(defun %walk (state callback tree depth)
  (when tree
    (flet ((cb (node &rest args)
             (apply callback node :depth depth args)))
      (etypecase tree
        (list
         (loop
           :for i :from 0
           :for previous = nil :then (first rest)
           :for rest :on tree
           :for node = (car rest)
           :collect (%walk state
                           callback
                           (cb node
                               :aroundp t
                               :nth i
                               :firstp (eq tree rest)
                               :lastp (null (cdr rest))
                               :previous previous)
                           (1+ depth))))
        (node
         (case (node-type tree)
           (parens
            (cb tree :beforep t)
            (%walk state
                   callback
                   (node-children tree)
                   (1+ depth))
            (cb tree :afterp t))
           (t
            (cb tree))))))))

(defun walk (state callback)
  (%walk state callback (tree state) 0))

;; This is equivalent to unparse with the leading and trailing
;; whitespace fixes. It is _much_ more succint!
#++
(let ((state (parse " (+ 2) ")))
  (with-output-to-string (out)
    (walk state (lambda (node &rest args &key depth aroundp beforep afterp
                                           firstp lastp nth)
                  ;; Debug info
                  (format t "~&~s ~{~s~^ ~}" node args)
                  ;; Printing stuff
                  (cond
                    (beforep
                     (write-char #\( out))
                    (afterp
                     (write-char #\) out))
                    ((not (or aroundp beforep afterp))
                     (write-node node state out)))
                  ;; Removing useless whitespaces
                  (unless (and (plusp depth)
                               aroundp
                               (whitespace-node-p node)
                               (or firstp lastp))
                    node)))))

(defun lint (&key buffer-string point-max &allow-other-keys)
  (let ((state (parse buffer-string))
        (diagnostics '()))
    (walk state
          (lambda (node &rest args &key depth aroundp beforep afterp
                                     firstp lastp nth
                                     previous)
            (declare (ignorable beforep afterp nth args))
            ;; Debug info
            ;; (format t "~&~s ~{~s~^ ~}" node args)
            ;; Removing useless whitespaces
            (unless (valid-node-p node)
              (push (list (node-start node)
                          point-max
                          :error
                          "Syntax error")
                    diagnostics))
            (when (and (plusp depth)
                       aroundp
                       (whitespace-node-p node))
              ;; (break)
              (cond
                (firstp
                 (push (list (node-start node)
                             (node-end node)
                             :warning
                             "Extraneous leading whitespaces.")
                       diagnostics))
                ((and lastp (not (line-comment-node-p previous)))
                 (push (list (node-start node)
                             (node-end node)
                             :warning
                             "Extraneous trailing whitespaces.")
                       diagnostics))))
            node))
    diagnostics))
