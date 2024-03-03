
(uiop:define-package #:breeze.analysis
    (:documentation "Linter, formatter, and maybe more.")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader #:breeze.pattern)
  ;; Tree/Form predicate
  (:export
   #:in-package-node-p
   #:find-node
   #:find-path-to-position)
  (:export #:lint))

(in-package #:breeze.analysis)


;;; Integrating pattern.lisp and lossless-parser.lisp

;; (defpattern in-package package-designator)

#++
(defmethod match ((pattern string) (input string))
  (string= pattern input))


;;; Basic tree inspection

(defun node-length (node)
  "Returns the number of children of NODE, or nil if it doesn't have any
children nodes."
  (let ((children (node-children node)))
    (when (listp children)
      (length children))))

;; TODO I want to check if a node is an "in-package" node...
;; - case converting if necessary
;; - skip whitespaces
;; - check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.



#++ (defun node-string= (string node))
#++ (defun node-string-equal (string node))

(defun plusp* (x)
  (and (numberp x)
       (plusp x)))

;; TODO use the stuff I made in "patterns.lisp"
(defun in-package-node-p (state node)
  "Is NODE a cl:in-package node?"
  (cond
    ((token-node-p node)
     (let ((string (node-content state node)))
       (if (plusp* (position #\: string))
           (destructuring-bind (package-name symbol-name)
               (remove-if #'alexandria:emptyp
                          (uiop:split-string string :separator '(#\:)))
             #++ ;; We don't depend on split-sequence...
             (split-sequence:split-sequence
              #\: string
              :remove-empty-subseqs t)
             (and (member package-name '("cl" "common-lisp" "cl-user" "common-lisp-user")
                          :test #'string-equal)
                  (string-equal "in-package" symbol-name)))
           (string-equal "in-package" string))))
    ((parens-node-p node)
     (in-package-node-p state (first (node-children node)))
     #++
     (and (in-package-node-p state (first (node-children node)))
          ;; TODO make sure there _is_ a third children to return!
          ;; N.B. the "second" children would be a whitespace
          (third (node-children node))))))

(defun find-node (position nodes)
  "Given a list of NODES, return which node contains the POSITION."
  (when (listp nodes)
    (loop :for node :in nodes
          :for start = (node-start node)
          :for end = (node-end node)
          :for i :from 0
          :when (and (<= start end) (< position end))
            :do (return (cons node i)))))

(defun find-path-to-position (state position)
  "Given a list of NODES, return a path (list of cons (node . index))"
  (loop :for found = (find-node position (tree state))
          :then (find-node position (node-children (car found)))
        #++ (let ((node (car found)))
              (and (listp (node-content state node))
                   ;; (car (node-content state node))
                   (find-node position (node-content state node))))
        :while found
        :collect found))



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
            ;; (format *debug-io* "~&~s ~{~s~^ ~}" node args)
            ;; Removing useless whitespaces
            (unless (valid-node-p node)
              (push (list (node-start node)
                          point-max
                          :error
                          "Syntax error")
                    diagnostics))
            #++ ;; TODO WIP Checking if an in-package form references
            ;; a package that exists
            (alexandria:when-let ((package-designator (in-package-node-p state node)))
              (push
               (list (node-start node)
                     (node-end node)
                     :warning
                     (format nil "This is indeed an in-package form. ~s" package-designator))
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
