#|

contextual completions:

(with-slot (B) A)
- A should probably comes from nearby
- A should be a "clos object", or a form that evaluates to one
- B should be a one of A's slots

(in-package #:A)
- A must be the name or nickname of a package
- silly question: can A be a local package nickname???

(defpackage name
  (A )
  (:use B)
  (:import-from C D)
  (:export E)
  (:nicknames F)
  (:shadow G)
  (:shadowing-import H I)
  (:intern J)
  (:documentation K)
  (:size L))
- A must be one of defpackage's options
- B must not be aleady in another :use option
  - that would be redundant
- C should probably not be already in another :import-from options
  - though it's totally legal, and could be done on purpose
- B, C, H must be package designators
- F must be a "new" package designator
- E, J and G must be symbol designators
- K must be a string
- L must be a positive integer
- D must be a symbol designator for a symbol from the package C
  - _technically_, D doesn't need to be exported, but that not what
    the majority of users would expect
- I must be a symbol designator
  - IDK what else could be said about this one...


(declaim A)
(defun B (...)
  (declare C))
- A is very probably a delaration about B
  - e.g. inline, ftype, optimization
- C is probably a declaration about B's arguments

fiind-sym
=> should suggest find-symbol

|#

(defpackage #:breeze.completion
  (:documentation "Commands to complete text")
  (:use #:cl)
  (:import-from #:breeze.command
                #:define-command
                #:current-buffer
                #:return-value-from-command)
  (:import-from #:breeze.parser
                #:node-iterator)
  (:export #:completions-at-point))

(in-package #:breeze.completion)

;; TODO this shouldn't show up in the list of commands suggested by
;; "quickfix", and it shouldn't generate an interactive command when
(define-command completions-at-point (&optional string)
  "completion-at-point"
  (declare (ignorable string))
  (let* (($node (node-iterator (current-buffer)))
         (node (breeze.iterator:value $node)))
    (declare (ignorable node))
    ;; (break "~s" (breeze.parser:node-string $node))
    (return-value-from-command
     (list "prin1" "print")
     #++
     (when (breeze.parser:token-node-p node)

       '("asfd" "qwer" "uiop")))))

;; find-most-similar-symbol

#|
print

(breeze.listener::find-most-similar-symbol "make-node-iterator")
|#
