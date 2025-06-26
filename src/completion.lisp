
(defpackage #:breeze.completion
  (:documentation "Commands to complete text")
  (:use #:cl)
  (:import-from #:breeze.command
                #:define-command
                #:current-buffer
                #:return-value-from-command)
  (:import-from #:breeze.lossless-reader
                #:node-iterator)
  (:export #:completions-at-point))

(in-package #:breeze.completion)

(define-command completions-at-point (&optional string)
  ""
  (let* (($node (node-iterator (current-buffer)))
         (node (breeze.iterator:value $node)))
    ;; (break "~s" (breeze.lossless-reader:node-string $node))
    (return-value-from-command
     (list "prin1" "print")
     #++
     (when (or
            (breeze.lossless-reader:token-node-p node)
            (breeze.lossless-reader:symbol-node-p node))

       '("asfd" "qwer" "uiop")))))

#|
print

(breeze.listener::find-most-similar-symbol "make-node-iterator")
|#
