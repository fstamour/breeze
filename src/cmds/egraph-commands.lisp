
(defpackage #:breeze.egraph-command
  (:documentation "Commands that use equivalence graphs (e-graphs) to refactor code.")
  (:use #:cl #:breeze.analysis)
  (:import-from #:breeze.command
                #:define-command
                #:message)
  (:import-from #:breeze.command-utils
                #:pulse-node
                #:current-node))

(in-package #:breeze.egraph-command)

;; "E-Graph Good" → egg →🥚 → scramble
(export
 (define-command scramble ()
   "Scramble the code at point."
   (alexandria:when-let (($node (current-node)))
     (message (node-string $node)))))
