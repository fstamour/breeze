
(defpackage #:breeze.egraph-command
  (:documentation "Commands that use equivalence graphs (e-graphs) to refactor code.")
  (:use #:cl #:breeze.analysis)
  (:import-from #:breeze.command
                #:define-command
                #:pulse
                #:current-buffer
                #:message))

(in-package #:breeze.egraph-command)

(defun node-at-point ())

;; TODO This should go in a file for "commands that uses parse trees"
(defun pulse-node (node)
  (pulse (start node) (end node)))

(defun current-node (&key pulsep)
  (alexandria:when-let*
      ((buffer (current-buffer))
       (node-iterator (node-iterator buffer)))
    (when pulsep (pulse-node node-iterator))
    node-iterator))

;; "E-Graph Good" → egg →🥚 → scramble
(export
 (define-command scramble ()
   "Scramble the code at point."
   (alexandria:when-let (($node (current-node)))
     (message (node-string $node)))))
