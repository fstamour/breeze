(defpackage #:breeze.whitespaces
  (:documentation "Linter checks about whitespaces.")
  (:use #:cl
        #:breeze.analysis
        #:breeze.checks
        #:breeze.diagnostics)
  (:import-from #:alexandria
                #:when-let*
                #:when-let))

(in-package #:breeze.whitespaces)

;; This assumes that NODE-ITERATOR points to a whitespace node
(defcheck warn-extraneous-whitespaces (node-iterator)
  :node-type breeze.parser:whitespace
  (if (rootp node-iterator)
      ;; Check for spaces at top-level, just before a form. Said
      ;; otherwise: an indented top-level form.
      (unless (lastp node-iterator)
        (when-let* (($next (next-iterator node-iterator))
                    (next-node (value $next))
                    (whitespaces (node-string node-iterator)))
          (unless (every (lambda (c) (char= #\Newline c)) whitespaces)
            (let ((pos (position #\Newline whitespaces :from-end t)))
              (when (or (and (null pos) (firstp node-iterator))
                        (and pos (/= pos (length whitespaces))))
                (char/= #\Newline (char whitespaces (1- (length whitespaces))))
                (node-style-warning
                 node-iterator
                 "Extraneous leading whitespaces at top-level."
                 :replacement (when pos (subseq whitespaces 0 (1+ pos)))))))))
      (let ((firstp (firstp node-iterator))
            (lastp (lastp node-iterator)))
        (cond
          ((and firstp lastp)
           (node-style-warning
            node-iterator "Extraneous whitespaces."
            :replacement nil))
          ((and firstp (parens-node-p
                        (parent-node node-iterator)))
           (node-style-warning
            node-iterator "Extraneous leading whitespaces."
            :replacement nil))
          ((and lastp (not (line-comment-node-p
                            (previous-sibling node-iterator))))
           (node-style-warning
            node-iterator "Extraneous trailing whitespaces."
            :replacement nil))
          #++
          ((< 1 (count #\newline (source node-iterator)
                       :start (start node-iterator)
                       :end (end node-iterator)))
           (let ((leading-newlines (position #\newline (source node-iterator)
                                             :test #'char/=
                                             :start (start node-iterator)
                                             :end (end node-iterator))))
             (node-style-warning
              node-iterator "Extraneous newlines."
              :replacement (subseq (source node-iterator)
                                   (1- leading-newlines)
                                   (end node-iterator)))))
          ((and (not (or firstp lastp))
                ;; Longer than 1
                (< 1 (- (end node-iterator) (start node-iterator)))
                ;; "contains no newline"
                (not (position #\Newline
                               (source node-iterator)
                               :start (start node-iterator)
                               :end (end node-iterator)))
                ;; is not followed by a line comment
                (not (line-comment-node-p
                      (next-sibling node-iterator))))
           (node-style-warning
            node-iterator "Extraneous internal whitespaces."
            :replacement " "))))))

;; TODO this isn't a check...
;; TODO this is not used
#++
(defcheck warn-missing-indentation (node-iterator)
  (node-style-warning
   node-iterator
   "Missing indentation"
   :replacement (or
                 (let ((previous-node (copy-iterator node-iterator)))
                   ;; TODO this assumes that the node before is a
                   ;; whitespace node with a newlind in it and the node
                   ;; before that is anything and the node before it
                   ;; again represents the right indentation.
                   (go-backward previous-node 3)
                   (let* ((whitespaces (node-string previous-node))
                          (pos (position #\Newline whitespaces :from-end t)))
                     (when pos
                       (let ((indentation (subseq whitespaces (1+ pos))))
                         ;; TODO this assumes that the node doesn't contain
                         ;; newlines and so doesn't require other fixes.
                         (concatenate 'string indentation (node-string node-iterator))))))
                 'null)))


(defcheck missing-whitespace (node-iterator)
  :node-type (:not breeze.parser:whitespace)
  (when-let ((next-node (next-sibling node-iterator)))
    (unless (whitespace-node-p next-node)
      (node-style-warning node-iterator
                          "Missing space between forms."
                          ;; TODO the name "replacement doesn't make much sense anymore
                          ;; TODO the "quickfix command doesn't know how to handle this yet.
                          :replacement '(:insert-after " ")))))
