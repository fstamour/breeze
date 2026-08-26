(defpackage #:breeze.top-levelness
  (:documentation "Utilities to deal with forms that preserves top-levelness.")
  (:use #:cl #:breeze.iterator #:breeze.parser)
  (:import-from #:breeze.pattern
                #:let-match)
  (:export #:top-level-node-iterator
           #:top-level-forms
           #:top-level-node-p
           #:map-top-level-forms))

(in-package #:breeze.top-levelness)

(defmethod top-level-node-iterator ((iterator node-iterator))
  ;; TODO get the actual top-level node, not the root one (see `map-top-level-forms')
  (root iterator))

(defun top-level-forms ($node)
  (let-match
      (((:either
         (progn (:maybe ?body))
         (eval-when _ (:maybe ?body))
         ((:either macrolet symbol-macrolet)
          _ #| definitions |#
          (:zero-or-more (declare _))
          (:maybe ?body))
         (locally
             (:zero-or-more (declare _))
           (:maybe ?body))))
       $node
       :skipp #'whitespace-or-comment-node-p)
    ?body))

;; TODO add tests
(defun top-level-preserving-node-p ($node)
  "Does $NODE preserve the top-levelness of the child it was entered from?
  $NODE's position at the depth just below it designates that child."
  (let (($body (top-level-forms (copy-iterator $node))))
    (and $body
         (<= (pos $body)
             ;; magic: it looks deeper in the iterator
             (position-at-depth $node (1+ (depth $node)))))))

;; TODO add tests
(defun top-level-node-p ($node)
  (let (($parent (parent $node)))
    (or (null $parent)
        (not (find-nearest-enclosing-node
              $parent
              (complement #'top-level-preserving-node-p))))))

;; TODO add tests
(defmethod map-top-level-forms (function (state state))
  (loop :with iterator = (make-node-iterator state)
        :until (tree-iterator-done-p iterator)
        :do (let* ((node (tree-iterator-value iterator))
                   (whitespace-or-comment-p (whitespace-or-comment-node-p node))
                   ;; `top-level-forms' is an expensive match, and
                   ;; whitespaces and comments can never match it.
                   (body (unless whitespace-or-comment-p
                           (top-level-forms iterator))))
              (cond
                (body (copy-iterator body iterator))
                (t
                 (unless whitespace-or-comment-p
                   (funcall function iterator))
                 ;; advance the iterator
                 (incf (pos iterator))
                 ;; go up if the current level is done
                 (when (next-up iterator)
                   (incf (pos iterator))))))))
