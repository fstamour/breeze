(in-package #:breeze.parse-tree)


;;; Enclosing form navigation


;; TODO this generic, it should work for any kind of iterators
(defun normalize-pattern-predicate (matcher &key skipp)
  "Convert MATCHER into a predicate on iterator.

MATCHER may be:
  - a function: returned as-is
  - a symbol: converted into a function that match a list forms whose first element is that symbol
  - a list (pattern): compiled with `breeze.pattern:compile-pattern` and wrapped into a lambda."
  (etypecase matcher
    (function matcher)
    (symbol
     (let ((predicate (normalize-pattern-predicate
                       `(((:symbol ,matcher)))
                       :skipp skipp)))
       (lambda (iterator)
         (and (children iterator)
              (funcall predicate iterator)))))
    ((or cons vector breeze.pattern:pattern)
     (let ((compiled-pattern (breeze.pattern:compile-pattern matcher)))
       (lambda (iterator)
         (breeze.pattern:match compiled-pattern
           ;; match advances the iterator, but we don't want the
           ;; predicate to modify the input...
           (copy-iterator iterator)
           :skipp skipp))))))

;; TODO maybe add a keyword argument to control whether to return
;; NODE-ITERATOR if it satisfies PREDICATE (i.e. should
;; `find-enclosing-node' include NODE-ITERATOR itself; or should it
;; only search the parents)
(defun find-enclosing-node (node-iterator matcher
                            &key (direction :bottom-up)
                            &aux (predicate (normalize-pattern-predicate
                                             matcher
                                             :skipp #'whitespace-or-comment-node-p)))
  "Find the first enclosing ancestor of NODE-ITERATOR that satisfies MATCHER.

Returns a an iterator, or NIL if no ancestor matches.

MATCHER is a designator for a function that accepts a
node-iterator (see `normalize-pattern-predicate').

DIRECTION controls the traversal order:
  :bottom-up (default) walk from NODE-ITERATOR towards the root,
  returns the nearest (innermost) ancestor that matches.
  :top-down walk from the root towards NODE-ITERATOR, returns the
  furthest (outermost) ancestor that matches."
  (ecase direction
    (:bottom-up
     (loop :with it := (copy-iterator node-iterator)
           ;; :for before := (copy-iterator it)
           :do (when (funcall predicate it)
                 (return it))
               (unless (go-up it)
                 (return nil))))
    (:top-down
     (loop :with it := (goto-root (copy-iterator node-iterator))
           :do (when (funcall predicate it)
                 (return it))
               (unless (go-down it)
                 (return nil))))))

(defun find-nearest-enclosing-node (node-iterator matcher)
  "Find the closest enclosing node matching MATCHER.
MATCHER may be a symbol, a pattern list, a pattern, or a predicate
function.

Returns a node-iterator or NIL."
  (find-enclosing-node node-iterator matcher :direction :bottom-up))

(defun find-furthest-enclosing-node (node-iterator matcher)
  "Find the most distant enclosing node matching MATCHER.
MATCHER may be a symbol, a pattern list, a pattern, or a predicate
function.

Returns a node-iterator, or NIL."
  (find-enclosing-node node-iterator matcher :direction :top-down))

(defun find-all-enclosing-nodes (node-iterator matcher)
  "Return all enclosing nodes matching MATCHER, nearest-first.
MATCHER may be a symbol, a pattern list, or a predicate function.
Each element is a node-iterator copy positioned at the matching node."
  (loop
    :with predicate := (normalize-pattern-predicate matcher)
    :with it := (copy-iterator node-iterator)
    :while (go-up it)
    :when (funcall predicate it)
      :collect (copy-iterator it)))
