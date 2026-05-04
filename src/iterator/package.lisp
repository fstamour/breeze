
(defpackage #:breeze.iterator
  (:documentation "Iterators for vectors, nested vectors (trees), streams and concatenations.")
  (:use #:cl)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.class-utils
                #:define-class)
  ;; Generics
  (:export #:donep
           #:next
           #:value
           #:reset
           #:copy-iterator
           #:current-position)
  ;; Classes and Constructors
  (:export #:iterator
           #:vector-iterator
           #:tree-iterator
           #:make-vector-iterator
           #:make-tree-iterator
           #:concat-iterator
           #:make-concat-iterator
           #:stream-iterator
           #:make-stream-iterator)
  ;; Accessors
  (:export
   #:tree-iterator-done-p
   #:tree-iterator-value
   #:tree-iterator-next
   #:pos
   #:subtree
   #:subtrees
   #:positions
   #:children
   #:push-subtree
   #:pop-subtree
   #:go-forward
   #:go-backward
   #:go-down
   #:go-up
   #:next-up
   #:next-preorder
   #:subtree-at-depth
   #:root-subtree
   #:goto-root
   #:next-up
   #:value-at-depth
   #:parent-value
   #:parent
   #:root-value
   #:rootp
   #:root
   #:previous-sibling
   #:next-sibling
   #:previous-iterator
   #:next-iterator
   #:map-subtree-preorder)
  ;; Other utility functions
  (:export
   #:firstp
   #:lastp
   #:before-last-p
   #:depth
   #:collect
   #:map-iterator
   #:iterator-value))
