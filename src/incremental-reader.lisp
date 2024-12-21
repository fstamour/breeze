
(uiop:define-package #:breeze.incremental-reader
    (:documentation "")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader)
  (:import-from #:breeze.lossless-reader
                #:reparse)
  (:import-from #:breeze.analysis
                #:find-node*))

(in-package #:breeze.incremental-reader)

;; The simplest (stupidest) way to incrementally parse a buffer would
;; be to find the first top-level node that includes the changes and
;; start re-parsing from there.

(defun check-edit (source edit)
  (if edit
      (destructuring-bind (type position detail)
          edit
        (when (minusp position)
          (error "Edit starts at negative position"))
        (when (< (length source) position)
          (error "Edit starts outside the source"))
        (ecase type
          (:insert-at
           (when (zerop (length detail))
             (error "Invalid insertion of the empty string")))
          (:delete-at
           (when (zerop detail)
             (error "Invalid deletion of 0 characters"))
           (when (minusp detail)
             (error "Invalid deletion of negative number of characters"))
           (when (< (length source) (+ position detail))
             (error "Invalid deletion: ends outside the source")))))
      (error "NIL is not a valid edit")))

(defun apply-edit-to-source (state edit &aux (source (source state)))
  (destructuring-bind (type position detail)
      edit
    (setf (source state)
          ;; Would be nice not to have to rebuild the whole
          ;; buffer's string
          (ecase type
            (:insert-at
             (concatenate 'string
                          (subseq source 0 position)
                          detail
                          (subseq source position)))
            (:delete-at
             (concatenate 'string
                          (subseq source 0 position)
                          (subseq source (+ position detail))))))))

(defun find-node* (position nodes)
  "Given a list of NODES, return the list that starts at the element just
before the node that contains the POSITION."
  (when (listp nodes)
    (loop
      :for previous-rest = nil :then rest
      :for rest :on nodes
      :for node := (car rest)
      :when (and node
                 (<= (node-start node) (node-end node))
                 (< position (node-end node)))
        :do (return previous-rest))))

;; This is very inefficient, for starter it handles only one edit at a
;; time...
(defun edit-and-parse (state edit)
  (check-edit (source state) edit)
  (destructuring-bind (type position detail)
      edit
    (ecase type
      (:insert-at
       (cond
         ;; If the previous parse was empty, just start from scratch
         ((null (tree state))
          (setf (source state) detail
                (pos state) 0)
          (parse nil state))
         ;; Inserts at the beginning
         ;; This is wrong because it assumes the first node is
         ;; unaffected by the insert
         ((zerop position)
          (let ((new-nodes (tree (parse detail))))
            (apply-edit-to-source state edit)
            (loop
              :with offset = (length detail)
              :for node :in (tree state)
              :do
                 ;; ""fixing"" the nodes' positions, this is only
                 ;; superficial thought, it only fixes the "top-level"
                 ;; nodes... which would screw the analysis later on.
                 ;;
                 ;; Possible alternative: add another field to the
                 ;; nodes: offset... _maybe_ it could be in the state
                 ;; itself... we could make the node immutable again™.
                 (incf (node-start node) offset)
                 (unless (minusp (node-end node))
                   (incf (node-end node) offset)))
            (setf (tree state) (append new-nodes (tree state))
                  (pos state) (length (source state)))))
         ;; Inserts at the end
         ((= (length (source state)) position)
          (apply-edit-to-source state edit)
          (let ((new-nodes (reparse state))
                (last-node (last (tree state))))
            (setf (cdr last-node) new-nodes)))
         ;; Inserts in the middle
         (t
          (let ((rest (find-node* position (tree state)))
                #++ (suffix-before (subseq (source state)
                                           (node-end (second rest)))))
            (apply-edit-to-source state edit)
            (setf (pos state) (node-start (second rest)))
            ;; TODO note the lack of re-use xD
            ;; - (first rest) is unchanged
            ;; - (second rest) is the first node to re-parse
            ;; - (third rest) is the first node that we might be able to re-use
            (let ((new-node (breeze.lossless-reader::read-any state)))
              (cond
                ((and
                  (third rest)
                  ;; we stopped parsing the new node at an analoguous
                  ;; location
                  (= (+ (length detail) (node-start (third rest)))
                     (node-end new-node))
                  ;; the parser would've been at the same state
                  (char= (char (source state) (+ (length detail) (node-start (third rest))))
                         (char (source state) (node-end new-node)))
                  ;; if there were more edits, it would be more complex...
                  )
                 (setf (pos state) (length (source state)))
                 (setf (cdr rest) (cons new-node (cddr rest))))
                (t
                 (setf (cdr rest) (cons new-node (reparse state))))))))))
      (:delete-at
       (error "Not implemented")))))

#++
(let* ((buffer-string
         "")
       (state (parse buffer-string))
       (edit '(:insert-at 0 " ")))
  (destructuring-bind (type position detail)
      edit
    (ecase type
      (:insert-at
       (unless (<= (length (source state)) position)
         (error "Insertion outside of the previous state..."))
       (unless (tree state)
         (setf (source state) detail
               (pos state) 0)
         (parse nil state)))
      (:delete-at)))
  (with-output-to-string (output)
    (unparse state output)))
