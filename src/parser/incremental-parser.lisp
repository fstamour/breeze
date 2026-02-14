
(uiop:define-package #:breeze.incremental-parser
    (:documentation "Parsing lisp code incrementally")
  (:use #:cl)
  (:use-reexport #:breeze.parser)
  (:import-from #:breeze.parser
                #:reparse)
  (:import-from #:alexandria
                #:when-let)
  (:export #:apply-edit-to-source))

(in-package #:breeze.incremental-parser)

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
             (error "Invalid deletion: ends outside the source")))
          ;; TODO :replace-at
          ))
      (error "NIL is not a valid edit")))

;; TODO move into "parse-state.lisp"
(defun apply-edit-to-source (state edit &aux (source (source state)))
  (let ((new-string (apply-edit-to-string edit source)))
    (setf (source state)
          new-string
          (current-position (iterator state)) 0
          (slot-value (iterator state) 'vector) new-string))
  (breeze.parser:parse (source state) state))

(defun apply-edit-to-string (edit string)
  (destructuring-bind (type position detail)
      edit
    ;; TODO Would be nice not to have to rebuild the whole string
    (ecase type
      (:insert-at
       (concatenate 'string
                    (subseq string 0 position)
                    detail
                    (subseq string position)))
      (:delete-at
       (concatenate 'string
                    (subseq string 0 position)
                    (subseq string (+ position detail))))
      (:replace-at
       (destructuring-bind (start . end) position
         (concatenate 'string
                      (subseq string 0 start)
                      detail
                      (subseq string end)))))))

;; This is very inefficient, for starter it handles only one edit at a
;; time...
#++ ;; TODO this is completely broken because it was written back when
    ;; the parse tree was still using lists instead of vectors.
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
                 (add-offset node offset))
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
          (let ((rest (goto-position
                       (make-node-iterator state)
                       position))
                #++ (suffix-before (subseq (source state)
                                           (end (second rest)))))
            (apply-edit-to-source state edit)
            (setf (pos state) (start (second rest)))
            ;; TODO note the lack of reuse xD
            ;; - (first rest) is unchanged
            ;; - (second rest) is the first node to re-parse
            ;; - (third rest) is the first node that we might be able to reuse
            (let ((new-node (breeze.parser::read-any state)))
              (cond
                ((and
                  (third rest)
                  ;; we stopped parsing the new node at an analogous
                  ;; location
                  (= (+ (length detail) (start (third rest)))
                     (end new-node))
                  ;; the parser would've been at the same state
                  (char= (char (source state) (+ (length detail) (start (third rest))))
                         (char (source state) (end new-node)))
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


;; TODO keep track of the buffers/files, process these kind of edits
;; "object":
;;
;; (:DELETE-AT 18361 1)
;; (:INSERT-AT 17591 ";")
