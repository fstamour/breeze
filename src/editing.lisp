(defpackage #:breeze.editing
  (:documentation "Text and structural editing")
  (:use #:cl
        #:breeze.lossless-reader
        #:breeze.command
        #:breeze.buffer)
  (:export #:kill-sexp))

(in-package #:breeze.editing)

;; (trace replace-region)

#|
;; TODO add NOTE: "can't splice comment", but I wish I could
;; e.g.  `  ;; (some | code)`
;; paredit-splice-sexp or paredit-splice-sexp-killing-backward


(|asdf)qwer
M-<up> paredit-splice-sexp-killing-backward
asfdqwer
-- should be
asdf qwer

|#

(define-command kill-sexp ()
  "Kill the expression following point."
  (let* ((buffer (current-buffer))
         (point (point buffer))
         (node-iterator (copy-iterator buffer)))
    #++ (break "point: ~D, start: ~D end: ~D ~s" point
           (start node-iterator)
           (end node-iterator) (node-string node-iterator))
    (flet ((maybe-include-next-node (it)
             (if (= point (1- (end it)))
                 (1- (end it))
                 (end
                  (let ((next-node (next-sibling it)))
                    ;; (break "next-node: ~s" next-node)
                    (if (and next-node (whitespace-node-p next-node))
                        next-node
                        it))))))
      (cond
        ((whitespace-node-p (value node-iterator))
         (let* ((lastp (lastp node-iterator))
                (start (if lastp (start node-iterator) point)))
           (unless lastp
             (next node-iterator :dont-recurse-p t))
           (replace-region start (maybe-include-next-node node-iterator) "")))
        ((= point (end node-iterator))
         (message "END"))
        (t
         (replace-region point (maybe-include-next-node node-iterator) ""))))))

#|
;; TODO
(define-command forward-slurp)

;; TODO
(define-command backward-slurp)

;; TODO
(define-command forward-barf)

;; TODO
(define-command backward-barf)
|#


#++ ;; TODO
(define-command beginning-of-defun ()
  "Move backward to the beginning of a defun.")
