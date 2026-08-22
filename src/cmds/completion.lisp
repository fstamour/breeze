#|

contextual completions:

(with-slot (B) A)
- A should probably comes from nearby
- A should be a "clos object", or a form that evaluates to one
- B should be a one of A's slots

(in-package #:A)
- A must be the name or nickname of a package
- silly question: can A be a local package nickname???

(defpackage name
  (A )
  (:use B)
  (:import-from C D)
  (:export E)
  (:nicknames F)
  (:shadow G)
  (:shadowing-import H I)
  (:intern J)
  (:documentation K)
  (:size L))
- A must be one of defpackage's options
- B must not be already in another :use option
  - that would be redundant
- C should probably not be already in another :import-from options
  - though it's totally legal, and could be done on purpose
- B, C, H must be package designators
- F must be a "new" package designator
- E, J and G must be symbol designators
- K must be a string
- L must be a positive integer
- D must be a symbol designator for a symbol from the package C
  - _technically_, D doesn't need to be exported, but that not what
    the majority of users would expect
- I must be a symbol designator
  - IDK what else could be said about this one...


(declaim A)
(defun B (...)
  (declare C))
- A is very probably a declaration about B
  - e.g. inline, ftype, optimization
- C is probably a declaration about B's arguments

fiind-sym
=> should suggest find-symbol


(loop ... :fin|) > should complete "finally" !!!

when in and .asd file
- :module | => complete a directory name
- :file | => complete a file name

|#

(defpackage #:breeze.completion
  (:documentation "Commands to complete text")
  (:use #:cl #:breeze.generics #:breeze.parser)
  (:import-from #:breeze.command
                #:define-command
                #:current-buffer
                #:return-value-from-command
                #:noninteractive)
  (:import-from #:breeze.parser
                #:node-iterator)
  (:export #:completions-at-point))

(in-package #:breeze.completion)

;; Trying to get the "previous s-expression"
(defun maybe-previous-node ($node point)
  #++ "The the node is not the first, and if the point is at the beginning of
a whitespace, return the previous node. Otherwise return $NODE unchanged."
  ;; (break "node before: ~s" (value $node))
  (let ((node (value $node)))
    (cond
      ((or (token-node-p node)
           (and (not (parens-node-p node))
                (firstp $node)))
       $node)
      (;; (= point (start $node))
       (whitespace-node-p node)
       ;; check previous
       (let* (($previous (previous-iterator $node))
              (previous-node (value $previous)))
         (if (token-node-p previous-node)
             $previous
             $node)))
      ((and
        (or (= point (end node))
            (= point (1- (end node))))
        (parens-node-p node))
       ;; get the last non- whitespace-or-comment-node-p child of
       ;; node...
       (loop
         :with children := (children node)
         :for i :from (1- (length children)) :downto 0
         :for child := (aref children i)
         :unless (whitespace-or-comment-node-p child)
           :do (let (($child (copy-iterator $node)))
                 (go-down $child)
                 (setf (pos $child) i)
                 (return $child))
         :finally (return $node)))
      (t $node))))

;; (trace maybe-previous-node)
;; (trace completions-at-point)
;; (untrace return-value-from-command)
;; def
;; (forc-o)
;; (in-)

#|
emacs lisp:

(setq completion-at-point-functions/bak (copy-list completion-at-point-functions))
(setq completion-at-point-functions completion-at-point-functions/bak)
(delq 'slime--completion-at-point completion-at-point-functions)
(add-to-list 'completion-at-point-functions 'breeze-completion-at-point)

(setq completion-at-point-functions '(slime--completion-at-point t))
|#

#|


TODO make this testable ffs
TODO add tests!

"def|"
"d|ef"
"|def"
"(in-|)"

|#


;; TODO move to "dogfood"
(export 'inspect-buffer-at-point)
(define-command inspect-buffer-at-point ()
  "For debugging..."
  (let* ((buffer (current-buffer))
         (point (breeze.buffer:point buffer))
         ($node (node-iterator buffer))
         (node (value $node)))
    (breeze.command:message "point: ~s~%extract: ~%~s ~%node: ~s"
                            point
                            (breeze.string:around (source buffer)
                                                  point
                                                  10
                                                  "<here>")
                            node)))

(define-command completions-at-point (&optional string)
  "completion-at-point"
  (declare (ignorable string))
  (declare noninteractive)

  ;; to enable/disable this while developing it
  ;; (progn (return-value-from-command nil) (return-from completions-at-point nil))

  (let* ((buffer (current-buffer))
         ($node (maybe-previous-node (node-iterator buffer)
                                     (breeze.command:current-point)))
         (node (value $node)))
    ;; (break "node: ~s" node)
    (cond
      ((token-node-p node)
       (let* ((package (breeze.buffer:current-package buffer))
              (needle
                ;; TODO I use `name' to avoid the package marker, but this is wrong :D
                (name node)
                ;; TODO maybe use `breeze.parser:node-string-designator' instead
                ;; (breeze.parser:node-string $node)
                )
              ;; TODO apropos-list won't work if the token contains
              ;; package markers
              (candidates (remove-if-not
                           'breeze.xref:externalp
                           (or (and package (apropos-list needle package))
                               (apropos-list needle)))))
         ;; (break "candidates ~s" candidates)
         (return-value-from-command
          (loop :for c :in candidates
                :collect (string-downcase c)))
         #++ (when candidates
               ;; check for symbols in the current package first
               (loop :for symbol :in candidates
                     :when (eq package (symbol-package symbol))
                       :do (return-value-from-command (list (string-downcase symbol))))))))
    ;; Fallback: found nothing
    (return-value-from-command '())
    #++
    (return-value-from-command
     (list "prin1" "print")
     #++
     (when (token-node-p node)

       '("asfd" "qwer" "uiop")))))



;; find-most-similar-symbol

#|
print

(breeze.listener::find-most-similar-symbol "make-node-iterator")
|#
