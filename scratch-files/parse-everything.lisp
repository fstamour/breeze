#|

Goal parse the source code of:
- all of breeze's transitive dependencies
- asdf and uiop
- sbcl, ecl, etc
- everything in local-projects/
- everything in quicklisp
- everything in ultralisp

Motivation:
- test the parser, and the pattern matching

What am I going to try first:
- I already have something to parse breeze's files.
- And I just wrote something to iterator over every top-level form
- I should continue with those but adding more systems and files

TODO:
- lint everything in the workspace

|#

(defpackage #:breeze.parse-everything
  (:documentation "")
  (:use #:cl #:breeze.workspace #:breeze.analysis))

(in-package #:breeze.parse-everything)

(time
 (defparameter *test-workspace*
   (let ((*workspace* (make-workspace))
         (all-files ()))
     (breeze.asdf::walk-dependencies
      (breeze.asdf:find-all-related-systems "breeze")
      (lambda (system)
        (format t "~%System: ~s" system)
        ;; "remove nil" because (breeze.asdf:system-files '#:uiop)
        ;; returns (nil nil) 🤔
        (let ((files (remove nil (breeze.asdf:find-all-related-files system))))
          (setf all-files (append all-files files))
          (format t "~%all-files: ~D" (length all-files)))))
     (setf all-files (delete-duplicates all-files))
     (format t "~%Deduplicated: ~D" (length all-files))
     (add-files-to-workspace all-files)
     *workspace*)))
;; roughly 0.9 seconds to read 410 files

(hash-table-count
 (breeze.workspace::buffers *test-workspace*))
;; => 430

;; print all the files the workspace *test-workspace*
(map-workpace-buffers
 (lambda (buffer)
   (print (filename buffer)))
 *test-workspace*)

;; find all the unique extensions
(let ((types))
  (map-workpace-buffers
   (lambda (buffer)
     (pushnew (pathname-type (filename buffer))
              types
              :test 'equalp))
   *test-workspace*)
  types)
;; => (:UNSPECIFIC "sexp" "asd" "lisp")

(defun binding->$node (bindings from)
  (let ((binding (find-binding bindings from)))
    (when bindings
      (to binding))))

(defun binding->node (bindings from)
  (let (($node (binding->$node bindings from)))
    (when $node
      (value $node))))

(defun binding->name (bindings from)
  (let ((node (binding->node bindings from)))
    (when node
      (name node))))

(defparameter *top-level-forms*
  (compile-pattern
   '(:either
     ((:symbol "DEFPACKAGE" "CL") ?package-name)
     ((:symbol "DEFINE-PACKAGE" "UIOP") ?package-name)
     ((:symbol "IN-PACKAGE" "CL"))
     ((:symbol "DEFSYSTEM"
       ;; "DEFSYSTEM" #++ "ASDF"
       ))
     ((:symbol "DEFPARAMETER" "CL") ?var-name)
     ((:symbol "DEFVAR" "CL") ?var-name)
     ((:symbol "DEFUN" "CL") ?function-name)
     ((:symbol "DEFGENERIC" "CL") ?generic-name)
     ((:symbol "DEFMETHOD" "CL") ?method-name)
     ((:symbol "MACROLET" "CL"))
     ((:symbol "DEFMACRO" "CL") ?macro-name)
     ((:symbol "DEFINE-CONDITION" "CL"))
     ((:symbol "DEFTYPE" "CL") ?type-name)
     (setf (documentation ?x ?doc-type) ?docstring)
     ((:symbol "DEFCLASS" "CL") ?class-name)
     ((:symbol "DEFSTRUCT" "CL") ?struct-name)
     ((:symbol "DECLAIM" "CL"))
     ((:symbol "PROCLAIM" "CL"))
;;; breeze...
     ((:symbol "DEFINE-NODE-TYPE" "BREEZE.PARSE-TREE"))
     ((:symbol "DEFREADER" "BREEZE.PARSER"))
     ((:symbol "DEFINE-NODE-MATCHER" "BREEZE.ANALYSIS"))
     ((:symbol "DEFINE-COMMAND" "BREEZE.COMMAND"))
     ((:symbol "DEFUN-SUGGEST" "BREEZE.LISTENER"))
;;; alexandria
     ((:symbol "DEFINE-CONSTANT" "ALEXANDRIA") ?var-name)
;;; parachute
     ((:symbol "DEFINE-TEST+RUN" "PARACHUTE"))
     ((:symbol "DEFINE-TEST" "PARACHUTE"))

     ((:symbol "DEF-SUITE" "FIVEAM"))
     ((:symbol "IN-SUITE" "FIVEAM"))
     ((:symbol "DEF-FIXTURE" "FIVEAM"))
     ((:symbol "TEST" "FIVEAM"))
     ((:symbol "DEF-TEST" "FIVEAM"))
     ((:var ?unknown-top-level-symbol (:symbol))))))

(defparameter *top-level-patterns*
  (compile-pattern
   `(:either
     ,*top-level-forms*
     (:var ?top-level-token (:symbol :wild))
     ;; ?other
     )))

(defparameter *results* ())

(let ((defs)
      (uses)
      (others)
      (custom-dispatch-macros))
  (map-workpace-buffers
   (lambda (buffer)
     (format t "~&~a" buffer)
     ;; filter by extension because of systems like alexadria that
     ;; include their licence file as a :static-file
     (when (member (pathname-type (filename buffer))
                   '("asd" "lisp")
                   :test 'equalp)

;;;;;;;;;;;;;;;;;; Look for reader macros
       (let* (($node (node-iterator buffer))
              (pos (search "set-dispatch-macro-character"
                           (source $node)
                           :test #'char-equal)))
         (when pos
           (goto-position $node pos)
           (let ((node (value $node)))
             ;; if it's a string-node, it's probably a docstring (e.g. in named-readtable)
             (unless (string-node-p node)
               ;; (break "dispatch-macro-character: ~%~s ~%~s" buffer (node-string (parent $node)))
               (let ((bindings (match (compile-pattern '(set-dispatch-macro-character ?dispatch-char ?sub-char))
                                 (parent $node)
                                 :skipp #'whitespace-or-comment-node-p)))
                 (when bindings
                   (let (($disp-char (binding->$node bindings '?dispatch-char))
                         ($sub-char (binding->$node bindings '?sub-char)))
                     #|
                     for example:       ; ; ; ;
                     #<BREEZE.BUFFER:BUFFER "/home/fstamour/quicklisp/dists/quicklisp/software/iterate-release-b0f9a9c6-git/iterate.lisp"> ; ; ; ;
                     (sharp-char 19611 19614 (token 19612 19614 :name "#")) ; ; ; ;
                     (sharp-char 19615 19618 (token 19616 19618 :name "L")) ; ; ; ;
                     |#
                     #++
                     (break "dispatch-macro-character: ~%~s ~%~s ~%~s"
                            buffer
                            (value $disp-char)
                            (value $sub-char))
                     (when (and (sharp-char-node-p (value $disp-char))
                                (sharp-char-node-p (value $sub-char)))
                       (push (list buffer
                                   $disp-char
                                   (name (children (value $disp-char)))
                                   (name (children (value $sub-char))))
                             custom-dispatch-macros)))))))))

;;;;;;;;;;;;;;;;;; Look for unusual top-level forms
       (map-top-level-forms
        (lambda ($node &aux
                         (node (value $node))
                         (type (node-type node)))
;;;;;;;;;;;;; Look for "syntax errors"
          ;; TODO this only checks for invalid _top-level_ nodes
          ;; So far, there's only "iterate.lisp" that fails because of
          ;; a custom reader macro
          (when (and (not (alexandria:ends-with-subseq "/iterate.lisp" (name buffer)))
                     (not (valid-node-p node)))
            (break "invalid node: ~%~s ~%~s ~%~s" buffer node (node-string $node)))
;;;;;;;;;;;;;; Inspect the forms
          ;; (format t "~&~a ~s" $node (node-type node))
          ;; (format t "~&~a" (node-string $node))
          ;; TODO we currently have no way to match #+, #-, etc.
          (cond
            ((or (sharp-feature-node-p node)
                 (sharp-feature-not-node-p node))
             ;; skip the #+ and #- node as well as the next node
             ;; 🤔 what if there's a "#+f #+g x" ?
             (next $node)
             (breeze.pattern::skip $node #'whitespace-or-comment-node-p)
             (next $node))
            ((or (sharp-eval-node-p node)
                 ;; e.g. bordeaux-threads's version.sexp
                 (string-node-p node))
             ;; skip!
             (next $node))
            (t
             (let (($node (copy-iterator $node)))
               (let ((bindings (match *top-level-patterns*
                                 $node
                                 :skipp #'whitespace-or-comment-node-p)))
                 (if bindings
                     (dolist (from '(?macro-name ?unknown-top-level-symbol))
                       (let (($node (binding->$node bindings from))
                             (node (binding->node bindings from)))
                         (when node
                           (let* ((symbol-name (name node))
                                  (package-name (breeze.buffer:current-package
                                                 buffer (start node)))
                                  (ref (list
                                        $node
                                        buffer
                                        (and package-name
                                             (node-string-designator package-name))
                                        symbol-name)))
                             (if (or
                                  (alexandria:starts-with-subseq "DEF" symbol-name)
                                  (alexandria:starts-with-subseq "WITH" symbol-name))
                                 (if (eq from '?macro-name)
                                     (pushnew ref defs :test 'equal :key 'rest)
                                     (pushnew ref uses :test 'equal :key 'rest))
                                 (pushnew `(,@ref ,from) others :test 'equal :key 'rest))))))
                     (break "Unrecognized top-level: ~%~s~%~s~%~s"
                            buffer
                            (node-string $node)
                            type)))))))
        buffer)))
   *test-workspace*)
  ;; Save everything
  (setf *results*
        `((:defs . ,defs)
          (:uses . ,uses)
          (:others . ,others)
          (:custom-dispatch-macros . ,custom-dispatch-macros))))



;;;; SBCL sources

;; this is where I cloned sbcl
(merge-pathnames "dev/git/sbcl/" (user-homedir-pathname))
