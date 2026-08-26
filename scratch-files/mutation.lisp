(defpackage #:breeze.mutation
  (:documentation "Mutation testing proof of concept")
  (:use #:cl))

(in-package #:breeze.mutation)

#|

The plan:
- pick a test at random
- find all the functions called by this test
- try to replace one of the function in the call graph
- run the test

|#


;;; Picking a test

;; parachute::*test-indexes*

(parachute:find-test '#:find-all-enclosing-nodes
                     'breeze.test.node-iterator)
;; => #<PARACHUTE:TEST BREEZE.TEST.NODE-ITERATOR::FIND-ALL-ENCLOSING-NODES>



;;; Call graph

;; (swank-backend:calls-who function-name)

;; smoke test
(defun f (x) x)

(swank-backend:calls-who 'f)
;; => :NOT-IMPLEMENTED
;; -_-

(swank-backend:who-calls 'f)
NIL

(defun g (x) (f x))
(swank-backend:who-calls 'f)
;; When evaluating G:
((G
  (:ERROR
   "Error: DEFINITION-SOURCE of function G did not contain meaningful information.")))
;; when compiling G:
((G
  (:LOCATION
   (:BUFFER-AND-FILE "mutation.lisp"
    "/.../breeze/scratch-files/mutation.lisp")
   (:OFFSET 707 13) (:SNIPPET "(f x)"))))
;; when evaluating G after having compiled it once 🙄
((G
  (:LOCATION
   (:BUFFER-AND-FILE "mutation.lisp"
    "/.../breeze/scratch-files/mutation.lisp")
   (:OFFSET 707 13) (:SNIPPET "(f x)")))
 (G
  (:ERROR
   "Error: DEFINITION-SOURCE of function G did not contain meaningful information.")))

(swank/backend:list-callees 'g)
((F
  (:LOCATION
   (:BUFFER-AND-FILE "mutation.lisp"
    "/.../breeze/scratch-files/mutation.lisp")
   (:OFFSET 596 0) (:SNIPPET "(defun f (x) x)"))))
(swank/backend:list-callers 'f)
((G
  (:LOCATION
   (:BUFFER-AND-FILE "mutation.lisp"
    "/.../breeze/scratch-files/mutation.lisp")
   (:OFFSET 707 0) (:SNIPPET "(defun g (x) (f x))"))))

(defun h (x) (+ (g x) (h x)))

(let ((f2 (lambda (x) (* 2 x))))
  (defun g2 (x)
    (f2 x))
  (swank-backend:who-calls f2))
;; => nil


;;; Back-up plan: instrumentation
;;;
;;; Starting with shadowing `cl:defun' and replacing it with our own
;;; macro.

(symbol-package 'defun)
;; => #<PACKAGE "COMMON-LISP-USER">

(shadow '(defun) *package*)

(symbol-package 'defun)
;; => #<PACKAGE "BREEZE.MUTATION">

(defmacro defun (&whole whole name lambda-list &body body)
  (multiple-value-bind
        (remaining-forms declarations documentation)
      (alexandria:parse-body body :documentation t :whole whole)
    `(cl:defun ,name ,lambda-list
       ,documentation
       ,declarations
       ,@remaining-forms)))

(defun f (x) (+ 3 x))
(f 4)
;; => 7

;; making sure the docstring is handled correctly
(defun f (x)
  "The function f."
  (+ 3 x))
(documentation 'f 'function)
;; => "The function f."


;;; Now we add some actual instrumentation
(defparameter *call-stack* nil)

(defmacro defun (&whole whole name lambda-list &body body)
  (multiple-value-bind
        (remaining-forms declarations documentation)
      (alexandria:parse-body body :documentation t :whole whole)
    `(cl:defun ,name ,lambda-list
       ,documentation
       ,declarations
       (let ((*call-stack* (cons ',name *call-stack*)))
         (progn ,@remaining-forms)))))

;; recompile f and make sure it still works
(defun f (x)
  "hi"
  (+ 3 x))
(f 4)
;; => 7

(defun g (x) (* 2 (f x)))
(g 3)
;; => 12

(defun f (x)
  (break "call-stack: ~s" *call-stack*)
  (+ 3 x))

(g 4)

#|
beautiful:

call-stack: (F G)
   [Condition of type SIMPLE-CONDITION]

Restarts:
 0: [CONTINUE] Return from BREAK.
|#


;;; Now we need to somewhat save that information, to keep it
;;; simple... I'm just going to push the *call-stack* somewhere at the
;;; beginning of a call

(defparameter *all-paths* ())

(defmacro defun (&whole whole name lambda-list &rest body)
  (multiple-value-bind
        (remaining-forms declarations documentation)
      (alexandria:parse-body body :documentation t :whole whole)
    `(cl:defun ,name ,lambda-list
       ,documentation
       ,declarations
       (let ((*call-stack* (cons ',name *call-stack*)))
         (push *call-stack* *all-paths*)
         (progn ,@remaining-forms)))))

(defun f (x) (+ 3 x))
(f 4)
;; => 7

(defun g (x) (* 2 (f x)))
(g 2)
;; => 10

*all-paths*
;; => ((F G) (G) (F))


;;; we obviously need some deduplication...

(setf *all-paths* nil)
;; (defparameter *all-paths* (make-hash-table :test 'equalp))

(defmacro with-call-stack-tracking (() &body body)
  `(let ((*all-paths* (make-hash-table :test 'equalp)))
     (progn ,@body)))

(defmacro defun (&whole whole name lambda-list &rest body)
  (multiple-value-bind
        (remaining-forms declarations documentation)
      (alexandria:parse-body body :documentation t :whole whole)
    `(cl:defun ,name ,lambda-list
       ,documentation
       ,declarations
       (let ((*call-stack* (cons ',name *call-stack*)))
         (when *all-paths*
           (setf (gethash *call-stack* *all-paths*) *call-stack*))
         (progn ,@remaining-forms)))))

;; I'm sure this is all very thread-safe /s

;; recompile f and g again
(progn
  (defun f (x) (+ 3 x))
  (defun g (x) (* 2 (f x))))

(values (g 32) *all-paths*)
;; => 70
;; => NIL
;; as expected

(with-call-stack-tracking ()
  (values (g 32) *all-paths*))
;; => 70
;; => #<HASH-TABLE :TEST EQUALP :COUNT 2 {1234A5EF93}>

(cl:defun dump-all-paths ()
  (alexandria:hash-table-keys *all-paths*))

(with-call-stack-tracking ()
  (g 32)
  (dump-all-paths))
;; => ((F G) (G))

(defun h (x) (f x))
(defun h2 (x) (f (f x)))
(defun h3 (x)
  (+ (f (f x))
     (h (f (* 5 x)))))

(with-call-stack-tracking ()
  (g 32)
  (h 1)
  (h2 10)
  (h3 25)
  (dump-all-paths))
;; => ((F H H3) (H H3) (F H3) (H3) (F H2) (H2) (F H) (H) (F G) (G))


;;; to be able to use this, we'll need to be able to actually instrument things
;;;
;;; I'll borrow a trick from the system `cover': use
;;; `*macroexpand-hook*' to hijack `cl:in-package'

(export 'defun)

(shadow '(in-package) *package*)

(defmacro in-package (name)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (let ((package (find-package ',name)))
       ;; actually do what `in-package' should do:
       (setf *package* package)
       ;; then shadow some imports:
       (shadowing-import
        (set-difference
         ;; The set of symbols we would like to shadow
         '(breeze.mutation:defun)
         ;; unless they already are shadowed
         (package-shadowing-symbols package))
        package)
       ;; return the package
       package)))

(cl:defun make-macroexpand-hook (original-macroexpand-hook)
  (lambda (expander macro-form environment)
    (if
     ;; if it's a (cl:in-package ...) form
     (and (listp macro-form)
             (eq (car macro-form) 'cl:in-package))
        (funcall original-macroexpand-hook
                 ;; then expand breeze.mutation::in-package macro
                 ;; instead of cl:in-package
                 (macro-function 'in-package)
                 `(in-package ,@(cdr macro-form))
                 environment)
        ;; not a cl:in-package, just expand "normally"
        (funcall original-macroexpand-hook expander macro-form environment))))

(defmacro with-instrumentation (() &body body)
  (alexandria:with-gensyms (original-macroexpand-hook)
    `(let ((,original-macroexpand-hook *macroexpand-hook*))
       (let ((*macroexpand-hook* (make-macroexpand-hook ,original-macroexpand-hook)))
         (progn ,@body)))))


(with-instrumentation ()
  (load (asdf:system-relative-pathname
         "breeze"
         "tests/dummy-package.lisp")))
;; => T


(breeze.dummy.test:2x 21)
42

(with-call-stack-tracking ()
  (breeze.dummy.test:2x 21)
  (dump-all-paths))
;; => ((BREEZE.DUMMY.TEST:MUL BREEZE.DUMMY.TEST:|2X|) (BREEZE.DUMMY.TEST:|2X|))


;;; let's try instrumenting something more concrete

(with-instrumentation ()
  (load (asdf:system-relative-pathname
         "breeze"
         "src/utils.lisp")
        :print t))

(with-call-stack-tracking ()
  (parachute:test (find-package 'breeze.test.utils)
    :report 'parachute:largescale)
  (dump-all-paths))

#|
Total:          10
Passed:         10 (100%)
Failed:          0 (  0%)
Skipped:         0 (  0%)

((BREEZE.UTILS:LENGTH>1?) (BREEZE.UTILS:BEFORE-LAST))
|#

;; As expected, this package (breeze.utils) have a very low "direct" coverage.
;; What if we run _all_ the tests?
(with-call-stack-tracking ()
  (breeze.test.main:run-tests)
  (dump-all-paths))

;; => ((BREEZE.UTILS:LENGTH>1?) (BREEZE.UTILS:BEFORE-LAST))
;;
;; same... which means that either there are unused functions in the
;; "utils" package (likely), or that the tests don't cover these (also
;; likely).
;;
;;
;; I could look at the "coverage" data to figure out which of the
;; functions are not called, but there are only 2 that were
;; "covered"... I'll do it "manually"

(loop :for s :being :the :external-symbols :of (find-package '#:breeze.utils)
      :unless (member s '(breeze.utils:length>1? breeze.utils:before-last))
      :collect s)
#|
(BREEZE.UTILS:WALK BREEZE.UTILS:WALK-LIST BREEZE.UTILS:LENGTH=1
 BREEZE.UTILS:PACKAGE-APROPOS BREEZE.UTILS:FIND-VERSION-CONTROL-ROOT
 BREEZE.UTILS:STREAM-SIZE BREEZE.UTILS:WALK-CAR BREEZE.UTILS:NRUN
 BREEZE.UTILS:FIND-ASDF-IN-PARENT-DIRECTORIES BREEZE.UTILS:WITH-COLLECTORS)

Here, mutation testing and coverage info _cannot_ help figuring out if
any of these are unused. What I would need is

|#

(loop :for s :being :the :external-symbols :of (find-package '#:breeze.utils)
      :unless (or (member s '(breeze.utils:length>1? breeze.utils:before-last))
                  (swank-backend:who-calls s))
        :collect s)
#|

Those are the functions that are truly not called by any other piece
of code in breeze:

(BREEZE.UTILS:WALK BREEZE.UTILS:WALK-LIST BREEZE.UTILS:PACKAGE-APROPOS
 BREEZE.UTILS:STREAM-SIZE BREEZE.UTILS:WALK-CAR BREEZE.UTILS:WITH-COLLECTORS)

- walk, walk-list, walk-car, stream-size are indeed obsolete leftovers
  from the very early versions of breeze. they should be removed.
- with-collectors... it's general utility macro. If it's not used, I
  should also get rid of it
- package-apropos is supposed to be usable by the end-user

|#

;; Trying to find if any of these symbols are imported in another
;; package.

(package-used-by-list '#:breeze.utils)
;; (#<PACKAGE "BREEZE.XREF"> #<PACKAGE "BREEZE.TEST.UTILS">)

(loop
  :for p :in (list-all-packages)
  :unless (or (eq p #.(find-package '#:breeze.utils))
              (member p '#.(package-used-by-list '#:breeze.utils)))
    :append (loop :for s :being :the symbols :of p
                  :when (member s '(BREEZE.UTILS:WALK BREEZE.UTILS:WALK-LIST BREEZE.UTILS:PACKAGE-APROPOS
                                    BREEZE.UTILS:STREAM-SIZE BREEZE.UTILS:WALK-CAR BREEZE.UTILS:WITH-COLLECTORS))
                    :collect (list p s)))
;; ((#<PACKAGE "BREEZE.TEST-FILE"> BREEZE.UTILS:WITH-COLLECTORS))

#|

Actually... that's an interesting case: in the package "breeze-utils",
there's also a macro named "with".

In src/test-file.lisp's `breeze.test-file::%read-spec-file', there's a
form that looks like this:

    (with
        (open-file (stream pathname))
        (collectors (test parts))
        ...)

The "collectors" part get expanded to `breeze.utils:with-collectors'.

So runtime information cannot know that... (there's a symbol used by a
function, but the symbol is not present in the function's package.

1. I think instrumenting the macros should cover (ha!) that case: we
would see "there's a macroexpansion of X that happened inside the
macroexpansion of Y"... actually, no need to instrument the macros,
just update the macroexpansion hook.

2. maybe use (swank-backend:who-macroexpands)

|#

(swank-backend:who-macroexpands 'breeze.utils:with-collectors)
#|
((BREEZE.TEST-FILE::%READ-SPEC-FILE
  (:LOCATION
   (:FILE "/.../breeze/src/test-file.lisp")
   (:POSITION 2755)
   (:SNIPPET "(with
      ((open-file (stream pathname))
       (collectors (tests parts))
       (let ((attributes (make-hash-table))
             (eof '#.(gensym \"eof\"))
             (start 0)))
       (labels
           ((peek (&optional (peek-type t))
              "))))
|#


;; I can update the form that I used to list the "unused functions" to
;; properly handle macros
(loop :for s :being :the :external-symbols :of (find-package '#:breeze.utils)
      :unless (or (member s '(breeze.utils:length>1? breeze.utils:before-last))
                  (if (macro-function s)
                      (swank-backend:who-macroexpands s)
                      (swank-backend:who-calls s)))
        :collect s)
#|
(BREEZE.UTILS:WALK BREEZE.UTILS:WALK-LIST BREEZE.UTILS:PACKAGE-APROPOS
 BREEZE.UTILS:STREAM-SIZE BREEZE.UTILS:WALK-CAR)
|#

;;; !!!! I just noticed that I already removed those functions!!! but
;;; they are still part of the defpackage

(defun suspicious-exports (package)
  (loop :for s :being :the :external-symbols of package
        :unless (or
                 (find-class s nil)
                 (boundp s)
                 (fboundp s))
          :collect s))
;; P.S. this is just a heuristic

(suspicious-exports '#:breeze.utils)
;; => (BREEZE.UTILS:WALK BREEZE.UTILS:WALK-LIST BREEZE.UTILS:STREAM-SIZE  BREEZE.UTILS:WALK-CAR)

(loop :for package :in (breeze.dogfood:find-breeze-packages)
      :for sus := (remove-if-not
                   (lambda (symbol)
                     (and (eq package (symbol-package symbol))
                          (not (member symbol
                                       ;; Known false-positives
                                       '(breeze.iterator:positions
                                         breeze.iterator:subtrees
                                         breeze.command:noninteractive
                                         breeze.string:string-designator)))))
                   (suspicious-exports package))
      :when sus
        :do (format t "~&The package ~s exports those symbols that are neither a special variable, a function or a class:~{~%  - ~s ~}"
                    (string-downcase (package-name package))
                    (mapcar 'string-downcase sus)))
#|

Here are the TRUE POSITIVES that this snippet found:
  The 4 symbols we already knew about in breeze.utils
  #:describe-command in breeze.command
    just not implemented
  #:root-node-p in breeze.egraph
    it was removed (IIRC)
  #:notifications in breeze.command
    it's there, but it's commented out (TODO)
  #:symbol-starts-with in breeze.string
    it was moved into breeze.pattern a while ago
  #:after-change-function in breeze.workspace
    it was removed
  #:dot-node-p in breeze.parse-tree
    it was removed
  #:sharp and #:sharp-node-p in breeze.parse-tree
    it's commented out (TODO)
  #:generate-breeze-reference in breeze.dogfood
    it was removed
  #:insert-parachute-define-test
    it was moved into breeze+parachute
  #:top-level-checks in breeze.checks
    it was removed
  #:defpattern in breeze.pattern
    I don't remember that... it's the only occurence
  #:calls-who in breeze.xref
    this is a leftover from a very early version of breeze

Here are some of the FALSE POSITIVES:
  #:positions and #:subtrees in breeze.iterator
    they are slot names
  #:noninteractive in breeze.command
    it's a symbol used as a kind of custom declaration, it's a "witness value"
  #:string-designator in breeze.string
    it's a type

Interestingly, this also brought up symbols that are currently
exported in the runtime, but are not part of the source code anymore.

MGL-PAX could help the situation, as it forces you to say "what kind
of export" each symbols are...

|#


;;; on with the mutation testing

#|

- swank's calls-who is not implemented (for sbcl at least)
- but we can call swank's who-calls on all the exported function of a package.

|#

(loop :for s :being :the :external-symbols :of :breeze.utils
      :when (and (fboundp s)
                 (not (macro-function s)))
        :collect (list s (swank/backend:who-calls s)))
;; TODO I haven't done anything with that yet
