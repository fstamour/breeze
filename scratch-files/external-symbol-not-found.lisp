(defpackage #:breeze.external-symbol-not-found
  (:use #:cl)
  (:local-nicknames (#:esnf #:external-symbol-not-found))
  (:documentation ""))

#+ (or)
(ql:quickload 'external-symbol-not-found)

(in-package #:breeze.external-symbol-not-found)



(define-condition external-symbol-not-found (simple-error)
  ((package
    :reader external-symbol-not-found-package
    :initarg :package
    :documentation "The symbol's package.")
   (symbol
    :reader external-symbol-not-found-symbol
    :initarg :symbol
    :documentation "The name of symbol if it wasn't found. The symbol
    if it's not exported. This should be 2 different conditions.")
   (condition
    :reader external-symbol-not-found-condition
    :initarg :condition
    :documentation "The implementation condition.")))


(progn
  (unless (find-package 'temp)
    (make-package 'temp :use nil))
  (read-from-string "temp:symbol"))

#|
Symbol "SYMBOL" not found in the TEMP package.

  Line: 1, Column: 10, File-Position: 10

  Stream: #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}>
   [Condition of type SB-INT:SIMPLE-READER-PACKAGE-ERROR]

Restarts:
 0: [CONTINUE] Use symbol anyway.
 1: [RETRY] Retry SLIME interactive evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "worker" RUNNING {101A86C2A3}>)

Backtrace:
  0: (SB-IMPL::READ-TOKEN #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}> #\t)
  1: (SB-IMPL::READ-MAYBE-NOTHING #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}> #\t)
  2: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}> T (NIL) T)
  3: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}> T (NIL) NIL)
  4: (READ #<SB-IMPL::STRING-INPUT-STREAM {101A8AB153}> T NIL NIL)
  5: (SB-IMPL::%READ-FROM-STRING/SAFE "temp:symbol" T NIL 0 NIL NIL)
  6: (SB-INT:SIMPLE-EVAL-IN-LEXENV (READ-FROM-STRING "temp:symbol") #<NULL-LEXENV>)
  7: (SB-INT:SIMPLE-EVAL-IN-LEXENV (PROGN (UNLESS (FIND-PACKAGE #) (MAKE-PACKAGE # :USE NIL)) (READ-FROM-STRING "temp:symbol")) #<NULL-LEXENV>)
  8: (EVAL (PROGN (UNLESS (FIND-PACKAGE #) (MAKE-PACKAGE # :USE NIL)) (READ-FROM-STRING "temp:symbol")))
  9: ((LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL))
 --more--
|#

(handler-bind
    ((error
       #'(lambda (e)
	   (when (esnf:external-symbol-not-found-p e)
	     (let ((new-condition
	      (make-condition 'external-symbol-not-found
			      :package (esnf:external-symbol-not-found-package e)
			      :symbol (esnf:external-symbol-not-found-symbol-name e)
			      :condition e
			      :format-control (simple-condition-format-control e)
			      :format-arguments (simple-condition-format-arguments e))))
	       (error new-condition))))))
  (progn
    (unless (find-package 'temp)
      (make-package 'temp :use nil))
    (read-from-string "temp:symbol")))


#|
Symbol "SYMBOL" not found in the TEMP package.
   [Condition of type EXTERNAL-SYMBOL-NOT-FOUND]

Restarts:
 0: [RETRY] Retry SLIME interactive evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "worker" RUNNING {101CC92193}>)

Backtrace:
  0: (SB-IMPL::READ-TOKEN #<SB-IMPL::STRING-INPUT-STREAM {804DFF173}> #\t)
  1: (SB-IMPL::READ-MAYBE-NOTHING #<SB-IMPL::STRING-INPUT-STREAM {804DFF173}> #\t)
  2: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {804DFF173}> T (NIL) T)
  3: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {804DFF173}> T (NIL) NIL)
  4: (READ #<SB-IMPL::STRING-INPUT-STREAM {804DFF173}> T NIL NIL)
  5: (SB-IMPL::%READ-FROM-STRING "temp:symbol" T NIL 0 NIL NIL)
  6: ((LAMBDA ()))
  7: (SB-INT:SIMPLE-EVAL-IN-LEXENV (HANDLER-BIND ((ERROR #)) (PROGN (UNLESS # #) (READ-FROM-STRING "temp:symbol"))) #<NULL-LEXENV>)
  8: (EVAL (HANDLER-BIND ((ERROR #)) (PROGN (UNLESS # #) (READ-FROM-STRING "temp:symbol"))))
  9: ((LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL))
 --more--
|#
