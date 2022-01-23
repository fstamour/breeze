(defpackage #:breeze.external-symbol-not-found
  (:use #:cl)
  (:local-nicknames (#:esnf #:external-symbol-not-found))
  (:documentation ""))

#+ (or)
(ql:quickload 'external-symbol-not-found)

(in-package #:breeze.external-symbol-not-found)



(define-condition external-symbol-not-found (error)
  ((package
    :reader external-symbol-not-found-package
    :initarg :package
    :documentation "The symbol's package.")
   (symbol
    :reader external-symbol-not-found-symbol
    :initarg :symbol
    :documentation "The name of symbol if it wasn't found. The symbol
    if it's not exported. This sould be 2 different conditions.")
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

(handler-case (progn
                (unless (find-package 'temp)
                  (make-package 'temp :use nil))
                (read-from-string "temp:symbol"))
  (error (e)
    (when (esnf:external-symbol-not-found-p e)
      (error
       (make-condition 'external-symbol-not-found
		       :package (esnf:external-symbol-not-found-package e)
		       :symbol (esnf:external-symbol-not-found-symbol-name e)
		       :condition e)))))

#|
Condition BREEZE.EXTERNAL-SYMBOL-NOT-FOUND::EXTERNAL-SYMBOL-NOT-FOUND was signalled.
   [Condition of type EXTERNAL-SYMBOL-NOT-FOUND]

Restarts:
 0: [RETRY] Retry SLIME interactive evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "worker" RUNNING {101A8CE0C3}>)

Backtrace:
  0: ((LAMBDA ()))
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (HANDLER-CASE (PROGN (UNLESS # #) (READ-FROM-STRING "temp:symbol")) (ERROR (E) (WHEN # #))) #<NULL-LEXENV>)
  2: (EVAL (HANDLER-CASE (PROGN (UNLESS # #) (READ-FROM-STRING "temp:symbol")) (ERROR (E) (WHEN # #))))
  3: ((LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL))
  4: (SWANK::CALL-WITH-RETRY-RESTART "Retry SLIME interactive evaluation request." #<FUNCTION (LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL) {101A88FC7B}>)
  5: (SWANK::CALL-WITH-BUFFER-SYNTAX NIL #<FUNCTION (LAMBDA NIL :IN SWANK:INTERACTIVE-EVAL) {101A88FC5B}>)
  6: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SWANK:INTERACTIVE-EVAL "(handler-case (progn ..)
  7: (EVAL (SWANK:INTERACTIVE-EVAL "(handler-case (progn ..)
  8: (SWANK:EVAL-FOR-EMACS (SWANK:INTERACTIVE-EVAL "(handler-case (progn ..)
  9: ((LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD))
 10: (SWANK/SBCL::CALL-WITH-BREAK-HOOK #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD) {2273525B}>)
 11: ((FLET SWANK/BACKEND:CALL-WITH-DEBUGGER-HOOK :IN "/usr/home/fstamour/.config/stumpwm/slime/swank/sbcl.lisp") #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD..
 12: (SWANK::CALL-WITH-BINDINGS ((*STANDARD-INPUT* . #<SWANK/GRAY::SLIME-INPUT-STREAM {101587D213}>)) #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD) {227354CB}>)
 13: ((LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD))
 14: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
 15: ((FLET "WITHOUT-INTERRUPTS-BODY-11" :IN SB-THREAD::RUN))
 16: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
 17: ((FLET "WITHOUT-INTERRUPTS-BODY-4" :IN SB-THREAD::RUN))
 18: (SB-THREAD::RUN)
 19: ("foreign function: call_into_lisp")
 20: ("foreign function: funcall1")
|#
