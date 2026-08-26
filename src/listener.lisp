(in-package #:common-lisp-user)

(defpackage #:breeze.listener
  (:documentation "RPC, REPL and more.")
  (:use :cl #:breeze.analysis #:breeze.command)
  (:import-from #:alexandria
                #:when-let*
                #:when-let
                #:symbolicate #| used in suggestions.lisp |#)
  (:import-from #:breeze.xref
                #:classp
                #:function-designator-p)
  (:import-from #:breeze.string
                #:optimal-string-alignment-distance*)
  (:import-from #:breeze.buffer
                #:current-package
                #:point)
  (:import-from #:breeze.command-utils
                #:pulse-node)
  (:export
   #:rpc-eval
   #:interactive-eval-string
   #:interactive-eval)
  (:export
   #:compose-hook
   #:*interactive-eval-hooks*
   #:*interactive-eval-after-hooks*))

(in-package #:breeze.listener)



(defun rpc-eval (string)
  "Low-level eval used as a kind of RPC. We need this to avoid issue from
differences between swank and slynk."
  (let ((*package* (find-package "CL-USER"))
        (*readtable* (copy-readtable nil)))
    (eval (read-from-string string))))


(defparameter *interactive-eval-after-hooks* '())

;; TODO Use with-timeout

(defun format-values-for-echo-area (values)
  (let ((*print-readably* nil))
    (cond ((null values) "; No value")
          ((and (integerp (car values)) (null (cdr values)))
           (let ((i (car values)))
             (format nil "~D (~a bit~:p, #x~X, #o~O, #b~B)"
                     i (integer-length i) i i i)))
          ((and (typep (car values) 'ratio)
                (null (cdr values))
                (ignore-errors
                 ;; The ratio may be too large to be represented as
                 ;; a single float
                 (format nil "~D (~:*~f)"

                         (car values)))))
          (t (format nil "~{~S~^, ~}" values)))))


#|
TODO When trying to evaluate a defmethod that changes the lambda-list, it would be nice to have a restart to force it...

Here's what the condition looks like in sbcl:

attempt to add the method
  #<STANDARD-METHOD (#<STANDARD-CLASS BREEZE.BUFFER:BUFFER>) {1211C4D993}>
to the generic function
  #<STANDARD-GENERIC-FUNCTION BREEZE.BUFFER:CURRENT-PACKAGE (1)>;
 but the method has more optional arguments than the generic function.
   [Condition of type SB-INT:SIMPLE-PROGRAM-ERROR]



#<SB-INT:SIMPLE-PROGRAM-ERROR {1211CB2653}>
--------------------
The object is a CONDITION of type SB-INT:SIMPLE-PROGRAM-ERROR.
FORMAT-CONTROL: "~@<attempt to add the method~2I~_~S~I~_to the generic function~2I~_~S;~I~_ but ~?~:>"
FORMAT-ARGUMENTS: @1=(#<STANDARD-METHOD (#<STANDARD-CLASS BREEZE.BUFFER:BUFFER>) {1211C4D993}> #<STANDARD-GENERIC-FUNCTION BREEZE.BUFFER:CURRENT-PACKAGE (1)> "the method has ~A optional arguments than the generic function." ("more"))
|#

(defparameter *interactive-eval-hooks*
  ;; TODO add a hook to add a restart to disable the hook that invoked
  ;; the debugger
  '(call-with-package-variance-restarts
    call-with-correction-suggestion))


;; TODO add a hook that adds a message "did you forget to specify the format's stream?"
;;
;; TODO add a type of condition that wraps another condition and adds
;; more context to it when printed.
;;
;; (format "oups" 'x)
;; "oups" is not a string with a fill-pointer

#|

TODO

../../../quicklisp/local-projects/breeze/src/top-levelness.lisp:1:1:
  warning:
    BREEZE.TOP-LEVELNESS also uses the following packages:
      (BREEZE.PARSE-STATE BREEZE.PARSE-TREE)
    See also:
      Common Lisp Hyperspec, DEFPACKAGE [:macro]
      SBCL Manual, *ON-PACKAGE-VARIANCE* [:variable]
    ==>
      (SB-IMPL::%DEFPACKAGE "BREEZE.TOP-LEVELNESS" 'NIL 'NIL 'NIL 'NIL
                            '("CL" "BREEZE.ITERATOR" "BREEZE.PARSER")
                            '(("BREEZE.PATTERN" "LET-MATCH")) 'NIL
                            '("TOP-LEVEL-NODE-ITERATOR" "TOP-LEVEL-FORMS"
                              "TOP-LEVEL-NODE-P" "MAP-TOP-LEVEL-FORMS")
                            '("BREEZE.TOP-LEVELNESS") 'NIL ...)


Compilation failed.

|#

(defun call-with-package-variance-restarts (thunk)
  (handler-case
      (funcall thunk)
    #+sbcl
    (sb-int:package-at-variance (c)
      (unless #1=(get 'call-with-package-variance-restarts 'disabled)
              (ignore-errors
               (let* ((package-name (car (simple-condition-format-arguments c)))
                      (package (find-package package-name))
                      (symbols (cadr (simple-condition-format-arguments c))))
                 ;; TODO add a restart to change sbcl's *on-package-variance*
                 (restart-case
                     (invoke-debugger c)
                   (continue ()
                     :report (lambda (stream)
                               (format stream "Continue with the extra exported symbols.")))
                   (disable ()
                     :report (lambda (stream)
                               (format stream "Disable the hook `call-with-package-variance-restarts' for this session."))
                     (setf *interactive-eval-hooks*
                           (remove 'call-with-package-variance-restarts *interactive-eval-hooks*)))
                   (unexport ()
                     :report (lambda (stream)
                               (format stream "Unexport ~{~s~^ ~} from ~s"
                                       symbols package))
                     ;; TODO maybe one might want to **unintern** some
                     ;; of the symbols too
                     (mapcar (lambda (s)
                               (unexport s package))
                             symbols)
                     (message "The symbols ~{~s~^ ~} where unexported from the package ~s"
                              symbols package)))))))))

(defun compose-hooks (hooks)
  ;; clear as mud
  (reduce (lambda (f a)
             (lambda (c)
               (funcall f (lambda ()
                            (funcall a c)))))
          hooks))

(defun interactive-eval-string (string)
  ;; TODO maybe keep an history (pushnew string *recent-forms* :test #'string=)
  ;; TODO add a restart to retry
  (funcall
   (compose-hooks *interactive-eval-hooks*)
   (lambda ()
     (prog1
         ;; &optional package readtable
         ;; TODO infer the *readtable*
         (let ((*readtable* (copy-readtable nil))
               (values (multiple-value-list
                        (eval
                         (read-from-string string)))))
           (message "~a" (format-values-for-echo-area values)))
       (run-interactive-eval-after-hooks string)))))

(defun run-interactive-eval-after-hooks (string)
  (loop
    :for (name . hook) :in *interactive-eval-after-hooks*
    :do
       (handler-case
           (funcall hook string)
         (error (condition)
           (format *error-output*
                   "~&Error signaled while running \"~a\" interactive-eval hook: ~a~%  "
                   name
                   condition)))))

;; TODO eval-node
;;  that would be useful to implement a command for updating tests

(defparameter *interactive-eval-last-context* ()
  "For debugging only.")

#++
(defparameter *current-node* nil
  "The node iterator currently being evaluated.")

;; 2025-06-12 it finally works!
(define-command interactive-eval ()
  "A command to interactively evaluate code."
  (let* ((context (context*))
         (buffer (current-buffer context))
         ($node (node-iterator buffer))
         (*package* (or (current-package buffer)
                        *package*)))
    ;; TODO if the form is a comment
    (when (whitespace-or-comment-node-p $node)
      ;; TODO find the closest node to evaluate
      ;; TODO check if "firstp" and/or "lastp"
      (when-let ((previous (previous-sibling $node
                                             #| TODO add :skipp to previous-sibling |#)))
        #++ (break "point: ~s   end previous: ~s"
               (point buffer) (end previous))
        #++
        (when (= (point buffer) (end previous))
          (decf (pos $node)))
        (decf (pos $node))))
    (setf *interactive-eval-last-context*
          (list :context context
                :*package* *package*))
    (when-let* (($node (root $node))
                (node (value $node)))
      (pulse-node node)
      (let ((string (node-string $node)))
        (setf *interactive-eval-last-context*
              (list :context context
                    :*package* *package*
                    :string string))
        (interactive-eval-string (node-string $node))))))
