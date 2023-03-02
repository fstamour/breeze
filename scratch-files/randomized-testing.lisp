#|
Proof of concept to add facilities for randomized testing in parachute.
See https://github.com/Shinmera/parachute/issues/18
|#


;;; Who needs a system definition file anyway?

(cl:in-package #:cl-user)

(ql:quickload '(parachute random-state))


;;; Some more boilerplate

(defpackage #:randomized-testing
  (:documentation "Scratch file to explore randomized tests with parachute")
  (:use #:cl)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false)
  (:import-from #:parachute
                #:eval-in-context
                #:*context*
                #:*parent*
                #:comparison-result
                #:status))

(in-package #:randomized-testing)


;;; First things first, what's the current state of affair?

(defun buggy (x)
  "Like cl:identity, but returns the wrong value for 2."
  (if (= 2 x) 3 x))

(defun make-fake-prng ()
  (let ((x 0))
    (lambda ()
      (incf x))))

(define-test+run buggy
  (let ((prng (make-fake-prng)))
    (loop
      :repeat 10 ; we run the randomized test 10 times
      :for x = (funcall prng)
      :do (is = x (buggy x)))))

#|
？ BREEZE.RANDOMIZED-TESTING::BUGGY
0.000 ✔   (is = x (buggy x))
0.000 ✘   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))         ; These are not useful
0.000 ✔   (is = x (buggy x))         ; They are not descriptive either
0.000 ✔   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))
0.000 ✔   (is = x (buggy x))
0.004 ✘ BREEZE.RANDOMIZED-TESTING::BUGGY

;; Summary:
Passed:     9                        ;  Should we count them differently?
Failed:     1
Skipped:    0

;; Failures:
1/  10 tests failed in BREEZE.RANDOMIZED-TESTING::BUGGY  ; What about this count?
The test form   (buggy x)            ; For more complex tests, this would not be
evaluated to    3                    ; enough to know which inputs failed the test
when            2
was expected to be equal under =.

#<PARACHUTE:PLAIN 11, FAILED results>
((IS = X (BUGGY X)))
|#




;; Variant with a description
(define-test+run buggy
  (let ((prng (make-fake-prng)))
    (loop
      :repeat 10                 ; we run the randomized test 10 times
      :for x = (funcall prng)
      :do (is = x (buggy x) "Failed with input ~a" x))))
#|
？ RANDOMIZED-TESTING::BUGGY
  0.000 ✔   (is = x (buggy x))
  0.000 ✘   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.000 ✔   (is = x (buggy x))
  0.004 ✘ RANDOMIZED-TESTING::BUGGY

;; Summary:
Passed:     9
Failed:     1
Skipped:    0

;; Failures:
   1/  10 tests failed in RANDOMIZED-TESTING::BUGGY
The test form   (buggy x)
evaluated to    3
when            2
was expected to be equal under =.
Failed with input 2

#<PARACHUTE:PLAIN 11, FAILED results>
((IS = X (BUGGY X)))
|#



;;; Another variant without using the `is` tester macros

;; expanded the macro `is`
(define-test+run buggy
  (let ((prng (make-fake-prng)))
    (loop
      :repeat 10 ; we run the randomized test 10 times
      :for x = (funcall prng)
      :do (eval-in-context *context*
                 (make-instance 'comparison-result
                                :expression '(is = x (buggy x))
                                :value-form '(buggy x)
                                :body (lambda () (buggy x))
                                :expected x
                                :comparison '=)))))

;; Manually improved the expanded macro `is`
(define-test+run buggy
  (let ((prng (make-fake-prng)))
    (loop
      :repeat 10 ; we run the randomized test 10 times
      :for x = (funcall prng)
      :do (eval-in-context *context*
                 (make-instance 'comparison-result
                                :expression `(is = x (buggy ,x))
                                :value-form `(buggy ,x)
                                :body (lambda () (buggy x))
                                :expected x
                                :comparison '=)))))

#|
？ RANDOMIZED-TESTING::BUGGY
  0.000 ✔   (is = x (buggy 1))
  0.000 ✘   (is = x (buggy 2))
  0.000 ✔   (is = x (buggy 3))
  0.000 ✔   (is = x (buggy 4))
  0.000 ✔   (is = x (buggy 5))
  0.000 ✔   (is = x (buggy 6))
  0.000 ✔   (is = x (buggy 7))
  0.000 ✔   (is = x (buggy 8))
  0.000 ✔   (is = x (buggy 9))
  0.000 ✔   (is = x (buggy 10))
  0.004 ✘ RANDOMIZED-TESTING::BUGGY

;; Summary:
Passed:     9
Failed:     1
Skipped:    0

;; Failures:
   1/  10 tests failed in RANDOMIZED-TESTING::BUGGY
The test form   (buggy 2)
evaluated to    3
when            2
was expected to be equal under =.

#<PARACHUTE:PLAIN 11, FAILED results>
((IS = X (BUGGY 2)))
|#


;;; One proof-of-concept where we add special syntax to the the `is` macro

(defun gen-integer (prng)
  (funcall prng))

;; Imagine we modify `is`, instead of creating a new macro `is*`
(defmacro is* (comp expected form &optional description &rest format-args)
  (print (if (eq (car form) 'check)
             (destructuring-bind (bindings body)
                 (rest form)
               (let ((vars (mapcar #'car bindings)))
                 `(loop :repeat 10
                        :with prng = (make-fake-prng)
                        ,@(loop :for (var generator) :in bindings
                                :append `(:for ,var := (funcall ,generator prng)))
                        :do
                           (eval-in-context
                            *context*
                            (make-instance 'comparison-result
                                           :expression '(is ,comp ,expected ,body)
                                           :value-form `(let
                                                            ,(list ,@(loop :for var :in vars
                                                                           :collect ``(,',var ,,var)))
                                                          ,',body)
                                           :body (lambda () ,body)
                                           :expected ,expected
                                           :comparison ,(parachute::maybe-quote comp)
                                           ,@(when description
                                               `(:description (format NIL ,description ,@format-args))))))))
             ;; Fallback to normal behaviour
             `(is ,comp ,expected ,form ,description ,@format-args))))

(define-test+run buggy
  (is* = x (check ((x #'gen-integer))
                  (buggy x))))

;;; Conclusions:
;;;  - poor editor support, it has no idea what `check` is
;;;  - the backtick nesting gave me an aneurism
;;;  - can't nest easily... or at all?


;;; Second proof-of-concep: Custom context and result
;;;
;;; Insight: we almost certainly want to stop at the first failure
;;; This makes everything much simpler (reporting, shrinking, etc.)
;;;
;;; It went well, so I added more features, like being able to specify
;;; the seed, the number of iterations, and how the generator is
;;; created.
;;;
;;; It uses random-state as a prng, for reproducibility.

(defclass randomized-context ()
  ())

(defmethod parachute::add-result (result (context randomized-context)))

(defmethod parachute:eval-in-context ((context randomized-context) result)
  (multiple-value-prog1
      (call-next-method)
    (ecase (status result)
      (:failed (setf (result *parent*) result
                     (status *parent*) :failed)
       (signal 'randomized-test-failure))
      (:passed (incf (number-of-successful-tests *parent*)))
      (:skip (incf (number-of-skipped-tests *parent*))))))




(define-condition randomized-test-failure ()
  ())


(defclass randomized-result (parachute:result)
  ((seed :initarg :seed :accessor seed)
   (number-of-skipped-tests :accessor number-of-skipped-tests :initform 0)
   (number-of-successful-tests :accessor number-of-successful-tests :initform 0)
   ;; When a failure occur:
   (result :accessor result)
   (generated-values :accessor generated-values)))

(defmethod parachute::add-result (result (parent-result randomized-result)))


(defmethod parachute:format-result ((result randomized-result) (type (eql :extensive)))
  (if (result result)
      (format NIL "Randomized test failed after ~D passed and ~D skipped tests.~
                   ~&Seed: ~S~
                   ~&With ~{~&  ~A = ~S~}~
                   ~&~A"
              (number-of-successful-tests result)
              (number-of-skipped-tests result)
              (seed result)
              (generated-values result)
              (parachute:format-result (result result) :extensive))
      (format NIL "~D randomized tests passed and ~D skipped tests"
              (number-of-successful-tests result)
              (number-of-skipped-tests result))))

;; TODO print seed
(defmethod parachute:format-result ((result randomized-result) (type (eql :oneline)))
  (if (result result)
      (format NIL "~S ~D passed ~D skipped"
              (result result)
              (number-of-successful-tests result)
              (number-of-skipped-tests result))
      (format NIL "~D passed ~D skipped"
              (number-of-successful-tests result)
              (number-of-skipped-tests result))))




(defmacro randomize (options-and-bindings &body body)
  (let* ((prng-var (gensym "PRNG-VAR"))
         (result (gensym "RESULT"))
         (bindings (remove-if #'keywordp options-and-bindings :key #'car))
         (generators (loop :for (var generator) :in bindings :collect `(,(gensym  (symbol-name var)) ,generator)))
         (options (remove-if-not #'keywordp options-and-bindings :key #'car))
         ;; Options:
         (seed '(random-state:hopefully-sufficiently-random-seed))
         (prng '(random-state:make-generator :pcg seed))
         (times 10))
    ;; Option parsing
    (loop :for option :in options
          :do (alexandria:destructuring-ecase option
                ((:seed user-provided-seed)
                 (setf seed user-provided-seed))
                ((:prng user-provided-prng)
                 (setf prng user-provided-prng))
                ((:times user-provided-times)
                 (setf times user-provided-times))))
    (alexandria:once-only (seed)
      `(let* (,@generators
              (,prng-var
                ;; Wrap random-state, to set the seed, and always generate the same size of fixnums
                (let* ((seed ,seed)
                                (generator ,prng))
                           (lambda ()
                             ;; TODO Save the generated values
                             (random-state:random-int generator most-negative-fixnum most-positive-fixnum))))
              (,result (make-instance 'randomized-result
                                      :expression 'randomize
                                      :seed ,seed))
              (parachute:*parent* ,result)
              (parachute:*context* (make-instance 'randomized-context)))
         (loop
           :repeat ,times
           ,@(loop :for (var) :in bindings
                   :for (gensym) :in generators
                   :append `(:for ,var := (funcall ,gensym
                                                   ,prng-var)))
           :do (handler-case
                   (progn ,@body)
                 (randomized-test-failure (condition)
                   (declare (ignore condition))
                   (setf (generated-values ,result)
                         (list ,@(loop :for (var) :in bindings
                                       :append `(',var ,var))))
                   (return)))
           :finally (setf (status ,result) :success))))))


(defun gen-integer (&optional
                      (min most-negative-fixnum)
                      (max most-positive-fixnum))
  ;; TODO Check min max are within most-pos/neg-fixnum
  ;; TODO Check max > min
  ;; TODO type annotations
  (let ((range (- max min)))
    (lambda (prng)
      (+ min (mod (funcall prng) range)))))



(define-test+run buggy
  (randomize (;; (:seed 32)
              ;; (:prng (random-state:make-generator :pcg seed))
              (x (gen-integer 0 3)))
    (is = x (identity x))
    (is = x (buggy x))))

#|
Conclusions:
 - implementation is much simpler, as we don't have to modify any of
   the existing tester macros, reports or results.
 - can probably be nested easily (didn't try yet)
 - support multiple tests _inside_
 - the report is nice(r)

Concerns:
 - How does it currently behave when nested?
 - Same when nesting define-test, group,
 - the user might think that they can use randomize anywhere e.g.
   (true (randomize ...)), we should probably handle that. At least
   give a nice error or warning if we don't figure out how could that
   work
 - someone might want to _not_ stop at the first error
 - timeouts?
 - do we really want to depend on random-state?
 - what if we want to generate a value based on a previous one?
 - filtering generated values is somewhat trivial, but the loop needs
   to be updated to count the actual number of test run.
     - there should be a condition if it fails to generate values (e.g.
       it generated 10 000 values, but no tests were run because the
       values were filtered out)
 - I'm not familiar with all of parachute's features (like the fixtures),
   I don't know if this interacts well with everything.

Next:
 - bug: currently if the test pass, it's not reported at all -_-
 - print a form that can be copy-pasted to reproduce the issue
 - try using the randomized-result as the context too
 - the syntax for the extra options is not great, the code to handle
   the options is horrible
 - split the randomize macro in parts
 - explore using an object to represent the generators
 - better nesting support
 - save the generated values for later shrinking

Also
 - be able to provide hard-coded values?
 - would it make sense to provide different probability distributions (e.g. bias)?
   - yes, you don't necessarily want to generate huge arrays all the time
   - maybe just make sure to provide a max size (this should actually help with shrinking)
 - be able to disable shriking
 - more utilities like `gen-integer`
 - documentation
 - model-based testing
 - would be nice to provide a way to log extra information _only for failures_
 - DSL to generate complex data (e.g. trees)
 - what I call "generators" are often called "arbitraries"
 - provide an "approximately equal" comparator, for floats

|#


#|

Examples of generators (just brainstorming, it might not all make sense):

- boolean (t or nil)
- integer; positive, negative, with or w/o zero; uint16...
- float/double
- fractions
- complex
- chars
- array/vector/string
- cons/list/alist/plist
- hash-table
- circular structures
- symbols/keyword?
- make-instances / struct
- lambdas (functions)
- permutations
- combinations (one-of/take-n)
- types
- streams
- json
- conditions
- alternate (e.g. _either_ integer or boolean)
- date, datetime

|#


#|

Examples of functions to tests (as is to add to parachutes's tests
- identity/broken-identity
- sort/broken-sort
- reverse/...
- add
- concatenate
- format something...
- subseq
- search
- uax/anything unicode-related
- some arithmetic (e.g. trigger a division by 0)
- left-pad
- collatz

|#


#|

Examples of pre-made property checks
- idempotency:   applying a second time doesn't change the result
- commutativity and associativity
- anti-commutativity and noncommutativity (that's a mouthful)
- distributivity
- de morgan's
- inverse; e.g f is the inverse of g, or f is its own inverse
- identity; e.g. f doesn't change its value
- https://en.wikipedia.org/wiki/Category:Properties_of_binary_operations

|#


;;; Model-based testing

;; TODO class model
;; models could have invariant assertions on themselves (e.g. if you
;; model a list with just the number of element, then that number
;; should never be negative)

;; TODO class assertion
;; use it for pre/post conditions and for invariants
;; TODO method to easily apply a list of assertions
;;  - check-invariants
;;  - every-assertions (like check, but doesn't signal)
;; TODO something about class-invariant
;; TODO Would loop invariants and variants be too crazy?

(defclass action ()
  ((name :initarg :name :initform (error "name must be supplied") :accessor name)
   ;; TODO find a better name than "run"
   (run :initarg :run :initform (error "run must be supplied") :accessor run)
   ;; TODO rename to precondition
   (runp :initarg :runp :initform (constantly t) :accessor runp)
   ;; TODO add postcondition
   ))

;; TODO methods
;; - applicablep
;; - format-action

(defun make-action (name run &optional (runp (constantly t)))
  (make-instance 'action
                 :name name
                 :run run
                 :runp runp))

;; TODO (defmacro define-actions ...)

;; - actions should probably be an array
;; - TODO use (remove-if (applicablep ...) ...)
;; - This could be a method
;; - TODO The user might want to store some state in the model, they
;;   should be able to decide if an actions is applicable base on the
;;   model too.
(defun choose-action (actions #|TODO model|# thing)
  (let ((number-of-actions (length actions)))
    (loop
      :repeat 1000
      :for choice := (random number-of-actions)
      :for action := (nth choice actions)
      :for applicable = (funcall (runp action) thing)
      :until applicable
      :finally (if applicable
                   (return action)
                   (error "Failed to find an applicable action.")))))

;;; Using a 0-dimention array as a pointer
(defun make-ref (x)
  (make-array nil :initial-element x))

(defun deref (ref)
  (aref ref 0))

(defun (setf deref) (new-value ref)
  (setf (aref ref 0) new-value))

;; TODO the actions and invariants are part of the model
(let ((invariants (list (lambda (model thing)
                          (declare (ignorable model))
                          (= model (length thing)))))
      (actions (list (make-action 'push
                                  (lambda (model-ref thing-ref)
                                    (declare (ignorable model-ref))
                                    (push (random 42) (deref thing-ref))
                                    (incf (deref model-ref))))
                     (make-action 'pop
                                  (lambda (model-ref thing-ref)
                                    (declare (ignorable model-ref))
                                    (decf (deref model-ref))
                                    (let ((value (pop (deref thing-ref))))
                                      (declare (ignorable value))
                                      ;; TODO Test the value
                                      ))
                                  ;; Can only apply "pop" if the list is not empty
                                  (lambda (thing)
                                    (plusp (length thing))))
                     (make-action 'length
                                  (lambda (model-ref thing-ref)
                                    (declare (ignorable model-ref))
                                    (length (deref thing-ref)))))))
  (loop
    :with thing = (make-ref ()) ;; TODO what if we want to generate that thing?
    :with model = (make-ref 0)  ;; TODO what if we want to initialize model based on thing?
    :with log = ()
    :repeat 10
    :for action = (choose-action actions (deref thing))
    :do
       (format *debug-io* "~&action: ~a model: ~a thing: ~a" (name action) (deref model) (deref thing))
       (push action log)
       (finish-output)
       ;; Apply the action
       (handler-case
           (funcall (run action) model thing)
         (error (condition)
           (error "An error was signaled while applying the actions ~S ~a" action condition)))
       ;; Check the invariants
       (unless (every #'(lambda (invariant)
                          (funcall invariant (deref model) (deref thing)))
                      invariants)
         (error "Invariant failed model: ~a thing: ~a" (deref model) (deref thing)))))

(let ((list (make-ref '(b c))))
  (push 'a (deref list))
  (deref list))

#|

Crazy idea: we could prefer to choose actions that increase the code coverage
Like in any optimization problems, it might make sense to add randomness (e.g. not
always chose an action that increase code coverage.
This would probably require a way to undo an action.
OR just start over...

Another idea: add a some "weight" to the actions, an action with a higher weight
would have more chance to be chosen. (very much like in a markov chain)

What about fault injection?
- It could be nice to stop some action while it's executing to see if
the invariants still holds.
- If some lambda are passed, maybe signal a condition from them

What about running several actions at the same time?


Maybe generate the list of commands before applying them? :/

tips: save some kind of trace and assert things on it...
e.g. for a stack, keep the number of push and pop and make sure it
matches the number of element
a.k.a trace-based testing
That could be useful for heavily concurrent code...

Maybe the model should be responsible of keeping the log of actions

What about using the models "in the real thing"?

|#



#|

Examples of functions to tests with MBT:
- parser
- a chat server or client
- list
- stack
- queue
- heap
- hash-table
- file system?
- some kind of event-driven loop
  - e.g. a for character in a game... mario!
- todo app thingy


|#
