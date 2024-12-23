
(cl:in-package #:cl-user)

(defpackage #:breeze.test.lossless-reader
  (:documentation "Test package for #:breeze.lossless-reader")
  (:use #:cl #:breeze.lossless-reader)
  ;; Importing non-exported symbols
  (:import-from #:breeze.lossless-reader
                #:*state-control-string*
                #:state-context
                #:%nodes
                #:read-sharpsign-backslash
                #:read-sharpsign-quote
                #:read-sharpsign-left-parens
                #:read-sharpsign-asterisk
                #:read-sharpsign-colon
                #:read-sharpsign-dot
                #:read-sharpsign-b
                #:read-sharpsign-o
                #:read-sharpsign-x
                #:read-sharpsign-r
                #:read-sharpsign-c
                #:read-sharpsign-a
                #:read-sharpsign-s
                #:read-sharpsign-p
                #:read-sharpsign-equal
                #:read-sharpsign-sharpsign
                #:read-sharpsign-plus
                #:read-sharpsign-minus
                #:%token-symbol-node)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type)
  (:import-from #:breeze.kite
                #:is-equalp*
                #:is-equalp))

(in-package #:breeze.test.lossless-reader)

(defvar *test-strings* (make-hash-table :test 'equal))

(defun register-test-string (string &optional (origin (list t)) &aux (ht *test-strings*))
  (let ((old-origin (gethash string ht)))
    (unless (or (and (equal '(t) origin)
                     (member t old-origin))
                (and old-origin
                     (equal origin (alexandria:lastcar old-origin))))
      (setf (gethash string ht)
            (append old-origin (alexandria:ensure-list origin)))))
  string)



;; Add _some_ randomized test strings
(defun wrap-in-parens (s) (format nil "(~a)" s))
(defun wrap-in-block-comment (s) (format nil "#|~a|#" s))
(defun prefix-with-line-comment (s) (format nil ";~a" s))
;; reverse

(defun randomize-1 (fn s origin)
  (unless (equal (alexandria:lastcar origin) fn)
    (funcall fn s)))

(defun randomize-test-strings (&aux (before (hash-table-count *test-strings*)))
  (loop
    :for randomizer :in (list
                         'reverse
                         'wrap-in-parens
                         'wrap-in-block-comment
                         'prefix-with-line-comment)
    :do
       (loop
         :for s :being :the :hash-key :of (alexandria:copy-hash-table *test-strings*)
           :using (hash-value origin)
         :for r = (randomize-1 randomizer s origin)
         :when r
           :do (register-test-string r randomizer)))
  (format nil "~d new test strings generated (was ~d, now ~d)"
          (- (hash-table-count *test-strings*) before)
          before (hash-table-count *test-strings*)))

#++
(randomize-test-strings)

#++
(maphash (lambda (k v)
           (unless (equal '(t) v)
             (remhash k *test-strings*)))
         *test-strings*)

#++
(remove-duplicates
 (loop
   :for s :being :the :hash-key :of *test-strings*
     :using (hash-value origin)
   :collect (alexandria:lastcar origin)))

#++
(remove-duplicates
 (alexandria:hash-table-values *test-strings*)
 :test 'equal)

#++
(alexandria:hash-table-keys *test-strings*)

(define-test+run parse-randomized
  (parachute:finish
   (loop :for input :being :the :hash-key :of *test-strings*
         :do (restart-case
                 (parse input)
               (continue ()
                 :report (lambda (stream)
                           (format stream "Continue to the next test.")))
               (remove-and-continue ()
                 :report (lambda (stream)
                           (format stream "Remove ~s from *test-strings* and continue to the next test." string))
                 (remhash string *test-strings*))))))

(defmacro ∀ ((n &rest vars) &body body)
  (if vars
      (alexandria:once-only (n)
        `(loop :for ,(first vars) :below ,n :do (∀ (,n ,@(rest vars)) ,@body)))
      `(progn ,@body)))

;; Generate all possible strings of 1 charcater
;; Generate all ASCII strings of length 2 and 3
#++
(define-test+run parse-exhaustive
  (parachute:finish
   (loop :for i :below char-code-limit
         :do (parse (string (code-char i)))))
  (parachute:finish
   (∀ (256 x y)
     (parse (map 'string 'code-char (list x y)))))
  (let (input message)
    (parachute:finish
     (∀ (128 x y z)
       (setf
        ;; message (format nil "Failed with input x: ~s y: ~s z: ~s" x y z)
        input (map 'string 'code-char (list x y z)))
       (parse input))
     ;; "~a" message
     )))

#++
(∀ (128 x y z)
  (map 'string 'code-char (list x y z)))



; file: /home/fstamour/dev/breeze/tests/lossless-reader.randomized.lisp
; in:
;      PARACHUTE:DEFINE-TEST+RUN BREEZE.TEST.LOSSLESS-READER::PARSE-RANDOMIZED
;     (REMHASH STRING BREEZE.TEST.LOSSLESS-READER::*TEST-STRINGS*)
;
; caught WARNING:
;   undefined variable: COMMON-LISP:STRING

;     (FORMAT STREAM "Remove ~s from *test-strings* and continue to the next test."
;             STRING)
;
; caught WARNING:
;   undefined variable: COMMON-LISP:STRING
;
; compilation unit finished
;   Undefined variable:
;     STRING
;   caught 2 WARNING conditions
;   caught 16 STYLE-WARNING conditions
;   printed 1 note
