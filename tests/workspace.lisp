(defpackage #:breeze.test.workspace
  (:documentation "Test package for #:breeze.workspace")
  (:use #:cl #:breeze.workspace)
  (:import-from #:breeze.indirection
                #:with-simple-indirections)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.workspace)



(define-test infer-project-name
  (false
   (with-simple-indirections
       ((breeze.utils:find-version-control-root))
     (infer-project-name "some path"))
   "infer-project-name should return nil if the version control root directory was not found")
  (is string= "foobar"
      (with-simple-indirections
          ((breeze.utils:find-version-control-root
            #p"/home/nobody/projects/foobar/"))
        (infer-project-name "some path"))
      "infer-project-name should return the name of the version control root directory when it is found"))

(defun goto-all-positions (#| name |# content node-iterator)
  (progn
    ;; (breeze.logging:log-info "Going through ~s (~s characters)" name (length content))
    (loop
      :with length = (length content)
      :for i :below length
      ;; :for percent = (/ i length)
      :do
         ;; (breeze.logging:log-info "Going through ~s: ~D/~D" name i length)
         (breeze.lossless-reader:goto-position node-iterator i))))

#|

TODO: I know it's far from a performant implementation, but this is
pretty bad... it conses waaayyy too much...

INFO Going through "src/lossless-reader.lisp" (48044 characters)
Evaluation took:
  4.495 seconds of real time
  4.494639 seconds of total run time (4.491079 user, 0.003560 system)
  [ Real times consist of 0.008 seconds GC time, and 4.487 seconds non-GC time. ]
  [ Run times consist of 0.005 seconds GC time, and 4.490 seconds non-GC time. ]
  100.00% CPU
  15,702,211,735 processor cycles
  473,133,760 bytes consed

Made it much worse -_-

INFO Going through "src/lossless-reader.lisp" (48756 characters)
Evaluation took:
  24.695 seconds of real time
  24.693697 seconds of total run time (11.422253 user, 13.271444 system)
  [ Real times consist of 0.004 seconds GC time, and 24.691 seconds non-GC time. ]
  [ Run times consist of 0.005 seconds GC time, and 24.689 seconds non-GC time. ]
  100.00% CPU
  86,267,524,595 processor cycles
  702,386,496 bytes consed

# Profiling.

## With the "deterministic profiler"

Evaluation took:
  23.591 seconds of real time
  23.591976 seconds of total run time (10.807726 user, 12.784250 system)
  [ Real times consist of 0.004 seconds GC time, and 23.587 seconds non-GC time. ]
  [ Run times consist of 0.004 seconds GC time, and 23.588 seconds non-GC time. ]
  100.00% CPU
  1 form interpreted
  16 lambdas converted
  82,428,998,400 processor cycles
  704,487,520 bytes consed

  seconds  |     gc     |    consed   |    calls   |  sec/call  |  name
--------------------------------------------------------------
     1.435 |      0.000 | 281,764,624 | 17,608,824 |   0.000000 | DONEP
     1.377 |      0.005 | 419,160,112 |  8,730,900 |   0.000000 | NEXT
     1.019 |      0.000 |           0 |  8,779,656 |   0.000000 | VALUE
     0.947 |      0.000 |           0 |     48,756 |   0.000019 | GOTO-POSITION
     0.000 |      0.000 |           0 |          1 |   0.000103 | MAKE-RECURSIVE-ITERATOR
--------------------------------------------------------------
     4.778 |      0.005 | 700,924,736 | 35,168,137 |            | Total

estimated total profiling overhead: 16.89 seconds
overhead estimation parameters:
  6.866e-9s/call, 4.80316e-7s total profiling, 2.6407e-7s internal profiling

These functions were not called:
 COLLECT BREEZE.ITERATOR::CURRENT-DEPTH-DONE-P
 BREEZE.ITERATOR::MAYBE-DIG-IN BREEZE.ITERATOR::MAYBE-DIG-OUT
 RECURSE-INTO

without the profiler:

INFO Going through "src/lossless-reader.lisp" (48756 characters)
Evaluation took:
  3.955 seconds of real time
  3.955378 seconds of total run time (3.955378 user, 0.000000 system)
  [ Run times consist of 0.002 seconds GC time, and 3.954 seconds non-GC time. ]
  100.00% CPU
  13,815,993,205 processor cycles
  702,367,376 bytes consed

So, it looks indeed like the profiler added over 17 seconds of overhead


## With the "statistical profiler"


|#

#++
(require 'sb-sprof)

;; sb-sprof:*max-samples*

#++
(time
 (let* ((filename "src/lossless-reader.lisp")
        (content (alexandria:read-file-into-string
                  (breeze.utils:breeze-relative-pathname filename)))
        (node-iterator (breeze.lossless-reader:make-node-iterator content)))
   (sb-sprof:with-profiling (:max-samples (1- (expt 2 31))
                             :reset t
                             ;; :mode :alloc
                             :mode :cpu
                             ;; :report :flat
                             :report :graph
                             )
     (loop :repeat 10 :do
                   (goto-all-positions #| filename |# content node-iterator)))))


#++
(let* ((filename "src/lossless-reader.lisp")
       (content (alexandria:read-file-into-string
                 (breeze.utils:breeze-relative-pathname filename)))
       (node-iterator (breeze.lossless-reader:make-node-iterator content)))
  (time
   (goto-all-positions #| filename |# content node-iterator)))


#|

This is magical ✨

INFO Going through "src/lossless-reader.lisp" (48819 characters)
Evaluation took:
  3.379 seconds of real time
  3.381958 seconds of total run time (3.381958 user, 0.000000 system)
  100.09% CPU
  11,814,836,460 processor cycles
  0 bytes consed

Final result (this was done on a newer sbcl, but a much slower
computer (10+ years old laptop instead of a recent breefy desktop):
Evaluation took:
  0.048 seconds of real time
  0.049120 seconds of total run time (0.049120 user, 0.000000 system)
  102.08% CPU
  117,609,216 processor cycles
  0 bytes consed

Final result, but for parsing and "going through" _all_ the files:

Evaluation took:
  0.359 seconds of real time
  0.341030 seconds of total run time (0.340919 user, 0.000111 system)
  94.99% CPU
  2 forms interpreted
  37 lambdas converted
  864,359,514 processor cycles
  7,822,160 bytes consed


|#

(define-test+run add-to-workspace
  (finish
   (let ((*workspace* (make-instance 'workspace)))
     (loop
       :with root = (breeze.utils:breeze-relative-pathname "")
       :for file :in (breeze.asdf:system-files 'breeze)
       :for name = (enough-namestring file root)
       :for content = (alexandria:read-file-into-string file)
       :for buffer = (add-to-workspace `( :buffer-name ,name
                                          :buffer-string ,content
                                          :point 1))
       :for node-iterator = (breeze.lossless-reader:node-iterator buffer)
       ;; :when (string= name "src/egraph.lisp")
       :do
          (finish
           ;; (breeze.logging:log-info "Going through ~s (~s characters)" name (length content))
           (progn ;; time
            (goto-all-positions content node-iterator))
           "Should be able to \"goto\" every positions in ~s" name))
     *workspace*)))
