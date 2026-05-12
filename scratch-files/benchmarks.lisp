;; (ql:quickload "breeze/test")

(in-package #:breeze.test.workspace)

(sb-profile:profile
 lint-every-file-in-breeze
 breeze.lint:lint-buffer
 breeze.lint:analyse
 breeze.diagnostics:diag-node)

#++
(sb-profile:unprofile
 breeze.iterator:next
 breeze.iterator:next-preorder)

(sb-profile:profile
 breeze.parser:make-node-iterator
 breeze.parser:node-string-designator
 breeze.parser:parse-symbol-node
 breeze.parser:token-node-p
 breeze.iterator:value
 breeze.iterator:donep
 breeze.iterator:tree-iterator-value
 breeze.iterator:tree-iterator-done-p)

(sb-profile:profile
 breeze.pattern:match
 breeze.pattern::skip
 breeze.analysis::match-symbol
 breeze.analysis::match-package
 breeze.analysis::match-parser-state
 breeze.analysis::match-qualification
 breeze.analysis:quotedp
 breeze.analysis::quotedness)

(sb-profile:profile breeze.pattern:match :methods t)

(sb-profile:reset)
(time (loop :repeat 6 :do (time (analyse-every-file-in-breeze))))
(time (loop :repeat 6 :do (time (lint-every-file-in-breeze))))
;; it looks like `buffer.lint:lint-buffer' doesn't add much overhead on top of `buffer.lint:analyse'
(sb-profile:report :print-no-call-list nil)

(sb-profile:unprofile)

#|

Total
0.292s  671M cycles  37MB consed
0.334s  769M cycles  37MB consed
0.321s  739M cycles  37MB consed

So around 1/3 seconds...


Which ones took longer?

breeze/src/cl.lisp                0.025s  58M cycles  882kB consed
breeze/src/parser/parse-tree.lisp 0.030s  14M cycles  1.5MB consed
breeze/src/parser/parser.lisp     0.035s  81M cycles  1.4MB consed
breeze/src/egraph.lisp            0.045s  98M cycles  1.3MB consed
breeze/src/command.lisp           0.025s  57M cycles  1.4MB consed


|#


(require :sb-sprof)

(sb-sprof:reset)

(sb-sprof:with-profiling
    #| 1000 samples, 0.01s in between => 10 seconds |#
    (:max-samples 1000
     :sample-interval 0.01 #| 0.01 is the default |#
     :report :flat
     :loop t
     :mode :time #| (or :cpu :alloc :time |#
     :threads (list (bt:current-thread)))
  (lint-every-file-in-breeze))


(sb-sprof:report :type :graph)





(time (goto-all-positions))
