(cl:in-package #:common-lisp-user)

(defpackage #:breeze.refactor.test
  (:use :cl)
  (:import-from #:breeze.test
		#:deftest
		#:is)
  (:import-from #:breeze.reader
		#:node-content
		#:parse-string
		#:unparse-to-string

		;; Types of node
		#:skipped-node
		#:symbol-node
		#:read-eval-node
		#:character-node
		#:list-node
		#:function-node

		;; Type predicates
		#:skipped-node-p
		#:symbol-node-p
		#:read-eval-node-p
		#:character-node-p
		#:list-node-p
		#:function-node-p))

(in-package #:breeze.refactor.test)
