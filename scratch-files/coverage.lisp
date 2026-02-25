;; https://lispcookbook.github.io/cl-cookbook/testing.html#code-coverage

(cl:in-package #:cl-user)

(defpackage #:breeze.coverage
  (:documentation "")
  (:use #:cl))

(in-package #:breeze.coverage)

(require '#:sb-cover)

;; 1. turn on instrumentation at compile-time
(declaim (optimize sb-cover:store-coverage-data))

;; 2. load the code

;; 3. run the code (e.g. a test suite)

;; 4. generate a coverage report
(sb-cover:report "coverage/")

;; 5. turn off the instrumentation
(declaim (optimize (sb-cover:store-coverage-data 0)))
