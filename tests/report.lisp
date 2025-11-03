(defpackage #:breeze.test.report
  (:documentation "Tests for the package breeze.report")
  (:use #:cl #:breeze.report)
  ;; importing non-exported symbols, for testing
  (:import-from #:breeze.report
                #:render-files
                #:pages)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish
                #:fail))

(in-package #:breeze.test.report)

(define-test+run report
  (fail (make-instance 'report))
  (finish (make-instance 'report :output-dir "out/")))

(define-test+run slug
  :dependencies (report)
  (is string= "nil" (slug nil nil))
  (is string= "" (slug nil ""))
  (is string= "" (slug nil "   "))
  (is string= "" (slug nil "	"))
  (is string= "" (slug nil (string #\newline)))
  (is string= "233csmth3e" (slug nil "#<SMTH>"))
  (is string= "a--path--to--some--file.txt" (slug nil "a/path/to/some/file.txt")))

(define-test+run url-to
  :dependencies (slug)
  (is string= "nil" (url-to nil nil nil))
  (is string= "tests--report.lisp"
      (url-to nil "tests/report.lisp" nil))
  (is string= "foo-tests--report.lisp"
      (url-to nil "tests/report.lisp" :foo))
  (is string= "listing-tests--report.lisp.html"
      (url-to nil "tests/report.lisp" :listing)))

(define-test+run pathname-to
  :dependencies (slug)
  (is equalp #P"out/listing-tests--report.lisp.html"
      (pathname-to (make-instance 'report :output-dir "out/")
                   "tests/report.lisp" :listing)))

(define-test+run link-to-id
  (is string= "<a href=\"#life\">42</a>" (link-to-id nil "42" "life")))

(define-test+run link-to-file
  :dependencies (pathname-to)
  (is string=
      "<a href=\"listing-asdf.html\">asdf</a>"
      (link-to-file
       (make-instance 'report :output-dir "out/") "asdf")))

(define-test+run link-to-page
  :dependencies (pathname-to)
  (is string=
      "<a href=\"listing-asdf.html#asdf-42\">asdf &#8212; untitled page 42</a>"
      (link-to-page
       (make-instance 'report :output-dir "out/") "asdf" 42))
  (is string=
      "<a href=\"listing-asdf.html#asdf-42\">Not untitled!</a>"
      (link-to-page
       (make-instance 'report :output-dir "out/") "asdf" 42
       "Not untitled!")))

(define-test+run paragraphs
  (is equalp `("asd" ,(format nil "qwe~%ert") "jkl")
      (paragraphs
       (format nil "asd~5%qwe~%ert~2%jkl"))))

(define-test+run remove-leading-semicolons
  (is string= "" (remove-leading-semicolons "; ; ; ")))

(define-test+run escape-html
  (is string= "<br>" (escape-html "<br>"))
  (is string= "&lt;a&gt;" (escape-html "<a>"))
  (is string= "<a href=\"\"></a>" (escape-html "<a href=\"\"></a>"))
  (is string= "=> (#&lt;ASDF/SYSTEM:SYSTEM \"breeze/test\"&gt; #&lt;ASDF/SYSTEM:SYSTEM \"breeze/config\"&gt;)"
      (escape-html "=> (#<ASDF/SYSTEM:SYSTEM \"breeze/test\"> #<ASDF/SYSTEM:SYSTEM \"breeze/config\">)")))


;;; TODO I want to generate example pages, especially for commands

;; Here I render a buffer with syntax errors into an html file
;; TODO extract function "render-buffer" out of "render-lisp-flle"
#++
(let* ((root (merge-pathnames
              "docs/"
              (asdf:system-source-directory 'breeze)))
       (content "(
  #|
  #r")
       (filename "example.lisp")
       (state (breeze.parser:parse content))
       (pages (pages state)))
  (;; with-output-to-string (out)
   ;; alexandria:with-output-to-file (out (merge-pathnames "example.lisp.html" root))
   progn
    (render-files
     (make-instance 'report :output-dir root)
     (list (list filename state pages))
     (merge-pathnames "example.lisp.html" root))))
