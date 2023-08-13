(defpackage #:breeze.report
  (:documentation "Using breeze's code to generate report to improve breeze.")
  (:use #:cl)
  (:import-from #:breeze.lossless-reader
                #:parse
                #:tree
                #:write-node))

(in-package #:breeze.report)



(alexandria:with-output-to-file (out
                                 (breeze.utils:breeze-relative-pathname "docs/report.html")
                                 :if-exists :supersede)
  (format out "<html>")
  (let ((files (breeze.asdf:system-files 'breeze)))
    (format out "<ol>~{<li>~a</li>~}</ol>" files)
    (loop :for file :in files
          :for content-str = (alexandria:read-file-into-string file)
          :for content = (parse content-str)
          :do
             (format out "<hr>")
             (format out "<h2>~a</h2>"
                     (uiop:enough-pathname
                      file
                      (asdf:system-source-directory 'breeze)))
             (loop :for form :in (tree content)
                   :do (format out "<pre style=\"border-style: outset; width: 80em;\">")
                       (write-node form content out)
                       (format out "</pre>"))))
  (format out "</html>"))
