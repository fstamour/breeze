(defpackage #:breeze.html
  (:documentation "Utilities to generate html.")
  (:use #:cl)
  (:import-from #:breeze.string
                #:with-fmt)
  (:export #:escape-html
           #:with-html-page
           #:with-html-file))

(in-package #:breeze.html)

(defun escape-html (string)
  (nth-value 0
      (cl-ppcre:regex-replace-all
       ;; I'm sure this is rock solid /s
       (cl-ppcre:create-scanner "<((?!a |\/a|br).*?)>"
                                :multi-line-mode t)
       string
       "&lt;\\1&gt;")))

(defmacro with-html-page ((stream-var) &body body)
  (let ((fmt (intern "FMT")))
    `(with-fmt (,stream-var)
       (,fmt "<!DOCTYPE html>")
       (,fmt "<html>")
       (,fmt "<meta charset=\"UTF-8\" />")
       ;; https://github.com/emareg/classlesscss
       (,fmt "<link rel=\"stylesheet\" href=\"style.css\" title=\"classless\" />")
       ;; https://github.com/raj457036/attriCSS/tree/master
       (,fmt "<link rel=\"alternate stylesheet\" href=\"brightlight-green.css\" title=attri-css-brightlight-green\" />")
       ,@body
       (,fmt "</html>"))))

(defmacro with-html-file ((stream-var filename) &body body)
  `(alexandria:with-output-to-file
       (,stream-var ,filename :if-exists :supersede)
     (with-html-page (,stream-var)
       ,@body)))
