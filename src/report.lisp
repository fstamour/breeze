#|

FIXME I originally named this "report" because I wanted
something "holistic", but now I started calling this "listing", which
is not holistic.

TODO I _could_ generate objects instead of directly generating
html... that way it _could_ be possible to generate something else
than html.

TODO Nice to haves: line numbers

TODO Render file-comment ";;;;" differently
TODO Render comments differently based on the number of ";"

TODO Split comment/paragraphs when the line starts with "TODO"

|#


(defpackage #:breeze.report
  (:documentation "Using breeze's code to generate report to improve breeze.")
  (:use #:cl #:breeze.lossless-reader)
  (:import-from #:breeze.utils
                #:nrun)
  (:export #:report
           #:slug
           #:url-to
           #:pathname-to)
  (:export #:paragraphs
           #:remove-leading-semicolons
           #:escape-html)
  (:export #:link-to-id
           #:link-to-file
           #:link-to-page))

(in-package #:breeze.report)


;;; Report object and core methods

(defclass report ()
  ((output-dir
    :initform (error "OUTPUT-DIR is required.")
    :initarg :output-dir
    :accessor output-dir
    :documentation "The path to the directory where the report's files will be created."))
  (:documentation "A report object is used to carry around configuration and state. Methods
can be specialised on this class for further customization."))

(defmethod slug (report thing)
  "Get an url-safe version of THING."
  (with-output-to-string (o)
    (loop
      :with string = (string-trim '(#\Space #\Tab #\Newline)
                                  (string thing))
      :for c :across string
      :do (cond
            ((or (alphanumericp c) (position c "-_."))
             (write-char (char-downcase c) o))
            ((char= #\space c) (write-char #\- o))
            ((char= #\/ c) (write-string "--" o))
            (t (format o "~(~16r~)" (char-code c)))))))

(defmethod url-to (report thing prefix)
  (format nil "~@[~a-~]~a"
          (and prefix (slug report prefix))
          (slug report thing)))

(defmethod url-to (report thing (prefix (eql :listing)))
  (format nil "~@[~a-~]~a.html"
          (and prefix (slug report prefix))
          (slug report thing)))

(defmethod pathname-to (report thing prefix)
  (merge-pathnames
   (url-to report thing prefix)
   (output-dir report)))


;;; The rest

(defun paragraphs (string)
  "Split a string in \"paragraphs\" (a bit like markdown does, where two
newlines or more marks the start of a new paragraph)."
  ;; TODO What if it starts with newlines? or if it's only newlines; I
  ;; should probably use string-trim before split.
  (cl-ppcre:split
   (cl-ppcre:create-scanner "\\n\\n+"
                            :multi-line-mode t
                            :single-line-mode t)
   string))



(defun remove-leading-semicolons (string)
  "Remove leading semicolons (e.g. from line comments)."
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "^[ ;]+"
                            :multi-line-mode t)
   string ""))

(defun escape-html (string)
  (nth-value 0
      (cl-ppcre:regex-replace-all
       ;; I'm sure this is rock solid /s
       (cl-ppcre:create-scanner "<((?!a |\/a|br).*?)>"
                                :multi-line-mode t)
       string
       "&lt;\\1&gt;")))



(defun page-node-p (node)
  "Is the node (from the lossless parser) a new-page (^L) character?"
  (eq 'breeze.lossless-reader::page (node-type node)))

(defun pages (state)
  "Split a parse-tree by top-level new-page character (^L)"
  (loop
    :with pages = nil
    :with page = nil
    :for node :across
              (tree state)
    :when (and page (page-node-p node))
      :do (push (nreverse page) pages)
          (setf page nil)
    :do (push node page)
    :finally
       (when page
         (push (nreverse page) pages))
       (return (nreverse pages))))



;; TODO in some systems, the *.asd file is not at the root of the project, perhaps usethe vc root?
(defun system-enough-pathname (pathname system)
  "Given a pathname, return the relative pathname from the root of the project (system)."
  (uiop:enough-pathname
   pathname
   (asdf:system-source-directory system)))


(defun parse-system (system)
  "Parse (with lossless-reader) all files we want to include."
  ;; TODO include all files that are tracked under git...
  (loop
    :for file :in (sort (breeze.asdf:find-all-related-files system)
                        #'string<
                        :key #'namestring)
    :for filename = (system-enough-pathname file system)
    :for content-str = (alexandria:read-file-into-string file)
    :for state = (progn
                   (format *trace-output* "~&Parsing file ~s..." file)
                   (parse content-str))
    :collect (list filename state (pages state))))

#++
(parse-system 'breeze)



(defun line-comment-or-ws (node)
  (and node
       (or (line-comment-node-p node) (whitespace-node-p node))))

(defun group-line-comments (nodes)
  (let* ((run (nrun nodes #'line-comment-or-ws))
         (start (node-start (first run)))
         (end (node-end (first nodes))))
    (values start end)))

#++
(let ((node-list (tree (parse (format nil "; c~%  (+ 2 2) #| |#")))))
  (group-line-comments node-list))

(defun page-title-node (page)
  "Try to infer the page's title. (Reminder: page is a list of node)"
  (loop
    :for node :in (if (page-node-p (first page)) ; skip the page node
                      (rest page)
                      page)
    :when (line-comment-node-p node)
      :do (return node)
    :while (whitespace-node-p node)))

(defun render-line-comment (out comment)
  (format out "~{<p>~a</p>~%~}"
          (paragraphs (escape-html comment))))

#++
(defun render-node (out state node)
  (format out "~a"
          (escape-html
           (node-content state node))))


;; This assumes the packages are loaded in the current image!
(defun cl-token-p (string)
  (multiple-value-bind
        (value error)
      (ignore-errors (read-from-string string))
    (and (not (typep error 'error))
         (eq #.(find-package "CL")
             (symbol-package value)))))

(defun token-style (state node)
  (if (valid-node-p node)
      (let ((content (node-content state node)))
        (cond
          ((char= #\: (char content 0)) 'keyword)
          ((numberp (ignore-errors (read-from-string content))) 'number)
          ((alexandria:starts-with-subseq "check-" content) 'special)
          ((position #\: content) 'symbol)
          ((cl-token-p content) 'symbol)))
      'syntaxerror))

(defun render-escaped (out string)

  (write-string (escape-html string) out))

;; TODO use a node-iterator instead, maybe
(defun escaped-node-content (state node)
  "Return the content of NODE as an HTML-escaped string"
  (escape-html (node-content state node)))

(defun render-node (out state node &optional (depth 0))
  (case (node-type node)
    (string
     (format out "<span class=\"string\">~a</span>"
             (escaped-node-content state node)))
    (token
     (alexandria:if-let ((style (token-style state node)))
       (format out "<span class=\"~(~a~)\">~a</span>"
               (token-style state node)
               (node-content state node))
       (render-escaped out (node-content state node))))
    (parens
     (format out "<span class=\"~:[syntaxerror ~;~]paren~d\">(<span class=\"progn\">"
             (valid-node-p node)
             (min (1+ depth) 6))
     (map nil (lambda (node)
                (render-node out state node (1+ depth)))
          (node-children node))
     (format out "</span>)</span>"))
    (t (format out "<span class=\"~a\">~a</span>"
               (string-downcase (node-type node))
               (escaped-node-content state node)))))

(defun render-page (out state page)
  "Render 1 page as html, where PAGE is a list of nodes."
  (loop
    :for nodes = page :then (rest nodes)
    :for node = (first nodes)
    :while node
    :do
       (cond
         (;; Group line comments
          (line-comment-node-p node)
          (multiple-value-bind (start end)
              (group-line-comments nodes)
            (render-line-comment out (remove-leading-semicolons
                                      (source-substring state start end)))))
         (;; don't print whitespace nodes
          (or (whitespace-node-p node) (page-node-p node)))
         (t
          (format out "~%<pre><code>")
          (render-node out state node)
          (format out "~%</code></pre>")))))

(defmacro with-html ((stream-var) &body body)
  `(labels ((fmt (&rest rest)
              (apply #'format ,stream-var rest)))
     ,@body))

(defmacro with-html-file ((stream-var filename) &body body)
  `(alexandria:with-output-to-file (,stream-var
                                    (breeze.utils:breeze-relative-pathname ,filename)
                                    :if-exists :supersede)
     (with-html (,stream-var)
       (fmt "<!DOCTYPE html>")
       (fmt "<html>")
       ;; https://github.com/emareg/classlesscss
       (fmt "<link rel=\"stylesheet\" href=\"style.css\" title=\"classless\" >")
       ;; https://github.com/raj457036/attriCSS/tree/master
       (fmt "<link rel=\"alternate stylesheet\" href=\"brightlight-green.css\" title=attri-css-brightlight-green\" >")
       (fmt "<link rel=\"alternate stylesheet\" href=\"https://unpkg.com/normalize.css\" title=\"concrete\" >")
       (fmt "<link rel=\"alternate stylesheet\" href=\"https://unpkg.com/concrete.css\" title=\"concrete\">")
       ,@body
       (fmt "</html>"))))

(defmethod link-to-id (report name id &optional (path ""))
  (format nil "<a href=\"~a#~a\">~a</a>" path id name))

(defmethod link-to-file (report filename)
  (format nil "<a href=\"~a\">~a</a>"
          (url-to report (namestring filename) :listing)
          filename))

;; (link-to-file nil "asdf")

(defmethod page-id (report filename page-number)
  (format nil "~a-~d" filename page-number))

(defun link-to-page (report filename page-number &optional name)
  (link-to-id
   report
   (or name
       ;; &#8212; is an em-dash
       (format nil "~a &#8212; untitled page ~d" filename page-number))
   (page-id report filename page-number)
   (url-to report (namestring filename) :listing)))

;; (link-to-page nil "asdf.lisp" 42)

(defun system-listing-pathname (report system)
  "Get the path to SYSTEM's generated listings."
  (pathname-to report system :listing))

(defun render-system-toc (report out files)
  ;; Table of content
  (with-html (out)
    (fmt "<ol>")
    (loop
      :for (filename state pages) :in files
      :do
         (fmt "<li>")
         (fmt "~a" (link-to-file report (namestring filename)))
         (progn ;; TODO when (breeze.utils:length>1? pages)
           (fmt "<ol>")
           (loop
             :for page :in pages
             :for i :from 1
             :for page-title = (let ((node (page-title-node page)))
                                 (when node
                                   (escape-html
                                    (remove-leading-semicolons (node-content state node)))))
             :do (fmt "<li>~a</li>" (link-to-page report filename i page-title)))
           (fmt "</ol>"))
         (fmt "</li>"))
    (fmt "</ol>")))

(defun render-lisp-file (report out file)
  (with-html (out)
    (destructuring-bind (filename state pages)
        file
      (let ((number-of-pages (length pages)))
        ;; TODO not h2
        (fmt "<h2 id=\"~a~:*\">~a</h2>~%" filename)
        (loop
          :for page :in pages
          :for i :from 1
          :do
             (if (> number-of-pages 1)
                 (fmt "<hr id=\"~a\"></h3>" (page-id report filename i))
                 (fmt "<div id=\"~a\"></div>" (page-id report filename i)))
             (render-page out state page))))))

(defun render (report system &aux (pathname (system-listing-pathname report (asdf:coerce-name system))))
  (format *debug-io* "~&Rendering listing for system ~s" system)
  (let ((files (parse-system system)))
    (with-html-file (out pathname)
      (fmt "<html>")
      ;; TODO sort files differently
      (render-system-toc report out files)
      (fmt "</html>"))
    (loop
      :for file :in files
      :for (filename state pages) = file
      :for listing-filename = (pathname-to report (namestring filename) :listing)
      :do
         (format *trace-output* "~&Writing ~a..."
                 (namestring listing-filename))
         (with-html-file (out listing-filename)
           (render-lisp-file report out file))))
  pathname)

#++
(render
 (make-instance 'report
                :output-dir (breeze.utils:breeze-relative-pathname "docs/"))
 'breeze)
