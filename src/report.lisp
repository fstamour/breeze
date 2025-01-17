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
  (:use #:cl #:breeze.lossless-reader))

(in-package #:breeze.report)

#++
(ql:quickload "cl-ppcre")

#++ ;; this is annoying af...
(setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)


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

#++;; TODO Make a test
(paragraphs
 (format nil "asd~5%qwe~%ert~2%jkl"))



(defun remove-leading-semicolons (string)
  "Remove leading semicolons (e.g. from line comments)."
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "^[ ;]+"
                            :multi-line-mode t)
   string ""))

#++
(remove-leading-semicolons "; ; ; ")
;; => ""


(defun escape-html (string)
  (cl-ppcre:regex-replace-all
   ;; I'm sure this is rock solid /s
   (cl-ppcre:create-scanner "<((?!a |\/a|br).*?)>"
                            :multi-line-mode t)
   string
   "&lt;\\1&gt;"))

#++
(progn
  (escape-html "<br>")
  (escape-html "<a>")
  (escape-html "<a href=\"\"></a>")
  (escape-html "=> (#<ASDF/SYSTEM:SYSTEM \"breeze/test\"> #<ASDF/SYSTEM:SYSTEM \"breeze/config\">)"))



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



(defun enough-breeze (pathname)
  "Given a pathname, return the relative pathname from the root of the
breeze project (system)."
  (uiop:enough-pathname
   pathname
   (asdf:system-source-directory 'breeze)))


(defun parse-system (&optional (system 'breeze))
  "Parse (with lossless-reader) all files we want to include."
  ;; TODO include all files that are tracked under git...
  (loop
    :for file :in (sort (breeze.asdf:find-all-related-files system)
                        #'string<
                        :key #'namestring)
    :for filename = (enough-breeze file)
    :for content-str = (alexandria:read-file-into-string file)
    :for state = (progn
                   (format *trace-output* "~&Parsing file ~s..." file)
                   (parse content-str))
    :collect (list filename state (pages state))))

#++
(parse-system)



;; TODO move to utils; add tests...
(defun nrun (list predicate)
  "Destructively extract the first run of elements that satisfies
PREDICATE, if the first element of LIST satisfies PREDICATE, . Returns
the run and update LIST's first cons to point to the last element of
the run."
  (when (and list
             ;; I didn't want to think about the case where there is
             ;; no first run.
             (funcall predicate (first list)))
    (loop
      ;; keep track of the last "head of list", so we can 1: return
      ;; the last element for which PREDICATE was true and 2: set its
      ;; cdr to nil to make the list end there.
      :for last-cons = nil :then cons
      ;; iterate cons by cons
      :for cons :on list
      ;; extract the next element to check
      :for el = (first cons)
      ;; does the new element pass the test?
      :while (funcall predicate el)
      ;; once we find an element that doesn't pass the test, we
      ;; <strong>update</strong> LIST so that the first element is now
      ;; the <em>last</em> element that did pass the test. That way
      ;; the caller can easily access the first and last element of
      ;; the run; and it happens that it plays very well with (loop
      ;; :for :on), because that construct would skip the next
      ;; element...
      :finally
         (let (;; Copy the original first cons of the list
               (run (cons (car list) (cdr list))))
           (setf
            ;; sever the link between the run and the rest of the list
            (cdr last-cons) nil
            ;; update the first element of the list
            (car list) (car last-cons)
            ;; update the rest of the list
            (cdr list) cons)
           (return run)))))

#++ ;; Demonstrate that nrun modifies the list
(let ((list (copy-seq '(1 3 4 5))))
  (values (nrun list #'oddp)
          list))
;; => (1 3), (3 4 5)



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

(defun escaped-node-content (state node)
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

(defmacro with-html-file ((stream-var filename) &body body)
  `(alexandria:with-output-to-file (,stream-var
                                    (breeze.utils:breeze-relative-pathname ,filename)
                                    :if-exists :supersede)
     (labels ((fmt (&rest rest)
                (apply #'format out rest)))
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

;; I made this into a macro mainly to reduce the indentation... meh
(defmacro with-system-listing ((system stream-var filename) &body body)
  `(with-html-file (,stream-var ,filename)
     (let ((files (parse-system ,system)))
       (fmt "<html>")
       ,@body
       (fmt "</html>"))))

(defun link-to-id (name id)
  (format nil "<a href=\"#~a\">~a</a>" id name))

(defun file-id (filename)
  filename)

(defun link-to-file (filename)
  (link-to-id (file-id filename) filename))

;; (link-to-file "asdf")

(defun page-id (filename page-number)
  (format nil "~a-~d" filename page-number))

(defun link-to-page (filename page-number &optional name)
  (link-to-id
   ;; that's an em-dash
   (or name
       (format nil "~a &#8212; page ~d" filename page-number))
       (page-id filename page-number)))

;; (link-to-page "asdf" 42)

(defun system-listing-pathname (system)
  (format nil "docs/listing-~a.html"
          (cl-ppcre:regex-replace-all "/" (asdf:coerce-name system) "--")))



(defun render (system &aux (pathname (system-listing-pathname system)))
  (format *debug-io* "~&Rendering listing for system ~s" system)
  (with-system-listing (system out pathname)
    ;; Table of content
    (fmt "<ol>")
    (loop
      :for (filename state pages) :in files
      :do
         (fmt "<li>")
         (fmt "~a" (link-to-file filename))
         (progn ;;when (breeze.utils:length>1? pages)
           (fmt "<ol>")
           (loop
             :for page :in pages
             :for i :from 1
             :for page-title = (let ((node (page-title-node page)))
                                 (when node
                                   (escape-html
                                    (remove-leading-semicolons (node-content state node)))))
             :do (fmt "<li>~a</li>" (link-to-page filename i page-title)))
           (fmt "</ol>"))
         (fmt "</li>"))
    (fmt "</ol>")
    ;; The actual content
    (loop
      :for (filename state pages) :in files
      :for number-of-pages = (length pages)
      :do
         (fmt "<h2 id=\"~a~:*\">~a</h2>~%" filename)
         (loop
           :for page :in pages
           :for i :from 1
           :do
              (if (> number-of-pages 1)
                  (fmt "<hr id=\"~a\"></h3>" (page-id filename i))
                  (fmt "<div id=\"~a\"></div>" (page-id filename i)))
              (render-page out state page))))
  pathname)

#++
(render 'breeze)
