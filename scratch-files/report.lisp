(defpackage #:breeze.report
  (:documentation "Using breeze's code to generate report to improve breeze.")
  (:use #:cl)
  (:import-from #:breeze.lossless-reader
                #:parse
                #:tree
                #:node-content
                #:node-start
                #:node-end
                #:node-type
                #:source-substring
                #:comment-node-p
                #:whitespace-node-p
                #:line-comment-node-p))

(in-package #:breeze.report)

#++ ;; this is annoying af...
(setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)


(defun find-all-breeze-systems ()
  "Find all systems defined in breeze.asd."
  (let ((result ())
        (asd-pathname (asdf:system-source-file 'breeze)))
    (asdf:map-systems (lambda (system)
                        ;; TODO Perhaps use asdf:primary-system-name
                        (when (equal asd-pathname
                                     (asdf:system-source-file system))
                          (push system result))))
    result))

#++ (mapcar 'asdf:coerce-name (find-all-breeze-systems))


(defun enough-breeze (pathname)
  "Given a pathname, return the relative pathname from the root of the
breeze project (system)."
  (uiop:enough-pathname
   pathname
   (asdf:system-source-directory 'breeze)))

(defun remove-leading-semicolons (string)
  "Remove leading semicolons (e.g. from line comments)."
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "^[ ;]+"
                            :multi-line-mode t)
   string ""))

#++
(remove-leading-semicolons "; ; ; ")
;; => ""




(defun paragraphs (string)
  (cl-ppcre:split
   (cl-ppcre:create-scanner "\\n\\n+"
                            :multi-line-mode t
                            :single-line-mode t)
   string))

#+example
(paragraphs
 (format nil "asd~5%qwe~%ert~2%jkl"))






(defun page-node-p (node)
  "Is the node (from the lossless parser) a new-page (^L) character?"
  (eq 'breeze.lossless-reader::page (node-type node)))


(defun pages (state)
  "Split a parse-tree by top-level new-page character (^L)"
  (loop
    :with pages = nil
    :with page = nil
    :for node :in
              (tree state)
    :when (page-node-p node)
      :do (push (nreverse page) pages)
          (setf page nil)
    :do (push node page)
    :finally
       (when page
         (push (nreverse page) pages))
       (return (nreverse pages))))

;; TODO rename to parse-system-file, maybe (probably)?
(defun parse-system (&optional (system 'breeze))
  "Parse (with lossless-reader) all files we want to include."
  ;; TODO include files from other systems in this project.
  ;; TODO include all files that are tracked under git...
  (loop
    :for file :in (breeze.asdf:system-files
                   system
                   :include-asd (asdf:primary-system-p system))
    :for filename = (enough-breeze file)
    :for content-str = (alexandria:read-file-into-string file)
    :for state = (parse content-str)
    :collect (list filename state (pages state))))

#++
(parse-system)



;; TODO move to utils; add tests...
(defun nrun (list predicate)
  "If the first element of LIST satisfies PREDICATE, destructively
extract the first run of elements that satisfies PREDICATE. Returns
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

;; (defun render-line-comments (nodes))


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
          (paragraphs comment)))

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
          (whitespace-node-p node))
         (t
          ;; TODO I should escape STRING
          (format out "~%<pre>~a</pre>" (node-content state node))))))

(defmacro with-html-file ((stream-var filename) &body body)
  `(alexandria:with-output-to-file (,stream-var
                                    (breeze.utils:breeze-relative-pathname ,filename)
                                    :if-exists :supersede)
     (labels ((fmt (&rest rest)
                (apply #'format out rest)))
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
  (with-html-file (system out pathname)
    ;; TODO "back to listings"
    (fmt "<ol>")
    (loop
      :for (filename state pages) :in files
      :do
         (fmt "<li>")
         (fmt "~a" (link-to-file filename))
         (when (breeze.utils:length>1? pages)
           (fmt "<ol>")
           (loop
             :for page :in pages
             :for i :from 1
             :for page-title = (let ((node (page-title-node page)))
                                 (when node
                                   (breeze.utils:summarize
                                    (remove-leading-semicolons (node-content state node)))))
             :do (fmt "<li>~a</li>" (link-to-page filename i page-title)))
           (fmt "</ol>"))
         (fmt "</li>"))
    (fmt "</ol>")
    (loop
      :for (filename state pages) :in files
      :for number-of-pages = (length pages)
      :do
         (fmt "<h2 id=\"~a~:*\">~a</h2>~%" filename)
         (loop
           :for page :in pages
           :for i :from 1
           :do
              (when (> number-of-pages 1)
                (fmt "<h3 id=\"~a\">Page ~d</h3>" (page-id filename i) i))
              (render-page out state page))))
  pathname)

#++
(render 'breeze)

#++
(mapcar 'render (find-all-breeze-systems))

(defun listings.html ()
  (with-html-file (out (breeze.utils:breeze-relative-pathname "docs/listings.html"))
    (fmt "<ol>")
    (loop
      :for system :in (find-all-breeze-systems)
      :for name = (asdf:coerce-name system)
      :for file = (file-namestring (system-listing-pathname system))
      :do (fmt "<li><a href=\"~a\">~a</a></li>" file name))
    (fmt "</ol>")))

#|

FIXME I originally named this "report" because I wanted
something "holistic", but now I started calling this "listing", which
is not holistic.

TODO In the same vein... I would like to have _all_ the listings in
the same file (currently 1 file per system). I want this because it
would be easier to convert to something else afterwards.

TODO I _could_ generate objects instead of directly generating
html... that way it _could_ be possible to generate something else
than html.

TODO Nice to haves: line numbers

|#
