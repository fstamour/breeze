
(in-package #:common-lisp-user)

(pushnew :hunchentoot-no-ssl *features*)

(ql:quickload 'hunchentoot)

(defpackage #:breeze.documentation-server
  (:use :cl :alexandria :hunchentoot))

(in-package #:breeze.documentation-server)

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))

(start *server*)
;; (stop *server*)

(define-easy-handler (home :uri "/") ()
  (with-output-to-string (spinneret:*html*)
    (breeze.documentation::generate-documentation-to-stream
     spinneret:*html*)))


(defvar *style-dispatcher*
  (create-static-file-dispatcher-and-handler
   ;; uri
   "/style.css"
   ;; path of the file
   (breeze.utils:breeze-relative-pathname "docs/style.css")
   ;; content type
   "text/css"))

(pushnew *style-dispatcher* *dispatch-table*)



(define-easy-handler (tests :uri "/tests") ()
  (with-output-to-string (spinneret:*html*)
    (spinneret:with-html
      (:doctype)
      (:html
       (:head
        (:title "Tests")
        (:link :rel "stylesheet" :href "style.css"))
       (:body
        (:h1 "Tests"
             (loop
               :for package :being :the :hash-key :of (breeze.xref:tests-by-package)
                 :using (hash-value test-definition)
               :do
                  (:h2 (package-name package))
                  (loop :for (name _ body) :in test-definition
                        :do
                           (:dl
                            (:dt (symbol-name name))
                            (:dd
                             (:pre (format nil "~a" body))
                             "This test calls:"
                             (:ol
                              (loop :for symbol :in (br:test-calls-who name)
                                    :do
                                       (:li (symbol-name symbol)) ))))))))))))

(define-easy-handler (tests :uri "/test-results") ()
  (with-output-to-string (spinneret:*html*)
    (spinneret:with-html
      (:doctype)
      (:html
       (:head
        (:title "Tests results")
        (:link :rel "stylesheet" :href "style.css"))
       (:body
        (:h1 "Tests results"
             (loop
               :for package :being :the :hash-key :of (breeze.xref:tests-by-package)
                 :using (hash-value test-definition)
               :do
                  (:h2 (package-name package))
                  (loop
                    :for (name _ body) :in test-definition
                    :for result := (breeze.test:test-results name)
                    :do
                       (:dl
                        (:dt (symbol-name name))
                        (:dd
                         (:pre ;; (format nil "~A" test-results)
                               (with-output-to-string (output)
                                 (describe result output))
                               )))))))))))



(defun empty-tests-by-package ()
  (hash-table-alist
   (cl-hash-util:collecting-hash-table
    (:mode :append)
    (loop :for test-name
            :being :the :hash-key :of breeze.test:*test*
              :using (hash-value test-definition)
          :unless (third test-definition)
            :do (cl-hash-util:collect (second test-definition) test-name)))))

(defun undocumented-symbols-by-kind ()
  (hash-table-alist
   (cl-hash-util:collecting-hash-table
    (:mode :append)
    (loop :for (kind what) :in
                           (loop
                             :for package :in (br:current-packages)
                             :append (br:find-undocumented-symbols package))
          :do (cl-hash-util:collect kind what)))))

(define-easy-handler (todo :uri "/todo") ()
  (with-output-to-string (spinneret:*html*)
    (spinneret:with-html
      (:doctype)
      (:html
       (:head
        (:title "Todo")
        (:link :rel "stylesheet" :href "style.css"))
       (:body
        (:h1 "Todo")
        (:div
         (:h2 "Empty tests")
         (:p "Tests that were declared with \"deftest\" but are empty.")
         (loop
           :for (package . tests) :in (empty-tests-by-package)
           :do
              (:h3 (package-name package))
              (loop :for name :in tests
                    :do (:dl (:dt (symbol-name name))))))
        (:div
         (:h2 "Missing documentation")
         (:p "Symbols that are exported but don't have a docstring.")
         (loop
           :for (kind . whats) :in (undocumented-symbols-by-kind)
           :do
              (:h3 (symbol-name kind))
              (loop :for what :in whats
                    :do (:dl (:dt (format nil "~(~a~)" what)))))))))))






#|

contentEditable="true" spellcheck="false"

observer.disconnect();

var observer = new MutationObserver((mutations, observer) => {
  mutations.forEach((mutation) => {
    console.log("mutation: ", mutation);
  });
});

observer.observe(document.body, {
  subtree: true,
  attributes: true,
  attributeOldValue: true,
  childList: true,
  characterData: true,
  characterDataOldValue: true,
})


var style = window.getComputedStyle(document.body)
style.getPropertyValue('--bar')

window.getComputedStyle(document.documentElement).getPropertyValue('--color-font-general');
document.documentElement.style.setProperty('--color-font-general', '#000');

|#
