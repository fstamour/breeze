;; See https://shinmera.github.io/staple/
;; (staple:generate :breeze :if-exists :supersede)

(staple:load-system-quietly '#:staple-markdown)

(defmethod staple:packages ((system (eql (asdf:find-system '#:breeze))))
  (mapcar #'find-package '(
            ;; It's formatted this way so it's easy to sort
            :breeze.asdf
            :breeze.definition
            :breeze.file-watcher
            :breeze.selftest
            :breeze.test
            :breeze.test-runner
            ;; :breeze.user
            :breeze.utils
            :breeze.worker
            :breeze.xref
            )))

(defmethod staple:documents ((system (eql (asdf:find-system '#:breeze))))
  (list (asdf:system-relative-pathname system "README.md")))
