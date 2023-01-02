

(defun package-test (package)
  "Find all tests defined in PACKAGE."
  (let ((package (find-package package)))
    (loop
      :for test-name :being :the :hash-key :of breeze.test:*test*
        :using (hash-value test-definition)
      :for test-package = (second test-definition)
      :when (eq test-package package)
        :collect test-name)))

(defun tests-by-package ()
  "Return a hash-table of tests keyed by package."
  (cl-hash-util:collecting-hash-table (:mode :append)
    (loop
      :for test-name :being :the :hash-key :of breeze.test:*test*
        :using (hash-value test-definition)
      :for package = (second test-definition)
      :do (cl-hash-util:collect package test-definition))))


(defun calls-who (function-name)
  "Take a function name and returns a list of all the functions it calls."
  (uiop:while-collecting (collect)
    (walk-car
     (function-body function-name)
     (lambda (el)
       (when (function-body el)
         (collect el))))))

;; TODO
#+todo ;; It's done, but not tested nor used
(defun list-function (&optional (package *package*))
  "List all the functions, optionally filter by package"
  (loop :for function-name :being :the :hash-key :of *function*
        :when (if package
                  (eq package (symbol-package function-name))
                  t)
          :collect function-name))

;; TODO
#+todo ;; Look at parse-smth in cover.lisp
(defun function-without-documentation (&optional (package *package*)))
