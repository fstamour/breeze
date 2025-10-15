

(defvar *code-id-counter* 0)
(defparameter *code* (make-hash-table))

;; (clrhash *code*)

(define-easy-handler (code :uri "/code")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (let ((id (or id (incf *code-id-counter*))))
    (with-output-to-string (*html*)
      (with-html ("Code")
        (:ul
         (loop
           :for snippet :being :the :hash-key :of *snippets*
           :for fn = (snippet-function-symbol snippet)
           :do (:li
                (:form :method "post"
                       :action "/code/add-snippet-form"
                       (:input :type "hidden"
                               :value id
                               :name "id")
                       (:input :type "hidden"
                               :name "snippet"
                               :value snippet)
                       (:input :type "submit"
                               :value (format nil "Insert ~(~a~)" snippet))))))
        (unless (gethash id *code*)
          (setf (gethash id *code*) (list (format nil ";;; Code ~a" id))))
        (loop
          :for block :in (gethash id *code*)
          :for block-id :from 0
          :do
             (:article (:pre block))
             (:form :method "post"
                    :action "/code/edit-block"
                    (:input :type "hidden"
                            :value id
                            :name "id")
                    (:input :type "hidden"
                            :name "block-id"
                            :value block-id)
                    (:input :type "submit"
                            :value "Edit"))
             (:form :method "post"
                    :action "/code/delete-block"
                    (:input :type "hidden"
                            :value id
                            :name "id")
                    (:input :type "hidden"
                            :name "block-id"
                            :value block-id)
                    (:input :type "submit"
                            :value "Delete")))
        ))))

(define-easy-handler (add-snippet-form :uri "/code/add-snippet-form")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (with-output-to-string (*html*)
    (with-html ("Add snippet to code")
      (:h2 (format nil "Snippet \"~(~a~)\"" snippet))
      (:form :method "post"
             :action "/code/add-snippet"
             (:input :type "hidden"
                     :value id
                     :name "id")
             (:input :type "hidden"
                     :name "snippet"
                     :value snippet)
             (loop
               :for spec :in (snippet-inputs
                              (intern snippet #.*package*))
               :do (render-input spec) (:br))
             (:input :type "submit")))))

(define-easy-handler (add-snippet :uri "/code/add-snippet")
    ((id :parameter-type 'integer)
     (snippet :parameter-type 'string))
  (setf (gethash id *code*)
        ;; Append the current code with the result of calling the snippet.
        (append
         (gethash id *code*)
         (list
    ;; Call the snippet function
          (apply (snippet-function-symbol* snippet)
                 nil
                 ;; Transform the parameters to keywords
                 (loop :for (key value) :on
                                        (hash-table-plist
                                         (cl-hash-util:collecting-hash-table ()
                                                                             (loop
                                                                               :for (key . value) :in (post-parameters*)
                                                                               :do
                                                                                  (unless (member key '("id" "snippet")
                                                                                                  :test 'string=)
                                                                                    (when (and value
                                                                                               (> (length value) 0))
                                                                                      (cl-hash-util:collect
                                                                                          (make-keyword (string-upcase key))
                                                                                        value))))))
                       :by #'cddr
                       :append (list key
                                     (if (cdr value)
                                         value
                                         (car value))))))))
  (redirect (format nil "/code?id=~a" id)))

(cl-hash-util:collecting-hash-table
 (:mode (list
         #'(lambda (current new)
             (append (ensure-list current)
                     (list new)))
         #'(lambda (new)
             new)))
 (cl-hash-util:collect :a 1)
 (cl-hash-util:collect :a 2)
 (cl-hash-util:collect :b 1)
 )

(loop
  :for snippet :being :the :hash-key :of *snippets*
  :for fn = (snippet-function-symbol snippet)
  :collect (list fn (documentation fn 'function)))

(define-easy-handler (snippet :uri "/code/snippet")
    ((snippet :parameter-type 'string))
  (with-output-to-string (*html*)
    (with-html ("Snippet")
      (:p
       (format nil snippet)
       ;; Warning: internalizing strings from the internet _might_ not be a good idea
       (let ((snippet (intern (string-upcase snippet))))
         (if (gethash snippet *snippets*)
             (:ul
              (multiple-value-bind (required optional)
                  (parse-ordinary-lambda-list
                   (third
                    (gethash snippet *snippets*)))
                (loop :for arg :in required
                      :do (:li (format nil "~a" arg)))
                (loop :for (arg default _) :in optional
                      :do (:li (format nil "~a" arg)))))
             "Not found"))))))
