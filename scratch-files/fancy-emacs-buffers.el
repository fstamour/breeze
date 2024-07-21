;;; -*- lexical-binding: t; -*-

;;; Trying out/taking notes to make a "fancy" UI

(defmacro breeze-comment (&rest _))

(breeze-comment
 ;; See overlay properties and breeze-overlay-put-modifications-hooks
 (defun breeze-modification-hook (overlay afterp beg end &optional pre-change-length)
   (let ((content (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
     (if afterp
         (message "After %S" content)
       (message "Before: %S" content)))))

;; See overlay properties and breeze-overlay-put-modifications-hooks*
(defun breeze-modification-hook (overlay afterp beg end &optional pre-change-length)
  (let* ((content (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay)))
         (transform (overlay-get overlay 'breeze-transform))
         (target (overlay-get overlay 'breeze-target))
         (inhibit-read-only t)
         (buffer-undo-list t)
         (beg (overlay-start target))
         (end (overlay-end target)))
    (save-excursion
      (goto-char beg)
      (delete-char (- end beg))
      ;; TODO add error-handling around the funcall
      (insert (propertize (funcall transform content) 'read-only t)))))

;; Trying my best xD
(defun breeze-reset-buffer (name)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (when-let ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (remove-overlays)
        (erase-buffer)
        (kill-all-local-variables t))
      ;; (kill-buffer buffer)
      ))
  (get-buffer-create name))

;; (let ((buffer (breeze-reset-buffer "test"))) (display-buffer buffer))

(cl-defun breeze-insert (string)
  (when string
    (if (listp string)
        (insert (apply 'propertize string))
      (insert string))))

;; TODO perhaps not the best name...
(cl-defun breeze-make-overlay-with-text
    (&optional before inside after front-advance (rear-advance t))
  "Insert BEFORE, INSIDE and AFTER; then create an overlay that spans
the same range as INSIDE."
  (breeze-insert before)
  (let ((start (point)))
    (breeze-insert inside)
    (let ((end (point)))
      (breeze-insert after)
      (make-overlay start end (current-buffer)
                    front-advance rear-advance))))

(defun breeze-overlay-put-modifications-hooks (overlay)
  "Setup the OVERLAY's hooks to call breeze-modification-hook on any
kind of modifications."
  ;; With overlays, these hooks are called both before and after
  ;; each change.
  (dolist (property '(modification-hooks
                      insert-in-front-hooks
                      insert-behind-hooks))
    (overlay-put overlay property (list 'breeze-modification-hook))))

(cl-defun breeze-display-buffer (&optional pop)
  (if pop
      (pop-to-buffer (current-buffer))
    (display-buffer (current-buffer)
                    'display-buffer-same-window)))

(defface breeze-section-heading
  (let ((common '( :extend t
                   :weight bold
                   :inherit separator-line
                   :height 1.2
                   :underline t)))
    `((((class color) (background light))
       ,@common
       :foreground "DarkGoldenrod4")
      (((class color) (background dark))
       ,@common
       :foreground "LightGoldenrod2")))
  "Headings"
  ;; TODO :group
  )

;; (face-all-attributes 'breeze-section-heading)

(defun breeze-overlay-put* (overlay props)
  (cl-loop for (name value) on props by 'cddr
           do (overlay-put overlay name value)))


(let ((buffer (breeze-reset-buffer "*stupid-tests*"))
      (transform (lambda (string)
                   (string-reverse string))))
  (with-current-buffer buffer
    (breeze-display-buffer)
    ;; (setq buffer-read-only t)
    (let* ((inhibit-read-only t)
           (buffer-undo-list t)
           (heading-props `( read-only t
                             front-sticky t
                             face breeze-section-heading))
           (overlay1 (breeze-make-overlay-with-text
                      `( "Input\n" ,@heading-props rear-nonsticky t)
                      ""
                      `("\n" read-only t)))
           (overlay2 (breeze-make-overlay-with-text
                      `( "Output\n" ,@heading-props ))))
      (breeze-overlay-put-modifications-hooks overlay1)
      (breeze-overlay-put* overlay1
                           `( breeze-transform ,transform
                              breeze-target ,overlay2))
      (breeze-overlay-put* overlay2 `(font-lock-face bold))
      (progn        ; save-excursion
        (goto-char (overlay-start overlay1))
        (insert "test")))))


(breeze-comment
 (let ((str "hello"))
   (add-text-properties
    0 (length str)
    `(,@nil
      show-help-function nil
      intangible nil ;; this is obsolete
      inhibit-isearch nil
      read-only t
      mouse-face highlight
      rear-nonsticky t
      face font-lock-doc-face)
    str)
   (insert str)))


;; (make-overlay beg end &optional buffer front-advance rear-advance)
;; (overlay-put overlay prop value)

;; (overlay-at pos)
;; (overlays-in beg end)
;; (overlays-list)
;; (overlay-properties)

;; (mapcar 'overlay-properties (overlays-at (point)))

;; The 'evaporate property is "crucial" for non-temporary buffers

;; Important note: (erase-buffer) doesn't remove overlays...



;; (put-text-property )


;;; Takeaways
;; - pop-up or display a buffer
;; - reset/clean the buffer
;; - sprinkle with overlays or text properties
;; - overlays are not usually deleted automatically



;; (thing-at-point THING &optional NO-PROPERTIES)
;; ‘symbol’, ‘list’, ‘sexp’, ‘defun’,
;; ‘filename’, ‘existing-filename’, ‘url’, ‘email’, ‘uuid’, ‘word’,
;; ‘sentence’, ‘whitespace’, ‘line’, ‘number’, ‘face’ and ‘page’.
