;; Using emacs to edit images??

(defvar *demo-root* (concat
		     (vc-find-root
		      (buffer-file-name
		       (current-buffer))
		      ".git")
		     "/demo"))

(defvar *demo-annotations* ())

(defun demo-frame-annotation (svg)
  (assoc-string
   svg
   *demo-annotations*))

(defun demo-annotate-frame (root svg)
  (save-window-excursion
    ;; (toggle-frame-fullscreen )
    (save-excursion
      (let ((buffer
	     (find-file (concat root "/" svg))))
	(unwind-protect
	    (progn
	      (delete-other-windows)
	      (with-current-buffer buffer
		(let* ((size (image-size (image--get-image)))
		       (width (car size))
		       (height (cdr size))
		       (scale (min (/ (window-width) width)
				   (/ (window-height) height))))
		  (image--change-size scale)))
	      (push
	       (cons svg
		     (read-string "Annotate this frame: "))
	       *demo-annotations*))
	  (kill-buffer buffer))))))

(defun demo-annotate-all-frames-in-folder (root)
  (cl-loop for svg in (directory-files root nil "\\.svg$")
	   repeat 1
	   unless (demo-frame-annotation svg)
	   do (demo-annotate-frame root svg)))

(demo-annotate-all-frames-in-folder
 (concat
  *demo-root*
  "/annotated"))




(defun demo-read-annotation-from-text-file (file)
  (cl-loop for line in
	   (split-string
	    (with-temp-buffer
	      (insert-file-contents file)
	      (buffer-substring-no-properties (point-min) (point-max)))
	    "\n"
	    t)
	   collect
	   (save-match-data
	     (and (string-match
		   "^\\([a-z0-9]+\\) \\([^ ]+\\) ?\\(.*\\)$" line)
		  (let ((filename
			 (match-string 2 line))
			(hash (match-string 1 line))
			(annotation (match-string 3 line)))
		    (list
		     filename
		     :sha1 hash
		     :annotation
		     (if (string-empty-p annotation)
			 ;; Get from global variable
			 (getf
			  (cdr (demo-frame-annotation
				(match-string 2 line)))
			  :annotation)
		       annotation)))))))



(defvar *demo-annotation-from-file*
  (let* ((root (concat
		*demo-root*
		"/annotated"))
	 (file (concat root "/annotations.txt")))
    (demo-read-annotation-from-text-file file)))


(getf
 (cdr
  (assoc-string "termtosvg_00001.svg"
		*demo-annotation-from-file*))
 :annotation)

;; (setf *demo-annotations* *demo-annotation-from-file*)
;; (setf *demo-annotation-from-file* *demo-annotations*)

;; Write it back
(let* ((root (concat
	      *demo-root*
	      "/annotated"))
       (file (concat root "/annotations2.txt")))
  (with-temp-buffer
    (cl-loop for (filename _ hash _ annotation) in *demo-annotation-from-file*
	     do (insert hash " " filename " " annotation "\n"))
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))
    (buffer-substring-no-properties (point-min) (point-max))
    ))
