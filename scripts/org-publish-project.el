
(message "Installing packages (from ELPA)")

(when package-enable-at-startup
  (package-initialize)
  (setf package-selected-packages
        '(htmlize))
  (package-install-selected-packages t))


(message "Publishing...")

(require 'org)
(require 'org-id)
(require 'ox-publish)
(require 'htmlize)

(defvar *breeze-root*
  (expand-file-name
   (concat
    (file-name-directory (or load-file-name buffer-file-name))
    "../"))
  "Breeze repo's root")

;; (pop org-publish-project-alist)
(cl-pushnew
 `("breeze"
   :base-directory ,(file-name-concat *breeze-root* "docs/")
   :publishing-function org-html-publish-to-html
   :publishing-directory ,(file-name-concat *breeze-root* "public/")

   :author "Francis St-Amour"
   :creator "Francis St-Amour"
   :with-author nil

   :html-style nil

   :html-validation-link nil

   :html-link-up ""                     ; this is the default
   :html-link-home "index.html"

   ;; this is a format string, the first %s is the up "url",
   ;; and the second is the "home" url. Here, we assume the
   ;; first is empty
   :html-home/up-format "<div id=\"org-div-home-and-up\"> %s
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
   :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"

   :auto-sitemap t
   ;; https://emacs.stackexchange.com/questions/70824/how-to-use-makeindex
   :makeindex t ;; TODO need to add a bunch of #+index: term
   :with-toc nil
   )
 org-publish-project-alist
 :key #'car
 :test #'string=)

;; See (describe-variable 'org-publish-project-alist)
;; See https://orgmode.org/manual/Publishing-options.html for more options
(let* ((forcep t)              ; "forcep" is for interactive sessions.
       (org-id-link-to-org-use-id t)
       (default-directory *breeze-root*)
       (root "docs"))
  (org-id-update-id-locations (directory-files root t "\\.org$"))
  (org-publish "breeze" forcep)
  (dolist (file (directory-files root t "listing-.*\\.html$"))
    (copy-file file "public/" :ok-if-already-exists))
  (copy-file "docs/reference.html" "public/" :ok-if-already-exists)
  (copy-file "docs/style.css" "public/" :ok-if-already-exists))
