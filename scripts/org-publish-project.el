
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
(require 'org-roam)

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
   :recursive t
   :publishing-function org-html-publish-to-html
   :publishing-directory ,(file-name-concat *breeze-root* "public/")

   :author "Francis St-Amour"
   :creator "Francis St-Amour"
   :with-author nil

   ;; See also: org-html-head-include-default-style,
   ;; org-html-style-default, org-html-style
   :html-style nil
   :html-head-include-default-style nil

   :html-validation-link nil

   :html-link-up ""                     ; this is the default
   :html-link-home "/index.html"

   ;; this is a format string, the first %s is the up "url",
   ;; and the second is the "home" url. Here, we assume the
   ;; first is empty
   :html-home/up-format "<div id=\"org-div-home-and-up\"> %s
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
   :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"/src-block.css\" />"

   :auto-sitemap t
   ;; https://emacs.stackexchange.com/questions/70824/how-to-use-makeindex
   :makeindex t ;; TODO need to add a bunch of #+index: term
   :with-toc nil
   )
 org-publish-project-alist
 :key #'car
 :test #'string=)

;; Don't ask for confirmation to run the code blocks.
(setf org-confirm-babel-evaluate nil)

;; orb-babel: Enable some languages to be able to evaluate the source
;; blocks when publishing.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (lisp . t)
   (plantuml . t)))

;; Set the root of the org-roam files
(setq org-roam-directory (file-name-concat *breeze-root* "docs/"))

;; Make sure org-roam's database is up-to-date
(org-roam-db-sync)

;; fontify code in code blocks
;; (setq org-src-fontify-natively t)

;; Enable syntax coloring when using ox-html to publish to html The
;; default value 'inline-css it can't be used when emacs is in batch
;; mode.
(setf org-html-htmlize-output-type 'css)

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
  (copy-file "docs/style.css" "public/" :ok-if-already-exists)
  (copy-file "docs/src-block.css" "public/" :ok-if-already-exists))
