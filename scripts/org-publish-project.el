
(message "Installing packages (from ELPA)")

(when package-enable-at-startup
  (package-initialize)
  (setf package-selected-packages
        '(htmlize))
  (package-install-selected-packages t))


(message "Publishing...")

(require 'org)
(require 'org-id)
(require 'htmlize)

;; See (describe-variable 'org-publish-project-alist)
;; See https://orgmode.org/manual/Publishing-options.html for more options
(let* ((forcep t)              ; "forcep" is for interactive sessions.
       (org-id-link-to-org-use-id t)
       (default-directory
        (expand-file-name
         (concat
          (file-name-directory (or load-file-name buffer-file-name))
          "..")))
       (root "docs")
       (project-alist
        `("breeze"
          :base-directory ,root
          :publishing-function org-html-publish-to-html
          :publishing-directory "./public"

          :author "Francis St-Amour"
          :creator "Francis St-Amour"
          :with-author nil

          :html-style nil

          :html-validation-link nil

          :html-link-up ""              ; this is the default
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
          )))
  (org-id-update-id-locations (directory-files root t "\\.org$"))
  (org-publish project-alist forcep)
  (dolist (file (directory-files "docs/" t "listing-.*\\.html$"))
    (copy-file file "public/" :ok-if-already-exists))
  (copy-file "docs/reference.html" "public/" :ok-if-already-exists)
  (copy-file "docs/style.css" "public/" :ok-if-already-exists))
