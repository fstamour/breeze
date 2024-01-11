
(message "Installing packages (from ELPA)")

(package-initialize)
(setf package-selected-packages
      '(htmlize))
(package-install-selected-packages t)


(message "Publishing...")

(require 'org)
(require 'org-id)
(require 'htmlize)



;; See (describe-variable 'org-publish-project-alist)

(let* ((forcep t)               ; "forcep" is for interactive sessions.
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

          :auto-sitemap t
          ;; :makeindex t
          :with-toc nil
          )))
  (org-id-update-id-locations (directory-files root t "\\.org$"))
  (org-publish project-alist forcep))
