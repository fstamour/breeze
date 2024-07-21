;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   (setq-local org-roam-directory
                               (file-truename
                                (file-name-concat
                                 (locate-dominating-file default-directory ".dir-locals.el")
                                 "docs/")))
                   (setq-local org-roam-db-location
                               (file-name-concat org-roam-directory "org-roam.db"))))
         (org-roam-capture-templates . (("d" "default" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}
") :unnarrowed t))))))
