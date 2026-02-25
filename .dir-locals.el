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
") :unnarrowed t)))
         (eval . (progn
                   (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
                   (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC")))))))

;; TODO maybe set `ispell-local-dictionary', `ispell-dictionary' or
;; `ispell-personal-dictionary' to have a custom dictionary (see also
;; `ispell-hunspell-add-multi-dic')
