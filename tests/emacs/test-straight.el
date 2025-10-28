;; Work in progress: test that breeze is loadable with straight

(load (find-library-name "straight"))

(straight-use-package
 '(breeze :type git :host codeberg :repo "fstamour/breeze"
	  :files "emacs))
