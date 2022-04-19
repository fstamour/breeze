# <a name="emacs">Emacs integration</a>

## Features

* Snippets
* Capture ideas easily (`breeze-capture`)
* Create project interactively
* Refactor
* Provides a minor-mode `breeze-mode`

## Setup

	(load `quicklisp/local-projects/breeze/src/breeze.el`)
	(add-hook 'lisp-mode-hook #'breeze-mode)

## Commands and default keymap

| Command | Description | Default Key |
| - | - | - |
| breeze-quickfix | Choose from a list of commands applicable to the current context. | `C-.` |
| breeze-capture | Quickly create a lisp file in a pre-determined directory. | |
| breeze-scaffold-project | Interactively create a project in `quicklisp`'s `local-projects` folder using `quickproject`. | |
