# <a name="emacs">Emacs integration</a>

## Features

* Snippets
* Capture ideas easily (`breeze-capture`)
* Create project interactively
* Refactor
* Add a minor-mode `breeze-mode`

> 💥 Warning: some functions require `slime` to be connected and `breeze` to be loaded, but no checks are done currently.

## Setup

	(load `quicklisp/local-projects/breeze/src/breeze.el`)
	(add-hook 'lisp-mode-hook #'breeze-mode)

## Customizations

In emacs, `M-x customize`, search for `breeze`.

| Custom | Description |
| - | - |
| breeze-default-author | The default author when generating asdf system. (When using `breeze-insert-asdf`.) |
| breeze-default-licence | The default licence when generating asdf system. (When using `breeze-insert-asdf`.) |
| breeze-capture-folder | The folder where to save scratch files. |
| breeze-capture-template | The fixed template to insert in a new scratch file. |

## Commands and default keymap

| Command | Description | Default Key |
| - | - | - |
| breeze-capture | Create an empty lisp file. | `C-c c` |
| breeze-insert | Choose something to insert | `C-c ,` |
| breeze-quickproject | Interactively create a project in `quicklisp`'s `local-projects` folder using `quickproject`. | |
| breeze-reevaluate-form | Choose and (re)evaluate a recently evaluated form. (`swank:interactive-eval` must be advised first.) | `C-c e e` |
| breeze-run-tests | Call `(breeze:run-all-tests)` | |

### Snippets

> Tips: Snippets don't have key bindings by default, but `breeze-insert` does.

> All snippets are skeletons.

| Command | Description |
| - | - |
| breeze-insert-defpackage | Insert a `defpackage` form. |
| breeze-insert-defun | Insert a `defun` form. |
| breeze-insert-defmacro | Insert a `defmacro` form. |
| breeze-insert-defvar | Insert a `defvar` form.  |
| breeze-insert-defparameter | Insert a `defparameter` form. |
| breeze-insert-let | Insert a `let` form. |
| breeze-insert-asdf | Insert a `defsystem` (from `asdf`) form and some auxiliary forms. |
| breeze-insert-loop-clause-for-hash | Insert `:for X :being :the :hash-key :of Y :using (hash-value Z)`, which is the clause to loop over a `hash-table` in a  `loop` form. |

(define-key breeze-mode-map (kbd "<f5>")
  'breeze-run-tests)

### Refactoring

| Command | Description | Default Key |
| - | - | - |
| breeze-move-form-into-let | Move the current form into the nearest parent `let` form. | |
