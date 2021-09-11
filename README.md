# <a name="readme">Breeze</a>

Breeze is a set of tools that aims to make lisp development a breeze
(hence the name).

It is still in its early development.

Please take a look at the
[notes.org](https://github.com/fstamour/breeze/blob/master/notes.org)
to get a better idea.

![ci](https://github.com/fstamour/breeze/actions/workflows/ci.yml/badge.svg)

[github.com/fstamour/breeze](https://github.com/fstamour/breeze)

## Goals and non-goals

### Goals

- Make it easier to develop in common lisp
  - by any means
- With any editor (or even without one)
- Be as portable as possible
- Be useful to new and experimented developper (or even
  non-developpers, we'll get there)

### Non-goals

- Replace slime, sly, slimv, slima, etc
- Replace existing test framework
  - One test "framework" is included in breeze, but only for convenience/experimentation purposes.
- Force the user to use a set of conventions
  - If there are conventions used by breeze, for convenience, they should be customizable.
  - e.g. Currently, some refactoring utilies only work when the user
    use `cl:defpackage` (as opposed to `uiop:defpackage`) and there's
    one `defpackage` per file, but it doesn't have to be that way.

## Features

* Redefine `defun` to keep track of the definitions (before any macro expansion).
* A minimal test library that supports introspection.
* Workers - generic threads with message passing (thanks to [chanl](https://github.com/zkat/chanl)).
* Test-runner - a worker that runs tests (on demand).
* Can re-run all tests on function or test redefinition.
* [Emacs integration](#emacs)
* Integration with slime
* WIP File watcher
* Integration with quickproject
* In the future: integration with different test frameworks

## Getting started

This project is currently not in quicklisp, you'll need to start by
cloning this repository in quicklisp's local-projects folder.

From the repl:

	(ql:quickload :breeze)
	(in-package :breeze.user)
	(br:main)

Or from the command line, with nix (assumes quicklisp is already setup):

	# Will run sbcl and load breeze with quicklisp
	./scripts/shell.nix

Don't know what to do next? Call `(br:next)`.

### Contributing

Start by forking and cloning this repository into quicklisp's
local-projects directory.

Setup the pre-commit hook

	git config core.hooksPath githooks

Look for TODOs in the code

	grep -ir --include='*.lisp' todo
	# or
	rg -i todo

### How to run the (self-) tests

From the repl:

	(ql:quickload 'breeze)
	(br:selftest)

Or from the command line:

	./scripts/test.sh

### How to generate the documentation

From the repl:

	(breeze.documentation::generate-documentation)

Or from the command line:

	./scripts/doc.sh

With either method, the documentation is generated into to `docs/`
folder.

## Notes

- "selftests" are called like that to prevent confusion with
  `breeze.test`, which is the test framework that breeze provides.

## Support me

I'm doing this for fun, but if you find this useful or just want to
cheer me up :) here's a link for that:

<a href="https://ko-fi.com/F2F21YR7I">Support me on Ko-Fi</a>