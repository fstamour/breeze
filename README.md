# <a name="readme">Breeze</a>

<a href="https://travis-ci.org/fstamour/breeze">![Build Status](https://travis-ci.org/fstamour/breeze.svg?branch=master)</a>

<a href="https://ko-fi.com/F2F21YR7I">Support me on Ko-Fi</a>

Breeze is a set of tools that aims to make lisp development a breeze (hence the name).

It is still in its early development.

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
	./shell.nix

Don't know what to do next? Call `(br:next)`.

### Contributing

Start by forking and cloning this repository into quicklisp's
local-projects directory.

Optional: setup the pre-commit hook (currently assumes that
[nix](https://nixos.org/) is installed).

	git config core.hooksPath githooks

Look for TODOs in the code

	grep -ir --include='*.lisp' todo
	# or
	rg -i todo

### How to run the (self-) tests

From the repl:

	(ql:quickload 'breeze)
	(br:selftest)

Or from the command line, with nix:

	./doc.nix

### How to generate the documentation

From the repl:

	(breeze.documentation::generate-documentation)

Or from the command line, with nix:

	./doc.nix

With either method, the documentation is generated into to `docs/`
folder.

## Notes

- "selftests" are called like that to prevent confusion with
  `breeze.test`, which is the test framework that breeze provides.
