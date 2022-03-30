# <a name="readme">Breeze</a>

Breeze is a set of tools that aims to make lisp development a breeze
(hence the name).

It is very much alpha quality, I'm experimenting with a lot of things
in parallel.

Please take a look at the
[notes.org](https://github.com/fstamour/breeze/blob/master/notes.org)
to get a better idea.

![ci](https://github.com/fstamour/breeze/actions/workflows/ci.yml/badge.svg)

[github.com/fstamour/breeze](https://github.com/fstamour/breeze)

## What is this?

This is a git repository that contains lots of common lisp code that I
use to make developping common lisp easier. It is a personal projet
that I work on from time to time, but that I use (and break) pretty
much all the time.

## Features

* [Emacs integration](#emacs)
* Integration with quickproject
* Context-aware, configurable snippets and refactorings
* Command for quick code capture (trying out code in a new file)

Currently, breeze's main interface is emacs. It add an emacs
minor-mode with 2-3 bindings.

Most notably, there is one binding that call a command called
`breeze-quickfix` (might rename in the future). This command suggests
applicable action given the current context (file name, file content,
position in the file). For example, if the file ends with ".asd" it
will suggest a command to insert a `defsystem` form. If breeze was
already configured, it will pre-fill the `:maintainer`, `:author` and
`licence` fields. Another example is that if the file is empty, or
contains only comments, it will suggest to insert a `defpackage` or
`uiop:define-package` form. It is also able to detect when you're
trying to edit/evaluate forms that are in a package that doesn't
exists (_did you forget to evaluate the `defpackage` form_).

The integration with quickproject is pretty simple and let's you
quickly create new projects from the comfort of your editor. The
integration consists of one command that asks you for some
information, like the project name and licence. It takes some default
values from breeze's configuration, but let's you change them. All
this to ease the use of quickproject.

Another simple command that helps me is `breeze-capture`, it creates a
new file in a pre-determined (must be configured) folder and fills it
with some pre-configured content (template) and let's you code right
away. This could've easily be done in emacs (that's how I prototyped
the firts version), but doing this in common lisp makes it easy to
port it to other editors (or just the repl) in the future.

I must stress that this whole project is in constant flux, and until I add more and more tests, stuff might break any time.

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

## Getting started

This project is not in quicklisp, and I don't plan to add it to
quicklisp until it stabilize (which might take years). But I make sure
that I only use dependencies from quicklisp so that if somebody wants
to try it out they'll just need to clone this repository in
quicklisp's local-projects folder.

From the repl:

	(ql:quickload "breeze")

Then load `<breeze>/src/breeze.el` in emacs.

### How to run the tests

From the repl:

	(asdf:test-system "breeze")


Or from the command line:

	./scripts/test.sh

### How to generate the documentation

From the repl:

	(breeze.documentation::generate-documentation)

Or from the command line:

	./scripts/doc.sh

With either method, the documentation is generated into to `docs/`
folder.

### Contributing

Start by forking and cloning this repository into quicklisp's
local-projects directory.

Setup the pre-commit hook

	git config core.hooksPath githooks

Look for TODOs in the code

	grep -ir --include='*.lisp' todo
	# or
	rg -i todo

Peruse the [notes.org](notes.org).

## Support me

I'm doing this for fun, but if you find this useful or just want to
cheer me up :) here's a link for that:

<a href="https://ko-fi.com/F2F21YR7I">Support me on Ko-Fi</a>
