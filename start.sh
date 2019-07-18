#!/usr/bin/env nix-shell
#! nix-shell -p rlwrap lispPackages.quicklisp -i "sh -e -- $@"

rlwrap quicklisp run -- --eval '(ql:quickload "breeze")' --eval '(in-package #:breeze.user)' $@

