#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "rlwrap sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(load \"tests/selftest.lisp\")' --eval '(breeze.selftest:selftest)'

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
	  rlwrap
    sbcl
  ];
}
