#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "rlwrap sbcl --eval '(ql:quickload (list :staple :staple-markdown :breeze))' --eval '(staple:generate :breeze :if-exists :supersede)'"
## --non-interactive

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
	  rlwrap
    sbcl
  ];
}
