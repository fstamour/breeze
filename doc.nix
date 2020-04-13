#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "sbcl --non-interactive --eval '(ql:quickload (list :staple :staple-markdown :breeze))' --eval '(staple:generate :breeze :if-exists :supersede)' --eval '(terpri)'"

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    sbcl
  ];
}
