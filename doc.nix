#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "sbcl --non-interactive --eval '(ql:quickload (list :breeze))' --eval '(breeze.documentation::generate-documentation)'"

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    sbcl
  ];
}
