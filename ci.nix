#!/usr/bin/env nix-shell
#!nix-shell ./ci.nix --command "./ci.sh"

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    lisp-quicklisp
    sbcl
  ];
}
