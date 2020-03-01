#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "rlwrap sbcl --non-interactive --eval '(ql:quickload :staple)' --eval '(staple:generate :breeze :if-exists :supersede)'"

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
	  rlwrap
    sbcl
  ];
}
