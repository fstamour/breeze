#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "./test.sh"

with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
	  rlwrap
    sbcl
  ];
}
