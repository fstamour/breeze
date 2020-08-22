#!/usr/bin/env nix-shell
#!nix-shell ./shell.nix --command "./ci.sh";

  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    lisp-quicklisp
    sbcl
  ];
}
