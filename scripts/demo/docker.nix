# bunch of examples: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix
# some reference doc: https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools
# and some very basic tuto: https://nix.dev/tutorials/building-and-running-docker-images

{ pkgs ? import <nixpkgs> { }
, pkgsLinux ? import <nixpkgs> { system = "x86_64-linux"; }
}:

pkgs.dockerTools.buildImage {
  name = "breeze-demo-recorder-base";
  tag = "latest";

  contents = with pkgsLinux; [
    sbcl
    lispPackages.quicklisp
    emacs
    xvfb_run
    ffmpeg # to record video
    bashInteractive
    coreutils # needed by me and quicklisp
    scrot # to take screenshots
    libressl # for emacs to connect to elpa/melpa
    findutils # grep # for my sanity, when debugging
  ];

  config = {
    Env = [
      # TODO This variable doesn't contains what I hoped
      # it point to /nix/store/<some-hash-A>-libressl-3.4.1-bin
      # instead of  /nix/store/<some-hash-B>-libressl-3.4.1
      "LIBRESSL_PEM=${pkgsLinux.libressl}"
    ];
    Cmd = [ "${pkgsLinux.bashInteractive}/bin/bash" ];
  };
}
