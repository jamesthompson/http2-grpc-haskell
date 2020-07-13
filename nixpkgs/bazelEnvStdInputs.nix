{ pkgs }:

with pkgs;

{

  dev = [
    bazel
    buildifier
    binutils
    cacert
    fd
    ghcid
    git
    go
    nix
    nixfmt
    perl
    python3
    stack
    unzip
    which
    zip
    hlint
  ];

}
