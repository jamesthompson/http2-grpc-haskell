{ pkgs }:

with pkgs;

{

  dev = [
    bazel
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
    unzip
    which
    zip
    openjdk
    hlint
  ];

}
