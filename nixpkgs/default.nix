let
  sha256 = "0kp8s36zgmy3vq7vnwa6r9agkqgvhqlzvdzk7py96vnfrg0r021b";
  rev = "20ef86ed52f30ae3c6a6c51599c344bb3df3fedc";
in import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
