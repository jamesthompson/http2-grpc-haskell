{ pkgs ? import ./nixpkgs/default.nix { }
, bazelEnvStdInputs ? import ./nixpkgs/bazelEnvStdInputs.nix { inherit pkgs; }
}:

with pkgs;

mkShell {

  BAZEL_USE_CPP_ONLY_TOOLCHAIN = 1;
  LANG = "C.UTF-8";

  buildInputs = bazelEnvStdInputs.dev;

  shellHook = ''
    BAZELRC_LOCAL=".bazelrc.local"
    ARCH=""
    if [ "$(uname)" == "Darwin" ]; then
      ARCH="darwin"
    elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
      ARCH="linux"
    fi
    echo "build --host_platform=@rules_haskell//haskell/platforms:''${ARCH}_x86_64_nixpkgs" > $BAZELRC_LOCAL
    echo "run --host_platform=@rules_haskell//haskell/platforms:''${ARCH}_x86_64_nixpkgs" >> $BAZELRC_LOCAL
    echo "query --package_path %workspace%:${bazel}/bin/bazel/base_workspace" >> $BAZELRC_LOCAL
    source ${bazel}/share/bash-completion/completions/bazel
  '';
}
