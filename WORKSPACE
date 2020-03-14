workspace(name = "http2_grpc_haskell")

# Toolchain setup ---------------------------------------------------------------

# Load core dependency repositories
load("//bazel:http2_grpc_haskell_workspace_deps.bzl", "http2_grpc_haskell_workspace_deps")
http2_grpc_haskell_workspace_deps()


# Configure rules_haskell and the GHC toolchain

ghc_version = "8.6.5"
stackage_snapshot = "lts-15.3"

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
haskell_register_ghc_nixpkgs(repository = "@rules_haskell//nixpkgs:default.nix", version = ghc_version)
load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")
rules_haskell_toolchains(version = ghc_version)
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")
rules_haskell_worker_dependencies()

# Nixpkgs ensures we have the correct cc toolchain
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_cc_configure", "nixpkgs_python_configure", "nixpkgs_package")
nixpkgs_cc_configure(
  nix_file = "@rules_haskell//nixpkgs:cc-toolchain.nix",
  nix_file_deps = ["@rules_haskell//nixpkgs:default.nix"],
  repository = "@rules_haskell//nixpkgs:default.nix",
)
# Nixpkgs ensures we have the correct python toolchain
nixpkgs_python_configure(
  repository = "@rules_haskell//nixpkgs:default.nix",
)
# Nixpkgs ensures we have zlib, needed for stack targets
nixpkgs_package(
  name = "nixpkgs_zlib",
  attribute_path = "zlib",
  repository = "//nixpkgs:default.nix",
)
nixpkgs_package(
  name = "zlib.dev",
  build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
filegroup(
  name = "include",
  srcs = glob(["include/*.h"]),
  visibility = ["//visibility:public"],
)
cc_library(
  name = "zlib",
  linkstatic = 1,
  srcs = ["@nixpkgs_zlib//:lib"],
  hdrs = [":include"],
  strip_include_prefix = "include",
  visibility = ["//visibility:public"],
)
""",
  repository = "//nixpkgs:default.nix",
)


# Setup haskell library dependencies from stackage, and some http2- extras ------

load("//bazel:haskell_deps.bzl", "define_stackage_dependencies", "define_http2_lib_deps")
define_stackage_dependencies(stackage_snapshot)
define_http2_lib_deps()


# Protobuf Stuff ----------------------------------------------------------------

load("//bazel:haskell_deps.bzl", "proto_lens_protoc_binary")
proto_lens_protoc_binary()

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

register_toolchains("//:protobuf-toolchain")
