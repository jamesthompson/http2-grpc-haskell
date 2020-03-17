""" Haskell library dependencies """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository", "new_git_repository")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

# Listed stackage packages allowing another workspace using this
# as a dependency to configure its `stackage` dependency as a superset
# taking these required packages into account.
http2_grpc_stackage_packages = [
  "async",
  "base",
  "base",
  "binary",
  "bytestring",
  "case-insensitive",
  "containers",
  "data-default-class",
  "deepseq",
  "ghc",
  "ghc-paths",
  "ghc-source-gen",
  "http-types",
  "lens-family",
  "lens-family-core",
  "lifted-async",
  "lifted-base",
  "mtl",
  "network-byte-order",
  "parser-combinators",
  "pretty",
  "proto-lens",
  "proto-lens-protoc",
  "proto-lens-runtime",
  "psqueues",
  "selective",
  "stm",
  "text",
  "time",
  "time-manager",
  "tls",
  "transformers-base",
  "unix",
  "unordered-containers",
  "vector",
  "wai",
  "warp",
  "warp-tls",
  "zlib",
]

def define_http2_grpc_stackage_dependencies(stackage_snapshot):
  """ Define stackage-sourced packages.
  """

  stack_snapshot(
    name = "stackage",
    snapshot = stackage_snapshot,
    packages = http2_grpc_stackage_packages,
     haddock = False,
     extra_deps = {
       "zlib": [
         "@zlib.dev//:zlib"
       ],
     },
     tools = [
       "@happy",
     ],
   )

def define_http2_grpc_lib_deps():
  """ Define hackage-sourced, cabal-built packages """

  http_archive(
    name = "network",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
  name = "network",
  srcs = glob(["**"]),
  version = "2.8.0.1",
  deps = [
    "@stackage//:base",
    "@stackage//:bytestring",
    "@stackage//:unix",
  ],
  visibility = ["//visibility:public"],
  haddock = False,
)
    """,
    sha256 = "61f55dbfed0f0af721a8ea36079e9309fcc5a1be20783b44ae500d9e4399a846",
    strip_prefix = "network-2.8.0.1",
    urls = ["http://hackage.haskell.org/package/network-2.8.0.1/network-2.8.0.1.tar.gz"],
  )

  http_archive(
    name = "http2",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
  name = "http2",
  srcs = glob(["**"]),
  version = "2.0.3",
  deps = [
    "@stackage//:base",
    "@stackage//:bytestring",
    "@stackage//:case-insensitive",
    "@stackage//:containers",
    "@stackage//:http-types",
    "@stackage//:network-byte-order",
    "@network//:network",
    "@stackage//:psqueues",
    "@stackage//:stm",
    "@stackage//:time-manager",
  ],
  visibility = ["//visibility:public"],
  haddock = False,
)
    """,
    sha256 = "ba5105f31d0e83d5bd7f6f6c5deef6b277cd6a82f9701e19920f54e0c00b1093",
    strip_prefix = "http2-2.0.3",
    urls = ["http://hackage.haskell.org/package/http2-2.0.3/http2-2.0.3.tar.gz"],
  )

  new_git_repository(
    name = "http2-client",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
  name = "http2-client",
  srcs = glob(["**"]),
  version = "0.9.1.0",
  deps = [
    "@stackage//:base",
    "@stackage//:async",
    "@stackage//:bytestring",
    "@stackage//:containers",
    "@stackage//:deepseq",
    "@http2//:http2",
    "@stackage//:lifted-async",
    "@stackage//:lifted-base",
    "@stackage//:mtl",
    "@network//:network",
    "@stackage//:stm",
    "@stackage//:time",
    "@stackage//:tls",
    "@stackage//:transformers-base",
  ],
  visibility = ["//visibility:public"],
  haddock = False,
)
    """,
    commit = "7201f65b31bf0a2b0bc4a904173233e1e728697b",
    remote = "https://github.com/lucasdicioccio/http2-client",
    shallow_since = "1572717292 +0100"
  )

def proto_lens_protoc_binary():
  """ We must build the proto-lens protoc binary
      as stack_snapshot will only provision the library
  """
  http_archive(
    name = "proto-lens-protoc",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
  name = "proto-lens-protoc",
  srcs = glob(["**"]),
  deps = [
    "@stackage//:base",
    "@stackage//:bytestring",
    "@stackage//:containers",
    "@stackage//:ghc",
    "@stackage//:ghc-paths",
    "@stackage//:ghc-source-gen",
    "@stackage//:lens-family",
    "@stackage//:pretty",
    "@stackage//:proto-lens",
    "@stackage//:proto-lens-protoc",
    "@stackage//:proto-lens-runtime",
    "@stackage//:text",
  ],
  visibility = ["//visibility:public"],
)
    """,
    sha256 = "b946740b94c8d300cd8e278ded9045905ef1985824cef6b81af0d79b119927be",
    strip_prefix = "proto-lens-protoc-0.6.0.0",
    urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.6.0.0/proto-lens-protoc-0.6.0.0.tar.gz"],
  )
