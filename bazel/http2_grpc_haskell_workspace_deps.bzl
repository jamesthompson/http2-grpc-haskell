""" http_grpc_haskell workspace dependencies """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

def http2_grpc_haskell_workspace_deps():
    """ Defines repositories for core rules sets
    """

    # rules_haskell
    git_repository(
        name = "rules_haskell",
        remote = "https://github.com/tweag/rules_haskell",
        commit = "59200ae766f9efa63ad3a2dbd32694d24f0429b1",
        shallow_since = "1589818440 +0000",
    )

    http_archive(
        name = "happy",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "happy", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
        sha256 = "fb9a23e41401711a3b288f93cf0a66db9f97da1ce32ec4fffea4b78a0daeb40f",
        strip_prefix = "happy-1.19.12",
        urls = ["http://hackage.haskell.org/package/happy-1.19.12/happy-1.19.12.tar.gz"],
    )

    # rules_proto
    http_archive(
        name = "rules_proto",
        sha256 = "73ebe9d15ba42401c785f9d0aeebccd73bd80bf6b8ac78f74996d31f2c0ad7a6",
        strip_prefix = "rules_proto-2c0468366367d7ed97a1f702f9cd7155ab3f73c5",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/2c0468366367d7ed97a1f702f9cd7155ab3f73c5.tar.gz",
            "https://github.com/bazelbuild/rules_proto/archive/2c0468366367d7ed97a1f702f9cd7155ab3f73c5.tar.gz",
        ],
    )
