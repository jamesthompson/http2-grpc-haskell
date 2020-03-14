# haskell-grpc-native

This organization and repository aim at providing unofficial
[gRPC](https://grpc.io/) implementations for Haskell using native libraries.

## Context

Haskell is not an [officially supported gRPC language](https://packages.grpc.io/).
This repository offers a set of libraries that are compatible with gRPC. A main
goal of this initiative is that an intermediate Haskell developper should find
the packages reasonably easy to install and easy to tinker with.

## Repo organization

The code is split in a number of packages following this three ideas:

- share a common types packages
- have server and client-specific packages
- add a serialization-library specific packages

## Bazel for building

Drop inside a nix-shell to utilize bazel with all of the necessary (macOS and/or linux) build tooling.

GHC will be provisioned by nix in the bazel workspace bootstrapping.

Bazel targets can be listed with:

```sh
bazel query //...
```

Current build targets of interest include:

- [`//http2-grpc-types:http2_grpc_types`](http2-grpc-types/BUILD.bazel)
- [`//http2-grpc-proto-lens:http2_grpc_proto_lens`](http2-grpc-proto-lens/BUILD.bazel)
- [`//http2-client-grpc:http2_client_grpc`](http2-client-grpc/BUILD.bazel)
- [`//warp-grpc:warp_grpc`](warp-grpc/BUILD.bazel)

you can build all the targets, which cold should take on the order of 5 mins or so, with:

```sh
bazel build //...
```

drop into an individual target GHCi repl session thus (for example):

```sh
bazel run //http2-client-grpc:http2_client_grpc@repl
```

## Usage

We will soon provide examples in this repository. We are currently migrating
repositories from scattered places into this organization.

## Alternatives

There is a [low-level API](https://github.com/grpc/grpc-haskell) in the official gRPC repository.
There also is a [more-complete API](https://github.com/awakesecurity/gRPC-haskell) which uses a binding to the C-library and which is supported by AwakeSecurity.
