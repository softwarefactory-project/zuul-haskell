# zuul

[![Hackage](https://img.shields.io/hackage/v/zuul.svg)](https://hackage.haskell.org/package/zuul)

A client library for [zuul](https://zuul-ci.org).

## Features

- JSON decoder for API endpoints.
- HTTP client helper functions.
- Command line utility to compute informations.
  - live-changes: count the number of change running in a pipeline.
  - nodepool-labels: get the label used in project pipelines.


## Contribute

Run tests:

```
$ cabal build
$ cabal test
```

Run cli:

```
$ cabal run zuul-cli -- --help
```

Install cli:

```
$ cabal install --installdir=~/.local/bin
```

Build container:

```
$ podman build -t quay.io/software-factory/zuul-stats .
```
