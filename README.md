# zuul

A zuul client library.

Main [documentation](https://docs.softwarefactory-project.io/zuul-haskell/)


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
