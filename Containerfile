FROM quay.io/software-factory/zuul-worker-haskell as builder

COPY README.md LICENSE zuul.cabal /tmp/src
COPY app/ /tmp/src/app
COPY test/ /tmp/src/test/
COPY src/ /tmp/src/src/
RUN cd /tmp/src && cabal build && cabal install --installdir=/bin

FROM registry.fedoraproject.org/fedora:33
COPY --from=builder /bin/zuul-cli /bin/
CMD /bin/zuul-cli
