FROM docker.io/alpine:latest as builder

RUN apk update

RUN apk add build-base curl numactl-dev zlib-dev gmp gmp-dev

RUN apk add postgresql17 postgresql17-contrib postgresql17-openrc libpq libpq-dev

RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup

RUN chmod +x /usr/bin/ghcup

ARG GHC=9.10.1
ARG CABAL=3.14

RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

WORKDIR /src

COPY . .

RUN cabal update && cabal build

FROM docker.io/alpine:latest

RUN apk add gmp gmp-dev libpq

WORKDIR /app

COPY --from=builder /src/dist-newstyle/build/x86_64-linux/ghc-9.10.1/backend-0.1.0.0/x/backend/build/backend/backend .

EXPOSE 3000

COPY .env .

CMD ["./backend"]
