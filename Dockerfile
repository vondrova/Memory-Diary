# Stage 1: Build Elm frontend
FROM node:20-bookworm-slim AS elm-build

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

RUN curl -sL https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    | gunzip > /usr/local/bin/elm && chmod +x /usr/local/bin/elm

COPY frontend/elm.json ./
COPY frontend/src ./src

RUN elm make src/Main.elm --output=main.js

# Stage 2: Build Haskell backend
FROM haskell:9.10-bookworm AS haskell-build

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev postgresql-client pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY backend/stack.yaml backend/package.yaml ./

RUN mkdir -p app src/Domain test && \
    touch README.md CHANGELOG.md && \
    printf 'module Main where\nimport Lib\nmain :: IO ()\nmain = startApp\n' > app/Main.hs && \
    printf 'module Lib where\nstartApp :: IO ()\nstartApp = return ()\n' > src/Lib.hs && \
    printf 'module Types where\n' > src/Types.hs && \
    printf 'module Models where\n' > src/Models.hs && \
    printf 'module Domain.Recurrence where\n' > src/Domain/Recurrence.hs && \
    printf 'module Domain.Aggregation where\n' > src/Domain/Aggregation.hs && \
    printf 'import Test.Hspec\nmain :: IO ()\nmain = hspec (return ())\n' > test/Spec.hs && \
    printf 'module LibSpec where\nimport Test.Hspec\nspec :: Spec\nspec = return ()\n' > test/LibSpec.hs

RUN stack build --install-ghc --only-dependencies

COPY backend/ ./
RUN stack test --install-ghc
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Stage 3: Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates libgmp10 libpq5 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=haskell-build /usr/local/bin/backend-exe /app/memory-diary-exe
COPY --from=elm-build      /app/main.js               /app/static/main.js
COPY frontend/index.html /app/static/index.html
COPY frontend/styles.css /app/static/styles.css

ENV DATABASE_URL=postgresql://memory_diary:memory_diary@postgres:5432/memory_diary

EXPOSE 3000

CMD ["/app/memory-diary-exe"]
