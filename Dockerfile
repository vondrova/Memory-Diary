# Stage 1: Build Elm frontend
FROM node:20-bookworm-slim AS elm-build

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Cache npm packages separately from Elm sources
COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci

# Pre-download Elm packages before copying sources so that the package-download
# layer is cached independently of source changes.  A minimal Main that imports
# a few core libraries is enough to trigger a full package-registry fetch for
# all transitive dependencies listed in elm.json.
COPY frontend/elm.json ./
RUN mkdir -p src && \
    echo 'module Main exposing (main)' > src/Main.elm && \
    echo 'import Html' >> src/Main.elm && \
    echo 'main = Html.text ""' >> src/Main.elm && \
    npx elm make src/Main.elm --output=/dev/null 2>&1; \
    rm src/Main.elm

# Build the real application
COPY frontend/src ./src
RUN npx elm make src/Main.elm --output=main.js


# Stage 2: Build Haskell backend
FROM haskell:9.10-bookworm AS haskell-build

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev postgresql-client pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY backend/stack.yaml backend/package.yaml ./

# Stub sources so "stack build --only-dependencies" can parse the project
RUN mkdir -p app src test && \
    touch README.md CHANGELOG.md && \
    printf 'module Main (main) where\nmain :: IO ()\nmain = return ()\n' > app/Main.hs && \
    printf 'module Api where\n'    > src/Api.hs    && \
    printf 'module Models where\n' > src/Models.hs && \
    printf 'module Server where\n' > src/Server.hs && \
    printf 'main :: IO ()\nmain = return ()\n' > test/Spec.hs

RUN stack build --install-ghc --only-dependencies

# Copy real sources, run tests, and install the binary
COPY backend/ ./
RUN stack test --install-ghc
RUN stack build --install-ghc --copy-bins --local-bin-path /usr/local/bin


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
ENV STATIC_PATH=/app/static
ENV PHOTOS_PATH=/data/photos

EXPOSE 3000

CMD ["/app/memory-diary-exe"]
