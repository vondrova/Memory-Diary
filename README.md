# Memory Diary

A personal couple's web diary built with a **Haskell** (Servant + Persistent/PostgreSQL) backend and an **Elm** frontend.

## Features

- Timeline of shared memories with photos, tags, and locations
- Calendar view with monthly overview
- Relationship counter (anniversaries & monthiversaries)
- Couple profiles with avatars
- Birthdays and important-day reminders with countdown
- Notes notebook (per-person and shared)
- Bucket-list style plans with categories
- Statistics: recorded time, top tags, top places, monthly activity
- Soft-delete trash with restore
- Full-text and tag/location/date-range filtering
- Geocoding via Nominatim (OpenStreetMap)

## System requirements

| Tool | Version |
|------|---------|
| GHC / Stack | Stack ≥ 2.13, GHC 9.10 |
| Node.js | ≥ 18 |
| Elm | 0.19.1 (installed via npm) |
| PostgreSQL | ≥ 14 |
| libpq-dev | (Debian/Ubuntu: `apt install libpq-dev`) |

On Debian/Ubuntu install the native libraries needed for the Haskell build:

```bash
sudo apt install libpq-dev postgresql-client pkg-config
```

## Local development

### 1. Install frontend tools

```bash
npm install
npm run install:frontend
```

### 2. Build Elm frontend

```bash
npm run build
```

Or with a clean Elm cache:

```bash
npm run build:clean
```

### 3. Build & run Haskell backend

Requires a running PostgreSQL instance (see `docker-compose.yml` for a ready-made one):

```bash
docker compose up -d postgres
```

Then:

```bash
npm run backend:build
npm run start          # listens on http://localhost:3000
```

### 4. Run Haskell tests

```bash
cd backend
stack test
```

`stack test` requires `libpq-dev`. Pure unit tests run without a database.
Database integration tests run only when `TEST_DATABASE_URL` is set and the
database name ends in `_test`:

```bash
createdb memory_diary_test
TEST_DATABASE_URL=postgresql://memory_diary:memory_diary@localhost:5432/memory_diary_test stack test
```

Integration tests delete all data in this dedicated test database before each case.

## Docker (production)

The `Dockerfile` performs a multi-stage build: Elm frontend, Haskell backend, and a slim runtime image.

```bash
docker compose up --build
```

The app is then available on **http://localhost:3000**.

Environment variables consumed by the container:

| Variable | Default | Description |
|----------|---------|-------------|
| `DATABASE_URL` | `postgresql://memory_diary:memory_diary@postgres:5432/memory_diary` | PostgreSQL connection string |
| `STATIC_PATH` | `/app/static` | Directory with `main.js`, `index.html`, and `styles.css` |
| `PHOTOS_PATH` | `/data/photos` | Directory for uploaded photos |

## Architecture

### Backend

The backend follows a strict layered architecture:

```
HTTP layer   Handler/*     — request parsing, auth, HTTP error mapping
Domain layer Domain/*      — pure business logic, no IO
             Domain.Validation  — input validation rules
             Domain.Filter      — memory search predicates
             Domain.Aggregation — statistics computation
             Domain.Recurrence  — annual/monthly date arithmetic
Data layer   Models.hs     — Persistent schema (PostgreSQL)
             Types.hs      — JSON DTOs (input/output, decoupled from DB types)
```

Key design decisions:

- **Handler ↔ Domain separation**: handlers contain no business logic; they call Domain functions for validation and computation, then persist results.
- **Atomic soft-delete**: moving an item to the trash table and deleting it from its source table happen inside a single database transaction.
- **Servant type-safe routing**: the full API surface is declared in `Api.hs` as a Haskell type; the compiler rejects handler mismatches.
- **Schema migrations**: `Models.hs` drives automatic schema migration via Persistent's `migrateAll`. Legacy data transformations (`migrateLegacySchema`, `syncCatalogValues`, …) run once at startup.

### Frontend

The Elm SPA follows The Elm Architecture (TEA) with an explicit module split:

```
Update/     — one sub-module per domain entity (Memories, Notes, Plans, …)
Page/       — one view module per page, receives the whole Model
Api.elm     — all HTTP calls (Cmd Msg values)
Helpers.elm — pure formatting and domain-view helpers
```

All business logic lives on the backend. The frontend validates forms for UX only; the backend enforces the same rules independently.

## Testing

### Backend

```
Domain/*Spec.hs         Pure unit tests — no database required
Handler/IntegrationSpec — full CRUD + trash workflow per entity; requires
                          TEST_DATABASE_URL pointing to a *_test database
Handler/HelpersSpec     — deduplication and JSON name extraction
Handler/PhotosSpec      — filename safety validation
TypesSpec               — JSON round-trip for request/response types
```

Run all tests (unit only):

```bash
cd backend && stack test
```

Run with integration tests:

```bash
createdb memory_diary_test
TEST_DATABASE_URL=postgresql://memory_diary:memory_diary@localhost:5432/memory_diary_test \
  cd backend && stack test
```

### Frontend

```bash
npm run test:frontend
```

Covers: date/time formatting, photo filename safety, form validation, calendar helpers, zodiac sign lookup, countdown display, URL helpers, and calendar day filtering. 144 tests.

## Project structure

```
backend/          Haskell backend (Stack project)
  src/
    Api.hs        Servant API type definitions
    App.hs        Application monad (ReaderT + PostgreSQL pool)
    Models.hs     Persistent database schema
    Types.hs      JSON DTOs
    Server.hs     Route assembly
    Domain/       Pure business logic (Aggregation, Filter, Recurrence, Validation)
    Handler/      HTTP handlers (Memories, ImportantDays, Notes, Plans, Stats, ...)
  test/           HSpec unit tests for domain modules
frontend/         Elm 0.19 SPA
  src/
    Main.elm      Entry point
    Types.elm     Model, Msg, type aliases
    Init.elm      Initial model + flags
    Update.elm    Elm Architecture update function
    View.elm      Root view dispatcher
    Api.elm       HTTP calls
    DateUtils.elm Date and calendar utilities
    FormUtils.elm Shared form helpers
    Helpers.elm   Pure formatting and domain-view helpers
    Language.elm  Czech/English translations
    Routing.elm   URL/page mapping
    Api/          Decoders and encoders
    Page/         Per-page view modules
  index.html      Static HTML shell
  styles.css      Application styles
```

## Acknowledgements

Development was assisted by AI tools (Claude by Anthropic and OpenAI Codex) as well as community resources including Stack Overflow. 
