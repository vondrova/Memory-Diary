# Changelog for `backend`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2026-06-21

### Added

- Servant JSON API for memories, notes, plans, important days, relationship, profiles, stats, geocoding, trash, tags, locations, and plan categories
- Persistent/PostgreSQL database schema with automatic migration via `migrateAll`
- `Domain.Validation` — pure validation rules for all input types; returns `ValidationError` for blank required fields, reversed time ranges, and invalid note owners
- `Domain.Filter` — pure predicate-based filtering of memory rows by text query, tag, location, and date bounds
- `Domain.Recurrence` — pure computation of next anniversary, next monthiversary, and ordinal numbers for relationship milestones; recurring upcoming dates for important days
- `Domain.Aggregation` — pure statistics aggregation over memory rows: total recorded time, memory count, photo count, visited places, average duration, longest memory, top tags, top locations, monthly activity
- `Handler.Memories` — CRUD endpoints for memories, including photo association and bulk tag/location operations; soft-delete to trash
- `Handler.ImportantDays` — CRUD endpoints for manually stored important days; backend computes recurring next occurrence and days-until values
- `Handler.Notes` — CRUD endpoints for per-person and shared couple notes; owner normalisation to stable keys (`left`, `right`, `shared`)
- `Handler.Plans` — CRUD endpoints for couple plans grouped by category; category rename and delete with bulk plan updates
- `Handler.Stats` — stats endpoint backed by `Domain.Aggregation`
- `Handler.Tags` — bulk rename and delete endpoints for tags, locations, and plan categories
- `Handler.Trash` — list, restore, and permanently delete soft-deleted entries
- `Handler.Photos` — multipart upload endpoint that stores photos in `PHOTOS_PATH` and serves them as static files
- `Handler.Geocode` — proxy for Nominatim address search
- `Handler.Relationship` — CRUD for relationship metadata (start date, heart colour) and person profiles (avatar figure, photo, birthday)
- Legacy migration helpers for older database schemas (`migrateLegacySchema`, `migrateLegacyNoteOwners`, `migrateLegacyTrashKinds`, `syncCatalogValues`)
- Hspec test suite: unit tests for all `Domain.*` modules, handler helper tests, photo upload tests, JSON round-trip tests via `TypesSpec`, and integration tests covering full CRUD and trash workflows
- GitHub Actions CI pipeline: Elm build, Elm tests, elm-format check, Haskell build, Haskell tests, fourmolu check, Haddock generation
- Multi-stage Dockerfile and Docker Compose configuration
