# Memory Diary

A personal web application for recording and rediscovering life memories, built with a Haskell backend and an Elm frontend.

## Project Goal

Memory Diary is a shared diary for a couple or a small private group. The application stores memories, photos, notes, plans, relationship milestones, important dates, and derived statistics in one place. The frontend is responsible for presentation and user interaction only; business rules, persistence, filtering, aggregation, recurrence calculations, trash handling, and file storage are implemented on the Haskell backend.

## Functional Requirements

- Users can create, edit, list, and soft-delete memory entries. Each memory has a title, a start and end date/time, and optional description, location, tags, and photos.
- Deleted memories, notes, plans, and important days are moved to trash. Users can restore them or permanently delete trash entries.
- Users can browse memories in a chronological timeline and in a monthly calendar view.
- Users can filter memories by text query, tag, location, start date, and end date. Filters can be combined and are evaluated by the backend.
- Users can see "On This Day" memories from previous years for the current date.
- Users can upload photos. The backend stores uploaded files and returns public file paths used by the frontend.
- Users can manage notes for each person or for shared information.
- Users can manage plans grouped by category and marked as done or pending.
- Users can rename and delete tags, locations, and plan categories with backend-side bulk operations.
- Users can store relationship metadata, including start date and heart colour. The backend computes days together, next yearly anniversary, next monthly anniversary, and ordinal anniversary numbers.
- Users can maintain two profile avatars, including display mode, optional photo, figure colour, accessory, expression, and birthday.
- Users can store manual important days and birthdays. The backend computes recurring upcoming dates where applicable.
- Users can view statistics computed by the backend: total recorded time, number of memories, number of photos, visited places, average memory duration, longest memory, most used tags, most used locations, and monthly activity.
- Users can search address suggestions through the backend geocoding endpoint.

## Non-Functional Requirements

- The repository contains both the approved project description and the implementation.
- All source code, comments, documentation, and project metadata are written in English.
- The backend is a Stack-based Haskell project.
- The backend exposes a JSON API using Servant and stores data through Persistent/PostgreSQL.
- The frontend is an Elm single-page application that consumes the backend API.
- The Elm frontend can be compiled into production static assets.
- CI/CD is configured with GitHub Actions and runs frontend build, Elm tests, backend build, backend tests, and Haddock generation.
- The code is split into domain modules, API handlers, DTO types, and presentation modules so that business logic stays on the backend and rendering stays on the frontend.
- The backend has Haddock-compatible documentation comments for public modules and functions where useful.
- The project can be run either locally or with Docker Compose.
- Tests cover the key pure backend domain logic, JSON API contracts, Elm helper functions, and Elm decoders.

## Architecture

- `backend/src/Api.hs` defines the Servant API surface.
- `backend/src/Domain/*` contains pure business logic such as validation, recurrence, filtering, and aggregation.
- `backend/src/Handler/*` contains HTTP handlers that validate inputs, execute database operations, and convert database entities to DTOs.
- `backend/src/Models.hs` defines Persistent database models.
- `backend/src/Types.hs` defines the public JSON DTOs used between backend and frontend.
- `frontend/src/Api.elm` contains HTTP commands and request wiring.
- `frontend/src/Api/Decode.elm` and `frontend/src/Api/Encode.elm` contain JSON decoding and encoding.
- `frontend/src/Page/*` contains page-specific views.
- `frontend/src/DateUtils.elm`, `frontend/src/FormUtils.elm`, `frontend/src/Language.elm`, and `frontend/src/Routing.elm` contain focused frontend-only helpers for dates, forms, translations, and URL mapping.
- `frontend/src/Update.elm`, `frontend/src/Init.elm`, `frontend/src/View.elm`, and `frontend/src/Types.elm` implement the Elm application state, messages, update logic, and rendering.
- `frontend/index.html` and `frontend/styles.css` provide the static HTML shell and styling for the compiled Elm application.


## Acknowledgements

Development was assisted by AI tools (Claude by Anthropic and OpenAI Codex) as well as community resources including Stack Overflow.
