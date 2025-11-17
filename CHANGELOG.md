# Changelog for `TodoistSDK`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Released

## 0.1.2.1 - 2025-11-17

### Added
- Initial release of TodoistSDK
- Tagless final/mtl-style architecture with type class-based operations
- Two interpreters: TodoistIO (real HTTP requests) and Trace (operation recording)
- Complete API coverage for Projects (CRUD, archive/unarchive, collaborators)
- Complete API coverage for Tasks (CRUD, complete/uncomplete, move operations)
- Complete API coverage for Comments (CRUD for projects and tasks)
- Complete API coverage for Sections (CRUD operations)
- Complete API coverage for Labels (CRUD, shared labels)
- Builder pattern for ergonomic request construction
- Cursor-based pagination support with automatic and manual modes
- Comprehensive test suite (unit tests + integration tests)
- MIT license

### Known Limitations
- REST API v2 is used (v1 is deprecated by Todoist)
- Limited error types (BadRequest, NotFound, Forbidden, Unauthorized, HttpError)
- Some Trace interpreter methods not implemented
- No CI/CD pipeline configured
- Minimal README documentation

### Dependencies
- base >= 4.7 && < 5
- text >= 2.1.2
- transformers >= 0.6.1.1
- req >= 3.13.4
- bytestring >= 0.12.2.0
- aeson >= 1.5