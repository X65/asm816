---
title: Assembly Module System Specification
status: draft
audience: compiler/assembler implementers
scope: module system, imports/exports, pub macros, inner modules, build pipeline, diagnostics, tests
assumptions:
  - assembler uses logos (lexer) + chumsky (parser) + ariadne (diagnostics)
constraints:
  - file defines module (no interface/helper files)
  - module names are lowercase snake_case
  - no directory-index modules (no foo/mod.asm style)
  - allow inner modules within a file using `mod name { ... }`
  - pub macros are importable and expand at call site, resolving names in caller scope
  - importing scope must provide all dependencies for pub macros (no implicit imports)
---

# Goals

Implement a real module system for an assembler without textual `.include` for cross-file symbol sharing:

- File-based modules: creating a file is enough to define a module.
- Stable namespacing via module paths derived from filesystem layout.
- Separate compilation within one build session via an in-memory global module index (no `.ami` or other interface artifacts).
- `pub` exports control what is visible cross-module (symbols and macros).
- Macros behave like inline functions: importable, expanded at call site, resolved in the caller's scope.

# Non-goals

- No directory-index modules (no implicit directory module, no `foo/mod.asm` semantics).
- No `mod name;` forward declarations or "parent must declare children".
- No implicit imports during macro expansion.
- No requirement to support compiling against libraries without sources (can be added later with an optional interface format, but not part of v1).

# Terminology

- Module root: configured directory that is the base for module resolution.
- Module path: `seg1::seg2::...::segN`, where each segment is lowercase snake_case.
- Compilation unit: one `.asm` file.
- Inner module: module defined inside a file via `mod name { ... }`.
- Build session: one assembler invocation that loads/parses/caches all modules in a workspace.

# Naming rules

## Module segment grammar

Each segment MUST match:

- `^[a-z][a-z0-9_]*$` (lowercase snake_case)

## Filesystem mapping

Given a module root `ROOT`:

- File `ROOT/foo.asm` defines module `foo`
- File `ROOT/foo/bar.asm` defines module `foo::bar`
- File `ROOT/foo/bar/baz.asm` defines module `foo::bar::baz`

Directories are namespaces only; they do not define modules.

Explicitly supported layout:

- `foo.asm` may exist alongside `foo/*.asm` submodules.

Not supported:

- directory-index modules (no `ROOT/foo/mod.asm` special meaning)

## Optional module header

A source file MAY begin with:

- `module <module_path>`

If present, it MUST equal the canonical module path derived from the file path.
If missing, the canonical module path is assumed.

# Source language surface

## Exports

- `pub` marks a declaration as exported.
- Only exported items are visible to other modules.

Exportable item kinds (minimum):

- Labels/symbols (code/data)
- Constants
- Macros

Everything not marked `pub` is module-private and must not be importable.

## Inner modules

A file may define inner modules using:

- `mod <name> { ... }`

This defines a module:

- `<current_module>::<name>`

Rules:

- `<name>` MUST match module segment grammar.
- Inner modules can nest: `mod a { mod b { ... } }` defines `<current>::a::b`.
- Inner modules are part of the enclosing compilation unit; they do not correspond to separate files.

Conflict rule (hard error):

- If a module path is defined both by an inner module and by a file module, the build MUST fail.

Example conflict:

- `ROOT/foo.asm` defines `mod util { ... }` -> `foo::util`
- `ROOT/foo/util.asm` exists -> `foo::util`
- Error: duplicate module definition `foo::util`

## Imports

### Module import

- `import <module_path> [as <alias>]`

Semantics:

- Makes the module available for qualified lookup in the importing module.
- Optional `alias` introduces a local module name for qualified access.

Import resolution is filesystem-based:

- `import foo::bar` resolves to `ROOT/foo/bar.asm` under configured roots.
- If not found: error (no fallback magic, no parent declaration system).

### Selective import (optional but recommended)

- `use <module_path>::{a, b as c, ...}`

Semantics:

- Brings selected exported names into unqualified lookup scope.
- Only works for exported (`pub`) items.

No `open` import is specified for v1.

# Name lookup and symbol identity

## Qualified names

Qualified reference forms:

- `<module_path>::<exported_name>`
- `<alias>::<exported_name>`

Resolution rules:

- The referenced module MUST be explicitly imported in the current module.
- The referenced name MUST be exported by that module.

## Unqualified names

Unqualified lookup searches, in order:

1. Local scope (labels, locals, constants)
2. Names introduced by `use`
3. Optional minimal builtins/prelude (if any)

Unqualified lookup MUST NOT search other modules unless explicitly introduced via `use`.

## Global uniqueness (mangling)

Internally, the assembler MUST mangle global/link symbols to avoid collisions across modules.

Example deterministic scheme:

- `foo::bar::baz` -> `__M__foo__bar__baz`

Mangling must be deterministic and suitable for diagnostics/debug info mapping.

# Macro system (call-site expansion, caller-scope resolution)

## Macro export/import

- Macros can be marked `pub`.
- `pub` macros are importable via `import` and usable via qualified names (and optionally via `use`).

## Expansion model (critical)

Imported macros expand in the caller's scope (call-site expansion):

- All identifier resolution inside the macro body occurs as if the macro text was written at the expansion site.
- The macro does NOT capture the defining module's scope.
- The importing scope MUST provide all dependency symbols (via local definitions and/or explicit imports and/or `use`).

## No implicit imports

Macro expansion MUST NOT cause implicit module imports.
If macro expansion references `x::y::z`, the caller must have `import x::y` (or an alias) in scope, otherwise error:

- "missing import required by macro expansion: import x::y"

## Macro body representation

Because there are no `.ami` interface files, macro bodies must be retained in-memory across the build session.
Store macro bodies as token trees (or AST fragments) with spans for diagnostics.

## Minimal hygiene (recommended)

Provide at least one mechanism to avoid label collisions:

- Macro-local labels syntax: `%%label` expands to a unique gensym per macro expansion.

Without this, macro correctness becomes fragile in assembly.

## Optional diagnostics metadata (recommended)

Allow macros to include a non-binding `requires` hint:

- Used only to improve error messages; it never auto-imports.

Example:

- `pub macro m(...) requires { import foo::bar; use foo::bar::{x}; } { ... }`

# Build model (no interface artifacts)

The assembler operates on a workspace in one invocation and builds an in-memory module index.

## Required property: parse once, reuse

The assembler MUST cache parsed module files in memory for the duration of the build session and reuse those results across resolution/expansion/codegen.

## Two-phase compilation (required)

### Phase 1: Discover + parse + index

1. Discover all `.asm` files under configured roots and map each to a canonical module path.
2. Parse each module once (cache tokens + AST).
3. Walk the AST to extract:
   - `pub` exports (symbols, consts, macros)
   - inner modules (`mod {}`) and their exports, recursively
   - import/use declarations (for dependency checks and better diagnostics)
4. Validate globally:
   - module path segments are lowercase snake_case
   - optional `module` header matches canonical path
   - no duplicate module definitions (file vs inner, or multiple roots defining same module path)

Output: `GlobalIndex` mapping:

- module path -> exports table
- (module path, macro name) -> macro definition token tree + spans
- module path -> inner module interfaces (if any)
- module path -> import/use declarations

### Phase 2: Assemble

For each module to assemble:

1. Build an `ImportEnv` from its `import`/`use` declarations and the `GlobalIndex`.
2. Assemble the module, expanding macros by fetching their definitions from `GlobalIndex` and expanding at call site with caller-scope resolution.
3. Produce object output (`.o`/`.obj`) and any optional debug maps.

Cycles:

- Import cycles are permitted as long as all referenced items are `pub` exports present in the index.
- Macro expansion must still obey "no implicit imports"; errors are per expansion site.

# Diagnostics requirements (ariadne)

All diagnostics must include:

- module path
- file path
- span (byte range) into the correct source file
- actionable suggestion where possible

Required errors:

- invalid module name segment (not lowercase snake_case)
- `module` header mismatch with canonical path (if header present)
- module not found (import path has no corresponding file)
- duplicate module definition (file vs inner module, or multiple roots)
- access to non-exported item across modules
- missing import required by qualified lookup
- macro expansion missing import / missing symbol at call site
- ambiguous resolution across roots (same module path in multiple roots)

Macro diagnostics should show:

- error at expansion site
- note pointing to macro definition site
- optional expansion trace stack for nested macro calls

# Tests (golden style recommended)

Create tests validating:

- Filesystem mapping -> module paths (including foo.asm + foo/*.asm coexistence)
- `pub` visibility boundaries (private items not accessible)
- Qualified references require explicit `import`
- `use` introduces unqualified names and supports aliasing
- Inner modules: `mod {}` create `<parent>::<child>` modules
- Conflict: inner module vs file module definition is rejected
- Macro semantics (critical):
  - Imported `pub macro` expands in caller scope
  - Macro depends on unqualified symbol `helper`
    - caller defines `helper` -> success
    - caller does not define/import it -> error
  - Macro contains qualified reference `x::y::z`
    - caller imports `x::y` -> success
    - caller does not import -> error with "add import x::y"
  - `%%label` generates unique labels per expansion

Golden outputs:

- expected assembled bytes and/or expected diagnostics output
- ensure deterministic ordering in error reporting and symbol dumps

# Implementation plan (agent tasks)

1. Workspace discovery:
   - scan roots for `.asm`, build module_path -> file_path map
   - detect duplicates across roots

2. Lexer/parser:
   - parse `module`, `import`, `use`, `pub`, `mod name {}` and macro definitions/calls
   - produce spans compatible with ariadne

3. In-memory caches:
   - token cache (logos)
   - AST cache (chumsky)

4. GlobalIndex build:
   - extract exports and macro definitions (token trees)
   - extract inner modules recursively
   - validate naming and duplicates

5. Resolver:
   - build ImportEnv per module
   - enforce explicit imports for qualified access
   - enforce `pub` visibility

6. Macro engine:
   - expand macros at call site with caller-scope resolution
   - implement `%%label` gensym
   - implement error reporting with definition-site notes

7. Assembler backend integration:
   - mangle global symbols with module paths
   - ensure relocations/debug info map back to unmangled names for UX

8. Test suite:
   - golden tests for bytes and diagnostics
   - macro semantic tests are required blockers

# Acceptance criteria

- No `.include` is required for sharing symbols between modules.
- Creating a `.asm` file defines a module; `pub` is the only export mechanism.
- `import` is explicit and required for qualified cross-module access.
- `pub macro` expansion matches call-site semantics and resolves in caller scope with no implicit imports.
- Duplicate module definitions and missing imports are caught deterministically with precise diagnostics.
- Tests cover the critical semantics and prevent regressions.
