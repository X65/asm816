# Project Guidelines

## Code Style

- **Rust**: 2024 edition, standard conventions with explicit types (e.g., `FileId(u32)`, `Span` as `Range<BytePos>`). Use `rustfmt` for formatting; enforce with `cargo fmt --check` in CI/builds. Prefer small allocations (e.g., `smallvec` for lists).
- **Assembly**: Canonical ca65 syntax ([cc65 docs](https://cc65.github.io/doc/ca65.html)). Formatter normalizes to: labels at column 0 with `:`, instructions indented 4 spaces, normalized casing, single space after commas, comments `;` with ≥2 spaces. Input permissive, output canonical.

## Architecture

- Multi-crate: `asm816_core` (logic), `asm816_cli` (CLI). Components: SourceManager (files/includes), lexer (logos), parser (chumsky), IR (line-oriented AST), assembler (2-pass), encoder (opcodes), diagnostics (ariadne).
- Data flow: CLI → SourceManager → Lexer → Parser → Pass1 (layout/symbols/fixups) → Pass2 (encode) → Vec<u8> .bin + Diags.
- Key types: `Spanned<T>`, `Diag` (with spans), `SymTab` (IndexMap), `CpuMode`, `Fixup`. IR close to assembly (e.g., `Program` with `Item` enum).

## Build and Test

- Build: `cargo build --release`
- Test: Snapshot-style (assemble snippets, compare bytes/diags). Use `cargo test` for units once implemented.

## Project Conventions

- Naming: PascalCase types, snake_case fields. Mnemonics case-insensitive, normalized. Symbols in IndexMap for stable iteration.
- Structures: Line-oriented IR, 2-pass assembler. Directives `.`-prefixed (e.g., `.org`, `.byte`). CPU modes via `.a8/.a16/.i8/.i16` (explicit, no inference MVP).
- Errors: `Diag` with spans, ariadne rendering. Exit 0 success, non-zero errors.

## Integration Points

- Crates: `clap` (CLI), `logos` (lexer), `chumsky` (parser), `ariadne` (diags). Recommended: `indexmap`, `rustc-hash`, `smallvec`.
- Syntax: ca65-compatible, outputs raw `.bin`.

## Security

- Standard Rust safety. Validate include paths to prevent traversal. No network calls.</content>
  <parameter name="filePath">/home/smoku/devel/X65/asm816/.github/copilot-instructions.md
