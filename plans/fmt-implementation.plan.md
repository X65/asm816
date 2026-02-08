---
title: Implement asm816 formatter (fmt) with a pretty-printer (no options)
project: asm816
kind: prompt
audience: agentic-coding
timezone: Europe/Warsaw
decisions:
  - "Pretty-printer crate: pretty"
  - "Line width: 100 columns (fixed)"
  - "Indent: 4 spaces (fixed)"
  - "Comments: preserved (standalone comment lines + trailing comments)"
success_criteria:
  - "asm816 fmt is deterministic: same source => identical formatted output"
  - "Formatter is IR-driven: IR -> Doc -> String (no ad-hoc concat as the main strategy)"
  - "Comments are preserved in output"
  - "At least 10 golden formatting tests exist and pass"
constraints:
  - "No formatter options/flags/config in this task"
  - "No dialect support"
  - "No nightly features"
---

# Task summary

Implement `asm816 fmt` as a deterministic source formatter for asm816 assembly.

Pipeline:

1) parse source into IR (logos + chumsky already chosen)
2) preserve comments into IR (or comment side-structures referenced by spans)
3) build a `pretty` Doc from IR
4) render with fixed width 100 and indent 4 spaces
5) output formatted text

No user-configurable options in this task.

# Chosen library

Use `pretty` as the formatter core (Wadler/Leijen document combinators).

- Dependency: <https://crates.io/crates/pretty>

Do not integrate any formatter platform (no `dprint-core`) in this task.

# Formatting rules (fixed)

Assembly layout:

- Labels start at column 0: `label:`
- Instruction/directive lines:
  - 4 leading spaces
  - mnemonic/directive
  - if operands exist: single space then operands
  - if trailing comment exists: two spaces then `;` comment text
- Standalone comment lines:
  - preserved exactly as comment text content, emitted as:
    - `; ...` at column 0 (no forced indentation)
- Always end file with a single trailing newline.

Whitespace normalization:

- Collapse runs of spaces/tabs in code areas to single spaces as needed by the rules above.
- Do not attempt column alignment (no mnemonic/operand/comment columns beyond the simple rule above).

Line wrapping:

- Use `pretty` grouping with fixed width 100.
- If a line would exceed width, break at safe separators (commas for operand lists, or between operands for addressing forms where applicable).
- Never reflow comment text; if a comment makes a line exceed width, keep it as-is.

# Comment preservation strategy (must implement)

Preserve comments in output.

Support:

1) Standalone comment lines:
   - lex as comment tokens
   - represent in IR as `Stmt::CommentLine(String)`
2) Trailing comments on code lines:
   - lex comment token with same-line association
   - represent in IR as `Stmt::{Instruction|Directive|Assign|Label...} { trailing_comment: Option<String> }`

Notes:

- If the current parser discards line/column info, add minimal span/line tracking required to associate a comment with the statement on the same line.
- Keep this minimal: only handle `;` line comments for now.

# Required code changes

## 1) Add formatter module

Location recommendation:

- `crates/asm-core/src/fmt.rs` (+ `mod fmt;` re-export as needed)

Public API (no options):

- `pub fn format_ir(ir: &Ir) -> String`
- `pub fn format_source(src: &str, filename: &str) -> Result<String, Error>`
  - Parses to IR using existing pipeline, then formats.
- If CLI exists:
  - `asm816 fmt <file>` reads file, formats, writes back only if changed.
  - No flags.

## 2) Integrate `pretty`

Implement IR -> Doc using `pretty::Arena`:

- `fn doc_stmt(...) -> DocBuilder`
- `fn doc_expr(...) -> DocBuilder` (for expressions used in operands/directives)

Use:

- `text()`, `line()` / `softline()`, `hardline()`
- `nest(4, ...)` for blocks when introduced later
- `group()` to allow flattening vs breaking

Render:

- fixed width: 100

## 3) Ensure IR captures comments

Update lexer and parser to keep comment tokens and attach them as described above.
If easiest, parse to a statement stream that includes comment statements explicitly.

# Golden tests (formatter)

Add formatting goldens separate from assembly-output goldens.

Directory structure:

- `tests/fmt/<case_name>/input.s`
- `tests/fmt/<case_name>/expected.s`

Test behavior:

- read input.s
- run `format_source`
- compare to expected.s byte-for-byte

Add at least 10 cases covering:

- labels + instructions
- directives: `.byte`, `.word`, `.asciiz`, `.res`, `.org`
- blank lines handling
- standalone comment lines
- trailing comments
- expression spacing (e.g. `LABEL+1`, `($20),Y`, `#$10`)
- at least 2 cases that exceed 100 columns to validate deterministic wrapping

# Definition of done

- `cargo test` runs formatter goldens and they pass.
- `asm816 fmt <file>` formats deterministically with:
  - width 100
  - indent 4
  - preserved comments
- Implementation uses `pretty` Doc combinators as the core layout mechanism.
