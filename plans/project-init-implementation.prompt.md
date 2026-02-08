# asm816 MVP Bootstrap Plan (Phase 1)

## Summary
Implement a full workspace bootstrap from the current single-file crate into `asm816_core` + `asm816_cli`, delivering a working 2-pass assembler MVP for a practical opcode subset, with diagnostics and raw `.bin` output.

Locked decisions:
- Opcode scope: phased MVP subset first.
- `.include`: defer support for now; warn and skip.
- `.org`: single-origin model.
- `fmt`: stubbed canonical formatter behavior (implemented only for MVP syntax).

## Public APIs and Interfaces
Create these public interfaces in `crates/asm816_core/src/lib.rs`:

- `pub struct AssembleOptions { pub cpu_mode: CpuMode, pub include_dirs: Vec<PathBuf> }`
- `pub struct CheckOptions { pub cpu_mode: CpuMode, pub include_dirs: Vec<PathBuf> }`
- `pub struct FmtOptions {}`
- `pub struct PipelineWarnings { pub diags: Vec<Diag> }`
- `pub fn assemble_path(input: &Path, out: &Path, opts: &AssembleOptions) -> Result<PipelineWarnings, Vec<Diag>>`
- `pub fn check_path(input: &Path, opts: &CheckOptions) -> Result<PipelineWarnings, Vec<Diag>>`
- `pub fn format_path(input: &Path, opts: &FmtOptions) -> Result<String, Vec<Diag>>`

CLI contract in `crates/asm816_cli`:
- `assemble <input> -o <out.bin> [-I <dir>...] [--a8|--a16] [--i8|--i16]`
- `check <input> [-I <dir>...] [--a8|--a16] [--i8|--i16]`
- `fmt <input>`

Exit behavior:
- Exit `0` when no `Error` diagnostics exist.
- Exit non-zero when any `Error` diagnostic exists.
- Warnings are printed but do not fail by themselves.

## Repository Restructure
1. Convert root `Cargo.toml` to a workspace with members:
`crates/asm816_core`, `crates/asm816_cli`.
2. Add `crates/asm816_core/Cargo.toml` with deps:
`logos`, `chumsky`, `ariadne`, `indexmap`, `rustc-hash`, `smallvec`.
3. Add `crates/asm816_cli/Cargo.toml` with deps:
`clap` and path dependency on `asm816_core`.
4. Keep root `README.md`; update examples to call `asm816_cli`.

## Core Module Plan (`asm816_core`)
1. `source`
- Implement `FileId`, `Span`, `Spanned<T>`, `SourceFile`, `SourceManager`.
- Load primary file and normalize newlines.
- Track include dirs for future use.
- If `.include` is encountered later, emit warning diagnostic and ignore directive effect.

2. `diag`
- Implement `Severity`, `DiagLabel`, `Diag`.
- Implement renderer `render_diags(&SourceManager, &[Diag])` using `ariadne`.

3. `lex`
- Implement `logos` tokenization with file spans and lexeme copies.
- Tokenize identifiers, numbers (`$hex`, `%bin`, decimal), string literals, punctuation/operators, newline/comments.

4. `expr`
- Implement AST: numeric/symbol/pc/unary/binary plus `<expr` and `>expr`.
- Implement evaluator returning `i64` or unresolved symbol errors.
- `^` is binary XOR in MVP; bank-byte extraction is deferred.

5. `ir`
- Implement line-oriented IR:
`Program`, `Item::{Label,Assign,Directive,Instruction,EmptyLine}`,
`Directive`, `Instruction`, `Operand`, `DirArg`.

6. `parse`
- Use chumsky for expression parser and line parsing glue.
- Parse directives:
`.org`, `.byte`, `.word`, `.asciiz`, `.res`, `.a8`, `.a16`, `.i8`, `.i16`, `.include`.
- Parse assignment `NAME = expr`, labels `NAME:`, instructions + operand shapes.
- Mnemonics/directives are case-insensitive in parse, normalized for encode/fmt.

7. `asm/pass1`
- Build symbol table and line layout with deterministic sizes.
- Enforce single-origin rule:
first `.org` allowed, repeated `.org` => error.
- `.res` and `.org` args must be pass1-evaluable or error.
- CPU mode state transitions via `.a8/.a16/.i8/.i16`.
- Record fixups for late-evaluated expressions (labels, branches, unresolved expressions).

8. `encode`
- Implement table-driven opcode map for MVP subset:
`LDA, LDX, LDY, STA, STX, STY, ADC, SBC, JMP, JSR, RTS, NOP, CLC, SEC, BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS`.
- Addressing modes in MVP:
`Imp, ImmA, ImmXY, Abs, AbsX, AbsY, Dp, DpX, DpY, IndX, IndY, Rel8`.
- Mode resolution policy:
known constants may choose Dp variants when valid; unresolved ambiguous operands default to Abs-size layout for determinism.

9. `asm/pass2`
- Evaluate expressions with final symbol table.
- Encode instructions/directives into `Vec<u8>`.
- Apply range checks:
`byte`, `word`, immediate widths, branch rel8.
- Branch out-of-range emits error diagnostic.
- `.include` emits warning and produces no bytes.

10. `fmt`
- Implement stub formatter:
parse to IR and reprint canonical style for supported syntax.
- Canonical output:
labels at col 0, instructions/directives indented 4 spaces, uppercase mnemonics/directives, normalized comma spacing.
- Unsupported constructs produce diagnostics instead of panic.

## CLI Plan (`asm816_cli`)
1. Build Clap app with `assemble`, `check`, `fmt`.
2. Map width flags to default `CpuMode` (default A8/I8).
3. Call core pipeline functions.
4. Print diagnostics through core renderer.
5. On `assemble`, write produced bytes to `-o` path only when no errors.

## Testing Plan
Add tests in `crates/asm816_core/tests` and module unit tests:

1. Lexer tests:
token kinds and spans for labels, directives, immediates, comments, strings.

2. Parser tests:
- Label + instruction line parsing.
- Assignment + expression precedence.
- Directive argument parsing.
- Invalid syntax diagnostics include correct spans.

3. Pass1 tests:
- Forward-referenced labels produce fixups.
- `.org` repeated triggers single-origin error.
- `.res` unknown expression errors.
- CPU mode transitions affect immediate sizing metadata.

4. Pass2 encoding tests:
- Small program emits exact bytes.
- Branch forward/backward in range encodes correctly.
- Branch out-of-range emits error.
- `.byte/.word/.asciiz/.res` emit expected data.

5. Deferred include behavior:
- `.include "x.s"` emits warning diagnostic and zero output effect.

6. CLI integration tests:
- `assemble` success writes file and returns zero.
- `check` reports diagnostics and no output write.
- `fmt` returns canonicalized text for supported syntax.

## Acceptance Mapping
This plan satisfies MVP with one explicit deferral policy:
- `.include` behavior is present but non-functional by design (warning + skip).
- All other required directives and 2-pass behavior remain implemented.
- Diagnostics stay source-spanned and rendered with ariadne.
- Output remains raw `.bin` generated from in-memory bytes.

## Assumptions and Defaults
- Default CPU mode is `.a8` + `.i8` unless flags override.
- Number literals accepted: decimal, `$hex`, `%binary`.
- Immediate width is driven only by CPU mode directives/flags.
- No optimizer passes; encode decisions remain deterministic.
- Single-file primary input model in MVP; include dirs accepted now for forward compatibility.
- Formatter is intentionally limited to constructs implemented in MVP.
