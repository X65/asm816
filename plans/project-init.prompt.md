---
title: "asm816 - WDC 65816/6502 assembler (Rust)"
canonical_syntax: "https://cc65.github.io/doc/ca65.html"
outputs:
  - ".bin (raw bytes, Vec<u8>)"
tooling:
  lexer: "logos"
  parser: "chumsky"
  diagnostics: "ariadne"
  cli: "clap"
non_goals:
  - "No optimization"
  - "No linker/object emission in v1 (raw .bin only)"
  - "No full dialect compatibility upfront (accept common subset, print canonical ca65 style)"
---

# Goals

- Implement a simple, direct translating assembler for WDC 65816 with strong 6502 compatibility.
- Canonical syntax is ca65 (source of truth): https://cc65.github.io/doc/ca65.html
- Accept a practical subset of common 6502/65816 syntax variants, but normalize output to canonical ca65-like formatting.
- Provide good diagnostics with multi-file spans via ariadne.
- Implement a 2-pass assembler with a stable IR and fixups, emitting a raw `.bin` from a `Vec<u8>`.

# Workspace layout

- `crates/asm816_core`
  - `source/` - file loading, `.include` resolution, source storage
  - `lex/` - logos lexer
  - `parse/` - chumsky parser -> IR
  - `expr/` - expression AST + evaluator
  - `ir/` - IR types shared across pipeline
  - `asm/` - pass1/pass2, symbol table, fixups
  - `encode/` - opcode table + operand encoders
  - `diag/` - internal Diag model + ariadne rendering
  - `fmt/` - canonical formatter (optional in MVP; stub is fine)
- `crates/asm816_cli`
  - `main.rs` - clap CLI, calls core pipeline, writes `.bin`, prints diagnostics

# External crates

Required:

- `clap` - CLI
- `logos` - lexer
- `chumsky` - parser
- `ariadne` - diagnostics rendering

Recommended:

- `indexmap` - deterministic symbol maps (stable iteration)
- `rustc-hash` - fast hash maps (symbols/opcode lookup)
- `smallvec` - reduce allocations for small lists (labels, args)

# Data flow

```
CLI (clap)
  │ args: input files, include paths, output, dialect options, cpu mode defaults
  ▼
Source manager (source)
  │ reads main + .include graph
  │ assigns FileId, stores text, normalizes newlines
  ▼
Lexer (logos)
  │ SourceText → Vec<Token{kind, span}>
  ▼
Parser (chumsky)
  │ Tokens → IR (line-oriented)
  │   - LabelDef(name, span)
  │   - Assign(name, expr, span)
  │   - Directive(name, args, span)
  │   - Instr(mnemonic, operand_ast, span)
  │ + collects parse Diags
  ▼
Pass 1 (asm/pass1)
  │ IR → Layout
  │   - PC tracking, org/segment changes
  │   - define symbols, compute sizes
  │   - record Fixups: { span, pc, kind, expr }
  │   - track A/XY width state if enabled (.a8/.a16/.i8/.i16, optional smart)
  ▼
Pass 2 (asm/pass2 + expr + encode)
  │ IR + Symbols → Bytes
  │   - evaluate exprs (now resolved)
  │   - choose addressing mode + opcode
  │   - write operand bytes
  │   - resolve relative branches / range checks
  │   - emit Diags on overflow/undefined/invalid mode
  ▼
Output writer
  │ - raw binary (Vec<u8> → file)
  │ - optional listing / map file (pc ↔ source spans)
  ▼
Diagnostics (ariadne)
  │ Vec<Diag> → Report(s) printed using your Sources cache
```

# Proposed core data types (Rust)

## Spans and source IDs

```rust
pub type BytePos = usize;
pub type Span = std::ops::Range<BytePos>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub file: FileId,
    pub span: Span,
    pub value: T,
}
```

## Source storage

```rust
pub struct SourceFile {
    pub path: std::path::PathBuf,
    pub text: String,
}

pub struct SourceManager {
    files: Vec<SourceFile>,                 // index is FileId.0
    path_to_id: std::collections::HashMap<std::path::PathBuf, FileId>,
    include_dirs: Vec<std::path::PathBuf>,
}
```

## Diagnostics

```rust
pub enum Severity { Error, Warning, Note }

pub struct DiagLabel {
    pub file: FileId,
    pub span: Span,
    pub message: String,
}

pub struct Diag {
    pub severity: Severity,
    pub message: String,
    pub primary: DiagLabel,
    pub labels: Vec<DiagLabel>,
    pub help: Option<String>,
    pub code: Option<String>,
}
```

Rendering converts `Diag` -> `ariadne::Report` and prints using a cache backed by `SourceManager`.

## Lexer tokens (logos)

```rust
#[derive(logos::Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    // identifiers, numbers, strings
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident,
    #[regex(r"(\$[0-9A-Fa-f]+)|(%[01]+)|([0-9]+)")]
    Number,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,

    // punctuation/operators
    #[token(":")] Colon,
    #[token(",")] Comma,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBracket,
    #[token("]")] RBracket,
    #[token("#")] Hash,
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("&")] Amp,
    #[token("|")] Pipe,
    #[token("^")] Caret,
    #[token("<<")] Shl,
    #[token(">>")] Shr,
    #[token("=")] Eq,

    // directives: keep as Ident token, parse leading '.' in parser or lexer, your choice
    #[token(".")] Dot,

    // line structure
    #[regex(r";[^\n\r]*", logos::skip)]
    Comment,
    #[regex(r"[ \t]+", logos::skip)]
    Ws,
    #[regex(r"\r?\n")]
    Newline,

    #[error]
    Error,
}

pub struct Token {
    pub kind: TokenKind,
    pub file: FileId,
    pub span: Span,
    pub lexeme: String, // store slice copy for simplicity in MVP
}
```

## IR (line-oriented)

Keep IR close to assembly concepts, not a high-level language.

```rust
pub struct Program {
    pub items: Vec<Item>,
}

pub enum Item {
    Label(LabelDef),
    Assign(Assign),
    Directive(Directive),
    Instruction(Instruction),
    EmptyLine(Spanned<()>),
}

pub struct LabelDef {
    pub name: Spanned<String>,
}

pub struct Assign {
    pub name: Spanned<String>,
    pub expr: Spanned<Expr>,
}

pub struct Directive {
    pub name: Spanned<String>,      // e.g. ".byte", ".word", ".asciiz", ".res", ".org", ".include"
    pub args: Vec<Spanned<DirArg>>,
}

pub enum DirArg {
    Expr(Expr),
    String(String),
    Ident(String),
}

pub struct Instruction {
    pub mnemonic: Spanned<String>,  // parse case-insensitively, keep original for fmt
    pub operand: Option<Spanned<Operand>>,
}
```

## Operands and expressions

```rust
pub enum Operand {
    // immediate: #expr
    Imm(Expr),

    // plain expression: could become dp/abs/long depending on value and context
    Expr(Expr),

    // indexed
    ExprX(Expr),
    ExprY(Expr),

    // indirect forms: (expr), (expr,X), (expr),Y, [expr], [expr],Y
    Ind(Expr),
    IndX(Expr),
    IndY(Expr),
    LongInd(Expr),
    LongIndY(Expr),

    // stack-relative (65816): expr,S and (expr,S),Y
    StackRel(Expr),
    StackRelIndY(Expr),
}

pub enum Expr {
    Number(i64),              // store parsed value; keep base/width in parser if needed later
    Symbol(String),
    CurrentPc,                // "*"
    Unary { op: UnOp, rhs: Box<Expr> },
    Binary { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },

    // ca65-style low/high/bank extraction (subset):
    LowByte(Box<Expr>),       // <expr
    HighByte(Box<Expr>),      // >expr
    BankByte(Box<Expr>),      // ^expr (optional; decide exact syntax)
}

pub enum UnOp { Plus, Minus }
pub enum BinOp { Add, Sub, Mul, Div, And, Or, Xor, Shl, Shr }
```

# Assembler state, pass 1/2 types

## Symbols

```rust
pub enum SymValue {
    Known(i64),
    Unknown,
}

pub enum SymKind { Label, Const }

pub struct Symbol {
    pub kind: SymKind,
    pub value: SymValue,
    pub defined_at: Option<(FileId, Span)>,
}

pub struct SymTab {
    pub map: indexmap::IndexMap<String, Symbol>,
}
```

## CPU mode tracking (needed for 65816 immediate widths)

MVP support via explicit directives (ca65-like):

- `.a8` / `.a16` (accumulator width)
- `.i8` / `.i16` (index width)
  Optional later: `.smart on|off` to infer from `REP/SEP` when constant operands.

```rust
#[derive(Clone, Copy)]
pub enum Width { W8, W16 }

#[derive(Clone, Copy)]
pub struct CpuMode {
    pub a: Width,
    pub xy: Width,
}
```

## Fixups and layout

```rust
pub enum FixupKind {
    Rel8,        // branch
    Rel16,       // BRL
    ImmA,        // immediate sized by A width
    ImmXY,       // immediate sized by XY width
    Abs16,       // absolute 16
    Long24,      // long 24
    // extend as needed
}

pub struct Fixup {
    pub at_pc: u32,               // where operand begins in output
    pub kind: FixupKind,
    pub expr: Spanned<Expr>,
}

pub struct LineLayout {
    pub item_index: usize,
    pub pc_start: u32,
    pub size: u32,
    pub mode_in: CpuMode,
    pub mode_out: CpuMode,
}
```

# Pass 1 algorithm (layout + symbols + fixups)

Inputs:

- `Program IR`
- initial `CpuMode` default (configurable via CLI)

Outputs:

- `SymTab` with all label definitions (and constant assignments when evaluable)
- `Vec<LineLayout>`
- `Vec<Fixup>` (unresolved expressions or late-bound width decisions)
- `Vec<Diag>`

Rules:

- Maintain `pc` and `mode` as you walk items.
- For `Label:` define symbol at current `pc`.
- For `NAME = expr`, attempt eval using current symbols; store Known or Unknown.
- For directives:
  - `.org expr` sets `pc` (requires Known in pass1 for deterministic layout; else error)
  - `.byte/.word/.asciiz/.res` update `pc` by computed size; `.res` requires known size
  - `.include "file"` is handled earlier by SourceManager or by parser stage (choose one; recommended: SourceManager expands includes to tokens/IR with correct spans).

- For instructions:
  - Determine addressing form from parsed `Operand` shape.
  - Determine size:
    - non-immediate: size based on addressing mode (dp=2, abs=3, long=4, rel=2, etc.)
    - immediate: size depends on `CpuMode` (A or XY) unless explicitly sized syntax exists (optional extension later)

  - If size depends on unknown info and cannot be decided, emit error (assembler must have deterministic layout).

# Pass 2 algorithm (encode)

Inputs:

- `Program IR`, `LineLayout`, `SymTab`, `Fixups`, opcode table

Outputs:

- `Vec<u8>` bytes
- `Vec<Diag>`

Rules:

- Allocate output buffer:
  - simplest MVP: contiguous from first `.org` to last byte, or require a single origin and grow `Vec<u8>` as `pc` increases.

- For each instruction:
  - resolve final addressing mode (some may be value-dependent: dp vs abs; MVP can require explicit dp/abs via syntax shape or directive policy)
  - lookup `(mnemonic, addr_mode)` in opcode table
  - evaluate operand expression(s)
  - range-check (branch signed range, immediate width, truncation)
  - write bytes (little-endian)

- For directives:
  - write bytes for `.byte/.word/.asciiz` etc.

# Opcode table (table-driven, non-optimizing)

Implement a static mapping:

```rust
pub enum AddrMode {
    Imp, Acc,
    ImmA, ImmXY,
    Rel8, Rel16,
    Dp, DpX, DpY,
    Abs, AbsX, AbsY,
    Long, LongX,
    Ind, IndX, IndY,
    LongInd, LongIndY,
    StackRel, StackRelIndY,
}

pub struct OpInfo {
    pub opcode: u8,
    pub mode: AddrMode,
    pub size: u8,
}
```

Lookup key: normalized mnemonic + AddrMode.

MVP scope:

- Implement a complete 6502 base set first.
- Add 65816 extensions incrementally (BRL, long addressing, stack-relative, block moves, etc.).

# CLI (asm816_cli)

Subcommands:

- `assemble <input> -o <out.bin> [-I <dir>...] [--a8|--a16] [--i8|--i16]`
- `check <input>` (parse + pass1 + pass2 without writing)
- `fmt <input>` (optional in MVP; can be stubbed to "parse then print canonical style")

Exit code:

- non-zero if any Error severity diagnostics emitted.

# Canonical formatting rule ("high-level by default")

Formatter outputs ca65-like canonical style:

- labels at column 0 with trailing `:`
- instructions/directives indented by 4 spaces under labels
- normalize mnemonic casing (choose one, document it)
- single space after commas
- comments `;` preceded by at least two spaces (optional)
  This is purely presentation sugar; parsed input can be more permissive.

# MVP directive set

Required in v1:

- `.org expr`
- `.byte expr[, expr...]`
- `.word expr[, expr...]`
- `.asciiz "..."` (emit bytes + trailing 0)
- `.res expr` (emit expr zeros)
- `.include "path"` (basic)
- `.a8/.a16/.i8/.i16`

Nice-to-have (v1.1+):

- `.segment/.proc/.endproc` (scoping)
- `.macro/.endmacro`
- `.if/.else/.endif`

# Implementation steps (agent tasks)

1. Create workspace + crate skeletons (`asm816_core`, `asm816_cli`).
2. Implement SourceManager:
   - load file, store text, assign FileId
   - basic include resolution strategy (either pre-parse include expansion or parse-time include directive handling).

3. Implement logos lexer producing `Vec<Token>` with spans and lexemes.
4. Implement chumsky parser:
   - parse lines into IR Items
   - parse expressions into Expr AST
   - collect parse diagnostics (unknown tokens, malformed literals, etc.)

5. Implement diagnostics pipeline:
   - internal `Diag` types
   - `Diag -> ariadne::Report` renderer using SourceManager as cache

6. Implement pass 1:
   - PC tracking, label table, size estimation, CPU mode directives

7. Implement opcode table + encoder for a minimal 6502 set:
   - enough to assemble small programs (LDA/STA/ADC/SBC/JSR/RTS/branches, etc.)

8. Implement pass 2:
   - expression evaluation
   - encoding + range checks
   - output `Vec<u8>`

9. Implement `.bin` writer and CLI glue.
10. Add snapshot-style tests:

- assemble small snippets and compare bytes
- test diagnostics output shape (message + span correctness)

# Acceptance criteria (MVP)

- Assembles a single-file program using ca65-like syntax into correct `.bin`.
- Supports forward-referenced labels (2-pass) with correct branch resolution and errors when out of range.
- Produces readable, multi-span diagnostics with correct file/line highlighting (ariadne).
- Supports `.org`, `.byte`, `.word`, `.asciiz`, `.res`, labels, `NAME = expr`, and `.a8/.a16/.i8/.i16`.
- Output is a raw `.bin` created from an in-memory `Vec<u8>`.

# Notes

- ca65 documentation is the canonical syntax reference and should be used to resolve ambiguities:
  [https://cc65.github.io/doc/ca65.html](https://cc65.github.io/doc/ca65.html)
- For initial simplicity, require deterministic layout in pass 1:
  - `.org` and `.res` arguments must be pass1-evaluable.
  - immediate widths must be determined via `.a8/.a16/.i8/.i16` (no implicit inference required in MVP).
