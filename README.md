# asm816

A simple WDC 65C816 / 6502 assembler written in Rust.

Canonical syntax: ca65 (source of truth)
<https://cc65.github.io/doc/ca65.html>

## Goals

- Assemble 65C816 and 6502 source code into a raw `.bin` output.
- Stay close to ca65 syntax while accepting a practical subset of common 6502/65816 conventions.
- Provide clear, source-spanned error messages.
- Keep assembly readable by supporting a canonical, formatter-friendly style.

Non-goals (v1):

- No optimization.
- No object files or linking (raw `.bin` only).

## Install

Requires a recent Rust toolchain.

```bash
git clone <repo-url>
cd asm816
cargo build --release -p asm816_cli
```

## Usage

Assemble to a raw binary:

```bash
cargo run --release -p asm816_cli -- assemble path/to/main.s -o out.bin
```

Add include directories:

```bash
cargo run --release -p asm816_cli -- assemble path/to/main.s -o out.bin -I include -I vendor
```

Use module imports instead of `.include`:

```asm
; main.asm
import libs::constants
.org $8000
LDA #libs::constants::ONE
```

```asm
; libs/constants.asm
pub ONE = 1
```

Notes:

- module files map from roots as `foo/bar.asm` -> `foo::bar`
- `pub` controls cross-module visibility
- selective imports are supported: `use foo::bar::{ONE, TWO as T}`
- inner modules are supported: `mod util { ... }` -> `<current>::util`
- public macros are supported and expand at call site:
  - define: `pub macro emit { LDA #ONE }`
  - invoke: `foo::macros::emit` or via `use ...::{emit}` then `emit`
  - macro-local labels: `%%loop`
- `.include` is deprecated and now reported as an error

Set default 65816 register widths (used to size immediate operands):

```bash
cargo run --release -p asm816_cli -- assemble path/to/main.s -o out.bin --a16 --i16
```

Validate input without writing output:

```bash
cargo run --release -p asm816_cli -- check path/to/main.s
```

Format to canonical style (if available in your build):

```bash
cargo run --release -p asm816_cli -- fmt path/to/main.s
```

## Exit codes

- `0` on success
- non-zero if any errors were reported
