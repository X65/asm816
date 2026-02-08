# 6502 + 65C02 Core Coverage Completion Plan

## Summary
Expand `asm816_core` from MVP opcode subset to full official 6502 + official 65C02 core instruction/mode coverage (excluding Rockwell bit ops), while keeping `.include` deferred.

Deliverables are:
- full parser/IR/encoder support for the selected ISA scope,
- explicit addressing policy (`<expr` forces zero-page),
- exhaustive opcode/mode tests,
- golden sample assembly-to-binary tests.

## Locked Scope Decisions
- ISA target: 6502 + official 65C02 core.
- Exclusions: no undocumented opcodes, no Rockwell `RMB/SMB/BBR/BBS`, defer `.include`.
- Address-width policy: explicitness required.
- Zero-page forcing syntax: `<expr` (including indexed forms).
- Done criteria: opcode matrix tests + sample binary goldens.

## Important API / Interface / Type Changes
- Extend internal operand/type model in `crates/asm816_core/src/ir.rs`:
  - add accumulator operand shape (`Acc`) for `ASL A`-style syntax.
  - keep existing expression/indexed forms; allow mode resolution to distinguish `JMP` indirect variants.
- Extend addressing mode enum in `crates/asm816_core/src/encode.rs`:
  - add `Acc`, zero-page indirect `(zp)` mode, and `JMP (abs,X)` mode.
- Replace ad-hoc opcode `match` with table-driven opcode metadata in `crates/asm816_core/src/encode.rs`:
  - rows contain mnemonic, mode, opcode, and ISA tier (`6502` vs `65C02`).
- Keep CLI surface unchanged (`asm816_cli` commands/flags unchanged).
- Keep current public pipeline APIs unchanged unless required by tests.

## Implementation Plan

### 1. Parser/IR Rewrite for Full Operand Shapes
1. Update parser grammar in `crates/asm816_core/src/parse.rs` to support:
   - explicit accumulator operand (`A`),
   - all indirect syntaxes needed by 6502/65C02 core:
     - `(expr)`, `(expr, X)`, `(expr), Y`, and `JMP (expr, X)`.
2. Keep case-insensitive mnemonic parsing and normalized uppercase mnemonics.
3. Preserve existing expression grammar; use `<expr` as explicit zero-page forcing marker.

### 2. Mode Resolution Policy (Pass1/Pass2)
1. Refactor instruction layout selection in `crates/asm816_core/src/asm/mod.rs`:
   - no auto ZP/ABS relaxation for ambiguous forms.
   - when both ZP and ABS exist, plain `expr` selects ABS.
   - `<expr` selects ZP (or ZP-indexed equivalent).
2. Preserve branch-relative handling and add `BRA` to rel8 set.
3. Ensure `ASL/LSR/ROL/ROR/INC/DEC` with no operand resolve correctly (`Acc` vs `Imp`) by mnemonic rules.
4. Keep deterministic pass1 sizing with no backtracking.

### 3. Full Opcode Table Expansion
1. Build full official 6502 table coverage.
2. Add official 65C02 core additions, including:
   - `BRA`, `STZ`, `TRB`, `TSB`, `PHX/PLX/PHY/PLY`,
   - `BIT` new modes (`#imm`, indexed forms),
   - `DEC A`, `INC A`,
   - `(zp)` memory-indirect forms,
   - `JMP (abs,X)`.
3. Explicitly exclude Rockwell bit-manipulation branch family and undocumented ops.

### 4. Encoding + Validation Rules
1. Implement byte/word emission for all new modes, including:
   - accumulator/implied one-byte instructions,
   - zero-page indirect pointer-byte checks,
   - `JMP (abs,X)` absolute operand encoding.
2. Add strict range diagnostics:
   - forced `<expr` must fit byte range,
   - relative branch offset range checks remain enforced.
3. Improve diagnostics for ambiguous intent:
   - if plain small constant uses ABS where ZP also exists, optional warning hint: use `<expr` for ZP.

### 5. Test Expansion (Required Deliverable)
1. Add opcode/mode matrix tests in `crates/asm816_core/tests`:
   - one test generator/table that validates every opcode row emits exact opcode + operand width.
2. Add parser behavior tests:
   - accumulator explicit and implicit forms,
   - new indirect forms,
   - explicit ZP forcing behavior.
3. Add policy tests:
   - `LDA $10` uses ABS when both forms exist,
   - `LDA <$10` uses ZP,
   - out-of-range `<$1234` errors.
4. Add sample binary goldens:
   - fixture directories with `input.s` + `expected.bin`,
   - at least one 6502-focused and one 65C02-focused program,
   - deterministic harness in `crates/asm816_core/tests` (no CWD dependence).

### 6. Verification Gate
1. Run `cargo test -p asm816_core`.
2. Run workspace tests if core passes.
3. Validate that existing MVP tests are migrated/updated to explicit mode policy where required.

## Test Cases and Scenarios
- Full mnemonic/mode opcode matrix parity against table metadata.
- Branch correctness:
  - forward/backward in-range,
  - out-of-range error.
- Accumulator/inherent form correctness:
  - `ASL`, `ASL A`, `INC A`, `DEC A`.
- 65C02 mode correctness:
  - `(zp)` loads/stores,
  - `JMP (abs,X)`,
  - `BRA`.
- Explicitness policy:
  - ABS default on ambiguous plain operands,
  - `<expr` forces ZP.
- Golden samples:
  - assemble fixture `.s` files and byte-compare against committed `.bin`.

## Assumptions and Defaults
- `.include` remains warning+ignored for this milestone.
- No CPU-dialect CLI gating added in this pass; accepted opcode set is combined 6502+65C02 core.
- Immediate-width mode directives (`.a8/.a16/.i8/.i16`) remain as currently implemented unless they block selected ISA correctness.
- No undocumented opcode support in this milestone.
- No backward-compatibility guarantee for previous auto ZP/ABS behavior (explicit mode policy replaces it).
