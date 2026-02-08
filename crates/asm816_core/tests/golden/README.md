# Golden Fixture Layout

Each test case is a flat file stem under this directory:

- `foo.s` (required): assembly source
- `foo.bin` (optional): expected assembled bytes
- `foo.err` (optional): expected Ariadne diagnostics snapshot for erroring cases

Rules:

- Exactly one of `foo.bin` or `foo.err` must exist for each `foo.s`.
- If both `foo.bin` and `foo.err` exist, the test hard-aborts immediately.
- If `foo.bin` exists, assembly must succeed and output bytes must match.
- If `foo.err` exists, assembly must fail and rendered diagnostics must match.

The test harness in `crates/asm816_core/tests/golden.rs` validates every `*.s` file against these rules. To refresh error snapshots, run tests with `ASM816_UPDATE_GOLDEN=1`.
