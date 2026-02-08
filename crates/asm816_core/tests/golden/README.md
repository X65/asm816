# Golden Fixture Layout

Each fixture directory contains:

- `input.s` (required): assembly source
- `expected.bin` (optional): expected assembled bytes
- `expect_error` (optional): expected Ariadne diagnostics snapshot for erroring cases

Rules:

- If `expect_error` is absent, `expected.bin` must exist and assembly must succeed.
- If `expect_error` is present, assembly must fail and rendered diagnostics must match that snapshot.
- `expected.bin` can still be provided with `expect_error` when output bytes should also be asserted.

The test harness in `crates/asm816_core/tests/golden.rs` discovers every subdirectory under this folder and validates each case against these rules. To refresh snapshots, run tests with `ASM816_UPDATE_GOLDEN=1`.
