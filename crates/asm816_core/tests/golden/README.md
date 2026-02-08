# Golden Fixture Layout

Each fixture directory must contain:

- `input.s` - assembly source
- `expected.bin` - expected assembled bytes

The test harness in `crates/asm816_core/tests/golden.rs` discovers every
subdirectory of this folder and compares assembled output byte-for-byte.
