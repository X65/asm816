use asm816_core::{CpuMode, compile_source_text, diag::has_errors};

#[test]
fn assembles_small_program() {
    let src = r#"
.org $8000
start:
    LDA #$01
    STA $10
    ADC #2
    BNE start
    RTS
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(
        bytes,
        vec![0xA9, 0x01, 0x85, 0x10, 0x69, 0x02, 0xD0, 0xF8, 0x60]
    );
}

#[test]
fn resolves_forward_branch() {
    let src = r#"
    BNE target
    NOP
target:
    RTS
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xD0, 0x01, 0xEA, 0x60]);
}

#[test]
fn errors_on_out_of_range_branch() {
    let src = r#"
start:
    BNE target
    .res 200
target:
    NOP
"#;

    let (_bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(has_errors(&diags));
}

#[test]
fn emits_directive_bytes() {
    let src = r#"
    .byte 1, $02, %11
    .word $1234
    .asciiz "A"
    .res 2
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![1, 2, 3, 0x34, 0x12, b'A', 0x00, 0x00, 0x00]);
}

#[test]
fn warns_and_skips_include() {
    let src = r#"
    .include "foo.s"
    NOP
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert_eq!(bytes, vec![0xEA]);
    assert!(
        diags
            .iter()
            .any(|diag| matches!(diag.severity, asm816_core::diag::Severity::Warning))
    );
}

#[test]
fn errors_on_repeated_org() {
    let src = r#"
    .org $8000
    .org $9000
    NOP
"#;

    let (_bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(has_errors(&diags));
}

#[test]
fn supports_indexed_indirect_operand() {
    let src = r#"
    LDA ($10, X)
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xA1, 0x10]);
}
