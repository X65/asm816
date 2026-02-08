use asm816_core::{CpuMode, compile_source_text, diag::has_errors};

#[test]
fn plain_small_operand_defaults_to_absolute() {
    let (bytes, diags) = compile_source_text("LDA $10\n", CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xAD, 0x10, 0x00]);
}

#[test]
fn lowbyte_prefix_forces_direct_page() {
    let (bytes, diags) = compile_source_text("LDA <$10\n", CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xA5, 0x10]);
}

#[test]
fn forcing_direct_page_on_jsr_is_rejected() {
    let (_bytes, diags) = compile_source_text("JSR <$1234\n", CpuMode::default());
    assert!(has_errors(&diags));
}

#[test]
fn supports_65c02_indirect_forms_and_bra() {
    let src = r#"
    LDA ($10)
    JMP ($1234, X)
    BRA *
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xB2, 0x10, 0x7C, 0x34, 0x12, 0x80, 0xFE]);
}

#[test]
fn accumulator_operand_is_mnemonic_sensitive() {
    let src = r#"
A = $20
LDA A
ASL A
"#;

    let (bytes, diags) = compile_source_text(src, CpuMode::default());
    assert!(!has_errors(&diags), "diagnostics: {diags:#?}");
    assert_eq!(bytes, vec![0xAD, 0x20, 0x00, 0x0A]);
}
