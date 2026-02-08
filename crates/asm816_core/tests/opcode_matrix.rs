use asm816_core::{
    CpuMode, compile_source_text,
    diag::has_errors,
    encode::{AddrMode, opcode_table},
};

#[test]
fn encodes_every_defined_opcode_entry() {
    for entry in opcode_table() {
        let (operand_text, operand_bytes) = operand_for_mode(entry.mode);
        let source = if operand_text.is_empty() {
            format!("{}\n", entry.mnemonic)
        } else {
            format!("{} {operand_text}\n", entry.mnemonic)
        };

        let (bytes, diags) = compile_source_text(&source, CpuMode::default());
        assert!(
            !has_errors(&diags),
            "unexpected diagnostics for {} {:?}: {diags:#?}",
            entry.mnemonic,
            entry.mode
        );

        let mut expected = Vec::with_capacity(1 + operand_bytes.len());
        expected.push(entry.opcode);
        expected.extend_from_slice(&operand_bytes);
        assert_eq!(
            bytes, expected,
            "byte mismatch for {} {:?}",
            entry.mnemonic, entry.mode
        );
    }
}

fn operand_for_mode(mode: AddrMode) -> (&'static str, Vec<u8>) {
    match mode {
        AddrMode::Imp => ("", vec![]),
        AddrMode::Acc => ("A", vec![]),
        AddrMode::ImmA | AddrMode::ImmXY => ("#$12", vec![0x12]),
        AddrMode::Rel8 => ("*", vec![0xFE]),
        AddrMode::Dp => ("<$34", vec![0x34]),
        AddrMode::DpX => ("<$34, X", vec![0x34]),
        AddrMode::DpY => ("<$34, Y", vec![0x34]),
        AddrMode::Abs => ("$1234", vec![0x34, 0x12]),
        AddrMode::AbsX => ("$1234, X", vec![0x34, 0x12]),
        AddrMode::AbsY => ("$1234, Y", vec![0x34, 0x12]),
        AddrMode::Ind => ("($1234)", vec![0x34, 0x12]),
        AddrMode::IndX => ("($34, X)", vec![0x34]),
        AddrMode::IndY => ("($34), Y", vec![0x34]),
        AddrMode::DpInd => ("($34)", vec![0x34]),
        AddrMode::IndAbsX => ("($1234, X)", vec![0x34, 0x12]),
    }
}
