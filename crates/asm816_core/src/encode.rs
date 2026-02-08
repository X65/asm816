#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AddrMode {
    Imp,
    ImmA,
    ImmXY,
    Rel8,
    Dp,
    DpX,
    DpY,
    Abs,
    AbsX,
    AbsY,
    Ind,
    IndX,
    IndY,
}

pub fn is_branch_mnemonic(mnemonic: &str) -> bool {
    matches!(
        mnemonic,
        "BCC" | "BCS" | "BEQ" | "BMI" | "BNE" | "BPL" | "BVC" | "BVS"
    )
}

pub fn immediate_uses_xy(mnemonic: &str) -> bool {
    matches!(mnemonic, "LDX" | "LDY")
}

pub fn opcode_for(mnemonic: &str, mode: AddrMode) -> Option<u8> {
    match (mnemonic, mode) {
        ("LDA", AddrMode::ImmA) => Some(0xA9),
        ("LDA", AddrMode::Dp) => Some(0xA5),
        ("LDA", AddrMode::DpX) => Some(0xB5),
        ("LDA", AddrMode::Abs) => Some(0xAD),
        ("LDA", AddrMode::AbsX) => Some(0xBD),
        ("LDA", AddrMode::AbsY) => Some(0xB9),
        ("LDA", AddrMode::IndX) => Some(0xA1),
        ("LDA", AddrMode::IndY) => Some(0xB1),

        ("LDX", AddrMode::ImmXY) => Some(0xA2),
        ("LDX", AddrMode::Dp) => Some(0xA6),
        ("LDX", AddrMode::DpY) => Some(0xB6),
        ("LDX", AddrMode::Abs) => Some(0xAE),
        ("LDX", AddrMode::AbsY) => Some(0xBE),

        ("LDY", AddrMode::ImmXY) => Some(0xA0),
        ("LDY", AddrMode::Dp) => Some(0xA4),
        ("LDY", AddrMode::DpX) => Some(0xB4),
        ("LDY", AddrMode::Abs) => Some(0xAC),
        ("LDY", AddrMode::AbsX) => Some(0xBC),

        ("STA", AddrMode::Dp) => Some(0x85),
        ("STA", AddrMode::DpX) => Some(0x95),
        ("STA", AddrMode::Abs) => Some(0x8D),
        ("STA", AddrMode::AbsX) => Some(0x9D),
        ("STA", AddrMode::AbsY) => Some(0x99),
        ("STA", AddrMode::IndX) => Some(0x81),
        ("STA", AddrMode::IndY) => Some(0x91),

        ("STX", AddrMode::Dp) => Some(0x86),
        ("STX", AddrMode::DpY) => Some(0x96),
        ("STX", AddrMode::Abs) => Some(0x8E),

        ("STY", AddrMode::Dp) => Some(0x84),
        ("STY", AddrMode::DpX) => Some(0x94),
        ("STY", AddrMode::Abs) => Some(0x8C),

        ("ADC", AddrMode::ImmA) => Some(0x69),
        ("ADC", AddrMode::Dp) => Some(0x65),
        ("ADC", AddrMode::DpX) => Some(0x75),
        ("ADC", AddrMode::Abs) => Some(0x6D),
        ("ADC", AddrMode::AbsX) => Some(0x7D),
        ("ADC", AddrMode::AbsY) => Some(0x79),
        ("ADC", AddrMode::IndX) => Some(0x61),
        ("ADC", AddrMode::IndY) => Some(0x71),

        ("SBC", AddrMode::ImmA) => Some(0xE9),
        ("SBC", AddrMode::Dp) => Some(0xE5),
        ("SBC", AddrMode::DpX) => Some(0xF5),
        ("SBC", AddrMode::Abs) => Some(0xED),
        ("SBC", AddrMode::AbsX) => Some(0xFD),
        ("SBC", AddrMode::AbsY) => Some(0xF9),
        ("SBC", AddrMode::IndX) => Some(0xE1),
        ("SBC", AddrMode::IndY) => Some(0xF1),

        ("JMP", AddrMode::Abs) => Some(0x4C),
        ("JMP", AddrMode::Ind) => Some(0x6C),
        ("JSR", AddrMode::Abs) => Some(0x20),
        ("RTS", AddrMode::Imp) => Some(0x60),
        ("NOP", AddrMode::Imp) => Some(0xEA),
        ("CLC", AddrMode::Imp) => Some(0x18),
        ("SEC", AddrMode::Imp) => Some(0x38),

        ("BCC", AddrMode::Rel8) => Some(0x90),
        ("BCS", AddrMode::Rel8) => Some(0xB0),
        ("BEQ", AddrMode::Rel8) => Some(0xF0),
        ("BMI", AddrMode::Rel8) => Some(0x30),
        ("BNE", AddrMode::Rel8) => Some(0xD0),
        ("BPL", AddrMode::Rel8) => Some(0x10),
        ("BVC", AddrMode::Rel8) => Some(0x50),
        ("BVS", AddrMode::Rel8) => Some(0x70),

        _ => None,
    }
}

pub fn mode_size_fixed(mode: AddrMode) -> Option<u32> {
    match mode {
        AddrMode::Imp => Some(1),
        AddrMode::ImmA | AddrMode::ImmXY => None,
        AddrMode::Rel8
        | AddrMode::Dp
        | AddrMode::DpX
        | AddrMode::DpY
        | AddrMode::IndX
        | AddrMode::IndY => Some(2),
        AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind => Some(3),
    }
}
