#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AddrMode {
    Imp,
    Acc,
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
    DpInd,
    IndAbsX,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IsaTier {
    Base6502,
    Core65C02,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OpcodeDef {
    pub mnemonic: &'static str,
    pub mode: AddrMode,
    pub opcode: u8,
    pub isa: IsaTier,
}

const OPCODES: &[OpcodeDef] = &[
    // 6502 base
    op("ADC", AddrMode::ImmA, 0x69, IsaTier::Base6502),
    op("ADC", AddrMode::Dp, 0x65, IsaTier::Base6502),
    op("ADC", AddrMode::DpX, 0x75, IsaTier::Base6502),
    op("ADC", AddrMode::Abs, 0x6D, IsaTier::Base6502),
    op("ADC", AddrMode::AbsX, 0x7D, IsaTier::Base6502),
    op("ADC", AddrMode::AbsY, 0x79, IsaTier::Base6502),
    op("ADC", AddrMode::IndX, 0x61, IsaTier::Base6502),
    op("ADC", AddrMode::IndY, 0x71, IsaTier::Base6502),
    op("AND", AddrMode::ImmA, 0x29, IsaTier::Base6502),
    op("AND", AddrMode::Dp, 0x25, IsaTier::Base6502),
    op("AND", AddrMode::DpX, 0x35, IsaTier::Base6502),
    op("AND", AddrMode::Abs, 0x2D, IsaTier::Base6502),
    op("AND", AddrMode::AbsX, 0x3D, IsaTier::Base6502),
    op("AND", AddrMode::AbsY, 0x39, IsaTier::Base6502),
    op("AND", AddrMode::IndX, 0x21, IsaTier::Base6502),
    op("AND", AddrMode::IndY, 0x31, IsaTier::Base6502),
    op("ASL", AddrMode::Acc, 0x0A, IsaTier::Base6502),
    op("ASL", AddrMode::Dp, 0x06, IsaTier::Base6502),
    op("ASL", AddrMode::DpX, 0x16, IsaTier::Base6502),
    op("ASL", AddrMode::Abs, 0x0E, IsaTier::Base6502),
    op("ASL", AddrMode::AbsX, 0x1E, IsaTier::Base6502),
    op("BCC", AddrMode::Rel8, 0x90, IsaTier::Base6502),
    op("BCS", AddrMode::Rel8, 0xB0, IsaTier::Base6502),
    op("BEQ", AddrMode::Rel8, 0xF0, IsaTier::Base6502),
    op("BIT", AddrMode::Dp, 0x24, IsaTier::Base6502),
    op("BIT", AddrMode::Abs, 0x2C, IsaTier::Base6502),
    op("BMI", AddrMode::Rel8, 0x30, IsaTier::Base6502),
    op("BNE", AddrMode::Rel8, 0xD0, IsaTier::Base6502),
    op("BPL", AddrMode::Rel8, 0x10, IsaTier::Base6502),
    op("BRK", AddrMode::Imp, 0x00, IsaTier::Base6502),
    op("BVC", AddrMode::Rel8, 0x50, IsaTier::Base6502),
    op("BVS", AddrMode::Rel8, 0x70, IsaTier::Base6502),
    op("CLC", AddrMode::Imp, 0x18, IsaTier::Base6502),
    op("CLD", AddrMode::Imp, 0xD8, IsaTier::Base6502),
    op("CLI", AddrMode::Imp, 0x58, IsaTier::Base6502),
    op("CLV", AddrMode::Imp, 0xB8, IsaTier::Base6502),
    op("CMP", AddrMode::ImmA, 0xC9, IsaTier::Base6502),
    op("CMP", AddrMode::Dp, 0xC5, IsaTier::Base6502),
    op("CMP", AddrMode::DpX, 0xD5, IsaTier::Base6502),
    op("CMP", AddrMode::Abs, 0xCD, IsaTier::Base6502),
    op("CMP", AddrMode::AbsX, 0xDD, IsaTier::Base6502),
    op("CMP", AddrMode::AbsY, 0xD9, IsaTier::Base6502),
    op("CMP", AddrMode::IndX, 0xC1, IsaTier::Base6502),
    op("CMP", AddrMode::IndY, 0xD1, IsaTier::Base6502),
    op("CPX", AddrMode::ImmXY, 0xE0, IsaTier::Base6502),
    op("CPX", AddrMode::Dp, 0xE4, IsaTier::Base6502),
    op("CPX", AddrMode::Abs, 0xEC, IsaTier::Base6502),
    op("CPY", AddrMode::ImmXY, 0xC0, IsaTier::Base6502),
    op("CPY", AddrMode::Dp, 0xC4, IsaTier::Base6502),
    op("CPY", AddrMode::Abs, 0xCC, IsaTier::Base6502),
    op("DEC", AddrMode::Dp, 0xC6, IsaTier::Base6502),
    op("DEC", AddrMode::DpX, 0xD6, IsaTier::Base6502),
    op("DEC", AddrMode::Abs, 0xCE, IsaTier::Base6502),
    op("DEC", AddrMode::AbsX, 0xDE, IsaTier::Base6502),
    op("DEX", AddrMode::Imp, 0xCA, IsaTier::Base6502),
    op("DEY", AddrMode::Imp, 0x88, IsaTier::Base6502),
    op("EOR", AddrMode::ImmA, 0x49, IsaTier::Base6502),
    op("EOR", AddrMode::Dp, 0x45, IsaTier::Base6502),
    op("EOR", AddrMode::DpX, 0x55, IsaTier::Base6502),
    op("EOR", AddrMode::Abs, 0x4D, IsaTier::Base6502),
    op("EOR", AddrMode::AbsX, 0x5D, IsaTier::Base6502),
    op("EOR", AddrMode::AbsY, 0x59, IsaTier::Base6502),
    op("EOR", AddrMode::IndX, 0x41, IsaTier::Base6502),
    op("EOR", AddrMode::IndY, 0x51, IsaTier::Base6502),
    op("INC", AddrMode::Dp, 0xE6, IsaTier::Base6502),
    op("INC", AddrMode::DpX, 0xF6, IsaTier::Base6502),
    op("INC", AddrMode::Abs, 0xEE, IsaTier::Base6502),
    op("INC", AddrMode::AbsX, 0xFE, IsaTier::Base6502),
    op("INX", AddrMode::Imp, 0xE8, IsaTier::Base6502),
    op("INY", AddrMode::Imp, 0xC8, IsaTier::Base6502),
    op("JMP", AddrMode::Abs, 0x4C, IsaTier::Base6502),
    op("JMP", AddrMode::Ind, 0x6C, IsaTier::Base6502),
    op("JSR", AddrMode::Abs, 0x20, IsaTier::Base6502),
    op("LDA", AddrMode::ImmA, 0xA9, IsaTier::Base6502),
    op("LDA", AddrMode::Dp, 0xA5, IsaTier::Base6502),
    op("LDA", AddrMode::DpX, 0xB5, IsaTier::Base6502),
    op("LDA", AddrMode::Abs, 0xAD, IsaTier::Base6502),
    op("LDA", AddrMode::AbsX, 0xBD, IsaTier::Base6502),
    op("LDA", AddrMode::AbsY, 0xB9, IsaTier::Base6502),
    op("LDA", AddrMode::IndX, 0xA1, IsaTier::Base6502),
    op("LDA", AddrMode::IndY, 0xB1, IsaTier::Base6502),
    op("LDX", AddrMode::ImmXY, 0xA2, IsaTier::Base6502),
    op("LDX", AddrMode::Dp, 0xA6, IsaTier::Base6502),
    op("LDX", AddrMode::DpY, 0xB6, IsaTier::Base6502),
    op("LDX", AddrMode::Abs, 0xAE, IsaTier::Base6502),
    op("LDX", AddrMode::AbsY, 0xBE, IsaTier::Base6502),
    op("LDY", AddrMode::ImmXY, 0xA0, IsaTier::Base6502),
    op("LDY", AddrMode::Dp, 0xA4, IsaTier::Base6502),
    op("LDY", AddrMode::DpX, 0xB4, IsaTier::Base6502),
    op("LDY", AddrMode::Abs, 0xAC, IsaTier::Base6502),
    op("LDY", AddrMode::AbsX, 0xBC, IsaTier::Base6502),
    op("LSR", AddrMode::Acc, 0x4A, IsaTier::Base6502),
    op("LSR", AddrMode::Dp, 0x46, IsaTier::Base6502),
    op("LSR", AddrMode::DpX, 0x56, IsaTier::Base6502),
    op("LSR", AddrMode::Abs, 0x4E, IsaTier::Base6502),
    op("LSR", AddrMode::AbsX, 0x5E, IsaTier::Base6502),
    op("NOP", AddrMode::Imp, 0xEA, IsaTier::Base6502),
    op("ORA", AddrMode::ImmA, 0x09, IsaTier::Base6502),
    op("ORA", AddrMode::Dp, 0x05, IsaTier::Base6502),
    op("ORA", AddrMode::DpX, 0x15, IsaTier::Base6502),
    op("ORA", AddrMode::Abs, 0x0D, IsaTier::Base6502),
    op("ORA", AddrMode::AbsX, 0x1D, IsaTier::Base6502),
    op("ORA", AddrMode::AbsY, 0x19, IsaTier::Base6502),
    op("ORA", AddrMode::IndX, 0x01, IsaTier::Base6502),
    op("ORA", AddrMode::IndY, 0x11, IsaTier::Base6502),
    op("PHA", AddrMode::Imp, 0x48, IsaTier::Base6502),
    op("PHP", AddrMode::Imp, 0x08, IsaTier::Base6502),
    op("PLA", AddrMode::Imp, 0x68, IsaTier::Base6502),
    op("PLP", AddrMode::Imp, 0x28, IsaTier::Base6502),
    op("ROL", AddrMode::Acc, 0x2A, IsaTier::Base6502),
    op("ROL", AddrMode::Dp, 0x26, IsaTier::Base6502),
    op("ROL", AddrMode::DpX, 0x36, IsaTier::Base6502),
    op("ROL", AddrMode::Abs, 0x2E, IsaTier::Base6502),
    op("ROL", AddrMode::AbsX, 0x3E, IsaTier::Base6502),
    op("ROR", AddrMode::Acc, 0x6A, IsaTier::Base6502),
    op("ROR", AddrMode::Dp, 0x66, IsaTier::Base6502),
    op("ROR", AddrMode::DpX, 0x76, IsaTier::Base6502),
    op("ROR", AddrMode::Abs, 0x6E, IsaTier::Base6502),
    op("ROR", AddrMode::AbsX, 0x7E, IsaTier::Base6502),
    op("RTI", AddrMode::Imp, 0x40, IsaTier::Base6502),
    op("RTS", AddrMode::Imp, 0x60, IsaTier::Base6502),
    op("SBC", AddrMode::ImmA, 0xE9, IsaTier::Base6502),
    op("SBC", AddrMode::Dp, 0xE5, IsaTier::Base6502),
    op("SBC", AddrMode::DpX, 0xF5, IsaTier::Base6502),
    op("SBC", AddrMode::Abs, 0xED, IsaTier::Base6502),
    op("SBC", AddrMode::AbsX, 0xFD, IsaTier::Base6502),
    op("SBC", AddrMode::AbsY, 0xF9, IsaTier::Base6502),
    op("SBC", AddrMode::IndX, 0xE1, IsaTier::Base6502),
    op("SBC", AddrMode::IndY, 0xF1, IsaTier::Base6502),
    op("SEC", AddrMode::Imp, 0x38, IsaTier::Base6502),
    op("SED", AddrMode::Imp, 0xF8, IsaTier::Base6502),
    op("SEI", AddrMode::Imp, 0x78, IsaTier::Base6502),
    op("STA", AddrMode::Dp, 0x85, IsaTier::Base6502),
    op("STA", AddrMode::DpX, 0x95, IsaTier::Base6502),
    op("STA", AddrMode::Abs, 0x8D, IsaTier::Base6502),
    op("STA", AddrMode::AbsX, 0x9D, IsaTier::Base6502),
    op("STA", AddrMode::AbsY, 0x99, IsaTier::Base6502),
    op("STA", AddrMode::IndX, 0x81, IsaTier::Base6502),
    op("STA", AddrMode::IndY, 0x91, IsaTier::Base6502),
    op("STX", AddrMode::Dp, 0x86, IsaTier::Base6502),
    op("STX", AddrMode::DpY, 0x96, IsaTier::Base6502),
    op("STX", AddrMode::Abs, 0x8E, IsaTier::Base6502),
    op("STY", AddrMode::Dp, 0x84, IsaTier::Base6502),
    op("STY", AddrMode::DpX, 0x94, IsaTier::Base6502),
    op("STY", AddrMode::Abs, 0x8C, IsaTier::Base6502),
    op("TAX", AddrMode::Imp, 0xAA, IsaTier::Base6502),
    op("TAY", AddrMode::Imp, 0xA8, IsaTier::Base6502),
    op("TSX", AddrMode::Imp, 0xBA, IsaTier::Base6502),
    op("TXA", AddrMode::Imp, 0x8A, IsaTier::Base6502),
    op("TXS", AddrMode::Imp, 0x9A, IsaTier::Base6502),
    op("TYA", AddrMode::Imp, 0x98, IsaTier::Base6502),
    // 65C02 core additions
    op("ADC", AddrMode::DpInd, 0x72, IsaTier::Core65C02),
    op("AND", AddrMode::DpInd, 0x32, IsaTier::Core65C02),
    op("BIT", AddrMode::ImmA, 0x89, IsaTier::Core65C02),
    op("BIT", AddrMode::DpX, 0x34, IsaTier::Core65C02),
    op("BIT", AddrMode::AbsX, 0x3C, IsaTier::Core65C02),
    op("BRA", AddrMode::Rel8, 0x80, IsaTier::Core65C02),
    op("CMP", AddrMode::DpInd, 0xD2, IsaTier::Core65C02),
    op("DEC", AddrMode::Acc, 0x3A, IsaTier::Core65C02),
    op("EOR", AddrMode::DpInd, 0x52, IsaTier::Core65C02),
    op("INC", AddrMode::Acc, 0x1A, IsaTier::Core65C02),
    op("JMP", AddrMode::IndAbsX, 0x7C, IsaTier::Core65C02),
    op("LDA", AddrMode::DpInd, 0xB2, IsaTier::Core65C02),
    op("ORA", AddrMode::DpInd, 0x12, IsaTier::Core65C02),
    op("PHX", AddrMode::Imp, 0xDA, IsaTier::Core65C02),
    op("PHY", AddrMode::Imp, 0x5A, IsaTier::Core65C02),
    op("PLX", AddrMode::Imp, 0xFA, IsaTier::Core65C02),
    op("PLY", AddrMode::Imp, 0x7A, IsaTier::Core65C02),
    op("SBC", AddrMode::DpInd, 0xF2, IsaTier::Core65C02),
    op("STA", AddrMode::DpInd, 0x92, IsaTier::Core65C02),
    op("STZ", AddrMode::Dp, 0x64, IsaTier::Core65C02),
    op("STZ", AddrMode::DpX, 0x74, IsaTier::Core65C02),
    op("STZ", AddrMode::Abs, 0x9C, IsaTier::Core65C02),
    op("STZ", AddrMode::AbsX, 0x9E, IsaTier::Core65C02),
    op("STP", AddrMode::Imp, 0xDB, IsaTier::Core65C02),
    op("TRB", AddrMode::Dp, 0x14, IsaTier::Core65C02),
    op("TRB", AddrMode::Abs, 0x1C, IsaTier::Core65C02),
    op("TSB", AddrMode::Dp, 0x04, IsaTier::Core65C02),
    op("TSB", AddrMode::Abs, 0x0C, IsaTier::Core65C02),
    op("WAI", AddrMode::Imp, 0xCB, IsaTier::Core65C02),
];

const fn op(mnemonic: &'static str, mode: AddrMode, opcode: u8, isa: IsaTier) -> OpcodeDef {
    OpcodeDef {
        mnemonic,
        mode,
        opcode,
        isa,
    }
}

pub fn is_branch_mnemonic(mnemonic: &str) -> bool {
    matches!(
        mnemonic,
        "BCC" | "BCS" | "BEQ" | "BMI" | "BNE" | "BPL" | "BVC" | "BVS" | "BRA"
    )
}

pub fn immediate_uses_xy(mnemonic: &str) -> bool {
    matches!(mnemonic, "LDX" | "LDY" | "CPX" | "CPY")
}

pub fn opcode_for(mnemonic: &str, mode: AddrMode) -> Option<u8> {
    OPCODES
        .iter()
        .find(|entry| entry.mnemonic == mnemonic && entry.mode == mode)
        .map(|entry| entry.opcode)
}

pub fn mode_size_fixed(mode: AddrMode) -> Option<u32> {
    match mode {
        AddrMode::Imp | AddrMode::Acc => Some(1),
        AddrMode::ImmA | AddrMode::ImmXY => None,
        AddrMode::Rel8
        | AddrMode::Dp
        | AddrMode::DpX
        | AddrMode::DpY
        | AddrMode::IndX
        | AddrMode::IndY
        | AddrMode::DpInd => Some(2),
        AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind | AddrMode::IndAbsX => {
            Some(3)
        }
    }
}

pub fn opcode_table() -> &'static [OpcodeDef] {
    OPCODES
}
