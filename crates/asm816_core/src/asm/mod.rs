use indexmap::IndexMap;

use crate::{
    diag::{Diag, DiagLabel},
    encode::{AddrMode, immediate_uses_xy, is_branch_mnemonic, mode_size_fixed, opcode_for},
    expr::{Expr, eval_expr},
    ir::{DirArg, Directive, Instruction, Item, Operand, Program},
    source::{FileId, Span, Spanned},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Width {
    W8,
    W16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CpuMode {
    pub a: Width,
    pub xy: Width,
}

impl Default for CpuMode {
    fn default() -> Self {
        Self {
            a: Width::W8,
            xy: Width::W8,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymValue {
    Known(i64),
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymKind {
    Label,
    Const,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub kind: SymKind,
    pub value: SymValue,
    pub defined_at: Option<(FileId, Span)>,
}

#[derive(Clone, Debug, Default)]
pub struct SymTab {
    pub map: IndexMap<String, Symbol>,
}

impl SymTab {
    pub fn get_known(&self, name: &str) -> Option<i64> {
        self.map.get(name).and_then(|symbol| match symbol.value {
            SymValue::Known(value) => Some(value),
            SymValue::Unknown => None,
        })
    }

    fn define(
        &mut self,
        name: &str,
        kind: SymKind,
        value: SymValue,
        file: FileId,
        span: Span,
        diags: &mut Vec<Diag>,
    ) {
        if let Some(existing) = self.map.get(name) {
            if let Some((def_file, def_span)) = &existing.defined_at {
                diags.push(
                    Diag::error(file, span.clone(), format!("symbol `{name}` redefined"))
                        .with_label(DiagLabel {
                            file: *def_file,
                            span: def_span.clone(),
                            message: "previous definition here".to_string(),
                        }),
                );
                return;
            }
        }

        self.map.insert(
            name.to_string(),
            Symbol {
                kind,
                value,
                defined_at: Some((file, span)),
            },
        );
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FixupKind {
    Rel8,
    ImmA,
    ImmXY,
    DpOrAbs,
    DpXOrAbsX,
    DpYOrAbsY,
}

#[derive(Clone, Debug)]
pub struct Fixup {
    pub at_pc: u32,
    pub kind: FixupKind,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct LineLayout {
    pub item_index: usize,
    pub pc_start: u32,
    pub size: u32,
    pub mode_in: CpuMode,
    pub mode_out: CpuMode,
    pub addr_mode: Option<AddrMode>,
}

#[derive(Clone, Debug)]
pub struct Pass1Result {
    pub symtab: SymTab,
    pub layouts: Vec<LineLayout>,
    pub fixups: Vec<Fixup>,
    pub diags: Vec<Diag>,
    pub origin: Option<u32>,
}

pub fn pass1(program: &Program, initial_mode: CpuMode) -> Pass1Result {
    let mut symtab = SymTab::default();
    let mut diags = Vec::new();
    let mut layouts = Vec::with_capacity(program.items.len());
    let mut fixups = Vec::new();

    let mut mode = initial_mode;
    let mut pc = 0u32;
    let mut seen_org = false;
    let mut origin = None;

    for (item_index, item) in program.items.iter().enumerate() {
        let mode_in = mode;
        let pc_start = pc;
        let mut size = 0u32;
        let mut addr_mode = None;

        match item {
            Item::Label(label) => {
                symtab.define(
                    &label.name.value,
                    SymKind::Label,
                    SymValue::Known(pc as i64),
                    label.name.file,
                    label.name.span.clone(),
                    &mut diags,
                );
            }
            Item::Assign(assign) => {
                let value = try_eval_expr(&assign.expr.value, &symtab, pc as i64)
                    .map(SymValue::Known)
                    .unwrap_or(SymValue::Unknown);
                symtab.define(
                    &assign.name.value,
                    SymKind::Const,
                    value,
                    assign.name.file,
                    assign.name.span.clone(),
                    &mut diags,
                );
            }
            Item::Directive(directive) => {
                let name = directive.name.value.as_str();
                match name {
                    ".org" => {
                        if seen_org {
                            diags.push(Diag::error(
                                directive.name.file,
                                directive.span.clone(),
                                "single-origin mode allows only one `.org`",
                            ));
                        } else if let Some(value) =
                            directive_expr_value(directive, 0, &symtab, pc, &mut diags)
                        {
                            if value < 0 || value > u32::MAX as i64 {
                                diags.push(Diag::error(
                                    directive.name.file,
                                    directive.span.clone(),
                                    "`.org` value is out of range",
                                ));
                            } else {
                                pc = value as u32;
                                origin = Some(pc);
                                seen_org = true;
                            }
                        }
                    }
                    ".byte" => {
                        size = directive
                            .args
                            .iter()
                            .map(|arg| match &arg.value {
                                DirArg::Expr(_) | DirArg::Ident(_) => 1u32,
                                DirArg::String(value) => value.len() as u32,
                            })
                            .sum();
                    }
                    ".word" => {
                        size = (directive.args.len() as u32) * 2;
                    }
                    ".asciiz" => {
                        if let Some(first) = directive.args.first() {
                            match &first.value {
                                DirArg::String(value) => {
                                    size = value.len() as u32 + 1;
                                }
                                _ => {
                                    diags.push(Diag::error(
                                        first.file,
                                        first.span.clone(),
                                        "`.asciiz` expects a string literal",
                                    ));
                                }
                            }
                        } else {
                            diags.push(Diag::error(
                                directive.name.file,
                                directive.span.clone(),
                                "`.asciiz` requires one argument",
                            ));
                        }
                    }
                    ".res" => {
                        if let Some(value) =
                            directive_expr_value(directive, 0, &symtab, pc, &mut diags)
                        {
                            if value < 0 {
                                diags.push(Diag::error(
                                    directive.name.file,
                                    directive.span.clone(),
                                    "`.res` count must be non-negative",
                                ));
                            } else {
                                size = value as u32;
                            }
                        }
                    }
                    ".a8" => mode.a = Width::W8,
                    ".a16" => mode.a = Width::W16,
                    ".i8" => mode.xy = Width::W8,
                    ".i16" => mode.xy = Width::W16,
                    ".include" => {
                        diags.push(
                            Diag::error(
                                directive.name.file,
                                directive.span.clone(),
                                "`.include` is no longer supported; use module `import` instead",
                            )
                            .with_help("replace `.include \"x.s\"` with `import module::path`"),
                        );
                    }
                    _ => {
                        diags.push(Diag::error(
                            directive.name.file,
                            directive.span.clone(),
                            format!("unsupported directive `{}`", directive.name.value),
                        ));
                    }
                }
            }
            Item::Instruction(instruction) => {
                let (selected_mode, selected_size, maybe_fixup) =
                    resolve_instruction_layout(instruction, mode, &symtab, pc, &mut diags);
                size = selected_size;
                addr_mode = Some(selected_mode);
                if let Some(fixup) = maybe_fixup {
                    fixups.push(fixup);
                }
            }
            Item::CommentLine(_) => {}
            Item::EmptyLine(_) => {}
        }

        layouts.push(LineLayout {
            item_index,
            pc_start,
            size,
            mode_in,
            mode_out: mode,
            addr_mode,
        });

        pc = pc.saturating_add(size);
    }

    Pass1Result {
        symtab,
        layouts,
        fixups,
        diags,
        origin,
    }
}

pub fn pass2(program: &Program, pass1: &Pass1Result) -> (Vec<u8>, Vec<Diag>) {
    let mut bytes = Vec::new();
    let mut diags = Vec::new();

    let base_origin = pass1.origin.unwrap_or(0);

    for layout in &pass1.layouts {
        let item = &program.items[layout.item_index];

        if layout.size > 0 {
            if layout.pc_start < base_origin {
                diags.push(Diag::error(
                    file_for_item(item),
                    item.span(),
                    "internal layout error: pc before origin",
                ));
                continue;
            }

            let expected_offset = (layout.pc_start - base_origin) as usize;
            if bytes.len() < expected_offset {
                bytes.resize(expected_offset, 0);
            }
        }

        let before = bytes.len();

        match item {
            Item::Directive(directive) => {
                encode_directive(directive, layout, pass1, &mut bytes, &mut diags);
            }
            Item::Instruction(instruction) => {
                encode_instruction(instruction, layout, pass1, &mut bytes, &mut diags);
            }
            _ => {}
        }

        let emitted = (bytes.len() - before) as u32;
        if emitted != layout.size {
            diags.push(Diag::error(
                file_for_item(item),
                item.span(),
                format!(
                    "internal size mismatch: pass1 expected {}, pass2 emitted {emitted}",
                    layout.size
                ),
            ));
            if emitted < layout.size {
                bytes.resize(before + layout.size as usize, 0);
            } else {
                bytes.truncate(before + layout.size as usize);
            }
        }
    }

    (bytes, diags)
}

fn resolve_instruction_layout(
    instruction: &Instruction,
    mode: CpuMode,
    symtab: &SymTab,
    pc: u32,
    diags: &mut Vec<Diag>,
) -> (AddrMode, u32, Option<Fixup>) {
    let mnemonic = instruction.mnemonic.value.as_str();
    let operand = instruction.operand.as_ref();

    let mut fixup = None;

    let mut selected_mode = if is_branch_mnemonic(mnemonic) {
        AddrMode::Rel8
    } else {
        match operand.map(|operand| &operand.value) {
            None => resolve_no_operand_mode(mnemonic),
            Some(Operand::Acc) => AddrMode::Acc,
            Some(Operand::Imm(_)) => {
                if immediate_uses_xy(mnemonic) {
                    AddrMode::ImmXY
                } else {
                    AddrMode::ImmA
                }
            }
            Some(Operand::Expr(expr)) => select_expr_mode(
                mnemonic,
                expr,
                AddrMode::Dp,
                AddrMode::Abs,
                instruction,
                diags,
            ),
            Some(Operand::ExprX(expr)) => select_expr_mode(
                mnemonic,
                expr,
                AddrMode::DpX,
                AddrMode::AbsX,
                instruction,
                diags,
            ),
            Some(Operand::ExprY(expr)) => select_expr_mode(
                mnemonic,
                expr,
                AddrMode::DpY,
                AddrMode::AbsY,
                instruction,
                diags,
            ),
            Some(Operand::Ind(expr)) => select_indirect_mode(mnemonic, expr),
            Some(Operand::IndX(_)) => {
                if mnemonic == "JMP" && opcode_for(mnemonic, AddrMode::IndAbsX).is_some() {
                    AddrMode::IndAbsX
                } else {
                    AddrMode::IndX
                }
            }
            Some(Operand::IndY(_)) => AddrMode::IndY,
        }
    };

    if opcode_for(mnemonic, selected_mode).is_none() {
        diags.push(Diag::error(
            instruction.mnemonic.file,
            instruction.span.clone(),
            format!("unsupported addressing mode for `{mnemonic}`"),
        ));
        selected_mode = AddrMode::Imp;
    }

    if let Some(expr) = operand_expression(instruction) {
        if try_eval_expr(expr, symtab, pc as i64).is_none() {
            let fixup_kind = match selected_mode {
                AddrMode::Rel8 => FixupKind::Rel8,
                AddrMode::ImmA => FixupKind::ImmA,
                AddrMode::ImmXY => FixupKind::ImmXY,
                AddrMode::Dp | AddrMode::DpInd | AddrMode::IndX | AddrMode::IndY => {
                    FixupKind::DpOrAbs
                }
                AddrMode::DpX => FixupKind::DpXOrAbsX,
                AddrMode::DpY => FixupKind::DpYOrAbsY,
                _ => FixupKind::DpOrAbs,
            };
            if let Some(spanned_expr) = operand_expression_spanned(instruction) {
                fixup = Some(Fixup {
                    at_pc: pc.saturating_add(1),
                    kind: fixup_kind,
                    expr: spanned_expr,
                });
            }
        }
    }

    let size = instruction_size(selected_mode, mode);
    (selected_mode, size, fixup)
}

fn encode_directive(
    directive: &Directive,
    layout: &LineLayout,
    pass1: &Pass1Result,
    bytes: &mut Vec<u8>,
    diags: &mut Vec<Diag>,
) {
    match directive.name.value.as_str() {
        ".org" | ".a8" | ".a16" | ".i8" | ".i16" => {}
        ".include" => {}
        ".byte" => {
            for arg in &directive.args {
                match &arg.value {
                    DirArg::Expr(expr) => match eval_arg_expr(expr, pass1, layout.pc_start) {
                        Ok(value) => {
                            if let Some(byte) = checked_u8(value, arg.file, arg.span.clone(), diags)
                            {
                                bytes.push(byte);
                            }
                        }
                        Err(message) => {
                            diags.push(Diag::error(arg.file, arg.span.clone(), message))
                        }
                    },
                    DirArg::String(value) => bytes.extend_from_slice(value.as_bytes()),
                    DirArg::Ident(name) => diags.push(Diag::error(
                        arg.file,
                        arg.span.clone(),
                        format!("unexpected identifier `{name}` in `.byte`"),
                    )),
                }
            }
        }
        ".word" => {
            for arg in &directive.args {
                match &arg.value {
                    DirArg::Expr(expr) => match eval_arg_expr(expr, pass1, layout.pc_start) {
                        Ok(value) => {
                            if let Some(word) =
                                checked_u16(value, arg.file, arg.span.clone(), diags)
                            {
                                bytes.extend_from_slice(&word.to_le_bytes());
                            }
                        }
                        Err(message) => {
                            diags.push(Diag::error(arg.file, arg.span.clone(), message))
                        }
                    },
                    _ => diags.push(Diag::error(
                        arg.file,
                        arg.span.clone(),
                        "`.word` expects expression arguments",
                    )),
                }
            }
        }
        ".asciiz" => {
            if let Some(first) = directive.args.first() {
                match &first.value {
                    DirArg::String(value) => {
                        bytes.extend_from_slice(value.as_bytes());
                        bytes.push(0);
                    }
                    _ => diags.push(Diag::error(
                        first.file,
                        first.span.clone(),
                        "`.asciiz` expects a string literal",
                    )),
                }
            }
        }
        ".res" => {
            if let Some(first) = directive.args.first() {
                match &first.value {
                    DirArg::Expr(expr) => match eval_arg_expr(expr, pass1, layout.pc_start) {
                        Ok(value) if value >= 0 => bytes.resize(bytes.len() + value as usize, 0),
                        Ok(_) => diags.push(Diag::error(
                            first.file,
                            first.span.clone(),
                            "`.res` count must be non-negative",
                        )),
                        Err(message) => {
                            diags.push(Diag::error(first.file, first.span.clone(), message))
                        }
                    },
                    _ => diags.push(Diag::error(
                        first.file,
                        first.span.clone(),
                        "`.res` expects an expression",
                    )),
                }
            }
        }
        _ => {}
    }
}

fn encode_instruction(
    instruction: &Instruction,
    layout: &LineLayout,
    pass1: &Pass1Result,
    bytes: &mut Vec<u8>,
    diags: &mut Vec<Diag>,
) {
    let mnemonic = instruction.mnemonic.value.as_str();
    let mode = layout.addr_mode.unwrap_or(AddrMode::Imp);

    let opcode = match opcode_for(mnemonic, mode) {
        Some(opcode) => opcode,
        None => {
            diags.push(Diag::error(
                instruction.mnemonic.file,
                instruction.span.clone(),
                format!("no opcode for `{mnemonic}` in mode {mode:?}"),
            ));
            return;
        }
    };

    bytes.push(opcode);

    match mode {
        AddrMode::Imp | AddrMode::Acc => {}
        AddrMode::Rel8 => {
            let Some(operand) = instruction.operand.as_ref() else {
                diags.push(Diag::error(
                    instruction.mnemonic.file,
                    instruction.span.clone(),
                    "branch instruction requires an operand",
                ));
                bytes.push(0);
                return;
            };

            let Some(expr) = operand_expr_from_operand(operand) else {
                diags.push(Diag::error(
                    operand.file,
                    operand.span.clone(),
                    "invalid branch operand",
                ));
                bytes.push(0);
                return;
            };

            match eval_expr(
                expr,
                &|name| pass1.symtab.get_known(name),
                layout.pc_start as i64,
            ) {
                Ok(target) => {
                    let next_pc = layout.pc_start as i64 + layout.size as i64;
                    let delta = target - next_pc;
                    if (-128..=127).contains(&delta) {
                        bytes.push(delta as i8 as u8);
                    } else {
                        diags.push(Diag::error(
                            operand.file,
                            operand.span.clone(),
                            format!("branch target out of range: offset {delta}"),
                        ));
                        bytes.push(0);
                    }
                }
                Err(message) => {
                    diags.push(Diag::error(operand.file, operand.span.clone(), message));
                    bytes.push(0);
                }
            }
        }
        AddrMode::ImmA | AddrMode::ImmXY => {
            let Some(operand) = instruction.operand.as_ref() else {
                diags.push(Diag::error(
                    instruction.mnemonic.file,
                    instruction.span.clone(),
                    "missing immediate operand",
                ));
                bytes.push(0);
                return;
            };

            let Some(expr) = operand_expr_from_operand(operand) else {
                diags.push(Diag::error(
                    operand.file,
                    operand.span.clone(),
                    "invalid immediate operand",
                ));
                bytes.push(0);
                return;
            };

            match eval_expr(
                expr,
                &|name| pass1.symtab.get_known(name),
                layout.pc_start as i64,
            ) {
                Ok(value) => {
                    let width = match mode {
                        AddrMode::ImmA => layout.mode_in.a,
                        AddrMode::ImmXY => layout.mode_in.xy,
                        _ => Width::W8,
                    };

                    match width {
                        Width::W8 => {
                            if let Some(byte) =
                                checked_u8(value, operand.file, operand.span.clone(), diags)
                            {
                                bytes.push(byte);
                            }
                        }
                        Width::W16 => {
                            if let Some(word) =
                                checked_u16(value, operand.file, operand.span.clone(), diags)
                            {
                                bytes.extend_from_slice(&word.to_le_bytes());
                            }
                        }
                    }
                }
                Err(message) => {
                    diags.push(Diag::error(operand.file, operand.span.clone(), message))
                }
            }
        }
        AddrMode::Dp
        | AddrMode::DpX
        | AddrMode::DpY
        | AddrMode::IndX
        | AddrMode::IndY
        | AddrMode::DpInd => {
            encode_expression_byte_operand(instruction, layout, pass1, bytes, diags);
        }
        AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind | AddrMode::IndAbsX => {
            encode_expression_word_operand(instruction, layout, pass1, bytes, diags);
        }
    }
}

fn encode_expression_byte_operand(
    instruction: &Instruction,
    layout: &LineLayout,
    pass1: &Pass1Result,
    bytes: &mut Vec<u8>,
    diags: &mut Vec<Diag>,
) {
    let Some(operand) = instruction.operand.as_ref() else {
        diags.push(Diag::error(
            instruction.mnemonic.file,
            instruction.span.clone(),
            "missing operand",
        ));
        bytes.push(0);
        return;
    };

    let Some(expr) = operand_expr_from_operand(operand) else {
        diags.push(Diag::error(
            operand.file,
            operand.span.clone(),
            "invalid operand",
        ));
        bytes.push(0);
        return;
    };

    match eval_expr(
        expr,
        &|name| pass1.symtab.get_known(name),
        layout.pc_start as i64,
    ) {
        Ok(value) => {
            if let Some(byte) = checked_u8(value, operand.file, operand.span.clone(), diags) {
                bytes.push(byte);
            }
        }
        Err(message) => {
            diags.push(Diag::error(operand.file, operand.span.clone(), message));
            bytes.push(0);
        }
    }
}

fn encode_expression_word_operand(
    instruction: &Instruction,
    layout: &LineLayout,
    pass1: &Pass1Result,
    bytes: &mut Vec<u8>,
    diags: &mut Vec<Diag>,
) {
    let Some(operand) = instruction.operand.as_ref() else {
        diags.push(Diag::error(
            instruction.mnemonic.file,
            instruction.span.clone(),
            "missing operand",
        ));
        bytes.extend_from_slice(&0u16.to_le_bytes());
        return;
    };

    let Some(expr) = operand_expr_from_operand(operand) else {
        diags.push(Diag::error(
            operand.file,
            operand.span.clone(),
            "invalid operand",
        ));
        bytes.extend_from_slice(&0u16.to_le_bytes());
        return;
    };

    match eval_expr(
        expr,
        &|name| pass1.symtab.get_known(name),
        layout.pc_start as i64,
    ) {
        Ok(value) => {
            if let Some(word) = checked_u16(value, operand.file, operand.span.clone(), diags) {
                bytes.extend_from_slice(&word.to_le_bytes());
            }
        }
        Err(message) => {
            diags.push(Diag::error(operand.file, operand.span.clone(), message));
            bytes.extend_from_slice(&0u16.to_le_bytes());
        }
    }
}

fn checked_u8(value: i64, file: FileId, span: Span, diags: &mut Vec<Diag>) -> Option<u8> {
    if (-128..=255).contains(&value) {
        Some((value & 0xFF) as u8)
    } else {
        diags.push(Diag::error(
            file,
            span,
            format!("value {value} does not fit in 8 bits"),
        ));
        None
    }
}

fn checked_u16(value: i64, file: FileId, span: Span, diags: &mut Vec<Diag>) -> Option<u16> {
    if (-32768..=65535).contains(&value) {
        Some((value & 0xFFFF) as u16)
    } else {
        diags.push(Diag::error(
            file,
            span,
            format!("value {value} does not fit in 16 bits"),
        ));
        None
    }
}

fn eval_arg_expr(expr: &Expr, pass1: &Pass1Result, current_pc: u32) -> Result<i64, String> {
    eval_expr(
        expr,
        &|name| pass1.symtab.get_known(name),
        current_pc as i64,
    )
}

fn directive_expr_value(
    directive: &Directive,
    index: usize,
    symtab: &SymTab,
    pc: u32,
    diags: &mut Vec<Diag>,
) -> Option<i64> {
    let Some(arg) = directive.args.get(index) else {
        diags.push(Diag::error(
            directive.name.file,
            directive.span.clone(),
            format!("`{}` expects argument {}", directive.name.value, index + 1),
        ));
        return None;
    };

    let DirArg::Expr(expr) = &arg.value else {
        diags.push(Diag::error(
            arg.file,
            arg.span.clone(),
            format!("`{}` expects an expression", directive.name.value),
        ));
        return None;
    };

    match eval_expr(expr, &|name| symtab.get_known(name), pc as i64) {
        Ok(value) => Some(value),
        Err(message) => {
            diags.push(
                Diag::error(
                    arg.file,
                    arg.span.clone(),
                    format!("expression must be known in pass1"),
                )
                .with_label(DiagLabel {
                    file: arg.file,
                    span: arg.span.clone(),
                    message,
                }),
            );
            None
        }
    }
}

fn resolve_no_operand_mode(mnemonic: &str) -> AddrMode {
    if opcode_for(mnemonic, AddrMode::Imp).is_some() {
        AddrMode::Imp
    } else if opcode_for(mnemonic, AddrMode::Acc).is_some() {
        AddrMode::Acc
    } else {
        AddrMode::Imp
    }
}

fn select_expr_mode(
    mnemonic: &str,
    expr: &Expr,
    dp_mode: AddrMode,
    abs_mode: AddrMode,
    instruction: &Instruction,
    diags: &mut Vec<Diag>,
) -> AddrMode {
    let forced_dp = is_forced_direct_expr(expr);

    if mnemonic == "JSR" || (mnemonic == "JMP" && abs_mode == AddrMode::Abs) {
        if forced_dp {
            diags.push(Diag::error(
                instruction.mnemonic.file,
                instruction.span.clone(),
                format!(
                    "forced direct-page operand is not valid for `{mnemonic}`; use plain expression"
                ),
            ));
        }
        return abs_mode;
    }

    if forced_dp {
        if opcode_for(mnemonic, dp_mode).is_some() {
            return dp_mode;
        }

        if opcode_for(mnemonic, abs_mode).is_some() {
            diags.push(Diag::error(
                instruction.mnemonic.file,
                instruction.span.clone(),
                format!("`{mnemonic}` does not support forced direct-page in this addressing form"),
            ));
            return abs_mode;
        }
    }

    if opcode_for(mnemonic, abs_mode).is_some() {
        abs_mode
    } else {
        dp_mode
    }
}

fn select_indirect_mode(mnemonic: &str, _expr: &Expr) -> AddrMode {
    if mnemonic == "JMP" {
        AddrMode::Ind
    } else if opcode_for(mnemonic, AddrMode::DpInd).is_some() {
        AddrMode::DpInd
    } else {
        AddrMode::Ind
    }
}

fn is_forced_direct_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::LowByte(_))
}

fn instruction_size(mode: AddrMode, cpu_mode: CpuMode) -> u32 {
    match mode {
        AddrMode::ImmA => match cpu_mode.a {
            Width::W8 => 2,
            Width::W16 => 3,
        },
        AddrMode::ImmXY => match cpu_mode.xy {
            Width::W8 => 2,
            Width::W16 => 3,
        },
        _ => mode_size_fixed(mode).unwrap_or(1),
    }
}

fn try_eval_expr(expr: &Expr, symtab: &SymTab, pc: i64) -> Option<i64> {
    eval_expr(expr, &|name| symtab.get_known(name), pc).ok()
}

fn operand_expression(instruction: &Instruction) -> Option<&Expr> {
    instruction
        .operand
        .as_ref()
        .and_then(operand_expr_from_operand)
}

fn operand_expression_spanned(instruction: &Instruction) -> Option<Spanned<Expr>> {
    let operand = instruction.operand.as_ref()?;
    let expr = operand_expr_from_operand(operand)?;
    Some(Spanned {
        file: operand.file,
        span: operand.span.clone(),
        value: expr.clone(),
    })
}

fn operand_expr_from_operand(operand: &Spanned<Operand>) -> Option<&Expr> {
    match &operand.value {
        Operand::Imm(expr)
        | Operand::Expr(expr)
        | Operand::ExprX(expr)
        | Operand::ExprY(expr)
        | Operand::Ind(expr)
        | Operand::IndX(expr)
        | Operand::IndY(expr) => Some(expr),
        Operand::Acc => None,
    }
}

fn file_for_item(item: &Item) -> FileId {
    match item {
        Item::Label(label) => label.name.file,
        Item::Assign(assign) => assign.name.file,
        Item::Directive(directive) => directive.name.file,
        Item::Instruction(instruction) => instruction.mnemonic.file,
        Item::CommentLine(comment) => comment.file,
        Item::EmptyLine(empty) => empty.file,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        asm::{CpuMode, Width, pass1},
        diag::has_errors,
        lex::lex_file,
        parse::parse_tokens,
        source::SourceManager,
    };

    #[test]
    fn detects_repeated_org() {
        let mut manager = SourceManager::new(Vec::new());
        let file = manager.add_virtual_file("test.s", ".org $8000\n.org $9000\n");
        let (tokens, mut diags) = lex_file(&manager, file);
        let (program, parse_diags) = parse_tokens(&tokens);
        diags.extend(parse_diags);
        assert!(!has_errors(&diags));

        let pass1 = pass1(&program, CpuMode::default());
        assert!(has_errors(&pass1.diags));
    }

    #[test]
    fn tracks_mode_changes() {
        let mut manager = SourceManager::new(Vec::new());
        let file = manager.add_virtual_file("test.s", ".a16\nLDA #$1234\n");
        let (tokens, lex_diags) = lex_file(&manager, file);
        assert!(lex_diags.is_empty());
        let (program, parse_diags) = parse_tokens(&tokens);
        assert!(parse_diags.is_empty());

        let pass1 = pass1(&program, CpuMode::default());
        assert!(pass1.diags.is_empty());
        let instruction_layout = &pass1.layouts[1];
        assert_eq!(instruction_layout.mode_in.a, Width::W16);
        assert_eq!(instruction_layout.size, 3);
    }
}
