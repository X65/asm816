use crate::{
    expr::format_expr,
    ir::{DirArg, Directive, Instruction, Item, Operand, Program},
};

pub fn format_program(program: &Program) -> String {
    let mut out = String::new();

    for item in &program.items {
        match item {
            Item::Label(label) => {
                out.push_str(&label.name.value);
                out.push(':');
                out.push('\n');
            }
            Item::Assign(assign) => {
                out.push_str("    ");
                out.push_str(&assign.name.value);
                out.push_str(" = ");
                out.push_str(&format_expr(&assign.expr.value));
                out.push('\n');
            }
            Item::Directive(directive) => {
                out.push_str("    ");
                out.push_str(&format_directive(directive));
                out.push('\n');
            }
            Item::Instruction(instruction) => {
                out.push_str("    ");
                out.push_str(&format_instruction(instruction));
                out.push('\n');
            }
            Item::EmptyLine(_) => {
                out.push('\n');
            }
        }
    }

    out
}

fn format_directive(directive: &Directive) -> String {
    let mut out = directive.name.value.to_ascii_uppercase();
    if !directive.args.is_empty() {
        let joined = directive
            .args
            .iter()
            .map(|arg| format_dir_arg(&arg.value))
            .collect::<Vec<_>>()
            .join(", ");
        out.push(' ');
        out.push_str(&joined);
    }
    out
}

fn format_dir_arg(arg: &DirArg) -> String {
    match arg {
        DirArg::Expr(expr) => format_expr(expr),
        DirArg::String(value) => {
            let escaped = value
                .replace('\\', "\\\\")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t")
                .replace('"', "\\\"");
            format!("\"{escaped}\"")
        }
        DirArg::Ident(ident) => ident.clone(),
    }
}

fn format_instruction(instruction: &Instruction) -> String {
    let mut out = instruction.mnemonic.value.to_ascii_uppercase();
    if let Some(operand) = &instruction.operand {
        out.push(' ');
        out.push_str(&format_operand(&operand.value));
    }
    out
}

fn format_operand(operand: &Operand) -> String {
    match operand {
        Operand::Imm(expr) => format!("#{}", format_expr(expr)),
        Operand::Acc => "A".to_string(),
        Operand::Expr(expr) => format_expr(expr),
        Operand::ExprX(expr) => format!("{}, X", format_expr(expr)),
        Operand::ExprY(expr) => format!("{}, Y", format_expr(expr)),
        Operand::Ind(expr) => format!("({})", format_expr(expr)),
        Operand::IndX(expr) => format!("({}, X)", format_expr(expr)),
        Operand::IndY(expr) => format!("({}), Y", format_expr(expr)),
    }
}
