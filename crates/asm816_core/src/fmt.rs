use pretty::{Arena, DocAllocator, DocBuilder};

use crate::{
    diag::{Diag, has_errors},
    expr::{BinOp, Expr, UnOp},
    ir::{DirArg, Directive, Instruction, Item, Operand, Program},
    lex::lex_file,
    parse::parse_tokens,
    source::SourceManager,
};

const FORMAT_WIDTH: usize = 100;
const CODE_INDENT: &str = "    ";
const CONTINUATION_INDENT: isize = 4;

type Doc<'a> = DocBuilder<'a, Arena<'a>>;

pub fn format_program(program: &Program) -> String {
    format_ir(program)
}

pub fn format_ir(program: &Program) -> String {
    let arena = Arena::new();
    let mut lines = Vec::with_capacity(program.items.len());
    for item in &program.items {
        lines.push(doc_item(&arena, item));
    }

    let body = if lines.is_empty() {
        arena.nil()
    } else {
        arena.intersperse(lines, arena.hardline())
    };
    let doc = body.append(arena.hardline());

    render_doc(doc)
}

pub fn format_source(src: &str, filename: &str) -> Result<String, Vec<Diag>> {
    let mut source_manager = SourceManager::new(Vec::new());
    let file = source_manager.add_virtual_file(filename, src);
    let (tokens, mut diags) = lex_file(&source_manager, file);
    let (program, parse_diags) = parse_tokens(&tokens);
    diags.extend(parse_diags);

    if has_errors(&diags) {
        return Err(diags);
    }

    Ok(format_ir(&program))
}

fn render_doc(doc: Doc<'_>) -> String {
    let mut out = Vec::new();
    doc.render(FORMAT_WIDTH, &mut out)
        .expect("writing to Vec<u8> cannot fail");
    String::from_utf8(out).expect("formatter produced invalid UTF-8")
}

fn doc_item<'a>(arena: &'a Arena<'a>, item: &Item) -> Doc<'a> {
    match item {
        Item::Label(label) => with_trailing_comment(
            arena,
            arena
                .text(label.name.value.clone())
                .append(arena.text(":"))
                .group(),
            label.trailing_comment.as_deref(),
        ),
        Item::Assign(assign) => {
            let code = arena
                .text(assign.name.value.clone())
                .append(arena.text(" = "))
                .append(doc_expr(arena, &assign.expr.value))
                .group();
            with_trailing_comment(arena, indented_code(arena, code), assign.trailing_comment.as_deref())
        }
        Item::Directive(directive) => with_trailing_comment(
            arena,
            indented_code(arena, doc_directive(arena, directive)),
            directive.trailing_comment.as_deref(),
        ),
        Item::Instruction(instruction) => with_trailing_comment(
            arena,
            indented_code(arena, doc_instruction(arena, instruction)),
            instruction.trailing_comment.as_deref(),
        ),
        Item::CommentLine(comment) => arena
            .text(";")
            .append(arena.text(comment.value.clone())),
        Item::EmptyLine(_) => arena.nil(),
    }
}

fn indented_code<'a>(arena: &'a Arena<'a>, code: Doc<'a>) -> Doc<'a> {
    arena.text(CODE_INDENT).append(code)
}

fn with_trailing_comment<'a>(
    arena: &'a Arena<'a>,
    code: Doc<'a>,
    comment: Option<&str>,
) -> Doc<'a> {
    match comment {
        Some(comment) => code
            .append(arena.text("  ;"))
            .append(arena.text(comment.to_string()))
            .group(),
        None => code,
    }
}

fn doc_directive<'a>(arena: &'a Arena<'a>, directive: &Directive) -> Doc<'a> {
    let head = arena.text(directive.name.value.clone());
    if directive.args.is_empty() {
        return head;
    }

    let args = directive.args.iter().map(|arg| doc_dir_arg(arena, &arg.value));
    let joined = arena
        .intersperse(args, arena.text(",").append(arena.line()))
        .group()
        .nest(CONTINUATION_INDENT);

    head.append(arena.space()).append(joined)
}

fn doc_dir_arg<'a>(arena: &'a Arena<'a>, arg: &DirArg) -> Doc<'a> {
    match arg {
        DirArg::Expr(expr) => doc_expr(arena, expr),
        DirArg::String(value) => arena.text(escape_string(value)),
        DirArg::Ident(ident) => arena.text(ident.clone()),
    }
}

fn doc_instruction<'a>(arena: &'a Arena<'a>, instruction: &Instruction) -> Doc<'a> {
    let head = arena.text(instruction.mnemonic.value.clone());
    let Some(operand) = &instruction.operand else {
        return head;
    };

    head.append(arena.space())
        .append(doc_operand(arena, &operand.value).nest(CONTINUATION_INDENT))
}

fn doc_operand<'a>(arena: &'a Arena<'a>, operand: &Operand) -> Doc<'a> {
    match operand {
        Operand::Imm(expr) => arena.text("#").append(doc_expr(arena, expr)),
        Operand::Acc => arena.text("A"),
        Operand::Expr(expr) => doc_expr(arena, expr),
        Operand::ExprX(expr) => doc_expr(arena, expr).append(arena.text(",X")),
        Operand::ExprY(expr) => doc_expr(arena, expr).append(arena.text(",Y")),
        Operand::Ind(expr) => arena
            .text("(")
            .append(doc_expr(arena, expr))
            .append(arena.text(")")),
        Operand::IndX(expr) => arena
            .text("(")
            .append(doc_expr(arena, expr))
            .append(arena.text(",X)")),
        Operand::IndY(expr) => arena
            .text("(")
            .append(doc_expr(arena, expr))
            .append(arena.text("),Y")),
    }
}

fn doc_expr<'a>(arena: &'a Arena<'a>, expr: &Expr) -> Doc<'a> {
    doc_expr_with_prec(arena, expr, 0)
}

fn doc_expr_with_prec<'a>(arena: &'a Arena<'a>, expr: &Expr, parent_prec: u8) -> Doc<'a> {
    match expr {
        Expr::Number(value) => arena.as_string(*value),
        Expr::Symbol(name) => arena.text(name.clone()),
        Expr::CurrentPc => arena.text("*"),
        Expr::Unary { op, rhs } => {
            let prec = 4;
            let op = match op {
                UnOp::Plus => "+",
                UnOp::Minus => "-",
            };
            let doc = arena
                .text(op)
                .append(doc_expr_with_prec(arena, rhs, prec))
                .group();
            maybe_parenthesize(arena, doc, parent_prec > prec)
        }
        Expr::LowByte(inner) => {
            let prec = 4;
            let doc = arena
                .text("<")
                .append(doc_expr_with_prec(arena, inner, prec))
                .group();
            maybe_parenthesize(arena, doc, parent_prec > prec)
        }
        Expr::HighByte(inner) => {
            let prec = 4;
            let doc = arena
                .text(">")
                .append(doc_expr_with_prec(arena, inner, prec))
                .group();
            maybe_parenthesize(arena, doc, parent_prec > prec)
        }
        Expr::Binary { op, lhs, rhs } => {
            let (prec, symbol) = match op {
                BinOp::Or => (1, "|"),
                BinOp::Xor => (2, "^"),
                BinOp::And => (3, "&"),
                BinOp::Shl => (4, "<<"),
                BinOp::Shr => (4, ">>"),
                BinOp::Add => (5, "+"),
                BinOp::Sub => (5, "-"),
                BinOp::Mul => (6, "*"),
                BinOp::Div => (6, "/"),
            };

            let lhs = doc_expr_with_prec(arena, lhs, prec);
            let rhs = doc_expr_with_prec(arena, rhs, prec + 1);
            let doc = lhs.append(arena.text(symbol)).append(rhs).group();
            maybe_parenthesize(arena, doc, parent_prec > prec)
        }
    }
}

fn maybe_parenthesize<'a>(arena: &'a Arena<'a>, doc: Doc<'a>, parenthesize: bool) -> Doc<'a> {
    if parenthesize {
        arena.text("(").append(doc).append(arena.text(")"))
    } else {
        doc
    }
}

fn escape_string(value: &str) -> String {
    let escaped = value
        .replace('\\', "\\\\")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
        .replace('"', "\\\"");
    format!("\"{escaped}\"")
}
