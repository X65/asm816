use chumsky::{
    IterParser,
    error::Rich,
    extra,
    prelude::{Parser, choice, end, just, recursive, select_ref},
    span::SimpleSpan,
};

use crate::{
    diag::{Diag, DiagLabel},
    expr::{BinOp, Expr, UnOp},
    ir::{Assign, DirArg, Directive, Instruction, Item, LabelDef, Operand, Program},
    lex::{Token, TokenKind},
    source::{FileId, Span, Spanned},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ParseToken {
    Ident(String),
    Number(String),
    String(String),

    Colon,
    Comma,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Hash,
    Plus,
    Minus,
    Star,
    Slash,
    Amp,
    Pipe,
    Caret,
    Shl,
    Shr,
    Lt,
    Gt,
    Eq,
    Dot,
}

#[derive(Clone, Debug)]
struct LineToken {
    kind: ParseToken,
    span: Span,
}

#[derive(Clone, Debug)]
struct Node<T> {
    value: T,
    span: SimpleSpan<usize>,
}

impl<T> Node<T> {
    fn new(value: T, span: SimpleSpan<usize>) -> Self {
        Self { value, span }
    }
}

#[derive(Clone, Debug, Default)]
struct ParsedLine {
    label: Option<Node<String>>,
    tail: Option<LineTail>,
}

#[derive(Clone, Debug)]
enum LineTail {
    Assign {
        name: Node<String>,
        expr: Node<Expr>,
    },
    Directive {
        name: Node<String>,
        args: Vec<Node<ParsedDirArg>>,
    },
    Instruction {
        mnemonic: Node<String>,
        operand: Option<Node<ParsedOperand>>,
    },
}

#[derive(Clone, Debug)]
enum ParsedDirArg {
    Expr(Expr),
    String(String),
    Ident(String),
}

#[derive(Clone, Debug)]
enum ParsedOperand {
    Imm(Expr),
    Expr(Expr),
    ExprX(Expr),
    ExprY(Expr),
    Ind(Expr),
    IndX(Expr),
    IndY(Expr),
}

#[derive(Clone, Copy)]
enum PrefixOp {
    Plus,
    Minus,
    Low,
    High,
}

type PError<'src> = Rich<'src, ParseToken>;
type PExtra<'src> = extra::Err<PError<'src>>;

pub fn parse_tokens(tokens: &[Token]) -> (Program, Vec<Diag>) {
    let mut program = Program::default();
    let mut diags = Vec::new();

    let mut line_start = 0usize;
    for (idx, token) in tokens.iter().enumerate() {
        if token.kind == TokenKind::Newline {
            parse_line(&tokens[line_start..idx], &mut program, &mut diags);
            line_start = idx + 1;
        }
    }

    if line_start < tokens.len() {
        parse_line(&tokens[line_start..], &mut program, &mut diags);
    }

    (program, diags)
}

fn parse_line(raw_tokens: &[Token], program: &mut Program, diags: &mut Vec<Diag>) {
    if raw_tokens.is_empty() {
        return;
    }

    let file = raw_tokens[0].file;
    let line_tokens = convert_line_tokens(raw_tokens, diags);
    if line_tokens.is_empty() {
        return;
    }

    let parse_input: Vec<ParseToken> = line_tokens.iter().map(|token| token.kind.clone()).collect();

    match line_parser().parse(parse_input.as_slice()).into_result() {
        Ok(parsed) => push_parsed_line(parsed, file, &line_tokens, program),
        Err(errors) => push_parse_errors(errors, file, &line_tokens, diags),
    }
}

fn push_parsed_line(
    parsed: ParsedLine,
    file: FileId,
    line_tokens: &[LineToken],
    program: &mut Program,
) {
    if let Some(label) = parsed.label {
        let name = to_spanned(label, file, line_tokens);
        program.items.push(Item::Label(LabelDef { name }));
    }

    let Some(tail) = parsed.tail else {
        return;
    };

    match tail {
        LineTail::Assign { name, expr } => {
            let name = to_spanned(name, file, line_tokens);
            let expr = to_spanned(expr, file, line_tokens);
            program.items.push(Item::Assign(Assign { name, expr }));
        }
        LineTail::Directive { name, args } => {
            let name = to_spanned(name, file, line_tokens);
            let converted_args = args
                .into_iter()
                .map(|arg| {
                    let arg = to_spanned(arg, file, line_tokens);
                    Spanned {
                        file: arg.file,
                        span: arg.span,
                        value: match arg.value {
                            ParsedDirArg::Expr(expr) => DirArg::Expr(expr),
                            ParsedDirArg::String(value) => DirArg::String(value),
                            ParsedDirArg::Ident(value) => DirArg::Ident(value),
                        },
                    }
                })
                .collect::<Vec<_>>();

            let span = if let Some(last) = converted_args.last() {
                name.span.start..last.span.end
            } else {
                name.span.clone()
            };

            program.items.push(Item::Directive(Directive {
                name,
                args: converted_args,
                span,
            }));
        }
        LineTail::Instruction { mnemonic, operand } => {
            let mnemonic = to_spanned(mnemonic, file, line_tokens);
            let operand = operand.map(|operand| {
                let operand = to_spanned(operand, file, line_tokens);
                Spanned {
                    file: operand.file,
                    span: operand.span,
                    value: match operand.value {
                        ParsedOperand::Imm(expr) => Operand::Imm(expr),
                        ParsedOperand::Expr(expr) => Operand::Expr(expr),
                        ParsedOperand::ExprX(expr) => Operand::ExprX(expr),
                        ParsedOperand::ExprY(expr) => Operand::ExprY(expr),
                        ParsedOperand::Ind(expr) => Operand::Ind(expr),
                        ParsedOperand::IndX(expr) => Operand::IndX(expr),
                        ParsedOperand::IndY(expr) => Operand::IndY(expr),
                    },
                }
            });

            let span = if let Some(operand) = &operand {
                mnemonic.span.start..operand.span.end
            } else {
                mnemonic.span.clone()
            };

            program.items.push(Item::Instruction(Instruction {
                mnemonic,
                operand,
                span,
            }));
        }
    }
}

fn push_parse_errors(
    errors: Vec<PError<'_>>,
    file: FileId,
    line_tokens: &[LineToken],
    diags: &mut Vec<Diag>,
) {
    for error in errors {
        let span = parser_span_to_byte_span(*error.span(), line_tokens);
        diags.push(
            Diag::error(file, span.clone(), "parse error").with_label(DiagLabel {
                file,
                span,
                message: format!("{error:?}"),
            }),
        );
    }
}

fn convert_line_tokens(raw_tokens: &[Token], diags: &mut Vec<Diag>) -> Vec<LineToken> {
    let mut out = Vec::with_capacity(raw_tokens.len());

    for token in raw_tokens {
        let kind = match token.kind {
            TokenKind::Ident => ParseToken::Ident(token.lexeme.clone()),
            TokenKind::Number => ParseToken::Number(token.lexeme.clone()),
            TokenKind::String => match unescape_string_literal(&token.lexeme) {
                Ok(value) => ParseToken::String(value),
                Err(message) => {
                    diags.push(Diag::error(token.file, token.span.clone(), message));
                    ParseToken::String(String::new())
                }
            },
            TokenKind::Colon => ParseToken::Colon,
            TokenKind::Comma => ParseToken::Comma,
            TokenKind::LParen => ParseToken::LParen,
            TokenKind::RParen => ParseToken::RParen,
            TokenKind::LBracket => ParseToken::LBracket,
            TokenKind::RBracket => ParseToken::RBracket,
            TokenKind::Hash => ParseToken::Hash,
            TokenKind::Plus => ParseToken::Plus,
            TokenKind::Minus => ParseToken::Minus,
            TokenKind::Star => ParseToken::Star,
            TokenKind::Slash => ParseToken::Slash,
            TokenKind::Amp => ParseToken::Amp,
            TokenKind::Pipe => ParseToken::Pipe,
            TokenKind::Caret => ParseToken::Caret,
            TokenKind::Shl => ParseToken::Shl,
            TokenKind::Shr => ParseToken::Shr,
            TokenKind::Lt => ParseToken::Lt,
            TokenKind::Gt => ParseToken::Gt,
            TokenKind::Eq => ParseToken::Eq,
            TokenKind::Dot => ParseToken::Dot,
            TokenKind::Newline => continue,
        };

        out.push(LineToken {
            kind,
            span: token.span.clone(),
        });
    }

    out
}

fn line_parser<'src>() -> impl Parser<'src, &'src [ParseToken], ParsedLine, PExtra<'src>> {
    let ident = select_ref! {
        ParseToken::Ident(name) => name.clone(),
    };

    let register_x = ident.clone().try_map(|name: String, span| {
        if name.eq_ignore_ascii_case("X") {
            Ok(())
        } else {
            Err(Rich::custom(span, "expected register `X`"))
        }
    });

    let register_y = ident.clone().try_map(|name: String, span| {
        if name.eq_ignore_ascii_case("Y") {
            Ok(())
        } else {
            Err(Rich::custom(span, "expected register `Y`"))
        }
    });

    let expr = expr_parser();
    let expr_node = expr
        .clone()
        .map_with(|value, extra| Node::new(value, extra.span()));

    let label = ident
        .clone()
        .map_with(|value, extra| Node::new(value, extra.span()))
        .then_ignore(just(ParseToken::Colon));

    let assign = ident
        .clone()
        .map_with(|value, extra| Node::new(value, extra.span()))
        .then_ignore(just(ParseToken::Eq))
        .then(expr_node.clone())
        .map(|(name, expr)| LineTail::Assign { name, expr });

    let dir_arg = choice((
        select_ref! { ParseToken::String(value) => ParsedDirArg::String(value.clone()) },
        expr.clone().map(|expr| match expr {
            Expr::Symbol(name) => ParsedDirArg::Ident(name),
            other => ParsedDirArg::Expr(other),
        }),
    ))
    .map_with(|value, extra| Node::new(value, extra.span()));

    let directive = just(ParseToken::Dot)
        .ignore_then(ident.clone())
        .map_with(|name, extra| Node::new(format!(".{}", name.to_ascii_lowercase()), extra.span()))
        .then(
            dir_arg
                .separated_by(just(ParseToken::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .or_not(),
        )
        .map(|(name, args)| LineTail::Directive {
            name,
            args: args.unwrap_or_default(),
        });

    let operand = choice((
        just(ParseToken::Hash)
            .ignore_then(expr.clone())
            .map(ParsedOperand::Imm),
        just(ParseToken::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(ParseToken::Comma))
            .then_ignore(register_x.clone())
            .then_ignore(just(ParseToken::RParen))
            .map(ParsedOperand::IndX),
        just(ParseToken::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(ParseToken::RParen))
            .then_ignore(just(ParseToken::Comma))
            .then_ignore(register_y.clone())
            .map(ParsedOperand::IndY),
        just(ParseToken::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(ParseToken::RParen))
            .map(ParsedOperand::Ind),
        expr.clone()
            .then_ignore(just(ParseToken::Comma))
            .then_ignore(register_x)
            .map(ParsedOperand::ExprX),
        expr.clone()
            .then_ignore(just(ParseToken::Comma))
            .then_ignore(register_y)
            .map(ParsedOperand::ExprY),
        expr.map(ParsedOperand::Expr),
    ))
    .map_with(|value, extra| Node::new(value, extra.span()));

    let instruction = ident
        .map_with(|name, extra| Node::new(name.to_ascii_uppercase(), extra.span()))
        .then(operand.or_not())
        .map(|(mnemonic, operand)| LineTail::Instruction { mnemonic, operand });

    label
        .or_not()
        .then(choice((assign, directive, instruction)).or_not())
        .then_ignore(end())
        .map(|(label, tail)| ParsedLine { label, tail })
}

fn expr_parser<'src>() -> impl Parser<'src, &'src [ParseToken], Expr, PExtra<'src>> + Clone {
    recursive(|expr| {
        let number = select_ref! {
            ParseToken::Number(raw) => raw.clone(),
        }
        .try_map(|raw, span| {
            parse_number_literal(&raw)
                .map(Expr::Number)
                .map_err(|message| Rich::custom(span, message))
        });

        let symbol = select_ref! {
            ParseToken::Ident(name) => Expr::Symbol(name.clone()),
        };

        let current_pc = just(ParseToken::Star).to(Expr::CurrentPc);

        let atom = choice((
            number,
            symbol,
            current_pc,
            expr.clone()
                .delimited_by(just(ParseToken::LParen), just(ParseToken::RParen)),
        ));

        let unary = choice((
            just(ParseToken::Plus).to(PrefixOp::Plus),
            just(ParseToken::Minus).to(PrefixOp::Minus),
            just(ParseToken::Lt).to(PrefixOp::Low),
            just(ParseToken::Gt).to(PrefixOp::High),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .then(atom)
        .map(|(ops, mut rhs)| {
            for op in ops.into_iter().rev() {
                rhs = match op {
                    PrefixOp::Plus => Expr::Unary {
                        op: UnOp::Plus,
                        rhs: Box::new(rhs),
                    },
                    PrefixOp::Minus => Expr::Unary {
                        op: UnOp::Minus,
                        rhs: Box::new(rhs),
                    },
                    PrefixOp::Low => Expr::LowByte(Box::new(rhs)),
                    PrefixOp::High => Expr::HighByte(Box::new(rhs)),
                };
            }
            rhs
        });

        let product = unary.clone().foldl(
            choice((
                just(ParseToken::Star).to(BinOp::Mul),
                just(ParseToken::Slash).to(BinOp::Div),
            ))
            .then(unary.clone())
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let sum = product.clone().foldl(
            choice((
                just(ParseToken::Plus).to(BinOp::Add),
                just(ParseToken::Minus).to(BinOp::Sub),
            ))
            .then(product.clone())
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let shifts = sum.clone().foldl(
            choice((
                just(ParseToken::Shl).to(BinOp::Shl),
                just(ParseToken::Shr).to(BinOp::Shr),
            ))
            .then(sum.clone())
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let ands = shifts.clone().foldl(
            just(ParseToken::Amp)
                .to(BinOp::And)
                .then(shifts.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let xors = ands.clone().foldl(
            just(ParseToken::Caret)
                .to(BinOp::Xor)
                .then(ands.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        xors.clone().foldl(
            just(ParseToken::Pipe).to(BinOp::Or).then(xors).repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
    })
}

fn parse_number_literal(raw: &str) -> Result<i64, String> {
    if let Some(hex) = raw.strip_prefix('$') {
        return i64::from_str_radix(hex, 16)
            .map_err(|_| format!("invalid hexadecimal literal `{raw}`"));
    }

    if let Some(binary) = raw.strip_prefix('%') {
        return i64::from_str_radix(binary, 2)
            .map_err(|_| format!("invalid binary literal `{raw}`"));
    }

    raw.parse::<i64>()
        .map_err(|_| format!("invalid decimal literal `{raw}`"))
}

fn to_spanned<T>(node: Node<T>, file: FileId, line_tokens: &[LineToken]) -> Spanned<T> {
    Spanned {
        file,
        span: parser_span_to_byte_span(node.span, line_tokens),
        value: node.value,
    }
}

fn parser_span_to_byte_span(span: SimpleSpan<usize>, line_tokens: &[LineToken]) -> Span {
    if line_tokens.is_empty() {
        return 0..0;
    }

    let len = line_tokens.len();
    let start = span.start.min(len);
    let end = span.end.min(len);

    if start < end {
        return line_tokens[start].span.start..line_tokens[end - 1].span.end;
    }

    if start < len {
        return line_tokens[start].span.clone();
    }

    let eof = line_tokens[len - 1].span.end;
    eof..eof
}

fn unescape_string_literal(input: &str) -> Result<String, String> {
    if input.len() < 2 || !input.starts_with('"') || !input.ends_with('"') {
        return Err("invalid string literal".to_string());
    }

    let mut out = String::new();
    let mut chars = input[1..input.len() - 1].chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }

        let escaped = chars
            .next()
            .ok_or_else(|| "unterminated escape".to_string())?;
        match escaped {
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            other => {
                return Err(format!("unsupported escape `\\{other}`"));
            }
        }
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::{lex::lex_file, source::SourceManager};

    use super::parse_tokens;

    #[test]
    fn parses_label_and_instruction() {
        let mut manager = SourceManager::new(Vec::new());
        let file = manager.add_virtual_file("test.s", "start: LDA #1\n");
        let (tokens, lex_diags) = lex_file(&manager, file);
        assert!(lex_diags.is_empty());

        let (program, parse_diags) = parse_tokens(&tokens);
        assert!(parse_diags.is_empty());
        assert_eq!(program.items.len(), 2);
    }
}
