use chumsky::{IterParser, error::Rich, extra, prelude::*, text};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Number(i64),
    Symbol(String),
    CurrentPc,
    Unary {
        op: UnOp,
        rhs: Box<Expr>,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    LowByte(Box<Expr>),
    HighByte(Box<Expr>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Plus,
    Minus,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Clone, Copy)]
enum PrefixOp {
    Plus,
    Minus,
    Low,
    High,
}

type ExprError<'src> = Rich<'src, char>;
type ExprExtra<'src> = extra::Err<ExprError<'src>>;

pub fn parse_expr(input: &str) -> Result<Expr, String> {
    expr_parser()
        .then_ignore(end())
        .parse(input)
        .into_result()
        .map_err(|errs| format_parser_errors(errs, input))
}

pub fn eval_expr(
    expr: &Expr,
    lookup: &dyn Fn(&str) -> Option<i64>,
    current_pc: i64,
) -> Result<i64, String> {
    match expr {
        Expr::Number(v) => Ok(*v),
        Expr::Symbol(name) => lookup(name).ok_or_else(|| format!("undefined symbol `{name}`")),
        Expr::CurrentPc => Ok(current_pc),
        Expr::Unary { op, rhs } => {
            let value = eval_expr(rhs, lookup, current_pc)?;
            match op {
                UnOp::Plus => Ok(value),
                UnOp::Minus => Ok(-value),
            }
        }
        Expr::Binary { op, lhs, rhs } => {
            let left = eval_expr(lhs, lookup, current_pc)?;
            let right = eval_expr(rhs, lookup, current_pc)?;
            match op {
                BinOp::Add => Ok(left + right),
                BinOp::Sub => Ok(left - right),
                BinOp::Mul => Ok(left * right),
                BinOp::Div => {
                    if right == 0 {
                        Err("division by zero".to_string())
                    } else {
                        Ok(left / right)
                    }
                }
                BinOp::And => Ok(left & right),
                BinOp::Or => Ok(left | right),
                BinOp::Xor => Ok(left ^ right),
                BinOp::Shl => Ok(left << right),
                BinOp::Shr => Ok(left >> right),
            }
        }
        Expr::LowByte(inner) => Ok(eval_expr(inner, lookup, current_pc)? & 0xFF),
        Expr::HighByte(inner) => Ok((eval_expr(inner, lookup, current_pc)? >> 8) & 0xFF),
    }
}

pub fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Number(value) => value.to_string(),
        Expr::Symbol(name) => name.clone(),
        Expr::CurrentPc => "*".to_string(),
        Expr::Unary { op, rhs } => {
            let op = match op {
                UnOp::Plus => "+",
                UnOp::Minus => "-",
            };
            format!("{op}{}", format_expr(rhs))
        }
        Expr::Binary { op, lhs, rhs } => {
            let op = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::And => "&",
                BinOp::Or => "|",
                BinOp::Xor => "^",
                BinOp::Shl => "<<",
                BinOp::Shr => ">>",
            };
            format!("({} {op} {})", format_expr(lhs), format_expr(rhs))
        }
        Expr::LowByte(inner) => format!("<{}", format_expr(inner)),
        Expr::HighByte(inner) => format!(">{}", format_expr(inner)),
    }
}

fn expr_parser<'src>() -> impl Parser<'src, &'src str, Expr, ExprExtra<'src>> {
    recursive(|expr| {
        let number = choice((
            just('$')
                .ignore_then(text::digits(16).to_slice())
                .try_map(|raw: &str, span| {
                    i64::from_str_radix(raw, 16)
                        .map(Expr::Number)
                        .map_err(|_| Rich::custom(span, "invalid hexadecimal literal"))
                }),
            just('%')
                .ignore_then(one_of("01").repeated().at_least(1).collect::<String>())
                .try_map(|raw: String, span| {
                    i64::from_str_radix(&raw, 2)
                        .map(Expr::Number)
                        .map_err(|_| Rich::custom(span, "invalid binary literal"))
                }),
            text::int(10).try_map(|raw: &str, span| {
                raw.parse::<i64>()
                    .map(Expr::Number)
                    .map_err(|_| Rich::custom(span, "invalid decimal literal"))
            }),
        ))
        .padded();

        let symbol = text::ident()
            .map(|name: &str| Expr::Symbol(name.to_string()))
            .padded();
        let current_pc = just('*').to(Expr::CurrentPc).padded();

        let atom = choice((
            number,
            symbol,
            current_pc,
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        ));

        let unary = choice((
            just('+').to(PrefixOp::Plus),
            just('-').to(PrefixOp::Minus),
            just('<').to(PrefixOp::Low),
            just('>').to(PrefixOp::High),
        ))
        .padded()
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
            choice((just('*').to(BinOp::Mul), just('/').to(BinOp::Div)))
                .padded()
                .then(unary.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let sum = product.clone().foldl(
            choice((just('+').to(BinOp::Add), just('-').to(BinOp::Sub)))
                .padded()
                .then(product.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let shifts = sum.clone().foldl(
            choice((just("<<").to(BinOp::Shl), just(">>").to(BinOp::Shr)))
                .padded()
                .then(sum.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let ands = shifts.clone().foldl(
            just('&')
                .to(BinOp::And)
                .padded()
                .then(shifts.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        let xors = ands.clone().foldl(
            just('^')
                .to(BinOp::Xor)
                .padded()
                .then(ands.clone())
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        xors.clone().foldl(
            just('|').to(BinOp::Or).padded().then(xors).repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
    })
}

fn format_parser_errors(errs: Vec<ExprError<'_>>, input: &str) -> String {
    errs.into_iter()
        .map(|err| {
            let span = err.span();
            let where_at = format!("{}..{}", span.start, span.end);
            format!("{} at {where_at} in `{input}`", err.reason())
        })
        .collect::<Vec<_>>()
        .join("; ")
}

#[cfg(test)]
mod tests {
    use super::{eval_expr, parse_expr};

    #[test]
    fn parses_precedence() {
        let expr = parse_expr("1 + 2 * 3").expect("parse failed");
        let value = eval_expr(&expr, &|_| None, 0).expect("eval failed");
        assert_eq!(value, 7);
    }
}
