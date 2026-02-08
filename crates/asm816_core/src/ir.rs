use crate::{
    expr::Expr,
    source::{Span, Spanned},
};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub enum Item {
    Label(LabelDef),
    Assign(Assign),
    Directive(Directive),
    Instruction(Instruction),
    EmptyLine(Spanned<()>),
}

#[derive(Clone, Debug)]
pub struct LabelDef {
    pub name: Spanned<String>,
}

#[derive(Clone, Debug)]
pub struct Assign {
    pub name: Spanned<String>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct Directive {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<DirArg>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum DirArg {
    Expr(Expr),
    String(String),
    Ident(String),
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub mnemonic: Spanned<String>,
    pub operand: Option<Spanned<Operand>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(Expr),
    Acc,
    Expr(Expr),
    ExprX(Expr),
    ExprY(Expr),
    Ind(Expr),
    IndX(Expr),
    IndY(Expr),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Label(label) => label.name.span.clone(),
            Item::Assign(assign) => assign.name.span.start..assign.expr.span.end,
            Item::Directive(directive) => directive.span.clone(),
            Item::Instruction(instruction) => instruction.span.clone(),
            Item::EmptyLine(empty) => empty.span.clone(),
        }
    }
}
