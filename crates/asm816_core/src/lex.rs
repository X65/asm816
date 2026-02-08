use logos::Logos;

use crate::{
    diag::Diag,
    source::{FileId, SourceManager, Span},
};

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(skip r"[ \t\f]+")]
#[logos(skip(r";[^\n]*", allow_greedy = true))]
pub enum TokenKind {
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    #[regex(r"\$[0-9A-Fa-f]+|%[01]+|[0-9]+")]
    Number,

    #[regex(r#"\"([^\"\\]|\\.)*\""#)]
    String,

    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("#")]
    Hash,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("=")]
    Eq,
    #[token(".")]
    Dot,

    #[regex(r"\n")]
    Newline,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub file: FileId,
    pub span: Span,
    pub lexeme: String,
}

pub fn lex_file(source_manager: &SourceManager, file: FileId) -> (Vec<Token>, Vec<Diag>) {
    let source = &source_manager.file(file).text;
    let mut tokens = Vec::new();
    let mut diags = Vec::new();

    for (kind, span) in TokenKind::lexer(source).spanned() {
        match kind {
            Ok(kind) => {
                tokens.push(Token {
                    kind,
                    file,
                    span: span.start..span.end,
                    lexeme: source[span.clone()].to_string(),
                });
            }
            Err(_) => {
                diags.push(
                    Diag::error(file, span.start..span.end, "invalid token")
                        .with_help("remove or replace this character"),
                );
            }
        }
    }

    (tokens, diags)
}

#[cfg(test)]
mod tests {
    use super::{TokenKind, lex_file};
    use crate::source::SourceManager;

    #[test]
    fn lexes_basic_tokens() {
        let mut manager = SourceManager::new(Vec::new());
        let file = manager.add_virtual_file("test.s", "label: LDA #$10\n");
        let (tokens, diags) = lex_file(&manager, file);

        assert!(diags.is_empty());
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ident,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Hash,
                TokenKind::Number,
                TokenKind::Newline,
            ]
        );
    }
}
