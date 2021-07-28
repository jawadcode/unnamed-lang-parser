use logos::Logos;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
/// Token kind used within the `Token` struct
pub enum TokenKind {
    Function,
    Let,
    If,
    Then,
    Else,
    Match,
    Begin,
    End,
    Ident,
    IntLit,
    FloatLit,
    StringLit,
    Plus,
    Minus,
    Multiply,
    Divide,
    Less,
    Greater,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    LessEq,
    GreaterEq,
    NotEq,
    Equals,
    LeftParen,
    RightParen,
    Assign,
    Colon,
    Pipe,
    FatArrow,
    Comma,
    Delimiter,
    Comment,
    Whitespace,
    Error,
    EOF,
}

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum LogosToken {
    #[token("function")]
    Function,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("match")]
    Match,

    #[token("begin")]
    Begin,

    #[token("end")]
    End,

    #[regex(r#"([A-Za-z]|_)([A-Za-z]|_|\d)*"#)]
    Ident,

    #[regex("[0-9]+", priority = 2)]
    IntLit,

    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    FloatLit,

    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    StringLit,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("!")]
    LogicalNot,

    #[token("&&")]
    LogicalAnd,

    #[token("||")]
    LogicalOr,

    #[token("<=")]
    LessEq,

    #[token(">=")]
    GreaterEq,

    #[token("!=")]
    NotEq,

    #[token("==")]
    Equals,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("=")]
    Assign,

    #[token(":")]
    Colon,

    #[token("|")]
    Pipe,

    #[token("=>")]
    FatArrow,

    #[token(",")]
    Comma,

    #[token(";")]
    Delimiter,

    #[regex(r"(?m)\(\*([^*]|\*+[^*)])*\*+\)")]
    Comment,

    #[regex(r"[ \t\r\n\f]+")]
    Whitespace,

    #[error]
    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Function => "function",
                TokenKind::Let => "let",
                TokenKind::If => "if",
                TokenKind::Then => "then",
                TokenKind::Else => "else",
                TokenKind::Match => "match",
                TokenKind::Begin => "begin",
                TokenKind::End => "end",
                TokenKind::Ident => "Identifier",
                TokenKind::IntLit => "Integer Literal",
                TokenKind::FloatLit => "Float Literal",
                TokenKind::StringLit => "String Literal",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Multiply => "*",
                TokenKind::Divide => "/",
                TokenKind::Less => "<",
                TokenKind::Greater => ">",
                TokenKind::LogicalNot => "!",
                TokenKind::LogicalAnd => "&&",
                TokenKind::LogicalOr => "||",
                TokenKind::LessEq => "<=",
                TokenKind::GreaterEq => ">=",
                TokenKind::NotEq => "!=",
                TokenKind::Equals => "==",
                TokenKind::LeftParen => "(",
                TokenKind::RightParen => ")",
                TokenKind::Assign => "=",
                TokenKind::Colon => ":",
                TokenKind::Pipe => "|",
                TokenKind::FatArrow => "=>",
                TokenKind::Comma => ",",
                TokenKind::Delimiter => ";",
                TokenKind::Comment => "Comment",
                TokenKind::Whitespace => "<WS>",
                TokenKind::Error => "<?>",
                _ => unreachable!(),
            }
        )
    }
}

impl LogosToken {
    pub fn kind(&self) -> TokenKind {
        match self {
            LogosToken::Function => TokenKind::Function,
            LogosToken::Let => TokenKind::Let,
            LogosToken::If => TokenKind::If,
            LogosToken::Then => TokenKind::Then,
            LogosToken::Else => TokenKind::Else,
            LogosToken::Match => TokenKind::Match,
            LogosToken::Begin => TokenKind::Begin,
            LogosToken::End => TokenKind::End,
            LogosToken::Ident => TokenKind::Ident,
            LogosToken::IntLit => TokenKind::IntLit,
            LogosToken::FloatLit => TokenKind::FloatLit,
            LogosToken::StringLit => TokenKind::StringLit,
            LogosToken::Plus => TokenKind::Plus,
            LogosToken::Minus => TokenKind::Minus,
            LogosToken::Multiply => TokenKind::Multiply,
            LogosToken::Divide => TokenKind::Divide,
            LogosToken::Less => TokenKind::Less,
            LogosToken::Greater => TokenKind::Greater,
            LogosToken::LogicalNot => TokenKind::LogicalNot,
            LogosToken::LogicalAnd => TokenKind::LogicalAnd,
            LogosToken::LogicalOr => TokenKind::LogicalOr,
            LogosToken::LessEq => TokenKind::LessEq,
            LogosToken::GreaterEq => TokenKind::GreaterEq,
            LogosToken::NotEq => TokenKind::NotEq,
            LogosToken::Equals => TokenKind::Equals,
            LogosToken::LeftParen => TokenKind::LeftParen,
            LogosToken::RightParen => TokenKind::RightParen,
            LogosToken::Assign => TokenKind::Assign,
            LogosToken::Colon => TokenKind::Colon,
            LogosToken::Pipe => TokenKind::Pipe,
            LogosToken::FatArrow => TokenKind::FatArrow,
            LogosToken::Comma => TokenKind::Comma,
            LogosToken::Delimiter => TokenKind::Delimiter,
            LogosToken::Comment => TokenKind::Comment,
            LogosToken::Error => TokenKind::Error,
            LogosToken::Whitespace => TokenKind::Whitespace,
        }
    }
}
