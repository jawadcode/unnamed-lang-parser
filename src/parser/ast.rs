use std::fmt;

use crate::lexer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(usize),
    Float(f64),
    String(String),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::String(s) => write!(f, r#""{}""#, s),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    PrefixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    InfixOp {
        op: TokenKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::FnCall { name, args } => {
                write!(
                    f,
                    "({} {})",
                    name,
                    args.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Expr::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::InfixOp { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::PostfixOp { op, expr } => write!(f, "({} {})", op, expr),
        }
    }
}
