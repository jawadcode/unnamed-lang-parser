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
    If {
        cond: Box<Expr>,
        true_value: Box<Expr>,
        false_value: Box<Expr>,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
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

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: Box<Expr>,
    },
    FnDef {
        name: String,
        params: Vec<String>,
        body: Box<Expr>,
    },
    ExprStmt {
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
            Expr::If {
                cond,
                true_value,
                false_value,
            } => write!(f, "(if {} {} {})", cond, true_value, false_value),
            Expr::Block { stmts, expr } => {
                write!(
                    f,
                    "(block {} {})",
                    stmts
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" "),
                    if let Some(ex) = expr {
                        format!("{}", ex)
                    } else {
                        "".to_string()
                    }
                )
            }
            Expr::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::InfixOp { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::PostfixOp { op, expr } => write!(f, "({} {})", op, expr),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let { name, value } => write!(f, "(let ({} {}))", name, value),
            Stmt::FnDef { name, params, body } => {
                write!(f, "(define {} ({}) {})", name, params.join(" "), body)
            }
            Stmt::ExprStmt { expr } => write!(f, "{}", expr),
        }
    }
}
