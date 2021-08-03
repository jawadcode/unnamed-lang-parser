use std::fmt;

use crate::lexer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Bool(bool),
    Int(usize),
    Float(f64),
    String(String),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Bool(b) => write!(f, "{}", b),
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
    Match {
        expr: Box<Expr>,
        arms: Vec<(Vec<Expr>, Expr)>,
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
            } => write!(
                f,
                "(if :cond {} :then {} :else {})",
                cond, true_value, false_value
            ),
            Expr::Match { expr, arms } => {
                write!(
                    f,
                    "(match {} {})",
                    expr,
                    arms.iter()
                        .map(|arm| format!(
                            "({} {})",
                            if arm.0.len() > 1 {
                                format!(
                                    "({})",
                                    arm.0
                                        .iter()
                                        .map(ToString::to_string)
                                        .collect::<Vec<_>>()
                                        .join(" ")
                                )
                            } else {
                                arm.0[0].to_string()
                            },
                            arm.1
                        ))
                        .collect::<Vec<_>>()
                        .join(" "),
                )
            }
            Expr::Block { stmts, expr } => {
                write!(
                    f,
                    "(block{}{}{})",
                    if stmts.len() > 0 { " " } else { "" },
                    stmts
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" "),
                    if let Some(ex) = expr {
                        format!(" {}", ex)
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
