use crate::lexer::{Token, TokenKind};

use super::{ast, Parser};

trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Minus => ((), 51),
            TokenKind::LogicalNot => ((), 101),
            _ => {
                return None;
                // unreachable!("Not a prefix operator: {:?}")#
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            TokenKind::LogicalOr => (1, 2),
            TokenKind::LogicalAnd => (3, 4),
            TokenKind::Equals | TokenKind::NotEq => (5, 6),
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEq | TokenKind::GreaterEq => {
                (7, 8)
            }
            TokenKind::Plus | TokenKind::Minus => (9, 10),
            TokenKind::Multiply | TokenKind::Divide => (11, 12),
            // TokenKind::Exponent => (22, 21),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        Some(match self {
            // TokenKind::Propagate => (101, ()),
            _ => return None,
        })
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expression(&mut self, binding_power: u8) -> Option<ast::Expr> {
        let mut lhs = match self.peek() {
            // Parse int, float and string literals
            lit @ TokenKind::IntLit | lit @ TokenKind::FloatLit | lit @ TokenKind::StringLit => {
                self.parse_literal(lit)?
            }

            // Parse identifier or function invocation
            TokenKind::Ident => self.parse_identifier()?,

            // Parse grouping
            TokenKind::LeftParen => self.parse_grouping()?,

            // Parse prefix operator + expression (`op` holds the matched `TokenType`)
            op @ TokenKind::Minus | op @ TokenKind::LogicalNot => self.parse_prefix_op(op)?,
            _ => todo!(),
        };

        loop {
            let op = match self.peek() {
                op @ TokenKind::Plus
                | op @ TokenKind::Minus
                | op @ TokenKind::Multiply
                | op @ TokenKind::Divide
                | op @ TokenKind::LogicalAnd
                | op @ TokenKind::LogicalOr
                | op @ TokenKind::Less
                | op @ TokenKind::Greater
                | op @ TokenKind::LogicalNot
                | op @ TokenKind::LessEq
                | op @ TokenKind::GreaterEq
                | op @ TokenKind::NotEq
                | op @ TokenKind::Equals => op,
                TokenKind::EOF => break,
                TokenKind::RightParen | TokenKind::Delimiter => break,
                kind => {
                    // panic!("Unknown operator: `{}`", kind)
                    return None;
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op);
                // no recursive call here, because we have already
                // parsed our operand `lhs`
                lhs = ast::Expr::PostfixOp {
                    op,
                    expr: Box::new(lhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op);
                let rhs = self.parse_expression(right_binding_power)?;
                lhs = ast::Expr::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            }
            // Not an operator --> end of expression
            break;
        }

        Some(lhs)
    }

    #[inline(always)]
    fn parse_literal(&mut self, lit: TokenKind) -> Option<ast::Expr> {
        let literal_text = {
            // Consume the literal and get the text for it
            let literal_token = self.next()?;
            self.text(literal_token)
        };
        let lit = match lit {
            TokenKind::IntLit => ast::Lit::Int(
                literal_text
                    // Laziness at its peak:
                    .parse()
                    .ok()?,
            ),
            TokenKind::FloatLit => ast::Lit::Float(literal_text.parse().ok()?),
            TokenKind::StringLit => ast::Lit::String(
                // Trim the quotes
                literal_text[1..(literal_text.len() - 1)].to_string(),
            ),
            _ => unreachable!(),
        };

        Some(ast::Expr::Literal(lit))
    }

    fn parse_function_call(&mut self, name: String) -> Option<ast::Expr> {
        let mut args = Vec::new();
        while !self.at(TokenKind::Delimiter)? {
            args.push(if self.peek() == TokenKind::LeftParen {
                self.consume(TokenKind::LeftParen)?;
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen)?;
                expr
            } else {
                self.parse_identifier()?
            });
        }

        Some(ast::Expr::FnCall { name, args })
    }

    #[inline(always)]
    fn parse_identifier(&mut self) -> Option<ast::Expr> {
        let name = {
            let ident_token = self.next()?;
            self.text(ident_token).to_string()
        };

        Some(match self.peek() {
            TokenKind::LeftParen
            | TokenKind::Ident
            | TokenKind::IntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit => self.parse_function_call(name)?,
            _ => ast::Expr::Ident(name),
        })
    }

    #[inline(always)]
    fn parse_grouping(&mut self) -> Option<ast::Expr> {
        // No special AST node, just influences the structure by evaluating the expression
        // within the parens first
        self.consume(TokenKind::LeftParen);
        let expr = self.parse_expression(0);
        self.consume(TokenKind::RightParen);
        expr
    }

    #[inline(always)]
    fn parse_prefix_op(&mut self, op: TokenKind) -> Option<ast::Expr> {
        self.consume(op);
        let ((), right_binding_power) = op.prefix_binding_power()?;
        let expr = self.parse_expression(right_binding_power)?;
        Some(ast::Expr::PrefixOp {
            op,
            expr: Box::new(expr),
        })
    }

    pub fn expression(&mut self) -> Option<ast::Expr> {
        self.parse_expression(0)
    }
}

#[test]
fn parse_expr() {
    let test = "10 + -9 * 0 - 90 / -90 != 69 / -4";
    let expr = Parser::new(test).expression().unwrap();
    println!("Test: \"{}\":\n{}", test, expr);

    let test = "hello";
    let expr = Parser::new(test).expression().unwrap();
    println!("Test: \"{}\":\n{}", test, expr);

    let test = r#"say "hello there" "good fellow";"#;
    let expr = Parser::new(test).expression().unwrap();
    println!("Test: \"{}\":\n{}", test, expr);

    let test = "add 1 2 (69 + 420);";
    let expr = Parser::new(test).expression().unwrap();
    println!("Test: \"{}\":\n{}", test, expr);
}
