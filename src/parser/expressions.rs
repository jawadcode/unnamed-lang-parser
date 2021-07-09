use crate::lexer::{Token, TokenKind};

use super::{ast, Parser};

trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> ((), u8);

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            TokenKind::Minus => ((), 51),
            TokenKind::LogicalNot => ((), 101),
            _ => unreachable!("Not a prefix operator: {:?}"),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
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
        };
        Some(result)
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        let result = match self {
            // TokenKind::Propagate => (101, ()),
            _ => return None,
        };
        Some(result)
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
        let mut lhs = match self.peek() {
            // Parse int, float and string literals
            lit @ TokenKind::IntLit | lit @ TokenKind::FloatLit | lit @ TokenKind::StringLit => {
                self.parse_literal(lit)
            }

            TokenKind::Ident => {
                let name = {
                    let ident_token = self.next().unwrap();
                    self.text(ident_token).to_string()
                };
                if !self.at(TokenKind::LeftParen) {
                    // Plain identifier
                    ast::Expr::Ident(name)
                } else {
                    // Function call
                    let mut args = Vec::new();
                    // Consoom parens and function arguments
                    while !self.at(TokenKind::Delimiter) {
                        let arg = self.parse_expression(0);
                        args.push(arg);
                    }

                    ast::Expr::FnCall { name, args }
                }
            }

            // Parse grouping
            TokenKind::LeftParen => {
                // No special AST node, just influences the structure by evaluating the expression
                // within the parens first
                self.consume(TokenKind::LeftParen);
                let expr = self.parse_expression(0);
                self.consume(TokenKind::RightParen);
                expr
            }

            // Parse prefix operator + expression (`op` holds the matched `TokenType`)
            op @ TokenKind::Minus | op @ TokenKind::LogicalNot => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                let expr = self.parse_expression(right_binding_power);
                ast::Expr::PrefixOp {
                    op,
                    expr: Box::new(expr),
                }
            }
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
                kind => panic!("Unknown operator: `{}`", kind),
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
                let rhs = self.parse_expression(right_binding_power);
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

        lhs
    }

    #[inline(always)]
    fn parse_literal(&mut self, lit: TokenKind) -> ast::Expr {
        let literal_text = {
            // Consume the literal and get the text for it
            let literal_token = self.next().unwrap();
            self.text(literal_token)
        };
        let lit = match lit {
            TokenKind::IntLit => ast::Lit::Int(
                literal_text
                    // Laziness at its peak:
                    .parse()
                    .expect(&format!("Invalid integer literal '{}'", literal_text)),
            ),
            TokenKind::FloatLit => ast::Lit::Float(
                literal_text
                    .parse()
                    .expect(&format!("Invalid float literal '{}'", literal_text)),
            ),
            TokenKind::StringLit => ast::Lit::String(
                // Trim the quotes
                literal_text[1..(literal_text.len() - 1)].to_string(),
            ),
            _ => unreachable!(),
        };

        ast::Expr::Literal(lit)
    }

    pub fn expression(&mut self) -> ast::Expr {
        self.parse_expression(0)
    }
}

#[test]
fn parse_expr() {
    let test = "10 + -9 * 0 - 90 / -90 != 69 / -4";
    let expr = Parser::new(test).expression();
    println!("Test: \"{}\":\n{}", test, expr);
}
