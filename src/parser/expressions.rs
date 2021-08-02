use crate::{
    lexer::{Token, TokenKind},
    parser::ErrorInfo,
};

use super::{ast, ExprResult, Parser, SyntaxError};

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
                // unreachable!("Not a prefix operator: {:?}")
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
        // Keep this here incase we need it and also for the sake of completeness
        // Some(match self {
        //     // TokenKind::Propagate => (101, ()),
        // })
        return None;
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expression(&mut self, binding_power: u8) -> ExprResult {
        let mut lhs = match self.peek() {
            lit @ TokenKind::IntLit | lit @ TokenKind::FloatLit | lit @ TokenKind::StringLit => {
                self.parse_literal(lit)?
            }

            TokenKind::Ident => self.parse_identifier()?,

            TokenKind::If => self.parse_if_expr()?,

            TokenKind::Match => self.parse_match_expr()?,

            TokenKind::Begin => self.parse_block_expr()?,

            TokenKind::LeftParen => self.parse_grouping()?,

            op @ TokenKind::Minus | op @ TokenKind::LogicalNot => self.parse_prefix_op(op)?,

            kind => {
                let token = self.next().unwrap();
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    token_kind: kind,
                    info: ErrorInfo::new(token.span, self.input),
                });
            }
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
                TokenKind::EOF
                | TokenKind::RightParen
                | TokenKind::Delimiter
                | TokenKind::Comma
                | TokenKind::Pipe
                | TokenKind::FatArrow
                | TokenKind::Then
                | TokenKind::Else
                | TokenKind::End => break,
                kind => {
                    let token = self.next().unwrap();
                    return Err(SyntaxError::UnexpectedToken {
                        expected: r#"operator or expression terminator"#.to_string(),
                        token_kind: kind,
                        info: ErrorInfo::new(token.span, self.input),
                    });
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op)?;
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

                self.consume(op)?;
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

        Ok(lhs)
    }

    /// Parse string, integer or float literal
    fn parse_literal(&mut self, lit: TokenKind) -> ExprResult {
        // Consume the literal and get the text for it
        let literal_token = self.next().unwrap();
        let literal_text = self.text(literal_token);

        let lit = match lit {
            TokenKind::IntLit => ast::Lit::Int(
                literal_text
                    // Laziness at its peak:
                    .parse()
                    .map_err(|_| SyntaxError::FailedToParseLiteral {
                        token_kind: literal_token.kind,
                        info: ErrorInfo::new(literal_token.span, self.input),
                    })?,
            ),
            TokenKind::FloatLit => ast::Lit::Float(literal_text.parse().map_err(|_| {
                SyntaxError::FailedToParseLiteral {
                    token_kind: literal_token.kind,
                    info: ErrorInfo::new(literal_token.span, self.input),
                }
            })?),
            TokenKind::StringLit => ast::Lit::String(
                // Trim the quotes
                literal_text[1..(literal_text.len() - 1)].to_string(),
            ),
            _ => unreachable!(),
        };

        Ok(ast::Expr::Literal(lit))
    }

    /// Parse function call arguments parsing function arguments as expressions
    fn parse_function_call(&mut self, name: String) -> ExprResult {
        let mut args = Vec::new();
        while !(self.at(TokenKind::RightParen)
            || self.at(TokenKind::Delimiter)
            || self.at(TokenKind::Comma)
            || self.at(TokenKind::Pipe)
            || self.at(TokenKind::FatArrow)
            || self.at(TokenKind::Then)
            || self.at(TokenKind::Else)
            || self.at(TokenKind::End)
            || self.at(TokenKind::EOF))
        {
            args.push(match self.peek() {
                TokenKind::LeftParen => {
                    self.consume(TokenKind::LeftParen)?;
                    let expr = self.expression()?;
                    self.consume(TokenKind::RightParen)?;
                    expr
                }
                TokenKind::Ident => {
                    let ident_token = self.next().unwrap();
                    ast::Expr::Ident(self.text(ident_token).to_string())
                }
                lit @ TokenKind::StringLit
                | lit @ TokenKind::IntLit
                | lit @ TokenKind::FloatLit => self.parse_literal(lit)?,
                _ => unreachable!(),
            });
        }

        Ok(ast::Expr::FnCall { name, args })
    }

    /// Parse identifier or function call
    fn parse_identifier(&mut self) -> ExprResult {
        let name = {
            let ident_token = self.next().unwrap();
            self.text(ident_token).to_string()
        };

        Ok(
            // If any of the following appear as the next token
            // then it must be a function call with a function argument following it
            match self.peek() {
                TokenKind::LeftParen
                | TokenKind::Ident
                | TokenKind::IntLit
                | TokenKind::FloatLit
                | TokenKind::StringLit => self.parse_function_call(name)?,
                _ => ast::Expr::Ident(name),
            },
        )
    }

    /// Parse if(+else) expression
    fn parse_if_expr(&mut self) -> ExprResult {
        self.consume(TokenKind::If)?;
        let cond = Box::new(self.expression()?);
        self.consume(TokenKind::Then)?;
        let true_value = Box::new(self.expression()?);
        self.consume(TokenKind::Else)?;
        let false_value = Box::new(self.expression()?);
        Ok(ast::Expr::If {
            cond,
            true_value,
            false_value,
        })
    }

    /// Parse match expression
    fn parse_match_expr(&mut self) -> ExprResult {
        self.consume(TokenKind::Match)?;
        let expr = Box::new(self.expression()?);

        let mut arms: Vec<(Vec<ast::Expr>, ast::Expr)> = vec![];
        while self.at(TokenKind::Pipe) {
            self.consume(TokenKind::Pipe)?;
            let mut patterns = vec![self.expression()?];
            while self.at(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
                patterns.push(self.expression()?);
            }
            self.consume(TokenKind::FatArrow)?;
            let value = self.expression()?;
            arms.push((patterns, value));
        }

        self.consume(TokenKind::End)?;
        Ok(ast::Expr::Match { expr, arms })
    }

    /// Parse block expression
    pub(crate) fn parse_block_expr(&mut self) -> ExprResult {
        self.consume(TokenKind::Begin)?;

        let mut stmts = Vec::new();
        while self.is_statement() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        let expr = self.expression().map(Box::new).ok();

        self.consume(TokenKind::End)?;
        Ok(ast::Expr::Block { stmts, expr })
    }

    /// No special AST node, just influences the structure by evaluating the expression within the parens first
    fn parse_grouping(&mut self) -> ExprResult {
        self.consume(TokenKind::LeftParen)?;
        let expr = self.parse_expression(0);
        self.consume(TokenKind::RightParen)?;
        expr
    }

    /// Parse prefix operation
    fn parse_prefix_op(&mut self, op: TokenKind) -> ExprResult {
        let token = self.next().unwrap();

        let ((), right_binding_power) =
            op.prefix_binding_power()
                .ok_or(SyntaxError::UnexpectedToken {
                    expected: r#""-" or "!""#.to_string(),
                    token_kind: op,
                    info: ErrorInfo::new(token.span, self.input),
                })?;
        let expr = Box::new(self.parse_expression(right_binding_power)?);
        Ok(ast::Expr::PrefixOp { op, expr })
    }

    /// Parse expression
    pub fn expression(&mut self) -> ExprResult {
        self.parse_expression(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_expr {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            println!("Sample: '{}'", $sample);
            match Parser::new(test).expression() {
                Ok(expr) => {
                    println!(
                        "Sample: \"{}\"\nGot:    {}\nWanted: {}",
                        $sample, expr, $sexpr
                    );
                    assert_eq!(expr.to_string(), $sexpr)
                }
                Err(err) => {
                    eprintln!("{:#?}", err);
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_arithmetic_comparison_operators() {
        assert_expr!(
            "10 + -9 * 0 - 90 / -90 != 69 / -4",
            "(!= (- (+ 10 (* (- 9) 0)) (/ 90 (- 90))) (/ 69 (- 4)))"
        );
    }

    #[test]
    fn parse_identifier() {
        assert_expr!("hello", "hello");
    }

    #[test]
    fn parse_simple_function_call() {
        assert_expr!("add 1 (69 + 420) 2;", "(add 1 (+ 69 420) 2)");
    }

    #[test]
    fn parse_nested_function_call() {
        assert_expr!(
            "add (69 + 420) (add 57893 43280);",
            "(add (+ 69 420) (add 57893 43280))"
        );
    }

    #[test]
    fn parse_if_expr() {
        assert_expr!(
            "if 10 > 89 + 90 then 1 + 1 else -1",
            "(if :cond (> 10 (+ 89 90)) :then (+ 1 1) :else (- 1))"
        );
    }

    #[test]
    fn parse_match_expr() {
        assert_expr!(
            r#"
match x
  | 1    => 69
  | 2, 3 => 420
  | _    => x * x * x
end"#,
            "(match x (1 69) ((2 3) 420) (_ (* (* x x) x)))"
        );
    }

    #[test]
    fn parse_block_expr() {
        assert_expr!(
            r#"
begin
  let thing = 123;
  let thing2 = 234;
  thing + thing2
end"#,
            "(block (let (thing 123)) (let (thing2 234)) (+ thing thing2))"
        );
    }
}
