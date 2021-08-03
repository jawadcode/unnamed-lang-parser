use crate::lexer::{Token, TokenKind};

use super::{ast, ErrorInfo, Parser, StmtResult, SyntaxError};

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_statement(&mut self) -> StmtResult {
        Ok(match self.peek() {
            TokenKind::Let => self.parse_let()?,
            TokenKind::Function => self.parse_fndef()?,
            kind => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: r#""let" or "function""#.to_string(),
                    token_kind: kind,
                    info: ErrorInfo::new(self.next().unwrap().span, self.input),
                });
            }
        })
    }

    #[inline(always)]
    pub fn is_statement(&mut self) -> bool {
        matches!(self.peek(), TokenKind::Let | TokenKind::Function)
    }

    fn parse_let(&mut self) -> StmtResult {
        self.consume(TokenKind::Let)?;
        let ident = self.next().unwrap();
        let name = self.text(ident).to_string();
        self.consume(TokenKind::Assign)?;
        let value = Box::new(self.expression()?);
        self.consume(TokenKind::Delimiter)?;
        Ok(ast::Stmt::Let { name, value })
    }

    fn parse_fndef(&mut self) -> StmtResult {
        self.consume(TokenKind::Function)?;
        let ident = self.next().unwrap();
        let name = self.text(ident).to_string();

        let mut params = Vec::new();
        if self.at(TokenKind::Colon) {
            self.consume(TokenKind::Colon)?;
            while self.at(TokenKind::Ident) {
                let param_token = self.next().unwrap();
                let param = self.text(param_token).to_string();
                params.push(param);
            }
        }

        self.consume(TokenKind::Assign)?;
        let body = Box::new(self.expression()?);
        Ok(ast::Stmt::FnDef { name, params, body })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_stmt {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            match Parser::new(test).parse_statement() {
                Ok(stmt) => {
                    println!(
                        "Sample: \"{}\"\nGot:    {}\nWanted: {}",
                        $sample, $sexpr, stmt
                    );
                    assert_eq!(stmt.to_string(), $sexpr)
                }
                Err(err) => {
                    eprintln!("{:#?}", err);
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_let() {
        assert_stmt!(
            "let thing = (57489 + 423) * 8989 - 9;",
            "(let (thing (- (* (+ 57489 423) 8989) 9)))"
        );
    }

    #[test]
    fn parse_basic_fndef() {
        assert_stmt!("function add: x y = x + y", "(define add (x y) (+ x y))");
    }

    #[test]
    fn parse_basic_fndef_no_params() {
        assert_stmt!(
            r#"function yes = print "yes""#,
            r#"(define yes () (print "yes"))"#
        );
    }

    #[test]
    fn parse_fndef_with_block() {
        assert_stmt!(
            r#"
function add: x y = begin
    let thing1 = x + 1;
    let thing2 = y + 2;
    thing1 + thing2
end"#,
            "(define add (x y) (block (let (thing1 (+ x 1))) (let (thing2 (+ y 2))) (+ thing1 thing2)))"
        );
    }
}
