use crate::lexer::{Token, TokenKind};

use super::{ast, Parser};

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_statement(&mut self) -> Option<ast::Stmt> {
        Some(match self.peek() {
            TokenKind::Let => self.parse_let()?,
            TokenKind::Function => self.parse_fndef()?,
            _ => return None,
        })
    }

    fn parse_let(&mut self) -> Option<ast::Stmt> {
        self.consume(TokenKind::Let)?;
        let ident = self.next()?;
        let name = self.text(ident).to_string();
        self.consume(TokenKind::Assign)?;
        let value = Box::new(self.expression()?);
        self.consume(TokenKind::Delimiter)?;
        Some(ast::Stmt::Let { name, value })
    }

    fn parse_fndef(&mut self) -> Option<ast::Stmt> {
        self.consume(TokenKind::Function)?;
        let ident = self.next()?;
        let name = self.text(ident).to_string();

        let mut params = Vec::new();
        if self.at(TokenKind::Colon) {
            while self.at(TokenKind::Ident) {
                let param_token = self.next()?;
                let param = self.text(param_token).to_string();
                params.push(param);
            }
        }

        self.consume(TokenKind::Assign)?;
        let body = Box::new(self.expression()?);
        Some(ast::Stmt::FnDef { name, params, body })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_stmt {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            let stmt = Parser::new(test).parse_statement().unwrap();
            println!(
                "Sample: \"{}\":\nGot:    {}\nWanted: {}",
                $sample, stmt, $sexpr
            );
            assert_eq!(format!("{}", stmt), $sexpr)
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
        assert_stmt!("function add: x y = x + y", "(define add (x y) (+ x y)");
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
