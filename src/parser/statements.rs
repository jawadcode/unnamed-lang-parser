use crate::lexer::{Token, TokenKind};

use super::{ast, Parser};

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn statement(&mut self) -> Option<ast::Stmt> {
        Some(match self.peek() {
            TokenKind::Let => {
                self.consume(TokenKind::Let)?;
                let ident = self.next()?;
                let name = self.text(ident).to_string();
                self.consume(TokenKind::Assign)?;
                let value = self.expression()?;
                self.consume(TokenKind::Delimiter)?;
                ast::Stmt::Let {
                    name,
                    value: Box::new(value),
                }
            }
            _ => unreachable!(),
        })
    }
}
