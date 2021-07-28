mod ast;
mod expressions;
mod statements;

pub use ast::*;
pub use expressions::*;
pub use statements::*;

use std::iter::Peekable;

use crate::lexer::{Lexer, Token, TokenKind};

/// Wraps `Lexer` to filter out whitespace and comments
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
}

/// The parser
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// To extract a token's text from
    input: &'input str,
    /// The lexer wrapped in `Peekable`
    tokens: Peekable<I>,
}

/* TokenIter */

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    /// Get next `Token` from `Lexer`  (if the next token is whitespace or a comment then loop until it is not)
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next_token = self.lexer.next()?;
            if !matches!(next_token.kind, TokenKind::Whitespace | TokenKind::Comment) {
                return Some(next_token);
            } // else continue
        }
    }
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }
}

/* Parser */

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
}

// Some utility functions for parsing
impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Get the source text of a token
    pub fn text(&self, token: Token) -> &'input str {
        token.text(&self.input)
    }

    /// Look ahead a single token and get its `TokenKind` without consuming it
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::EOF)
    }

    /// Check if the next token (using `peek()`) is of the specified `kind`
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Get and consume the next token
    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Move forward a single token and check that the kind of token is `expected`
    pub(crate) fn consume(&mut self, expected: TokenKind) -> Option<()> {
        let token = self.next()?;
        if token.kind != expected {
            return None;
        }
        Some(())
        // .expect(&format!(
        //     "Expected to consume `{}` but there was no next token",
        //     expected
        // ));
        // assert_eq!(
        //     token.kind, expected,
        //     "Expected to consume `{}` but found `{}`",
        //     expected, token.kind
        // );
    }
}
