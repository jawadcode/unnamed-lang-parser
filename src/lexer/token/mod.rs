use std::fmt;
use std::ops::{Index, Range};

mod token_kinds;
pub use token_kinds::*;

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline(always)]
    /// Returns the length of the token
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    /// Returns the token's text as a slice of the input string
    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} ({}, {})",
            self.kind, self.span.start, self.span.end
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Custom span to store the position of a token in a source string
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end: u32,
}

impl Span {
    /// Return the line number (0 based) and column number (0 based)
    /// of the token (relative to the input string)
    pub(crate) fn get_line_and_column(&self, input: &str) -> (usize, usize) {
        let start = self.start as usize;
        let mut line = 0;
        let mut column = 0;
        for (index, byte) in input.bytes().enumerate() {
            if index == start {
                break;
            }
            if byte == b'\n' {
                line += 1;
                column = 0;
            }
            column += 1;
        }
        (line, column)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}
