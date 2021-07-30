use crate::lexer::{Span, Token, TokenKind};
use std::fmt;

pub struct Error {
    pub expected: TokenKind,
    pub token: Token,
    /// 0-based
    pub line: usize,
    /// 0-based
    pub column: usize,
    pub prev_line: Option<String>,
    pub curr_line: String,
}

impl Error {
    pub fn new(expected: TokenKind, token: Token, input: &str) -> Self {
        let (line, column) = token.get_line_and_column(input);
        let mut lines = input.lines();
        let prev_line = if line > 0 {
            lines.nth(line - 1).map(ToOwned::to_owned)
        } else {
            None
        };
        let curr_line = lines.next().unwrap().to_owned();

        Self {
            expected,
            token,
            line,
            column,
            prev_line,
            curr_line,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.line + 1;
        let column = self.column + 1;

        let width_of_num = (f64::log10(line as f64).floor() + 2_f64) as usize; // and the space after it

        writeln!(
            f,
            "\x1b[1;31mSyntax Error\x1b[0;0m at line {}, column {}:\n",
            line, column
        )?;
        if let Some(prev) = &self.prev_line {
            let padding = width_of_num - (f64::log10(line as f64 - 1_f64).floor() + 1_f64) as usize;
            writeln!(
                f,
                "\x1b[1;37m{}{}|\x1b[0;0m{}",
                line - 1,
                " ".repeat(padding),
                prev
            )?;
        }
        writeln!(f, "\x1b[1;37m{} |\x1b[0;0m{}", line, self.curr_line)?;
        writeln!(
            f,
            "{}{}",
            " ".repeat(width_of_num + column - 1),
            "^".repeat(self.token.len())
        )?;
        writeln!(f, "Unexpected {}, expected {}", self.token, self.expected)
    }
}

#[test]
fn printerr() {
    let test = format!(
        "{}function hello: x = begin
    let msg - \"Hello\" + x;
    print msg
end",
        "\n".repeat(100)
    );
    let err = Error::new(
        TokenKind::Assign,
        Token {
            kind: TokenKind::Minus,
            span: Span {
                start: 138,
                end: 139,
            },
        },
        test.as_str(),
    );

    println!("{}", err);
}
