use crate::lexer::{Span, TokenKind};
use std::fmt;

pub enum SyntaxError {
    UnexpectedToken {
        expected: String,
        token_kind: TokenKind,
        info: ErrorInfo,
    },
    FailedToParseLiteral {
        token_kind: TokenKind,
        info: ErrorInfo,
    },
    UnexpectedEndOfInput {
        info: ErrorInfo,
    },
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{}",
            match self {
                Self::UnexpectedToken {
                    expected,
                    token_kind,
                    info,
                } => format!(
                    "{}\x1b[1;31mUnexpected {}, expected {}\x1b[0;0m",
                    info, token_kind, expected
                ),
                Self::FailedToParseLiteral { token_kind, info } =>
                    format!("{}\x1b[1;31mFailed to parse {}\x1b[0;0m", info, token_kind),
                Self::UnexpectedEndOfInput { info } =>
                    format!("{}\x1b[1;31mUnexpected end of input\x1b[0;0m", info),
            }
        )
    }
}

pub struct ErrorInfo {
    pub span: Span,
    pub line: usize,
    pub column: usize,
    pub prev_line: Option<String>,
    pub curr_line: String,
}

impl ErrorInfo {
    pub fn new(span: Span, input: &str) -> Self {
        let (line, column) = span.get_line_and_column(input);
        let mut lines = input.lines();
        let prev_line = if line > 0 {
            lines.nth(line - 1).map(ToOwned::to_owned)
        } else {
            None
        };
        let curr_line = lines.next().unwrap().to_owned();

        Self {
            span,
            line,
            column,
            prev_line,
            curr_line,
        }
    }
}

impl fmt::Display for ErrorInfo {
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
            "^".repeat((self.span.end - self.span.start) as usize)
        )
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
    let err = ErrorInfo::new(
        Span {
            start: 138,
            end: 139,
        },
        test.as_str(),
    );

    println!("{}", err);
}
