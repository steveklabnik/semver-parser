//! Lexer for semver ranges.
//!
//! Breaks a string of input into an iterator of tokens that can be used with a parser.
//!
//! This should be used with the [`range_parser`] module.
//!
//! [`range_parser`]: ../range_parser/index.html
//!
//! # Examples
//!
//! Example without errors:
//!
//! ```rust
//! use semver_parser::range_lexer::{RangeLexer, Token};
//!
//! let mut l = RangeLexer::new("foo 123 *");
//!
//! assert_eq!(Some(Ok(Token::Identifier("foo"))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(3, 4))), l.next());
//! assert_eq!(Some(Ok(Token::Numeric(123))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(7, 8))), l.next());
//! assert_eq!(Some(Ok(Token::Star)), l.next());
//! assert_eq!(None, l.next());
//! ```
//!
//! Example with error:
//!
//! ```rust
//! use semver_parser::range_lexer::{RangeLexer, Token, Error};
//!
//! let mut l = RangeLexer::new("foo / *");
//!
//! assert_eq!(Some(Ok(Token::Identifier("foo"))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(3, 4))), l.next());
//! assert_eq!(Some(Err(Error::UnexpectedChar('/'))), l.next());
//! ```

use std::str;
use self::Token::*;
use self::Error::*;

macro_rules! scan_while {
    ($slf:expr, $start:expr, $first:pat $(| $rest:pat)*) => {{
        let mut __end = $start;

        loop {
            if let Some((idx, c)) = $slf.one() {
                __end = idx;

                match c {
                    $first $(| $rest)* => $slf.step(),
                    _ => break,
                }

                continue;
            } else {
                __end = $slf.input.len();
            }

            break;
        }

        __end
    }}
}

/// Semver tokens.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<'input> {
    /// `=`
    Eq,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,
    /// '^`
    Caret,
    /// '~`
    Tilde,
    /// '*`
    Star,
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `-`
    Hyphen,
    /// '||'
    Or,
    /// any number of whitespace (`\t\r\n `) and its span.
    Whitespace(usize, usize),
    /// Numeric component, like 0001
    Numeric(u64),
    /// Identifier component, like alpha1
    Identifier(&'input str),
    /// Build metadata component, things after '+'.
    BuildMetadata(&'input str),
}

impl<'input> Token<'input> {
    /// Check if the current token is a whitespace token.
    pub fn is_whitespace(&self) -> bool {
        match *self {
            Whitespace(_, _) => true,
            _ => false,
        }
    }

    /// Check if the current token is a wildcard token.
    pub fn is_wildcard(&self) -> bool {
        match *self {
            Star | Identifier("X") | Identifier("x") => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    /// Unexpected character.
    UnexpectedChar(char),
    /// Encountered a span that is not a legal number.
    IllegalNumber(usize, usize),
}

/// Lexer for semver tokens belonging to a range.
#[derive(Debug)]
pub struct RangeLexer<'input> {
    input: &'input str,
    chars: str::CharIndices<'input>,
    // lookeahead
    c1: Option<(usize, char)>,
    c2: Option<(usize, char)>,
}

impl<'input> RangeLexer<'input> {
    /// Construct a new lexer for the given input.
    pub fn new(input: &str) -> RangeLexer {
        let mut chars = input.char_indices();
        let c1 = chars.next();
        let c2 = chars.next();

        RangeLexer {
            input: input,
            chars: chars,
            c1: c1,
            c2: c2,
        }
    }

    /// Shift all lookahead storage by one.
    fn step(&mut self) {
        self.c1 = self.c2;
        self.c2 = self.chars.next();
    }

    fn step_n(&mut self, n: usize) {
        for _ in 0..n {
            self.step();
        }
    }

    /// Access the one character, or set it if it is not set.
    fn one(&mut self) -> Option<(usize, char)> {
        self.c1
    }

    /// Access two characters.
    fn two(&mut self) -> Option<(usize, char, char)> {
        self.c1.and_then(
            |(start, c1)| self.c2.map(|(_, c2)| (start, c1, c2)),
        )
    }

    /// Consume a number.
    fn number(&mut self, start: usize) -> Result<u64, Error> {
        let end = scan_while!(self, start, '0'...'9');

        match self.input[start..end].parse::<u64>() {
            Ok(n) => Ok(n),
            Err(_) => Err(IllegalNumber(start, end)),
        }
    }

    /// Consume an identifier.
    fn identifier(&mut self, start: usize) -> Result<&'input str, Error> {
        let end = scan_while!(self, start, '0'...'9' | 'A'...'Z' | 'a'...'z' | '+' | '-');
        Ok(&self.input[start..end])
    }

    /// Consume build metadata
    fn build_metadata(&mut self, start: usize) -> Result<&'input str, Error> {
        let end = scan_while!(self, start, '0'...'9' | 'A'...'Z' | 'a'...'z' | '+' | '-' | '.');
        Ok(&self.input[start..end])
    }

    /// Consume whitespace.
    fn whitespace(&mut self, start: usize) -> Result<Token<'input>, Error> {
        let end = scan_while!(self, start, ' ' | '\t' | '\n' | '\r');
        Ok(Whitespace(start, end))
    }
}

impl<'input> Iterator for RangeLexer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // two subsequent char tokens.
            if let Some((_, a, b)) = self.two() {
                let two = match (a, b) {
                    ('<', '=') => Some(LtEq),
                    ('>', '=') => Some(GtEq),
                    ('|', '|') => Some(Or),
                    _ => None,
                };

                if let Some(two) = two {
                    self.step_n(2);
                    return Some(Ok(two));
                }
            }

            // single char and start of numeric tokens.
            if let Some((start, c)) = self.one() {
                let tok = match c {
                    ' ' | '\t' | '\n' | '\r' => {
                        self.step();
                        return Some(self.whitespace(start));
                    }
                    '=' => Eq,
                    '>' => Gt,
                    '<' => Lt,
                    '^' => Caret,
                    '~' => Tilde,
                    '*' => Star,
                    '.' => Dot,
                    ',' => Comma,
                    '-' => Hyphen,
                    '+' => {
                        self.step();
                        return Some(self.build_metadata(start).map(BuildMetadata));
                    }
                    '0' => Numeric(0),
                    '1'...'9' => {
                        self.step();
                        return Some(self.number(start).map(Numeric));
                    }
                    'a'...'z' | 'A'...'Z' => {
                        self.step();
                        return Some(self.identifier(start).map(Identifier));
                    }
                    c => return Some(Err(UnexpectedChar(c))),
                };

                self.step();
                return Some(Ok(tok));
            };

            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        RangeLexer::new(input)
            .map(Result::unwrap)
            .collect::<Vec<_>>()
    }

    #[test]
    pub fn all_tokens() {
        assert_eq!(
            lex("=><.^~*01234<=>=||"),
            vec![
                Eq,
                Gt,
                Lt,
                Dot,
                Caret,
                Tilde,
                Star,
                Numeric(0),
                Numeric(1234),
                LtEq,
                GtEq,
                Or,
            ]
        );
    }

    #[test]
    pub fn empty() {
        assert_eq!(lex(""), vec![]);
    }

    #[test]
    pub fn numeric_leading_zeros() {
        assert_eq!(
            lex("0000"),
            vec![Numeric(0), Numeric(0), Numeric(0), Numeric(0)]
        );
        assert_eq!(
            lex("0001"),
            vec![Numeric(0), Numeric(0), Numeric(0), Numeric(1)]
        );
    }

    #[test]
    pub fn numeric_all_numbers() {
        let expected: Vec<Token> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            .into_iter()
            .map(Numeric)
            .collect::<Vec<_>>();

        let actual: Vec<_> = lex("0 1 2 3 4 5 6 7 8 9")
            .into_iter()
            .filter(|t| !t.is_whitespace())
            .collect();

        assert_eq!(actual, expected);
    }
}
