use range_lexer::{self, RangeLexer, Token};
use self::Error::*;
use range::{Predicate, Op, VersionReq, WildcardVersion};
use comparator::Comparator;
use version::Identifier;
use std::mem;

/// Evaluate if parser contains the given pattern as a separator, surrounded by whitespace.
macro_rules! has_ws_separator {
    ($slf:expr, $pat:pat) => {{
        $slf.skip_whitespace()?;

        match $slf.peek() {
            $pat => {
                // pop the separator.
                $slf.pop()?;
                // strip suffixing whitespace.
                $slf.skip_whitespace()?;
                true
            },
            _ => false,
        }
    }}
}

#[derive(Debug)]
pub enum Error<'input> {
    /// Needed more tokens for parsing, but none are available.
    UnexpectedEnd,
    /// Unexpected token.
    UnexpectedToken(Token<'input>),
    /// An error occurred in the lexer.
    Lexer(range_lexer::Error),
    /// More input available.
    MoreInput,
    /// Encountered empty predicate in a set of predicates.
    EmptyPredicate,
    /// Encountered an empty range.
    EmptyRange,
}

impl<'input> From<range_lexer::Error> for Error<'input> {
    fn from(value: range_lexer::Error) -> Self {
        Error::Lexer(value)
    }
}

/// A recursive-descent parser for parsing version requirements.
pub struct RangeParser<'input> {
    /// Source of token.
    lexer: RangeLexer<'input>,
    /// Lookaehead.
    c1: Option<Token<'input>>,
}

impl<'input> RangeParser<'input> {
    pub fn new(input: &'input str) -> Result<RangeParser<'input>, Error<'input>> {
        let mut lexer = RangeLexer::new(input);

        let c1 = if let Some(c1) = lexer.next() {
            Some(c1?)
        } else {
            None
        };

        Ok(RangeParser {
            lexer: lexer,
            c1: c1,
        })
    }

    /// Pop one token.
    #[inline(always)]
    fn pop(&mut self) -> Result<Token<'input>, Error<'input>> {
        let c1 = if let Some(c1) = self.lexer.next() {
            Some(c1?)
        } else {
            None
        };

        mem::replace(&mut self.c1, c1).ok_or_else(|| UnexpectedEnd)
    }

    /// Peek one token.
    #[inline(always)]
    fn peek(&mut self) -> Option<&Token<'input>> {
        self.c1.as_ref()
    }

    /// Skip whitespace if present.
    fn skip_whitespace(&mut self) -> Result<(), Error<'input>> {
        match self.peek() {
            Some(&Token::Whitespace(_, _)) => self.pop().map(|_| ()),
            _ => Ok(()),
        }
    }

    fn comma_predicate(&mut self) -> Result<Option<Predicate>, Error<'input>> {
        if !has_ws_separator!(self, Some(&Token::Comma)) {
            return Ok(None);
        }

        if let Some(predicate) = self.predicate()? {
            Ok(Some(predicate))
        } else {
            Err(EmptyPredicate)
        }
    }

    fn or_range(&mut self) -> Result<Option<VersionReq>, Error<'input>> {
        if !has_ws_separator!(self, Some(&Token::Or)) {
            return Ok(None);
        }

        Ok(Some(self.range()?))
    }

    /// Optionally parse a dot, then a numeric component.
    ///
    /// The second component of the tuple indicates if a wildcard has been encountered, and is
    /// always `false` if the first component is `Some`.
    ///
    /// If a dot is not encountered, `(None, false)` is returned.
    ///
    /// If a wildcard is encountered, `(None, true)` is returned.
    fn dot_numeric(&mut self) -> Result<(Option<u64>, bool), Error<'input>> {
        match self.peek() {
            Some(&Token::Dot) => {}
            _ => return Ok((None, false)),
        }

        // pop the peeked dot.
        self.pop()?;

        match self.pop()? {
            Token::Numeric(number) => Ok((Some(number), false)),
            Token::Star => Ok((None, true)),
            tok => Err(UnexpectedToken(tok)),
        }
    }

    /// Parse a single identifier.
    fn identifier(&mut self) -> Result<Identifier, Error<'input>> {
        let identifier = match self.pop()? {
            Token::Identifier(identifier) => {
                // TODO: Borrow?
                Identifier::AlphaNumeric(identifier.to_string())
            }
            Token::Numeric(n) => Identifier::Numeric(n),
            tok => return Err(UnexpectedToken(tok)),
        };

        Ok(identifier)
    }

    /// Parse pre-release identifiers.
    fn pre(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
        match self.peek() {
            Some(&Token::Hyphen) => {}
            _ => return Ok(vec![]),
        }

        // pop the peeked hyphen.
        self.pop()?;

        let mut pre = Vec::new();

        pre.push(self.identifier()?);

        loop {
            match self.peek() {
                Some(&Token::Dot) => {}
                _ => break,
            }

            // pop the peeked hyphen.
            self.pop()?;

            pre.push(self.identifier()?);
        }

        Ok(pre)
    }

    /// Parse optional build metadata.
    fn build_metadata(&mut self) -> Result<(), Error<'input>> {
        match self.peek() {
            Some(&Token::BuildMetadata(_)) => {}
            _ => return Ok(()),
        }

        // pop the build metadata.
        self.pop()?;
        Ok(())
    }

    /// Parse a single predicate.
    pub fn predicate(&mut self) -> Result<Option<Predicate>, Error<'input>> {
        // empty predicate.
        if self.peek().is_none() {
            return Ok(None);
        }

        let head = self.pop()?;

        let (tok, mut op) = match head {
            Token::Eq => (None, Op::Ex),
            Token::Gt => (None, Op::Gt),
            Token::GtEq => (None, Op::GtEq),
            Token::Lt => (None, Op::Lt),
            Token::LtEq => (None, Op::LtEq),
            Token::Tilde => (None, Op::Tilde),
            Token::Caret => (None, Op::Compatible),
            Token::Star => return Ok(None),
            // default op
            head => (Some(head), Op::Compatible),
        };

        // make use of token carried over, or pop a new one if operator found.
        let tok = if let Some(tok) = tok {
            tok
        } else {
            // pop a new token, whie skipping one set of whitespace
            match self.pop()? {
                Token::Whitespace(_, _) => self.pop()?,
                tok => tok,
            }
        };

        let major = match tok {
            Token::Numeric(number) => number,
            tok => return Err(UnexpectedToken(tok)),
        };

        let (minor, minor_wildcard) = self.dot_numeric()?;
        let (patch, patch_wildcard) = self.dot_numeric()?;
        let pre = self.pre()?;

        // TODO: avoid illegal combinations, like `1.*.0`.
        if minor_wildcard {
            op = Op::Wildcard(WildcardVersion::Minor);
        }

        if patch_wildcard {
            op = Op::Wildcard(WildcardVersion::Patch);
        }

        // ignore build metadata
        self.build_metadata()?;

        Ok(Some(Predicate {
            op: op,
            major: major,
            minor: minor,
            patch: patch,
            pre: pre,
        }))
    }

    pub fn range(&mut self) -> Result<VersionReq, Error<'input>> {
        let mut predicates = Vec::new();

        if let Some(predicate) = self.predicate()? {
            predicates.push(predicate);

            while let Some(next) = self.comma_predicate()? {
                predicates.push(next);
            }
        }

        Ok(VersionReq { predicates: predicates })
    }

    pub fn range_set(&mut self) -> Result<Comparator, Error<'input>> {
        let mut ranges = Vec::new();
        ranges.push(self.range()?);

        while let Some(next) = self.or_range()? {
            ranges.push(next);
        }

        Ok(Comparator { ranges: ranges })
    }

    /// Check if we have reached the end of input.
    pub fn is_eof(&mut self) -> bool {
        self.c1.is_none()
    }
}

fn parse_comparator<'input>(input: &'input str) -> Result<Comparator, Error<'input>> {
    let mut parser = RangeParser::new(input)?;
    let range_set = parser.range_set()?;

    if !parser.is_eof() {
        return Err(MoreInput);
    }

    Ok(range_set)
}

fn parse<'input>(input: &'input str) -> Result<VersionReq, Error<'input>> {
    let mut parser = RangeParser::new(input)?;
    let range = parser.range()?;

    if !parser.is_eof() {
        return Err(MoreInput);
    }

    Ok(range)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_sets() {
        assert_eq!(1, parse_comparator("1.0.0").unwrap().ranges.len());
        assert_eq!(2, parse_comparator("1.0.0 || ^4").unwrap().ranges.len());

        let c3 = parse_comparator("1.0.0 || ^4 || ~10, ~11, ~12").unwrap();

        assert_eq!(3, c3.ranges.len());
        assert_eq!(1, c3.ranges[0].predicates.len());
        assert_eq!(1, c3.ranges[1].predicates.len());
        assert_eq!(3, c3.ranges[2].predicates.len());
    }

    #[test]
    fn test_parsing_wildcards() {
        assert_eq!(
            Op::Wildcard(WildcardVersion::Patch),
            parse("1.0.*").unwrap().predicates[0].op
        );
        assert_eq!(
            Op::Wildcard(WildcardVersion::Patch),
            parse("1.*.*").unwrap().predicates[0].op
        );
        assert_eq!(
            Op::Wildcard(WildcardVersion::Minor),
            parse("1.*.0").unwrap().predicates[0].op
        );
    }

    #[test]
    fn test_parsing_default() {
        let r = parse("1.0.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 1,
                minor: Some(0),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_exact_01() {
        let r = parse("=1.0.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Ex,
                major: 1,
                minor: Some(0),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_exact_02() {
        let r = parse("=0.9.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Ex,
                major: 0,
                minor: Some(9),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_exact_03() {
        let r = parse("=0.1.0-beta2.a").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Ex,
                major: 0,
                minor: Some(1),
                patch: Some(0),
                pre: vec![
                    Identifier::AlphaNumeric(String::from("beta2")),
                    Identifier::AlphaNumeric(String::from("a")),
                ],
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_greater_than() {
        let r = parse("> 1.0.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Gt,
                major: 1,
                minor: Some(0),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_greater_than_01() {
        let r = parse(">= 1.0.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::GtEq,
                major: 1,
                minor: Some(0),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_greater_than_02() {
        let r = parse(">= 2.1.0-alpha2").unwrap();

        assert_eq!(
            Predicate {
                op: Op::GtEq,
                major: 2,
                minor: Some(1),
                patch: Some(0),
                pre: vec![Identifier::AlphaNumeric(String::from("alpha2"))],
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_less_than() {
        let r = parse("< 1.0.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Lt,
                major: 1,
                minor: Some(0),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_less_than_eq() {
        let r = parse("<= 2.1.0-alpha2").unwrap();

        assert_eq!(
            Predicate {
                op: Op::LtEq,
                major: 2,
                minor: Some(1),
                patch: Some(0),
                pre: vec![Identifier::AlphaNumeric(String::from("alpha2"))],
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_tilde() {
        let r = parse("~1").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Tilde,
                major: 1,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_compatible() {
        let r = parse("^0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_blank() {
        let r = parse("").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_wildcard() {
        let r = parse("*").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_x() {
        let r = parse("x").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_capital_x() {
        let r = parse("X").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_minor_wildcard_star() {
        let r = parse("1.*").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Minor),
                major: 1,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_minor_wildcard_x() {
        let r = parse("1.x").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Minor),
                major: 1,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_minor_wildcard_capital_x() {
        let r = parse("1.X").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Minor),
                major: 1,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_patch_wildcard_star() {
        let r = parse("1.2.*").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Patch),
                major: 1,
                minor: Some(2),
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_patch_wildcard_x() {
        let r = parse("1.2.x").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Patch),
                major: 1,
                minor: Some(2),
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_patch_wildcard_capital_x() {
        let r = parse("1.2.X").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Wildcard(WildcardVersion::Patch),
                major: 1,
                minor: Some(2),
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_multiple_01() {
        let r = parse("> 0.0.9, <= 2.5.3").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Gt,
                major: 0,
                minor: Some(0),
                patch: Some(9),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(
            Predicate {
                op: Op::LtEq,
                major: 2,
                minor: Some(5),
                patch: Some(3),
                pre: Vec::new(),
            },
            r.predicates[1]
        );
    }

    #[test]
    pub fn test_multiple_02() {
        let r = parse("0.3.0, 0.4.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(3),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(4),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[1]
        );
    }

    #[test]
    pub fn test_multiple_03() {
        let r = parse("<= 0.2.0, >= 0.5.0").unwrap();

        assert_eq!(
            Predicate {
                op: Op::LtEq,
                major: 0,
                minor: Some(2),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(
            Predicate {
                op: Op::GtEq,
                major: 0,
                minor: Some(5),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[1]
        );
    }

    #[test]
    pub fn test_multiple_04() {
        let r = parse("0.1.0, 0.1.4, 0.1.6").unwrap();

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(1),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(1),
                patch: Some(4),
                pre: Vec::new(),
            },
            r.predicates[1]
        );

        assert_eq!(
            Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(1),
                patch: Some(6),
                pre: Vec::new(),
            },
            r.predicates[2]
        );
    }

    #[test]
    pub fn test_multiple_05() {
        let r = parse(">=0.5.1-alpha3, <0.6").unwrap();

        assert_eq!(
            Predicate {
                op: Op::GtEq,
                major: 0,
                minor: Some(5),
                patch: Some(1),
                pre: vec![Identifier::AlphaNumeric(String::from("alpha3"))],
            },
            r.predicates[0]
        );

        assert_eq!(
            Predicate {
                op: Op::Lt,
                major: 0,
                minor: Some(6),
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[1]
        );
    }

    #[test]
    fn test_parse_build_metadata_with_predicate() {
        assert_eq!(
            parse("^1.2.3+meta").unwrap().predicates[0].op,
            Op::Compatible
        );
        assert_eq!(parse("~1.2.3+meta").unwrap().predicates[0].op, Op::Tilde);
        assert_eq!(parse("=1.2.3+meta").unwrap().predicates[0].op, Op::Ex);
        assert_eq!(parse("<=1.2.3+meta").unwrap().predicates[0].op, Op::LtEq);
        assert_eq!(parse(">=1.2.3+meta").unwrap().predicates[0].op, Op::GtEq);
        assert_eq!(parse("<1.2.3+meta").unwrap().predicates[0].op, Op::Lt);
        assert_eq!(parse(">1.2.3+meta").unwrap().predicates[0].op, Op::Gt);
    }

    #[test]
    pub fn test_parse_errors() {
        assert!(parse("\0").is_err());
        assert!(parse(">= >= 0.0.2").is_err());
        assert!(parse(">== 0.0.2").is_err());
        assert!(parse("a.0.0").is_err());
        assert!(parse("1.0.0-").is_err());
        assert!(parse(">=").is_err());
        assert!(parse("> 0.1.0,").is_err());
        assert!(parse("> 0.3.0, ,").is_err());
        assert!(parse("> 0. 1").is_err());
    }

    #[test]
    pub fn test_large_major_version() {
        assert!(parse("18446744073709551617.0.0").is_err());
    }

    #[test]
    pub fn test_large_minor_version() {
        assert!(parse("0.18446744073709551617.0").is_err());
    }

    #[test]
    pub fn test_large_patch_version() {
        assert!(parse("0.0.18446744073709551617").is_err());
    }
}
