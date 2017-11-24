//! Version range and requirements data and functions (used for version comparison).
//!
//! This module contains [`Predicate`] struct which holds data for comparison
//! [`version::Version`] structs: [`VersionReq`] struct as a collection of
//! [`Predicate`]s, functions for parsing those structs and some helper data structures
//! and functions.
//!
//! # Examples
//!
//! Parsing version range and matching it with concrete version:
//!
//! ```
//! use semver_parser::range;
//! use semver_parser::version;
//!
//! # fn try_main() -> Result<(), String> {
//! let r = range::parse("1.0.0")?;
//!
//! assert_eq!(range::Predicate {
//!         op: range::Op::Compatible,
//!         major: 1,
//!         minor: Some(0),
//!         patch: Some(0),
//!         pre: Vec::new(),
//!     },
//!     r.predicates[0]
//! );
//!
//! let m = version::parse("1.0.0")?;
//! for p in &r.predicates {
//!     match p.op {
//!         range::Op::Compatible => {
//!             assert_eq!(p.major, m.major);
//!         }
//!         _ => {
//!             unimplemented!();
//!         }
//!     }
//! }
//! # Ok(())
//! # }
//! #
//! # fn main() {
//! #   try_main().unwrap();
//! # }
//! ```
//! [`Predicate`]: ./struct.Predicate.html
//! [`VersionReq`]: ./struct.VersionReq.html
//! [`version::Version`]: ../version/struct.Version.html

use common::{self, numeric_identifier, letters_numbers_dash_dot};
use version::Identifier;
use std::str::{FromStr, from_utf8};
use recognize::*;

/// Struct holding collection of version requirements.
///
/// High-level collection of requirements for versions. Requirements are [`Predicate`] structs.
///
/// # Examples
///
/// Simple single-predicate `VersionReq`:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let r = range::parse("1.0.0")?;
///
/// assert_eq!(range::Predicate {
///         op: range::Op::Compatible,
///         major: 1,
///         minor: Some(0),
///         patch: Some(0),
///         pre: Vec::new(),
///     },
///     r.predicates[0]
/// );
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
///
/// Multiple predicates in `VersionReq`:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let r = range::parse("> 0.0.9, <= 2.5.3")?;
///
/// assert_eq!(range::Predicate {
///         op: range::Op::Gt,
///         major: 0,
///         minor: Some(0),
///         patch: Some(9),
///         pre: Vec::new(),
///     },
///     r.predicates[0]
/// );
///
/// assert_eq!(range::Predicate {
///         op: range::Op::LtEq,
///         major: 2,
///         minor: Some(5),
///         patch: Some(3),
///         pre: Vec::new(),
///     },
///     r.predicates[1]
/// );
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// [`Predicate`]: ./struct.Predicate.html
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct VersionReq {
    /// Collection of predicates.
    pub predicates: Vec<Predicate>,
}

/// Enum representing a `*` version part.
///
/// This is one of variants of the [`Op`] enum wich is part of [`Predicate`] enum.
/// All variants represent some "match-all" cases for specific numeric parts of version.
///
/// # Examples
///
/// Parsing wildcard predicate and checking that its predicates are empty.
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let r = range::parse("*")?;
///
/// assert!(r.predicates.is_empty());
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
/// [`Op`]: ./enum.Op.html
/// [`Predicate`]: ./struct.Predicate.html
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Hash, Clone)]
pub enum WildcardVersion {
    /// Wildcard minor version `1.*.3`.
    Minor,
    /// Wildcard patch version `1.2.*`.
    Patch,
}

/// Enum representing operation in [`Predicate`].
///
/// This enum represents an operation for comparing two [`version::Version`]s.
///
/// # Examples
///
/// Parsing `Op` from string:
///
/// ```
/// use semver_parser::range;
/// use std::str::FromStr;
///
/// # fn try_main() -> Result<(), String> {
/// let exact = range::Op::from_str("=")?;
/// assert_eq!(exact, range::Op::Ex);
/// let gt_eq = range::Op::from_str(">=")?;
/// assert_eq!(gt_eq, range::Op::GtEq);
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
/// [`Predicate`]: ./struct.Predicate.html
/// [`version::Version`]: ../version/struct.Version.html
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Hash)]
pub enum Op {
    /// Exact, `=`.
    Ex,
    /// Greater than, `>`.
    Gt,
    /// Greater than or equal to, `>=`.
    GtEq,
    /// Less than, `<`.
    Lt,
    /// Less than or equal to, `<=`.
    LtEq,
    /// [Tilde](http://doc.crates.io/specifying-dependencies.html#tilde-requirements)
    /// requirements, like `~1.0.0` - a minimal version with some ability to update.
    Tilde,
    /// [Compatible](http://doc.crates.io/specifying-dependencies.html#caret-requirements)
    /// by definition of semver, indicated by `^`.
    Compatible,
    /// `x.y.*`, `x.*`, `*`.
    Wildcard(WildcardVersion),
}

impl FromStr for Op {
    type Err = String;

    fn from_str(s: &str) -> Result<Op, String> {
        match s {
            "=" => Ok(Op::Ex),
            ">" => Ok(Op::Gt),
            ">=" => Ok(Op::GtEq),
            "<" => Ok(Op::Lt),
            "<=" => Ok(Op::LtEq),
            "~" => Ok(Op::Tilde),
            "^" => Ok(Op::Compatible),
            _ => Err(String::from("Could not parse Op")),
        }
    }
}

/// Struct representing a version comparison predicate.
///
/// Struct contaions operation code and data for comparison of [`version::Version`]s.
///
/// # Examples
///
/// Parsing [`Predicate`] from string and checking its fields:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let p = range::parse_predicate(">=1.1")?;
/// assert_eq!(p.op, range::Op::GtEq);
/// assert_eq!(p.major, 1);
/// assert_eq!(p.minor.unwrap(), 1);
/// assert!(p.patch.is_none());
/// assert!(p.pre.is_empty());
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
/// [`Predicate`]: ./struct.Predicate.html
/// [`version::Version`]: ../version/struct.Version.html
#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord)]
pub struct Predicate {
    /// Operation code for this predicate, like "greater than" or "exact match".
    pub op: Op,
    /// Major version.
    pub major: u64,
    /// Optional minor version.
    pub minor: Option<u64>,
    /// Optional patch version.
    pub patch: Option<u64>,
    /// Collection of `Identifier`s of version, like `"alpha1"` in `"1.2.3-alpha1"`.
    pub pre: Vec<Identifier>,
}

fn numeric_or_wild(s: &[u8]) -> Option<(Option<u64>, usize)> {
    if let Some((val, len)) = numeric_identifier(s) {
        Some((Some(val), len))
    } else if let Some(len) = OneOf(b"*xX").p(s) {
        Some((None, len))
    } else {
        None
    }
}

fn dot_numeric_or_wild(s: &[u8]) -> Option<(Option<u64>, usize)> {
    b'.'.p(s).and_then(|len| {
        numeric_or_wild(&s[len..]).map(|(val, len2)| (val, len + len2))
    })
}

fn operation(s: &[u8]) -> Option<(Op, usize)> {
    if let Some(len) = "=".p(s) {
        Some((Op::Ex, len))
    } else if let Some(len) = ">=".p(s) {
        Some((Op::GtEq, len))
    } else if let Some(len) = ">".p(s) {
        Some((Op::Gt, len))
    } else if let Some(len) = "<=".p(s) {
        Some((Op::LtEq, len))
    } else if let Some(len) = "<".p(s) {
        Some((Op::Lt, len))
    } else if let Some(len) = "~".p(s) {
        Some((Op::Tilde, len))
    } else if let Some(len) = "^".p(s) {
        Some((Op::Compatible, len))
    } else {
        None
    }
}

fn whitespace(s: &[u8]) -> Option<usize> {
    ZeroOrMore(OneOf(b"\t\r\n ")).p(s)
}

/// Function parsing [`Predicate`] from string.
///
/// Function parsing [`Predicate`] from string to `Result<`[`Predicate`]`, String>`,
/// where `Err` will contain error message in case of failed parsing.
///
/// # Examples
///
/// Parsing [`Predicate`] from string and cheking its fields:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let p = range::parse_predicate(">=1.1")?;
/// assert_eq!(p.op, range::Op::GtEq);
/// assert_eq!(p.major, 1);
/// assert_eq!(p.minor.unwrap(), 1);
/// assert!(p.patch.is_none());
/// assert!(p.pre.is_empty());
///
/// let f = range::parse_predicate("not-a-version-predicate");
/// assert!(f.is_err());
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
/// [`Predicate`]: ./struct.Predicate.html
pub fn parse_predicate(range: &str) -> Result<Predicate, String> {
    let s = range.trim().as_bytes();
    let mut i = 0;
    let mut operation = if let Some((op, len)) = operation(&s[i..]) {
        i += len;
        op
    } else {
        // operations default to Compatible
        Op::Compatible
    };
    if let Some(len) = whitespace.p(&s[i..]) {
        i += len;
    }
    let major = if let Some((major, len)) = numeric_identifier(&s[i..]) {
        i += len;
        major
    } else {
        return Err("Error parsing major version number: ".to_string());
    };
    let minor = if let Some((minor, len)) = dot_numeric_or_wild(&s[i..]) {
        i += len;
        if minor.is_none() {
            operation = Op::Wildcard(WildcardVersion::Minor);
        }
        minor
    } else {
        None
    };
    let patch = if let Some((patch, len)) = dot_numeric_or_wild(&s[i..]) {
        i += len;
        if patch.is_none() {
            operation = Op::Wildcard(WildcardVersion::Patch);
        }
        patch
    } else {
        None
    };
    let (pre, pre_len) = common::parse_optional_meta(&s[i..], b'-')?;
    i += pre_len;
    if let Some(len) = (b'+', letters_numbers_dash_dot).p(&s[i..]) {
        i += len;
    }
    if i != s.len() {
        return Err(
            "Extra junk after valid predicate: ".to_string() + from_utf8(&s[i..]).unwrap(),
        );
    }
    Ok(Predicate {
        op: operation,
        major: major,
        minor: minor,
        patch: patch,
        pre: pre,
    })
}

/// Function for parsing [`VersionReq`] from string.
///
/// Function for parsing [`VersionReq`] from string to `Result<`[`VersionReq`]`, String>`,
/// where `Err` will contain error message in case of failed parsing.
///
/// # Examples
///
/// Simple single-predicate [`VersionReq`]:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let r = range::parse("1.0.0")?;
///
/// assert_eq!(range::Predicate {
///         op: range::Op::Compatible,
///         major: 1,
///         minor: Some(0),
///         patch: Some(0),
///         pre: Vec::new(),
///     },
///     r.predicates[0]
/// );
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// ```
///
/// Multiple predicates in [`VersionReq`]:
///
/// ```
/// use semver_parser::range;
///
/// # fn try_main() -> Result<(), String> {
/// let r = range::parse("> 0.0.9, <= 2.5.3")?;
///
/// assert_eq!(range::Predicate {
///         op: range::Op::Gt,
///         major: 0,
///         minor: Some(0),
///         patch: Some(9),
///         pre: Vec::new(),
///     },
///     r.predicates[0]
/// );
///
/// assert_eq!(range::Predicate {
///         op: range::Op::LtEq,
///         major: 2,
///         minor: Some(5),
///         patch: Some(3),
///         pre: Vec::new(),
///     },
///     r.predicates[1]
/// );
/// # Ok(())
/// # }
/// #
/// # fn main() {
/// #   try_main().unwrap();
/// # }
/// [`VersionReq`]: ./struct.VersionReq.html
pub fn parse(ranges: &str) -> Result<VersionReq, String> {
    // null is an error
    if ranges == "\0" {
        return Err(String::from("Null is not a valid VersionReq"));
    }

    // an empty range is a major version wildcard
    // so is a lone * or x of either capitalization
    if (ranges == "") || (ranges == "*") || (ranges == "x") || (ranges == "X") {
        return Ok(VersionReq { predicates: vec![] });
    }


    let ranges = ranges.trim();

    let predicates: Result<Vec<_>, String> = ranges
        .split(",")
        .map(|range| parse_predicate(range))
        .collect();

    let predicates = try!(predicates);

    if predicates.len() == 0 {
        return Err(String::from("VersionReq did not parse properly"));
    }

    Ok(VersionReq { predicates: predicates })
}

#[cfg(test)]
mod tests {
    use super::*;
    use range;
    use version::Identifier;

    #[test]
    fn test_parsing_default() {
        let r = range::parse("1.0.0").unwrap();

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
        let r = range::parse("=1.0.0").unwrap();

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
        let r = range::parse("=0.9.0").unwrap();

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
        let r = range::parse("=0.1.0-beta2.a").unwrap();

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
        let r = range::parse("> 1.0.0").unwrap();

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
        let r = range::parse(">= 1.0.0").unwrap();

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
        let r = range::parse(">= 2.1.0-alpha2").unwrap();

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
        let r = range::parse("< 1.0.0").unwrap();

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
        let r = range::parse("<= 2.1.0-alpha2").unwrap();

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
        let r = range::parse("~1").unwrap();

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
        let r = range::parse("^0").unwrap();

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
        let r = range::parse("").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_wildcard() {
        let r = range::parse("*").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_x() {
        let r = range::parse("x").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_capital_x() {
        let r = range::parse("X").unwrap();
        assert!(r.predicates.is_empty());
    }

    #[test]
    fn test_parsing_minor_wildcard_star() {
        let r = range::parse("1.*").unwrap();

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
        let r = range::parse("1.x").unwrap();

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
        let r = range::parse("1.X").unwrap();

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
        let r = range::parse("1.2.*").unwrap();

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
        let r = range::parse("1.2.x").unwrap();

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
        let r = range::parse("1.2.X").unwrap();

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
        let r = range::parse("> 0.0.9, <= 2.5.3").unwrap();

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
        let r = range::parse("0.3.0, 0.4.0").unwrap();

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
        let r = range::parse("<= 0.2.0, >= 0.5.0").unwrap();

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
        let r = range::parse("0.1.0, 0.1.4, 0.1.6").unwrap();

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
        let r = range::parse(">=0.5.1-alpha3, <0.6").unwrap();

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
            range::parse("^1.2.3+meta").unwrap().predicates[0].op,
            Op::Compatible
        );
        assert_eq!(
            range::parse("~1.2.3+meta").unwrap().predicates[0].op,
            Op::Tilde
        );
        assert_eq!(
            range::parse("=1.2.3+meta").unwrap().predicates[0].op,
            Op::Ex
        );
        assert_eq!(
            range::parse("<=1.2.3+meta").unwrap().predicates[0].op,
            Op::LtEq
        );
        assert_eq!(
            range::parse(">=1.2.3+meta").unwrap().predicates[0].op,
            Op::GtEq
        );
        assert_eq!(
            range::parse("<1.2.3+meta").unwrap().predicates[0].op,
            Op::Lt
        );
        assert_eq!(
            range::parse(">1.2.3+meta").unwrap().predicates[0].op,
            Op::Gt
        );
    }

    #[test]
    pub fn test_parse_errors() {
        assert!(range::parse("\0").is_err());
        assert!(range::parse(">= >= 0.0.2").is_err());
        assert!(range::parse(">== 0.0.2").is_err());
        assert!(range::parse("a.0.0").is_err());
        assert!(range::parse("1.0.0-").is_err());
        assert!(range::parse(">=").is_err());
        assert!(range::parse("> 0.1.0,").is_err());
        assert!(range::parse("> 0.3.0, ,").is_err());
    }

    #[test]
    pub fn test_large_major_version() {
        assert!(range::parse("18446744073709551617.0.0").is_err());
    }

    #[test]
    pub fn test_large_minor_version() {
        assert!(range::parse("0.18446744073709551617.0").is_err());
    }

    #[test]
    pub fn test_large_patch_version() {
        assert!(range::parse("0.0.18446744073709551617").is_err());
    }

    #[test]
    pub fn test_op_partialord_lt() {
        let expect_less = Op::Ex;
        let other = Op::Gt;
        assert!(expect_less.lt(&other));
    }

    #[test]
    pub fn test_op_partialord_le() {
        let strictly_lt = Op::Ex;
        let other = Op::Lt;
        assert!(strictly_lt.le(&other));
        assert!(other.le(&other));
    }

    #[test]
    pub fn test_op_partialord_gt() {
        let expect_gt = Op::Compatible;
        let other = Op::GtEq;
        assert!(expect_gt.gt(&other));
    }

    #[test]
    pub fn test_op_partialord_ge() {
        let strictly_gt = Op::Compatible;
        let other = Op::Tilde;
        assert!(strictly_gt.ge(&other));
        assert!(other.ge(&other));
    }

    #[test]
    pub fn test_wildcard_partialord_lt() {
        let expect_less = WildcardVersion::Major;
        let other = WildcardVersion::Minor;
        assert!(expect_less.lt(&other));
    }


    #[test]
    pub fn test_wildcard_partialord_le() {
        let strictly_lt = WildcardVersion::Minor;
        let other = WildcardVersion::Patch;
        assert!(strictly_lt.le(&other));
        assert!(other.le(&other));
    }

    #[test]
    pub fn test_wildcard_partialord_gt() {
        let expect_greater = WildcardVersion::Patch;
        let other = WildcardVersion::Minor;
        assert!(expect_greater.gt(&other));
    }

    #[test]
    pub fn test_wildcard_partialord_ge() {
        let strictly_gt = WildcardVersion::Minor;
        let other = WildcardVersion::Major;
        assert!(strictly_gt.ge(&other));
        assert!(other.ge(&other));
    }
}
