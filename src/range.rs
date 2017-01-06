use regex::Regex;
use common;
use version::Identifier;
use std::str::FromStr;
use std::error::Error;
use std::num::ParseIntError;

lazy_static! {
    static ref REGEX: Regex = {
        // an operation can be:
        //
        // * =
        // * >
        // * >=
        // * <
        // * <=
        // * ~
        // * ^
        let operation = r"=|>|>=|<|<=|~|\^";

        // a numeric identifier is either zero or multiple numbers without a leading zero
        let numeric_identifier = r"0|[1-9][0-9]*";

        let major = numeric_identifier;

        // minor can be either a number or a wildcard. *, x, and X are wildcards.
        let minor = format!(r"{}|\*|[xX]", numeric_identifier);

        // patch can be either a number or a wildcard. *, x, and X are wildcards.
        let patch = format!(r"{}|\*|[xX]", numeric_identifier);

        let letters_numbers_dash_dot = r"[-.A-Za-z0-9]+";

        // This regex does not fully parse prereleases, just extracts the whole prerelease string.
        // parse_version() will parse this further.
        let pre = letters_numbers_dash_dot;

        // This regex does not fully parse builds, just extracts the whole build string.
        // parse_version() will parse this further.
        let build = letters_numbers_dash_dot;

        let regex = format!(r"(?x) # heck yes x mode
            ^\s*                    # leading whitespace
            (?P<operation>{})?\s*   # optional operation
            (?P<major>{})           # major version
            (?:\.(?P<minor>{}))?    # optional dot and then minor
            (?:\.(?P<patch>{}))?    # optional dot and then patch
            (?:-(?P<pre>{}))?       # optional prerelease version
            (?:\+(?P<build>{}))?    # optional build metadata
            \s*$                    # trailing whitespace
            ",
            operation,
            major,
            minor,
            patch,
            pre,
            build);
        let regex = Regex::new(&regex);

        // this unwrap is okay because everything above here is const, so this will never fail.
        regex.unwrap()
    };
}

#[derive(Debug)]
pub struct VersionReq {
    pub predicates: Vec<Predicate>,
}

#[derive(PartialEq,Debug)]
pub enum WildcardVersion {
    Major,
    Minor,
    Patch,
}

#[derive(PartialEq,Debug)]
pub enum Op {
    Ex, // Exact
    Gt, // Greater than
    GtEq, // Greater than or equal to
    Lt, // Less than
    LtEq, // Less than or equal to
    Tilde, // e.g. ~1.0.0
    Compatible, // compatible by definition of semver, indicated by ^
    Wildcard(WildcardVersion), // x.y.*, x.*, *
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

#[derive(PartialEq,Debug)]
pub struct Predicate {
    pub op: Op,
    pub major: u64,
    pub minor: Option<u64>,
    pub patch: Option<u64>,
    pub pre: Vec<Identifier>,
}

pub fn parse(ranges: &str) -> Result<VersionReq, String> {
    // null is an error
    if ranges == "\0" {
        return Err(String::from("Null is not a valid VersionReq"));
    }

    // an empty range is a major version wildcard
    // so is a lone * or x of either capitalization
    if (ranges == "")
    || (ranges == "*")
    || (ranges == "x")
    || (ranges == "X") {
        return Ok(VersionReq {
            predicates: vec![Predicate {
                op: Op::Wildcard(WildcardVersion::Major),
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            }],
        });
    }


    let ranges = ranges.trim();

    let predicates: Result<Vec<_>, String> = ranges
        .split(",")
        .map(|range| {
            parse_predicate(range)
        })
        .collect();

    let predicates = try!(predicates);

    if predicates.len() == 0 {
        return Err(String::from("VersionReq did not parse properly"));
    }

    Ok(VersionReq {
        predicates: predicates,
    })
}

pub fn parse_predicate(range: &str) -> Result<Predicate, String> {
    let captures = match REGEX.captures(range.trim()) {
        Some(captures) => captures,
        None => return Err(From::from("VersionReq did not parse properly.")),
    };

    // operations default to Compatible
    // unwrap is okay because we validate that we only have correct strings in the regex
    let mut operation = captures.name("operation")
                                .map(str::parse)
                                .map(Result::unwrap)
                                .unwrap_or(Op::Compatible);

    // first unwrap is okay because we always have major
    let major: Result<_, ParseIntError> = captures.name("major")
                        .unwrap()
                        .parse();

    let major = match major {
                            Ok(number) => number,
                            Err(err) => return Err("Error parsing major version number: ".to_string() + err.description())
                };

    let minor = captures.name("minor");

    // oh my what have I done? This code is gross.
    let minor = if minor.is_some() {
        let minor = minor.unwrap();
        match minor.parse() {
            Ok(number) => Some(number),
            Err(_) => {
                // if we get an error, it's because it's a wildcard
                operation = Op::Wildcard(WildcardVersion::Minor);

                None
            },
        }
    } else {
        None
    };

    let patch = captures.name("patch");

    // oh my what have I done? This code is gross.
    let patch = if patch.is_some() {
        let patch = patch.unwrap();
        match patch.parse() {
            Ok(number) => Some(number),
            Err(_) => {
                // if we get an error, it's because it's a wildcard
                operation = Op::Wildcard(WildcardVersion::Patch);

                None
            },
        }
    } else {
        None
    };

    let pre = captures.name("pre").map(common::parse_meta).unwrap_or_else(Vec::new);

    Ok(Predicate {
        op: operation,
        major: major,
        minor: minor,
        patch: patch,
        pre: pre,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use range;
    use version::Identifier;

    #[test]
    fn test_parsing_default() {
        let r = range::parse("1.0.0").unwrap();

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::Ex,
                major: 0,
                minor: Some(1),
                patch: Some(0),
                pre: vec![Identifier::AlphaNumeric(String::from("beta2")),
                          Identifier::AlphaNumeric(String::from("a"))],
            },
            r.predicates[0]
        );
    }

    #[test]
    pub fn test_parsing_greater_than() {
        let r = range::parse("> 1.0.0").unwrap();

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::Wildcard(WildcardVersion::Major),
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_wildcard() {
        let r = range::parse("*").unwrap();

        assert_eq!(Predicate {
                op: Op::Wildcard(WildcardVersion::Major),
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_x() {
        let r = range::parse("x").unwrap();

        assert_eq!(Predicate {
                op: Op::Wildcard(WildcardVersion::Major),
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_capital_x() {
        let r = range::parse("X").unwrap();

        assert_eq!(Predicate {
                op: Op::Wildcard(WildcardVersion::Major),
                major: 0,
                minor: None,
                patch: None,
                pre: Vec::new(),
            },
            r.predicates[0]
        );
    }

    #[test]
    fn test_parsing_minor_wildcard_star() {
        let r = range::parse("1.*").unwrap();

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::Gt,
                major: 0,
                minor: Some(0),
                patch: Some(9),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(3),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::LtEq,
                major: 0,
                minor: Some(2),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(1),
                patch: Some(0),
                pre: Vec::new(),
            },
            r.predicates[0]
        );

        assert_eq!(Predicate {
                op: Op::Compatible,
                major: 0,
                minor: Some(1),
                patch: Some(4),
                pre: Vec::new(),
            },
            r.predicates[1]
        );

        assert_eq!(Predicate {
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

        assert_eq!(Predicate {
                op: Op::GtEq,
                major: 0,
                minor: Some(5),
                patch: Some(1),
                pre: vec![Identifier::AlphaNumeric(String::from("alpha3"))],
            },
            r.predicates[0]
        );

        assert_eq!(Predicate {
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
        assert_eq!(range::parse("^1.2.3+meta").unwrap().predicates[0].op,
                   Op::Compatible);
        assert_eq!(range::parse("~1.2.3+meta").unwrap().predicates[0].op,
                   Op::Tilde);
        assert_eq!(range::parse("=1.2.3+meta").unwrap().predicates[0].op,
                   Op::Ex);
        assert_eq!(range::parse("<=1.2.3+meta").unwrap().predicates[0].op,
                   Op::LtEq);
        assert_eq!(range::parse(">=1.2.3+meta").unwrap().predicates[0].op,
                   Op::GtEq);
        assert_eq!(range::parse("<1.2.3+meta").unwrap().predicates[0].op,
                   Op::Lt);
        assert_eq!(range::parse(">1.2.3+meta").unwrap().predicates[0].op,
                   Op::Gt);
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
    pub fn test_large_version() {
        assert!(range::parse("18446744073709551617.0.0").is_err());
    }

}
