extern crate regex;

#[macro_use]
extern crate lazy_static;

use regex::Regex;

lazy_static! {
    static ref REGEX: Regex = {
        // a numeric identifier is either zero or multiple numbers without a leading zero
        let numeric_identifier = r"0|(:?[1-9][0-9]*)";

        let major = numeric_identifier;
        let minor = numeric_identifier;
        let patch = numeric_identifier;

        let letters_numbers_dash_dot = r"[.-A-Za-z0-9]+";

        // This regex does not fully parse prereleases, just extracts the whole prerelease string.
        // parse_version() will parse this further.
        let pre = letters_numbers_dash_dot;
        
        // This regex does not fully parse builds, just extracts the whole build string.
        // parse_version() will parse this further.
        let build = letters_numbers_dash_dot;

        let regex = format!(r"^(?x) # heck yes x mode
            (?P<major>{})           # major version
            \.                      # dot
            (?P<minor>{})           # minor version
            \.                      # dot
            (?P<patch>{})           # patch version
            (:?-(?P<pre>{}))?       # optional prerelease version
            (:?\+(?P<build>{}))?    # optional build metadata
            $",
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

pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: Option<Vec<Identifier>>,
    pub build: Option<Vec<Identifier>>,
}

#[derive(Debug,PartialEq)]
pub enum Identifier {
    /// An identifier that's solely numbers.
    Numeric(u64),
    /// An identifier with letters and numbers.
    AlphaNumeric(String),
}

pub fn parse_version(version: &str) -> Result<Version, String> {
    let captures = match REGEX.captures(version.trim()) {
        Some(captures) => captures,
        None => return Err(From::from("Version did not parse properly.")),
    };

    let pre = captures.name("pre").map(parse_meta);

    let build = captures.name("build").map(parse_meta);

    Ok(Version {
        major: captures.name("major").unwrap().parse().unwrap(),
        minor: captures.name("minor").unwrap().parse().unwrap(),
        patch: captures.name("patch").unwrap().parse().unwrap(),
        pre: pre,
        build: build,
    })
}

// by the time we get here, we know that it's all valid characters, so this doesn't need to return
// a result or anything
fn parse_meta(pre: &str) -> Vec<Identifier> {
    // Originally, I wanted to implement this method via calling parse, but parse is tolerant of
    // leading zeroes, and we want anything with leading zeroes to be considered alphanumeric, not
    // numeric. So the strategy is to check with a regex first, and then call parse once we've
    // determined that it's a number without a leading zero.
    let regex = Regex::new(r"^[1-9][0-9]*$").unwrap();

    pre.split(".")
        .map(|part| {
            if regex.is_match(part) {
                // we can unwrap here because we know it is only digits due to the regex
                Identifier::Numeric(part.parse().unwrap())
            } else {
                Identifier::AlphaNumeric(part.to_string())
            }
        }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty() {
        let version = "";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), "empty string incorrectly considered a valid parse");
    }

    #[test]
    fn parse_blank() {
        let version = "  ";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), "blank string incorrectly considered a valid parse");
    }

    #[test]
    fn parse_no_minor_patch() {
        let version = "1";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), format!("'{}' incorrectly considered a valid parse", version));
    }

    #[test]
    fn parse_no_patch() {
        let version = "1.2";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), format!("'{}' incorrectly considered a valid parse", version));
    }

    #[test]
    fn parse_empty_pre() {
        let version = "1.2.3-";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), format!("'{}' incorrectly considered a valid parse", version));
    }

    #[test]
    fn parse_letters() {
        let version = "a.b.c";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), format!("'{}' incorrectly considered a valid parse", version));
    }

    #[test]
    fn parse_version_with_letters() {
        let version = "1.2.3 a.b.c";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), format!("'{}' incorrectly considered a valid parse", version));
    }

    #[test]
    fn parse_basic_version() {
        let version = "1.2.3";

        let parsed = parse_version(version).unwrap();

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);
    }

    #[test]
    fn parse_trims_input() {
        let version = "  1.2.3  ";

        let parsed = parse_version(version).unwrap();

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);
    }

    #[test]
    fn parse_version_no_major_leading_zeroes() {
        let version = "01.0.0";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), "01 incorrectly considered a valid major version");
    }

    #[test]
    fn parse_version_no_minor_leading_zeroes() {
        let version = "0.01.0";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), "01 incorrectly considered a valid minor version");
    }

    #[test]
    fn parse_version_no_patch_leading_zeroes() {
        let version = "0.0.01";

        let parsed = parse_version(version);

        assert!(parsed.is_err(), "01 incorrectly considered a valid patch version");
    }

    #[test]
    fn parse_version_basic_prerelease() {
        let version = "1.2.3-pre";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![Identifier::AlphaNumeric(String::from("pre"))]);
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_version_prerelease_alphanumeric() {
        let version = "1.2.3-alpha1";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![Identifier::AlphaNumeric(String::from("alpha1"))]);
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_version_basic_build() {
        let version = "1.2.3+build";

        let parsed = parse_version(version).unwrap();

        let expected_build = Some(vec![Identifier::AlphaNumeric(String::from("build"))]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_build_alphanumeric() {
        let version = "1.2.3+build5";

        let parsed = parse_version(version).unwrap();

        let expected_build = Some(vec![Identifier::AlphaNumeric(String::from("build5"))]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_pre_and_build() {
        let version = "1.2.3-alpha1+build5";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![Identifier::AlphaNumeric(String::from("alpha1"))]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![Identifier::AlphaNumeric(String::from("build5"))]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_complex_metadata_01() {
        let version = "1.2.3-1.alpha1.9+build5.7.3aedf  ";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![Identifier::Numeric(1),
                                     Identifier::AlphaNumeric(String::from("alpha1")),
                                     Identifier::Numeric(9)]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![Identifier::AlphaNumeric(String::from("build5")),
                                       Identifier::Numeric(7),
                                       Identifier::AlphaNumeric(String::from("3aedf"))]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_complex_metadata_02() {
        let version = "0.4.0-beta.1+0851523";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![Identifier::AlphaNumeric(String::from("beta")),
                                     Identifier::Numeric(1)]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![Identifier::AlphaNumeric(String::from("0851523"))]);
        assert_eq!(expected_build, parsed.build);
    }
}
