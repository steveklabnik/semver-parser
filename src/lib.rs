extern crate regex;

#[macro_use]
extern crate lazy_static;

use regex::Regex;

use std::error::Error;

lazy_static! {
    static ref REGEX: Regex = {
        // a numeric identifier is either zero or multiple numbers without a leading zero
        let numeric_identifier = r"0|(:?[1-9][0-9]*)";

        let major = numeric_identifier;
        let minor = numeric_identifier;
        let patch = numeric_identifier;
        
        let pre = r"\w+";

        let regex = format!(r"^(?P<major>{})\.(?P<minor>{})\.(?P<patch>{})(:?-(?P<pre>{}))?$", major, minor, patch, pre);
        println!("{}", regex);
        let regex = Regex::new(&regex);
        
        // this unwrap is okay because everything above here is const, so this will never fail.
        regex.unwrap()
    };
}

pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: Option<String>,
}

pub fn parse_version(version: &str) -> Result<Version, Box<Error>> {
    let captures = match REGEX.captures(version) {
        Some(captures) => captures,
        None => return Err(From::from("Version did not parse properly.")),
    };

    Ok(Version {
        major: captures.name("major").unwrap().parse().unwrap(),
        minor: captures.name("minor").unwrap().parse().unwrap(),
        patch: captures.name("patch").unwrap().parse().unwrap(),
        pre: captures.name("pre").map(ToString::to_string)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_version_core() {
        let version = "1.2.3";

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

        assert_eq!(Some(String::from("pre")), parsed.pre);
    }
}
