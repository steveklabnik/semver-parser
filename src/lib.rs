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
    pub pre: Option<Vec<String>>,
    pub build: Option<Vec<String>>,
}

pub fn parse_version(version: &str) -> Result<Version, Box<Error>> {
    let captures = match REGEX.captures(version.trim()) {
        Some(captures) => captures,
        None => return Err(From::from("Version did not parse properly.")),
    };

    let pre = captures.name("pre").map(|pre| {
        vec![pre.to_string()] 
    });

    let build = captures.name("build").map(|build| {
        vec![build.to_string()] 
    });

    Ok(Version {
        major: captures.name("major").unwrap().parse().unwrap(),
        minor: captures.name("minor").unwrap().parse().unwrap(),
        patch: captures.name("patch").unwrap().parse().unwrap(),
        pre: pre,
        build: build,
    })
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

        let expected_pre = Some(vec![String::from("pre")]);
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_version_prerelease_alphanumeric() {
        let version = "1.2.3-alpha1";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![String::from("alpha1")]);
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_version_basic_build() {
        let version = "1.2.3+build";

        let parsed = parse_version(version).unwrap();

        let expected_build = Some(vec![String::from("build")]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_build_alphanumeric() {
        let version = "1.2.3+build5";

        let parsed = parse_version(version).unwrap();

        let expected_build = Some(vec![String::from("build5")]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_pre_and_build() {
        let version = "1.2.3-alpha1+build5";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![String::from("alpha1")]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![String::from("build5")]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_complex_metadata_01() {
        let version = "1.2.3-1.alpha1.9+build5.7.3aedf  ";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![String::from("1.alpha1.9")]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![String::from("build5.7.3aedf")]);
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_version_complex_metadata_02() {
        let version = "0.4.0-beta.1+0851523";

        let parsed = parse_version(version).unwrap();

        let expected_pre = Some(vec![String::from("beta.1")]);
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = Some(vec![String::from("0851523")]);
        assert_eq!(expected_build, parsed.build);
    }
}
