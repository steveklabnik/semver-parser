extern crate regex;

use regex::Regex;

pub struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

pub fn parse_version(version: &str) -> Version {
    let re = Regex::new(r"(\d+).(\d+).(\d+)").unwrap();

    let captures = re.captures(version).unwrap();

    Version {
        major: captures.at(1).unwrap().parse().unwrap(),
        minor: captures.at(2).unwrap().parse().unwrap(),
        patch: captures.at(3).unwrap().parse().unwrap(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_version_core_01() {
        let version = "1.2.3";

        let parsed = parse_version(version);

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);
    }
}
