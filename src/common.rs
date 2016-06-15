use regex::Regex;
use version::Identifier;

// by the time we get here, we know that it's all valid characters, so this doesn't need to return
// a result or anything
pub fn parse_meta(s: &str) -> Vec<Identifier> {
    // Originally, I wanted to implement this method via calling parse, but parse is tolerant of
    // leading zeroes, and we want anything with leading zeroes to be considered alphanumeric, not
    // numeric. So the strategy is to check with a regex first, and then call parse once we've
    // determined that it's a number without a leading zero.
    s.split(".")
        .map(|part| {
            // another wrinkle: we made sure that any number starts with a
            // non-zero. But there's a problem: an actual zero is a number, yet
            // gets left out by this heuristic. So let's also check for the
            // single, lone zero.
            if is_alpha_numeric(part) {
                Identifier::AlphaNumeric(part.to_string())
            } else {
                // we can unwrap here because we know it is only digits due to the regex
                Identifier::Numeric(part.parse().unwrap())
            }
        }).collect()
}

pub fn is_alpha_numeric(s: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^(0|[1-9][0-9]*)$").unwrap();
    };
    !REGEX.is_match(s)
}
