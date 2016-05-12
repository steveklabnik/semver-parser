use regex::Regex;
use version::Identifier;

// by the time we get here, we know that it's all valid characters, so this doesn't need to return
// a result or anything
pub fn parse_meta(pre: &str) -> Vec<Identifier> {
    // Originally, I wanted to implement this method via calling parse, but parse is tolerant of
    // leading zeroes, and we want anything with leading zeroes to be considered alphanumeric, not
    // numeric. So the strategy is to check with a regex first, and then call parse once we've
    // determined that it's a number without a leading zero.
    let regex = Regex::new(r"^[1-9][0-9]*$").unwrap();

    pre.split(".")
        .map(|part| {
            // another wrinkle: we made sure that any number starts with a non-zero. But there's a
            // problem: an actual zero is a number, yet gets left out by this heuristic. So let's
            // also check for the single, lone zero.
            if regex.is_match(part) || part == "0" {
                // we can unwrap here because we know it is only digits due to the regex
                Identifier::Numeric(part.parse().unwrap())
            } else {
                Identifier::AlphaNumeric(part.to_string())
            }
        }).collect()
}

