use range::VersionReq;

/// A single range set combining a number of ranges with an or (`||`).
///
/// If any range in this set matches, the whole set matches.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Comparator {
    /// Set of ranges.
    pub ranges: Vec<VersionReq>,
}
