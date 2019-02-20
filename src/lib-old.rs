//! Collection of structures and helper functions for parsing semantic version.
//!
//! This crate contains data structures for holding version data and comparison of versions
//! according to [Semantic versioning](http://semver.org/).
//! Main structs are [`version::Version`] representing version itself and [`range::VersionReq`] as a
//! collection of [`range::Predicate`]
//! representing data for version comparison.
//!
//! # Examples
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
//! [`version::Version`]: ./version/struct.Version.html
//! [`range::Predicate`]: ./range/struct.Predicate.html
//! [`range::VersionReq`]: ./range/struct.VersionReq.html

#![doc(html_root_url = "https://docs.rs/semver-parser/0.9.0")]

pub mod comparator;
pub mod lexer;
pub mod parser;
pub mod range;
pub mod version;
