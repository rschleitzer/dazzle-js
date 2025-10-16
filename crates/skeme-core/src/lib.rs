//! # Skeme Core
//!
//! Core library for Skeme: Steel Scheme interpreter integration,
//! libxml2 XML parsing, and DSSSL primitive implementations.
//!
//! ## Architecture
//!
//! - `xml`: XML parsing and grove model (libxml2 integration)
//! - `scheme`: Steel Scheme interpreter setup and integration
//! - `primitives`: DSSSL primitive function implementations
//! - `grove`: Grove node representation and navigation
//! - `types`: DSSSL type system (quantities, colors, addresses, etc.)

pub mod grove;
pub mod primitives;
pub mod scheme;
pub mod types;
pub mod xml;

// Re-export key types for convenience
pub use grove::{Grove, Node, NodeList};
pub use scheme::SchemeEngine;
pub use xml::XmlParser;

/// Skeme version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }
}
