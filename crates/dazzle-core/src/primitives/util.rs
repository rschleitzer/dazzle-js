//! Utility primitives
//!
//! Implements ~20 utility primitives: keywords, time, language, style, debug.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all utility primitives
pub fn register_util_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // Register the internal string-append implementation (binary)
    engine.register_fn("builtin-string-append-two", util_string_append_two);

    // Register string->list implementation
    engine.register_fn("builtin-string->list", util_string_to_list);

    // TODO: Implement other utility primitives when needed:
    // - load (complex due to Steel's evaluation model)
    // - keyword?, keyword->string, string->keyword
    // - time, time->string
    // - error, debug

    Ok(())
}

/// String concatenation for DSSSL compatibility (binary version)
/// Concatenates exactly two strings
fn util_string_append_two(s1: String, s2: String) -> String {
    format!("{}{}", s1, s2)
}

/// Convert string to list of characters (R5RS string->list)
fn util_string_to_list(s: String) -> Vec<char> {
    s.chars().collect()
}
