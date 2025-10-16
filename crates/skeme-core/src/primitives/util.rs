//! Utility primitives
//!
//! Implements ~20 utility primitives: keywords, time, language, style, debug.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all utility primitives
pub fn register_util_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // Register the internal string-append implementation
    engine.register_fn("builtin-string-append-impl", util_string_append_impl);

    // TODO: Implement other utility primitives when needed:
    // - load (complex due to Steel's evaluation model)
    // - keyword?, keyword->string, string->keyword
    // - time, time->string
    // - error, debug

    Ok(())
}

/// String concatenation for DSSSL compatibility
/// Just concatenates all strings in the vector
fn util_string_append_impl(args: Vec<String>) -> String {
    args.into_iter().collect()
}
