//! Utility primitives
//!
//! Implements ~20 utility primitives: keywords, time, language, style, debug.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all utility primitives
pub fn register_util_primitives(_engine: &mut SchemeEngine) -> Result<()> {
    // TODO: Implement utility primitives when needed:
    // - load (complex due to Steel's evaluation model)
    // - keyword?, keyword->string, string->keyword
    // - time, time->string
    // - error, debug

    Ok(())
}
