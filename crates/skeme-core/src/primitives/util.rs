//! Utility primitives
//!
//! Implements ~20 utility primitives: keywords, time, language, style, debug.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all utility primitives
pub fn register_util_primitives(_engine: &mut SchemeEngine) -> Result<()> {
    // TODO: Implement utility primitives when needed:
    // - keyword?, keyword->string, string->keyword
    // - time, time->string
    // - error, debug
    // - get-variable (CLI variables)
    //
    // For Phase 1 MVP, these are not critical.

    Ok(())
}
