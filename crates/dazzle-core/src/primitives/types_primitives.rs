//! DSSSL type primitives (mostly stubs)
//!
//! Implements ~30 type primitives as stubs returning sensible defaults.
//! These are NOT needed for plain text code generation.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all DSSSL type primitives (as stubs)
pub fn register_type_primitives(_engine: &mut SchemeEngine) -> Result<()> {
    // NOTE: These primitives are NOT needed for plain text code generation.
    // They would be needed for full DSSSL formatting (print/PDF output).
    //
    // When needed in the future, implement:
    // - Quantities: quantity?, table-unit, etc. → return defaults
    // - Colors: color?, color-space? → return false
    // - Addresses: address?, current-node-address → return dummies
    // - Glyphs: glyph-id?, glyph-subst-table → return dummies

    Ok(())
}
