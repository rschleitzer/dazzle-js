//! DSSSL primitive implementations
//!
//! This module contains implementations of all ~134 DSSSL primitives
//! that Dazzle needs to implement (R5RS primitives are provided by Steel).
//!
//! ## Organization
//!
//! - `grove`: Grove query primitives (~50 functions)
//! - `processing`: Processing control primitives (~20 functions)
//! - `types`: DSSSL type primitives (~30 functions, mostly stubs)
//! - `util`: Utility primitives (~20 functions)

pub mod grove;
pub mod processing;
pub mod types_primitives;
pub mod util;

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all DSSSL primitives with the Scheme engine
pub fn register_all_primitives(engine: &mut SchemeEngine) -> Result<()> {
    grove::register_grove_primitives(engine)?;
    processing::register_processing_primitives(engine)?;
    types_primitives::register_type_primitives(engine)?;
    util::register_util_primitives(engine)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_primitives() {
        let mut engine = SchemeEngine::new().unwrap();
        register_all_primitives(&mut engine).unwrap();
    }
}
