//! Processing control primitives
//!
//! Implements ~20 primitives for template processing control.
//!
//! Key primitives:
//! - process-children, process-children-trim
//! - process-node-list, process-element-with-id
//! - next-match
//! - literal, sosofo-append, empty-sosofo

use crate::scheme::SchemeEngine;
use anyhow::Result;
use steel::rvals::Custom;
use steel::steel_vm::register_fn::RegisterFn;

/// A SOSOFO (Specification of a Sequence of Flow Objects)
///
/// For our text backend, this is simplified to just accumulated text.
/// In full DSSSL, this would be a complex tree of flow objects.
#[derive(Debug, Clone)]
pub struct Sosofo {
    /// The text content of this sosofo
    text: String,
}

impl Sosofo {
    /// Create a new sosofo with the given text
    pub fn new(text: String) -> Self {
        Sosofo { text }
    }

    /// Create an empty sosofo
    pub fn empty() -> Self {
        Sosofo {
            text: String::new(),
        }
    }

    /// Get the text content
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Append another sosofo
    pub fn append(&self, other: &Sosofo) -> Sosofo {
        Sosofo {
            text: format!("{}{}", self.text, other.text),
        }
    }
}

// Implement Steel's Custom trait for Sosofo
impl Custom for Sosofo {}

/// Register all processing control primitives
pub fn register_processing_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // SOSOFO constructors
    engine.register_fn("literal", processing_literal);
    engine.register_fn("empty-sosofo", processing_empty_sosofo);
    engine.register_fn("sosofo-append", processing_sosofo_append);

    // TODO: Context-dependent processing
    // - process-children (requires processing context)
    // - process-node-list (requires processing context)
    // - make (flow object creation)

    Ok(())
}

// ============================================================================
// SOSOFO Primitives
// ============================================================================

/// Create a sosofo with literal text
fn processing_literal(text: String) -> Sosofo {
    Sosofo::new(text)
}

/// Create an empty sosofo
fn processing_empty_sosofo() -> Sosofo {
    Sosofo::empty()
}

/// Append two sosofos
fn processing_sosofo_append(a: &Sosofo, b: &Sosofo) -> Sosofo {
    a.append(b)
}
