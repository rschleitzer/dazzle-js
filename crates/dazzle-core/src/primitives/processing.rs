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
use anyhow::{Context, Result};
use steel::rvals::Custom;

/// A SOSOFO (Specification of a Sequence of Flow Objects)
///
/// For our text backend, this is simplified to just accumulated text.
/// In full DSSSL, this would be a complex tree of flow objects.
#[derive(Debug, Clone)]
pub struct Sosofo {
    /// The text content of this sosofo
    text: String,
    /// Optional output filename (for entity flow objects)
    output_file: Option<String>,
}

impl Sosofo {
    /// Create a new sosofo with the given text
    pub fn new(text: String) -> Self {
        Sosofo {
            text,
            output_file: None,
        }
    }

    /// Create a new sosofo with text and output file
    pub fn with_file(text: String, filename: String) -> Self {
        Sosofo {
            text,
            output_file: Some(filename),
        }
    }

    /// Create an empty sosofo
    pub fn empty() -> Self {
        Sosofo {
            text: String::new(),
            output_file: None,
        }
    }

    /// Get the text content
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Get the output filename if this is an entity
    pub fn output_file(&self) -> Option<&str> {
        self.output_file.as_deref()
    }

    /// Append another sosofo
    pub fn append(&self, other: &Sosofo) -> Sosofo {
        Sosofo {
            text: format!("{}{}", self.text, other.text),
            output_file: self.output_file.clone().or_else(|| other.output_file.clone()),
        }
    }

    /// Write this sosofo to its output file (if it has one)
    pub fn write_to_file(&self) -> Result<()> {
        if let Some(filename) = &self.output_file {
            use std::fs;
            use std::path::Path;

            // Create parent directories if needed
            if let Some(parent) = Path::new(filename).parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
            }

            // Write the file
            fs::write(filename, &self.text)
                .with_context(|| format!("Failed to write file: {}", filename))?;

            Ok(())
        } else {
            Ok(()) // No file to write
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
    engine.register_fn("sosofo-append-two", processing_sosofo_append_two);

    // Flow object creation
    // Note: make is a keyword in DSSSL, but we'll handle it as a function
    // The real DSSSL `make` takes keyword arguments, but Steel doesn't support that directly
    // So we'll provide specific constructors
    engine.register_fn("make-entity", processing_make_entity);
    engine.register_fn("make-formatting-instruction", processing_make_formatting_instruction);

    // File writing
    engine.register_fn("write-sosofo", processing_write_sosofo);

    // TODO: Context-dependent processing
    // - process-children (requires processing context)
    // - process-node-list (requires processing context)

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

/// Append two sosofos (will be wrapped in Scheme for variadic support)
fn processing_sosofo_append_two(s1: &Sosofo, s2: &Sosofo) -> Sosofo {
    s1.append(s2)
}

// ============================================================================
// Flow Object Primitives
// ============================================================================

/// Create an entity flow object (outputs to a file)
/// In OpenJade: (make entity system-id: "filename" (literal "content"))
/// For us: (make-entity "filename" sosofo)
fn processing_make_entity(system_id: String, content: &Sosofo) -> Sosofo {
    // Create a sosofo with the filename attached
    Sosofo::with_file(content.text().to_string(), system_id)
}

/// Create a formatting-instruction flow object (plain text output)
/// In OpenJade: (make formatting-instruction data: "text")
/// For us: (make-formatting-instruction "text") or just use literal
fn processing_make_formatting_instruction(data: String) -> Sosofo {
    // For plain text output, this is just like literal
    Sosofo::new(data)
}

/// Write a sosofo to its associated file
/// Usage: (write-sosofo (make-entity "output.txt" (literal "content")))
fn processing_write_sosofo(sosofo: &Sosofo) -> Result<bool, String> {
    sosofo
        .write_to_file()
        .map(|_| true)
        .map_err(|e| format!("Failed to write sosofo: {}", e))
}
