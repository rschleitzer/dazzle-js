//! Flow Object Tree (FOT) builder trait
//!
//! This module defines the abstract interface for backend output generation,
//! following OpenJade's FOTBuilder architecture.
//!
//! ## Architecture
//!
//! Like OpenJade's separation between the DSSSL engine and FOT builders,
//! Dazzle defines:
//!
//! - **This module (`dazzle-core/fot`)**: Abstract `FotBuilder` trait
//! - **`dazzle-backend-sgml`**: SGML/code generation backend (file I/O)
//! - **`dazzle-backend-rtf` (future)**: RTF document formatting
//! - **`dazzle-backend-tex` (future)**: TeX document formatting
//! - **`dazzle-backend-html` (future)**: HTML output
//!
//! This allows the same Scheme interpreter to target different output formats.
//!
//! ## Flow Objects
//!
//! DSSSL defines "flow objects" as formatting primitives. Examples:
//!
//! - `sequence`: Container for other flow objects
//! - `paragraph`: Block-level text
//! - `display-group`: Display grouping
//! - **`entity`**: External entity reference (used for file output)
//! - **`formatting-instruction`**: Backend-specific formatting (used for raw text output)
//!
//! For code generation (SGML backend), we only need `entity` and `formatting-instruction`.
//! Document formatting backends need the full set.

use std::fmt::Debug;
use std::io::Result;

/// Flow Object Tree builder
///
/// Corresponds to OpenJade's `FOTBuilder` class hierarchy.
///
/// ## Code Generation vs. Document Formatting
///
/// **Code generation** (SGML backend) only needs:
/// - `entity()` - Create output file
/// - `formatting_instruction()` - Write text to current file
///
/// **Document formatting** (RTF/TeX/HTML backends) needs:
/// - All the paragraph/sequence/display primitives
/// - Character formatting (bold, italic, font changes)
/// - Page layout primitives
///
/// Implementations can return `Err` for unsupported flow objects.
pub trait FotBuilder: Debug {
    // ============================================================================
    // Code Generation Primitives (SGML backend)
    // ============================================================================

    /// Create an external entity (output file)
    ///
    /// **Usage**: Write generated code to a file.
    ///
    /// **Arguments**:
    /// - `system_id`: File path (e.g., "src/generated/Model.java")
    /// - `content`: File contents
    ///
    /// **DSSSL**: `entity` flow object
    ///
    /// **Example**:
    /// ```scheme
    /// (make entity
    ///   system-id: "Output.java"
    ///   (literal "public class Foo { }"))
    /// ```
    fn entity(&mut self, system_id: &str, content: &str) -> Result<()>;

    /// Insert backend-specific formatting instruction
    ///
    /// **Usage**: Append text to the current output.
    ///
    /// **DSSSL**: `formatting-instruction` flow object
    ///
    /// **Example**:
    /// ```scheme
    /// (make formatting-instruction
    ///   data: "System.out.println(\"Hello\");")
    /// ```
    fn formatting_instruction(&mut self, data: &str) -> Result<()>;

    /// Create a directory
    ///
    /// **Usage**: Create a directory path (and all parent directories if needed).
    ///
    /// **Arguments**:
    /// - `path`: Directory path to create (e.g., "src/generated/models")
    ///
    /// **DSSSL**: `directory` flow object (Dazzle extension)
    ///
    /// **Example**:
    /// ```scheme
    /// (make directory
    ///   path: "src/generated/models")
    /// ```
    fn directory(&mut self, path: &str) -> Result<()>;

    /// Get the current output buffer contents
    ///
    /// **Usage**: Retrieve accumulated text from `formatting_instruction` calls.
    ///
    /// This is used by the `entity` flow object to get buffer content before
    /// writing to a file.
    fn current_output(&self) -> &str;

    /// Clear the current output buffer
    ///
    /// **Usage**: Reset the buffer after writing an entity.
    ///
    /// Typically called after `entity()` writes a file.
    fn clear_buffer(&mut self);

    /// Get the current directory context
    ///
    /// **Usage**: Returns the directory that relative entity paths are resolved against.
    ///
    /// Returns `None` if no current directory is set (paths are relative to output_dir).
    fn current_directory(&self) -> Option<&str> {
        None
    }

    /// Set the current directory context
    ///
    /// **Usage**: Sets the directory that subsequent entity paths are resolved against.
    ///
    /// **Arguments**:
    /// - `path`: Directory path (relative to output_dir, or None to reset)
    ///
    /// This is automatically called by the `directory` flow object.
    fn set_current_directory(&mut self, _path: Option<String>) {
        // Default implementation does nothing (backends can override)
    }

    // ============================================================================
    // Document Formatting Primitives (future backends)
    // ============================================================================
    // These methods have default implementations that return errors,
    // since the SGML backend doesn't need them.

    /// Start a sequence (container for flow objects)
    ///
    /// **DSSSL**: `sequence` flow object
    fn start_sequence(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "sequence flow object not supported by this backend",
        ))
    }

    /// End a sequence
    fn end_sequence(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "sequence flow object not supported by this backend",
        ))
    }

    /// Start a paragraph
    ///
    /// **DSSSL**: `paragraph` flow object
    fn start_paragraph(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "paragraph flow object not supported by this backend",
        ))
    }

    /// End a paragraph
    fn end_paragraph(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "paragraph flow object not supported by this backend",
        ))
    }

    /// Insert literal text
    ///
    /// **DSSSL**: `literal` (text content within flow objects)
    ///
    /// **Note**: This is different from `formatting_instruction`.
    /// `literal` is for text within document formatting context,
    /// while `formatting_instruction` is for raw backend-specific output.
    fn literal(&mut self, _text: &str) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "literal not supported by this backend",
        ))
    }

    /// Start a display group
    ///
    /// **DSSSL**: `display-group` flow object
    fn start_display_group(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "display-group flow object not supported by this backend",
        ))
    }

    /// End a display group
    fn end_display_group(&mut self) -> Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "display-group flow object not supported by this backend",
        ))
    }

    // More flow objects (line-field, paragraph-break, etc.) would go here
    // when implementing document formatting backends.
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_trait_defined() {
        // This test just ensures the trait compiles
        // Actual tests will be in backend implementations
    }
}
