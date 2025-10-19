//! SGML backend for Dazzle
//!
//! This backend implements code generation by writing files to disk.
//!
//! Corresponds to OpenJade's `jade/SgmlFOTBuilder.cxx` (simplified).
//!
//! ## Flow Objects Supported
//!
//! - `entity`: Create output file
//! - `formatting-instruction`: Append text to current file
//!
//! All other flow objects return errors (not needed for code generation).
//!
//! ## Usage
//!
//! ```ignore
//! use dazzle_backend_sgml::SgmlBackend;
//! use dazzle_core::FotBuilder;
//!
//! let mut backend = SgmlBackend::new();
//! backend.entity("Output.java", "public class Foo {}")?;
//! ```

// Placeholder - will implement in Phase 4
// For now, just ensure it compiles

#[cfg(test)]
mod tests {
    #[test]
    fn test_placeholder() {
        // Backend implementation will come in Phase 4
    }
}
