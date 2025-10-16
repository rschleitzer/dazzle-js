//! # Skeme Template
//!
//! Template engine for Skeme: handles template loading, processing control,
//! and file I/O operations.
//!
//! ## Modules
//!
//! - `loader`: Template file loading and search path resolution
//! - `processor`: Template processing and execution
//! - `io`: File I/O primitives (`write-file`, `ensure-dir`)

pub mod io;
pub mod loader;
pub mod processor;

// Re-export key types
pub use loader::TemplateLoader;
pub use processor::TemplateProcessor;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_availability() {
        // Basic smoke test
    }
}
