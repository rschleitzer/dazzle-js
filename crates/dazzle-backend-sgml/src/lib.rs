//! SGML backend for Dazzle code generation
//!
//! This crate implements the `FotBuilder` trait for SGML-style output,
//! which is used for code generation rather than document formatting.
//!
//! ## Purpose
//!
//! Unlike OpenJade's document formatting backends (RTF, TeX, MIF, HTML),
//! the SGML backend is designed for **code generation**:
//!
//! - **`entity`**: Creates output files (e.g., generated Java classes)
//! - **`formatting-instruction`**: Appends text to the current output buffer
//!
//! This maps perfectly to code generation use cases where you want to
//! generate multiple source files from XML input.
//!
//! ## Architecture
//!
//! ```text
//! SgmlBackend
//!   ├─ output_dir: PathBuf (where to write files)
//!   ├─ current_buffer: String (collecting text for current file)
//!   └─ written_files: HashSet<PathBuf> (track what's been written)
//! ```
//!
//! ## Usage
//!
//! ```rust,ignore
//! use dazzle_backend_sgml::SgmlBackend;
//! use dazzle_core::fot::FotBuilder;
//!
//! let mut backend = SgmlBackend::new("output");
//!
//! // Append text to buffer
//! backend.formatting_instruction("public class Foo {\n")?;
//! backend.formatting_instruction("  // generated code\n")?;
//! backend.formatting_instruction("}\n")?;
//!
//! // Write buffer to file
//! backend.entity("src/Foo.java", &backend.current_output())?;
//! ```

use dazzle_core::fot::FotBuilder;
use std::collections::HashSet;
use std::fs;
use std::io::{Result, Write};
use std::path::{Path, PathBuf};

/// SGML backend for code generation
///
/// Implements the `FotBuilder` trait with support for:
/// - Creating output files (`entity`)
/// - Appending text to output buffer (`formatting_instruction`)
#[derive(Debug)]
pub struct SgmlBackend {
    /// Output directory (where files are written)
    output_dir: PathBuf,

    /// Current output buffer (text being accumulated)
    current_buffer: String,

    /// Set of files that have been written (to detect duplicates)
    written_files: HashSet<PathBuf>,
}

impl SgmlBackend {
    /// Create a new SGML backend
    ///
    /// # Arguments
    ///
    /// * `output_dir` - Directory where generated files will be written
    ///
    /// # Example
    ///
    /// ```rust
    /// use dazzle_backend_sgml::SgmlBackend;
    ///
    /// let backend = SgmlBackend::new("output");
    /// ```
    pub fn new<P: AsRef<Path>>(output_dir: P) -> Self {
        SgmlBackend {
            output_dir: output_dir.as_ref().to_path_buf(),
            current_buffer: String::new(),
            written_files: HashSet::new(),
        }
    }

    /// Get the list of files that have been written
    pub fn written_files(&self) -> &HashSet<PathBuf> {
        &self.written_files
    }
}

impl FotBuilder for SgmlBackend {
    /// Create an external entity (output file)
    ///
    /// Writes the provided content to a file in the output directory.
    ///
    /// # Arguments
    ///
    /// * `system_id` - Relative path for the output file (e.g., "src/Foo.java")
    /// * `content` - File contents to write
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use dazzle_backend_sgml::SgmlBackend;
    /// use dazzle_core::fot::FotBuilder;
    ///
    /// let mut backend = SgmlBackend::new("output");
    /// backend.entity("Foo.java", "public class Foo { }")?;
    /// # Ok::<(), std::io::Error>(())
    /// ```
    fn entity(&mut self, system_id: &str, content: &str) -> Result<()> {
        // Construct full output path
        let output_path = self.output_dir.join(system_id);

        // Create parent directories if needed
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        // Write file
        let mut file = fs::File::create(&output_path)?;
        file.write_all(content.as_bytes())?;

        // Track written file
        self.written_files.insert(output_path);

        Ok(())
    }

    /// Insert backend-specific formatting instruction
    ///
    /// Appends text to the current output buffer. This text can later
    /// be written to a file using `entity()`.
    ///
    /// # Arguments
    ///
    /// * `data` - Text to append
    ///
    /// # Example
    ///
    /// ```rust
    /// use dazzle_backend_sgml::SgmlBackend;
    /// use dazzle_core::fot::FotBuilder;
    ///
    /// let mut backend = SgmlBackend::new("output");
    /// backend.formatting_instruction("line 1\n")?;
    /// backend.formatting_instruction("line 2\n")?;
    ///
    /// assert_eq!(backend.current_output(), "line 1\nline 2\n");
    /// # Ok::<(), std::io::Error>(())
    /// ```
    fn formatting_instruction(&mut self, data: &str) -> Result<()> {
        self.current_buffer.push_str(data);
        Ok(())
    }

    /// Get the current output buffer contents
    ///
    /// This returns the accumulated text from `formatting_instruction` calls.
    fn current_output(&self) -> &str {
        &self.current_buffer
    }

    /// Clear the current output buffer
    ///
    /// Typically called after writing a file with `entity()`.
    fn clear_buffer(&mut self) {
        self.current_buffer.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_new_backend() {
        let backend = SgmlBackend::new("output");
        assert_eq!(backend.current_output(), "");
        assert_eq!(backend.written_files().len(), 0);
    }

    #[test]
    fn test_formatting_instruction() {
        let mut backend = SgmlBackend::new("output");

        backend.formatting_instruction("Hello ").unwrap();
        backend.formatting_instruction("World").unwrap();

        assert_eq!(backend.current_output(), "Hello World");
    }

    #[test]
    fn test_clear_buffer() {
        let mut backend = SgmlBackend::new("output");

        backend.formatting_instruction("Some text").unwrap();
        assert_eq!(backend.current_output(), "Some text");

        backend.clear_buffer();
        assert_eq!(backend.current_output(), "");
    }

    #[test]
    fn test_entity_creates_file() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        backend
            .entity("test.txt", "Hello, World!")
            .unwrap();

        let file_path = temp_dir.path().join("test.txt");
        assert!(file_path.exists());

        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "Hello, World!");
    }

    #[test]
    fn test_entity_creates_nested_directories() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        backend
            .entity("src/main/java/Foo.java", "public class Foo {}")
            .unwrap();

        let file_path = temp_dir.path().join("src/main/java/Foo.java");
        assert!(file_path.exists());
    }

    #[test]
    fn test_entity_tracks_written_files() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        backend.entity("file1.txt", "content1").unwrap();
        backend.entity("file2.txt", "content2").unwrap();

        assert_eq!(backend.written_files().len(), 2);
        assert!(backend
            .written_files()
            .contains(&temp_dir.path().join("file1.txt")));
        assert!(backend
            .written_files()
            .contains(&temp_dir.path().join("file2.txt")));
    }

    #[test]
    fn test_combined_workflow() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Build up content in buffer
        backend.formatting_instruction("public class Foo {\n").unwrap();
        backend.formatting_instruction("  // generated\n").unwrap();
        backend.formatting_instruction("}\n").unwrap();

        // Write buffer to file
        let content = backend.current_output().to_string();
        backend.entity("Foo.java", &content).unwrap();

        // Clear buffer for next file
        backend.clear_buffer();

        // Verify file was written
        let file_path = temp_dir.path().join("Foo.java");
        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "public class Foo {\n  // generated\n}\n");
    }
}
