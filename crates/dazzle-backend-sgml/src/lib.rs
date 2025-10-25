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
/// - Creating directories and setting the current directory context
#[derive(Debug)]
pub struct SgmlBackend {
    /// Output directory (where files are written)
    output_dir: PathBuf,

    /// Current directory context (for relative paths)
    /// When set, entity paths are resolved relative to this directory
    current_dir: Option<PathBuf>,

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
            current_dir: None,
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
    /// If a current directory is set (via `directory` flow object), relative paths
    /// are resolved against it.
    ///
    /// # Arguments
    ///
    /// * `system_id` - Relative path for the output file (e.g., "src/Foo.java" or "Foo.java")
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
        // Resolve path relative to current directory if set
        let relative_path = if let Some(ref current_dir) = self.current_dir {
            current_dir.join(system_id)
        } else {
            PathBuf::from(system_id)
        };

        // Construct full output path
        let output_path = self.output_dir.join(relative_path);

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

    /// Create a directory
    ///
    /// Creates the directory and all necessary parent directories.
    /// Also sets this directory as the current directory context for subsequent
    /// entity and directory operations.
    ///
    /// If a current directory is set, relative paths are resolved against it.
    ///
    /// # Arguments
    ///
    /// * `path` - Directory path to create (relative to current_dir or output_dir)
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use dazzle_backend_sgml::SgmlBackend;
    /// use dazzle_core::fot::FotBuilder;
    ///
    /// let mut backend = SgmlBackend::new("output");
    /// backend.directory("src/generated")?;
    /// backend.directory("models")?;  // Creates src/generated/models
    /// # Ok::<(), std::io::Error>(())
    /// ```
    fn directory(&mut self, path: &str) -> Result<()> {
        // Resolve path relative to current directory if set
        let relative_path = if let Some(ref current_dir) = self.current_dir {
            current_dir.join(path)
        } else {
            PathBuf::from(path)
        };

        // Construct full directory path
        let dir_path = self.output_dir.join(&relative_path);
        fs::create_dir_all(&dir_path)
            .map_err(|e| std::io::Error::new(
                e.kind(),
                format!("Failed to create directory {}: {}", dir_path.display(), e)
            ))?;

        // Normalize the path to handle .. and .
        // We do this by canonicalizing relative to output_dir
        let canonical = dir_path.canonicalize()
            .map_err(|e| std::io::Error::new(
                e.kind(),
                format!("Failed to canonicalize directory {}: {}", dir_path.display(), e)
            ))?;
        let output_canonical = self.output_dir.canonicalize()
            .map_err(|e| std::io::Error::new(
                e.kind(),
                format!("Failed to canonicalize output_dir {}: {}", self.output_dir.display(), e)
            ))?;
        let normalized_relative = canonical.strip_prefix(&output_canonical)
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|_| relative_path.clone());

        // Set this directory as the current directory context (use the normalized path)
        self.set_current_directory(Some(normalized_relative.to_string_lossy().to_string()));

        Ok(())
    }

    /// Get the current directory context
    fn current_directory(&self) -> Option<&str> {
        self.current_dir.as_ref().and_then(|p| p.to_str())
    }

    /// Set the current directory context
    fn set_current_directory(&mut self, path: Option<String>) {
        self.current_dir = path.map(PathBuf::from);
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

    #[test]
    fn test_directory_creates_dir() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        backend.directory("src/models").unwrap();

        let dir_path = temp_dir.path().join("src/models");
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_directory_creates_nested_dirs() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        backend.directory("src/main/java/models").unwrap();

        let dir_path = temp_dir.path().join("src/main/java/models");
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_directory_idempotent() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Creating the same directory twice should not fail
        backend.directory("src/models").unwrap();
        backend.directory("src/models").unwrap();

        let dir_path = temp_dir.path().join("src/models");
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_directory_sets_current_directory() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        assert!(backend.current_directory().is_none());

        backend.directory("src/models").unwrap();
        assert_eq!(backend.current_directory(), Some("src/models"));

        // Reset to None before creating a new absolute path
        backend.set_current_directory(None);
        backend.directory("src/controllers").unwrap();
        assert_eq!(backend.current_directory(), Some("src/controllers"));
    }

    #[test]
    fn test_entity_with_relative_path() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Set current directory
        backend.directory("generated/models").unwrap();

        // Create file with relative path
        backend.entity("User.rs", "pub struct User {}").unwrap();

        // Verify file was created in the current directory
        let file_path = temp_dir.path().join("generated/models/User.rs");
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "pub struct User {}");
    }

    #[test]
    fn test_entity_with_relative_path_multiple_files() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Set current directory
        backend.directory("generated/models").unwrap();

        // Create multiple files with relative paths
        backend.entity("User.rs", "pub struct User {}").unwrap();
        backend.entity("Post.rs", "pub struct Post {}").unwrap();
        backend.entity("Comment.rs", "pub struct Comment {}").unwrap();

        // Verify all files were created in the current directory
        assert!(temp_dir.path().join("generated/models/User.rs").exists());
        assert!(temp_dir.path().join("generated/models/Post.rs").exists());
        assert!(temp_dir.path().join("generated/models/Comment.rs").exists());
    }

    #[test]
    fn test_directory_switching() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create first directory and file
        backend.directory("src/models").unwrap();
        backend.entity("User.rs", "pub struct User {}").unwrap();

        // Reset and switch to another directory
        backend.set_current_directory(None);
        backend.directory("src/controllers").unwrap();
        backend.entity("UserController.rs", "pub struct UserController {}").unwrap();

        // Verify files in correct locations
        assert!(temp_dir.path().join("src/models/User.rs").exists());
        assert!(temp_dir.path().join("src/controllers/UserController.rs").exists());
    }

    #[test]
    fn test_reset_current_directory() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Set current directory
        backend.directory("src/models").unwrap();
        assert_eq!(backend.current_directory(), Some("src/models"));

        // Reset to None
        backend.set_current_directory(None);
        assert!(backend.current_directory().is_none());

        // File should now be created relative to output_dir
        backend.entity("root.txt", "content").unwrap();
        assert!(temp_dir.path().join("root.txt").exists());
    }

    #[test]
    fn test_directory_with_relative_path() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create base directory
        backend.directory("generated").unwrap();
        assert_eq!(backend.current_directory(), Some("generated"));

        // Create subdirectory with relative path
        backend.directory("models").unwrap();
        assert_eq!(backend.current_directory(), Some("generated/models"));

        // Verify the full path was created
        let dir_path = temp_dir.path().join("generated/models");
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_directory_nested_relative_paths() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create nested directories using relative paths
        backend.directory("src").unwrap();
        backend.directory("main").unwrap();
        backend.directory("java").unwrap();
        backend.directory("models").unwrap();

        // Current directory should be the full path
        assert_eq!(backend.current_directory(), Some("src/main/java/models"));

        // Verify the full nested path exists
        let dir_path = temp_dir.path().join("src/main/java/models");
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_directory_relative_then_entity() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create directory structure using relative paths
        backend.directory("generated").unwrap();
        backend.directory("models").unwrap();

        // Create file with relative path
        backend.entity("User.rs", "pub struct User {}").unwrap();

        // Verify file is in the correct nested location
        let file_path = temp_dir.path().join("generated/models/User.rs");
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "pub struct User {}");
    }

    #[test]
    fn test_directory_absolute_path_resets() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create nested directory
        backend.directory("src").unwrap();
        backend.directory("models").unwrap();
        assert_eq!(backend.current_directory(), Some("src/models"));

        // Reset to None and create new absolute path
        backend.set_current_directory(None);
        backend.directory("tests").unwrap();
        assert_eq!(backend.current_directory(), Some("tests"));

        // Verify both directories exist
        assert!(temp_dir.path().join("src/models").exists());
        assert!(temp_dir.path().join("tests").exists());
    }

    #[test]
    fn test_directory_parent_navigation() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Create: src/models
        backend.directory("src").unwrap();
        backend.directory("models").unwrap();
        assert_eq!(backend.current_directory(), Some("src/models"));

        // Navigate to parent and create sibling: src/controllers
        backend.directory("..").unwrap();
        assert_eq!(backend.current_directory(), Some("src"));
        backend.directory("controllers").unwrap();
        assert_eq!(backend.current_directory(), Some("src/controllers"));

        // Verify both directories exist
        assert!(temp_dir.path().join("src/models").exists());
        assert!(temp_dir.path().join("src/controllers").exists());

        // Create file in controllers
        backend.entity("UserController.rs", "pub struct UserController {}").unwrap();
        assert!(temp_dir.path().join("src/controllers/UserController.rs").exists());
    }

    #[test]
    fn test_directory_natural_nesting() {
        let temp_dir = TempDir::new().unwrap();
        let mut backend = SgmlBackend::new(temp_dir.path());

        // Natural nesting pattern
        backend.directory("generated").unwrap();

        // Create models subdirectory
        backend.directory("models").unwrap();
        backend.entity("User.rs", "pub struct User {}").unwrap();
        backend.entity("Post.rs", "pub struct Post {}").unwrap();

        // Go back to parent and create controllers subdirectory
        backend.directory("..").unwrap();
        backend.directory("controllers").unwrap();
        backend.entity("UserController.rs", "pub struct UserController {}").unwrap();

        // Go back to parent and create views subdirectory
        backend.directory("..").unwrap();
        backend.directory("views").unwrap();
        backend.entity("user.html", "<div>User</div>").unwrap();

        // Verify structure
        assert!(temp_dir.path().join("generated/models/User.rs").exists());
        assert!(temp_dir.path().join("generated/models/Post.rs").exists());
        assert!(temp_dir.path().join("generated/controllers/UserController.rs").exists());
        assert!(temp_dir.path().join("generated/views/user.html").exists());
    }
}
