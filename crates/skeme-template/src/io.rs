//! File I/O primitives
//!
//! Implements write-file, ensure-dir, and related file operations.

use anyhow::{Context, Result};
use std::path::Path;

/// Write content to a file
pub fn write_file(path: impl AsRef<Path>, content: &str) -> Result<()> {
    let path = path.as_ref();

    // Ensure parent directory exists
    if let Some(parent) = path.parent() {
        ensure_dir(parent)?;
    }

    std::fs::write(path, content)
        .with_context(|| format!("Failed to write file: {}", path.display()))
}

/// Ensure a directory exists (create if needed)
pub fn ensure_dir(path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();

    if !path.exists() {
        std::fs::create_dir_all(path)
            .with_context(|| format!("Failed to create directory: {}", path.display()))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_write_file() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");

        write_file(&file_path, "Hello, world!").unwrap();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "Hello, world!");
    }

    #[test]
    fn test_ensure_dir() {
        let dir = tempdir().unwrap();
        let nested_dir = dir.path().join("a/b/c");

        ensure_dir(&nested_dir).unwrap();

        assert!(nested_dir.exists());
        assert!(nested_dir.is_dir());
    }

    #[test]
    fn test_write_file_creates_parent_dirs() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("a/b/c/test.txt");

        write_file(&file_path, "test").unwrap();

        assert!(file_path.exists());
    }
}
