//! Template file loading
//!
//! Handles loading .scm template files with search path resolution.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Template file loader with search path support
pub struct TemplateLoader {
    search_paths: Vec<PathBuf>,
}

impl TemplateLoader {
    /// Create a new template loader
    pub fn new() -> Self {
        Self {
            search_paths: vec![PathBuf::from(".")],
        }
    }

    /// Add a search path (from -D flag)
    pub fn add_search_path(&mut self, path: impl Into<PathBuf>) {
        self.search_paths.push(path.into());
    }

    /// Find a template file in search paths
    pub fn find_template(&self, name: &str) -> Result<PathBuf> {
        for search_path in &self.search_paths {
            let candidate = search_path.join(name);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        anyhow::bail!("Template not found: {}", name)
    }

    /// Load a template file
    pub fn load_template(&self, path: &Path) -> Result<String> {
        std::fs::read_to_string(path)
            .with_context(|| format!("Failed to load template: {}", path.display()))
    }
}

impl Default for TemplateLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loader_creation() {
        let loader = TemplateLoader::new();
        assert!(!loader.search_paths.is_empty());
    }
}
