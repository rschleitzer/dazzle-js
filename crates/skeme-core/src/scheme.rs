//! Steel Scheme interpreter integration
//!
//! Sets up Steel Scheme engine and registers all DSSSL primitives

use anyhow::{Context, Result};
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use std::collections::HashMap;

use crate::primitives;

/// Scheme engine for template evaluation
pub struct SchemeEngine {
    pub(crate) engine: Engine,
    variables: HashMap<String, String>,
    search_paths: Vec<std::path::PathBuf>,
}

/// Prelude code that defines load and other helpers
/// Currently empty - load primitive removed due to Steel limitations
const PRELUDE: &str = r#"
;; Prelude loaded successfully
"#;

impl SchemeEngine {
    /// Create a new Scheme engine with DSSSL primitives registered
    pub fn new() -> Result<Self> {
        let engine = Engine::new();

        let mut scheme_engine = Self {
            engine,
            variables: HashMap::new(),
            search_paths: vec![std::env::current_dir()?],  // Default: current directory
        };

        // Register all DSSSL primitives
        primitives::grove::register_grove_primitives(&mut scheme_engine)?;
        primitives::processing::register_processing_primitives(&mut scheme_engine)?;
        primitives::util::register_util_primitives(&mut scheme_engine)?;

        // Load prelude (defines load and other helpers)
        scheme_engine.engine
            .compile_and_run_raw_program(PRELUDE.to_string())
            .map_err(|e| anyhow::anyhow!("Failed to load prelude: {:?}", e))?;

        Ok(scheme_engine)
    }

    /// Register a Rust function as a Scheme primitive
    ///
    /// This delegates to Steel's Engine::register_fn
    pub fn register_fn<FN, ARGS, RET>(&mut self, name: &'static str, func: FN) -> &mut Self
    where
        Engine: RegisterFn<FN, ARGS, RET>,
    {
        self.engine.register_fn(name, func);
        self
    }

    /// Set a variable (from CLI -V flags)
    pub fn set_variable(&mut self, name: String, value: String) {
        self.variables.insert(name.clone(), value.clone());

        // Also inject into Scheme environment as a global
        let _ = self.engine.compile_and_run_raw_program(format!(
            "(define {} \"{}\")",
            name,
            value.replace("\"", "\\\"")
        ));
    }

    /// Get a variable value
    pub fn get_variable(&self, name: &str) -> Option<&str> {
        self.variables.get(name).map(|s| s.as_str())
    }

    /// Add a template search directory
    pub fn add_search_path(&mut self, path: std::path::PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Get search paths (for debugging)
    pub fn search_paths(&self) -> &[std::path::PathBuf] {
        &self.search_paths
    }

    /// Find a file in the search paths
    fn find_in_search_paths(&self, filename: &str) -> Option<std::path::PathBuf> {
        use std::path::Path;

        // If it's an absolute path or contains path separators, use it directly
        let path = Path::new(filename);
        if path.is_absolute() || filename.contains('/') || filename.contains('\\') {
            if path.exists() {
                return Some(path.to_path_buf());
            }
            return None;
        }

        // Search in each search path
        for search_dir in &self.search_paths {
            let candidate = search_dir.join(filename);
            if candidate.exists() {
                return Some(candidate);
            }
        }

        None
    }

    /// Load and evaluate a Scheme file
    pub fn load_file(&mut self, path: &str) -> Result<()> {
        // Try to find the file in search paths
        let file_path = self.find_in_search_paths(path)
            .ok_or_else(|| anyhow::anyhow!("File not found in search paths: {}", path))?;

        let contents = std::fs::read_to_string(&file_path)
            .with_context(|| format!("Failed to read Scheme file: {}", file_path.display()))?;

        self.engine
            .compile_and_run_raw_program(contents)
            .map_err(|e| anyhow::anyhow!("Scheme error in {}: {:?}", file_path.display(), e))?;

        Ok(())
    }

    /// Evaluate a Scheme expression and return the result as a string
    pub fn eval(&mut self, expr: &str) -> Result<String> {
        let results = self
            .engine
            .compile_and_run_raw_program(expr.to_string())
            .map_err(|e| anyhow::anyhow!("Scheme error: {:?}", e))?;

        // Return the last result (like a REPL)
        if let Some(last) = results.last() {
            Ok(format!("{}", last))
        } else {
            Ok(String::from("#<void>"))
        }
    }

    /// Get direct access to the Steel engine for advanced operations
    pub fn engine_mut(&mut self) -> &mut Engine {
        &mut self.engine
    }

    /// Set the current grove for template processing
    /// This makes the grove available to the template via `current-grove`
    /// and the root node available via `current-root`
    pub fn set_current_grove(&mut self, grove: crate::grove::Grove) -> Result<()> {
        use steel::gc::Gc;
        use steel::rvals::SteelVal;

        // We need to keep the Grove (which owns the Document) alive
        // So we'll register both the grove and provide a function to get the root

        // Wrap the entire grove in a SteelVal
        let grove_val = SteelVal::Custom(Gc::new_mut(Box::new(grove)));
        self.engine.register_value("current-grove", grove_val);

        // Now evaluate Scheme code to extract the root and make it available
        // We'll do this by calling our grove-root primitive
        let code = "(define current-root (grove-root current-grove))";
        let _ = self.eval(code);

        Ok(())
    }
}

impl Default for SchemeEngine {
    fn default() -> Self {
        Self::new().expect("Failed to create SchemeEngine")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_creation() {
        let _engine = SchemeEngine::new().unwrap();
    }

    #[test]
    fn test_variables() {
        let mut engine = SchemeEngine::new().unwrap();
        engine.set_variable("foo".to_string(), "bar".to_string());
        assert_eq!(engine.get_variable("foo"), Some("bar"));
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut engine = SchemeEngine::new().unwrap();
        let result = engine.eval("(+ 2 3)").unwrap();
        assert_eq!(result, "5");
    }

    #[test]
    fn test_eval_string() {
        let mut engine = SchemeEngine::new().unwrap();
        let result = engine.eval(r#"(string-append "Hello" " " "World")"#).unwrap();
        assert!(result.contains("Hello World"));
    }

    #[test]
    fn test_grove_primitive_available() {
        let mut engine = SchemeEngine::new().unwrap();
        // Test that our registered primitives are available
        // gi, children, etc. should be defined
        let result = engine.eval("(procedure? gi)");
        assert!(result.is_ok());
    }
}
