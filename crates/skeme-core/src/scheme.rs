//! Steel Scheme interpreter integration
//!
//! Sets up Steel Scheme engine and registers all DSSSL primitives

use anyhow::Result;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use std::collections::HashMap;

use crate::primitives;

/// Scheme engine for template evaluation
pub struct SchemeEngine {
    pub(crate) engine: Engine,
    variables: HashMap<String, String>,
}

impl SchemeEngine {
    /// Create a new Scheme engine with DSSSL primitives registered
    pub fn new() -> Result<Self> {
        let engine = Engine::new();

        let mut scheme_engine = Self {
            engine,
            variables: HashMap::new(),
        };

        // Register all DSSSL primitives
        primitives::grove::register_grove_primitives(&mut scheme_engine)?;
        primitives::processing::register_processing_primitives(&mut scheme_engine)?;
        // primitives::util::register_util_primitives(&mut scheme_engine)?;

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
        self.variables.insert(name, value);
    }

    /// Get a variable value
    pub fn get_variable(&self, name: &str) -> Option<&str> {
        self.variables.get(name).map(|s| s.as_str())
    }

    /// Load and evaluate a Scheme file
    pub fn load_file(&mut self, path: &str) -> Result<()> {
        todo!("Implement load_file for {}", path)
    }

    /// Evaluate a Scheme expression
    pub fn eval(&mut self, expr: &str) -> Result<String> {
        todo!("Implement eval for: {}", expr)
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
}
