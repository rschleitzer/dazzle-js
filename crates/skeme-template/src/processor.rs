//! Template processor
//!
//! Executes templates with processing control.

use anyhow::Result;
use skeme_core::{Node, SchemeEngine};

/// Template processor
pub struct TemplateProcessor {
    engine: SchemeEngine,
}

impl TemplateProcessor {
    /// Create a new template processor
    pub fn new(engine: SchemeEngine) -> Self {
        Self { engine }
    }

    /// Process a template with the given root node
    pub fn process(&mut self, _root: &Node) -> Result<()> {
        todo!("Implement template processing")
    }

    /// Get the underlying Scheme engine
    pub fn engine_mut(&mut self) -> &mut SchemeEngine {
        &mut self.engine
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_processor_creation() {
        let engine = SchemeEngine::new().unwrap();
        let _processor = TemplateProcessor::new(engine);
    }
}
