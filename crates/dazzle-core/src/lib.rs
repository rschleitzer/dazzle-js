//! # Dazzle Core
//!
//! Core library for Dazzle: Scheme interpreter (ported from OpenJade),
//! DSSSL engine, and trait definitions for groves and backends.
//!
//! ## Architecture
//!
//! Dazzle follows OpenJade's clean separation:
//!
//! - `grove`: Trait definitions for document tree abstraction (like OpenJade's grove/)
//! - `fot`: Trait definitions for flow object tree backends (like OpenJade's FOTBuilder)
//! - `scheme`: Scheme interpreter (ported from OpenJade's style/)
//!   - `value`: Scheme value types (ELObj equivalent)
//!   - `parser`: S-expression parser (SchemeParser.cxx equivalent)
//!   - `interpreter`: Evaluator and environment (Interpreter.cxx equivalent)
//!   - `primitives`: All 236 DSSSL primitives (primitive.cxx equivalent)
//! - `dsssl`: DSSSL style engine (processing modes, rules, etc.)
//!
//! ## Design Principles
//!
//! 1. **Trait-based abstraction**: Groves and backends are defined as traits,
//!    allowing multiple implementations without coupling to the interpreter.
//!
//! 2. **Faithful port**: Preserves OpenJade's proven design and performance optimizations
//!    (instruction-based evaluation, string interning, lazy node lists).
//!
//! 3. **100% compatibility**: All valid R4RS/DSSSL code that works in OpenJade
//!    works in Dazzle without modification.

pub mod fot;
pub mod grove;
pub mod scheme;

// Re-export key types for convenience
pub use fot::FotBuilder;
pub use grove::{Grove, Node, NodeList};
pub use scheme::{Environment, EvalError, EvalResult, Evaluator, PairData, Parser, Procedure, Value};

/// Dazzle version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }
}
