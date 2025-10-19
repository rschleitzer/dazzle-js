//! Scheme interpreter (ported from OpenJade)
//!
//! This module contains the Scheme R4RS interpreter ported from OpenJade's `style/` directory.
//!
//! ## Architecture
//!
//! Following OpenJade's structure:
//!
//! - `value`: Scheme value types (corresponds to `ELObj` hierarchy)
//! - `environment`: Variable bindings and lexical scoping (corresponds to Interpreter identifier tables)
//! - `parser`: Lexer and S-expression parser (corresponds to `SchemeParser.cxx`)
//! - `evaluator`: Core eval loop (corresponds to `Interpreter.cxx`)
//! - `primitives/`: Built-in Scheme procedures (corresponds to `primitive.cxx`)
//!
//! ## OpenJade Correspondence
//!
//! | Dazzle Module          | OpenJade File              | Lines | Purpose                    |
//! |------------------------|----------------------------|-------|----------------------------|
//! | `value.rs`             | `style/ELObj.{h,cxx}`      | ~1500 | Value types & operations   |
//! | `environment.rs`       | `style/Interpreter.cxx`    | ~500  | Variable bindings          |
//! | `parser.rs`            | `style/SchemeParser.cxx`   | ~2500 | Tokenizer & parser         |
//! | `evaluator.rs`         | `style/Interpreter.cxx`    | ~2000 | Eval loop & control flow   |
//! | `primitives/`          | `style/primitive.cxx`      | ~5700 | 236 built-in procedures    |
//!
//! Total: ~12,200 lines C++ → estimated ~10,000 lines Rust

pub mod environment;
pub mod evaluator;
pub mod parser;
pub mod value;

// Re-export key types for convenience
pub use environment::Environment;
pub use evaluator::{EvalError, EvalResult, Evaluator};
pub use parser::{ParseError, Parser, Position, Tokenizer};
pub use value::{PairData, Procedure, Value};
