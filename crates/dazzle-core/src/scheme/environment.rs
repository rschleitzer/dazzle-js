//! Scheme environment (variable bindings and lexical scoping)
//!
//! This module implements the environment for storing variable bindings,
//! corresponding to OpenJade's `Interpreter` identifier management.
//!
//! ## OpenJade Correspondence
//!
//! OpenJade's `Interpreter` class maintains:
//! - `identTable_`: Hash table of identifiers (symbols)
//! - Environment frames (linked list for lexical scoping)
//! - Global environment (built-in procedures + user definitions)
//!
//! ## Design
//!
//! We use a simple linked environment structure:
//! - Each environment frame is a HashMap<Rc<str>, Value>
//! - Parent pointer for lexical scoping
//! - Gc-managed for safety

// Suppress warnings from gc_derive macro (third-party crate issue)
#![allow(non_local_definitions)]

use crate::scheme::value::Value;
use gc::{Gc, GcCell};
use std::collections::HashMap;
use std::rc::Rc;

/// An environment frame (variable bindings)
///
/// Corresponds to OpenJade's environment frames in `Interpreter`.
///
/// ## Lexical Scoping
///
/// Each frame has:
/// - `bindings`: Variables defined in this frame
/// - `parent`: Enclosing scope (None for global environment)
///
/// Variable lookup walks up the parent chain until found or global reached.
#[derive(Debug, Clone, gc::Trace, gc::Finalize)]
pub struct Environment {
    /// Variable bindings in this frame
    bindings: GcCell<HashMap<Rc<str>, Value>>,

    /// Parent environment (None for global scope)
    parent: Option<Gc<Environment>>,
}

impl Environment {
    /// Create a new global (top-level) environment
    pub fn new_global() -> Gc<Self> {
        Gc::new(Environment {
            bindings: GcCell::new(HashMap::new()),
            parent: None,
        })
    }

    /// Create a new child environment extending the given parent
    ///
    /// Used for:
    /// - Function calls (parameters in new scope)
    /// - `let`, `let*`, `letrec` bindings
    /// - Internal defines
    pub fn extend(parent: Gc<Environment>) -> Gc<Self> {
        Gc::new(Environment {
            bindings: GcCell::new(HashMap::new()),
            parent: Some(parent),
        })
    }

    /// Define a variable in this environment (set binding)
    ///
    /// **Mutates** the current frame.
    ///
    /// Used for:
    /// - `define` expressions
    /// - Function parameter bindings
    /// - `let` bindings
    ///
    /// ## DSSSL Note
    ///
    /// DSSSL is mostly functional, but `define` at top-level is imperative.
    /// Internal `define`s in `lambda` bodies create a new frame.
    pub fn define(&self, name: &str, value: Value) {
        let symbol = Rc::from(name);
        self.bindings.borrow_mut().insert(symbol, value);
    }

    /// Look up a variable in this environment or any parent
    ///
    /// Returns `None` if variable is undefined.
    ///
    /// Walks up the parent chain until:
    /// - Variable found → return value
    /// - Global reached and not found → return None
    ///
    /// Corresponds to OpenJade's `Interpreter::lookup()`.
    pub fn lookup(&self, name: &str) -> Option<Value> {
        let symbol = Rc::from(name);

        // Check this frame first
        if let Some(value) = self.bindings.borrow().get(&symbol) {
            return Some(value.clone());
        }

        // Walk up parent chain
        if let Some(ref parent) = self.parent {
            return parent.lookup(name);
        }

        // Not found
        None
    }

    /// Set an existing variable (mutation)
    ///
    /// Returns `Ok(())` if variable found and updated, `Err` if not found.
    ///
    /// Used for:
    /// - `set!` expressions
    ///
    /// ## R4RS vs DSSSL
    ///
    /// R4RS allows `set!` on any variable.
    /// DSSSL restricts mutation (mostly functional).
    /// For now, we allow it (OpenJade does).
    pub fn set(&self, name: &str, value: Value) -> Result<(), String> {
        let symbol = Rc::from(name);

        // Check this frame first
        if self.bindings.borrow().contains_key(&symbol) {
            self.bindings.borrow_mut().insert(symbol, value);
            return Ok(());
        }

        // Walk up parent chain
        if let Some(ref parent) = self.parent {
            return parent.set(name, value);
        }

        // Variable not found
        Err(format!("Undefined variable: {}", name))
    }

    /// Check if a variable is defined in this environment or any parent
    pub fn is_defined(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    /// Get the parent environment (if any)
    pub fn parent(&self) -> Option<Gc<Environment>> {
        self.parent.clone()
    }

    /// Get all bindings in this frame (not including parents)
    ///
    /// Used for debugging and introspection.
    pub fn local_bindings(&self) -> Vec<(Rc<str>, Value)> {
        self.bindings
            .borrow()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Get all bindings in this environment and all parents
    ///
    /// Returns a HashMap of all variable bindings, with child bindings
    /// shadowing parent bindings. Used for syncing to VM globals.
    pub fn all_bindings(&self) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        // Walk up parent chain first (so child bindings override parent)
        if let Some(ref parent) = self.parent {
            result = parent.all_bindings();
        }

        // Add/override with this frame's bindings
        for (name, value) in self.bindings.borrow().iter() {
            result.insert(name.to_string(), value.clone());
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_environment() {
        let env = Environment::new_global();
        env.define("x", Value::integer(42));

        assert_eq!(
            env.lookup("x").unwrap().is_integer(),
            true
        );
        assert!(env.lookup("y").is_none());
    }

    #[test]
    fn test_nested_environments() {
        let global = Environment::new_global();
        global.define("x", Value::integer(1));
        global.define("y", Value::integer(2));

        let local = Environment::extend(global.clone());
        local.define("y", Value::integer(20)); // Shadow y
        local.define("z", Value::integer(30));

        // Local lookup: z is local, y shadows global, x from global
        if let Value::Integer(n) = local.lookup("z").unwrap() {
            assert_eq!(n, 30);
        }
        if let Value::Integer(n) = local.lookup("y").unwrap() {
            assert_eq!(n, 20); // Shadowed
        }
        if let Value::Integer(n) = local.lookup("x").unwrap() {
            assert_eq!(n, 1); // From global
        }

        // Global is unchanged
        if let Value::Integer(n) = global.lookup("y").unwrap() {
            assert_eq!(n, 2); // Not shadowed
        }
    }

    #[test]
    fn test_set_variable() {
        let env = Environment::new_global();
        env.define("x", Value::integer(1));

        // Set existing variable
        assert!(env.set("x", Value::integer(2)).is_ok());
        if let Value::Integer(n) = env.lookup("x").unwrap() {
            assert_eq!(n, 2);
        }

        // Set undefined variable fails
        assert!(env.set("y", Value::integer(3)).is_err());
    }

    #[test]
    fn test_is_defined() {
        let global = Environment::new_global();
        global.define("x", Value::integer(1));

        let local = Environment::extend(global.clone());
        local.define("y", Value::integer(2));

        assert!(local.is_defined("x")); // From global
        assert!(local.is_defined("y")); // From local
        assert!(!local.is_defined("z")); // Undefined
    }

    #[test]
    fn test_deep_nesting() {
        let global = Environment::new_global();
        global.define("a", Value::integer(1));

        let env1 = Environment::extend(global.clone());
        env1.define("b", Value::integer(2));

        let env2 = Environment::extend(env1.clone());
        env2.define("c", Value::integer(3));

        let env3 = Environment::extend(env2.clone());
        env3.define("d", Value::integer(4));

        // Lookup walks all the way up
        assert!(env3.is_defined("a"));
        assert!(env3.is_defined("b"));
        assert!(env3.is_defined("c"));
        assert!(env3.is_defined("d"));
    }
}
