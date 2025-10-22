//! Scheme value types
//!
//! This module defines the `Value` enum, which represents all Scheme values.
//! Corresponds to OpenJade's `ELObj` class hierarchy.
//!
//! ## Design
//!
//! Like OpenJade, we use garbage collection for heap-allocated values:
//! - OpenJade: Custom mark-and-sweep collector (`Collector` class)
//! - Dazzle: Rust `gc` crate (conservative GC)
//!
//! ## Value Types
//!
//! **Basic types** (R4RS Scheme):
//! - Nil: Empty list `()`
//! - Bool: `#t` and `#f`
//! - Integer: Exact integers (i64)
//! - Real: Inexact reals (f64)
//! - Char: Unicode characters
//! - String: Immutable strings
//! - Symbol: Interned identifiers
//! - Pair: Cons cells (car, cdr)
//! - Vector: Arrays
//! - Procedure: Functions (built-in or user-defined)
//!
//! **DSSSL types** (code generation):
//! - NodeList: Document tree node collections
//! - Sosofo: Flow object sequences
//!
//! **DSSSL types** (document formatting - stubs for now):
//! - Quantity, Color, Address, etc.

// Suppress warnings from gc_derive macro (third-party crate issue)
#![allow(non_local_definitions)]

use gc::{Gc, GcCell};
use std::fmt;
use std::rc::Rc;

// Import Position for source location tracking
use crate::scheme::parser::Position;

/// Source code location information
///
/// Used for error reporting and stack traces.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceInfo {
    /// Source file path (template file)
    pub file: String,
    /// Position in the file (line:column)
    pub pos: Position,
}

impl SourceInfo {
    pub fn new(file: String, pos: Position) -> Self {
        SourceInfo { file, pos }
    }
}

impl fmt::Display for SourceInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.pos)
    }
}

/// A Scheme value
///
/// Corresponds to OpenJade's `ELObj` base class.
///
/// **Memory management**: Uses `Gc<T>` for heap-allocated values.
/// The `gc` crate provides conservative garbage collection similar
/// to OpenJade's `Collector`.
///
/// **Note**: We manually implement Trace/Finalize because Node and NodeList
/// variants use Rc (not Gc) and contain trait objects.
#[derive(Clone)]
pub enum Value {
    /// The empty list `()`
    ///
    /// OpenJade: `NilObj`
    Nil,

    /// Boolean: `#t` or `#f`
    ///
    /// OpenJade: `TrueObj` / `FalseObj`
    Bool(bool),

    /// Exact integer
    ///
    /// OpenJade: `IntegerObj` (long n_)
    Integer(i64),

    /// Inexact real number
    ///
    /// OpenJade: `RealObj` (double n_)
    Real(f64),

    /// Unicode character
    ///
    /// OpenJade: `CharObj` (Char ch_)
    Char(char),

    /// Immutable string
    ///
    /// OpenJade: `StringObj` (extends StringC)
    ///
    /// Using `Gc<String>` for garbage collection.
    String(Gc<String>),

    /// Interned symbol
    ///
    /// OpenJade: `SymbolObj` (StringObj* name_)
    ///
    /// Symbols are interned (shared) for efficiency.
    /// Using `Rc<str>` since symbols are immutable and shared.
    Symbol(Rc<str>),

    /// Keyword (DSSSL extension)
    ///
    /// OpenJade: `KeywordObj`
    ///
    /// Keywords are like symbols but in a separate namespace.
    Keyword(Rc<str>),

    /// Cons cell (pair)
    ///
    /// OpenJade: `PairObj` (ELObj* car_, ELObj* cdr_)
    ///
    /// Using `Gc<PairData>` for garbage-collected pairs.
    /// `GcCell` allows mutation (for set-car!/set-cdr!).
    Pair(Gc<GcCell<PairData>>),

    /// Vector (array)
    ///
    /// OpenJade: `VectorObj` (Vector<ELObj*>)
    ///
    /// Using `Gc<GcCell<Vec<Value>>>` for mutable vectors.
    Vector(Gc<GcCell<Vec<Value>>>),

    /// Procedure (function)
    ///
    /// OpenJade: `FunctionObj` (various subclasses)
    ///
    /// Can be:
    /// - Built-in primitive (Rust function)
    /// - User-defined lambda (compiled bytecode or AST)
    Procedure(Gc<Procedure>),

    // DSSSL types (grove and flow objects)
    /// A node in the document grove
    ///
    /// Represents a node from the XML document tree.
    /// Corresponds to OpenJade's node objects in the grove.
    ///
    /// **Phase 3**: Now fully implemented with libxml2 grove.
    ///
    /// Uses `Rc<Box<dyn Node>>` instead of Gc because:
    /// - Nodes are owned by the grove, not the Scheme GC
    /// - Grove lifetime is managed separately
    /// - Trait objects can't derive Trace automatically
    ///
    /// NOTE: Managed by Rc, not GC - see manual Trace impl below
    Node(Rc<Box<dyn crate::grove::Node>>),

    /// Node list (grove query result)
    ///
    /// Represents a collection of nodes from grove queries.
    /// Corresponds to DSSSL node-list objects.
    ///
    /// **Phase 3**: Now fully implemented with libxml2 grove.
    ///
    /// Uses `Rc<Box<dyn NodeList>>` for same reasons as Node.
    ///
    /// NOTE: Managed by Rc, not GC - see manual Trace impl below
    NodeList(Rc<Box<dyn crate::grove::NodeList>>),

    /// Sosofo (flow object sequence)
    ///
    /// Placeholder - will be properly implemented in Phase 4.
    Sosofo,

    /// Unspecified value
    ///
    /// Returned by expressions with unspecified results (like set!).
    ///
    /// OpenJade: `UnspecifiedObj`
    Unspecified,

    /// Error marker
    ///
    /// Used internally for error propagation.
    ///
    /// OpenJade: `ErrorObj`
    Error,
}

/// Pair data (car and cdr)
///
/// Separated from `Value::Pair` to allow mutation via `GcCell`.
#[derive(Clone, gc::Trace, gc::Finalize)]
pub struct PairData {
    pub car: Value,
    pub cdr: Value,
}

impl PairData {
    pub fn new(car: Value, cdr: Value) -> Self {
        PairData { car, cdr }
    }
}

/// Procedure (function)
///
/// Can be either a built-in primitive or user-defined lambda.
#[derive(gc::Finalize)]
pub enum Procedure {
    /// Built-in primitive function
    ///
    /// Takes arguments and returns a result.
    /// Primitives can fail (return Err) or succeed (return Ok(Value)).
    Primitive {
        name: &'static str,
        func: fn(&[Value]) -> Result<Value, String>,
    },

    /// User-defined lambda
    ///
    /// Captures:
    /// - `params`: Parameter names (formal parameters)
    /// - `body`: Expression to evaluate when called
    /// - `env`: Closure environment (captures lexical scope)
    /// - `source`: Source location (for error reporting)
    /// - `name`: Optional name (for named procedures defined with `define`)
    Lambda {
        params: Gc<Vec<String>>,
        body: Gc<Value>,
        env: Gc<crate::scheme::environment::Environment>,
        source: Option<SourceInfo>,
        name: Option<String>,
    },
}

impl Clone for Procedure {
    fn clone(&self) -> Self {
        match self {
            Procedure::Primitive { name, func } => Procedure::Primitive {
                name,
                func: *func,
            },
            Procedure::Lambda { params, body, env, source, name } => Procedure::Lambda {
                params: params.clone(),
                body: body.clone(),
                env: env.clone(),
                source: source.clone(),
                name: name.clone(),
            },
        }
    }
}

// Manual Trace implementation since function pointers don't need tracing
unsafe impl gc::Trace for Procedure {
    unsafe fn trace(&self) {
        match self {
            Procedure::Primitive { .. } => {
                // Primitives don't have GC'd data
            }
            Procedure::Lambda { params, body, env, source: _, name: _ } => {
                // Trace the lambda's garbage-collected fields
                // Note: source and name are not GC'd, so we don't trace them
                params.trace();
                body.trace();
                env.trace();
            }
        }
    }

    unsafe fn root(&self) {
        match self {
            Procedure::Primitive { .. } => {}
            Procedure::Lambda { params, body, env, source: _, name: _ } => {
                params.root();
                body.root();
                env.root();
            }
        }
    }

    unsafe fn unroot(&self) {
        match self {
            Procedure::Primitive { .. } => {}
            Procedure::Lambda { params, body, env, source: _, name: _ } => {
                params.unroot();
                body.unroot();
                env.unroot();
            }
        }
    }

    fn finalize_glue(&self) {
        gc::Finalize::finalize(self);
    }
}

// =============================================================================
// Value constructors (ergonomic API)
// =============================================================================

impl Value {
    /// Create a boolean value
    pub fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    /// Create an integer value
    pub fn integer(n: i64) -> Self {
        Value::Integer(n)
    }

    /// Create a real value
    pub fn real(n: f64) -> Self {
        Value::Real(n)
    }

    /// Create a character value
    pub fn char(ch: char) -> Self {
        Value::Char(ch)
    }

    /// Create a string value
    pub fn string(s: String) -> Self {
        Value::String(Gc::new(s))
    }

    /// Create a symbol value
    pub fn symbol(s: &str) -> Self {
        Value::Symbol(Rc::from(s))
    }

    /// Create a keyword value
    pub fn keyword(s: &str) -> Self {
        Value::Keyword(Rc::from(s))
    }

    /// Create a cons cell (pair)
    pub fn cons(car: Value, cdr: Value) -> Self {
        Value::Pair(Gc::new(GcCell::new(PairData::new(car, cdr))))
    }

    /// Create a vector
    pub fn vector(elements: Vec<Value>) -> Self {
        Value::Vector(Gc::new(GcCell::new(elements)))
    }

    /// Create a built-in primitive procedure
    pub fn primitive(name: &'static str, func: fn(&[Value]) -> Result<Value, String>) -> Self {
        Value::Procedure(Gc::new(Procedure::Primitive { name, func }))
    }

    /// Create a user-defined lambda procedure
    pub fn lambda(
        params: Vec<String>,
        body: Value,
        env: Gc<crate::scheme::environment::Environment>,
    ) -> Self {
        Value::Procedure(Gc::new(Procedure::Lambda {
            params: Gc::new(params),
            body: Gc::new(body),
            env,
            source: None,
            name: None,
        }))
    }

    /// Create a user-defined lambda procedure with source location
    pub fn lambda_with_source(
        params: Vec<String>,
        body: Value,
        env: Gc<crate::scheme::environment::Environment>,
        source: Option<SourceInfo>,
        name: Option<String>,
    ) -> Self {
        Value::Procedure(Gc::new(Procedure::Lambda {
            params: Gc::new(params),
            body: Gc::new(body),
            env,
            source,
            name,
        }))
    }

    /// Create a node value
    pub fn node(node: Box<dyn crate::grove::Node>) -> Self {
        Value::Node(Rc::new(node))
    }

    /// Create a node list value
    pub fn node_list(node_list: Box<dyn crate::grove::NodeList>) -> Self {
        Value::NodeList(Rc::new(node_list))
    }
}

// =============================================================================
// Value equality (Scheme equal? and eqv?)
// =============================================================================

impl Value {
    /// Scheme `equal?` - Deep structural equality
    ///
    /// Corresponds to OpenJade's `ELObj::equal()`
    ///
    /// Recursively compares:
    /// - Lists and vectors: Element-wise comparison
    /// - Strings: Content comparison
    /// - Numbers: Numeric equality
    /// - Everything else: Same as `eqv?`
    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            // Structural equality for lists
            (Value::Pair(p1), Value::Pair(p2)) => {
                let pair1 = p1.borrow();
                let pair2 = p2.borrow();
                pair1.car.equal(&pair2.car) && pair1.cdr.equal(&pair2.cdr)
            }

            // Structural equality for vectors
            (Value::Vector(v1), Value::Vector(v2)) => {
                let vec1 = v1.borrow();
                let vec2 = v2.borrow();
                if vec1.len() != vec2.len() {
                    return false;
                }
                vec1.iter().zip(vec2.iter()).all(|(a, b)| a.equal(b))
            }

            // String content comparison
            (Value::String(s1), Value::String(s2)) => **s1 == **s2,

            // For all other types, equal? is the same as eqv?
            _ => self.eqv(other),
        }
    }

    /// Scheme `eqv?` - Equivalence (same value, not necessarily same object)
    ///
    /// Corresponds to OpenJade's `ELObj::eqv()`
    ///
    /// Returns true if:
    /// - Both are the same boolean value
    /// - Both are the same number (integer or real)
    /// - Both are the same character
    /// - Both are the same symbol (symbols are interned)
    /// - Both are the same keyword
    /// - Both refer to the same pair/vector/procedure object
    /// - Both are nil
    pub fn eqv(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Integer(n1), Value::Integer(n2)) => n1 == n2,
            (Value::Real(n1), Value::Real(n2)) => n1 == n2,
            (Value::Char(c1), Value::Char(c2)) => c1 == c2,

            // Symbols and keywords: compare by content
            // NOTE: Currently uses string content comparison (O(n)).
            // FUTURE OPTIMIZATION: Implement global symbol table (interner) to enable
            // pointer-equality comparison (O(1)). This would require:
            //   - SymbolTable in Evaluator to intern all symbols
            //   - Parser and Environment using the interner
            //   - Change to: Rc::ptr_eq(s1, s2)
            // Current implementation is correct per R4RS, just not optimally fast.
            (Value::Symbol(s1), Value::Symbol(s2)) => **s1 == **s2,
            (Value::Keyword(k1), Value::Keyword(k2)) => **k1 == **k2,

            // For heap-allocated objects, compare object identity
            (Value::Pair(p1), Value::Pair(p2)) => Gc::ptr_eq(p1, p2),
            (Value::Vector(v1), Value::Vector(v2)) => Gc::ptr_eq(v1, v2),
            (Value::Procedure(proc1), Value::Procedure(proc2)) => Gc::ptr_eq(proc1, proc2),

            // Strings are not compared by eqv? - use equal? or eq?
            // (In Scheme, eqv? on strings is unspecified)
            (Value::String(_), Value::String(_)) => false,

            // Special types
            (Value::Node(n1), Value::Node(n2)) => {
                // Nodes are equal by pointer identity (same Rc)
                Rc::ptr_eq(n1, n2)
            }
            (Value::NodeList(nl1), Value::NodeList(nl2)) => {
                // NodeLists are equal by pointer identity (same object)
                Rc::ptr_eq(nl1, nl2)
            }
            (Value::Sosofo, Value::Sosofo) => true,
            (Value::Unspecified, Value::Unspecified) => true,
            (Value::Error, Value::Error) => true,

            // Different types are never eqv?
            _ => false,
        }
    }

    /// Scheme `eq?` - Object identity (same object in memory)
    ///
    /// For most types, same as `eqv?`. Symbols and keywords are interned,
    /// so `eq?` and `eqv?` are equivalent for them.
    pub fn eq(&self, other: &Value) -> bool {
        // For our implementation, eq? is the same as eqv?
        // since we intern symbols and use Gc pointers for heap objects
        self.eqv(other)
    }
}

// =============================================================================
// Value predicates (type checking)
// =============================================================================

impl Value {
    /// Is this the nil value?
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Is this a boolean?
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Is this true? (for conditionals)
    ///
    /// In Scheme, only `#f` is false; everything else (including nil) is true.
    pub fn is_true(&self) -> bool {
        !matches!(self, Value::Bool(false))
    }

    /// Is this an integer?
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer(_))
    }

    /// Is this a real number?
    pub fn is_real(&self) -> bool {
        matches!(self, Value::Real(_))
    }

    /// Is this a number (integer or real)?
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Real(_))
    }

    /// Is this a character?
    pub fn is_char(&self) -> bool {
        matches!(self, Value::Char(_))
    }

    /// Is this a string?
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    /// Is this a symbol?
    pub fn is_symbol(&self) -> bool {
        matches!(self, Value::Symbol(_))
    }

    /// Is this a pair?
    pub fn is_pair(&self) -> bool {
        matches!(self, Value::Pair(_))
    }

    /// Is this a list? (nil or pair)
    pub fn is_list(&self) -> bool {
        matches!(self, Value::Nil | Value::Pair(_))
    }

    /// Is this a vector?
    pub fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    /// Is this a procedure?
    pub fn is_procedure(&self) -> bool {
        matches!(self, Value::Procedure(_))
    }

    /// Is this a node?
    pub fn is_node(&self) -> bool {
        matches!(self, Value::Node(_))
    }

    /// Is this a node list?
    pub fn is_node_list(&self) -> bool {
        matches!(self, Value::NodeList(_))
    }
}

// =============================================================================
// Display / Debug
// =============================================================================

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Integer(n) => write!(f, "{}", n),
            Value::Real(n) => write!(f, "{}", n),
            Value::Char(ch) => write!(f, "#\\{}", ch),
            Value::String(s) => write!(f, "{:?}", **s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Keyword(s) => write!(f, "#:{}", s),
            Value::Pair(p) => {
                let pair = p.borrow();
                write!(f, "({:?} . {:?})", pair.car, pair.cdr)
            }
            Value::Vector(v) => {
                let vec = v.borrow();
                write!(f, "#(")?;
                for (i, val) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", val)?;
                }
                write!(f, ")")
            }
            Value::Procedure(proc) => match &**proc {
                Procedure::Primitive { name, .. } => write!(f, "#<primitive:{}>", name),
                Procedure::Lambda { .. } => write!(f, "#<lambda>"),
            },
            Value::Node(node) => {
                // Display node with its gi if available
                if let Some(gi) = node.gi() {
                    write!(f, "#<node:{}>", gi)
                } else {
                    write!(f, "#<node>")
                }
            }
            Value::NodeList(nl) => write!(f, "#<node-list:{}>", nl.length()),
            Value::Sosofo => write!(f, "#<sosofo>"),
            Value::Unspecified => write!(f, "#<unspecified>"),
            Value::Error => write!(f, "#<error>"),
        }
    }
}

// Implement Display to show values in a Scheme-readable way
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// =============================================================================
// Garbage Collection (Manual Trace Implementation)
// =============================================================================

/// Manual implementation of Trace for Value
///
/// We implement this manually because:
/// - Node and NodeList use Rc<Box<dyn Trait>>, which doesn't implement Trace
/// - These are managed by Rc, not the GC
/// - All other variants use Gc and need proper tracing
unsafe impl gc::Trace for Value {
    unsafe fn trace(&self) {
        match self {
            Value::Nil => {}
            Value::Bool(_) => {}
            Value::Integer(_) => {}
            Value::Real(_) => {}
            Value::Char(_) => {}
            Value::String(s) => s.trace(),
            Value::Symbol(s) => s.trace(),
            Value::Keyword(k) => k.trace(),
            Value::Pair(p) => p.trace(),
            Value::Vector(v) => v.trace(),
            Value::Procedure(proc) => proc.trace(),
            // Node and NodeList use Rc, not Gc - no tracing needed
            Value::Node(_) => {}
            Value::NodeList(_) => {}
            Value::Sosofo => {}
            Value::Unspecified => {}
            Value::Error => {}
        }
    }

    unsafe fn root(&self) {
        match self {
            Value::String(s) => s.root(),
            Value::Symbol(s) => s.root(),
            Value::Keyword(k) => k.root(),
            Value::Pair(p) => p.root(),
            Value::Vector(v) => v.root(),
            Value::Procedure(proc) => proc.root(),
            _ => {}
        }
    }

    unsafe fn unroot(&self) {
        match self {
            Value::String(s) => s.unroot(),
            Value::Symbol(s) => s.unroot(),
            Value::Keyword(k) => k.unroot(),
            Value::Pair(p) => p.unroot(),
            Value::Vector(v) => v.unroot(),
            Value::Procedure(proc) => proc.unroot(),
            _ => {}
        }
    }

    fn finalize_glue(&self) {
        match self {
            Value::String(s) => s.finalize_glue(),
            Value::Symbol(s) => s.finalize_glue(),
            Value::Keyword(k) => k.finalize_glue(),
            Value::Pair(p) => p.finalize_glue(),
            Value::Vector(v) => v.finalize_glue(),
            Value::Procedure(proc) => proc.finalize_glue(),
            _ => {}
        }
    }
}

/// Manual implementation of Finalize for Value
///
/// No finalization needed - all cleanup is handled by Drop impls
impl gc::Finalize for Value {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_constructors() {
        assert!(Value::bool(true).is_bool());
        assert!(Value::integer(42).is_integer());
        assert!(Value::real(3.14).is_real());
        assert!(Value::char('a').is_char());
        assert!(Value::string("hello".to_string()).is_string());
        assert!(Value::symbol("foo").is_symbol());
        assert!(Value::Nil.is_nil());
    }

    #[test]
    fn test_truth_values() {
        assert!(!Value::Bool(false).is_true());
        assert!(Value::Bool(true).is_true());
        assert!(Value::Nil.is_true()); // nil is true in Scheme!
        assert!(Value::integer(0).is_true()); // 0 is true in Scheme!
    }

    #[test]
    fn test_cons() {
        let pair = Value::cons(Value::integer(1), Value::integer(2));
        assert!(pair.is_pair());
        assert!(pair.is_list());
    }

    #[test]
    fn test_vector() {
        let vec = Value::vector(vec![Value::integer(1), Value::integer(2), Value::integer(3)]);
        assert!(vec.is_vector());
    }

    #[test]
    fn test_equality_simple() {
        // Numbers
        assert!(Value::integer(42).eqv(&Value::integer(42)));
        assert!(!Value::integer(42).eqv(&Value::integer(43)));
        assert!(Value::real(3.14).eqv(&Value::real(3.14)));

        // Booleans
        assert!(Value::bool(true).eqv(&Value::bool(true)));
        assert!(!Value::bool(true).eqv(&Value::bool(false)));

        // Characters
        assert!(Value::char('a').eqv(&Value::char('a')));
        assert!(!Value::char('a').eqv(&Value::char('b')));

        // Symbols (compared by content for now)
        let sym1 = Value::symbol("foo");
        let sym2 = Value::symbol("foo");
        assert!(sym1.eqv(&sym2)); // Same symbol content

        // Nil
        assert!(Value::Nil.eqv(&Value::Nil));
    }

    #[test]
    fn test_equality_strings() {
        let s1 = Value::string("hello".to_string());
        let s2 = Value::string("hello".to_string());
        let s3 = Value::string("world".to_string());

        // eqv? is false for different string objects (even with same content)
        assert!(!s1.eqv(&s2));

        // equal? compares string content
        assert!(s1.equal(&s2));
        assert!(!s1.equal(&s3));
    }

    #[test]
    fn test_equality_lists() {
        let list1 = Value::cons(
            Value::integer(1),
            Value::cons(Value::integer(2), Value::Nil),
        );
        let list2 = Value::cons(
            Value::integer(1),
            Value::cons(Value::integer(2), Value::Nil),
        );
        let list3 = Value::cons(
            Value::integer(1),
            Value::cons(Value::integer(3), Value::Nil),
        );

        // eqv? is false for different pair objects
        assert!(!list1.eqv(&list2));

        // equal? compares list structure
        assert!(list1.equal(&list2));
        assert!(!list1.equal(&list3));
    }

    #[test]
    fn test_equality_vectors() {
        let vec1 = Value::vector(vec![Value::integer(1), Value::integer(2)]);
        let vec2 = Value::vector(vec![Value::integer(1), Value::integer(2)]);
        let vec3 = Value::vector(vec![Value::integer(1), Value::integer(3)]);

        // eqv? is false for different vector objects
        assert!(!vec1.eqv(&vec2));

        // equal? compares vector contents
        assert!(vec1.equal(&vec2));
        assert!(!vec1.equal(&vec3));
    }
}
