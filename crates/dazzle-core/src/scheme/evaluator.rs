//! Scheme evaluator (eval loop)
//!
//! Ported from OpenJade's `Interpreter.cxx` (~2,000 lines).
//!
//! ## Core Responsibilities
//!
//! 1. **Evaluate expressions** - Transform Values into results
//! 2. **Special forms** - Handle if, let, define, lambda, quote, etc.
//! 3. **Function application** - Call procedures with arguments
//! 4. **Tail call optimization** - Prevent stack overflow in recursive functions
//!
//! ## OpenJade Correspondence
//!
//! | Dazzle          | OpenJade                  | Purpose                    |
//! |-----------------|---------------------------|----------------------------|
//! | `Evaluator`     | `Interpreter`             | Main evaluator state       |
//! | `eval()`        | `Interpreter::eval()`     | Core eval loop             |
//! | `apply()`       | `Interpreter::apply()`    | Function application       |
//! | `eval_special()`| `Interpreter::evalXXX()`  | Special form handlers      |
//!
//! ## Evaluation Rules (R4RS)
//!
//! - **Self-evaluating**: Numbers, strings, booleans, characters → return as-is
//! - **Symbols**: Look up in environment
//! - **Lists**: First element determines behavior:
//!   - Special form keyword → handle specially
//!   - Otherwise → evaluate all elements, apply first to rest

use crate::scheme::environment::Environment;
use crate::scheme::parser::Position;
use crate::scheme::value::{Procedure, Value};
use crate::scheme::arena::{Arena, ValueId, ValueData};
use crate::grove::{Grove, Node};
use crate::fot::FotBuilder;
use gc::Gc;
use std::rc::Rc;
use std::cell::RefCell;

// Thread-local evaluator context for primitives
//
// Similar to OpenJade's approach, we use thread-local storage to give
// primitives access to the evaluator state (current node, grove, etc.)
// without changing all primitive signatures.
//
// This is safe because:
// 1. Scheme evaluation is single-threaded in our implementation
// 2. The context is set/cleared around each eval call
// 3. Primitives only run during evaluation
thread_local! {
    static EVALUATOR_CONTEXT: RefCell<Option<EvaluatorContext>> = RefCell::new(None);
}

/// Context available to primitives during evaluation
#[derive(Clone)]
pub struct EvaluatorContext {
    pub grove: Option<Rc<dyn Grove>>,
    pub current_node: Option<Rc<Box<dyn Node>>>,
    pub backend: Option<Rc<RefCell<dyn FotBuilder>>>,
}

/// Get the current evaluator context (for use in primitives)
pub fn get_evaluator_context() -> Option<EvaluatorContext> {
    EVALUATOR_CONTEXT.with(|ctx| ctx.borrow().clone())
}

/// Check if evaluator context is currently set
fn has_evaluator_context() -> bool {
    EVALUATOR_CONTEXT.with(|ctx| ctx.borrow().is_some())
}

/// Set the evaluator context (called by evaluator before eval)
fn set_evaluator_context(ctx: EvaluatorContext) {
    EVALUATOR_CONTEXT.with(|c| *c.borrow_mut() = Some(ctx));
}

/// Clear the evaluator context (called by evaluator after eval)
fn clear_evaluator_context() {
    EVALUATOR_CONTEXT.with(|c| *c.borrow_mut() = None);
}

// =============================================================================
// Call Stack (for error reporting)
// =============================================================================

use crate::scheme::value::SourceInfo;

/// A call stack frame
///
/// Tracks function calls for error reporting with source locations.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Function name (or "<lambda>" for anonymous functions)
    pub function_name: String,
    /// Source location (file:line:column)
    pub source: Option<SourceInfo>,
}

impl CallFrame {
    pub fn new(function_name: String, source: Option<SourceInfo>) -> Self {
        CallFrame {
            function_name,
            source,
        }
    }
}

// =============================================================================
// Evaluation Error
// =============================================================================

/// Evaluation error with call stack
#[derive(Debug, Clone)]
pub struct EvalError {
    pub message: String,
    pub call_stack: Vec<CallFrame>,
}

impl EvalError {
    pub fn new(message: String) -> Self {
        EvalError {
            message,
            call_stack: Vec::new(),
        }
    }

    /// Create error with call stack
    pub fn with_stack(message: String, call_stack: Vec<CallFrame>) -> Self {
        EvalError {
            message,
            call_stack,
        }
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // OpenJade-style format: file:line:col:E: message
        write!(f, "{}", self.message)?;

        // Show call stack in reverse order (innermost to outermost, matching OpenJade)
        for (i, frame) in self.call_stack.iter().rev().enumerate() {
            if let Some(ref source) = frame.source {
                // First frame gets a newline before it, rest don't
                if i == 0 {
                    writeln!(f, "\n{}:{}:{}:I: called from here",
                             source.file, source.pos.line, source.pos.column)?;
                } else {
                    writeln!(f, "{}:{}:{}:I: called from here",
                             source.file, source.pos.line, source.pos.column)?;
                }
            } else {
                if i == 0 {
                    writeln!(f, "\n{}:I: called from here", frame.function_name)?;
                } else {
                    writeln!(f, "{}:I: called from here", frame.function_name)?;
                }
            }
        }

        Ok(())
    }
}

impl std::error::Error for EvalError {}

pub type EvalResult = Result<Value, EvalError>;

// =============================================================================
// DSSSL Processing Mode (OpenJade ProcessingMode.h/ProcessingMode.cxx)
// =============================================================================

/// Construction rule for DSSSL processing
///
/// Corresponds to OpenJade's `ElementRule` + `Rule` + `Action`.
/// Stores the pattern (element name) and action (expression to evaluate).
#[derive(Clone)]
pub struct ConstructionRule {
    /// Element name pattern (GI) - the actual element being matched
    pub element_name: String,

    /// Context pattern - parent element names (empty for simple patterns)
    /// For `(part title)`, this would be vec!["part"]
    /// For simple `title`, this would be empty
    pub context: Vec<String>,

    /// Construction expression (returns sosofo when evaluated)
    pub expr: Value,

    /// Source position where this rule was defined (for error reporting)
    pub source_file: Option<String>,
    pub source_pos: Option<Position>,
}

/// Processing mode containing construction rules
///
/// Corresponds to OpenJade's `ProcessingMode` class.
/// Stores all element construction rules defined in the template.
pub struct ProcessingMode {
    /// Construction rules indexed by element name for O(1) lookup
    /// HashMap<element_name, Vec<rules_for_that_element>>
    /// This avoids linear search through all rules for every element.
    pub rules: std::collections::HashMap<String, Vec<ConstructionRule>>,

    /// Default construction rule (fallback when no specific rule matches)
    pub default_rule: Option<Value>,
}

impl ProcessingMode {
    /// Create a new empty processing mode
    pub fn new() -> Self {
        ProcessingMode {
            rules: std::collections::HashMap::new(),
            default_rule: None,
        }
    }

    /// Add a construction rule
    pub fn add_rule(&mut self, element_name: String, context: Vec<String>, expr: Value, source_file: Option<String>, source_pos: Option<Position>) {
        // Insert rule into HashMap, grouped by element name
        self.rules
            .entry(element_name.clone())
            .or_insert_with(Vec::new)
            .push(ConstructionRule {
                element_name,
                context,
                expr,
                source_file,
                source_pos
            });
    }

    /// Add a default construction rule
    pub fn add_default_rule(&mut self, expr: Value) {
        self.default_rule = Some(expr);
    }

    /// Find matching rule for an element
    ///
    /// Corresponds to OpenJade's `ProcessingMode::findMatch()`.
    /// Returns the first rule matching the given element name and context.
    ///
    /// OPTIMIZATION: Uses HashMap lookup by element name (O(1)) instead of
    /// linear search through all rules (O(N)). Critical for large documents!
    pub fn find_match(&self, gi: &str, node: &dyn crate::grove::Node) -> Option<&ConstructionRule> {
        // Fast path: lookup rules for this specific element name
        let rules_for_element = self.rules.get(gi)?;

        // Now search only among rules for this element (typically 1-5 rules)
        rules_for_element.iter().find(|rule| {
            // Element name already matches (we looked it up by GI)

            // If rule has no context, it matches any parent
            if rule.context.is_empty() {
                return true;
            }

            // Check if parent chain matches the context
            let mut current = node.parent();
            for expected_parent in rule.context.iter().rev() {
                match current {
                    Some(ref parent_node) => {
                        if let Some(parent_gi) = parent_node.gi() {
                            if parent_gi != *expected_parent {
                                return false;
                            }
                            current = parent_node.parent();
                        } else {
                            return false;
                        }
                    }
                    None => return false,
                }
            }

            true
        })
    }
}

/// Manager for multiple processing modes
///
/// DSSSL supports multiple named modes for different processing contexts.
/// The unnamed mode (empty string key) is the initial/default mode.
pub struct ModeManager {
    /// Map of mode name to processing mode
    modes: std::collections::HashMap<String, ProcessingMode>,
}

impl ModeManager {
    /// Create a new mode manager with an empty default mode
    pub fn new() -> Self {
        let mut modes = std::collections::HashMap::new();
        modes.insert(String::new(), ProcessingMode::new());
        ModeManager { modes }
    }

    /// Get or create a mode by name
    pub fn get_or_create_mode(&mut self, name: &str) -> &mut ProcessingMode {
        self.modes.entry(name.to_string()).or_insert_with(ProcessingMode::new)
    }

    /// Get a mode by name (read-only)
    pub fn get_mode(&self, name: &str) -> Option<&ProcessingMode> {
        self.modes.get(name)
    }

    /// Get the default (unnamed) mode
    pub fn default_mode(&mut self) -> &mut ProcessingMode {
        self.get_or_create_mode("")
    }

    /// Get the default (unnamed) mode (read-only)
    pub fn get_default_mode(&self) -> Option<&ProcessingMode> {
        self.get_mode("")
    }
}

// =============================================================================
// Evaluator
// =============================================================================

/// Scheme evaluator
///
/// Corresponds to OpenJade's `Interpreter` class.
///
/// ## Usage
///
/// ```ignore
/// let mut evaluator = Evaluator::new();
/// let result = evaluator.eval(expr, env)?;
/// ```
pub struct Evaluator {
    /// Arena for arena-based values (Phase 2 migration)
    ///
    /// Used for hot primitives (car, cdr, cons, null?, equal?) to eliminate Gc overhead.
    /// During Phase 2, this works in dual-mode: Values are converted to ValueIds for hot
    /// primitives, then converted back.
    arena: Arena,

    /// Manager for multiple processing modes
    ///
    /// Corresponds to OpenJade's mode management.
    /// DSSSL supports multiple named modes for different processing contexts.
    mode_manager: ModeManager,

    /// Current mode name for rule definition
    ///
    /// When defining rules with `(element ...)` or `(default ...)`, they go into this mode.
    /// Empty string means the unnamed/default mode.
    /// Set by `(mode name ...)` special form.
    current_mode: String,

    /// Current processing mode for rule lookup
    ///
    /// When processing nodes with `process-children`, rules are looked up in this mode.
    /// Empty string means the unnamed/default mode.
    /// Set by `(with-mode name ...)` special form.
    current_processing_mode: String,

    /// Backend for output generation (FotBuilder)
    ///
    /// This is used by the `make` special form to write flow objects to output.
    /// Wrapped in Rc<RefCell<>> to allow shared mutable access.
    backend: Option<Rc<RefCell<dyn FotBuilder>>>,

    /// Call stack for error reporting
    ///
    /// Tracks function calls with their source locations.
    /// Used to generate helpful error messages with clickable file paths.
    call_stack: Vec<CallFrame>,

    /// Current source file being evaluated (for error reporting)
    ///
    /// Set when loading templates, used to provide context in errors.
    current_source_file: Option<String>,

    /// Current position in source (for error reporting)
    ///
    /// Tracks line and column for the expression being evaluated.
    current_position: Option<Position>,

    /// Line mappings for translating output lines to source files
    ///
    /// When templates are loaded from XML wrappers that concatenate multiple files,
    /// this maps output line numbers to (source_file, source_line) pairs.
    /// Used to provide accurate file names and line numbers in error messages.
    line_mappings: Vec<LineMapping>,
}

/// Line mapping entry - maps a line number in concatenated code to its source file and line
#[derive(Debug, Clone)]
pub struct LineMapping {
    /// Line number in the concatenated output (1-indexed)
    pub output_line: usize,
    /// Source file path
    pub source_file: String,
    /// Line number in the source file (1-indexed)
    pub source_line: usize,
}

impl Evaluator {
    /// Create a new evaluator without a grove
    pub fn new() -> Self {
        Evaluator {
            arena: Arena::new(),
            mode_manager: ModeManager::new(),
            current_mode: String::new(), // Start with unnamed/default mode
            current_processing_mode: String::new(), // Start with unnamed/default mode
            backend: None,
            call_stack: Vec::new(),
            current_source_file: None,
            current_position: None,
            line_mappings: Vec::new(),
        }
    }

    /// Create a new evaluator with a grove
    pub fn with_grove(grove: Rc<dyn Grove>) -> Self {
        let mut arena = Arena::new();
        arena.grove = Some(grove);
        Evaluator {
            arena,
            mode_manager: ModeManager::new(),
            current_mode: String::new(), // Start with unnamed/default mode
            current_processing_mode: String::new(), // Start with unnamed/default mode
            backend: None,
            call_stack: Vec::new(),
            current_source_file: None,
            current_position: None,
            line_mappings: Vec::new(),
        }
    }

    /// Set line mappings for error reporting
    pub fn set_line_mappings(&mut self, mappings: Vec<LineMapping>) {
        self.line_mappings = mappings;
    }

    /// Set the current source file (for error reporting)
    pub fn set_source_file(&mut self, file: String) {
        self.current_source_file = Some(file);
    }

    /// Get the current source file
    pub fn source_file(&self) -> Option<&str> {
        self.current_source_file.as_deref()
    }

    /// Set the current position (for error reporting)
    pub fn set_position(&mut self, position: Position) {
        self.current_position = Some(position);
    }

    /// Push a call frame onto the stack
    fn push_call_frame(&mut self, function_name: String, source: Option<SourceInfo>) {
        self.call_stack.push(CallFrame::new(function_name, source));
    }

    /// Pop a call frame from the stack
    fn pop_call_frame(&mut self) {
        self.call_stack.pop();
    }

    /// Create an error with the current call stack and position
    fn error_with_stack(&self, message: String) -> EvalError {
        // Include current position in the error message (OpenJade format)
        let full_message = match (&self.current_source_file, &self.current_position) {
            (Some(file), Some(pos)) => {
                format!("{}:{}:{}:E: {}", file, pos.line, pos.column, message)
            }
            (Some(file), None) => {
                format!("{}:E: {}", file, message)
            }
            _ => message,
        };
        EvalError::with_stack(full_message, self.call_stack.clone())
    }

    /// Set the backend
    pub fn set_backend(&mut self, backend: Rc<RefCell<dyn FotBuilder>>) {
        self.backend = Some(backend);
    }

    /// Set the grove
    pub fn set_grove(&mut self, grove: Rc<dyn Grove>) {
        self.arena.grove = Some(grove);
    }

    /// Get the grove
    pub fn grove(&self) -> Option<&Rc<dyn Grove>> {
        self.arena.grove.as_ref()
    }

    /// Get static string name for a known primitive
    ///
    /// Returns a `'static str` for the primitive name if it's known,
    /// allowing it to be stored in a Procedure::Primitive value.
    fn get_primitive_static_name(&self, name: &str) -> Option<&'static str> {
        match name {
            // R4RS list primitives
            "cons" => Some("cons"),
            "car" => Some("car"),
            "cdr" => Some("cdr"),
            "cadr" => Some("cadr"),
            "caddr" => Some("caddr"),
            "cadddr" => Some("cadddr"),
            "list" => Some("list"),
            "length" => Some("length"),
            "append" => Some("append"),
            "reverse" => Some("reverse"),
            "list-ref" => Some("list-ref"),
            "list-tail" => Some("list-tail"),
            "member" => Some("member"),
            "memv" => Some("memv"),
            "memq" => Some("memq"),
            "assoc" => Some("assoc"),
            "assv" => Some("assv"),
            "assq" => Some("assq"),
            "null?" => Some("null?"),
            "pair?" => Some("pair?"),
            "list?" => Some("list?"),

            // R4RS predicates
            "boolean?" => Some("boolean?"),
            "symbol?" => Some("symbol?"),
            "char?" => Some("char?"),
            "string?" => Some("string?"),
            "number?" => Some("number?"),
            "integer?" => Some("integer?"),
            "real?" => Some("real?"),
            "exact?" => Some("exact?"),
            "inexact?" => Some("inexact?"),
            "procedure?" => Some("procedure?"),
            "vector?" => Some("vector?"),
            "zero?" => Some("zero?"),
            "positive?" => Some("positive?"),
            "negative?" => Some("negative?"),
            "odd?" => Some("odd?"),
            "even?" => Some("even?"),

            // R4RS comparison
            "eq?" => Some("eq?"),
            "eqv?" => Some("eqv?"),
            "equal?" => Some("equal?"),
            "=" => Some("="),
            "<" => Some("<"),
            ">" => Some(">"),
            "<=" => Some("<="),
            ">=" => Some(">="),

            // R4RS arithmetic
            "+" => Some("+"),
            "-" => Some("-"),
            "*" => Some("*"),
            "/" => Some("/"),
            "quotient" => Some("quotient"),
            "remainder" => Some("remainder"),
            "modulo" => Some("modulo"),
            "abs" => Some("abs"),
            "max" => Some("max"),
            "min" => Some("min"),
            "floor" => Some("floor"),
            "ceiling" => Some("ceiling"),
            "truncate" => Some("truncate"),
            "round" => Some("round"),
            "sqrt" => Some("sqrt"),
            "expt" => Some("expt"),
            "exp" => Some("exp"),
            "log" => Some("log"),
            "sin" => Some("sin"),
            "cos" => Some("cos"),
            "tan" => Some("tan"),
            "asin" => Some("asin"),
            "acos" => Some("acos"),
            "atan" => Some("atan"),
            "number->string" => Some("number->string"),
            "string->number" => Some("string->number"),

            // R4RS strings
            "string" => Some("string"),
            "string-append" => Some("string-append"),
            "substring" => Some("substring"),
            "string-ref" => Some("string-ref"),
            "string-length" => Some("string-length"),
            "string=?" => Some("string=?"),
            "string<?" => Some("string<?"),
            "string>?" => Some("string>?"),
            "string<=?" => Some("string<=?"),
            "string>=?" => Some("string>=?"),
            "string-ci=?" => Some("string-ci=?"),
            "string-ci<?" => Some("string-ci<?"),
            "string-ci>?" => Some("string-ci>?"),
            "string-ci<=?" => Some("string-ci<=?"),
            "string-ci>=?" => Some("string-ci>=?"),
            "string->list" => Some("string->list"),
            "list->string" => Some("list->string"),
            "symbol->string" => Some("symbol->string"),
            "string->symbol" => Some("string->symbol"),
            "keyword?" => Some("keyword?"),
            "keyword->string" => Some("keyword->string"),
            "string->keyword" => Some("string->keyword"),

            // R4RS characters
            "char=?" => Some("char=?"),
            "char<?" => Some("char<?"),
            "char>?" => Some("char>?"),
            "char<=?" => Some("char<=?"),
            "char>=?" => Some("char>=?"),
            "char-ci=?" => Some("char-ci=?"),
            "char-ci<?" => Some("char-ci<?"),
            "char-ci>?" => Some("char-ci>?"),
            "char-ci<=?" => Some("char-ci<=?"),
            "char-ci>=?" => Some("char-ci>=?"),
            "char-alphabetic?" => Some("char-alphabetic?"),
            "char-numeric?" => Some("char-numeric?"),
            "char-whitespace?" => Some("char-whitespace?"),
            "char-upper-case?" => Some("char-upper-case?"),
            "char-lower-case?" => Some("char-lower-case?"),
            "char-upcase" => Some("char-upcase"),
            "char-downcase" => Some("char-downcase"),
            "char->integer" => Some("char->integer"),
            "integer->char" => Some("integer->char"),
            "char-property" => Some("char-property"),
            "char-script-case" => Some("char-script-case"),

            // R4RS vectors
            "vector" => Some("vector"),
            "make-vector" => Some("make-vector"),
            "vector-ref" => Some("vector-ref"),
            "vector-set!" => Some("vector-set!"),
            "vector-length" => Some("vector-length"),
            "vector->list" => Some("vector->list"),
            "list->vector" => Some("list->vector"),
            "vector-fill!" => Some("vector-fill!"),

            // R4RS logic
            "not" => Some("not"),

            // R4RS I/O (excluding special forms)
            "display" => Some("display"),
            "write" => Some("write"),
            "newline" => Some("newline"),
            "read" => Some("read"),
            // Note: "load" is NOT included - it's a special form with eval_load

            // Note: R4RS higher-order "map", "for-each", "apply" are NOT included
            // They are special forms with eval_map, eval_for_each, eval_apply

            // DSSSL grove primitives
            "node?" => Some("node?"),
            "sosofo?" => Some("sosofo?"),
            "current-node" => Some("current-node"),
            "gi" => Some("gi"),
            "id" => Some("id"),
            "data" => Some("data"),
            "node-property" => Some("node-property"),
            "attribute-string" => Some("attribute-string"),
            "parent" => Some("parent"),
            "ancestor" => Some("ancestor"),
            "children" => Some("children"),
            "descendants" => Some("descendants"),
            "follow" => Some("follow"),
            "preced" => Some("preced"),
            "ipreced" => Some("ipreced"),
            "attributes" => Some("attributes"),
            "ancestors" => Some("ancestors"),
            "document-element" => Some("document-element"),
            "have-ancestor?" => Some("have-ancestor?"),
            "hierarchical-number" => Some("hierarchical-number"),
            "hierarchical-number-recursive" => Some("hierarchical-number-recursive"),
            "absolute-first-sibling?" => Some("absolute-first-sibling?"),
            "absolute-last-sibling?" => Some("absolute-last-sibling?"),
            "select-elements" => Some("select-elements"),
            "element-with-id" => Some("element-with-id"),
            "match-element?" => Some("match-element?"),
            "first-sibling?" => Some("first-sibling?"),
            "last-sibling?" => Some("last-sibling?"),
            "child-number" => Some("child-number"),
            "element-number" => Some("element-number"),
            "node-list?" => Some("node-list?"),
            "node-list-first" => Some("node-list-first"),
            "node-list-rest" => Some("node-list-rest"),
            "node-list-length" => Some("node-list-length"),
            "empty-node-list" => Some("empty-node-list"),
            "node-list-empty?" => Some("node-list-empty?"),
            "node-list-head" => Some("node-list-head"),
            "node-list-tail" => Some("node-list-tail"),
            "node-list-sublist" => Some("node-list-sublist"),
            "node-list-ref" => Some("node-list-ref"),
            "node-list-reduce" => Some("node-list-reduce"),
            "node-list-reduce-right" => Some("node-list-reduce-right"),
            "node-list-map" => Some("node-list-map"),
            "node-list-filter" => Some("node-list-filter"),
            "node-list-contains?" => Some("node-list-contains?"),
            "node-list-some?" => Some("node-list-some?"),
            "node-list-every?" => Some("node-list-every?"),
            "node-list->list" => Some("node-list->list"),
            "node-list-union" => Some("node-list-union"),
            "node-list-intersection" => Some("node-list-intersection"),
            "node-list-difference" => Some("node-list-difference"),
            "node-list-remove-duplicates" => Some("node-list-remove-duplicates"),
            "node-list-last" => Some("node-list-last"),
            "node-list-reverse" => Some("node-list-reverse"),

            // DSSSL processing
            // Note: "process-children" and "process-node-list" are NOT included
            // They are special forms with eval_process_children, eval_process_node_list
            "literal" => Some("literal"),
            "next-match" => Some("next-match"),
            "sosofo-append" => Some("sosofo-append"),
            "empty-sosofo" => Some("empty-sosofo"),
            "format-number" => Some("format-number"),
            "format-number-list" => Some("format-number-list"),

            // DSSSL entity/notation primitives
            "entity-system-id" => Some("entity-system-id"),
            "entity-public-id" => Some("entity-public-id"),
            "entity-text" => Some("entity-text"),
            "entity-type" => Some("entity-type"),
            "notation-system-id" => Some("notation-system-id"),
            "notation-public-id" => Some("notation-public-id"),

            // DSSSL quantity primitives (stubs)
            "quantity?" => Some("quantity?"),
            "quantity" => Some("quantity"),
            "quantity->number" => Some("quantity->number"),
            "number->quantity" => Some("number->quantity"),
            "quantity-convert" => Some("quantity-convert"),
            "device-length" => Some("device-length"),
            "label-distance" => Some("label-distance"),

            // DSSSL color primitives (stubs)
            "color?" => Some("color?"),
            "color" => Some("color"),
            "color-space?" => Some("color-space?"),
            "color-space" => Some("color-space"),

            // DSSSL address primitives (stubs)
            "address?" => Some("address?"),
            "address" => Some("address"),
            "address-local?" => Some("address-local?"),
            "address-visited?" => Some("address-visited?"),

            // DSSSL glyph primitives (stubs)
            "glyph-id?" => Some("glyph-id?"),
            "glyph-id" => Some("glyph-id"),
            "glyph-subst-table?" => Some("glyph-subst-table?"),
            "glyph-subst-table" => Some("glyph-subst-table"),
            "glyph-subst" => Some("glyph-subst"),

            // DSSSL spacing primitives (stubs)
            "display-space" => Some("display-space"),
            "inline-space" => Some("inline-space"),
            "display-space?" => Some("display-space?"),
            "inline-space?" => Some("inline-space?"),

            // OpenJade extensions
            "time" => Some("time"),
            "time->string" => Some("time->string"),
            "time<=?" => Some("time<=?"),
            "time<?" => Some("time<?"),
            "time>=?" => Some("time>=?"),
            "time>?" => Some("time>?"),
            "language?" => Some("language?"),
            "language" => Some("language"),
            "style?" => Some("style?"),
            "string-equiv?" => Some("string-equiv?"),
            "label-length" => Some("label-length"),
            "external-procedure" => Some("external-procedure"),
            "declaration" => Some("declaration"),
            "dtd" => Some("dtd"),
            "epilog" => Some("epilog"),
            "prolog" => Some("prolog"),
            "sgml-declaration" => Some("sgml-declaration"),
            "sgml-parse" => Some("sgml-parse"),
            "entity-address" => Some("entity-address"),
            "entity-generated-system-id" => Some("entity-generated-system-id"),
            "entity-name-normalize" => Some("entity-name-normalize"),
            "general-name-normalize" => Some("general-name-normalize"),
            "normalize" => Some("normalize"),
            "first-child-gi" => Some("first-child-gi"),
            "tree-root" => Some("tree-root"),
            "declare-default-language" => Some("declare-default-language"),
            "read-entity" => Some("read-entity"),
            "set-visited!" => Some("set-visited!"),
            "sosofo-contains-node?" => Some("sosofo-contains-node?"),
            "page-number-sosofo" => Some("page-number-sosofo"),
            "ifollow" => Some("ifollow"),
            "with-language" => Some("with-language"),
            "all-element-number" => Some("all-element-number"),
            "ancestor-child-number" => Some("ancestor-child-number"),
            "element-number-list" => Some("element-number-list"),
            "inherited-attribute-string" => Some("inherited-attribute-string"),
            "inherited-element-attribute-string" => Some("inherited-element-attribute-string"),
            "inherited-start-indent" => Some("inherited-start-indent"),
            "inherited-end-indent" => Some("inherited-end-indent"),
            "inherited-line-spacing" => Some("inherited-line-spacing"),
            "inherited-font-family-name" => Some("inherited-font-family-name"),
            "inherited-font-size" => Some("inherited-font-size"),
            "inherited-font-weight" => Some("inherited-font-weight"),
            "inherited-font-posture" => Some("inherited-font-posture"),
            "inherited-dbhtml-value" => Some("inherited-dbhtml-value"),
            "inherited-pi-value" => Some("inherited-pi-value"),
            "node-list" => Some("node-list"),
            "node-list=?" => Some("node-list=?"),
            "node-list-count" => Some("node-list-count"),
            "node-list-union-map" => Some("node-list-union-map"),
            "node-list-symmetrical-difference" => Some("node-list-symmetrical-difference"),
            "node-list-address" => Some("node-list-address"),
            "node-list-error" => Some("node-list-error"),
            "node-list-no-order" => Some("node-list-no-order"),
            "origin-to-subnode-rel-forest-addr" => Some("origin-to-subnode-rel-forest-addr"),
            "named-node" => Some("named-node"),
            "named-node-list?" => Some("named-node-list?"),
            "named-node-list-names" => Some("named-node-list-names"),
            "select-by-class" => Some("select-by-class"),
            "select-children" => Some("select-children"),
            "process-children-trim" => Some("process-children-trim"),
            "process-element-with-id" => Some("process-element-with-id"),
            "process-first-descendant" => Some("process-first-descendant"),
            "process-matching-children" => Some("process-matching-children"),

            // Additional utility primitives
            "add" => Some("add"),
            "divide" => Some("divide"),
            "equal" => Some("equal"),
            "char-eq" => Some("char-eq"),
            "char-lt" => Some("char-lt"),
            "error" => Some("error"),
            "eof-object?" => Some("eof-object?"),
            "debug" => Some("debug"),
            "current-language" => Some("current-language"),
            "current-mode" => Some("current-mode"),
            "current-node-address" => Some("current-node-address"),
            "current-node-page-number-sosofo" => Some("current-node-page-number-sosofo"),

            // Not a known primitive
            _ => None,
        }
    }

    // =========================================================================
    // Arena Conversion Layer (Phase 2 migration)
    // =========================================================================
    //
    // These functions convert between old Value and new ValueId.
    // During Phase 2, hot primitives use arena (ValueId), while the rest
    // of the system still uses Value. These converters bridge the gap.

    /// Convert Value to ValueId (for hot primitives)
    fn value_to_arena(&mut self, value: &Value) -> ValueId {
        use crate::scheme::arena::{NIL_ID, TRUE_ID, FALSE_ID};

        match value {
            Value::Nil => NIL_ID,
            Value::Bool(true) => TRUE_ID,
            Value::Bool(false) => FALSE_ID,
            Value::Integer(n) => self.arena.int(*n),
            Value::Real(f) => self.arena.real(*f),
            Value::Quantity { magnitude, unit } => {
                self.arena.alloc(ValueData::Quantity { magnitude: *magnitude, unit: *unit })
            }
            Value::String(s) => self.arena.string((**s).clone()),
            Value::Symbol(s) => self.arena.symbol(s.clone()),
            Value::Keyword(k) => self.arena.keyword(k.clone()),
            Value::Char(c) => self.arena.char(*c),
            Value::Pair(pair) => {
                let p = pair.borrow();
                let car = self.value_to_arena(&p.car);
                let cdr = self.value_to_arena(&p.cdr);
                if let Some(pos) = &p.pos {
                    self.arena.cons_with_pos(car, cdr, pos.clone())
                } else {
                    self.arena.cons(car, cdr)
                }
            }
            Value::Vector(vec) => {
                let v = vec.borrow();
                let elements: Vec<ValueId> = v.iter().map(|val| self.value_to_arena(val)).collect();
                self.arena.vector(elements)
            }
            Value::Node(node) => {
                self.arena.alloc(ValueData::Node(node.clone()))
            }
            Value::NodeList(node_list) => {
                self.arena.alloc(ValueData::NodeList(node_list.clone()))
            }
            Value::Sosofo => {
                self.arena.alloc(ValueData::Sosofo)
            }
            Value::Unspecified => {
                crate::scheme::arena::UNSPECIFIED_ID
            }
            _ => {
                // For now, unsupported types return NIL
                NIL_ID
            }
        }
    }

    /// Convert ValueId to Value (from hot primitives)
    fn arena_to_value(&self, id: ValueId) -> Value {
        use crate::scheme::arena::{NIL_ID, TRUE_ID, FALSE_ID};

        // Fast path for constants
        if id == NIL_ID {
            return Value::Nil;
        }
        if id == TRUE_ID {
            return Value::Bool(true);
        }
        if id == FALSE_ID {
            return Value::Bool(false);
        }

        match self.arena.get(id) {
            ValueData::Nil => Value::Nil,
            ValueData::Bool(b) => Value::Bool(*b),
            ValueData::Integer(n) => Value::Integer(*n),
            ValueData::Real(f) => Value::Real(*f),
            ValueData::Quantity { magnitude, unit } => {
                Value::Quantity { magnitude: *magnitude, unit: *unit }
            }
            ValueData::String(s) => Value::String(Gc::new(s.clone())),
            ValueData::Symbol(s) => Value::Symbol(s.clone()),
            ValueData::Keyword(k) => Value::Keyword(k.clone()),
            ValueData::Char(c) => Value::Char(*c),
            ValueData::Pair { car, cdr, pos } => {
                let car_val = self.arena_to_value(*car);
                let cdr_val = self.arena_to_value(*cdr);
                if let Some(p) = pos {
                    Value::cons_with_pos(car_val, cdr_val, p.clone())
                } else {
                    Value::cons(car_val, cdr_val)
                }
            }
            ValueData::Vector(elements) => {
                let vals: Vec<Value> = elements.iter().map(|id| self.arena_to_value(*id)).collect();
                Value::vector(vals)
            }
            ValueData::Node(node) => {
                Value::Node(node.clone())
            }
            ValueData::NodeList(node_list) => {
                Value::NodeList(node_list.clone())
            }
            ValueData::Sosofo => Value::Sosofo,
            ValueData::Unspecified => Value::Unspecified,
            ValueData::Error => Value::Error,
            ValueData::Procedure(_) => {
                // Procedures cannot be converted back to old-style values
                // This shouldn't happen in normal operation
                Value::Unspecified
            }
        }
    }

    /// Apply arena primitive (Phase 2+3 hot path)
    fn apply_primitive(&mut self, name: &str, args: &[Value]) -> EvalResult {
        use crate::scheme::primitives::{
            car, cdr, cons, null, equal,
            cadr, caddr, cadddr, list, length,
            reverse, append, list_p, list_ref,
            pair_p, number_p, integer_p, real_p,
            string_p, symbol_p, char_p, boolean_p,
            zero_p, positive_p, negative_p, odd_p, even_p,
            add, subtract, multiply, divide,
            quotient, remainder, modulo,
            num_eq, num_lt, num_gt, num_le, num_ge,
            abs, min, max,
            floor, ceiling, truncate, round,
            sqrt, sin, cos, tan,
            asin, acos, atan, exp, log, expt,
            string_length, string_ref, substring, string_append,
            string_eq, string_lt, string_gt, string_le, string_ge,
            string_ci_eq, string_ci_lt, string_ci_gt,
            string_ci_le, string_ci_ge,
            char_eq, char_lt, char_gt, char_le, char_ge,
            char_ci_eq, char_ci_lt, char_ci_gt, char_ci_le, char_ci_ge,
            char_upcase, char_downcase,
            char_alphabetic_p, char_numeric_p, char_whitespace_p,
            char_to_integer, integer_to_char,
            char_property, char_script_case,
            symbol_to_string, string_to_symbol,
            keyword_p, keyword_to_string, string_to_keyword,
            memq, memv, member,
            assq, assv, assoc,
            not, eq_p, eqv_p,
            caar, cdar, cddr,
            caaar, caadr, cadar,
            cdaar, cdadr, cddar, cdddr,
            vector, make_vector, vector_length,
            vector_ref, vector_set,
            vector_to_list, list_to_vector, vector_fill,
            vector_p, procedure_p,
            set_car, set_cdr, list_tail,
            string_upcase, string_downcase, case_fold_down,
            string_index,
            string_to_list, list_to_string,
            gcd, lcm,
            exact_to_inexact, inexact_to_exact,
            make_string, string, reverse_bang,
            string_set, string_copy, string_fill,
            char_lower_case_p, char_upper_case_p,
            last, last_pair, list_copy,
            append_bang, iota,
            take, drop, split_at,
            filter, remove,
            numerator, denominator, rationalize,
            angle, magnitude, string_to_number_radix,
            number_to_string_radix,
            null_list_p, improper_list_p, circular_list_p,
            bitwise_and, bitwise_ior, bitwise_xor, bitwise_not,
            arithmetic_shift, bit_extract,
            bitwise_bit_set_p, bitwise_bit_count,
            display, newline, write, write_char,
            read_char, eof_object_p,
            format_number, format_number_list,
            empty_sosofo, sosofo_append, if_first_page, if_front_page,
            current_node,
            gi, data, id,
            children, parent, attributes,
            node_list_p, empty_node_list, node_list_empty_p,
            node_list_length, node_list_first,
            attribute_string,
            node_list_rest, node_list_ref, node_list_reverse,
            node_p, sosofo_p, quantity_p,
            color_p, color, display_space_p, inline_space_p,
            quantity_to_number, number_to_quantity, quantity_convert,
            device_length, label_distance,
            ancestor, descendants, follow, preced, ipreced,
            node_list_last, node_list_union, node_list_intersection,
            node_list_difference, node_list_remove_duplicates,
            select_elements, first_sibling_p, last_sibling_p,
            child_number, element_with_id,
            element_number, hierarchical_number, hierarchical_number_recursive,
            ancestors, document_element, have_ancestor_p,
            match_element_p, node_list_map,
            node_property, absolute_first_sibling_p, absolute_last_sibling_p,
            node_list_to_list, node_list_contains_p,
            entity_system_id, entity_public_id, entity_type,
            notation_system_id, notation_public_id,
            current_language, current_mode, current_node_address,
            current_node_page_number_sosofo, debug,
            exact_p, inexact_p, error,
            address_p, address_local_p, address_visited_p,
            color_space_p, color_space, display_space, inline_space,
            glyph_id_p, glyph_id, glyph_subst_table_p,
            glyph_subst_table, glyph_subst,
            time, time_to_string, time_le, time_lt,
            time_ge, time_gt,
            language_p, language, style_p,
            string_equiv_p, label_length, external_procedure,
            declaration, dtd, epilog, prolog,
            sgml_declaration, sgml_parse,
            entity_address, entity_generated_system_id,
            entity_name_normalize, general_name_normalize, normalize,
            first_child_gi, tree_root, declare_default_language,
            read_entity, set_visited,
            sosofo_contains_node_p, page_number_sosofo, ifollow, with_language,
            all_element_number, ancestor_child_number, element_number_list,
            inherited_attribute_string, inherited_element_attribute_string,
            inherited_start_indent, inherited_end_indent, inherited_line_spacing,
            inherited_font_family_name, inherited_font_size, inherited_font_weight,
            inherited_font_posture, inherited_dbhtml_value, inherited_pi_value,
            node_list, node_list_eq_p,
            node_list_union_map, node_list_symmetrical_difference, node_list_count,
            node_list_address, node_list_error, node_list_no_order,
            origin_to_subnode_rel_forest_addr,
            named_node, named_node_list_p, named_node_list_names,
            select_by_class, select_children,
            process_children_trim, process_element_with_id, process_first_descendant,
            process_matching_children, next_match,
        };

        // Special handling for eq? and eqv? - check pointer equality at Value level
        // to preserve identity semantics when converting from Value to ValueId
        if (name == "eq?" || name == "eqv?") && args.len() == 2 {
            // Check if the two Values are pointer-equal (same object)
            let ptr_equal = match (&args[0], &args[1]) {
                (Value::Pair(p1), Value::Pair(p2)) => gc::Gc::ptr_eq(p1, p2),
                (Value::String(s1), Value::String(s2)) => gc::Gc::ptr_eq(s1, s2),
                (Value::Procedure(pr1), Value::Procedure(pr2)) => gc::Gc::ptr_eq(pr1, pr2),
                _ => false,
            };
            if ptr_equal {
                return Ok(Value::bool(true));
            }
        }

        // Special handling for type predicates that check types not convertible to arena
        if name == "vector?" && args.len() == 1 {
            return Ok(Value::bool(matches!(args[0], Value::Vector(_))));
        }
        if name == "procedure?" && args.len() == 1 {
            return Ok(Value::bool(matches!(args[0], Value::Procedure(_))));
        }

        // Convert args to arena
        let arena_args: Vec<ValueId> = args.iter().map(|v| self.value_to_arena(v)).collect();

        // Call arena primitive
        let result_id = match name {
            "car" => car(&self.arena, &arena_args),
            "cdr" => cdr(&self.arena, &arena_args),
            "cons" => cons(&mut self.arena, &arena_args),
            "null?" => null(&self.arena, &arena_args),
            "equal?" => equal(&self.arena, &arena_args),
            "cadr" => cadr(&self.arena, &arena_args),
            "caddr" => caddr(&self.arena, &arena_args),
            "cadddr" => cadddr(&self.arena, &arena_args),
            "list" => list(&mut self.arena, &arena_args),
            "length" => length(&mut self.arena, &arena_args),
            "reverse" => reverse(&mut self.arena, &arena_args),
            "append" => append(&mut self.arena, &arena_args),
            "list?" => list_p(&self.arena, &arena_args),
            "list-ref" => list_ref(&self.arena, &arena_args),
            "pair?" => pair_p(&self.arena, &arena_args),
            "number?" => number_p(&self.arena, &arena_args),
            "integer?" => integer_p(&self.arena, &arena_args),
            "real?" => real_p(&self.arena, &arena_args),
            "string?" => string_p(&self.arena, &arena_args),
            "symbol?" => symbol_p(&self.arena, &arena_args),
            "char?" => char_p(&self.arena, &arena_args),
            "boolean?" => boolean_p(&self.arena, &arena_args),
            "zero?" => zero_p(&self.arena, &arena_args),
            "positive?" => positive_p(&self.arena, &arena_args),
            "negative?" => negative_p(&self.arena, &arena_args),
            "odd?" => odd_p(&self.arena, &arena_args),
            "even?" => even_p(&self.arena, &arena_args),
            "+" => add(&mut self.arena, &arena_args),
            "-" => subtract(&mut self.arena, &arena_args),
            "*" => multiply(&mut self.arena, &arena_args),
            "/" => divide(&mut self.arena, &arena_args),
            "quotient" => quotient(&mut self.arena, &arena_args),
            "remainder" => remainder(&mut self.arena, &arena_args),
            "modulo" => modulo(&mut self.arena, &arena_args),
            "=" => num_eq(&self.arena, &arena_args),
            "<" => num_lt(&self.arena, &arena_args),
            ">" => num_gt(&self.arena, &arena_args),
            "<=" => num_le(&self.arena, &arena_args),
            ">=" => num_ge(&self.arena, &arena_args),
            "abs" => abs(&mut self.arena, &arena_args),
            "min" => min(&mut self.arena, &arena_args),
            "max" => max(&mut self.arena, &arena_args),
            "floor" => floor(&mut self.arena, &arena_args),
            "ceiling" => ceiling(&mut self.arena, &arena_args),
            "truncate" => truncate(&mut self.arena, &arena_args),
            "round" => round(&mut self.arena, &arena_args),
            "sqrt" => sqrt(&mut self.arena, &arena_args),
            "sin" => sin(&mut self.arena, &arena_args),
            "cos" => cos(&mut self.arena, &arena_args),
            "tan" => tan(&mut self.arena, &arena_args),
            "asin" => asin(&mut self.arena, &arena_args),
            "acos" => acos(&mut self.arena, &arena_args),
            "atan" => atan(&mut self.arena, &arena_args),
            "exp" => exp(&mut self.arena, &arena_args),
            "log" => log(&mut self.arena, &arena_args),
            "expt" => expt(&mut self.arena, &arena_args),
            "string-length" => string_length(&mut self.arena, &arena_args),
            "string-ref" => string_ref(&mut self.arena, &arena_args),
            "substring" => substring(&mut self.arena, &arena_args),
            "string-append" => string_append(&mut self.arena, &arena_args),
            "string=?" => string_eq(&self.arena, &arena_args),
            "string<?" => string_lt(&self.arena, &arena_args),
            "string>?" => string_gt(&self.arena, &arena_args),
            "string<=?" => string_le(&self.arena, &arena_args),
            "string>=?" => string_ge(&self.arena, &arena_args),
            "string-ci=?" => string_ci_eq(&self.arena, &arena_args),
            "string-ci<?" => string_ci_lt(&self.arena, &arena_args),
            "string-ci>?" => string_ci_gt(&self.arena, &arena_args),
            "string-ci<=?" => string_ci_le(&self.arena, &arena_args),
            "string-ci>=?" => string_ci_ge(&self.arena, &arena_args),

            "char=?" => char_eq(&self.arena, &arena_args),
            "char<?" => char_lt(&self.arena, &arena_args),
            "char>?" => char_gt(&self.arena, &arena_args),
            "char<=?" => char_le(&self.arena, &arena_args),
            "char>=?" => char_ge(&self.arena, &arena_args),
            "char-ci=?" => char_ci_eq(&self.arena, &arena_args),
            "char-ci<?" => char_ci_lt(&self.arena, &arena_args),
            "char-ci>?" => char_ci_gt(&self.arena, &arena_args),
            "char-ci<=?" => char_ci_le(&self.arena, &arena_args),
            "char-ci>=?" => char_ci_ge(&self.arena, &arena_args),
            "char-upcase" => char_upcase(&mut self.arena, &arena_args),
            "char-downcase" => char_downcase(&mut self.arena, &arena_args),
            "char-alphabetic?" => char_alphabetic_p(&self.arena, &arena_args),
            "char-numeric?" => char_numeric_p(&self.arena, &arena_args),
            "char-whitespace?" => char_whitespace_p(&self.arena, &arena_args),
            "char->integer" => char_to_integer(&mut self.arena, &arena_args),
            "integer->char" => integer_to_char(&mut self.arena, &arena_args),
            "char-property" => char_property(&mut self.arena, &arena_args),
            "char-script-case" => char_script_case(&mut self.arena, &arena_args),

            "symbol->string" => symbol_to_string(&mut self.arena, &arena_args),
            "string->symbol" => string_to_symbol(&mut self.arena, &arena_args),
            "keyword?" => keyword_p(&self.arena, &arena_args),
            "keyword->string" => keyword_to_string(&mut self.arena, &arena_args),
            "string->keyword" => string_to_keyword(&mut self.arena, &arena_args),

            "memq" => memq(&self.arena, &arena_args),
            "memv" => memv(&self.arena, &arena_args),
            "member" => member(&self.arena, &arena_args),
            "assq" => assq(&self.arena, &arena_args),
            "assv" => assv(&self.arena, &arena_args),
            "assoc" => assoc(&self.arena, &arena_args),

            "not" => not(&self.arena, &arena_args),
            "eq?" => eq_p(&self.arena, &arena_args),
            "eqv?" => eqv_p(&self.arena, &arena_args),
            "caar" => caar(&self.arena, &arena_args),
            "cdar" => cdar(&self.arena, &arena_args),
            "cddr" => cddr(&self.arena, &arena_args),

            "caaar" => caaar(&self.arena, &arena_args),
            "caadr" => caadr(&self.arena, &arena_args),
            "cadar" => cadar(&self.arena, &arena_args),
            "cdaar" => cdaar(&self.arena, &arena_args),
            "cdadr" => cdadr(&self.arena, &arena_args),
            "cddar" => cddar(&self.arena, &arena_args),
            "cdddr" => cdddr(&self.arena, &arena_args),

            "vector" => vector(&mut self.arena, &arena_args),
            "make-vector" => make_vector(&mut self.arena, &arena_args),
            "vector-length" => vector_length(&mut self.arena, &arena_args),
            "vector-ref" => vector_ref(&self.arena, &arena_args),
            "vector-set!" => vector_set(&mut self.arena, &arena_args),
            "vector->list" => vector_to_list(&mut self.arena, &arena_args),
            "list->vector" => list_to_vector(&mut self.arena, &arena_args),
            "vector-fill!" => vector_fill(&mut self.arena, &arena_args),

            "vector?" => vector_p(&self.arena, &arena_args),
            "procedure?" => procedure_p(&self.arena, &arena_args),
            "set-car!" => set_car(&mut self.arena, &arena_args),
            "set-cdr!" => set_cdr(&mut self.arena, &arena_args),
            "list-tail" => list_tail(&self.arena, &arena_args),

            "string-upcase" => string_upcase(&mut self.arena, &arena_args),
            "string-downcase" => string_downcase(&mut self.arena, &arena_args),
            "case-fold-down" => case_fold_down(&mut self.arena, &arena_args), // DSSSL alias for string-downcase
            "string-index" => string_index(&mut self.arena, &arena_args),
            "string->number" => string_to_number_radix(&mut self.arena, &arena_args), // Updated to support radix
            "number->string" => number_to_string_radix(&mut self.arena, &arena_args), // Updated to support radix
            "string->list" => string_to_list(&mut self.arena, &arena_args),
            "list->string" => list_to_string(&mut self.arena, &arena_args),

            "gcd" => gcd(&mut self.arena, &arena_args),
            "lcm" => lcm(&mut self.arena, &arena_args),
            "exact->inexact" => exact_to_inexact(&mut self.arena, &arena_args),
            "inexact->exact" => inexact_to_exact(&mut self.arena, &arena_args),
            "make-string" => make_string(&mut self.arena, &arena_args),
            "string" => string(&mut self.arena, &arena_args),
            "reverse!" => reverse_bang(&mut self.arena, &arena_args),

            "string-set!" => string_set(&mut self.arena, &arena_args),
            "string-copy" => string_copy(&mut self.arena, &arena_args),
            "string-fill!" => string_fill(&mut self.arena, &arena_args),
            "char-lower-case?" => char_lower_case_p(&self.arena, &arena_args),
            "char-upper-case?" => char_upper_case_p(&self.arena, &arena_args),

            "last" => last(&self.arena, &arena_args),
            "last-pair" => last_pair(&self.arena, &arena_args),
            "list-copy" => list_copy(&mut self.arena, &arena_args),
            "append!" => append_bang(&mut self.arena, &arena_args),
            "iota" => iota(&mut self.arena, &arena_args),

            "take" => take(&mut self.arena, &arena_args),
            "drop" => drop(&self.arena, &arena_args),
            "split-at" => split_at(&mut self.arena, &arena_args),
            "filter" => filter(&self.arena, &arena_args),
            "remove" => remove(&self.arena, &arena_args),

            "numerator" => numerator(&mut self.arena, &arena_args),
            "denominator" => denominator(&mut self.arena, &arena_args),
            "rationalize" => rationalize(&self.arena, &arena_args),
            "angle" => angle(&mut self.arena, &arena_args),
            "magnitude" => magnitude(&mut self.arena, &arena_args),

            "null-list?" => null_list_p(&self.arena, &arena_args),
            "improper-list?" => improper_list_p(&self.arena, &arena_args),
            "circular-list?" => circular_list_p(&self.arena, &arena_args),

            "bitwise-and" => bitwise_and(&mut self.arena, &arena_args),
            "bitwise-ior" => bitwise_ior(&mut self.arena, &arena_args),
            "bitwise-xor" => bitwise_xor(&mut self.arena, &arena_args),
            "bitwise-not" => bitwise_not(&mut self.arena, &arena_args),
            "arithmetic-shift" => arithmetic_shift(&mut self.arena, &arena_args),
            "bit-extract" => bit_extract(&mut self.arena, &arena_args),
            "bitwise-bit-set?" => bitwise_bit_set_p(&self.arena, &arena_args),
            "bitwise-bit-count" => bitwise_bit_count(&mut self.arena, &arena_args),

            "display" => display(&self.arena, &arena_args),
            "newline" => newline(&self.arena, &arena_args),
            "write" => write(&self.arena, &arena_args),
            "write-char" => write_char(&self.arena, &arena_args),
            "read-char" => read_char(&self.arena, &arena_args),
            "eof-object?" => eof_object_p(&self.arena, &arena_args),

            "format-number" => format_number(&mut self.arena, &arena_args),
            "format-number-list" => format_number_list(&mut self.arena, &arena_args),

            "empty-sosofo" => empty_sosofo(&mut self.arena, &arena_args),
            "sosofo-append" => sosofo_append(&mut self.arena, &arena_args),
            "if-first-page" => if_first_page(&mut self.arena, &arena_args),
            "if-front-page" => if_front_page(&mut self.arena, &arena_args),

            "current-node" => current_node(&mut self.arena, &arena_args),

            "gi" => gi(&mut self.arena, &arena_args),
            "data" => data(&mut self.arena, &arena_args),
            "id" => id(&mut self.arena, &arena_args),

            "children" => children(&mut self.arena, &arena_args),
            "parent" => parent(&mut self.arena, &arena_args),
            "attributes" => attributes(&mut self.arena, &arena_args),

            "node-list?" => node_list_p(&self.arena, &arena_args),
            "empty-node-list" => empty_node_list(&mut self.arena, &arena_args),
            "node-list-empty?" => node_list_empty_p(&self.arena, &arena_args),
            "node-list-length" => node_list_length(&mut self.arena, &arena_args),
            "node-list-first" => node_list_first(&mut self.arena, &arena_args),

            "attribute-string" => attribute_string(&mut self.arena, &arena_args),

            "node-list-rest" => node_list_rest(&mut self.arena, &arena_args),
            "node-list-ref" => node_list_ref(&mut self.arena, &arena_args),
            "node-list-reverse" => node_list_reverse(&mut self.arena, &arena_args),

            "node?" => node_p(&self.arena, &arena_args),
            "sosofo?" => sosofo_p(&self.arena, &arena_args),
            "quantity?" => quantity_p(&self.arena, &arena_args),

            "color?" => color_p(&self.arena, &arena_args),
            "color" => color(&mut self.arena, &arena_args),
            "display-space?" => display_space_p(&self.arena, &arena_args),
            "inline-space?" => inline_space_p(&self.arena, &arena_args),

            "quantity->number" => quantity_to_number(&mut self.arena, &arena_args),
            "number->quantity" => number_to_quantity(&mut self.arena, &arena_args),
            "quantity-convert" => quantity_convert(&mut self.arena, &arena_args),
            "device-length" => device_length(&mut self.arena, &arena_args),
            "label-distance" => label_distance(&mut self.arena, &arena_args),

            "ancestor" => ancestor(&mut self.arena, &arena_args),
            "descendants" => descendants(&mut self.arena, &arena_args),
            "follow" => follow(&mut self.arena, &arena_args),
            "preced" => preced(&mut self.arena, &arena_args),
            "ipreced" => ipreced(&mut self.arena, &arena_args),

            "node-list-last" => node_list_last(&mut self.arena, &arena_args),
            "node-list-union" => node_list_union(&mut self.arena, &arena_args),
            "node-list-intersection" => node_list_intersection(&mut self.arena, &arena_args),
            "node-list-difference" => node_list_difference(&mut self.arena, &arena_args),
            "node-list-remove-duplicates" => node_list_remove_duplicates(&mut self.arena, &arena_args),

            "select-elements" => select_elements(&mut self.arena, &arena_args),
            "first-sibling?" => first_sibling_p(&mut self.arena, &arena_args),
            "last-sibling?" => last_sibling_p(&mut self.arena, &arena_args),
            "child-number" => child_number(&mut self.arena, &arena_args),
            "element-with-id" => element_with_id(&mut self.arena, &arena_args),

            "element-number" => element_number(&mut self.arena, &arena_args),
            "hierarchical-number" => hierarchical_number(&mut self.arena, &arena_args),
            "hierarchical-number-recursive" => hierarchical_number_recursive(&mut self.arena, &arena_args),

            "ancestors" => ancestors(&mut self.arena, &arena_args),
            "document-element" => document_element(&mut self.arena, &arena_args),
            "have-ancestor?" => have_ancestor_p(&mut self.arena, &arena_args),
            "match-element?" => match_element_p(&mut self.arena, &arena_args),
            "node-list-map" => node_list_map(&mut self.arena, &arena_args),

            "node-property" => node_property(&mut self.arena, &arena_args),
            "absolute-first-sibling?" => absolute_first_sibling_p(&mut self.arena, &arena_args),
            "absolute-last-sibling?" => absolute_last_sibling_p(&mut self.arena, &arena_args),
            "node-list->list" => node_list_to_list(&mut self.arena, &arena_args),
            "node-list-contains?" => node_list_contains_p(&mut self.arena, &arena_args),

            "entity-system-id" => entity_system_id(&mut self.arena, &arena_args),
            "entity-public-id" => entity_public_id(&mut self.arena, &arena_args),
            "entity-type" => entity_type(&mut self.arena, &arena_args),
            "notation-system-id" => notation_system_id(&mut self.arena, &arena_args),
            "notation-public-id" => notation_public_id(&mut self.arena, &arena_args),

            "current-language" => current_language(&self.arena, &arena_args),
            "current-mode" => current_mode(&self.arena, &arena_args),
            "current-node-address" => current_node_address(&self.arena, &arena_args),
            "current-node-page-number-sosofo" => current_node_page_number_sosofo(&mut self.arena, &arena_args),
            "debug" => debug(&self.arena, &arena_args),

            "add" => add(&mut self.arena, &arena_args),
            "divide" => divide(&mut self.arena, &arena_args),
            "equal" => equal(&self.arena, &arena_args),
            "char-eq" => char_eq(&self.arena, &arena_args),
            "char-lt" => char_lt(&self.arena, &arena_args),
            "exact?" => exact_p(&self.arena, &arena_args),
            "inexact?" => inexact_p(&self.arena, &arena_args),
            "error" => error(&mut self.arena, &arena_args),
            "address?" => address_p(&self.arena, &arena_args),
            "address-local?" => address_local_p(&self.arena, &arena_args),
            "address-visited?" => address_visited_p(&self.arena, &arena_args),
            "color-space?" => color_space_p(&self.arena, &arena_args),
            "color-space" => color_space(&self.arena, &arena_args),
            "display-space" => display_space(&mut self.arena, &arena_args),
            "inline-space" => inline_space(&mut self.arena, &arena_args),
            "glyph-id?" => glyph_id_p(&self.arena, &arena_args),
            "glyph-id" => glyph_id(&self.arena, &arena_args),
            "glyph-subst-table?" => glyph_subst_table_p(&self.arena, &arena_args),
            "glyph-subst-table" => glyph_subst_table(&self.arena, &arena_args),
            "glyph-subst" => glyph_subst(&self.arena, &arena_args),
            "time" => time(&self.arena, &arena_args),
            "time->string" => time_to_string(&mut self.arena, &arena_args),
            "time<=?" => time_le(&self.arena, &arena_args),
            "time<?" => time_lt(&self.arena, &arena_args),
            "time>=?" => time_ge(&self.arena, &arena_args),
            "time>?" => time_gt(&self.arena, &arena_args),
            "language?" => language_p(&self.arena, &arena_args),
            "language" => language(&mut self.arena, &arena_args),
            "style?" => style_p(&self.arena, &arena_args),
            "string-equiv?" => string_equiv_p(&self.arena, &arena_args),
            "label-length" => label_length(&mut self.arena, &arena_args),
            "external-procedure" => external_procedure(&mut self.arena, &arena_args),
            "declaration" => declaration(&self.arena, &arena_args),
            "dtd" => dtd(&self.arena, &arena_args),
            "epilog" => epilog(&self.arena, &arena_args),
            "prolog" => prolog(&self.arena, &arena_args),
            "sgml-declaration" => sgml_declaration(&self.arena, &arena_args),
            "sgml-parse" => sgml_parse(&self.arena, &arena_args),
            "entity-address" => entity_address(&self.arena, &arena_args),
            "entity-generated-system-id" => entity_generated_system_id(&mut self.arena, &arena_args),
            "entity-name-normalize" => entity_name_normalize(&mut self.arena, &arena_args),
            "general-name-normalize" => general_name_normalize(&mut self.arena, &arena_args),
            "normalize" => normalize(&mut self.arena, &arena_args),
            "first-child-gi" => first_child_gi(&mut self.arena, &arena_args),
            "tree-root" => tree_root(&mut self.arena, &arena_args),
            "declare-default-language" => declare_default_language(&self.arena, &arena_args),
            "read-entity" => read_entity(&mut self.arena, &arena_args),
            "set-visited!" => set_visited(&self.arena, &arena_args),
            "sosofo-contains-node?" => sosofo_contains_node_p(&self.arena, &arena_args),
            "page-number-sosofo" => page_number_sosofo(&mut self.arena, &arena_args),
            "ifollow" => ifollow(&self.arena, &arena_args),
            "with-language" => with_language(&self.arena, &arena_args),
            "all-element-number" => all_element_number(&mut self.arena, &arena_args),
            "ancestor-child-number" => ancestor_child_number(&mut self.arena, &arena_args),
            "element-number-list" => element_number_list(&self.arena, &arena_args),
            "inherited-attribute-string" => inherited_attribute_string(&mut self.arena, &arena_args),
            "inherited-element-attribute-string" => inherited_element_attribute_string(&mut self.arena, &arena_args),
            "inherited-start-indent" => inherited_start_indent(&mut self.arena, &arena_args),
            "inherited-end-indent" => inherited_end_indent(&mut self.arena, &arena_args),
            "inherited-line-spacing" => inherited_line_spacing(&mut self.arena, &arena_args),
            "inherited-font-family-name" => inherited_font_family_name(&mut self.arena, &arena_args),
            "inherited-font-size" => inherited_font_size(&mut self.arena, &arena_args),
            "inherited-font-weight" => inherited_font_weight(&mut self.arena, &arena_args),
            "inherited-font-posture" => inherited_font_posture(&mut self.arena, &arena_args),
            "inherited-dbhtml-value" => inherited_dbhtml_value(&mut self.arena, &arena_args),
            "inherited-pi-value" => inherited_pi_value(&mut self.arena, &arena_args),
            "node-list" => node_list(&mut self.arena, &arena_args),
            "node-list=?" => node_list_eq_p(&self.arena, &arena_args),
            "node-list-count" => node_list_count(&mut self.arena, &arena_args),
            "node-list-union-map" => node_list_union_map(&self.arena, &arena_args),
            "node-list-symmetrical-difference" => node_list_symmetrical_difference(&self.arena, &arena_args),
            "node-list-address" => node_list_address(&self.arena, &arena_args),
            "node-list-error" => node_list_error(&mut self.arena, &arena_args),
            "node-list-no-order" => node_list_no_order(&self.arena, &arena_args),
            "origin-to-subnode-rel-forest-addr" => origin_to_subnode_rel_forest_addr(&self.arena, &arena_args),
            "named-node" => named_node(&self.arena, &arena_args),
            "named-node-list?" => named_node_list_p(&self.arena, &arena_args),
            "named-node-list-names" => named_node_list_names(&self.arena, &arena_args),
            "select-by-class" => select_by_class(&self.arena, &arena_args),
            "select-children" => select_children(&self.arena, &arena_args),
            "process-children-trim" => process_children_trim(&self.arena, &arena_args),
            "process-element-with-id" => process_element_with_id(&self.arena, &arena_args),
            "process-first-descendant" => process_first_descendant(&self.arena, &arena_args),
            "process-matching-children" => process_matching_children(&self.arena, &arena_args),
            "next-match" => next_match(&self.arena, &arena_args),

            // Special handling for literal - it's not an arena primitive
            "literal" => {
                // literal can take 1 or 2 arguments:
                // (literal "text") or (literal data: "text")
                // For now, we just extract the text from the first string argument
                if args.is_empty() {
                    return Err(self.error_with_stack("literal: expected at least 1 argument".to_string()));
                }

                // Find the text argument - could be first arg or after a keyword
                let text = if args.len() == 1 {
                    // (literal "text")
                    match &args[0] {
                        Value::String(s) => s.to_string(),
                        _ => return Err(self.error_with_stack("literal: argument must be a string".to_string())),
                    }
                } else if args.len() == 2 {
                    // (literal data: "text") - second arg is the text
                    match &args[1] {
                        Value::String(s) => s.to_string(),
                        _ => return Err(self.error_with_stack("literal: text argument must be a string".to_string())),
                    }
                } else {
                    return Err(self.error_with_stack(format!(
                        "literal: expected 1 or 2 arguments, got {}",
                        args.len()
                    )));
                };

                if let Some(ref backend) = self.backend {
                    backend.borrow_mut().formatting_instruction(&text)
                        .map_err(|e| self.error_with_stack(format!("Backend error: {}", e)))?;
                }

                return Ok(Value::Unspecified);
            }

            _ => unreachable!("apply_primitive called with non-arena primitive: {}", name),
        }
        .map_err(|e| self.error_with_stack(e))?;

        // Convert result back to Value
        Ok(self.arena_to_value(result_id))
    }

    /// Set the current node
    pub fn set_current_node(&mut self, node: Box<dyn Node>) {
        self.arena.current_node = Some(Rc::new(node));
    }

    /// Get the current node
    pub fn current_node(&self) -> Option<Rc<Box<dyn Node>>> {
        self.arena.current_node.clone()
    }

    /// Clear the current node
    pub fn clear_current_node(&mut self) {
        self.arena.current_node = None;
    }

    /// Restore current node from saved state
    pub fn restore_current_node(&mut self, node: Option<Rc<Box<dyn Node>>>) {
        self.arena.current_node = node;
    }

    // =========================================================================
    // DSSSL Processing (OpenJade ProcessContext.cxx)
    // =========================================================================

    /// Start DSSSL processing from the root node
    ///
    /// Corresponds to OpenJade's `ProcessContext::process()`.
    /// After template loading, this triggers automatic tree processing.
    pub fn process_root(&mut self, env: Gc<Environment>) -> EvalResult {
        // Get the root node from the grove
        let root_node = match self.grove() {
            Some(grove) => grove.root(),
            None => return Err(EvalError::new("No grove set".to_string())),
        };

        // Set as current node and start processing
        self.set_current_node(root_node);
        self.process_node(env)
    }

    /// Process the current node
    ///
    /// Corresponds to OpenJade's `ProcessContext::processNode()`.
    ///
    /// ## Algorithm (from OpenJade):
    /// 1. If character data node, output directly
    /// 2. If element node:
    ///    a. Find matching construction rule by GI
    ///    b. If rule found, evaluate it (returns sosofo)
    ///    c. If no rule, default behavior: process-children
    pub fn process_node(&mut self, env: Gc<Environment>) -> EvalResult {
        let node = match self.current_node() {
            Some(n) => n.clone(),
            None => return Err(EvalError::new("No current node".to_string())),
        };

        // Get element name (GI)
        let gi = match node.gi() {
            Some(gi) => gi.to_string(),
            None => {
                // Not an element (e.g., text node, comment, etc.)
                // For text nodes, output their data content
                // Skip whitespace-only text nodes (OpenJade behavior)
                if node.is_text() {
                    if let Some(text) = node.data() {
                        // Skip if text is only whitespace
                        if !text.trim().is_empty() {
                            // Output text to backend using literal() (each backend handles its own escaping)
                            if let Some(ref backend) = self.backend {
                                backend.borrow_mut().literal(&text)
                                    .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                            }
                        }
                    }
                }
                return Ok(Value::Unspecified);
            }
        };

        // Find matching construction rule (in current processing mode)
        let mode_name = self.current_processing_mode.clone();
        let mode = self.mode_manager.get_mode(&mode_name);
        let rule = mode.and_then(|m| m.find_match(&gi, &**node));

        if let Some(rule) = rule {
            // Rule found - evaluate the construction expression
            // Save current source context
            let saved_file = self.current_source_file.clone();
            let saved_pos = self.current_position.clone();

            // Restore source context to where the rule was defined
            // This ensures error messages show the rule definition location, not the rule body location
            if let Some(ref rule_file) = rule.source_file {
                self.current_source_file = Some(rule_file.clone());
            }
            if let Some(ref rule_pos) = rule.source_pos {
                self.current_position = Some(rule_pos.clone());
            }

            // Evaluate the construction expression
            let result = self.eval(rule.expr.clone(), env);

            // Restore previous source context
            self.current_source_file = saved_file;
            self.current_position = saved_pos;

            result
        } else if let Some(default_expr) = mode.and_then(|m| m.default_rule.as_ref()).cloned() {
            // No specific rule found - use default rule
            self.eval(default_expr, env)
        } else {
            // No rule found (and no default) - OpenJade's implicit default behavior:
            // Process children automatically (DSSSL §10.1.5)
            self.eval_process_children(env)
        }
    }

    /// Evaluate an expression in an environment
    ///
    /// Corresponds to OpenJade's `Interpreter::eval()`.
    ///
    /// ## Evaluation Rules
    ///
    /// 1. **Self-evaluating**: Numbers, strings, bools, chars → return as-is
    /// 2. **Symbols**: Variable lookup in environment
    /// 3. **Lists**: Check first element for special forms, otherwise apply
    pub fn eval(&mut self, expr: Value, env: Gc<Environment>) -> EvalResult {
        // Save previous context state
        let context_was_set = has_evaluator_context();
        let previous_context = get_evaluator_context();

        // ALWAYS update context to reflect current evaluator state
        // This ensures current_node is correct for nested eval() calls
        set_evaluator_context(EvaluatorContext {
            grove: self.arena.grove.clone(),
            current_node: self.arena.current_node.clone(),
            backend: self.backend.clone(),
        });

        // Evaluate
        let result = self.eval_inner(expr, env);

        // Restore previous context state
        if context_was_set {
            if let Some(prev_ctx) = previous_context {
                set_evaluator_context(prev_ctx);
            }
        } else {
            clear_evaluator_context();
        }

        result
    }

    /// Inner eval implementation (separated to ensure context cleanup)
    fn eval_inner(&mut self, expr: Value, env: Gc<Environment>) -> EvalResult {
        match expr {
            // Self-evaluating literals
            Value::Nil => Ok(Value::Nil),
            Value::Bool(_) => Ok(expr),
            Value::Integer(_) => Ok(expr),
            Value::Real(_) => Ok(expr),
            Value::Quantity { .. } => Ok(expr),
            Value::Char(_) => Ok(expr),
            Value::String(_) => Ok(expr),
            Value::Procedure(_) => Ok(expr),
            Value::Vector(_) => Ok(expr), // Vectors are self-evaluating in R4RS
            Value::Unspecified => Ok(expr),
            Value::Error => Ok(expr),

            // DSSSL types (self-evaluating for now)
            Value::Node(_) => Ok(expr),
            Value::NodeList(_) => Ok(expr),
            Value::Sosofo => Ok(expr),

            // Symbols: variable lookup
            Value::Symbol(ref name) => {
                // First try environment lookup
                if let Some(val) = env.lookup(name) {
                    return Ok(val);
                }

                // Fallback: check if this is a known primitive name
                if let Some(static_name) = self.get_primitive_static_name(name) {
                    // Return a marker procedure that will be recognized during application
                    // Use a dummy function - the real dispatch happens in apply_primitive
                    return Ok(Value::primitive(static_name, |_args| {
                        Err("Primitive should be dispatched through apply_primitive".to_string())
                    }));
                }

                Err(self.error_with_stack(format!("Undefined variable: {}", name)))
            },

            // Keywords are self-evaluating
            Value::Keyword(_) => Ok(expr),

            // Lists: special forms or function application
            Value::Pair(_) => self.eval_list(expr, env),
        }
    }

    /// Evaluate a list (special form or function call)
    fn eval_list(&mut self, expr: Value, env: Gc<Environment>) -> EvalResult {
        // Extract position from the pair if available and update current position
        if let Value::Pair(ref p) = expr {
            let pair_data = p.borrow();
            if let Some(ref pos) = pair_data.pos {
                // If we have line mappings, translate the position to source file coordinates
                if !self.line_mappings.is_empty() {
                    if let Some(mapping) = self.line_mappings.iter().find(|m| m.output_line == pos.line) {
                        self.current_source_file = Some(mapping.source_file.clone());
                        self.current_position = Some(Position {
                            line: mapping.source_line,
                            column: pos.column,
                        });
                    } else {
                        // No mapping found, use original position
                        self.current_position = Some(pos.clone());
                    }
                } else {
                    // No line mappings, use original position
                    self.current_position = Some(pos.clone());
                }
            }
        }

        // Extract the operator (first element)
        let (operator, args) = self.list_car_cdr(&expr)?;

        // Check if operator is a symbol (special form keyword)
        if let Value::Symbol(ref sym) = operator {
            match &**sym {
                "quote" => self.eval_quote(args),
                "if" => self.eval_if(args, env),
                "define" => self.eval_define(args, env),
                "set!" => self.eval_set(args, env),
                "lambda" => self.eval_lambda(args, env),
                "let" => self.eval_let(args, env),
                "let*" => self.eval_let_star(args, env),
                "letrec" => self.eval_letrec(args, env),
                "begin" => self.eval_begin(args, env),
                "cond" => self.eval_cond(args, env),
                "case" => self.eval_case(args, env),
                "and" => self.eval_and(args, env),
                "or" => self.eval_or(args, env),
                "apply" => self.eval_apply(args, env),
                "map" => self.eval_map(args, env),
                "for-each" => self.eval_for_each(args, env),
                "node-list-filter" => self.eval_node_list_filter(args, env),
                "node-list-map" => self.eval_node_list_map(args, env),
                "node-list-some?" => self.eval_node_list_some(args, env),
                "load" => self.eval_load(args, env),

                // DSSSL special forms
                "define-unit" => self.eval_define_unit(args, env),
                "define-language" => self.eval_define_language(args, env),
                "declare-flow-object-class" => self.eval_declare_flow_object_class(args, env),
                "declare-characteristic" => self.eval_declare_characteristic(args, env),
                "declare-initial-value" => self.eval_declare_initial_value(args, env),
                "mode" => self.eval_mode(args, env),
                "with-mode" => self.eval_with_mode(args, env),
                "element" => self.eval_element(args, env),
                "default" => self.eval_default(args, env),
                "process-children" => self.eval_process_children(env),
                "process-children-trim" => self.eval_process_children_trim(env),
                "process-node-list" => self.eval_process_node_list(args, env),
                "make" => self.eval_make(args, env),
                "style" => self.eval_style(args, env),

                // Not a special form - evaluate as function call
                _ => self.eval_application(operator, args, env),
            }
        } else {
            // Operator is not a symbol - evaluate and apply
            self.eval_application(operator, args, env)
        }
    }

    /// Extract car and cdr from a list
    fn list_car_cdr(&self, list: &Value) -> Result<(Value, Value), EvalError> {
        if let Value::Pair(ref p) = list {
            let pair = p.borrow();
            Ok((pair.car.clone(), pair.cdr.clone()))
        } else {
            Err(EvalError::new("Expected list".to_string()))
        }
    }

    /// Convert a Vec to a list
    fn vec_to_list(&self, vec: Vec<Value>) -> Value {
        let mut result = Value::Nil;
        for val in vec.iter().rev() {
            result = Value::cons(val.clone(), result);
        }
        result
    }

    /// Convert a list to a Vec of elements
    pub fn list_to_vec(&self, list: Value) -> Result<Vec<Value>, EvalError> {
        let mut result = Vec::new();
        let mut current = list;

        loop {
            match current {
                Value::Nil => break,
                Value::Pair(ref p) => {
                    let pair = p.borrow();
                    result.push(pair.car.clone());
                    let cdr = pair.cdr.clone();
                    drop(pair); // Explicitly drop borrow before reassigning
                    current = cdr;
                }
                _ => return Err(EvalError::new("Improper list".to_string())),
            }
        }

        Ok(result)
    }

    // =========================================================================
    // Special Forms
    // =========================================================================

    /// (quote expr) → expr
    fn eval_quote(&mut self, args: Value) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 1 {
            return Err(EvalError::new("quote requires exactly 1 argument".to_string()));
        }
        Ok(args_vec[0].clone())
    }

    /// (if test consequent [alternate])
    fn eval_if(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 || args_vec.len() > 3 {
            return Err(EvalError::new(
                "if requires 2 or 3 arguments".to_string(),
            ));
        }

        let test = self.eval_inner(args_vec[0].clone(), env.clone())?;

        if test.is_true() {
            self.eval_inner(args_vec[1].clone(), env)
        } else if args_vec.len() == 3 {
            self.eval_inner(args_vec[2].clone(), env)
        } else {
            Ok(Value::Unspecified)
        }
    }

    /// (define name value) or (define (name params...) body...)
    fn eval_define(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "define requires at least 2 arguments".to_string(),
            ));
        }

        // Check if first arg is a symbol or a list
        match &args_vec[0] {
            Value::Symbol(ref name) => {
                // Simple variable definition: (define x value)
                if args_vec.len() != 2 {
                    return Err(EvalError::new(
                        "define with symbol requires exactly 2 arguments".to_string(),
                    ));
                }
                let value = self.eval_inner(args_vec[1].clone(), env.clone())?;
                env.define(name, value);
                Ok(Value::Unspecified)
            }

            Value::Pair(_) => {
                // Function definition: (define (name params...) body...)
                // This is syntactic sugar for: (define name (lambda (params...) body...))
                let (name_val, params) = self.list_car_cdr(&args_vec[0])?;

                if let Value::Symbol(ref name) = name_val {
                    // Parse parameters (handles #!optional)
                    let (param_names, required_count, optional_defaults) =
                        self.parse_lambda_params(params)?;

                    // Build body
                    let body = if args_vec.len() == 2 {
                        args_vec[1].clone()
                    } else {
                        let mut body_list = Value::Nil;
                        for expr in args_vec[1..].iter().rev() {
                            body_list = Value::cons(expr.clone(), body_list);
                        }
                        Value::cons(Value::symbol("begin"), body_list)
                    };

                    // Create lambda with function name and source info
                    let source_info = self.current_source_file.as_ref().map(|file| {
                        use crate::scheme::parser::Position;
                        SourceInfo::new(file.clone(), Position::new())
                    });

                    let lambda_value = if optional_defaults.is_empty() {
                        Value::lambda_with_source(
                            param_names,
                            body,
                            env.clone(),
                            source_info,
                            Some(name.to_string()),
                        )
                    } else {
                        Value::lambda_with_optional(
                            param_names,
                            required_count,
                            optional_defaults,
                            body,
                            env.clone(),
                            source_info,
                            Some(name.to_string()),
                        )
                    };

                    env.define(name, lambda_value);
                    Ok(Value::Unspecified)
                } else {
                    Err(EvalError::new(
                        "First element of define must be a symbol".to_string(),
                    ))
                }
            }

            _ => Err(EvalError::new(
                "First argument to define must be symbol or list".to_string(),
            )),
        }
    }

    /// Evaluate (define-unit name value)
    /// DSSSL unit definition - defines a unit (em, pi, pt, etc.) as a quantity value
    /// Examples:
    ///   (define-unit em %bf-size%)
    ///   (define-unit pi (/ 1in 6))
    fn eval_define_unit(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.len() != 2 {
            return Err(EvalError::new(
                "define-unit requires exactly 2 arguments: name and value".to_string(),
            ));
        }

        // First argument must be a symbol (unit name)
        if let Value::Symbol(ref name) = args_vec[0] {
            // Evaluate the value expression
            let value = self.eval_inner(args_vec[1].clone(), env.clone())?;
            // Define the unit name in the environment
            env.define(name, value);
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to define-unit must be a symbol".to_string(),
            ))
        }
    }

    /// Evaluate (define-language name props...)
    /// DSSSL language definition - defines the language name as a symbol
    fn eval_define_language(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Err(EvalError::new(
                "define-language requires at least 1 argument".to_string(),
            ));
        }

        // First argument must be a symbol (language name)
        if let Value::Symbol(ref name) = args_vec[0] {
            // Define the language name as a symbol bound to itself
            // This allows it to be used in (declare-default-language name)
            env.define(name, args_vec[0].clone());
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to define-language must be a symbol".to_string(),
            ))
        }
    }

    /// Evaluate (declare-flow-object-class name public-id)
    /// DSSSL flow object class declaration - defines the class name as a symbol
    fn eval_declare_flow_object_class(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Err(EvalError::new(
                "declare-flow-object-class requires at least 1 argument".to_string(),
            ));
        }

        // First argument must be a symbol (flow object class name)
        if let Value::Symbol(ref name) = args_vec[0] {
            // Define the class name as a symbol bound to itself
            // This allows it to be used in (make name ...) constructs
            env.define(name, args_vec[0].clone());
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to declare-flow-object-class must be a symbol".to_string(),
            ))
        }
    }

    /// Evaluate (declare-characteristic name public-id default-value)
    /// DSSSL characteristic declaration - defines the characteristic with its default value
    fn eval_declare_characteristic(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.len() < 3 {
            return Err(EvalError::new(
                "declare-characteristic requires at least 3 arguments (name, public-id, default-value)".to_string(),
            ));
        }

        // First argument must be a symbol (characteristic name)
        if let Value::Symbol(ref name) = args_vec[0] {
            // Third argument is the default value - evaluate it
            let default_value = self.eval(args_vec[2].clone(), env.clone())?;

            // Define the characteristic name as a variable with its default value
            env.define(name, default_value);
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to declare-characteristic must be a symbol".to_string(),
            ))
        }
    }

    /// Evaluate (declare-initial-value name value)
    /// DSSSL initial value declaration - sets the initial value for a characteristic
    /// Example: (declare-initial-value page-width 210mm)
    fn eval_declare_initial_value(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.len() != 2 {
            return Err(EvalError::new(
                "declare-initial-value requires exactly 2 arguments (name and value)".to_string(),
            ));
        }

        // First argument must be a symbol (characteristic name)
        if let Value::Symbol(ref name) = args_vec[0] {
            // Second argument is the value - evaluate it
            let value = self.eval_inner(args_vec[1].clone(), env.clone())?;

            // Define the characteristic name as a variable with its value
            env.define(name, value);
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to declare-initial-value must be a symbol".to_string(),
            ))
        }
    }

    /// DSSSL element construction rule (OpenJade SchemeParser::doElement)
    /// Syntax: (element element-pattern construction-expression)
    ///
    /// Element pattern can be:
    /// - A symbol: (element foo ...)
    /// - A list for context matching: (element (parent child) ...)
    ///
    /// Stores the rule in processing mode WITHOUT evaluating the body.
    /// The body will be evaluated later during tree processing when a matching element is found.
    fn eval_element(&mut self, args: Value, _env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.len() < 2 {
            return Err(self.error_with_stack(
                "element requires at least 2 arguments (element-pattern and construction-expression)".to_string(),
            ));
        }

        // First argument is the element pattern (symbol or list)
        // For context matching like (parent child), extract the last element and context
        let (element_name, context) = match &args_vec[0] {
            Value::Symbol(ref name) => (name.clone(), Vec::new()),
            Value::Pair(_) => {
                // List pattern like (parent child) - extract context and element
                let pattern_list = self.list_to_vec(args_vec[0].clone())?;
                if pattern_list.is_empty() {
                    return Err(self.error_with_stack(
                        "Element pattern list cannot be empty".to_string(),
                    ));
                }
                // Last element in the list is the actual element being matched
                let element_name = if let Value::Symbol(ref name) = pattern_list[pattern_list.len() - 1] {
                    name.clone()
                } else {
                    return Err(self.error_with_stack(
                        "Element pattern must contain only symbols".to_string(),
                    ));
                };
                // Elements before the last one are the context (parent chain)
                let mut context = Vec::new();
                for i in 0..pattern_list.len() - 1 {
                    if let Value::Symbol(ref parent_name) = pattern_list[i] {
                        context.push(parent_name.to_string());
                    } else {
                        return Err(self.error_with_stack(
                            "Element pattern must contain only symbols".to_string(),
                        ));
                    }
                }
                (element_name, context)
            }
            _ => {
                return Err(self.error_with_stack(
                    "First argument to element must be a symbol or list of symbols".to_string(),
                ));
            }
        };

        // Remaining arguments are the construction expressions
        // OpenJade behavior: error on multiple expressions for better error detection
        // (suggest using sosofo-append explicitly)
        if args_vec.len() > 2 {
            return Err(self.error_with_stack(
                "element can only contain one sosofo expression. Use (sosofo-append ...) to combine multiple sosofos".to_string(),
            ));
        }

        let construction_expr = args_vec[1].clone();

        // Store the construction expression for later evaluation
        // Capture the current source position (where the 'element' form is)
        // Add the rule to the current mode
        let mode_name = self.current_mode.clone();
        self.mode_manager.get_or_create_mode(&mode_name).add_rule(
            element_name.to_string(),
            context,
            construction_expr,
            self.current_source_file.clone(),
            self.current_position.clone()
        );

        Ok(Value::Unspecified)
    }

    /// DSSSL default construction rule
    /// Syntax: (default construction-expression)
    ///
    /// Defines a default rule that applies to all elements that don't have a specific rule.
    /// This is the catch-all rule.
    fn eval_default(&mut self, args: Value, _env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Err(self.error_with_stack(
                "default requires at least 1 argument (construction-expression)".to_string(),
            ));
        }

        // OpenJade behavior: error on multiple expressions for better error detection
        // (suggest using sosofo-append explicitly)
        if args_vec.len() > 1 {
            return Err(self.error_with_stack(
                "default can only contain one sosofo expression. Use (sosofo-append ...) to combine multiple sosofos".to_string(),
            ));
        }

        let construction_expr = args_vec[0].clone();

        // Store the default rule in the current mode
        let mode_name = self.current_mode.clone();
        self.mode_manager.get_or_create_mode(&mode_name).add_default_rule(construction_expr);

        Ok(Value::Unspecified)
    }

    /// DSSSL mode definition
    /// Syntax: (mode mode-name rule1 rule2 ...)
    ///
    /// Defines a named processing mode with its own construction rules.
    /// All element and default rules within the mode body are added to the specified mode.
    fn eval_mode(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Err(self.error_with_stack(
                "mode requires at least 1 argument (mode-name)".to_string(),
            ));
        }

        // First argument is the mode name (symbol)
        let mode_name = if let Value::Symbol(ref name) = args_vec[0] {
            name.clone()
        } else {
            return Err(self.error_with_stack(
                "First argument to mode must be a symbol".to_string(),
            ));
        };

        // Save the current mode
        let saved_mode = self.current_mode.clone();

        // Switch to the new mode
        self.current_mode = mode_name.to_string();

        // Evaluate all the body expressions (element/default definitions)
        let mut result = Value::Unspecified;
        for expr in args_vec.iter().skip(1) {
            result = self.eval(expr.clone(), env.clone())?;
        }

        // Restore the previous mode
        self.current_mode = saved_mode;

        Ok(result)
    }

    /// DSSSL with-mode - temporarily switch processing mode
    /// Syntax: (with-mode mode-name expr)
    ///
    /// Evaluates expr with the processing mode temporarily switched to mode-name.
    /// Rules are looked up in the specified mode during processing.
    fn eval_with_mode(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.len() < 2 {
            return Err(self.error_with_stack(
                "with-mode requires 2 arguments (mode-name and expression)".to_string(),
            ));
        }

        // First argument is the mode name (symbol)
        let mode_name = if let Value::Symbol(ref name) = args_vec[0] {
            name.clone()
        } else {
            return Err(self.error_with_stack(
                "First argument to with-mode must be a symbol".to_string(),
            ));
        };

        // Save the current processing mode
        let saved_processing_mode = self.current_processing_mode.clone();

        // Switch to the new processing mode
        self.current_processing_mode = mode_name.to_string();

        // Evaluate the expression in the new mode
        let result = self.eval(args_vec[1].clone(), env);

        // Restore the previous processing mode
        self.current_processing_mode = saved_processing_mode;

        result
    }

    /// DSSSL process-children (OpenJade ProcessContext::processChildren)
    /// Syntax: (process-children)
    ///
    /// Processes all children of the current node.
    /// For each child, matches construction rules and evaluates them.
    fn eval_process_children(&mut self, env: Gc<Environment>) -> EvalResult {
        // Get current node
        let current_node = match self.current_node() {
            Some(node) => node.clone(),
            None => return Err(EvalError::new("No current node".to_string())),
        };

        // Get ALL children (including text nodes)
        // Note: all_children() returns elements AND text, not just elements like children()
        let mut children = current_node.all_children();

        // Process each child (using DSSSL node-list iteration pattern)
        let mut result = Value::Unspecified;
        while !children.is_empty() {
            // Get first child
            if let Some(child_node) = children.first() {
                // Save current node
                let saved_node = self.current_node();

                // Set child as current node
                self.set_current_node(child_node);

                // Process the child node
                result = self.process_node(env.clone())?;

                // Restore current node
                self.restore_current_node(saved_node);
            }

            // Move to rest of children
            children = children.rest();
        }

        Ok(result)
    }

    /// DSSSL (process-children-trim)
    ///
    /// OpenJade semantics: Process children with whitespace trimming
    /// - Text nodes are output directly to backend (not through rules)
    /// - Leading whitespace is trimmed from first text node
    /// - Trailing whitespace is trimmed from last text node
    /// - Element nodes are processed through rules normally
    fn eval_process_children_trim(&mut self, env: Gc<Environment>) -> EvalResult {
        // Get current node
        let current_node = match self.current_node() {
            Some(node) => node.clone(),
            None => return Err(EvalError::new("No current node".to_string())),
        };

        // Get ALL children (including text nodes)
        let mut children = current_node.all_children();

        // Collect all children into a vector to support trimming
        let mut child_nodes = Vec::new();
        while !children.is_empty() {
            if let Some(child) = children.first() {
                child_nodes.push(child);
            }
            children = children.rest();
        }

        if child_nodes.is_empty() {
            return Ok(Value::Unspecified);
        }

        // Track position for trimming
        let mut at_start = true;

        // Process each child
        for (index, child_node) in child_nodes.iter().enumerate() {
            let is_last = index == child_nodes.len() - 1;

            if child_node.is_text() {
                // Text node: output directly with trimming
                if let Some(mut text) = child_node.data() {
                    // Trim leading whitespace from first text node
                    if at_start {
                        let trimmed = text.trim_start();
                        if trimmed.is_empty() {
                            // Skip whitespace-only nodes at start
                            continue;
                        }
                        text = trimmed.to_string();
                        at_start = false;
                    }

                    // Trim trailing whitespace from last text node
                    if is_last {
                        text = text.trim_end().to_string();
                    }

                    if !text.is_empty() {
                        // Output text directly to backend
                        if let Some(ref backend) = self.backend {
                            backend.borrow_mut().literal(&text)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                        }
                    }
                }
            } else if child_node.is_element() {
                // Element node: mark that we're no longer at start
                at_start = false;

                // Process through rules
                let saved_node = self.current_node();
                self.set_current_node(child_node.clone_node());
                let _result = self.process_node(env.clone())?;
                self.restore_current_node(saved_node);
            }
        }

        Ok(Value::Unspecified)
    }

    /// DSSSL (process-node-list node-list)
    /// Syntax: (process-node-list node-list)
    ///
    /// Processes all nodes in the given node-list.
    /// For each node, matches construction rules and evaluates them.
    fn eval_process_node_list(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 1 {
            return Err(EvalError::new(
                "process-node-list requires exactly 1 argument".to_string(),
            ));
        }

        // Evaluate the argument to get the node-list
        let node_list_value = self.eval(args_vec[0].clone(), env.clone())?;

        // Get the node-list (auto-convert single node to singleton node-list)
        let mut nodes = match node_list_value {
            Value::NodeList(ref nl) => nl.clone(),
            Value::Node(ref n) => {
                // Auto-convert single node to singleton node-list
                // n is Rc<Box<dyn Node>>, we need Vec<Box<dyn Node>>
                let node_box: Box<dyn crate::grove::Node> = (**n).clone_node();
                Rc::new(Box::new(crate::grove::VecNodeList::new(vec![node_box])) as Box<dyn crate::grove::NodeList>)
            }
            _ => {
                return Err(EvalError::new(format!(
                    "process-node-list: not a node-list: {:?}",
                    node_list_value
                )))
            }
        };

        // Process each node (using DSSSL node-list iteration pattern)
        let mut result = Value::Unspecified;
        while !nodes.is_empty() {
            // Get first node
            if let Some(node) = nodes.first() {
                // Save current node
                let saved_node = self.current_node();

                // Set this node as current node
                self.set_current_node(node);

                // Process the node
                result = self.process_node(env.clone())?;

                // Restore current node
                self.restore_current_node(saved_node);
            }

            // Move to rest of nodes
            nodes = Rc::new(nodes.rest());
        }

        Ok(result)
    }

    /// DSSSL make flow object (OpenJade FotBuilder)
    /// Syntax: (make flow-object-type keyword: value ... body-sosofo)
    ///
    /// Creates flow objects and writes them to the backend.
    /// Supports: entity, formatting-instruction
    fn eval_make(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Err(EvalError::new(
                "make requires at least a flow object type".to_string(),
            ));
        }

        // First argument is the flow object type (symbol)
        let fo_type = match &args_vec[0] {
            Value::Symbol(s) => s.as_ref(),
            _ => return Err(EvalError::new(
                "make: first argument must be a flow object type symbol".to_string(),
            )),
        };

        // Parse keyword arguments and collect body expressions
        let mut i = 1;
        let mut system_id = None;
        let mut data = None;
        let mut path = None;
        let mut gi = None;
        let mut attributes = None;
        let mut body_exprs = Vec::new();

        while i < args_vec.len() {
            match &args_vec[i] {
                Value::Keyword(kw) => {
                    // Next argument is the keyword value
                    if i + 1 >= args_vec.len() {
                        return Err(EvalError::new(
                            format!("make: keyword {} requires a value", kw),
                        ));
                    }
                    let value = self.eval(args_vec[i + 1].clone(), env.clone())?;

                    match kw.as_ref() {
                        "system-id" => {
                            if let Value::String(s) = value {
                                system_id = Some(s);
                            } else {
                                return Err(EvalError::new(
                                    "make: system-id must be a string".to_string(),
                                ));
                            }
                        }
                        "data" => {
                            if let Value::String(s) = value {
                                data = Some(s);
                            } else {
                                return Err(EvalError::new(
                                    format!("make: data must be a string, got {:?}", value),
                                ));
                            }
                        }
                        "path" => {
                            if let Value::String(s) = value {
                                path = Some(s);
                            } else {
                                return Err(EvalError::new(
                                    "make: path must be a string".to_string(),
                                ));
                            }
                        }
                        "gi" => {
                            if let Value::String(s) = value {
                                gi = Some(s);
                            } else {
                                return Err(EvalError::new(
                                    "make element: gi must be a string".to_string(),
                                ));
                            }
                        }
                        "attributes" => {
                            // attributes can be a list or #f
                            attributes = Some(value);
                        }
                        _ => {
                            // Ignore unknown keywords for now
                        }
                    }
                    i += 2;
                }
                _ => {
                    // Non-keyword argument - collect as body expression
                    body_exprs.push(args_vec[i].clone());
                    i += 1;
                }
            }
        }

        // Call backend method based on flow object type
        let backend = self.backend.clone();
        match backend {
            Some(ref backend) => {
                match fo_type {
                    "entity" => {
                        if let Some(sid) = system_id {
                            // Save current buffer (for nested entities)
                            let saved_buffer = backend.borrow().current_output().to_string();
                            backend.borrow_mut().clear_buffer();

                            // Evaluate body expressions (they append to buffer)
                            for expr in body_exprs {
                                self.eval(expr, env.clone())?;
                            }

                            // Get current buffer content and write to file
                            let content = backend.borrow().current_output().to_string();
                            backend.borrow_mut().entity(&sid, &content)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                            // Restore saved buffer (for parent entity context)
                            backend.borrow_mut().clear_buffer();
                            backend.borrow_mut().formatting_instruction(&saved_buffer)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                        } else {
                            return Err(EvalError::new(
                                "make entity requires system-id: keyword".to_string(),
                            ));
                        }
                    }
                    "formatting-instruction" => {
                        if let Some(d) = data {
                            // Append to current buffer
                            backend.borrow_mut().formatting_instruction(&d)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                        } else {
                            return Err(EvalError::new(
                                "make formatting-instruction requires data: keyword".to_string(),
                            ));
                        }
                    }
                    "literal" => {
                        // literal is typically called as (literal "text") not (make literal ...)
                        // but we support both forms for completeness
                        if let Some(d) = data {
                            backend.borrow_mut().formatting_instruction(&d)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                        } else {
                            return Err(EvalError::new(
                                "make literal requires data: keyword or a string body".to_string(),
                            ));
                        }
                    }
                    "directory" => {
                        if let Some(p) = path {
                            // Save current directory context
                            let prev_dir = backend.borrow().current_directory().map(|s| s.to_string());

                            // Create directory and set as current context
                            backend.borrow_mut().directory(&p)
                                .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                            // Evaluate body expressions in the new directory context
                            // (nested entities/directories will be created relative to this directory)
                            for expr in body_exprs {
                                self.eval(expr, env.clone())?;
                            }

                            // Restore previous directory context
                            backend.borrow_mut().set_current_directory(prev_dir);
                        } else {
                            return Err(EvalError::new(
                                "make directory requires path: keyword".to_string(),
                            ));
                        }
                    }
                    "sequence" => {
                        // Sequence evaluates all body expressions in order
                        // This is the primary composition mechanism for flow objects
                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }
                    }
                    "element" => {
                        // OpenJade extension: (make element gi: "name" attributes: '(("key" "val")) body...)
                        // Outputs: <name\nkey="val"\n>body</name\n>
                        //
                        // This is a compound flow object that generates HTML-like tags
                        // with OpenJade's special formatting (newline after tag name and each attribute)

                        let gi = gi.ok_or_else(|| EvalError::new(
                            "make element requires gi: keyword".to_string()
                        ))?;

                        // Start tag with newline after tag name
                        backend.borrow_mut().formatting_instruction(&format!("<{}\n", gi))
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        // Add attributes if present, each on its own line
                        if let Some(attrs_val) = attributes {
                            // attributes should be a list of (name value) pairs
                            let attrs_list = self.list_to_vec(attrs_val)?;
                            for attr_pair in attrs_list {
                                let pair_vec = self.list_to_vec(attr_pair)?;
                                if pair_vec.len() == 2 {
                                    if let (Value::String(name), Value::String(value)) =
                                        (&pair_vec[0], &pair_vec[1]) {
                                        // Escape special characters in attribute value
                                        let escaped_value = value
                                            .replace('&', "&amp;")
                                            .replace('"', "&quot;")
                                            .replace('<', "&lt;")
                                            .replace('>', "&gt;");
                                        backend.borrow_mut().formatting_instruction(&format!("{}=\"{}\"\n", name, escaped_value))
                                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                                    }
                                }
                            }
                        }

                        // Closing > for opening tag
                        backend.borrow_mut().formatting_instruction(">")
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        // Evaluate body expressions
                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }

                        // End tag with OpenJade's line break pattern
                        backend.borrow_mut().formatting_instruction(&format!("</{}\n>", gi))
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                    }
                    "paragraph" => {
                        // RTF paragraph flow object
                        // Start paragraph
                        backend.borrow_mut().start_paragraph()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        // Evaluate body expressions (literal text, etc.)
                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }

                        // End paragraph
                        backend.borrow_mut().end_paragraph()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                    }
                    "display-group" => {
                        // RTF display-group flow object
                        backend.borrow_mut().start_display_group()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }

                        backend.borrow_mut().end_display_group()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                    }
                    "simple-page-sequence" => {
                        // RTF simple-page-sequence flow object (main page container)
                        backend.borrow_mut().start_simple_page_sequence()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }

                        backend.borrow_mut().end_simple_page_sequence()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                    }
                    "line-field" => {
                        // RTF line-field flow object (inline text container)
                        backend.borrow_mut().start_line_field()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;

                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }

                        backend.borrow_mut().end_line_field()
                            .map_err(|e| EvalError::new(format!("Backend error: {}", e)))?;
                    }
                    "link" | "scroll" | "marginalia" | "leader" | "table" | "table-row" | "table-cell" | "table-column" | "table-part" | "paragraph-break" => {
                        // Flow objects that just process their children
                        // For SGML backend (code gen), we ignore these formatting constructs
                        for expr in body_exprs {
                            self.eval(expr, env.clone())?;
                        }
                    }
                    _ => {
                        // Unknown flow object type - error
                        return Err(EvalError::new(
                            format!("make: unknown flow object type '{}'", fo_type),
                        ));
                    }
                }
            }
            None => {
                return Err(EvalError::new(
                    "make: no backend available".to_string(),
                ));
            }
        }

        // Flow objects (make forms) always return a Sosofo
        Ok(Value::Sosofo)
    }

    /// (style keyword: value ...)
    ///
    /// Stub for DSSSL style objects used in document formatting.
    /// Dazzle focuses on code generation, so this returns a dummy value.
    fn eval_style(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        // Parse keyword arguments (but ignore them)
        let args_vec = self.list_to_vec(args)?;
        let mut i = 0;

        while i < args_vec.len() {
            match &args_vec[i] {
                Value::Keyword(_kw) => {
                    // Skip keyword and its value
                    if i + 1 >= args_vec.len() {
                        return Err(EvalError::new(
                            "style: keyword requires a value".to_string(),
                        ));
                    }
                    // Evaluate the value (to check for errors) but don't use it
                    let _value = self.eval(args_vec[i + 1].clone(), env.clone())?;
                    i += 2;
                }
                _ => {
                    return Err(EvalError::new(
                        format!("style: unexpected argument {:?}", args_vec[i]),
                    ));
                }
            }
        }

        // Return a dummy style object (we don't use it for code generation)
        Ok(Value::Symbol(Rc::from("dummy-style")))
    }

    /// (set! name value)
    fn eval_set(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 2 {
            return Err(EvalError::new(
                "set! requires exactly 2 arguments".to_string(),
            ));
        }

        if let Value::Symbol(ref name) = args_vec[0] {
            let value = self.eval(args_vec[1].clone(), env.clone())?;
            env.set(name, value)
                .map_err(|e| EvalError::new(e))?;
            Ok(Value::Unspecified)
        } else {
            Err(EvalError::new(
                "First argument to set! must be a symbol".to_string(),
            ))
        }
    }

    /// (lambda (params...) body...)
    fn eval_lambda(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "lambda requires at least 2 arguments (params and body)".to_string(),
            ));
        }

        // Parse parameter list (handles #!optional parameters)
        let params_list = &args_vec[0];
        let (param_names, required_count, optional_defaults) =
            self.parse_lambda_params(params_list.clone())?;

        // Extract body (one or more expressions)
        let body = if args_vec.len() == 2 {
            // Single body expression
            args_vec[1].clone()
        } else {
            // Multiple body expressions - wrap in (begin ...)
            let mut body_list = Value::Nil;
            for expr in args_vec[1..].iter().rev() {
                body_list = Value::cons(expr.clone(), body_list);
            }
            Value::cons(Value::symbol("begin"), body_list)
        };

        // Create lambda closure capturing current environment and source location
        // current_position has been set by eval_list to the position of the (lambda ...) expression
        let source_info = match (&self.current_source_file, &self.current_position) {
            (Some(file), Some(pos)) => {
                // Clone the position since we'll be mutating current_position later
                Some(SourceInfo::new(file.clone(), pos.clone()))
            }
            (Some(file), None) => {
                use crate::scheme::parser::Position;
                Some(SourceInfo::new(file.clone(), Position::new()))
            }
            _ => None,
        };

        // Create lambda with optional parameters if present
        if optional_defaults.is_empty() {
            Ok(Value::lambda_with_source(param_names, body, env, source_info, None))
        } else {
            Ok(Value::lambda_with_optional(
                param_names,
                required_count,
                optional_defaults,
                body,
                env,
                source_info,
                None,
            ))
        }
    }

    /// Parse lambda parameter list, handling #!optional parameters
    ///
    /// Returns: (param_names, required_count, optional_defaults)
    fn parse_lambda_params(
        &mut self,
        params: Value,
    ) -> Result<(Vec<String>, usize, Vec<Value>), EvalError> {
        if params.is_nil() {
            return Ok((Vec::new(), 0, Vec::new()));
        }

        let params_vec = self.list_to_vec(params)?;
        let mut param_names = Vec::new();
        let mut required_count = 0;
        let mut optional_defaults = Vec::new();
        let mut in_optional = false;

        for param in params_vec {
            // Check for #!optional marker
            if let Value::Symbol(ref sym) = param {
                if sym.as_ref() == "#!optional" {
                    in_optional = true;
                    continue;
                }
            }

            if !in_optional {
                // Required parameter - must be a symbol
                if let Value::Symbol(ref name) = param {
                    param_names.push(name.to_string());
                    required_count += 1;
                } else {
                    return Err(EvalError::new(format!(
                        "Parameter must be a symbol, got: {:?}",
                        param
                    )));
                }
            } else {
                // Optional parameter - can be symbol or (symbol default)
                match param {
                    Value::Symbol(ref name) => {
                        // Optional with no default: use #<unspecified>
                        param_names.push(name.to_string());
                        optional_defaults.push(Value::Unspecified);
                    }
                    Value::Pair(_) => {
                        // (name default-expr)
                        let opt_list = self.list_to_vec(param)?;
                        if opt_list.len() != 2 {
                            return Err(EvalError::new(format!(
                                "Optional parameter must be (name default), got list of length {}",
                                opt_list.len()
                            )));
                        }

                        if let Value::Symbol(ref name) = opt_list[0] {
                            param_names.push(name.to_string());
                            // Store the unevaluated default expression
                            if std::env::var("DEBUG_OPTIONAL").is_ok() {
                                eprintln!("[DEBUG_OPTIONAL] Storing default for param '{}': {:?}", name, opt_list[1]);
                            }
                            optional_defaults.push(opt_list[1].clone());
                        } else {
                            return Err(EvalError::new(format!(
                                "Optional parameter name must be a symbol, got: {:?}",
                                opt_list[0]
                            )));
                        }
                    }
                    _ => {
                        return Err(EvalError::new(format!(
                            "Optional parameter must be symbol or (symbol default), got: {:?}",
                            param
                        )));
                    }
                }
            }
        }

        Ok((param_names, required_count, optional_defaults))
    }

    /// (let ((var val)...) body...)
    fn eval_let(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "let requires at least 2 arguments".to_string(),
            ));
        }

        // Check if this is named let: (let name ((var val)...) body...)
        if let Value::Symbol(ref loop_name) = args_vec[0] {
            if args_vec.len() < 3 {
                return Err(EvalError::new(
                    "named let requires at least 3 arguments".to_string(),
                ));
            }

            // Named let: transform to (letrec ((name (lambda (vars...) body...))) (name vals...))
            let bindings_list = &args_vec[1];
            let bindings = self.list_to_vec(bindings_list.clone())?;
            let body = &args_vec[2..];

            // Extract variable names and initial values
            let mut var_names = Vec::new();
            let mut init_values = Vec::new();
            for binding in &bindings {
                let binding_vec = self.list_to_vec(binding.clone())?;
                if binding_vec.len() != 2 {
                    return Err(EvalError::new(
                        "named let binding must have exactly 2 elements".to_string(),
                    ));
                }
                var_names.push(binding_vec[0].clone());
                init_values.push(binding_vec[1].clone());
            }

            // Create lambda: (lambda (vars...) body...)
            let lambda_params = self.vec_to_list(var_names);
            let mut lambda_body = vec![Value::symbol("lambda"), lambda_params];
            lambda_body.extend_from_slice(body);
            let lambda_expr = self.vec_to_list(lambda_body);

            // Create letrec binding: ((name (lambda ...)))
            let letrec_binding = Value::cons(
                Value::symbol(loop_name),
                Value::cons(lambda_expr, Value::Nil),
            );
            let letrec_bindings = Value::cons(letrec_binding, Value::Nil);

            // Create function call: (name vals...)
            let mut call_expr = vec![Value::symbol(loop_name)];
            call_expr.extend_from_slice(&init_values);
            let call = self.vec_to_list(call_expr);

            // Evaluate: (letrec ((name (lambda ...))) (name vals...))
            return self.eval_letrec(self.vec_to_list(vec![letrec_bindings, call]), env);
        }

        // Standard let: (let ((var val)...) body...)
        let bindings_list = &args_vec[0];
        let bindings = self.list_to_vec(bindings_list.clone())?;

        // Create new environment extending current
        let new_env = Environment::extend(env.clone());

        // Evaluate bindings in OLD environment, define in NEW environment
        for binding in bindings {
            let binding_vec = self.list_to_vec(binding)?;
            if binding_vec.len() != 2 {
                return Err(EvalError::new(
                    "let binding must have exactly 2 elements".to_string(),
                ));
            }

            if let Value::Symbol(ref name) = binding_vec[0] {
                let value = self.eval_inner(binding_vec[1].clone(), env.clone())?;
                new_env.define(name, value);
            } else {
                return Err(EvalError::new(
                    "Binding variable must be a symbol".to_string(),
                ));
            }
        }

        // Evaluate body in new environment
        let body = &args_vec[1..];
        self.eval_sequence(body, new_env)
    }

    /// (let* ((var val)...) body...)
    fn eval_let_star(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "let* requires at least 2 arguments".to_string(),
            ));
        }

        // Parse bindings
        let bindings_list = &args_vec[0];
        let bindings = self.list_to_vec(bindings_list.clone())?;

        // Create new environment
        let current_env = Environment::extend(env);

        // Evaluate bindings sequentially in CURRENT environment
        for binding in bindings {
            let binding_vec = self.list_to_vec(binding)?;
            if binding_vec.len() != 2 {
                return Err(EvalError::new(
                    "let* binding must have exactly 2 elements".to_string(),
                ));
            }

            if let Value::Symbol(ref name) = binding_vec[0] {
                let value = self.eval_inner(binding_vec[1].clone(), current_env.clone())?;
                current_env.define(name, value);
            } else {
                return Err(EvalError::new(
                    "Binding variable must be a symbol".to_string(),
                ));
            }
        }

        // Evaluate body
        let body = &args_vec[1..];
        self.eval_sequence(body, current_env)
    }

    /// (letrec ((var val)...) body...)
    ///
    /// letrec allows recursive definitions - all bindings can refer to each other.
    /// Implementation:
    /// 1. Create new environment
    /// 2. Bind all variables to Unspecified first
    /// 3. Evaluate all values in the new environment
    /// 4. Update bindings with evaluated values
    /// 5. Evaluate body in the new environment
    fn eval_letrec(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "letrec requires at least 2 arguments".to_string(),
            ));
        }

        // Parse bindings
        let bindings_list = &args_vec[0];
        let bindings = self.list_to_vec(bindings_list.clone())?;

        // Create new environment extending current
        let new_env = Environment::extend(env);

        // First pass: bind all variables to Unspecified
        let mut var_names = Vec::new();
        for binding in &bindings {
            let binding_vec = self.list_to_vec(binding.clone())?;
            if binding_vec.len() != 2 {
                return Err(EvalError::new(
                    "letrec binding must have exactly 2 elements".to_string(),
                ));
            }

            if let Value::Symbol(ref name) = binding_vec[0] {
                var_names.push(name.to_string());
                new_env.define(name, Value::Unspecified);
            } else {
                return Err(EvalError::new(
                    "Binding variable must be a symbol".to_string(),
                ));
            }
        }

        // Second pass: evaluate all values in the new environment and update bindings
        for (i, binding) in bindings.iter().enumerate() {
            let binding_vec = self.list_to_vec(binding.clone())?;
            let value = self.eval_inner(binding_vec[1].clone(), new_env.clone())?;

            // Update the binding (set! will work since we already defined it)
            new_env.set(&var_names[i], value)
                .map_err(|e| EvalError::new(e))?;
        }

        // Evaluate body in new environment
        let body = &args_vec[1..];
        self.eval_sequence(body, new_env)
    }

    /// (begin expr...)
    fn eval_begin(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        self.eval_sequence(&args_vec, env)
    }

    /// (cond (test expr...)...)
    fn eval_cond(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let clauses = self.list_to_vec(args)?;

        for clause in clauses {
            let clause_vec = self.list_to_vec(clause)?;
            if clause_vec.is_empty() {
                return Err(EvalError::new("Empty cond clause".to_string()));
            }

            // Check for else clause
            if let Value::Symbol(ref sym) = clause_vec[0] {
                if &**sym == "else" {
                    return self.eval_sequence(&clause_vec[1..], env);
                }
            }

            // Evaluate test
            let test = self.eval_inner(clause_vec[0].clone(), env.clone())?;
            if test.is_true() {
                if clause_vec.len() == 1 {
                    return Ok(test);
                } else {
                    return self.eval_sequence(&clause_vec[1..], env);
                }
            }
        }

        Ok(Value::Unspecified)
    }

    /// (case key ((datum...) expr...)...)
    ///
    /// R4RS case statement:
    /// ```scheme
    /// (case expr
    ///   ((datum1 datum2 ...) result1 result2 ...)
    ///   ((datum3 datum4 ...) result3 result4 ...)
    ///   ...
    ///   [else resultN ...])
    /// ```
    ///
    /// The key expression is evaluated and compared with each datum using eqv?.
    /// The datums are NOT evaluated (they are literal constants).
    fn eval_case(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.is_empty() {
            return Err(EvalError::new("case requires at least 1 argument".to_string()));
        }

        // Evaluate the key expression
        let key = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Iterate through clauses
        for clause in &args_vec[1..] {
            let clause_vec = self.list_to_vec(clause.clone())?;
            if clause_vec.is_empty() {
                return Err(EvalError::new("Empty case clause".to_string()));
            }

            // Check for else clause
            if let Value::Symbol(ref sym) = clause_vec[0] {
                if &**sym == "else" {
                    return self.eval_sequence(&clause_vec[1..], env);
                }
            }

            // First element should be a list of datums
            let datums = self.list_to_vec(clause_vec[0].clone())?;

            // Check if key matches any datum using equal? (not eqv?)
            // NOTE: R4RS specifies eqv?, but that doesn't work for strings.
            // OpenJade uses equal? for case matching to handle string comparisons.
            for datum in datums {
                if key.equal(&datum) {
                    // Match found - evaluate body expressions
                    if clause_vec.len() == 1 {
                        // No expressions in clause - return unspecified
                        return Ok(Value::Unspecified);
                    } else {
                        return self.eval_sequence(&clause_vec[1..], env);
                    }
                }
            }
        }

        // No match found - R4RS says result is unspecified
        Ok(Value::Unspecified)
    }

    /// (and expr...)
    fn eval_and(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Ok(Value::bool(true));
        }

        let mut result = Value::bool(true);
        for expr in args_vec {
            result = self.eval_inner(expr, env.clone())?;
            if !result.is_true() {
                return Ok(Value::bool(false));
            }
        }

        Ok(result)
    }

    /// (or expr...)
    fn eval_or(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        for expr in args_vec {
            let result = self.eval_inner(expr, env.clone())?;
            if result.is_true() {
                return Ok(result);
            }
        }

        Ok(Value::bool(false))
    }

    /// Evaluate a sequence of expressions, return last result
    fn eval_sequence(&mut self, exprs: &[Value], env: Gc<Environment>) -> EvalResult {
        if exprs.is_empty() {
            return Ok(Value::Unspecified);
        }

        let mut result = Value::Unspecified;
        for expr in exprs {
            result = self.eval_inner(expr.clone(), env.clone())?;
        }

        Ok(result)
    }

    /// (apply proc args)
    ///
    /// Apply a procedure to a list of arguments.
    /// Example: (apply + '(1 2 3)) → 6
    fn eval_apply(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 2 {
            return Err(EvalError::new(
                "apply requires exactly 2 arguments".to_string(),
            ));
        }

        // Evaluate the procedure
        let proc = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate the argument list
        let arg_list = self.eval_inner(args_vec[1].clone(), env)?;

        // Convert argument list to vector
        let arg_values = self.list_to_vec(arg_list)?;

        // Apply the procedure
        self.apply(proc, arg_values)
    }

    /// (map proc list)
    ///
    /// Apply procedure to each element of list, return list of results.
    /// Example: (map (lambda (x) (* x 2)) '(1 2 3)) → '(2 4 6)
    /// (map proc list1 list2 ...)
    ///
    /// R4RS: Apply procedure to corresponding elements of lists.
    /// All lists must have the same length.
    /// Returns a list of results.
    ///
    /// Examples:
    /// - (map + '(1 2 3) '(4 5 6)) => (5 7 9)
    /// - (map list '(1 2) '(a b) '(x y)) => ((1 a x) (2 b y))
    fn eval_map(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new("map requires at least 2 arguments".to_string()));
        }

        // Evaluate the procedure
        let proc = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate all lists
        let mut lists = Vec::new();
        for i in 1..args_vec.len() {
            let list = self.eval_inner(args_vec[i].clone(), env.clone())?;
            let list_vec = self.list_to_vec(list)?;
            lists.push(list_vec);
        }

        // Check all lists have the same length
        if lists.is_empty() {
            return Ok(Value::Nil);
        }

        let length = lists[0].len();
        for list in &lists[1..] {
            if list.len() != length {
                return Err(EvalError::new(
                    "map: all lists must have the same length".to_string(),
                ));
            }
        }

        // Apply procedure to corresponding elements
        let mut result_vec = Vec::new();
        for i in 0..length {
            // Gather i-th element from each list
            let mut proc_args = Vec::new();
            for list in &lists {
                proc_args.push(list[i].clone());
            }

            // Apply procedure
            let result = self.apply(proc.clone(), proc_args)?;
            result_vec.push(result);
        }

        // Convert result vector back to list
        let mut result_list = Value::Nil;
        for elem in result_vec.into_iter().rev() {
            result_list = Value::cons(elem, result_list);
        }

        Ok(result_list)
    }

    /// (for-each proc list1 list2 ...)
    ///
    /// R4RS: Apply procedure to corresponding elements of lists for side effects.
    /// All lists must have the same length.
    /// Returns unspecified.
    ///
    /// Example: (for-each display '("a" "b" "c"))
    fn eval_for_each(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "for-each requires at least 2 arguments".to_string(),
            ));
        }

        // Evaluate the procedure
        let proc = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate all lists
        let mut lists = Vec::new();
        for i in 1..args_vec.len() {
            let list = self.eval_inner(args_vec[i].clone(), env.clone())?;
            let list_vec = self.list_to_vec(list)?;
            lists.push(list_vec);
        }

        // Check all lists have the same length
        if lists.is_empty() {
            return Ok(Value::Unspecified);
        }

        let length = lists[0].len();
        for list in &lists[1..] {
            if list.len() != length {
                return Err(EvalError::new(
                    "for-each: all lists must have the same length".to_string(),
                ));
            }
        }

        // Apply procedure to corresponding elements (for side effects)
        for i in 0..length {
            // Gather i-th element from each list
            let mut proc_args = Vec::new();
            for list in &lists {
                proc_args.push(list[i].clone());
            }

            // Apply procedure for side effects
            self.apply(proc.clone(), proc_args)?;
        }

        Ok(Value::Unspecified)
    }

    /// (node-list-filter predicate node-list)
    ///
    /// (node-list-filter pred node-list) → node-list
    ///
    /// Returns a node-list containing only nodes for which predicate returns #t.
    /// DSSSL: Filter a node-list based on a predicate function.
    fn eval_node_list_filter(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 2 {
            return Err(EvalError::new("node-list-filter requires exactly 2 arguments".to_string()));
        }

        // Evaluate the predicate
        let pred = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate the node-list
        let node_list_val = self.eval_inner(args_vec[1].clone(), env.clone())?;

        match node_list_val {
            Value::NodeList(ref nl) => {
                let mut filtered_nodes = Vec::new();

                // Iterate through the node-list
                let mut index = 0;
                loop {
                    if let Some(node) = nl.get(index) {
                        // Apply predicate to this node
                        let node_val = Value::node(node);
                        let result = self.apply(pred.clone(), vec![node_val.clone()])?;

                        // If predicate returns a truthy value (anything except #f), include this node
                        if !matches!(result, Value::Bool(false)) {
                            // Need to get the node again since we consumed it
                            if let Value::Node(n) = node_val {
                                filtered_nodes.push(n.as_ref().clone_node());
                            }
                        }

                        index += 1;
                    } else {
                        break;
                    }
                }

                Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(filtered_nodes))))
            }
            _ => Err(EvalError::new(format!("node-list-filter: second argument not a node-list: {:?}", node_list_val))),
        }
    }

    /// (node-list-map proc node-list) → node-list
    ///
    /// Applies proc to each node in node-list and returns a flattened node-list.
    /// Each result must be a node-list or a single node (which is treated as a singleton node-list).
    /// Results are concatenated (flattened) into a single node-list.
    /// If proc returns #f or any non-node value, processing stops (OpenJade compatibility).
    ///
    /// DSSSL: Maps a procedure over a node-list, flattening results into a single node-list.
    /// OpenJade: MapNodeListObj - stops processing when proc returns a non-node-list value.
    fn eval_node_list_map(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 2 {
            return Err(EvalError::new("node-list-map requires exactly 2 arguments".to_string()));
        }

        // Evaluate the procedure
        let proc = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate the node-list
        let node_list_val = self.eval_inner(args_vec[1].clone(), env.clone())?;

        // Collect all nodes from mapping results (flattened)
        let mut result_nodes: Vec<Box<dyn crate::grove::Node>> = Vec::new();

        match node_list_val {
            Value::Node(ref n) => {
                // Single node - apply proc and collect result
                let node_val = Value::node(n.as_ref().clone_node());
                let result = self.apply(proc, vec![node_val])?;

                // OpenJade: Result must be node or node-list. If not, stop processing.
                // Single nodes are auto-converted to singleton node-lists (DSSSL spec)
                match result {
                    Value::Node(n) => {
                        // Single node - treat as singleton node-list
                        result_nodes.push(n.as_ref().clone_node());
                    }
                    Value::NodeList(nl) => {
                        // Node-list - flatten all nodes
                        let mut index = 0;
                        while let Some(node) = nl.get(index) {
                            result_nodes.push(node);
                            index += 1;
                        }
                    }
                    _ => {
                        // Non-node result (e.g., #f) - stop processing (OpenJade compat)
                        // Return empty node-list
                    }
                }
            }
            Value::NodeList(ref nl) => {
                // Iterate through the node-list
                let mut index = 0;
                loop {
                    if let Some(node) = nl.get(index) {
                        // Apply procedure to this node
                        let node_val = Value::node(node);
                        let result = self.apply(proc.clone(), vec![node_val])?;

                        // OpenJade: Result must be node or node-list. If not, stop processing.
                        match result {
                            Value::Node(n) => {
                                // Single node - treat as singleton node-list
                                result_nodes.push(n.as_ref().clone_node());
                                index += 1;
                            }
                            Value::NodeList(nl_result) => {
                                // Node-list - flatten all nodes
                                let mut nl_index = 0;
                                while let Some(node) = nl_result.get(nl_index) {
                                    result_nodes.push(node);
                                    nl_index += 1;
                                }
                                index += 1;
                            }
                            _ => {
                                // Non-node result (e.g., #f) - stop processing (OpenJade compat)
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
            }
            _ => return Err(EvalError::new(format!("node-list-map: second argument must be a node or node-list: {:?}", node_list_val))),
        }

        // Return flattened node-list
        Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(result_nodes))))
    }

    /// (node-list-some? predicate node-list) → boolean
    ///
    /// Returns #t if the predicate returns true for at least one node in the node-list.
    /// Returns #f if the node-list is empty or the predicate returns false for all nodes.
    /// DSSSL: Test if any node in the node-list satisfies the predicate.
    fn eval_node_list_some(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 2 {
            return Err(EvalError::new("node-list-some? requires exactly 2 arguments".to_string()));
        }

        // Evaluate the predicate
        let pred = self.eval_inner(args_vec[0].clone(), env.clone())?;

        // Evaluate the node-list
        let node_list_val = self.eval_inner(args_vec[1].clone(), env.clone())?;

        match node_list_val {
            Value::NodeList(ref nl) => {
                // Iterate through the node-list
                let mut index = 0;
                loop {
                    if let Some(node) = nl.get(index) {
                        // Apply predicate to this node
                        let node_val = Value::node(node);
                        let result = self.apply(pred.clone(), vec![node_val])?;

                        // If predicate returns a truthy value (anything except #f), return #t immediately
                        if !matches!(result, Value::Bool(false)) {
                            return Ok(Value::bool(true));
                        }

                        index += 1;
                    } else {
                        break;
                    }
                }

                // If we get here, no node satisfied the predicate
                Ok(Value::bool(false))
            }
            _ => Err(EvalError::new(format!("node-list-some?: second argument not a node-list: {:?}", node_list_val))),
        }
    }

    /// (load filename)
    ///
    /// Load and evaluate Scheme code from a file.
    /// Returns the result of the last expression in the file.
    fn eval_load(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() != 1 {
            return Err(EvalError::new(
                "load requires exactly 1 argument".to_string(),
            ));
        }

        // Evaluate the filename argument
        let filename_val = self.eval_inner(args_vec[0].clone(), env.clone())?;

        let filename = match filename_val {
            Value::String(s) => s.to_string(),
            _ => return Err(EvalError::new(
                format!("load: filename must be a string, got {:?}", filename_val)
            )),
        };

        // Read the file
        let contents = std::fs::read_to_string(&filename)
            .map_err(|e| EvalError::new(format!("load: cannot read file '{}': {}", filename, e)))?;

        // Parse the file contents with filename for error reporting
        let mut parser = crate::scheme::parser::Parser::new_with_filename(&contents, filename.clone());
        let mut result = Value::Unspecified;

        // Save current source file and position, set to the loaded file for error reporting
        let prev_source_file = self.current_source_file.clone();
        let prev_position = self.current_position.clone();
        self.current_source_file = Some(filename.clone());

        // Evaluate each expression in sequence
        let eval_result = loop {
            // Get position before parsing
            let pos = parser.current_position();

            match parser.parse() {
                Ok(expr) => {
                    // Set position for this expression
                    self.current_position = Some(pos);

                    match self.eval_inner(expr, env.clone()) {
                        Ok(val) => result = val,
                        Err(e) => break Err(e),
                    }
                }
                Err(e) => {
                    // Check if we've reached end of input (not an error)
                    let error_msg = e.to_string();
                    if error_msg.contains("Unexpected end of input")
                        || error_msg.contains("Expected")
                        || error_msg.contains("EOF") {
                        break Ok(result);
                    }
                    break Err(EvalError::new(
                        format!("load: parse error in '{}': {}", filename, e)
                    ));
                }
            }
        };

        // Restore previous source file and position
        self.current_source_file = prev_source_file;
        self.current_position = prev_position;

        eval_result
    }

    // =========================================================================
    // Function Application
    // =========================================================================

    /// Apply a function to arguments
    fn eval_application(
        &mut self,
        operator: Value,
        args: Value,
        env: Gc<Environment>,
    ) -> EvalResult {
        // Save the position of this application expression (the call site)
        let application_pos = self.current_position.clone();
        let application_file = self.current_source_file.clone();

        // Evaluate operator
        let proc = self.eval_inner(operator, env.clone())?;

        // Evaluate arguments - extract position from the pair containing each argument
        let mut evaled_args = Vec::new();
        let mut current_args = args;
        loop {
            match current_args {
                Value::Nil => break,
                Value::Pair(ref p) => {
                    let pair_borrow = p.borrow();

                    // Extract position from this pair (which contains the argument)
                    // This gives us the position where the argument appears in the source
                    if let Some(ref pos) = pair_borrow.pos {
                        // Translate output position to source position using line mappings
                        if !self.line_mappings.is_empty() {
                            if let Some(mapping) = self.line_mappings.iter().find(|m| m.output_line == pos.line) {
                                self.current_source_file = Some(mapping.source_file.clone());
                                self.current_position = Some(Position {
                                    line: mapping.source_line,
                                    column: pos.column,
                                });
                            } else {
                                self.current_position = Some(pos.clone());
                            }
                        } else {
                            self.current_position = Some(pos.clone());
                        }
                    }

                    let arg = pair_borrow.car.clone();
                    let cdr = pair_borrow.cdr.clone();
                    drop(pair_borrow); // Release borrow before evaluating

                    evaled_args.push(self.eval_inner(arg, env.clone())?);
                    current_args = cdr;
                }
                _ => return Err(EvalError::new("Improper argument list".to_string())),
            }
        }

        // Restore the application position before calling apply
        // This ensures that when we push a call frame, we capture the CALL SITE, not the last argument's position
        self.current_position = application_pos;
        self.current_source_file = application_file;

        // Apply procedure
        self.apply(proc, evaled_args)
    }

    /// Apply a procedure to evaluated arguments
    fn apply(&mut self, proc: Value, args: Vec<Value>) -> EvalResult {
        if let Value::Procedure(ref p) = proc {
            match &**p {
                Procedure::Primitive { name, func } => {
                    match *name {
                        "car" | "cdr" | "cons" | "null?" | "equal?" |
                        "cadr" | "caddr" | "cadddr" | "list" | "length" |
                        "reverse" | "append" | "list?" | "list-ref" |
                        "pair?" | "number?" | "integer?" | "real?" | "string?" |
                        "symbol?" | "char?" | "boolean?" | "zero?" | "positive?" |
                        "negative?" | "odd?" | "even?" |
                        "+" | "-" | "*" | "/" | "quotient" | "remainder" | "modulo" |
                        "=" | "<" | ">" | "<=" | ">=" |
                        "abs" | "min" | "max" |
                        "floor" | "ceiling" | "truncate" | "round" |
                        "sqrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" |
                        "exp" | "log" | "expt" |
                        "string-length" | "string-ref" | "substring" | "string-append" |
                        "string=?" | "string<?" | "string>?" | "string<=?" | "string>=?" |
                        "string-ci=?" | "string-ci<?" | "string-ci>?" | "string-ci<=?" | "string-ci>=?" |
                        "char=?" | "char<?" | "char>?" | "char<=?" | "char>=?" |
                        "char-ci=?" | "char-ci<?" | "char-ci>?" | "char-ci<=?" | "char-ci>=?" |
                        "char-upcase" | "char-downcase" |
                        "char-alphabetic?" | "char-numeric?" | "char-whitespace?" |
                        "char->integer" | "integer->char" |
                        "char-property" | "char-script-case" |
                        "symbol->string" | "string->symbol" |
                        "keyword?" | "keyword->string" | "string->keyword" |
                        "memq" | "memv" | "member" |
                        "assq" | "assv" | "assoc" |
                        "not" | "eq?" | "eqv?" |
                        "caar" | "cdar" | "cddr" |
                        "caaar" | "caadr" | "cadar" |
                        "cdaar" | "cdadr" | "cddar" | "cdddr" |
                        "vector" | "make-vector" | "vector-length" |
                        "vector-ref" | "vector-set!" |
                        "vector->list" | "list->vector" | "vector-fill!" |
                        "vector?" | "procedure?" |
                        "set-car!" | "set-cdr!" | "list-tail" |
                        "string-upcase" | "string-downcase" | "case-fold-down" |
                        "string-index" |
                        "string->number" | "number->string" |
                        "string->list" | "list->string" |
                        "gcd" | "lcm" |
                        "exact->inexact" | "inexact->exact" |
                        "make-string" | "string" | "reverse!" |
                        "string-set!" | "string-copy" | "string-fill!" |
                        "char-lower-case?" | "char-upper-case?" |
                        "last" | "last-pair" | "list-copy" |
                        "append!" | "iota" |
                        "take" | "drop" | "split-at" |
                        "filter" | "remove" |
                        "numerator" | "denominator" | "rationalize" |
                        "angle" | "magnitude" |
                        "null-list?" | "improper-list?" | "circular-list?" |
                        "bitwise-and" | "bitwise-ior" | "bitwise-xor" | "bitwise-not" |
                        "arithmetic-shift" | "bit-extract" |
                        "bitwise-bit-set?" | "bitwise-bit-count" |
                        "format-number" | "format-number-list" |
                        "empty-sosofo" | "sosofo-append" | "if-first-page" | "if-front-page" |
                        "current-node" |
                        "gi" | "data" | "id" |
                        "children" | "parent" | "attributes" |
                        "node-list?" | "empty-node-list" | "node-list-empty?" |
                        "node-list-length" | "node-list-first" |
                        "attribute-string" |
                        "node-list-rest" | "node-list-ref" | "node-list-reverse" |
                        "node?" | "sosofo?" | "quantity?" |
                        "color?" | "color" | "display-space?" | "inline-space?" |
                        "quantity->number" | "number->quantity" | "quantity-convert" |
                        "device-length" | "label-distance" |
                        "ancestor" | "descendants" | "follow" | "preced" | "ipreced" |
                        "node-list-last" | "node-list-union" | "node-list-intersection" |
                        "node-list-difference" | "node-list-remove-duplicates" |
                        "select-elements" | "first-sibling?" | "last-sibling?" |
                        "child-number" | "element-with-id" |
                        "element-number" | "hierarchical-number" | "hierarchical-number-recursive" |
                        "ancestors" | "document-element" | "have-ancestor?" |
                        "match-element?" | "node-list-map" |
                        "node-property" | "absolute-first-sibling?" | "absolute-last-sibling?" |
                        "node-list->list" | "node-list-contains?" |
                        "entity-system-id" | "entity-public-id" | "entity-type" |
                        "notation-system-id" | "notation-public-id" |
                        "current-language" | "current-mode" | "current-node-address" |
                        "current-node-page-number-sosofo" | "debug" |
                        "add" | "divide" | "equal" | "char-eq" | "char-lt" |
                        "exact?" | "inexact?" | "error" |
                        "address?" | "address-local?" | "address-visited?" |
                        "color-space?" | "color-space" | "display-space" | "inline-space" |
                        "glyph-id?" | "glyph-id" | "glyph-subst-table?" | "glyph-subst-table" | "glyph-subst" |
                        "time" | "time->string" | "time<=?" | "time<?" | "time>=?" | "time>?" |
                        "language?" | "language" | "style?" |
                        "string-equiv?" | "label-length" | "external-procedure" |
                        "declaration" | "dtd" | "epilog" | "prolog" | "sgml-declaration" | "sgml-parse" |
                        "entity-address" | "entity-generated-system-id" | "entity-name-normalize" | "general-name-normalize" | "normalize" |
                        "first-child-gi" | "tree-root" | "declare-default-language" | "read-entity" | "set-visited!" |
                        "sosofo-contains-node?" | "page-number-sosofo" | "ifollow" | "with-language" |
                        "all-element-number" | "ancestor-child-number" | "element-number-list" |
                        "inherited-attribute-string" | "inherited-element-attribute-string" |
                        "inherited-start-indent" | "inherited-end-indent" | "inherited-line-spacing" |
                        "inherited-font-family-name" | "inherited-font-size" | "inherited-font-weight" |
                        "inherited-font-posture" | "inherited-dbhtml-value" | "inherited-pi-value" |
                        "node-list" | "node-list=?" | "node-list-count" | "node-list-union-map" |
                        "node-list-symmetrical-difference" |
                        "node-list-address" | "node-list-error" | "node-list-no-order" |
                        "origin-to-subnode-rel-forest-addr" |
                        "named-node" | "named-node-list?" | "named-node-list-names" |
                        "select-by-class" | "select-children" |
                        "process-children-trim" | "process-element-with-id" | "process-first-descendant" |
                        "process-matching-children" | "next-match" => {
                            self.apply_primitive(name, &args)
                        }
                        // Note: I/O operations (display, write, newline, etc.) stay in non-arena path
                        // because they need actual side effects (stdout/stdin interaction)
                        _ => {
                            // Check if this is a known primitive that should be dispatched to apply_primitive
                            // This handles marker primitives created during symbol lookup
                            if self.get_primitive_static_name(name).is_some() {
                                self.apply_primitive(name, &args)
                            } else {
                                // Call the function directly for non-arena primitives (I/O, etc.)
                                // Don't push call frames for primitives - only for user lambdas
                                // This matches OpenJade's behavior
                                func(&args).map_err(|e| self.error_with_stack(e))
                            }
                        }
                    }
                }
                Procedure::Lambda { params, required_count, optional_defaults, body, env, source, name } => {
                    // Check argument count - must have at least required_count, at most params.len()
                    if args.len() < *required_count {
                        return Err(self.error_with_stack(format!(
                            "Lambda expects at least {} arguments, got {}",
                            required_count,
                            args.len()
                        )));
                    }
                    if args.len() > params.len() {
                        return Err(self.error_with_stack(format!(
                            "Lambda expects at most {} arguments, got {}",
                            params.len(),
                            args.len()
                        )));
                    }

                    // Save current position (call site) before switching to lambda's definition location
                    let saved_file = self.current_source_file.clone();
                    let saved_pos = self.current_position.clone();

                    // Only push call frame for NAMED functions (not anonymous lambdas)
                    // This matches OpenJade's behavior - it only tracks named function calls
                    let pushed_frame = if let Some(func_name) = name.clone() {
                        let call_site = match (&saved_file, &saved_pos) {
                            (Some(file), Some(pos)) => Some(SourceInfo {
                                file: file.clone(),
                                pos: pos.clone(),
                            }),
                            _ => None,
                        };
                        self.push_call_frame(func_name, call_site);
                        true
                    } else {
                        false
                    };

                    // Switch to lambda's definition location for evaluating the body
                    if let Some(ref src) = source {
                        self.current_source_file = Some(src.file.clone());
                        self.current_position = Some(src.pos.clone());
                    }

                    // Create new environment extending the closure environment
                    let lambda_env = Environment::extend(env.clone());

                    // Bind required and provided arguments
                    for (param_name, arg_value) in params.iter().zip(args.iter()) {
                        lambda_env.define(param_name, arg_value.clone());
                    }

                    // Bind optional parameters that weren't provided with their defaults
                    if args.len() < params.len() {
                        let num_optional_provided = args.len().saturating_sub(*required_count);
                        let num_optional_defaults_needed = (params.len() - *required_count) - num_optional_provided;

                        for i in 0..num_optional_defaults_needed {
                            let param_idx = *required_count + num_optional_provided + i;
                            let param_name = &params[param_idx];
                            let default_expr = &optional_defaults[num_optional_provided + i];

                            if std::env::var("DEBUG_OPTIONAL").is_ok() {
                                eprintln!("[DEBUG_OPTIONAL] Evaluating default for param '{}': {:?}", param_name, default_expr);
                                // Try to look up the parameter name in the environment
                                if let Some(val) = env.lookup(param_name) {
                                    eprintln!("[DEBUG_OPTIONAL] Found '{}' in closure env: {:?}", param_name, val);
                                } else {
                                    eprintln!("[DEBUG_OPTIONAL] '{}' not found in closure env (expected)", param_name);
                                }
                            }

                            // Evaluate default expression in the closure environment
                            let default_value = self.eval_inner(default_expr.clone(), env.clone())?;

                            if std::env::var("DEBUG_OPTIONAL").is_ok() {
                                eprintln!("[DEBUG_OPTIONAL] Evaluated to: {:?}", default_value);
                            }

                            lambda_env.define(param_name, default_value);
                        }
                    }

                    // Evaluate body in the new environment
                    let result = self.eval_inner((**body).clone(), lambda_env);

                    // Restore previous position
                    self.current_source_file = saved_file;
                    self.current_position = saved_pos;

                    // Pop call frame if we pushed one
                    if pushed_frame {
                        self.pop_call_frame();
                    }

                    result
                }
            }
        } else if let Value::Symbol(sym) = &proc {
            // Special handling for symbols returned by external-procedure
            // These are primitive names that need to be dispatched to arena primitives
            let sym_str = sym.as_ref();

            // Dispatch directly to the arena primitive by name
            self.apply_primitive(sym_str, &args)
        } else {
            Err(self.error_with_stack(format!(
                "Not a procedure: {:?}",
                proc
            )))
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_env() -> Gc<Environment> {
        Environment::new_global()
    }

    #[test]
    fn test_eval_self_evaluating() {
        let mut eval = Evaluator::new();
        let env = make_env();

        assert!(eval.eval(Value::integer(42), env.clone()).unwrap().is_integer());
        assert!(eval.eval(Value::bool(true), env.clone()).unwrap().is_bool());
        assert!(eval.eval(Value::string("hello".to_string()), env).unwrap().is_string());
    }

    #[test]
    fn test_eval_quote() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (quote (1 2 3))
        let expr = Value::cons(
            Value::symbol("quote"),
            Value::cons(
                Value::cons(
                    Value::integer(1),
                    Value::cons(Value::integer(2), Value::cons(Value::integer(3), Value::Nil)),
                ),
                Value::Nil,
            ),
        );

        let result = eval.eval(expr, env).unwrap();
        assert!(result.is_list());
    }

    #[test]
    fn test_eval_if_true() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (if #t 1 2)
        let expr = Value::cons(
            Value::symbol("if"),
            Value::cons(
                Value::bool(true),
                Value::cons(Value::integer(1), Value::cons(Value::integer(2), Value::Nil)),
            ),
        );

        let result = eval.eval(expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 1);
        } else {
            panic!("Expected integer 1");
        }
    }

    #[test]
    fn test_eval_if_false() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (if #f 1 2)
        let expr = Value::cons(
            Value::symbol("if"),
            Value::cons(
                Value::bool(false),
                Value::cons(Value::integer(1), Value::cons(Value::integer(2), Value::Nil)),
            ),
        );

        let result = eval.eval(expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 2);
        } else {
            panic!("Expected integer 2");
        }
    }

    #[test]
    fn test_eval_define() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (define x 42)
        let expr = Value::cons(
            Value::symbol("define"),
            Value::cons(Value::symbol("x"), Value::cons(Value::integer(42), Value::Nil)),
        );

        eval.eval(expr, env.clone()).unwrap();

        // Check that x is defined
        assert!(env.is_defined("x"));
        if let Value::Integer(n) = env.lookup("x").unwrap() {
            assert_eq!(n, 42);
        }
    }

    #[test]
    fn test_eval_symbol_lookup() {
        let mut eval = Evaluator::new();
        let env = make_env();

        env.define("x", Value::integer(99));

        let result = eval.eval(Value::symbol("x"), env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 99);
        } else {
            panic!("Expected integer 99");
        }
    }

    #[test]
    fn test_eval_and() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (and #t #t)
        let expr = Value::cons(
            Value::symbol("and"),
            Value::cons(Value::bool(true), Value::cons(Value::bool(true), Value::Nil)),
        );

        let result = eval.eval(expr, env.clone()).unwrap();
        assert!(result.is_true());

        // (and #t #f)
        let expr = Value::cons(
            Value::symbol("and"),
            Value::cons(Value::bool(true), Value::cons(Value::bool(false), Value::Nil)),
        );

        let result = eval.eval(expr, env).unwrap();
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_or() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (or #f #t)
        let expr = Value::cons(
            Value::symbol("or"),
            Value::cons(Value::bool(false), Value::cons(Value::bool(true), Value::Nil)),
        );

        let result = eval.eval(expr, env.clone()).unwrap();
        assert!(result.is_true());

        // (or #f #f)
        let expr = Value::cons(
            Value::symbol("or"),
            Value::cons(Value::bool(false), Value::cons(Value::bool(false), Value::Nil)),
        );

        let result = eval.eval(expr, env).unwrap();
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_lambda_creation() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (lambda (x) x)
        let expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                Value::cons(Value::symbol("x"), Value::Nil),
                Value::cons(Value::symbol("x"), Value::Nil),
            ),
        );

        let result = eval.eval(expr, env).unwrap();
        assert!(result.is_procedure());
    }

    #[test]
    fn test_eval_lambda_application() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // ((lambda (x) x) 42)
        let lambda_expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                Value::cons(Value::symbol("x"), Value::Nil),
                Value::cons(Value::symbol("x"), Value::Nil),
            ),
        );

        let app_expr = Value::cons(lambda_expr, Value::cons(Value::integer(42), Value::Nil));

        let result = eval.eval(app_expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 42);
        } else {
            panic!("Expected integer 42");
        }
    }

    #[test]
    fn test_eval_lambda_multiple_params() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // ((lambda (x y) x) 1 2) - Just return first param
        let params = Value::cons(Value::symbol("x"), Value::cons(Value::symbol("y"), Value::Nil));
        let body = Value::symbol("x");

        let lambda_expr = Value::cons(Value::symbol("lambda"), Value::cons(params, Value::cons(body, Value::Nil)));

        let app_expr = Value::cons(
            lambda_expr,
            Value::cons(Value::integer(1), Value::cons(Value::integer(2), Value::Nil)),
        );

        let result = eval.eval(app_expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 1);
        } else {
            panic!("Expected integer 1");
        }
    }

    #[test]
    fn test_eval_lambda_wrong_arg_count() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // ((lambda (x) x) 1 2) - wrong argument count
        let lambda_expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                Value::cons(Value::symbol("x"), Value::Nil),
                Value::cons(Value::symbol("x"), Value::Nil),
            ),
        );

        let app_expr = Value::cons(
            lambda_expr,
            Value::cons(Value::integer(1), Value::cons(Value::integer(2), Value::Nil)),
        );

        let result = eval.eval(app_expr, env);
        assert!(result.is_err());
    }

    #[test]
    fn test_eval_lambda_closure() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (define x 10)
        env.define("x", Value::integer(10));

        // ((lambda (y) x) 20)
        // Should capture x from outer environment and ignore y
        let lambda_expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                Value::cons(Value::symbol("y"), Value::Nil),
                Value::cons(Value::symbol("x"), Value::Nil),
            ),
        );

        let app_expr = Value::cons(lambda_expr, Value::cons(Value::integer(20), Value::Nil));

        let result = eval.eval(app_expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 10); // Should get x from outer environment
        } else {
            panic!("Expected integer 10 from closure");
        }
    }

    #[test]
    fn test_eval_lambda_no_params() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // ((lambda () 42))
        let lambda_expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(Value::Nil, Value::cons(Value::integer(42), Value::Nil)),
        );

        let app_expr = Value::cons(lambda_expr, Value::Nil);

        let result = eval.eval(app_expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 42);
        } else {
            panic!("Expected integer 42");
        }
    }

    #[test]
    fn test_eval_lambda_multiple_body_expressions() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // ((lambda (x) 1 2 x) 99)
        // Should return x (last expression)
        let params = Value::cons(Value::symbol("x"), Value::Nil);
        let body1 = Value::integer(1);
        let body2 = Value::integer(2);
        let body3 = Value::symbol("x");

        let lambda_expr = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                params,
                Value::cons(body1, Value::cons(body2, Value::cons(body3, Value::Nil))),
            ),
        );

        let app_expr = Value::cons(lambda_expr, Value::cons(Value::integer(99), Value::Nil));

        let result = eval.eval(app_expr, env).unwrap();
        if let Value::Integer(n) = result {
            assert_eq!(n, 99);
        } else {
            panic!("Expected integer 99");
        }
    }

    #[test]
    fn test_element_rule_multiple_sosofos_error() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (element foo expr1 expr2) - should error
        let expr = Value::cons(
            Value::symbol("element"),
            Value::cons(
                Value::symbol("foo"),
                Value::cons(
                    Value::symbol("expr1"),
                    Value::cons(Value::symbol("expr2"), Value::Nil),
                ),
            ),
        );

        let result = eval.eval(expr, env);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("can only contain one sosofo expression"));
        assert!(err_msg.contains("sosofo-append"));
    }

    #[test]
    fn test_element_rule_single_sosofo_ok() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (element foo expr) - should succeed
        let expr = Value::cons(
            Value::symbol("element"),
            Value::cons(
                Value::symbol("foo"),
                Value::cons(Value::symbol("expr"), Value::Nil),
            ),
        );

        let result = eval.eval(expr, env);
        assert!(result.is_ok());
    }

    #[test]
    fn test_default_rule_multiple_sosofos_error() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (default expr1 expr2) - should error
        let expr = Value::cons(
            Value::symbol("default"),
            Value::cons(
                Value::symbol("expr1"),
                Value::cons(Value::symbol("expr2"), Value::Nil),
            ),
        );

        let result = eval.eval(expr, env);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("can only contain one sosofo expression"));
        assert!(err_msg.contains("sosofo-append"));
    }

    #[test]
    fn test_default_rule_single_sosofo_ok() {
        let mut eval = Evaluator::new();
        let env = make_env();

        // (default expr) - should succeed
        let expr = Value::cons(
            Value::symbol("default"),
            Value::cons(Value::symbol("expr"), Value::Nil),
        );

        let result = eval.eval(expr, env);
        assert!(result.is_ok());
    }
}
