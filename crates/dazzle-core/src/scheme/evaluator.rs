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
    /// Construction rules indexed by element name
    /// In OpenJade, rules are stored in intrusive linked lists and indexed lazily.
    /// We use a simple Vec for now - can optimize later with HashMap if needed.
    pub rules: Vec<ConstructionRule>,

    /// Default construction rule (fallback when no specific rule matches)
    pub default_rule: Option<Value>,
}

impl ProcessingMode {
    /// Create a new empty processing mode
    pub fn new() -> Self {
        ProcessingMode {
            rules: Vec::new(),
            default_rule: None,
        }
    }

    /// Add a construction rule
    pub fn add_rule(&mut self, element_name: String, context: Vec<String>, expr: Value, source_file: Option<String>, source_pos: Option<Position>) {
        self.rules.push(ConstructionRule {
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
    pub fn find_match(&self, gi: &str, node: &dyn crate::grove::Node) -> Option<&ConstructionRule> {
        self.rules.iter().find(|rule| {
            // Element name must match
            if rule.element_name != gi {
                return false;
            }

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
    /// The document grove (for element-with-id, etc.)
    grove: Option<Rc<dyn Grove>>,

    /// Current node context (for current-node primitive)
    ///
    /// This changes dynamically as we process the document tree.
    /// When evaluating a template, this starts as the root node.
    /// When processing children, it changes to each child node.
    current_node: Option<Rc<Box<dyn Node>>>,

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
            grove: None,
            current_node: None,
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
        Evaluator {
            grove: Some(grove),
            current_node: None,
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
        self.grove = Some(grove);
    }

    /// Get the grove
    pub fn grove(&self) -> Option<&Rc<dyn Grove>> {
        self.grove.as_ref()
    }

    /// Set the current node
    pub fn set_current_node(&mut self, node: Box<dyn Node>) {
        self.current_node = Some(Rc::new(node));
    }

    /// Get the current node
    pub fn current_node(&self) -> Option<Rc<Box<dyn Node>>> {
        self.current_node.clone()
    }

    /// Clear the current node
    pub fn clear_current_node(&mut self) {
        self.current_node = None;
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
        let root_node = match &self.grove {
            Some(grove) => grove.root(),
            None => return Err(EvalError::new("No grove set".to_string())),
        };

        // Set as current node and start processing
        self.current_node = Some(Rc::new(root_node));
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
        let node = match &self.current_node {
            Some(n) => n.clone(),
            None => return Err(EvalError::new("No current node".to_string())),
        };

        // Get element name (GI)
        let gi = match node.gi() {
            Some(gi) => gi,
            None => {
                // Not an element (e.g., text node, comment, etc.)
                // For text nodes, output their data content
                // Skip whitespace-only text nodes (OpenJade behavior)
                if node.is_text() {
                    if let Some(text) = node.data() {
                        // Skip if text is only whitespace
                        if !text.trim().is_empty() {
                            // Output text to backend with HTML escaping
                            if let Some(ref backend) = self.backend {
                                let escaped_text = text
                                    .replace('&', "&amp;")
                                    .replace('<', "&lt;")
                                    .replace('>', "&gt;");
                                backend.borrow_mut().formatting_instruction(&escaped_text)
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
            grove: self.grove.clone(),
            current_node: self.current_node.clone(),
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
            Value::Symbol(ref name) => env
                .lookup(name)
                .ok_or_else(|| self.error_with_stack(format!("Undefined variable: {}", name))),

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
        // If multiple expressions, wrap them in sosofo-append (OpenJade behavior)
        let construction_expr = if args_vec.len() == 2 {
            // Single expression - use as is
            args_vec[1].clone()
        } else {
            // Multiple expressions - wrap in (sosofo-append expr1 expr2 ...)
            let sosofo_append_sym = Value::symbol("sosofo-append");
            let mut exprs = vec![sosofo_append_sym];
            exprs.extend_from_slice(&args_vec[1..]);
            self.vec_to_list(exprs)
        };

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

        // If multiple expressions, wrap them in sosofo-append (OpenJade behavior)
        let construction_expr = if args_vec.len() == 1 {
            // Single expression - use as is
            args_vec[0].clone()
        } else {
            // Multiple expressions - wrap in (sosofo-append expr1 expr2 ...)
            let sosofo_append_sym = Value::symbol("sosofo-append");
            let mut exprs = vec![sosofo_append_sym];
            exprs.extend_from_slice(&args_vec);
            self.vec_to_list(exprs)
        };

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
        let current_node = match &self.current_node {
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
                let saved_node = self.current_node.clone();

                // Set child as current node
                self.current_node = Some(Rc::new(child_node));

                // Process the child node
                result = self.process_node(env.clone())?;

                // Restore current node
                self.current_node = saved_node;
            }

            // Move to rest of children
            children = children.rest();
        }

        Ok(result)
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
                let saved_node = self.current_node.clone();

                // Set this node as current node
                self.current_node = Some(Rc::new(node));

                // Process the node
                result = self.process_node(env.clone())?;

                // Restore current node
                self.current_node = saved_node;
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
                    "link" | "scroll" | "marginalia" | "leader" | "table" | "table-row" | "table-cell" | "table-column" | "table-part" | "paragraph" | "paragraph-break" | "sequence" => {
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

        Ok(Value::Unspecified)
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

        // No match found - R4RS specifies that case without an else clause
        // returns unspecified when no clause matches
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
                Procedure::Primitive { name: _, func } => {
                    // Don't push call frames for primitives - only for user lambdas
                    // This matches OpenJade's behavior
                    func(&args).map_err(|e| self.error_with_stack(e))
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

                            // Evaluate default expression in the closure environment
                            let default_value = self.eval_inner(default_expr.clone(), env.clone())?;
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
