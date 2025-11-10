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
            Value::Unspecified => {
                crate::scheme::arena::UNSPECIFIED_ID
            }
            _ => {
                // For now, unsupported types return NIL
                // Phase 3 will handle more types
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
            _ => {
                // For now, unsupported types return NIL
                // Phase 3 will handle more types
                Value::Nil
            }
        }
    }

    /// Apply arena primitive (Phase 2+3 hot path)
    fn apply_arena_primitive(&mut self, name: &str, args: &[Value]) -> EvalResult {
        use crate::scheme::arena_primitives::{
            // Phase 2: Hot primitives
            arena_car, arena_cdr, arena_cons, arena_null, arena_equal,
            // Phase 3 Batch 1: List operations
            arena_cadr, arena_caddr, arena_cadddr, arena_list, arena_length,
            arena_reverse, arena_append, arena_list_p, arena_list_ref,
            // Phase 3 Batch 2: Type predicates
            arena_pair_p, arena_number_p, arena_integer_p, arena_real_p,
            arena_string_p, arena_symbol_p, arena_char_p, arena_boolean_p,
            arena_zero_p, arena_positive_p, arena_negative_p, arena_odd_p, arena_even_p,
            // Phase 3 Batch 3: Arithmetic
            arena_add, arena_subtract, arena_multiply, arena_divide,
            arena_quotient, arena_remainder, arena_modulo,
            arena_num_eq, arena_num_lt, arena_num_gt, arena_num_le, arena_num_ge,
            arena_abs, arena_min, arena_max,
            arena_floor, arena_ceiling, arena_truncate, arena_round,
            arena_sqrt, arena_sin, arena_cos, arena_tan,
            arena_asin, arena_acos, arena_atan, arena_exp, arena_log, arena_expt,
            // Phase 3 Batch 4: String operations
            arena_string_length, arena_string_ref, arena_substring, arena_string_append,
            arena_string_eq, arena_string_lt, arena_string_gt, arena_string_le, arena_string_ge,
            arena_string_ci_eq, arena_string_ci_lt, arena_string_ci_gt,
            arena_string_ci_le, arena_string_ci_ge,
            // Phase 3 Batch 5: Character operations
            arena_char_eq, arena_char_lt, arena_char_gt, arena_char_le, arena_char_ge,
            arena_char_ci_eq, arena_char_ci_lt, arena_char_ci_gt, arena_char_ci_le, arena_char_ci_ge,
            arena_char_upcase, arena_char_downcase,
            arena_char_alphabetic_p, arena_char_numeric_p, arena_char_whitespace_p,
            arena_char_to_integer, arena_integer_to_char,
            arena_char_property, arena_char_script_case,
            // Phase 3 Batch 6: Symbol/Keyword operations
            arena_symbol_to_string, arena_string_to_symbol,
            arena_keyword_p, arena_keyword_to_string, arena_string_to_keyword,
            // Phase 3 Batch 7: List utilities
            arena_memq, arena_memv, arena_member,
            arena_assq, arena_assv, arena_assoc,
            // Phase 3 Batch 8: Logic and list accessors
            arena_not, arena_eq_p, arena_eqv_p,
            arena_caar, arena_cdar, arena_cddr,
            // Phase 3 Batch 9: Extended list accessors
            arena_caaar, arena_caadr, arena_cadar,
            arena_cdaar, arena_cdadr, arena_cddar, arena_cdddr,
            // Phase 3 Batch 10: Vector operations
            arena_vector, arena_make_vector, arena_vector_length,
            arena_vector_ref, arena_vector_set,
            arena_vector_to_list, arena_list_to_vector, arena_vector_fill,
            // Phase 3 Batch 11: Additional type predicates & utilities
            arena_vector_p, arena_procedure_p,
            arena_set_car, arena_set_cdr, arena_list_tail,
            // Phase 3 Batch 12: String utilities
            arena_string_upcase, arena_string_downcase,
            arena_string_to_list, arena_list_to_string,
            // Phase 3 Batch 13: Miscellaneous utilities
            arena_gcd, arena_lcm,
            arena_exact_to_inexact, arena_inexact_to_exact,
            arena_make_string, arena_string, arena_reverse_bang,
            // Phase 3 Batch 14: Additional string and character operations
            arena_string_set, arena_string_copy, arena_string_fill,
            arena_char_lower_case_p, arena_char_upper_case_p,
            // Phase 3 Batch 15: List utilities
            arena_last, arena_last_pair, arena_list_copy,
            arena_append_bang, arena_iota,
            // Phase 3 Batch 16: Extended list operations
            arena_take, arena_drop, arena_split_at,
            arena_filter, arena_remove,
            // Phase 3 Batch 17: Additional numeric operations
            arena_numerator, arena_denominator, arena_rationalize,
            arena_angle, arena_magnitude, arena_string_to_number_radix,
            // Phase 3 Batch 18: Conversion and utility operations
            arena_number_to_string_radix,
            arena_null_list_p, arena_improper_list_p, arena_circular_list_p,
            // Phase 3 Batch 19: Bitwise operations
            arena_bitwise_and, arena_bitwise_ior, arena_bitwise_xor, arena_bitwise_not,
            arena_arithmetic_shift, arena_bit_extract,
            arena_bitwise_bit_set_p, arena_bitwise_bit_count,
            // Phase 3 Batch 20: I/O and display operations
            arena_display, arena_newline, arena_write, arena_write_char,
            arena_read_char, arena_eof_object_p,
            // Phase 3 Batch 21: Number formatting
            arena_format_number, arena_format_number_list,
            // Phase 3 Batch 22: Simple constants
            arena_empty_sosofo,
            // Phase 3 Batch 23: Grove operations
            arena_current_node,
            // Phase 3 Batch 24: Grove node properties
            arena_gi, arena_data, arena_id,
            // Phase 3 Batch 25: Grove navigation
            arena_children, arena_parent, arena_attributes,
            // Phase 3 Batch 26: Node-list operations
            arena_node_list_p, arena_empty_node_list, arena_node_list_empty_p,
            arena_node_list_length, arena_node_list_first,
            // Phase 3 Batch 27: Attribute operations
            arena_attribute_string,
            // Phase 3 Batch 28: More node-list operations
            arena_node_list_rest, arena_node_list_ref, arena_node_list_reverse,
            // Phase 3 Batch 29: Type predicates
            arena_node_p, arena_sosofo_p, arena_quantity_p,
            // Phase 3 Batch 30: Color and spacing stubs
            arena_color_p, arena_color, arena_display_space_p, arena_inline_space_p,
            // Phase 3 Batch 31: Quantity operations
            arena_quantity_to_number, arena_number_to_quantity, arena_quantity_convert,
            arena_device_length, arena_label_distance,
            // Phase 3 Batch 32: Grove navigation operations
            arena_ancestor, arena_descendants, arena_follow, arena_preced, arena_ipreced,
            // Phase 3 Batch 33: Node-list set operations
            arena_node_list_last, arena_node_list_union, arena_node_list_intersection,
            arena_node_list_difference, arena_node_list_remove_duplicates,
            // Phase 3 Batch 34: Element selection and position operations
            arena_select_elements, arena_first_sibling_p, arena_last_sibling_p,
            arena_child_number, arena_element_with_id,
            // Phase 3 Batch 35: Element numbering operations
            arena_element_number, arena_hierarchical_number, arena_hierarchical_number_recursive,
            // Phase 3 Batch 36: Grove utility operations
            arena_ancestors, arena_document_element, arena_have_ancestor_p,
            arena_match_element_p, arena_node_list_map,
            // Phase 3 Batch 37: Final DSSSL operations
            arena_node_property, arena_absolute_first_sibling_p, arena_absolute_last_sibling_p,
            arena_node_list_to_list, arena_node_list_contains_p,
            // Phase 3 Batch 38: Entity and notation operations
            arena_entity_system_id, arena_entity_public_id, arena_entity_type,
            arena_notation_system_id, arena_notation_public_id,
            // Phase 3 Batch 39: Context and debugging primitives
            arena_current_language, arena_current_mode, arena_current_node_address,
            arena_current_node_page_number_sosofo, arena_debug,
            // Phase 3 Batch 41: Type predicates and error
            arena_exact_p, arena_inexact_p, arena_error,
            // Phase 3 Batch 42: Address type stubs
            arena_address_p, arena_address_local_p, arena_address_visited_p,
            // Phase 3 Batch 43: Color/display space stubs
            arena_color_space_p, arena_color_space, arena_display_space, arena_inline_space,
            // Phase 3 Batch 44: Glyph type stubs
            arena_glyph_id_p, arena_glyph_id, arena_glyph_subst_table_p,
            arena_glyph_subst_table, arena_glyph_subst,
            // Phase 3 Batch 45: Time type stubs
            arena_time, arena_time_to_string, arena_time_le, arena_time_lt,
            arena_time_ge, arena_time_gt,
            // Phase 3 Batch 46: Language and style type stubs
            arena_language_p, arena_language, arena_style_p,
            // Phase 3 Batch 47: String comparison and simple stubs
            arena_string_equiv_p, arena_label_length, arena_external_procedure,
            // Phase 3 Batch 48: DTD/SGML stubs
            arena_declaration, arena_dtd, arena_epilog, arena_prolog,
            arena_sgml_declaration, arena_sgml_parse,
            // Phase 3 Batch 49: Entity/normalization stubs
            arena_entity_address, arena_entity_generated_system_id,
            arena_entity_name_normalize, arena_general_name_normalize,
            // Phase 3 Batch 50: Simple navigation and declaration stubs
            arena_first_child_gi, arena_tree_root, arena_declare_default_language,
            arena_read_entity, arena_set_visited,
            // Phase 3 Batch 51: Sosofo and navigation stubs
            arena_sosofo_contains_node_p, arena_page_number_sosofo, arena_ifollow, arena_with_language,
            // Phase 3 Batch 52: Element numbering stubs
            arena_all_element_number, arena_ancestor_child_number, arena_element_number_list,
            // Phase 3 Batch 53: Inherited property stubs - Part 1
            arena_inherited_attribute_string, arena_inherited_element_attribute_string,
            arena_inherited_start_indent, arena_inherited_end_indent, arena_inherited_line_spacing,
            // Phase 3 Batch 54: Inherited property stubs - Part 2
            arena_inherited_font_family_name, arena_inherited_font_size, arena_inherited_font_weight,
            arena_inherited_font_posture, arena_inherited_dbhtml_value, arena_inherited_pi_value,
            // Phase 3 Batch 55: Node-list operation stubs - Part 1 (node-list-count is user-defined)
            arena_node_list, arena_node_list_eq_p,
            arena_node_list_union_map, arena_node_list_symmetrical_difference,
            // Phase 3 Batch 56: Node-list operation stubs - Part 2
            arena_node_list_address, arena_node_list_error, arena_node_list_no_order,
            arena_origin_to_subnode_rel_forest_addr,
            // Phase 3 Batch 57: Named node list stubs
            arena_named_node, arena_named_node_list_p, arena_named_node_list_names,
            // Phase 3 Batch 58: Selection operation stubs (1) - NOTE: select-children is user-defined
            arena_select_by_class,
            // Phase 3 Batch 59: Processing operation stubs (final 5!)
            arena_process_children_trim, arena_process_element_with_id, arena_process_first_descendant,
            arena_process_matching_children, arena_next_match,
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
            // Phase 2: Hot primitives (5)
            "car" => arena_car(&self.arena, &arena_args),
            "cdr" => arena_cdr(&self.arena, &arena_args),
            "cons" => arena_cons(&mut self.arena, &arena_args),
            "null?" => arena_null(&self.arena, &arena_args),
            "equal?" => arena_equal(&self.arena, &arena_args),
            // Phase 3 Batch 1: List operations (9)
            "cadr" => arena_cadr(&self.arena, &arena_args),
            "caddr" => arena_caddr(&self.arena, &arena_args),
            "cadddr" => arena_cadddr(&self.arena, &arena_args),
            "list" => arena_list(&mut self.arena, &arena_args),
            "length" => arena_length(&mut self.arena, &arena_args),
            "reverse" => arena_reverse(&mut self.arena, &arena_args),
            "append" => arena_append(&mut self.arena, &arena_args),
            "list?" => arena_list_p(&self.arena, &arena_args),
            "list-ref" => arena_list_ref(&self.arena, &arena_args),
            // Phase 3 Batch 2: Type predicates (13)
            "pair?" => arena_pair_p(&self.arena, &arena_args),
            "number?" => arena_number_p(&self.arena, &arena_args),
            "integer?" => arena_integer_p(&self.arena, &arena_args),
            "real?" => arena_real_p(&self.arena, &arena_args),
            "string?" => arena_string_p(&self.arena, &arena_args),
            "symbol?" => arena_symbol_p(&self.arena, &arena_args),
            "char?" => arena_char_p(&self.arena, &arena_args),
            "boolean?" => arena_boolean_p(&self.arena, &arena_args),
            "zero?" => arena_zero_p(&self.arena, &arena_args),
            "positive?" => arena_positive_p(&self.arena, &arena_args),
            "negative?" => arena_negative_p(&self.arena, &arena_args),
            "odd?" => arena_odd_p(&self.arena, &arena_args),
            "even?" => arena_even_p(&self.arena, &arena_args),
            // Phase 3 Batch 3: Arithmetic (29)
            "+" => arena_add(&mut self.arena, &arena_args),
            "-" => arena_subtract(&mut self.arena, &arena_args),
            "*" => arena_multiply(&mut self.arena, &arena_args),
            "/" => arena_divide(&mut self.arena, &arena_args),
            "quotient" => arena_quotient(&mut self.arena, &arena_args),
            "remainder" => arena_remainder(&mut self.arena, &arena_args),
            "modulo" => arena_modulo(&mut self.arena, &arena_args),
            "=" => arena_num_eq(&self.arena, &arena_args),
            "<" => arena_num_lt(&self.arena, &arena_args),
            ">" => arena_num_gt(&self.arena, &arena_args),
            "<=" => arena_num_le(&self.arena, &arena_args),
            ">=" => arena_num_ge(&self.arena, &arena_args),
            "abs" => arena_abs(&mut self.arena, &arena_args),
            "min" => arena_min(&mut self.arena, &arena_args),
            "max" => arena_max(&mut self.arena, &arena_args),
            "floor" => arena_floor(&mut self.arena, &arena_args),
            "ceiling" => arena_ceiling(&mut self.arena, &arena_args),
            "truncate" => arena_truncate(&mut self.arena, &arena_args),
            "round" => arena_round(&mut self.arena, &arena_args),
            "sqrt" => arena_sqrt(&mut self.arena, &arena_args),
            "sin" => arena_sin(&mut self.arena, &arena_args),
            "cos" => arena_cos(&mut self.arena, &arena_args),
            "tan" => arena_tan(&mut self.arena, &arena_args),
            "asin" => arena_asin(&mut self.arena, &arena_args),
            "acos" => arena_acos(&mut self.arena, &arena_args),
            "atan" => arena_atan(&mut self.arena, &arena_args),
            "exp" => arena_exp(&mut self.arena, &arena_args),
            "log" => arena_log(&mut self.arena, &arena_args),
            "expt" => arena_expt(&mut self.arena, &arena_args),
            // Phase 3 Batch 4: String operations (14)
            "string-length" => arena_string_length(&mut self.arena, &arena_args),
            "string-ref" => arena_string_ref(&mut self.arena, &arena_args),
            "substring" => arena_substring(&mut self.arena, &arena_args),
            "string-append" => arena_string_append(&mut self.arena, &arena_args),
            "string=?" => arena_string_eq(&self.arena, &arena_args),
            "string<?" => arena_string_lt(&self.arena, &arena_args),
            "string>?" => arena_string_gt(&self.arena, &arena_args),
            "string<=?" => arena_string_le(&self.arena, &arena_args),
            "string>=?" => arena_string_ge(&self.arena, &arena_args),
            "string-ci=?" => arena_string_ci_eq(&self.arena, &arena_args),
            "string-ci<?" => arena_string_ci_lt(&self.arena, &arena_args),
            "string-ci>?" => arena_string_ci_gt(&self.arena, &arena_args),
            "string-ci<=?" => arena_string_ci_le(&self.arena, &arena_args),
            "string-ci>=?" => arena_string_ci_ge(&self.arena, &arena_args),

            // Phase 3 Batch 5: Character operations (19)
            "char=?" => arena_char_eq(&self.arena, &arena_args),
            "char<?" => arena_char_lt(&self.arena, &arena_args),
            "char>?" => arena_char_gt(&self.arena, &arena_args),
            "char<=?" => arena_char_le(&self.arena, &arena_args),
            "char>=?" => arena_char_ge(&self.arena, &arena_args),
            "char-ci=?" => arena_char_ci_eq(&self.arena, &arena_args),
            "char-ci<?" => arena_char_ci_lt(&self.arena, &arena_args),
            "char-ci>?" => arena_char_ci_gt(&self.arena, &arena_args),
            "char-ci<=?" => arena_char_ci_le(&self.arena, &arena_args),
            "char-ci>=?" => arena_char_ci_ge(&self.arena, &arena_args),
            "char-upcase" => arena_char_upcase(&mut self.arena, &arena_args),
            "char-downcase" => arena_char_downcase(&mut self.arena, &arena_args),
            "char-alphabetic?" => arena_char_alphabetic_p(&self.arena, &arena_args),
            "char-numeric?" => arena_char_numeric_p(&self.arena, &arena_args),
            "char-whitespace?" => arena_char_whitespace_p(&self.arena, &arena_args),
            "char->integer" => arena_char_to_integer(&mut self.arena, &arena_args),
            "integer->char" => arena_integer_to_char(&mut self.arena, &arena_args),
            "char-property" => arena_char_property(&mut self.arena, &arena_args),
            "char-script-case" => arena_char_script_case(&mut self.arena, &arena_args),

            // Phase 3 Batch 6: Symbol/Keyword operations (5)
            "symbol->string" => arena_symbol_to_string(&mut self.arena, &arena_args),
            "string->symbol" => arena_string_to_symbol(&mut self.arena, &arena_args),
            "keyword?" => arena_keyword_p(&self.arena, &arena_args),
            "keyword->string" => arena_keyword_to_string(&mut self.arena, &arena_args),
            "string->keyword" => arena_string_to_keyword(&mut self.arena, &arena_args),

            // Phase 3 Batch 7: List utilities (6)
            "memq" => arena_memq(&self.arena, &arena_args),
            "memv" => arena_memv(&self.arena, &arena_args),
            "member" => arena_member(&self.arena, &arena_args),
            "assq" => arena_assq(&self.arena, &arena_args),
            "assv" => arena_assv(&self.arena, &arena_args),
            "assoc" => arena_assoc(&self.arena, &arena_args),

            // Phase 3 Batch 8: Logic and list accessors (6)
            "not" => arena_not(&self.arena, &arena_args),
            "eq?" => arena_eq_p(&self.arena, &arena_args),
            "eqv?" => arena_eqv_p(&self.arena, &arena_args),
            "caar" => arena_caar(&self.arena, &arena_args),
            "cdar" => arena_cdar(&self.arena, &arena_args),
            "cddr" => arena_cddr(&self.arena, &arena_args),

            // Phase 3 Batch 9: Extended list accessors (6)
            "caaar" => arena_caaar(&self.arena, &arena_args),
            "caadr" => arena_caadr(&self.arena, &arena_args),
            "cadar" => arena_cadar(&self.arena, &arena_args),
            "cdaar" => arena_cdaar(&self.arena, &arena_args),
            "cdadr" => arena_cdadr(&self.arena, &arena_args),
            "cddar" => arena_cddar(&self.arena, &arena_args),
            "cdddr" => arena_cdddr(&self.arena, &arena_args),

            // Phase 3 Batch 10: Vector operations (8)
            "vector" => arena_vector(&mut self.arena, &arena_args),
            "make-vector" => arena_make_vector(&mut self.arena, &arena_args),
            "vector-length" => arena_vector_length(&mut self.arena, &arena_args),
            "vector-ref" => arena_vector_ref(&self.arena, &arena_args),
            "vector-set!" => arena_vector_set(&mut self.arena, &arena_args),
            "vector->list" => arena_vector_to_list(&mut self.arena, &arena_args),
            "list->vector" => arena_list_to_vector(&mut self.arena, &arena_args),
            "vector-fill!" => arena_vector_fill(&mut self.arena, &arena_args),

            // Phase 3 Batch 11: Additional type predicates & utilities (5)
            "vector?" => arena_vector_p(&self.arena, &arena_args),
            "procedure?" => arena_procedure_p(&self.arena, &arena_args),
            "set-car!" => arena_set_car(&mut self.arena, &arena_args),
            "set-cdr!" => arena_set_cdr(&mut self.arena, &arena_args),
            "list-tail" => arena_list_tail(&self.arena, &arena_args),

            // Phase 3 Batch 12: String utilities (6)
            "string-upcase" => arena_string_upcase(&mut self.arena, &arena_args),
            "string-downcase" => arena_string_downcase(&mut self.arena, &arena_args),
            "string->number" => arena_string_to_number_radix(&mut self.arena, &arena_args), // Updated to support radix
            "number->string" => arena_number_to_string_radix(&mut self.arena, &arena_args), // Updated to support radix
            "string->list" => arena_string_to_list(&mut self.arena, &arena_args),
            "list->string" => arena_list_to_string(&mut self.arena, &arena_args),

            // Phase 3 Batch 13: Miscellaneous utilities (7)
            "gcd" => arena_gcd(&mut self.arena, &arena_args),
            "lcm" => arena_lcm(&mut self.arena, &arena_args),
            "exact->inexact" => arena_exact_to_inexact(&mut self.arena, &arena_args),
            "inexact->exact" => arena_inexact_to_exact(&mut self.arena, &arena_args),
            "make-string" => arena_make_string(&mut self.arena, &arena_args),
            "string" => arena_string(&mut self.arena, &arena_args),
            "reverse!" => arena_reverse_bang(&mut self.arena, &arena_args),

            // Phase 3 Batch 14: Additional string and character operations (5)
            "string-set!" => arena_string_set(&mut self.arena, &arena_args),
            "string-copy" => arena_string_copy(&mut self.arena, &arena_args),
            "string-fill!" => arena_string_fill(&mut self.arena, &arena_args),
            "char-lower-case?" => arena_char_lower_case_p(&self.arena, &arena_args),
            "char-upper-case?" => arena_char_upper_case_p(&self.arena, &arena_args),

            // Phase 3 Batch 15: List utilities (5)
            "last" => arena_last(&self.arena, &arena_args),
            "last-pair" => arena_last_pair(&self.arena, &arena_args),
            "list-copy" => arena_list_copy(&mut self.arena, &arena_args),
            "append!" => arena_append_bang(&mut self.arena, &arena_args),
            "iota" => arena_iota(&mut self.arena, &arena_args),

            // Phase 3 Batch 16: Extended list operations (5)
            "take" => arena_take(&mut self.arena, &arena_args),
            "drop" => arena_drop(&self.arena, &arena_args),
            "split-at" => arena_split_at(&mut self.arena, &arena_args),
            "filter" => arena_filter(&self.arena, &arena_args),
            "remove" => arena_remove(&self.arena, &arena_args),

            // Phase 3 Batch 17: Additional numeric operations (6)
            "numerator" => arena_numerator(&mut self.arena, &arena_args),
            "denominator" => arena_denominator(&mut self.arena, &arena_args),
            "rationalize" => arena_rationalize(&self.arena, &arena_args),
            "angle" => arena_angle(&mut self.arena, &arena_args),
            "magnitude" => arena_magnitude(&mut self.arena, &arena_args),

            // Phase 3 Batch 18: Conversion and utility operations (4)
            "null-list?" => arena_null_list_p(&self.arena, &arena_args),
            "improper-list?" => arena_improper_list_p(&self.arena, &arena_args),
            "circular-list?" => arena_circular_list_p(&self.arena, &arena_args),

            // Phase 3 Batch 19: Bitwise operations (8)
            "bitwise-and" => arena_bitwise_and(&mut self.arena, &arena_args),
            "bitwise-ior" => arena_bitwise_ior(&mut self.arena, &arena_args),
            "bitwise-xor" => arena_bitwise_xor(&mut self.arena, &arena_args),
            "bitwise-not" => arena_bitwise_not(&mut self.arena, &arena_args),
            "arithmetic-shift" => arena_arithmetic_shift(&mut self.arena, &arena_args),
            "bit-extract" => arena_bit_extract(&mut self.arena, &arena_args),
            "bitwise-bit-set?" => arena_bitwise_bit_set_p(&self.arena, &arena_args),
            "bitwise-bit-count" => arena_bitwise_bit_count(&mut self.arena, &arena_args),

            // Phase 3 Batch 20: I/O and display operations (6)
            "display" => arena_display(&self.arena, &arena_args),
            "newline" => arena_newline(&self.arena, &arena_args),
            "write" => arena_write(&self.arena, &arena_args),
            "write-char" => arena_write_char(&self.arena, &arena_args),
            "read-char" => arena_read_char(&self.arena, &arena_args),
            "eof-object?" => arena_eof_object_p(&self.arena, &arena_args),

            // Phase 3 Batch 21: Number formatting (2)
            "format-number" => arena_format_number(&mut self.arena, &arena_args),
            "format-number-list" => arena_format_number_list(&mut self.arena, &arena_args),

            // Phase 3 Batch 22: Simple constants (1)
            "empty-sosofo" => arena_empty_sosofo(&mut self.arena, &arena_args),

            // Phase 3 Batch 23: Grove operations (1)
            "current-node" => arena_current_node(&mut self.arena, &arena_args),

            // Phase 3 Batch 24: Grove node properties (3)
            "gi" => arena_gi(&mut self.arena, &arena_args),
            "data" => arena_data(&mut self.arena, &arena_args),
            "id" => arena_id(&mut self.arena, &arena_args),

            // Phase 3 Batch 25: Grove navigation (3)
            "children" => arena_children(&mut self.arena, &arena_args),
            "parent" => arena_parent(&mut self.arena, &arena_args),
            "attributes" => arena_attributes(&mut self.arena, &arena_args),

            // Phase 3 Batch 26: Node-list operations (5)
            "node-list?" => arena_node_list_p(&self.arena, &arena_args),
            "empty-node-list" => arena_empty_node_list(&mut self.arena, &arena_args),
            "node-list-empty?" => arena_node_list_empty_p(&self.arena, &arena_args),
            "node-list-length" => arena_node_list_length(&mut self.arena, &arena_args),
            "node-list-first" => arena_node_list_first(&mut self.arena, &arena_args),

            // Phase 3 Batch 27: Attribute operations (1)
            "attribute-string" => arena_attribute_string(&mut self.arena, &arena_args),

            // Phase 3 Batch 28: More node-list operations (3)
            "node-list-rest" => arena_node_list_rest(&mut self.arena, &arena_args),
            "node-list-ref" => arena_node_list_ref(&mut self.arena, &arena_args),
            "node-list-reverse" => arena_node_list_reverse(&mut self.arena, &arena_args),

            // Phase 3 Batch 29: Type predicates (3)
            "node?" => arena_node_p(&self.arena, &arena_args),
            "sosofo?" => arena_sosofo_p(&self.arena, &arena_args),
            "quantity?" => arena_quantity_p(&self.arena, &arena_args),

            // Phase 3 Batch 30: Color and spacing stubs (4)
            "color?" => arena_color_p(&self.arena, &arena_args),
            "color" => arena_color(&mut self.arena, &arena_args),
            "display-space?" => arena_display_space_p(&self.arena, &arena_args),
            "inline-space?" => arena_inline_space_p(&self.arena, &arena_args),

            // Phase 3 Batch 31: Quantity operations (5)
            "quantity->number" => arena_quantity_to_number(&mut self.arena, &arena_args),
            "number->quantity" => arena_number_to_quantity(&mut self.arena, &arena_args),
            "quantity-convert" => arena_quantity_convert(&mut self.arena, &arena_args),
            "device-length" => arena_device_length(&mut self.arena, &arena_args),
            "label-distance" => arena_label_distance(&mut self.arena, &arena_args),

            // Phase 3 Batch 32: Grove navigation operations (5)
            "ancestor" => arena_ancestor(&mut self.arena, &arena_args),
            "descendants" => arena_descendants(&mut self.arena, &arena_args),
            "follow" => arena_follow(&mut self.arena, &arena_args),
            "preced" => arena_preced(&mut self.arena, &arena_args),
            "ipreced" => arena_ipreced(&mut self.arena, &arena_args),

            // Phase 3 Batch 33: Node-list set operations (5)
            "node-list-last" => arena_node_list_last(&mut self.arena, &arena_args),
            "node-list-union" => arena_node_list_union(&mut self.arena, &arena_args),
            "node-list-intersection" => arena_node_list_intersection(&mut self.arena, &arena_args),
            "node-list-difference" => arena_node_list_difference(&mut self.arena, &arena_args),
            "node-list-remove-duplicates" => arena_node_list_remove_duplicates(&mut self.arena, &arena_args),

            // Phase 3 Batch 34: Element selection and position operations (5)
            "select-elements" => arena_select_elements(&mut self.arena, &arena_args),
            "first-sibling?" => arena_first_sibling_p(&mut self.arena, &arena_args),
            "last-sibling?" => arena_last_sibling_p(&mut self.arena, &arena_args),
            "child-number" => arena_child_number(&mut self.arena, &arena_args),
            "element-with-id" => arena_element_with_id(&mut self.arena, &arena_args),

            // Phase 3 Batch 35: Element numbering operations (3)
            "element-number" => arena_element_number(&mut self.arena, &arena_args),
            "hierarchical-number" => arena_hierarchical_number(&mut self.arena, &arena_args),
            "hierarchical-number-recursive" => arena_hierarchical_number_recursive(&mut self.arena, &arena_args),

            // Phase 3 Batch 36: Grove utility operations (5)
            "ancestors" => arena_ancestors(&mut self.arena, &arena_args),
            "document-element" => arena_document_element(&mut self.arena, &arena_args),
            "have-ancestor?" => arena_have_ancestor_p(&mut self.arena, &arena_args),
            "match-element?" => arena_match_element_p(&mut self.arena, &arena_args),
            "node-list-map" => arena_node_list_map(&mut self.arena, &arena_args),

            // Phase 3 Batch 37: Final DSSSL operations (5)
            "node-property" => arena_node_property(&mut self.arena, &arena_args),
            "absolute-first-sibling?" => arena_absolute_first_sibling_p(&mut self.arena, &arena_args),
            "absolute-last-sibling?" => arena_absolute_last_sibling_p(&mut self.arena, &arena_args),
            "node-list->list" => arena_node_list_to_list(&mut self.arena, &arena_args),
            "node-list-contains?" => arena_node_list_contains_p(&mut self.arena, &arena_args),

            // Phase 3 Batch 38: Entity and notation operations (5)
            "entity-system-id" => arena_entity_system_id(&mut self.arena, &arena_args),
            "entity-public-id" => arena_entity_public_id(&mut self.arena, &arena_args),
            "entity-type" => arena_entity_type(&mut self.arena, &arena_args),
            "notation-system-id" => arena_notation_system_id(&mut self.arena, &arena_args),
            "notation-public-id" => arena_notation_public_id(&mut self.arena, &arena_args),

            // Phase 3 Batch 39: Context and debugging primitives (5)
            "current-language" => arena_current_language(&self.arena, &arena_args),
            "current-mode" => arena_current_mode(&self.arena, &arena_args),
            "current-node-address" => arena_current_node_address(&self.arena, &arena_args),
            "current-node-page-number-sosofo" => arena_current_node_page_number_sosofo(&self.arena, &arena_args),
            "debug" => arena_debug(&self.arena, &arena_args),

            // Phase 3 Batch 40: Unwired existing primitives (5)
            "add" => arena_add(&mut self.arena, &arena_args),
            "divide" => arena_divide(&mut self.arena, &arena_args),
            "equal" => arena_equal(&self.arena, &arena_args),
            "char-eq" => arena_char_eq(&self.arena, &arena_args),
            "char-lt" => arena_char_lt(&self.arena, &arena_args),
            // Phase 3 Batch 41: Type predicates and error (3)
            "exact?" => arena_exact_p(&self.arena, &arena_args),
            "inexact?" => arena_inexact_p(&self.arena, &arena_args),
            "error" => arena_error(&mut self.arena, &arena_args),
            // Phase 3 Batch 42: Address type stubs (3)
            "address?" => arena_address_p(&self.arena, &arena_args),
            "address-local?" => arena_address_local_p(&self.arena, &arena_args),
            "address-visited?" => arena_address_visited_p(&self.arena, &arena_args),
            // Phase 3 Batch 43: Color/display space stubs (4)
            "color-space?" => arena_color_space_p(&self.arena, &arena_args),
            "color-space" => arena_color_space(&self.arena, &arena_args),
            "display-space" => arena_display_space(&mut self.arena, &arena_args),
            "inline-space" => arena_inline_space(&mut self.arena, &arena_args),
            // Phase 3 Batch 44: Glyph type stubs (5)
            "glyph-id?" => arena_glyph_id_p(&self.arena, &arena_args),
            "glyph-id" => arena_glyph_id(&self.arena, &arena_args),
            "glyph-subst-table?" => arena_glyph_subst_table_p(&self.arena, &arena_args),
            "glyph-subst-table" => arena_glyph_subst_table(&self.arena, &arena_args),
            "glyph-subst" => arena_glyph_subst(&self.arena, &arena_args),
            // Phase 3 Batch 45: Time type stubs (6)
            "time" => arena_time(&self.arena, &arena_args),
            "time->string" => arena_time_to_string(&mut self.arena, &arena_args),
            "time<=?" => arena_time_le(&self.arena, &arena_args),
            "time<?" => arena_time_lt(&self.arena, &arena_args),
            "time>=?" => arena_time_ge(&self.arena, &arena_args),
            "time>?" => arena_time_gt(&self.arena, &arena_args),
            // Phase 3 Batch 46: Language and style type stubs (3)
            "language?" => arena_language_p(&self.arena, &arena_args),
            "language" => arena_language(&self.arena, &arena_args),
            "style?" => arena_style_p(&self.arena, &arena_args),
            // Phase 3 Batch 47: String comparison and simple stubs (3)
            "string-equiv?" => arena_string_equiv_p(&self.arena, &arena_args),
            "label-length" => arena_label_length(&mut self.arena, &arena_args),
            "external-procedure" => arena_external_procedure(&self.arena, &arena_args),
            // Phase 3 Batch 48: DTD/SGML stubs (6)
            "declaration" => arena_declaration(&self.arena, &arena_args),
            "dtd" => arena_dtd(&self.arena, &arena_args),
            "epilog" => arena_epilog(&self.arena, &arena_args),
            "prolog" => arena_prolog(&self.arena, &arena_args),
            "sgml-declaration" => arena_sgml_declaration(&self.arena, &arena_args),
            "sgml-parse" => arena_sgml_parse(&self.arena, &arena_args),
            // Phase 3 Batch 49: Entity/normalization stubs (4)
            "entity-address" => arena_entity_address(&self.arena, &arena_args),
            "entity-generated-system-id" => arena_entity_generated_system_id(&mut self.arena, &arena_args),
            "entity-name-normalize" => arena_entity_name_normalize(&mut self.arena, &arena_args),
            "general-name-normalize" => arena_general_name_normalize(&mut self.arena, &arena_args),
            // Phase 3 Batch 50: Simple navigation and declaration stubs (5)
            "first-child-gi" => arena_first_child_gi(&mut self.arena, &arena_args),
            "tree-root" => arena_tree_root(&mut self.arena, &arena_args),
            "declare-default-language" => arena_declare_default_language(&self.arena, &arena_args),
            "read-entity" => arena_read_entity(&mut self.arena, &arena_args),
            "set-visited!" => arena_set_visited(&self.arena, &arena_args),
            // Phase 3 Batch 51: Sosofo and navigation stubs (4)
            "sosofo-contains-node?" => arena_sosofo_contains_node_p(&self.arena, &arena_args),
            "page-number-sosofo" => arena_page_number_sosofo(&self.arena, &arena_args),
            "ifollow" => arena_ifollow(&self.arena, &arena_args),
            "with-language" => arena_with_language(&self.arena, &arena_args),
            // Phase 3 Batch 52: Element numbering stubs (3)
            "all-element-number" => arena_all_element_number(&mut self.arena, &arena_args),
            "ancestor-child-number" => arena_ancestor_child_number(&mut self.arena, &arena_args),
            "element-number-list" => arena_element_number_list(&self.arena, &arena_args),
            // Phase 3 Batch 53: Inherited property stubs - Part 1 (5)
            "inherited-attribute-string" => arena_inherited_attribute_string(&mut self.arena, &arena_args),
            "inherited-element-attribute-string" => arena_inherited_element_attribute_string(&mut self.arena, &arena_args),
            "inherited-start-indent" => arena_inherited_start_indent(&mut self.arena, &arena_args),
            "inherited-end-indent" => arena_inherited_end_indent(&mut self.arena, &arena_args),
            "inherited-line-spacing" => arena_inherited_line_spacing(&mut self.arena, &arena_args),
            // Phase 3 Batch 54: Inherited property stubs - Part 2 (6)
            "inherited-font-family-name" => arena_inherited_font_family_name(&mut self.arena, &arena_args),
            "inherited-font-size" => arena_inherited_font_size(&mut self.arena, &arena_args),
            "inherited-font-weight" => arena_inherited_font_weight(&mut self.arena, &arena_args),
            "inherited-font-posture" => arena_inherited_font_posture(&mut self.arena, &arena_args),
            "inherited-dbhtml-value" => arena_inherited_dbhtml_value(&mut self.arena, &arena_args),
            "inherited-pi-value" => arena_inherited_pi_value(&mut self.arena, &arena_args),
            // Phase 3 Batch 55: Node-list operation stubs - Part 1 (5)
            "node-list" => arena_node_list(&self.arena, &arena_args),
            "node-list=?" => arena_node_list_eq_p(&self.arena, &arena_args),
            // node-list-count is user-defined (delegates to node-list-length + node-list-remove-duplicates)
            "node-list-union-map" => arena_node_list_union_map(&self.arena, &arena_args),
            "node-list-symmetrical-difference" => arena_node_list_symmetrical_difference(&self.arena, &arena_args),
            // Phase 3 Batch 56: Node-list operation stubs - Part 2 (4)
            "node-list-address" => arena_node_list_address(&self.arena, &arena_args),
            "node-list-error" => arena_node_list_error(&mut self.arena, &arena_args),
            "node-list-no-order" => arena_node_list_no_order(&self.arena, &arena_args),
            "origin-to-subnode-rel-forest-addr" => arena_origin_to_subnode_rel_forest_addr(&self.arena, &arena_args),
            // Phase 3 Batch 57: Named node list stubs (3)
            "named-node" => arena_named_node(&self.arena, &arena_args),
            "named-node-list?" => arena_named_node_list_p(&self.arena, &arena_args),
            "named-node-list-names" => arena_named_node_list_names(&self.arena, &arena_args),
            // Phase 3 Batch 58: Selection operation stubs (1) - NOTE: select-children is user-defined
            "select-by-class" => arena_select_by_class(&self.arena, &arena_args),
            // Phase 3 Batch 59: Processing operation stubs (5) - FINAL BATCH!
            "process-children-trim" => arena_process_children_trim(&self.arena, &arena_args),
            "process-element-with-id" => arena_process_element_with_id(&self.arena, &arena_args),
            "process-first-descendant" => arena_process_first_descendant(&self.arena, &arena_args),
            "process-matching-children" => arena_process_matching_children(&self.arena, &arena_args),
            "next-match" => arena_next_match(&self.arena, &arena_args),
            _ => unreachable!("apply_arena_primitive called with non-arena primitive: {}", name),
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
                    // Phase 2+3: Route arena primitives for 6-7x speedup
                    match *name {
                        // Phase 2: Hot primitives (5)
                        "car" | "cdr" | "cons" | "null?" | "equal?" |
                        // Phase 3 Batch 1: List operations (9)
                        "cadr" | "caddr" | "cadddr" | "list" | "length" |
                        "reverse" | "append" | "list?" | "list-ref" |
                        // Phase 3 Batch 2: Type predicates (13)
                        "pair?" | "number?" | "integer?" | "real?" | "string?" |
                        "symbol?" | "char?" | "boolean?" | "zero?" | "positive?" |
                        "negative?" | "odd?" | "even?" |
                        // Phase 3 Batch 3: Arithmetic (29)
                        "+" | "-" | "*" | "/" | "quotient" | "remainder" | "modulo" |
                        "=" | "<" | ">" | "<=" | ">=" |
                        "abs" | "min" | "max" |
                        "floor" | "ceiling" | "truncate" | "round" |
                        "sqrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" |
                        "exp" | "log" | "expt" |
                        // Phase 3 Batch 4: String operations (14)
                        "string-length" | "string-ref" | "substring" | "string-append" |
                        "string=?" | "string<?" | "string>?" | "string<=?" | "string>=?" |
                        "string-ci=?" | "string-ci<?" | "string-ci>?" | "string-ci<=?" | "string-ci>=?" |
                        // Phase 3 Batch 5: Character operations (19)
                        "char=?" | "char<?" | "char>?" | "char<=?" | "char>=?" |
                        "char-ci=?" | "char-ci<?" | "char-ci>?" | "char-ci<=?" | "char-ci>=?" |
                        "char-upcase" | "char-downcase" |
                        "char-alphabetic?" | "char-numeric?" | "char-whitespace?" |
                        "char->integer" | "integer->char" |
                        "char-property" | "char-script-case" |
                        // Phase 3 Batch 6: Symbol/Keyword operations (5)
                        "symbol->string" | "string->symbol" |
                        "keyword?" | "keyword->string" | "string->keyword" |
                        // Phase 3 Batch 7: List utilities (6)
                        "memq" | "memv" | "member" |
                        "assq" | "assv" | "assoc" |
                        // Phase 3 Batch 8: Logic and list accessors (6)
                        "not" | "eq?" | "eqv?" |
                        "caar" | "cdar" | "cddr" |
                        // Phase 3 Batch 9: Extended list accessors (6)
                        "caaar" | "caadr" | "cadar" |
                        "cdaar" | "cdadr" | "cddar" | "cdddr" |
                        // Phase 3 Batch 10: Vector operations (8)
                        "vector" | "make-vector" | "vector-length" |
                        "vector-ref" | "vector-set!" |
                        "vector->list" | "list->vector" | "vector-fill!" |
                        // Phase 3 Batch 11: Additional type predicates & utilities (5)
                        "vector?" | "procedure?" |
                        "set-car!" | "set-cdr!" | "list-tail" |
                        // Phase 3 Batch 12: String utilities (6)
                        "string-upcase" | "string-downcase" |
                        "string->number" | "number->string" |
                        "string->list" | "list->string" |
                        // Phase 3 Batch 13: Miscellaneous utilities (7)
                        "gcd" | "lcm" |
                        "exact->inexact" | "inexact->exact" |
                        "make-string" | "string" | "reverse!" |
                        // Phase 3 Batch 14: Additional string and character operations (5)
                        "string-set!" | "string-copy" | "string-fill!" |
                        "char-lower-case?" | "char-upper-case?" |
                        // Phase 3 Batch 15: List utilities (5)
                        "last" | "last-pair" | "list-copy" |
                        "append!" | "iota" |
                        // Phase 3 Batch 16: Extended list operations (5)
                        "take" | "drop" | "split-at" |
                        "filter" | "remove" |
                        // Phase 3 Batch 17: Additional numeric operations (6)
                        "numerator" | "denominator" | "rationalize" |
                        "angle" | "magnitude" |
                        // Phase 3 Batch 18: Conversion and utility operations (4)
                        "null-list?" | "improper-list?" | "circular-list?" |
                        // Phase 3 Batch 19: Bitwise operations (8)
                        "bitwise-and" | "bitwise-ior" | "bitwise-xor" | "bitwise-not" |
                        "arithmetic-shift" | "bit-extract" |
                        "bitwise-bit-set?" | "bitwise-bit-count" |
                        // Phase 3 Batch 21: Number formatting (2)
                        "format-number" | "format-number-list" |
                        // Phase 3 Batch 22: Simple constants (1)
                        "empty-sosofo" |
                        // Phase 3 Batch 23: Grove operations (1)
                        "current-node" |
                        // Phase 3 Batch 24: Grove node properties (3)
                        "gi" | "data" | "id" |
                        // Phase 3 Batch 25: Grove navigation (3)
                        "children" | "parent" | "attributes" |
                        // Phase 3 Batch 26: Node-list operations (5)
                        "node-list?" | "empty-node-list" | "node-list-empty?" |
                        "node-list-length" | "node-list-first" |
                        // Phase 3 Batch 27: Attribute operations (1)
                        "attribute-string" |
                        // Phase 3 Batch 28: More node-list operations (3)
                        "node-list-rest" | "node-list-ref" | "node-list-reverse" |
                        // Phase 3 Batch 29: Type predicates (3)
                        "node?" | "sosofo?" | "quantity?" |
                        // Phase 3 Batch 30: Color and spacing stubs (4)
                        "color?" | "color" | "display-space?" | "inline-space?" |
                        // Phase 3 Batch 31: Quantity operations (5)
                        "quantity->number" | "number->quantity" | "quantity-convert" |
                        "device-length" | "label-distance" |
                        // Phase 3 Batch 32: Grove navigation operations (5)
                        "ancestor" | "descendants" | "follow" | "preced" | "ipreced" |
                        // Phase 3 Batch 33: Node-list set operations (5)
                        "node-list-last" | "node-list-union" | "node-list-intersection" |
                        "node-list-difference" | "node-list-remove-duplicates" |
                        // Phase 3 Batch 34: Element selection and position operations (5)
                        "select-elements" | "first-sibling?" | "last-sibling?" |
                        "child-number" | "element-with-id" |
                        // Phase 3 Batch 35: Element numbering operations (3)
                        "element-number" | "hierarchical-number" | "hierarchical-number-recursive" |
                        // Phase 3 Batch 36: Grove utility operations (5)
                        "ancestors" | "document-element" | "have-ancestor?" |
                        "match-element?" | "node-list-map" |
                        // Phase 3 Batch 37: Final DSSSL operations (5)
                        "node-property" | "absolute-first-sibling?" | "absolute-last-sibling?" |
                        "node-list->list" | "node-list-contains?" |
                        // Phase 3 Batch 38: Entity and notation operations (5)
                        "entity-system-id" | "entity-public-id" | "entity-type" |
                        "notation-system-id" | "notation-public-id" |
                        // Phase 3 Batch 39: Context and debugging primitives (5)
                        "current-language" | "current-mode" | "current-node-address" |
                        "current-node-page-number-sosofo" | "debug" |
                        // Phase 3 Batch 40: Unwired existing primitives (5)
                        "add" | "divide" | "equal" | "char-eq" | "char-lt" |
                        // Phase 3 Batch 41: Type predicates and error (3)
                        "exact?" | "inexact?" | "error" |
                        // Phase 3 Batch 42: Address type stubs (3)
                        "address?" | "address-local?" | "address-visited?" |
                        // Phase 3 Batch 43: Color/display space stubs (4)
                        "color-space?" | "color-space" | "display-space" | "inline-space" |
                        // Phase 3 Batch 44: Glyph type stubs (5)
                        "glyph-id?" | "glyph-id" | "glyph-subst-table?" | "glyph-subst-table" | "glyph-subst" |
                        // Phase 3 Batch 45: Time type stubs (6)
                        "time" | "time->string" | "time<=?" | "time<?" | "time>=?" | "time>?" |
                        // Phase 3 Batch 46: Language and style type stubs (3)
                        "language?" | "language" | "style?" |
                        // Phase 3 Batch 47: String comparison and simple stubs (3)
                        "string-equiv?" | "label-length" | "external-procedure" |
                        // Phase 3 Batch 48: DTD/SGML stubs (6)
                        "declaration" | "dtd" | "epilog" | "prolog" | "sgml-declaration" | "sgml-parse" |
                        // Phase 3 Batch 49: Entity/normalization stubs (4)
                        "entity-address" | "entity-generated-system-id" | "entity-name-normalize" | "general-name-normalize" |
                        // Phase 3 Batch 50: Simple navigation and declaration stubs (5)
                        "first-child-gi" | "tree-root" | "declare-default-language" | "read-entity" | "set-visited!" |
                        // Phase 3 Batch 51: Sosofo and navigation stubs (4)
                        "sosofo-contains-node?" | "page-number-sosofo" | "ifollow" | "with-language" |
                        // Phase 3 Batch 52: Element numbering stubs (3)
                        "all-element-number" | "ancestor-child-number" | "element-number-list" |
                        // Phase 3 Batch 53: Inherited property stubs - Part 1 (5)
                        "inherited-attribute-string" | "inherited-element-attribute-string" |
                        "inherited-start-indent" | "inherited-end-indent" | "inherited-line-spacing" |
                        // Phase 3 Batch 54: Inherited property stubs - Part 2 (6)
                        "inherited-font-family-name" | "inherited-font-size" | "inherited-font-weight" |
                        "inherited-font-posture" | "inherited-dbhtml-value" | "inherited-pi-value" |
                        // Phase 3 Batch 55: Node-list operation stubs - Part 1 (4, node-list-count is user-defined)
                        "node-list" | "node-list=?" | "node-list-union-map" |
                        "node-list-symmetrical-difference" |
                        // Phase 3 Batch 56: Node-list operation stubs - Part 2 (4)
                        "node-list-address" | "node-list-error" | "node-list-no-order" |
                        "origin-to-subnode-rel-forest-addr" |
                        // Phase 3 Batch 57: Named node list stubs (3)
                        "named-node" | "named-node-list?" | "named-node-list-names" |
                        // Phase 3 Batch 58: Selection operation stubs (1) - NOTE: select-children is user-defined, not a primitive
                        "select-by-class" |
                        // Phase 3 Batch 59: Processing operation stubs (5) - FINAL BATCH!
                        "process-children-trim" | "process-element-with-id" | "process-first-descendant" |
                        "process-matching-children" | "next-match" => {
                            self.apply_arena_primitive(name, &args)
                        }
                        // Note: I/O operations (display, write, newline, etc.) stay in non-arena path
                        // because they need actual side effects (stdout/stdin interaction)
                        _ => {
                            // Don't push call frames for primitives - only for user lambdas
                            // This matches OpenJade's behavior
                            func(&args).map_err(|e| self.error_with_stack(e))
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
