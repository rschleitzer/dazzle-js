//! Stack-based virtual machine for executing bytecode instructions
//!
//! This module implements OpenJade's VM execution model:
//!
//! ```cpp
//! ELObj *VM::eval(const Insn *insn, ELObj **display, ELObj *arg) {
//!     initStack();
//!     // The inner loop.
//!     while (insn)
//!         insn = insn->execute(*this);  // Just a virtual call!
//!     return *sp;
//! }
//! ```
//!
//! Key advantages over tree-walking interpreter:
//! - No recursion - simple while loop
//! - Stack-based - minimal GC pressure
//! - Direct dispatch - no pattern matching overhead
//! - Cache-friendly - tight instruction loop

use crate::scheme::arena::{Arena, ProcedureData, ValueData, ValueId, NIL_ID, TRUE_ID, FALSE_ID};
use crate::scheme::instruction::Instruction;
use std::collections::HashMap;

/// Call frame for returns
#[derive(Debug, Clone)]
struct Frame {
    /// Return instruction pointer
    return_ip: usize,
    /// Frame pointer (stack base for this call)
    frame_pointer: usize,
    /// Environment display (lexical closure)
    display: Vec<Vec<ValueId>>,
}

/// Virtual machine state
pub struct VM<'a> {
    /// Value stack
    stack: Vec<ValueId>,
    /// Call frames
    frames: Vec<Frame>,
    /// Arena reference
    arena: &'a mut Arena,
    /// Global environment (name -> ValueId)
    globals: HashMap<String, ValueId>,
    /// Names of primitive functions (to distinguish from user-defined globals)
    primitive_names: std::collections::HashSet<String>,
}

impl<'a> VM<'a> {
    /// Create a new VM
    pub fn new(arena: &'a mut Arena) -> Self {
        VM {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(256),
            arena,
            globals: HashMap::new(),
            primitive_names: std::collections::HashSet::new(),
        }
    }

    /// Create a VM with all primitives registered
    pub fn with_primitives(arena: &'a mut Arena) -> Self {
        let mut vm = Self::new(arena);
        vm.register_primitives();
        vm
    }

    /// Register all arena-based primitives in the global environment
    pub fn register_primitives(&mut self) {
        use crate::scheme::primitives;

        // ===== R4RS List Operations =====
        self.register_primitive_immut("car", primitives::car);
        self.register_primitive_immut("cdr", primitives::cdr);
        self.register_primitive("cons", primitives::cons);
        self.register_primitive_immut("null?", primitives::null);
        self.register_primitive("list", primitives::list);
        self.register_primitive("length", primitives::length);
        self.register_primitive("reverse", primitives::reverse);
        self.register_primitive("append", primitives::append);
        self.register_primitive_immut("list?", primitives::list_p);
        self.register_primitive_immut("pair?", primitives::pair_p);
        self.register_primitive_immut("list-ref", primitives::list_ref);
        self.register_primitive_immut("list-tail", primitives::list_tail);

        // Composite list accessors
        self.register_primitive_immut("cadr", primitives::cadr);
        self.register_primitive_immut("caddr", primitives::caddr);
        self.register_primitive_immut("cadddr", primitives::cadddr);
        self.register_primitive_immut("caar", primitives::caar);
        self.register_primitive_immut("cdar", primitives::cdar);
        self.register_primitive_immut("cddr", primitives::cddr);
        self.register_primitive_immut("caaar", primitives::caaar);
        self.register_primitive_immut("caadr", primitives::caadr);
        self.register_primitive_immut("cadar", primitives::cadar);
        self.register_primitive_immut("cdaar", primitives::cdaar);
        self.register_primitive_immut("cdadr", primitives::cdadr);
        self.register_primitive_immut("cddar", primitives::cddar);
        self.register_primitive_immut("cdddr", primitives::cdddr);

        // List utilities
        self.register_primitive_immut("memq", primitives::memq);
        self.register_primitive_immut("memv", primitives::memv);
        self.register_primitive_immut("member", primitives::member);
        self.register_primitive_immut("assq", primitives::assq);
        self.register_primitive_immut("assv", primitives::assv);
        self.register_primitive_immut("assoc", primitives::assoc);
        self.register_primitive_immut("last", primitives::last);
        self.register_primitive_immut("last-pair", primitives::last_pair);

        // SRFI-1 list operations
        self.register_primitive("list-copy", primitives::list_copy);
        self.register_primitive("append!", primitives::append_bang);
        self.register_primitive("reverse!", primitives::reverse_bang);
        self.register_primitive("iota", primitives::iota);
        self.register_primitive("take", primitives::take);
        self.register_primitive_immut("drop", primitives::drop);
        self.register_primitive("split-at", primitives::split_at);
        self.register_primitive_immut("filter", primitives::filter);
        self.register_primitive_immut("remove", primitives::remove);
        self.register_primitive_immut("list?", primitives::list_p_improved);
        self.register_primitive_immut("null-list?", primitives::null_list_p);
        self.register_primitive_immut("improper-list?", primitives::improper_list_p);
        self.register_primitive_immut("circular-list?", primitives::circular_list_p);

        // Pair mutations
        self.register_primitive("set-car!", primitives::set_car);
        self.register_primitive("set-cdr!", primitives::set_cdr);

        // ===== R4RS Arithmetic =====
        self.register_primitive("+", primitives::add);
        self.register_primitive("-", primitives::subtract);
        self.register_primitive("*", primitives::multiply);
        self.register_primitive("/", primitives::divide);
        self.register_primitive("quotient", primitives::quotient);
        self.register_primitive("remainder", primitives::remainder);
        self.register_primitive("modulo", primitives::modulo);
        self.register_primitive("abs", primitives::abs);
        self.register_primitive("min", primitives::min);
        self.register_primitive("max", primitives::max);
        self.register_primitive("gcd", primitives::gcd);
        self.register_primitive("lcm", primitives::lcm);

        // Numeric comparisons
        self.register_primitive_immut("=", primitives::num_eq);
        self.register_primitive_immut("<", primitives::num_lt);
        self.register_primitive_immut(">", primitives::num_gt);
        self.register_primitive_immut("<=", primitives::num_le);
        self.register_primitive_immut(">=", primitives::num_ge);

        // Numeric predicates
        self.register_primitive_immut("zero?", primitives::zero_p);
        self.register_primitive_immut("positive?", primitives::positive_p);
        self.register_primitive_immut("negative?", primitives::negative_p);
        self.register_primitive_immut("odd?", primitives::odd_p);
        self.register_primitive_immut("even?", primitives::even_p);
        self.register_primitive_immut("exact?", primitives::exact_p);
        self.register_primitive_immut("inexact?", primitives::inexact_p);

        // Math functions
        self.register_primitive("floor", primitives::floor);
        self.register_primitive("ceiling", primitives::ceiling);
        self.register_primitive("truncate", primitives::truncate);
        self.register_primitive("round", primitives::round);
        self.register_primitive("sqrt", primitives::sqrt);
        self.register_primitive("sin", primitives::sin);
        self.register_primitive("cos", primitives::cos);
        self.register_primitive("tan", primitives::tan);
        self.register_primitive("asin", primitives::asin);
        self.register_primitive("acos", primitives::acos);
        self.register_primitive("atan", primitives::atan);
        self.register_primitive("exp", primitives::exp);
        self.register_primitive("log", primitives::log);
        self.register_primitive("expt", primitives::expt);

        // Numeric conversions
        self.register_primitive("exact->inexact", primitives::exact_to_inexact);
        self.register_primitive("inexact->exact", primitives::inexact_to_exact);
        self.register_primitive("numerator", primitives::numerator);
        self.register_primitive("denominator", primitives::denominator);
        self.register_primitive_immut("rationalize", primitives::rationalize);
        self.register_primitive("angle", primitives::angle);
        self.register_primitive("magnitude", primitives::magnitude);

        // ===== R4RS Type Predicates =====
        self.register_primitive_immut("number?", primitives::number_p);
        self.register_primitive_immut("integer?", primitives::integer_p);
        self.register_primitive_immut("real?", primitives::real_p);
        self.register_primitive_immut("string?", primitives::string_p);
        self.register_primitive_immut("symbol?", primitives::symbol_p);
        self.register_primitive_immut("char?", primitives::char_p);
        self.register_primitive_immut("boolean?", primitives::boolean_p);
        self.register_primitive_immut("vector?", primitives::vector_p);
        self.register_primitive_immut("procedure?", primitives::procedure_p);

        // ===== R4RS Comparison =====
        self.register_primitive_immut("equal?", primitives::equal);
        self.register_primitive_immut("eqv?", primitives::eqv_p);
        self.register_primitive_immut("eq?", primitives::eq_p);
        self.register_primitive_immut("not", primitives::not);

        // ===== R4RS Strings =====
        self.register_primitive("string-length", primitives::string_length);
        self.register_primitive("string-ref", primitives::string_ref);
        self.register_primitive("substring", primitives::substring);
        self.register_primitive("string-append", primitives::string_append);
        self.register_primitive_immut("string=?", primitives::string_eq);
        self.register_primitive_immut("string<?", primitives::string_lt);
        self.register_primitive_immut("string>?", primitives::string_gt);
        self.register_primitive_immut("string<=?", primitives::string_le);
        self.register_primitive_immut("string>=?", primitives::string_ge);
        self.register_primitive_immut("string-ci=?", primitives::string_ci_eq);
        self.register_primitive_immut("string-ci<?", primitives::string_ci_lt);
        self.register_primitive_immut("string-ci>?", primitives::string_ci_gt);
        self.register_primitive_immut("string-ci<=?", primitives::string_ci_le);
        self.register_primitive_immut("string-ci>=?", primitives::string_ci_ge);
        self.register_primitive("string-upcase", primitives::string_upcase);
        self.register_primitive("string-downcase", primitives::string_downcase);
        self.register_primitive("case-fold-down", primitives::case_fold_down);
        self.register_primitive("string-index", primitives::string_index);
        self.register_primitive("make-string", primitives::make_string);
        self.register_primitive("string", primitives::string);
        self.register_primitive("string-set!", primitives::string_set);
        self.register_primitive("string-copy", primitives::string_copy);
        self.register_primitive("string-fill!", primitives::string_fill);

        // String conversions
        self.register_primitive("string->number", primitives::string_to_number);
        self.register_primitive("number->string", primitives::number_to_string);
        self.register_primitive("string->list", primitives::string_to_list);
        self.register_primitive("list->string", primitives::list_to_string);
        self.register_primitive("string->number", primitives::string_to_number_radix);
        self.register_primitive("number->string", primitives::number_to_string_radix);

        // ===== R4RS Characters =====
        self.register_primitive_immut("char=?", primitives::char_eq);
        self.register_primitive_immut("char<?", primitives::char_lt);
        self.register_primitive_immut("char>?", primitives::char_gt);
        self.register_primitive_immut("char<=?", primitives::char_le);
        self.register_primitive_immut("char>=?", primitives::char_ge);
        self.register_primitive_immut("char-ci=?", primitives::char_ci_eq);
        self.register_primitive_immut("char-ci<?", primitives::char_ci_lt);
        self.register_primitive_immut("char-ci>?", primitives::char_ci_gt);
        self.register_primitive_immut("char-ci<=?", primitives::char_ci_le);
        self.register_primitive_immut("char-ci>=?", primitives::char_ci_ge);
        self.register_primitive("char-upcase", primitives::char_upcase);
        self.register_primitive("char-downcase", primitives::char_downcase);
        self.register_primitive_immut("char-alphabetic?", primitives::char_alphabetic_p);
        self.register_primitive_immut("char-numeric?", primitives::char_numeric_p);
        self.register_primitive_immut("char-whitespace?", primitives::char_whitespace_p);
        self.register_primitive_immut("char-lower-case?", primitives::char_lower_case_p);
        self.register_primitive_immut("char-upper-case?", primitives::char_upper_case_p);
        self.register_primitive("char->integer", primitives::char_to_integer);
        self.register_primitive("integer->char", primitives::integer_to_char);
        self.register_primitive("char-property", primitives::char_property);
        self.register_primitive("char-script-case", primitives::char_script_case);

        // ===== R4RS Symbols & Keywords =====
        self.register_primitive("symbol->string", primitives::symbol_to_string);
        self.register_primitive("string->symbol", primitives::string_to_symbol);
        self.register_primitive_immut("keyword?", primitives::keyword_p);
        self.register_primitive("keyword->string", primitives::keyword_to_string);
        self.register_primitive("string->keyword", primitives::string_to_keyword);

        // ===== R4RS Vectors =====
        self.register_primitive("vector", primitives::vector);
        self.register_primitive("make-vector", primitives::make_vector);
        self.register_primitive("vector-length", primitives::vector_length);
        self.register_primitive_immut("vector-ref", primitives::vector_ref);
        self.register_primitive("vector-set!", primitives::vector_set);
        self.register_primitive("vector->list", primitives::vector_to_list);
        self.register_primitive("list->vector", primitives::list_to_vector);
        self.register_primitive("vector-fill!", primitives::vector_fill);

        // ===== Bitwise Operations =====
        self.register_primitive("bitwise-and", primitives::bitwise_and);
        self.register_primitive("bitwise-ior", primitives::bitwise_ior);
        self.register_primitive("bitwise-xor", primitives::bitwise_xor);
        self.register_primitive("bitwise-not", primitives::bitwise_not);
        self.register_primitive("arithmetic-shift", primitives::arithmetic_shift);
        self.register_primitive("bit-extract", primitives::bit_extract);
        self.register_primitive_immut("bitwise-bit-set?", primitives::bitwise_bit_set_p);
        self.register_primitive("bitwise-bit-count", primitives::bitwise_bit_count);

        // ===== I/O Operations =====
        self.register_primitive_immut("display", primitives::display);
        self.register_primitive_immut("newline", primitives::newline);
        self.register_primitive_immut("write", primitives::write);
        self.register_primitive_immut("write-char", primitives::write_char);
        self.register_primitive_immut("read-char", primitives::read_char);
        self.register_primitive_immut("eof-object?", primitives::eof_object_p);

        // ===== DSSSL Formatting =====
        self.register_primitive("format-number", primitives::format_number);
        self.register_primitive("format-number-list", primitives::format_number_list);

        // ===== DSSSL Sosofo Operations =====
        self.register_primitive("empty-sosofo", primitives::empty_sosofo);
        self.register_primitive("sosofo-append", primitives::sosofo_append);
        self.register_primitive("if-first-page", primitives::if_first_page);
        self.register_primitive("if-front-page", primitives::if_front_page);
        self.register_primitive_immut("sosofo?", primitives::sosofo_p);

        // ===== DSSSL Grove Operations =====
        self.register_primitive("current-node", primitives::current_node);
        self.register_primitive("gi", primitives::gi);
        self.register_primitive("data", primitives::data);
        self.register_primitive("id", primitives::id);
        self.register_primitive("children", primitives::children);
        self.register_primitive("parent", primitives::parent);
        self.register_primitive("attributes", primitives::attributes);
        self.register_primitive("ancestor", primitives::ancestor);
        self.register_primitive("ancestors", primitives::ancestors);
        self.register_primitive("descendants", primitives::descendants);
        self.register_primitive("follow", primitives::follow);
        self.register_primitive("preced", primitives::preced);
        self.register_primitive("ipreced", primitives::ipreced);
        self.register_primitive("document-element", primitives::document_element);

        // ===== DSSSL Node List Operations =====
        self.register_primitive_immut("node-list?", primitives::node_list_p);
        self.register_primitive("empty-node-list", primitives::empty_node_list);
        self.register_primitive_immut("node-list-empty?", primitives::node_list_empty_p);
        self.register_primitive("node-list-length", primitives::node_list_length);
        self.register_primitive("node-list-first", primitives::node_list_first);
        self.register_primitive("node-list-rest", primitives::node_list_rest);
        self.register_primitive("node-list-ref", primitives::node_list_ref);
        self.register_primitive("node-list-last", primitives::node_list_last);
        self.register_primitive("node-list-reverse", primitives::node_list_reverse);
        self.register_primitive("node-list-union", primitives::node_list_union);
        self.register_primitive("node-list-intersection", primitives::node_list_intersection);
        self.register_primitive("node-list-difference", primitives::node_list_difference);
        self.register_primitive("node-list-remove-duplicates", primitives::node_list_remove_duplicates);
        self.register_primitive("node-list->list", primitives::node_list_to_list);
        self.register_primitive("node-list-contains?", primitives::node_list_contains_p);
        self.register_primitive("node-list-map", primitives::node_list_map);

        // ===== DSSSL Node Predicates =====
        self.register_primitive_immut("node?", primitives::node_p);
        self.register_primitive("first-sibling?", primitives::first_sibling_p);
        self.register_primitive("last-sibling?", primitives::last_sibling_p);
        self.register_primitive("absolute-first-sibling?", primitives::absolute_first_sibling_p);
        self.register_primitive("absolute-last-sibling?", primitives::absolute_last_sibling_p);
        self.register_primitive("have-ancestor?", primitives::have_ancestor_p);
        self.register_primitive("match-element?", primitives::match_element_p);

        // ===== DSSSL Node Queries =====
        self.register_primitive("attribute-string", primitives::attribute_string);
        self.register_primitive("node-property", primitives::node_property);
        self.register_primitive("select-elements", primitives::select_elements);
        self.register_primitive("element-with-id", primitives::element_with_id);
        self.register_primitive("child-number", primitives::child_number);
        self.register_primitive("element-number", primitives::element_number);
        self.register_primitive("hierarchical-number", primitives::hierarchical_number);
        self.register_primitive("hierarchical-number-recursive", primitives::hierarchical_number_recursive);

        // ===== DSSSL Entity & Notation Operations =====
        self.register_primitive("entity-system-id", primitives::entity_system_id);
        self.register_primitive("entity-public-id", primitives::entity_public_id);
        self.register_primitive("entity-type", primitives::entity_type);
        self.register_primitive("notation-system-id", primitives::notation_system_id);
        self.register_primitive("notation-public-id", primitives::notation_public_id);

        // ===== DSSSL Quantity & Color Operations =====
        self.register_primitive_immut("quantity?", primitives::quantity_p);
        self.register_primitive_immut("color?", primitives::color_p);
        self.register_primitive("color", primitives::color);
        self.register_primitive_immut("display-space?", primitives::display_space_p);
        self.register_primitive_immut("inline-space?", primitives::inline_space_p);
        self.register_primitive("quantity->number", primitives::quantity_to_number);
        self.register_primitive("number->quantity", primitives::number_to_quantity);
        self.register_primitive("quantity-convert", primitives::quantity_convert);
        self.register_primitive("device-length", primitives::device_length);
        self.register_primitive("label-distance", primitives::label_distance);
        self.register_primitive_immut("address?", primitives::address_p);
        self.register_primitive_immut("address-local?", primitives::address_local_p);
        self.register_primitive_immut("address-visited?", primitives::address_visited_p);
        self.register_primitive_immut("color-space?", primitives::color_space_p);
        self.register_primitive_immut("color-space", primitives::color_space);
        self.register_primitive("display-space", primitives::display_space);
        self.register_primitive("inline-space", primitives::inline_space);
        self.register_primitive_immut("glyph-id?", primitives::glyph_id_p);
        self.register_primitive_immut("glyph-id", primitives::glyph_id);

        // ===== DSSSL Utilities =====
        self.register_primitive_immut("current-language", primitives::current_language);
        self.register_primitive_immut("current-mode", primitives::current_mode);
        self.register_primitive_immut("current-node-address", primitives::current_node_address);
        self.register_primitive("current-node-page-number-sosofo", primitives::current_node_page_number_sosofo);
        self.register_primitive_immut("debug", primitives::debug);
        self.register_primitive("error", primitives::error);
    }

    /// Register a single primitive function (mutable arena)
    fn register_primitive(
        &mut self,
        name: &'static str,
        func: fn(&mut Arena, &[ValueId]) -> Result<ValueId, String>,
    ) {
        let proc = ProcedureData::Primitive { name, func };
        let proc_id = self.arena.alloc(ValueData::Procedure(proc));
        self.globals.insert(name.to_string(), proc_id);
        self.primitive_names.insert(name.to_string());
    }

    /// Register a primitive function that only needs immutable arena access
    /// Creates a wrapper that adapts the signature
    fn register_primitive_immut(
        &mut self,
        name: &'static str,
        immut_func: fn(&Arena, &[ValueId]) -> Result<ValueId, String>,
    ) {
        // We need to create a wrapper function because we can't use closures in function pointers
        // The trick is that &mut Arena can be coerced to &Arena, so this is safe
        // We use unsafe transmute to convert the function pointer type
        let wrapper: fn(&mut Arena, &[ValueId]) -> Result<ValueId, String> = unsafe {
            std::mem::transmute(immut_func)
        };

        let proc = ProcedureData::Primitive { name, func: wrapper };
        let proc_id = self.arena.alloc(ValueData::Procedure(proc));
        self.globals.insert(name.to_string(), proc_id);
        self.primitive_names.insert(name.to_string());
    }

    /// Set a global variable
    pub fn define_global(&mut self, name: String, value_id: ValueId) {
        self.globals.insert(name, value_id);
    }

    /// Look up a global variable
    pub fn lookup_global(&self, name: &str) -> Option<ValueId> {
        self.globals.get(name).copied()
    }

    /// Get a clone of all global variables
    pub fn get_globals(&self) -> HashMap<String, ValueId> {
        self.globals.clone()
    }

    /// Set all global variables (replaces existing globals)
    pub fn set_globals(&mut self, globals: HashMap<String, ValueId>) {
        self.globals = globals;
    }

    /// Extend global variables with additional bindings (merges, doesn't replace)
    pub fn extend_globals(&mut self, additional: HashMap<String, ValueId>) {
        for (name, value_id) in additional {
            self.globals.insert(name, value_id);
        }
    }

    /// Get only user-defined global variables (excluding primitives)
    pub fn get_user_globals(&self) -> HashMap<String, ValueId> {
        self.globals
            .iter()
            .filter(|(name, _)| !self.primitive_names.contains(*name))
            .map(|(name, value_id)| (name.clone(), *value_id))
            .collect()
    }

    /// Execute bytecode instructions starting at the given instruction pointer
    pub fn run(&mut self, instructions: &[Instruction], start_ip: usize) -> Result<ValueId, String> {
        let mut ip = start_ip;

        // Main execution loop - replaces recursive eval()
        loop {
            if ip >= instructions.len() {
                return Err(format!("Instruction pointer {} out of bounds", ip));
            }

            match &instructions[ip] {
                Instruction::Constant { value_id } => {
                    self.stack.push(*value_id);
                    ip += 1;
                }

                Instruction::Variable { depth, offset } => {
                    // Look up in lexical environment (display)
                    if let Some(frame) = self.frames.last() {
                        if *depth < frame.display.len() {
                            if *offset < frame.display[*depth].len() {
                                let value_id = frame.display[*depth][*offset];
                                self.stack.push(value_id);
                                ip += 1;
                                continue;
                            }
                        }
                    }
                    return Err(format!("Variable not found at depth {} offset {}", depth, offset));
                }

                Instruction::GlobalVariable { name } => {
                    if let Some(value_id) = self.lookup_global(name) {
                        self.stack.push(value_id);
                        ip += 1;
                    } else {
                        return Err(format!("Undefined variable: {} (globals: {:?})", name, self.globals.keys().collect::<Vec<_>>()));
                    }
                }

                Instruction::Apply { n_args } => {
                    // Pop procedure and arguments from stack
                    if self.stack.len() < n_args + 1 {
                        return Err("Stack underflow in Apply".to_string());
                    }

                    // Pop procedure first (it's at the top of the stack)
                    let proc_id = self.stack.pop().unwrap();

                    // Pop arguments (they're now at the top, in reverse order)
                    let args_start = self.stack.len() - n_args;
                    let args: Vec<ValueId> = self.stack.drain(args_start..).collect();

                    // Get procedure
                    let proc_data = self.arena.get(proc_id);

                    match proc_data {
                        ValueData::Procedure(ProcedureData::Primitive { name: _, func }) => {
                            // Call primitive
                            let result = func(self.arena, &args)?;
                            self.stack.push(result);
                            ip += 1;
                        }
                        ValueData::Procedure(ProcedureData::CompiledLambda {
                            params,
                            required_count,
                            optional_defaults,
                            body_ip,
                            env,
                            ..
                        }) => {
                            // Check argument count
                            if args.len() < *required_count {
                                return Err(format!(
                                    "Lambda: expected at least {} arguments, got {}",
                                    required_count,
                                    args.len()
                                ));
                            }

                            // Build display for lambda call
                            let mut display = Vec::new();

                            // Current frame: parameters (depth 0)
                            let mut frame_values = args;

                            // Add optional parameter defaults if needed
                            for i in frame_values.len()..params.len() {
                                if i - required_count < optional_defaults.len() {
                                    frame_values.push(optional_defaults[i - required_count]);
                                } else {
                                    frame_values.push(NIL_ID);
                                }
                            }

                            display.push(frame_values);

                            // Reconstruct parent environment from captured free variables (depth 1)
                            if *env != NIL_ID {
                                let env_data = self.arena.get(*env);
                                match env_data {
                                    ValueData::Vector(free_vars) => {
                                        // Captured environment becomes depth 1
                                        display.push(free_vars.clone());
                                    }
                                    _ => {}
                                }
                            }

                            // Push call frame
                            self.frames.push(Frame {
                                return_ip: ip + 1,
                                frame_pointer: self.stack.len(),
                                display,
                            });

                            // Jump to lambda body
                            ip = *body_ip;
                        }
                        ValueData::Procedure(ProcedureData::Lambda { .. }) => {
                            return Err("Interpreted lambda not supported in VM (must be compiled first)".to_string());
                        }
                        _ => return Err("Apply: not a procedure".to_string()),
                    }
                }

                Instruction::Test { else_ip } => {
                    let test_id = self.stack.pop().ok_or("Stack underflow in Test")?;
                    let test_data = self.arena.get(test_id);

                    // #f is false, everything else is true
                    if matches!(test_data, ValueData::Bool(false)) {
                        ip = *else_ip;
                    } else {
                        ip += 1;
                    }
                }

                Instruction::Jump { target_ip } => {
                    ip = *target_ip;
                }

                Instruction::MakeClosure {
                    params,
                    required_count,
                    body_ip,
                    n_free,
                } => {
                    // Pop free variables from stack and store in closure's environment
                    let mut free_vars = Vec::new();
                    for _ in 0..*n_free {
                        if let Some(var_id) = self.stack.pop() {
                            free_vars.push(var_id);
                        } else {
                            return Err("Stack underflow in MakeClosure".to_string());
                        }
                    }
                    // Reverse because we popped in reverse order
                    free_vars.reverse();

                    // Store captured environment as a vector
                    let env_id = if free_vars.is_empty() {
                        NIL_ID
                    } else {
                        self.arena.vector(free_vars)
                    };

                    let lambda = ProcedureData::CompiledLambda {
                        params: params.clone(),
                        required_count: *required_count,
                        optional_defaults: vec![],
                        body_ip: *body_ip,
                        env: env_id,
                        source: None,
                        name: None,
                    };

                    let closure_id = self.arena.alloc(ValueData::Procedure(lambda));
                    self.stack.push(closure_id);
                    ip += 1;
                }

                Instruction::Return => {
                    // Pop return value
                    let result_id = self.stack.pop().ok_or("Stack underflow in Return")?;

                    // Pop call frame
                    if let Some(frame) = self.frames.pop() {
                        // Restore stack
                        self.stack.truncate(frame.frame_pointer);
                        self.stack.push(result_id);
                        ip = frame.return_ip;
                    } else {
                        // Top-level return
                        return Ok(result_id);
                    }
                }

                // Inline primitive operations
                Instruction::Cons => {
                    let cdr_id = self.stack.pop().ok_or("Stack underflow in Cons")?;
                    let car_id = self.stack.pop().ok_or("Stack underflow in Cons")?;
                    let pair_id = self.arena.cons(car_id, cdr_id);
                    self.stack.push(pair_id);
                    ip += 1;
                }

                Instruction::Car => {
                    let pair_id = self.stack.pop().ok_or("Stack underflow in Car")?;
                    let pair_data = self.arena.get(pair_id);
                    match pair_data {
                        ValueData::Pair { car, .. } => {
                            self.stack.push(*car);
                            ip += 1;
                        }
                        _ => return Err("car: not a pair".to_string()),
                    }
                }

                Instruction::Cdr => {
                    let pair_id = self.stack.pop().ok_or("Stack underflow in Cdr")?;
                    let pair_data = self.arena.get(pair_id);
                    match pair_data {
                        ValueData::Pair { cdr, .. } => {
                            self.stack.push(*cdr);
                            ip += 1;
                        }
                        _ => return Err("cdr: not a pair".to_string()),
                    }
                }

                Instruction::IsNull => {
                    let value_id = self.stack.pop().ok_or("Stack underflow in IsNull")?;
                    let value_data = self.arena.get(value_id);
                    let result = matches!(value_data, ValueData::Nil);
                    self.stack.push(if result { TRUE_ID } else { FALSE_ID });
                    ip += 1;
                }

                Instruction::Add => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in Add")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in Add")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => {
                            self.arena.int(x + y)
                        }
                        (ValueData::Real(x), ValueData::Real(y)) => {
                            self.arena.real(x + y)
                        }
                        (ValueData::Integer(x), ValueData::Real(y)) => {
                            self.arena.real(*x as f64 + y)
                        }
                        (ValueData::Real(x), ValueData::Integer(y)) => {
                            self.arena.real(x + *y as f64)
                        }
                        _ => return Err("+: expected numbers".to_string()),
                    };
                    self.stack.push(result);
                    ip += 1;
                }

                Instruction::Subtract => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in Subtract")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in Subtract")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => {
                            self.arena.int(x - y)
                        }
                        (ValueData::Real(x), ValueData::Real(y)) => {
                            self.arena.real(x - y)
                        }
                        (ValueData::Integer(x), ValueData::Real(y)) => {
                            self.arena.real(*x as f64 - y)
                        }
                        (ValueData::Real(x), ValueData::Integer(y)) => {
                            self.arena.real(x - *y as f64)
                        }
                        _ => return Err("-: expected numbers".to_string()),
                    };
                    self.stack.push(result);
                    ip += 1;
                }

                Instruction::Multiply => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in Multiply")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in Multiply")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => {
                            self.arena.int(x * y)
                        }
                        (ValueData::Real(x), ValueData::Real(y)) => {
                            self.arena.real(x * y)
                        }
                        (ValueData::Integer(x), ValueData::Real(y)) => {
                            self.arena.real(*x as f64 * y)
                        }
                        (ValueData::Real(x), ValueData::Integer(y)) => {
                            self.arena.real(x * *y as f64)
                        }
                        _ => return Err("*: expected numbers".to_string()),
                    };
                    self.stack.push(result);
                    ip += 1;
                }

                Instruction::Divide => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in Divide")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in Divide")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => {
                            if *y == 0 {
                                return Err("/: division by zero".to_string());
                            }
                            self.arena.int(x / y)
                        }
                        (ValueData::Real(x), ValueData::Real(y)) => {
                            if *y == 0.0 {
                                return Err("/: division by zero".to_string());
                            }
                            self.arena.real(x / y)
                        }
                        (ValueData::Integer(x), ValueData::Real(y)) => {
                            if *y == 0.0 {
                                return Err("/: division by zero".to_string());
                            }
                            self.arena.real(*x as f64 / y)
                        }
                        (ValueData::Real(x), ValueData::Integer(y)) => {
                            if *y == 0 {
                                return Err("/: division by zero".to_string());
                            }
                            self.arena.real(x / *y as f64)
                        }
                        _ => return Err("/: expected numbers".to_string()),
                    };
                    self.stack.push(result);
                    ip += 1;
                }

                Instruction::Equal => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in Equal")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in Equal")?;
                    let result = self.values_equal(a_id, b_id);
                    self.stack.push(if result { TRUE_ID } else { FALSE_ID });
                    ip += 1;
                }

                Instruction::NumLt => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in NumLt")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in NumLt")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => x < y,
                        (ValueData::Real(x), ValueData::Real(y)) => x < y,
                        (ValueData::Integer(x), ValueData::Real(y)) => (*x as f64) < *y,
                        (ValueData::Real(x), ValueData::Integer(y)) => *x < (*y as f64),
                        _ => return Err("<: expected numbers".to_string()),
                    };
                    self.stack.push(if result { TRUE_ID } else { FALSE_ID });
                    ip += 1;
                }

                Instruction::NumGt => {
                    let b_id = self.stack.pop().ok_or("Stack underflow in NumGt")?;
                    let a_id = self.stack.pop().ok_or("Stack underflow in NumGt")?;
                    let a = self.arena.get(a_id);
                    let b = self.arena.get(b_id);

                    let result = match (a, b) {
                        (ValueData::Integer(x), ValueData::Integer(y)) => x > y,
                        (ValueData::Real(x), ValueData::Real(y)) => x > y,
                        (ValueData::Integer(x), ValueData::Real(y)) => (*x as f64) > *y,
                        (ValueData::Real(x), ValueData::Integer(y)) => *x > (*y as f64),
                        _ => return Err(">: expected numbers".to_string()),
                    };
                    self.stack.push(if result { TRUE_ID } else { FALSE_ID });
                    ip += 1;
                }

                Instruction::MakeList { n } => {
                    if self.stack.len() < *n {
                        return Err("Stack underflow in MakeList".to_string());
                    }

                    let start = self.stack.len() - n;
                    let items: Vec<ValueId> = self.stack.drain(start..).collect();

                    let mut list_id = NIL_ID;
                    for &item_id in items.iter().rev() {
                        list_id = self.arena.cons(item_id, list_id);
                    }

                    self.stack.push(list_id);
                    ip += 1;
                }

                Instruction::Pop => {
                    self.stack.pop().ok_or("Stack underflow in Pop")?;
                    ip += 1;
                }

                Instruction::Dup => {
                    let value_id = self.stack.last().copied().ok_or("Stack underflow in Dup")?;
                    self.stack.push(value_id);
                    ip += 1;
                }

                Instruction::SetVariable { depth, offset } => {
                    // Pop value from stack
                    let value_id = self.stack.pop().ok_or("Stack underflow in SetVariable")?;

                    // Set in lexical environment (display)
                    if let Some(frame) = self.frames.last_mut() {
                        if *depth < frame.display.len() {
                            if *offset < frame.display[*depth].len() {
                                frame.display[*depth][*offset] = value_id;
                                self.stack.push(value_id); // set! returns the value
                                ip += 1;
                                continue;
                            }
                        }
                    }
                    return Err(format!("set!: variable not found at depth {} offset {}", depth, offset));
                }

                Instruction::SetGlobalVariable { name } => {
                    // Pop value from stack
                    let value_id = self.stack.pop().ok_or("Stack underflow in SetGlobalVariable")?;

                    // Check if global exists
                    if self.globals.contains_key(name) {
                        self.globals.insert(name.clone(), value_id);
                        self.stack.push(value_id); // set! returns the value
                        ip += 1;
                    } else {
                        return Err(format!("set!: undefined variable: {}", name));
                    }
                }

                Instruction::DefineGlobal { name } => {
                    // Pop value from stack
                    let value_id = self.stack.pop().ok_or("Stack underflow in DefineGlobal")?;

                    // Debug: Log % variable definitions (DSSSL parameters)
                    if std::env::var("DAZZLE_DEBUG").is_ok() && name.starts_with('%') {
                        eprintln!("VM: Defining {} = {:?}", name, self.arena.get(value_id));
                    }

                    // Define (or redefine) global
                    self.globals.insert(name.clone(), value_id);

                    // define returns unspecified
                    use crate::scheme::arena::UNSPECIFIED_ID;
                    self.stack.push(UNSPECIFIED_ID);
                    ip += 1;
                }
            }
        }
    }

    /// Check if two values are equal (deep structural equality)
    fn values_equal(&self, a_id: ValueId, b_id: ValueId) -> bool {
        let a = self.arena.get(a_id);
        let b = self.arena.get(b_id);

        match (a, b) {
            (ValueData::Nil, ValueData::Nil) => true,
            (ValueData::Bool(x), ValueData::Bool(y)) => x == y,
            (ValueData::Integer(x), ValueData::Integer(y)) => x == y,
            (ValueData::Real(x), ValueData::Real(y)) => (x - y).abs() < f64::EPSILON,
            (ValueData::Char(x), ValueData::Char(y)) => x == y,
            (ValueData::String(x), ValueData::String(y)) => x == y,
            (ValueData::Symbol(x), ValueData::Symbol(y)) => x == y,
            (ValueData::Pair { car: car1, cdr: cdr1, .. }, ValueData::Pair { car: car2, cdr: cdr2, .. }) => {
                self.values_equal(*car1, *car2) && self.values_equal(*cdr1, *cdr2)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheme::compiler::Compiler;
    use crate::scheme::instruction::Program;
    use std::rc::Rc;

    #[test]
    fn test_vm_constant() {
        let mut arena = Arena::new();
        let value_id = arena.int(42);

        let mut program = Program::new();
        program.emit(Instruction::Constant { value_id });
        program.emit(Instruction::Return);

        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, 0).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(42)));
    }

    #[test]
    fn test_vm_arithmetic() {
        let mut arena = Arena::new();

        let mut program = Program::new();

        // Compute (+ 2 3)
        let a_id = arena.int(2);
        let b_id = arena.int(3);

        program.emit(Instruction::Constant { value_id: a_id });
        program.emit(Instruction::Constant { value_id: b_id });
        program.emit(Instruction::Add);
        program.emit(Instruction::Return);

        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, 0).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(5)));
    }

    #[test]
    fn test_vm_list_ops() {
        let mut arena = Arena::new();

        let mut program = Program::new();

        // Build (cons 1 2)
        let a_id = arena.int(1);
        let b_id = arena.int(2);

        program.emit(Instruction::Constant { value_id: a_id });
        program.emit(Instruction::Constant { value_id: b_id });
        program.emit(Instruction::Cons);
        program.emit(Instruction::Car); // Get car of the pair
        program.emit(Instruction::Return);

        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, 0).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(1)));
    }

    #[test]
    fn test_vm_with_primitive_call() {
        // Create arena and program separately
        let mut arena = Arena::new();
        let mut program = Program::new();

        // Build expression: (+ 10 32)
        let ten = arena.int(10);
        let thirty_two = arena.int(32);
        let arg_list = arena.cons(thirty_two, NIL_ID);
        let args = arena.cons(ten, arg_list);
        let plus = arena.symbol(Rc::from("+"));
        let expr = arena.cons(plus, args);

        // Compile the expression
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();

        // Get the compiled instructions
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);  // Top-level return

        // Run with primitives registered
        let mut vm = VM::with_primitives(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(42)));
    }

    #[test]
    fn test_compile_and_run_if() {
        // Create arena and program separately
        let mut arena = Arena::new();
        let mut program = Program::new();

        // Build: (if #t 100 200)
        let test = TRUE_ID;
        let consequent = arena.int(100);
        let alternative = arena.int(200);

        // Build list step by step to avoid multiple mutable borrows
        let alt_list = arena.cons(alternative, NIL_ID);
        let cons_list = arena.cons(consequent, alt_list);
        let args = arena.cons(test, cons_list);
        let if_sym = arena.symbol(Rc::from("if"));
        let expr = arena.cons(if_sym, args);

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);  // Top-level return

        // Run
        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(100)));
    }

    #[test]
    fn test_vm_let() {
        // Test: (let ((x 10) (y 20)) (+ x y))
        let mut arena = Arena::new();
        let mut program = Program::new();

        let x_sym = arena.symbol(Rc::from("x"));
        let y_sym = arena.symbol(Rc::from("y"));
        let plus_sym = arena.symbol(Rc::from("+"));
        let ten = arena.int(10);
        let twenty = arena.int(20);

        // Build binding (x 10)
        let x_val_list = arena.cons(ten, NIL_ID);
        let x_binding = arena.cons(x_sym, x_val_list);

        // Build binding (y 20)
        let y_val_list = arena.cons(twenty, NIL_ID);
        let y_binding = arena.cons(y_sym, y_val_list);

        // Build bindings list ((x 10) (y 20))
        let y_binding_list = arena.cons(y_binding, NIL_ID);
        let bindings = arena.cons(x_binding, y_binding_list);

        // Build body (+ x y)
        let y_list = arena.cons(y_sym, NIL_ID);
        let x_list = arena.cons(x_sym, y_list);
        let body = arena.cons(plus_sym, x_list);

        // Build let form
        let body_list = arena.cons(body, NIL_ID);
        let args = arena.cons(bindings, body_list);
        let let_sym = arena.symbol(Rc::from("let"));
        let expr = arena.cons(let_sym, args);

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Run with primitives
        let mut vm = VM::with_primitives(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(30)));
    }

    #[test]
    fn test_vm_let_star() {
        // Test: (let* ((x 10) (y (+ x 5))) y)
        // Should return 15 (y can reference x)
        let mut arena = Arena::new();
        let mut program = Program::new();

        let x_sym = arena.symbol(Rc::from("x"));
        let y_sym = arena.symbol(Rc::from("y"));
        let plus_sym = arena.symbol(Rc::from("+"));
        let ten = arena.int(10);
        let five = arena.int(5);

        // Build binding (x 10)
        let x_val_list = arena.cons(ten, NIL_ID);
        let x_binding = arena.cons(x_sym, x_val_list);

        // Build (+ x 5)
        let five_list = arena.cons(five, NIL_ID);
        let x_plus_list = arena.cons(x_sym, five_list);
        let y_val = arena.cons(plus_sym, x_plus_list);

        // Build binding (y (+ x 5))
        let y_val_list = arena.cons(y_val, NIL_ID);
        let y_binding = arena.cons(y_sym, y_val_list);

        // Build bindings list ((x 10) (y (+ x 5)))
        let y_binding_list = arena.cons(y_binding, NIL_ID);
        let bindings = arena.cons(x_binding, y_binding_list);

        // Build body (just y)
        let body_list = arena.cons(y_sym, NIL_ID);

        // Build let* form
        let args = arena.cons(bindings, body_list);
        let let_star_sym = arena.symbol(Rc::from("let*"));
        let expr = arena.cons(let_star_sym, args);

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Run with primitives
        let mut vm = VM::with_primitives(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(15)));
    }

    #[test]
    fn test_vm_cond() {
        // Test: (cond (#f 1) (#t 42) (else 99))
        // Should return 42
        let mut arena = Arena::new();
        let mut program = Program::new();

        let one = arena.int(1);
        let forty_two = arena.int(42);
        let ninety_nine = arena.int(99);

        // Build clause (#f 1)
        let clause1_body = arena.cons(one, NIL_ID);
        let clause1 = arena.cons(FALSE_ID, clause1_body);

        // Build clause (#t 42)
        let clause2_body = arena.cons(forty_two, NIL_ID);
        let clause2 = arena.cons(TRUE_ID, clause2_body);

        // Build clause (else 99)
        let else_sym = arena.symbol(Rc::from("else"));
        let clause3_body = arena.cons(ninety_nine, NIL_ID);
        let clause3 = arena.cons(else_sym, clause3_body);

        // Build clauses list
        let clause3_list = arena.cons(clause3, NIL_ID);
        let clause2_list = arena.cons(clause2, clause3_list);
        let clauses = arena.cons(clause1, clause2_list);

        // Build cond form
        let cond_sym = arena.symbol(Rc::from("cond"));
        let expr = arena.cons(cond_sym, clauses);

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Run
        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(42)));
    }

    #[test]
    fn test_vm_define_and_call() {
        // Test: (define x 42) x
        // Should return 42
        let mut arena = Arena::new();
        let mut program = Program::new();

        let x_sym = arena.symbol(Rc::from("x"));
        let forty_two = arena.int(42);

        // Build (define x 42)
        let val_list = arena.cons(forty_two, NIL_ID);
        let define_args = arena.cons(x_sym, val_list);
        let define_sym = arena.symbol(Rc::from("define"));
        let define_expr = arena.cons(define_sym, define_args);

        // Compile define
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(define_expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;

        // Now compile reference to x
        let mut compiler2 = Compiler::new(&arena);
        compiler2.compile(x_sym).unwrap();
        let compiled_program2 = compiler2.into_program();
        program.instructions.extend(compiled_program2.instructions);
        program.emit(Instruction::Return);

        // Run
        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(42)));
    }

    #[test]
    fn test_vm_lambda_call() {
        // Test: ((lambda (x y) (+ x y)) 10 20)
        // Should return 30
        let mut arena = Arena::new();
        let mut program = Program::new();

        let x_sym = arena.symbol(Rc::from("x"));
        let y_sym = arena.symbol(Rc::from("y"));
        let plus_sym = arena.symbol(Rc::from("+"));
        let ten = arena.int(10);
        let twenty = arena.int(20);

        // Build parameter list (x y)
        let y_param_list = arena.cons(y_sym, NIL_ID);
        let params = arena.cons(x_sym, y_param_list);

        // Build body (+ x y)
        let y_arg_list = arena.cons(y_sym, NIL_ID);
        let x_arg_list = arena.cons(x_sym, y_arg_list);
        let body = arena.cons(plus_sym, x_arg_list);

        // Build lambda (lambda (x y) (+ x y))
        let body_list = arena.cons(body, NIL_ID);
        let lambda_args = arena.cons(params, body_list);
        let lambda_sym = arena.symbol(Rc::from("lambda"));
        let lambda_expr = arena.cons(lambda_sym, lambda_args);

        // Build arguments (10 20)
        let twenty_list = arena.cons(twenty, NIL_ID);
        let args = arena.cons(ten, twenty_list);

        // Build application ((lambda ...) 10 20)
        let app_args = arena.cons(lambda_expr, args);
        let expr = app_args;

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Run with primitives
        let mut vm = VM::with_primitives(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(30)));
    }

    #[test]
    fn test_vm_closure_capture() {
        // Test: ((lambda (x) (lambda (y) (+ x y))) 10)
        // Returns a closure that captures x=10
        // Then call it with 20 to get 30
        let mut arena = Arena::new();
        let mut program = Program::new();

        let x_sym = arena.symbol(Rc::from("x"));
        let y_sym = arena.symbol(Rc::from("y"));
        let plus_sym = arena.symbol(Rc::from("+"));
        let ten = arena.int(10);
        let twenty = arena.int(20);

        // Build inner lambda body: (+ x y)
        let y_list = arena.cons(y_sym, NIL_ID);
        let x_list = arena.cons(x_sym, y_list);
        let inner_body = arena.cons(plus_sym, x_list);

        // Build inner lambda: (lambda (y) (+ x y))
        let y_params = arena.cons(y_sym, NIL_ID);
        let inner_body_list = arena.cons(inner_body, NIL_ID);
        let inner_lambda_args = arena.cons(y_params, inner_body_list);
        let lambda_sym = arena.symbol(Rc::from("lambda"));
        let inner_lambda = arena.cons(lambda_sym, inner_lambda_args);

        // Build outer lambda: (lambda (x) (lambda (y) (+ x y)))
        let x_params = arena.cons(x_sym, NIL_ID);
        let outer_body_list = arena.cons(inner_lambda, NIL_ID);
        let outer_lambda_args = arena.cons(x_params, outer_body_list);
        let outer_lambda = arena.cons(lambda_sym, outer_lambda_args);

        // Build application: ((lambda (x) ...) 10)
        let ten_list = arena.cons(ten, NIL_ID);
        let app1 = arena.cons(outer_lambda, ten_list);

        // Build call to closure: (my-closure 20)
        let closure_sym = arena.symbol(Rc::from("my-closure"));
        let twenty_list = arena.cons(twenty, NIL_ID);
        let app2 = arena.cons(closure_sym, twenty_list);

        // Compile first application
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(app1).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Compile second application and APPEND to same program
        let mut compiler2 = Compiler::new(&arena);
        let start_ip2 = compiler2.compile(app2).unwrap();
        let compiled_program2 = compiler2.into_program();

        // Append to existing program (so body_ip references remain valid)
        let offset = program.instructions.len();
        program.instructions.extend(compiled_program2.instructions);
        program.emit(Instruction::Return);

        // Adjust start_ip2 for the offset
        let adjusted_start_ip2 = offset + start_ip2;

        // Run with primitives to get the inner closure
        let mut vm = VM::with_primitives(&mut arena);
        let closure_id = vm.run(&program.instructions, start_ip).unwrap();

        // Store closure as global so we can reference it
        vm.define_global("my-closure".to_string(), closure_id);

        // Run - should get 30 (10 + 20)
        let result_id = vm.run(&program.instructions, adjusted_start_ip2).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(30)));
    }

    #[test]
    fn test_vm_case_multiple_datums() {
        // Test: (case 'b ((a) 1) ((b c) 42) (else 99))
        // Should return 42 (matching second clause, first datum)
        let mut arena = Arena::new();
        let mut program = Program::new();

        let a_sym = arena.symbol(Rc::from("a"));
        let b_sym = arena.symbol(Rc::from("b"));
        let c_sym = arena.symbol(Rc::from("c"));
        let one = arena.int(1);
        let forty_two = arena.int(42);
        let ninety_nine = arena.int(99);

        // Build key 'b (quoted)
        let key_list = arena.cons(b_sym, NIL_ID);
        let quote_sym = arena.symbol(Rc::from("quote"));
        let key = arena.cons(quote_sym, key_list);

        // Build clause ((a) 1)
        let clause1_datums = arena.cons(a_sym, NIL_ID);
        let clause1_body = arena.cons(one, NIL_ID);
        let clause1 = arena.cons(clause1_datums, clause1_body);

        // Build clause ((b c) 42)
        let c_list = arena.cons(c_sym, NIL_ID);
        let bc_list = arena.cons(b_sym, c_list);
        let clause2_datums = bc_list;
        let clause2_body = arena.cons(forty_two, NIL_ID);
        let clause2 = arena.cons(clause2_datums, clause2_body);

        // Build clause (else 99)
        let else_sym = arena.symbol(Rc::from("else"));
        let clause3_body = arena.cons(ninety_nine, NIL_ID);
        let clause3 = arena.cons(else_sym, clause3_body);

        // Build clauses list
        let clause3_list = arena.cons(clause3, NIL_ID);
        let clause2_list = arena.cons(clause2, clause3_list);
        let clauses = arena.cons(clause1, clause2_list);

        // Build case form (case key clauses...)
        let args = arena.cons(key, clauses);
        let case_sym = arena.symbol(Rc::from("case"));
        let expr = arena.cons(case_sym, args);

        // Compile
        let mut compiler = Compiler::new(&arena);
        let start_ip = compiler.compile(expr).unwrap();
        let compiled_program = compiler.into_program();
        program.instructions = compiled_program.instructions;
        program.emit(Instruction::Return);

        // Run
        let mut vm = VM::new(&mut arena);
        let result_id = vm.run(&program.instructions, start_ip).unwrap();
        let result = arena.get(result_id);

        assert!(matches!(result, ValueData::Integer(42)));
    }
}
