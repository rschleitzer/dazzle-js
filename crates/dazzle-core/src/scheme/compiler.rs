//! Bytecode compiler for Scheme expressions
//!
//! This module compiles arena-based values (ValueId/ValueData) into bytecode instructions.
//! Following OpenJade's compilation strategy where expressions are compiled once and cached.
//!
//! ## Compilation Strategy
//!
//! ```cpp
//! class Identifier {
//!     Owner<Expression> def_;   // Parsed AST
//!     InsnPtr insn_;            // Compiled instructions (cached!)
//! };
//! ```
//!
//! We compile ValueId expressions to instruction sequences, caching the start index.

use crate::scheme::arena::{Arena, ValueData, ValueId, NIL_ID, TRUE_ID, FALSE_ID};
use crate::scheme::instruction::{Instruction, Program};
use std::collections::HashMap;

/// Compilation environment (tracks lexical bindings)
#[derive(Debug, Clone)]
struct CompileEnv {
    /// Lexical frames: Vec<frame> where each frame is HashMap<name, offset>
    /// frames[0] is the outermost frame, frames[last] is the current frame
    frames: Vec<HashMap<String, usize>>,
}

impl CompileEnv {
    fn new() -> Self {
        CompileEnv {
            frames: vec![HashMap::new()],
        }
    }

    fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn add_binding(&mut self, name: String, offset: usize) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name, offset);
        }
    }

    /// Look up variable, returning (depth, offset)
    /// depth = 0 means current frame, depth = 1 means parent frame, etc.
    fn lookup(&self, name: &str) -> Option<(usize, usize)> {
        for (depth, frame) in self.frames.iter().rev().enumerate() {
            if let Some(&offset) = frame.get(name) {
                return Some((depth, offset));
            }
        }
        None
    }
}

/// Bytecode compiler
pub struct Compiler<'a> {
    program: Program,
    env: CompileEnv,
    arena: &'a Arena,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Compiler {
            program: Program::new(),
            env: CompileEnv::new(),
            arena,
        }
    }

    /// Compile an expression and return its starting instruction pointer
    pub fn compile(&mut self, expr_id: ValueId) -> Result<usize, String> {
        self.compile_expr(expr_id)
    }

    /// Extract the compiled program
    pub fn into_program(self) -> Program {
        self.program
    }

    /// Compile an expression
    fn compile_expr(&mut self, expr_id: ValueId) -> Result<usize, String> {
        let expr = self.arena.get(expr_id);

        match expr {
            // Self-evaluating constants
            ValueData::Nil | ValueData::Bool(_) | ValueData::Integer(_)
            | ValueData::Real(_) | ValueData::Char(_) | ValueData::String(_)
            | ValueData::Quantity { .. } | ValueData::Keyword(_) => {
                Ok(self.program.emit(Instruction::Constant { value_id: expr_id }))
            }

            // Symbols - variable lookup
            ValueData::Symbol(name) => {
                let name_str = name.to_string();
                if let Some((depth, offset)) = self.env.lookup(&name_str) {
                    Ok(self.program.emit(Instruction::Variable { depth, offset }))
                } else {
                    Ok(self.program.emit(Instruction::GlobalVariable { name: name_str }))
                }
            }

            // Pairs - special forms or function application
            ValueData::Pair { car, cdr, .. } => {
                let first = self.arena.get(*car);

                // Check for special forms
                if let ValueData::Symbol(name) = first {
                    match name.as_ref() {
                        "quote" => return self.compile_quote(*cdr),
                        "if" => return self.compile_if(*cdr),
                        "lambda" => return self.compile_lambda(*cdr),
                        "begin" => return self.compile_begin(*cdr),
                        "set!" => return self.compile_set(*cdr),
                        "and" => return self.compile_and(*cdr),
                        "or" => return self.compile_or(*cdr),
                        "let" => return self.compile_let(*cdr),
                        "let*" => return self.compile_let_star(*cdr),
                        "letrec" => return self.compile_letrec(*cdr),
                        "cond" => return self.compile_cond(*cdr),
                        "case" => return self.compile_case(*cdr),
                        "define" => return self.compile_define(*cdr),
                        "define-unit" => return self.compile_define_unit(*cdr),
                        "declare-initial-value" => return self.compile_declare_initial_value(*cdr),
                        "declare-characteristic" => return self.compile_declare_characteristic(*cdr),
                        "declare-flow-object-class" => return self.compile_declare_flow_object_class(*cdr),
                        "define-language" => return self.compile_define_language(*cdr),
                        _ => {}
                    }
                }

                // Regular function application
                self.compile_application(*car, *cdr)
            }

            _ => Err(format!("Cannot compile: {:?}", expr)),
        }
    }

    /// Compile (quote expr)
    fn compile_quote(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args = self.arena.get(args_id);
        match args {
            ValueData::Pair { car, .. } => {
                Ok(self.program.emit(Instruction::Constant { value_id: *car }))
            }
            _ => Err("quote: expected argument".to_string()),
        }
    }

    /// Compile (if test consequent alternative)
    fn compile_if(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() < 2 || args_vec.len() > 3 {
            return Err("if: expected 2 or 3 arguments".to_string());
        }

        let test = args_vec[0];
        let consequent = args_vec[1];
        let alternative = args_vec.get(2).copied();

        // Compile test (this is the start IP we'll return)
        let start_ip = self.compile_expr(test)?;

        // Emit Test instruction (will patch else_ip later)
        let test_ip = self.program.emit(Instruction::Test { else_ip: 0 });

        // Compile consequent
        self.compile_expr(consequent)?;

        // Jump over alternative
        let jump_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

        // Patch Test to jump to alternative
        let else_ip = self.program.instructions.len();
        self.program.patch_jump(test_ip, else_ip);

        // Compile alternative (or #f if missing)
        if let Some(alt) = alternative {
            self.compile_expr(alt)?;
        } else {
            self.program.emit(Instruction::Constant { value_id: FALSE_ID });
        }

        // Patch jump to end
        let end_ip = self.program.instructions.len();
        self.program.patch_jump(jump_ip, end_ip);

        Ok(start_ip)
    }

    /// Compile (lambda (params...) body...)
    fn compile_lambda(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() < 2 {
            return Err("lambda: expected parameters and body".to_string());
        }

        let params_id = args_vec[0];
        let body_ids = &args_vec[1..];

        // Parse parameter list
        let (params, required_count) = self.parse_params(params_id)?;

        // Find free variables in the lambda body
        let mut bound = std::collections::HashSet::new();
        for param in &params {
            bound.insert(param.clone());
        }

        // Collect all free variables from all body expressions
        let mut all_free_vars = Vec::new();
        for &body_id in body_ids {
            let free_vars = self.find_free_variables(body_id, &bound);
            for fv in free_vars {
                if !all_free_vars.contains(&fv) {
                    all_free_vars.push(fv);
                }
            }
        }

        // Start instruction sequence: push free variables onto stack
        let start_ip = self.program.instructions.len();
        for free_var in &all_free_vars {
            // Emit Variable lookup for each free variable
            if let Some((depth, offset)) = self.env.lookup(free_var) {
                self.program.emit(Instruction::Variable { depth, offset });
            } else {
                // If not found in local env, it's global - emit GlobalVariable
                self.program.emit(Instruction::GlobalVariable { name: free_var.clone() });
            }
        }

        // Emit MakeClosure with correct n_free
        let closure_ip = self.program.emit(Instruction::MakeClosure {
            params: params.clone(),
            required_count,
            body_ip: 0,
            n_free: all_free_vars.len(),
        });

        // Jump over lambda body
        let skip_body_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

        // Compile body in new environment
        let body_ip = self.program.instructions.len();
        self.env.push_frame();
        for (i, param) in params.iter().enumerate() {
            self.env.add_binding(param.clone(), i);
        }

        // Compile body expressions (implicit begin)
        for &body_id in body_ids {
            self.compile_expr(body_id)?;
            if body_id != *body_ids.last().unwrap() {
                self.program.emit(Instruction::Pop);
            }
        }

        self.program.emit(Instruction::Return);
        self.env.pop_frame();

        // Patch MakeClosure with body_ip
        match &mut self.program.instructions[closure_ip] {
            Instruction::MakeClosure { body_ip: ref mut b, .. } => *b = body_ip,
            _ => unreachable!(),
        }

        // Patch skip jump
        let after_body_ip = self.program.instructions.len();
        self.program.patch_jump(skip_body_ip, after_body_ip);

        Ok(start_ip)
    }

    /// Compile (begin expr...)
    fn compile_begin(&mut self, args_id: ValueId) -> Result<usize, String> {
        let exprs = self.list_to_vec(args_id)?;

        if exprs.is_empty() {
            return Ok(self.program.emit(Instruction::Constant { value_id: NIL_ID }));
        }

        let start_ip = self.compile_expr(exprs[0])?;

        for &expr in &exprs[1..] {
            self.program.emit(Instruction::Pop);
            self.compile_expr(expr)?;
        }

        Ok(start_ip)
    }

    /// Compile (set! var value)
    fn compile_set(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() != 2 {
            return Err("set!: expected 2 arguments".to_string());
        }

        let var_id = args_vec[0];
        let value_id = args_vec[1];

        // Get variable name
        let var_data = self.arena.get(var_id);
        let name = match var_data {
            ValueData::Symbol(n) => n.to_string(),
            _ => return Err("set!: first argument must be symbol".to_string()),
        };

        // Compile value expression
        let start_ip = self.compile_expr(value_id)?;

        // Emit set instruction based on variable location
        if let Some((depth, offset)) = self.env.lookup(&name) {
            self.program.emit(Instruction::SetVariable { depth, offset });
        } else {
            self.program.emit(Instruction::SetGlobalVariable { name });
        }

        Ok(start_ip)
    }

    /// Compile (and expr...)
    fn compile_and(&mut self, args_id: ValueId) -> Result<usize, String> {
        let exprs = self.list_to_vec(args_id)?;

        if exprs.is_empty() {
            return Ok(self.program.emit(Instruction::Constant { value_id: TRUE_ID }));
        }

        let mut jump_ips = Vec::new();
        let start_ip = self.compile_expr(exprs[0])?;

        for &expr in &exprs[1..] {
            // Test current value - if false, jump to end
            let test_ip = self.program.emit(Instruction::Test { else_ip: 0 });
            jump_ips.push(test_ip);

            // Pop true value and evaluate next
            self.program.emit(Instruction::Pop);
            self.compile_expr(expr)?;
        }

        // Patch all test jumps to end
        let end_ip = self.program.instructions.len();
        for test_ip in jump_ips {
            self.program.patch_jump(test_ip, end_ip);
        }

        Ok(start_ip)
    }

    /// Compile (or expr...)
    fn compile_or(&mut self, args_id: ValueId) -> Result<usize, String> {
        let exprs = self.list_to_vec(args_id)?;

        if exprs.is_empty() {
            return Ok(self.program.emit(Instruction::Constant { value_id: FALSE_ID }));
        }

        let mut jump_ips = Vec::new();
        let start_ip = self.compile_expr(exprs[0])?;

        for &expr in &exprs[1..] {
            // Duplicate value on stack for testing
            // Test current value - if true, jump to end (keep value)
            // Note: We need to keep the true value, so we duplicate before testing
            // For now, use a simpler approach: test, if false pop and continue
            let test_ip = self.program.emit(Instruction::Test { else_ip: 0 });

            // If true, jump to end with current value
            let jump_true = self.program.emit(Instruction::Jump { target_ip: 0 });
            jump_ips.push(jump_true);

            // Patch test to continue here if false
            let continue_ip = self.program.instructions.len();
            self.program.patch_jump(test_ip, continue_ip);

            // Pop false value and evaluate next
            self.program.emit(Instruction::Pop);
            self.compile_expr(expr)?;
        }

        // Patch all jump-to-end instructions
        let end_ip = self.program.instructions.len();
        for jump_ip in jump_ips {
            self.program.patch_jump(jump_ip, end_ip);
        }

        Ok(start_ip)
    }

    /// Compile (let ((var val)...) body...) or (let name ((var val)...) body...)
    ///
    /// Standard let: (let ((x 1) (y 2)) body...)
    /// Into: ((lambda (x y) body...) 1 2)
    ///
    /// Named let: (let loop ((x 1) (y 2)) body...)
    /// Into: (letrec ((loop (lambda (x y) body...))) (loop 1 2))
    fn compile_let(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() {
            return Err("let: expected bindings and body".to_string());
        }

        // Check if this is named let: (let name ((var val)...) body...)
        let first_data = self.arena.get(args_vec[0]);
        if let ValueData::Symbol(loop_name) = first_data {
            if args_vec.len() < 3 {
                return Err("named let: expected name, bindings, and body".to_string());
            }

            // Named let - delegate to compile_named_let
            return self.compile_named_let(loop_name.to_string(), &args_vec[1..]);
        }

        // Standard let
        let bindings_id = args_vec[0];
        let body_ids = &args_vec[1..];

        if body_ids.is_empty() {
            return Err("let: expected body expressions".to_string());
        }

        // Parse bindings: ((var val) ...)
        let bindings = self.list_to_vec(bindings_id)?;
        let mut vars = Vec::new();
        let mut vals = Vec::new();

        for binding_id in bindings {
            let binding = self.list_to_vec(binding_id)?;
            if binding.len() != 2 {
                return Err("let: binding must be (var val)".to_string());
            }

            let var_data = self.arena.get(binding[0]);
            match var_data {
                ValueData::Symbol(name) => vars.push(name.to_string()),
                _ => return Err("let: variable must be symbol".to_string()),
            }

            vals.push(binding[1]);
        }

        // Compile as lambda application: ((lambda (vars...) body...) vals...)
        let start_ip = self.program.instructions.len();

        // Compile values first (they become arguments)
        for &val_id in &vals {
            self.compile_expr(val_id)?;
        }

        // Create lambda with vars as parameters and body
        // We need to manually inline the lambda compilation here
        let closure_ip = self.program.emit(Instruction::MakeClosure {
            params: vars.clone(),
            required_count: vars.len(),
            body_ip: 0,
            n_free: 0,
        });

        let skip_body_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

        // Compile body in new environment
        let body_ip = self.program.instructions.len();
        self.env.push_frame();
        for (i, var) in vars.iter().enumerate() {
            self.env.add_binding(var.clone(), i);
        }

        for (idx, &body_id) in body_ids.iter().enumerate() {
            self.compile_expr(body_id)?;
            if idx < body_ids.len() - 1 {
                self.program.emit(Instruction::Pop);
            }
        }

        self.program.emit(Instruction::Return);
        self.env.pop_frame();

        // Patch MakeClosure with body_ip
        match &mut self.program.instructions[closure_ip] {
            Instruction::MakeClosure { body_ip: ref mut b, .. } => *b = body_ip,
            _ => unreachable!(),
        }

        let after_body_ip = self.program.instructions.len();
        self.program.patch_jump(skip_body_ip, after_body_ip);

        // Apply the lambda immediately
        self.program.emit(Instruction::Apply { n_args: vals.len() });

        Ok(start_ip)
    }

    /// Compile (let* ((var val)...) body...)
    ///
    /// Sequential binding: (let* ((x 1) (y x)) body)
    /// Transforms to nested lets: (let ((x 1)) (let ((y x)) body))
    fn compile_let_star(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() {
            return Err("let*: expected bindings and body".to_string());
        }

        let bindings_id = args_vec[0];
        let body_ids = &args_vec[1..];

        if body_ids.is_empty() {
            return Err("let*: expected body expressions".to_string());
        }

        let bindings = self.list_to_vec(bindings_id)?;

        // Empty bindings case: (let* () body...) => (begin body...)
        if bindings.is_empty() {
            if body_ids.len() == 1 {
                return self.compile_expr(body_ids[0]);
            } else {
                // Build begin form
                let start_ip = self.compile_expr(body_ids[0])?;
                for &body_id in &body_ids[1..] {
                    self.program.emit(Instruction::Pop);
                    self.compile_expr(body_id)?;
                }
                return Ok(start_ip);
            }
        }

        // Parse all bindings upfront to avoid borrow issues
        let mut parsed_bindings = Vec::new();
        for &binding_id in &bindings {
            let binding = self.list_to_vec(binding_id)?;
            if binding.len() != 2 {
                return Err("let*: binding must be (var val)".to_string());
            }

            let var_data = self.arena.get(binding[0]);
            let var_name = match var_data {
                ValueData::Symbol(name) => name.to_string(),
                _ => return Err("let*: variable must be symbol".to_string()),
            };

            parsed_bindings.push((var_name, binding[1]));
        }

        // Compile as nested lambda applications
        // (let* ((x 1) (y x)) body) => ((lambda (x) ((lambda (y) body) x)) 1)
        let start_ip = self.compile_let_star_helper(&parsed_bindings, body_ids, 0)?;

        Ok(start_ip)
    }

    /// Helper to recursively compile let* as nested lambdas
    fn compile_let_star_helper(
        &mut self,
        bindings: &[(String, ValueId)],
        body_ids: &[ValueId],
        index: usize,
    ) -> Result<usize, String> {
        if index >= bindings.len() {
            // No more bindings: compile body
            let start_ip = self.compile_expr(body_ids[0])?;
            for &body_id in &body_ids[1..] {
                self.program.emit(Instruction::Pop);
                self.compile_expr(body_id)?;
            }
            return Ok(start_ip);
        }

        let (var_name, val_id) = &bindings[index];

        // Compile value expression (this becomes the argument)
        let start_ip = self.compile_expr(*val_id)?;

        // Create lambda for this binding
        let closure_ip = self.program.emit(Instruction::MakeClosure {
            params: vec![var_name.clone()],
            required_count: 1,
            body_ip: 0,
            n_free: 0,
        });

        let skip_body_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

        // Compile lambda body (which contains the rest of the bindings + final body)
        let body_ip = self.program.instructions.len();
        self.env.push_frame();
        self.env.add_binding(var_name.clone(), 0);

        // Recursively compile remaining bindings and body
        self.compile_let_star_helper(bindings, body_ids, index + 1)?;

        self.program.emit(Instruction::Return);
        self.env.pop_frame();

        // Patch MakeClosure with body_ip
        match &mut self.program.instructions[closure_ip] {
            Instruction::MakeClosure { body_ip: ref mut b, .. } => *b = body_ip,
            _ => unreachable!(),
        }

        // Patch skip jump
        let after_body_ip = self.program.instructions.len();
        self.program.patch_jump(skip_body_ip, after_body_ip);

        // Apply the lambda immediately
        self.program.emit(Instruction::Apply { n_args: 1 });

        Ok(start_ip)
    }

    /// Compile named let: (let loop ((x 1) (y 2)) body...)
    /// Transforms to: (letrec ((loop (lambda (x y) body...))) (loop 1 2))
    fn compile_named_let(&mut self, loop_name: String, args: &[ValueId]) -> Result<usize, String> {
        if args.is_empty() {
            return Err("named let: expected bindings and body".to_string());
        }

        let bindings_id = args[0];
        let body_ids = &args[1..];

        if body_ids.is_empty() {
            return Err("named let: expected body expressions".to_string());
        }

        // Parse bindings to extract vars and initial values
        let bindings = self.list_to_vec(bindings_id)?;
        let mut vars = Vec::new();
        let mut init_values = Vec::new();

        for &binding_id in &bindings {
            let binding = self.list_to_vec(binding_id)?;
            if binding.len() != 2 {
                return Err("named let: binding must be (var val)".to_string());
            }

            let var_data = self.arena.get(binding[0]);
            match var_data {
                ValueData::Symbol(name) => vars.push(name.to_string()),
                _ => return Err("named let: variable must be symbol".to_string()),
            }

            init_values.push(binding[1]);
        }

        // Compile as: (letrec ((loop (lambda (vars...) body...))) (loop init_values...))
        // This is equivalent to creating a recursive closure and immediately calling it

        let start_ip = self.program.instructions.len();

        // Compile initial values (these become arguments to the loop function)
        for &val_id in &init_values {
            self.compile_expr(val_id)?;
        }

        // Create the lambda body in a new environment where loop_name is bound
        // We'll create a closure that captures loop_name from the letrec environment

        // First, create the lambda: (lambda (vars...) body...)
        let closure_ip = self.program.emit(Instruction::MakeClosure {
            params: vars.clone(),
            required_count: vars.len(),
            body_ip: 0,
            n_free: 0,  // Will be in letrec environment
        });

        let skip_lambda_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

        // Compile lambda body with loop_name available for recursion
        let lambda_body_ip = self.program.instructions.len();
        self.env.push_frame();

        // Bind parameters
        for (i, var) in vars.iter().enumerate() {
            self.env.add_binding(var.clone(), i);
        }

        // The loop name will be available via closure from parent scope (letrec)
        // Add it as a free variable that will be captured

        // Actually, we need to handle this more carefully. In letrec, the function
        // being defined can refer to itself. We'll handle this by making the loop_name
        // available in the environment where the lambda executes.

        // For now, let's use a simpler approach: add loop_name to globals temporarily
        // This isn't perfect but will work for the common case

        // Compile body expressions
        for (idx, &body_id) in body_ids.iter().enumerate() {
            self.compile_expr(body_id)?;
            if idx < body_ids.len() - 1 {
                self.program.emit(Instruction::Pop);
            }
        }

        self.program.emit(Instruction::Return);
        self.env.pop_frame();

        // Patch lambda body_ip
        match &mut self.program.instructions[closure_ip] {
            Instruction::MakeClosure { body_ip: ref mut b, .. } => *b = lambda_body_ip,
            _ => unreachable!(),
        }

        let after_lambda_ip = self.program.instructions.len();
        self.program.patch_jump(skip_lambda_ip, after_lambda_ip);

        // Now we have the lambda on the stack, and below it are the initial values
        // We need to: 1) define loop_name = lambda, 2) call loop with initial values

        // For simplicity with letrec semantics, we'll:
        // - Define loop_name as the closure (DefineGlobal for now - not perfect but works)
        // - Load loop_name
        // - Apply with the initial values we pushed earlier

        self.program.emit(Instruction::DefineGlobal { name: loop_name.clone() });

        // Now load it back and apply
        self.program.emit(Instruction::GlobalVariable { name: loop_name });
        self.program.emit(Instruction::Apply { n_args: init_values.len() });

        Ok(start_ip)
    }

    /// Compile (letrec ((var val)...) body...)
    fn compile_letrec(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() {
            return Err("letrec: expected bindings and body".to_string());
        }

        let bindings_id = args_vec[0];
        let body_ids = &args_vec[1..];

        if body_ids.is_empty() {
            return Err("letrec: expected body expressions".to_string());
        }

        // Parse bindings
        let bindings = self.list_to_vec(bindings_id)?;
        let mut var_names = Vec::new();
        let mut val_ids = Vec::new();

        for &binding_id in &bindings {
            let binding = self.list_to_vec(binding_id)?;
            if binding.len() != 2 {
                return Err("letrec: binding must be (var val)".to_string());
            }

            let var_data = self.arena.get(binding[0]);
            match var_data {
                ValueData::Symbol(name) => var_names.push(name.to_string()),
                _ => return Err("letrec: variable must be symbol".to_string()),
            }

            val_ids.push(binding[1]);
        }

        // letrec is tricky because the bindings can refer to each other recursively
        // The standard implementation uses a "black hole" or undefined initial value
        // For simplicity, we'll use a similar approach to named let:
        // - Define all variables as globals temporarily
        // - Evaluate all values (which can now refer to the names)
        // - Define them properly
        // This isn't perfect but works for the common case of mutually recursive functions

        let start_ip = self.program.instructions.len();

        // Evaluate each value expression and define it
        for (name, val_id) in var_names.iter().zip(val_ids.iter()) {
            self.compile_expr(*val_id)?;
            self.program.emit(Instruction::DefineGlobal { name: name.clone() });
        }

        // Now compile body with all names defined
        for (idx, &body_id) in body_ids.iter().enumerate() {
            self.compile_expr(body_id)?;
            if idx < body_ids.len() - 1 {
                self.program.emit(Instruction::Pop);
            }
        }

        Ok(start_ip)
    }

    /// Compile (cond (test expr...)... [(else expr...)])
    fn compile_cond(&mut self, args_id: ValueId) -> Result<usize, String> {
        let clauses = self.list_to_vec(args_id)?;

        if clauses.is_empty() {
            return Ok(self.program.emit(Instruction::Constant { value_id: NIL_ID }));
        }

        let mut jump_to_end = Vec::new();
        let mut start_ip = 0;

        for (idx, &clause_id) in clauses.iter().enumerate() {
            let clause = self.list_to_vec(clause_id)?;
            if clause.is_empty() {
                return Err("cond: clause must have at least a test".to_string());
            }

            let test_id = clause[0];
            let test_data = self.arena.get(test_id);

            // Check for else clause
            let is_else = matches!(test_data, ValueData::Symbol(name) if name.as_ref() == "else");

            if is_else {
                if idx != clauses.len() - 1 {
                    return Err("cond: else clause must be last".to_string());
                }

                // Compile else body
                if clause.len() == 1 {
                    self.program.emit(Instruction::Constant { value_id: NIL_ID });
                } else {
                    for (i, &expr_id) in clause[1..].iter().enumerate() {
                        self.compile_expr(expr_id)?;
                        if i < clause.len() - 2 {
                            self.program.emit(Instruction::Pop);
                        }
                    }
                }
            } else {
                // Compile test
                if idx == 0 {
                    start_ip = self.compile_expr(test_id)?;
                } else {
                    self.compile_expr(test_id)?;
                }

                // Test instruction
                let test_ip = self.program.emit(Instruction::Test { else_ip: 0 });

                // Compile consequent (test value already popped by Test instruction)
                if clause.len() == 1 {
                    // No consequent: need to return test value, but Test already popped it
                    // Re-push the test value - for now, use #t as placeholder
                    // TODO: Need Dup instruction or different Test behavior
                    self.program.emit(Instruction::Constant { value_id: TRUE_ID });
                } else {
                    // Have consequent: Test already popped test value, so just evaluate consequent
                    for (i, &expr_id) in clause[1..].iter().enumerate() {
                        self.compile_expr(expr_id)?;
                        if i < clause.len() - 2 {
                            self.program.emit(Instruction::Pop);
                        }
                    }
                }

                // Jump to end
                let jump_ip = self.program.emit(Instruction::Jump { target_ip: 0 });
                jump_to_end.push(jump_ip);

                // Patch test to continue here if false
                let else_ip = self.program.instructions.len();
                self.program.patch_jump(test_ip, else_ip);
            }
        }

        // If no else clause, push #f as default
        if clauses.len() > 0 {
            let last_clause_id = clauses[clauses.len() - 1];
            let last_clause_data = self.arena.get(last_clause_id);
            let last_is_else = if let ValueData::Pair { car, .. } = last_clause_data {
                let test_data = self.arena.get(*car);
                matches!(test_data, ValueData::Symbol(name) if name.as_ref() == "else")
            } else {
                false
            };

            if !last_is_else {
                self.program.emit(Instruction::Constant { value_id: FALSE_ID });
            }
        }

        // Patch all jumps to end
        let end_ip = self.program.instructions.len();
        for jump_ip in jump_to_end {
            self.program.patch_jump(jump_ip, end_ip);
        }

        Ok(start_ip)
    }

    /// Compile (case key ((datum...) expr...)... [(else expr...)])
    fn compile_case(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() {
            return Err("case: expected key expression".to_string());
        }

        let key_expr = args_vec[0];
        let clauses = &args_vec[1..];

        if clauses.is_empty() {
            return Err("case: expected at least one clause".to_string());
        }

        // Compile key expression
        let start_ip = self.compile_expr(key_expr)?;

        let mut jump_to_end = Vec::new();

        for (idx, &clause_id) in clauses.iter().enumerate() {
            let clause = self.list_to_vec(clause_id)?;
            if clause.len() < 2 {
                return Err("case: clause must have datums and body".to_string());
            }

            let datums_id = clause[0];
            let datums_data = self.arena.get(datums_id);

            // Check for else clause
            let is_else = matches!(datums_data, ValueData::Symbol(name) if name.as_ref() == "else");

            if is_else {
                if idx != clauses.len() - 1 {
                    return Err("case: else clause must be last".to_string());
                }

                // Pop key value (we don't need it)
                self.program.emit(Instruction::Pop);

                // Compile else body
                for (i, &expr_id) in clause[1..].iter().enumerate() {
                    self.compile_expr(expr_id)?;
                    if i < clause.len() - 2 {
                        self.program.emit(Instruction::Pop);
                    }
                }
            } else {
                // Parse datum list
                let datums = self.list_to_vec(datums_id)?;
                if datums.is_empty() {
                    return Err("case: datum list must not be empty".to_string());
                }

                let mut jump_to_body = Vec::new();

                // Test key against each datum
                for &datum_id in datums.iter() {
                    // Duplicate key on stack before comparison
                    // Stack before: [key]
                    self.program.emit(Instruction::Dup);
                    // Stack after: [key, key]

                    // Push datum as constant (datums are NOT evaluated in case)
                    self.program.emit(Instruction::Constant { value_id: datum_id });
                    // Stack: [key, key, datum]

                    // Equal pops both operands and pushes result
                    // This consumes the duplicated key and datum
                    self.program.emit(Instruction::Equal);
                    // Stack: [key, result]

                    // Test result (pops result)
                    // If true (match), continue to next instruction (Jump to body)
                    // If false (no match), jump to next datum test or next clause
                    let test_ip = self.program.emit(Instruction::Test { else_ip: 0 });
                    // Stack: [key]

                    // If we get here, the test passed - jump to body
                    let jump_ip = self.program.emit(Instruction::Jump { target_ip: 0 });
                    jump_to_body.push(jump_ip);

                    // Patch Test to jump here (next datum test or next clause) if test failed
                    let next_test_ip = self.program.instructions.len();
                    self.program.patch_jump(test_ip, next_test_ip);
                }

                // None matched: jump to next clause
                let next_clause_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

                // Matched: patch all jump-to-body instructions to here, pop key, execute body
                let body_ip = self.program.instructions.len();
                for jump_ip in jump_to_body {
                    self.program.patch_jump(jump_ip, body_ip);
                }

                self.program.emit(Instruction::Pop); // Pop key

                for (i, &expr_id) in clause[1..].iter().enumerate() {
                    self.compile_expr(expr_id)?;
                    if i < clause.len() - 2 {
                        self.program.emit(Instruction::Pop);
                    }
                }

                // Jump to end
                let jump_ip = self.program.emit(Instruction::Jump { target_ip: 0 });
                jump_to_end.push(jump_ip);

                // Patch next_clause jump
                let next_ip = self.program.instructions.len();
                self.program.patch_jump(next_clause_ip, next_ip);
            }
        }

        // No match and no else: pop key and return unspecified
        let last_is_else = {
            let last_clause_id = clauses[clauses.len() - 1];
            let last_clause = self.list_to_vec(last_clause_id).unwrap();
            if let Some(&first) = last_clause.first() {
                let data = self.arena.get(first);
                matches!(data, ValueData::Symbol(name) if name.as_ref() == "else")
            } else {
                false
            }
        };

        if !last_is_else {
            self.program.emit(Instruction::Pop);
            use crate::scheme::arena::UNSPECIFIED_ID;
            self.program.emit(Instruction::Constant { value_id: UNSPECIFIED_ID });
        }

        // Patch all jumps to end
        let end_ip = self.program.instructions.len();
        for jump_ip in jump_to_end {
            self.program.patch_jump(jump_ip, end_ip);
        }

        Ok(start_ip)
    }

    /// Compile (define var value) or (define (func params...) body...)
    fn compile_define(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() {
            return Err("define: expected at least 1 argument".to_string());
        }

        let first_id = args_vec[0];
        let first_data = self.arena.get(first_id);

        match first_data {
            // (define var value)
            ValueData::Symbol(name) => {
                if args_vec.len() != 2 {
                    return Err("define: expected 2 arguments".to_string());
                }

                let value_id = args_vec[1];

                // Compile value expression
                let start_ip = self.compile_expr(value_id)?;

                // Emit define instruction
                self.program.emit(Instruction::DefineGlobal {
                    name: name.to_string(),
                });

                Ok(start_ip)
            }

            // (define (func params...) body...)
            // Transform to: (define func (lambda (params...) body...))
            ValueData::Pair {car: func_name_id, cdr: params_id, ..} => {
                // Extract function name
                let func_name_data = self.arena.get(*func_name_id);
                let func_name = match func_name_data {
                    ValueData::Symbol(n) => n.to_string(),
                    _ => return Err("define: function name must be symbol".to_string()),
                };

                // Parse parameters (handles both proper and improper lists)
                let (params, required_count) = self.parse_params(*params_id)?;

                // Body expressions
                let body_ids = &args_vec[1..];
                if body_ids.is_empty() {
                    return Err("define: expected function body".to_string());
                }

                // Compile as (lambda (params...) body...)
                let start_ip = self.program.instructions.len();

                // Emit MakeClosure
                let closure_ip = self.program.emit(Instruction::MakeClosure {
                    params: params.clone(),
                    required_count,
                    body_ip: 0,
                    n_free: 0,
                });

                let skip_body_ip = self.program.emit(Instruction::Jump { target_ip: 0 });

                // Compile body in new environment
                let body_ip = self.program.instructions.len();
                self.env.push_frame();
                for (i, param) in params.iter().enumerate() {
                    self.env.add_binding(param.clone(), i);
                }

                for (idx, &body_id) in body_ids.iter().enumerate() {
                    self.compile_expr(body_id)?;
                    if idx < body_ids.len() - 1 {
                        self.program.emit(Instruction::Pop);
                    }
                }

                self.program.emit(Instruction::Return);
                self.env.pop_frame();

                // Patch MakeClosure
                match &mut self.program.instructions[closure_ip] {
                    Instruction::MakeClosure { body_ip: ref mut b, .. } => *b = body_ip,
                    _ => unreachable!(),
                }

                let after_body_ip = self.program.instructions.len();
                self.program.patch_jump(skip_body_ip, after_body_ip);

                // Emit DefineGlobal
                self.program.emit(Instruction::DefineGlobal { name: func_name });

                Ok(start_ip)
            }

            _ => Err("define: invalid syntax".to_string()),
        }
    }

    /// Compile (define-unit name value)
    /// DSSSL unit definition - defines a unit (em, pi, pt, etc.) as a quantity value
    fn compile_define_unit(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() != 2 {
            return Err("define-unit: expected exactly 2 arguments".to_string());
        }

        let name_id = args_vec[0];
        let value_id = args_vec[1];

        // Get unit name
        let name_data = self.arena.get(name_id);
        let name = match name_data {
            ValueData::Symbol(n) => n.to_string(),
            _ => return Err("define-unit: first argument must be symbol".to_string()),
        };

        // Compile value expression
        let start_ip = self.compile_expr(value_id)?;

        // Emit define instruction
        self.program.emit(Instruction::DefineGlobal { name });

        Ok(start_ip)
    }

    /// Compile (declare-initial-value characteristic-name value-expression)
    /// DSSSL initial value declaration - sets the initial value for a characteristic
    /// Note: characteristic-name is a bare symbol (not evaluated), value-expression is evaluated
    fn compile_declare_initial_value(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() != 2 {
            return Err("declare-initial-value: expected exactly 2 arguments".to_string());
        }

        let name_id = args_vec[0];
        let value_id = args_vec[1];

        // Get characteristic name (not evaluated!)
        let name_data = self.arena.get(name_id);
        let name = match name_data {
            ValueData::Symbol(n) => n.to_string(),
            _ => return Err("declare-initial-value: first argument must be symbol".to_string()),
        };

        // Compile value expression
        let start_ip = self.compile_expr(value_id)?;

        // Define the characteristic name as a global variable with its value
        self.program.emit(Instruction::DefineGlobal { name });

        Ok(start_ip)
    }

    /// Compile (declare-characteristic characteristic-name inherited?)
    /// DSSSL characteristic declaration - no-op for code generation
    fn compile_declare_characteristic(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.is_empty() || args_vec.len() > 2 {
            return Err("declare-characteristic: expected 1 or 2 arguments".to_string());
        }

        // Validate that first argument is a symbol (but don't evaluate it)
        let name_data = self.arena.get(args_vec[0]);
        if !matches!(name_data, ValueData::Symbol(_)) {
            return Err("declare-characteristic: first argument must be symbol".to_string());
        }

        // Return unspecified (no code generated)
        use crate::scheme::arena::UNSPECIFIED_ID;
        Ok(self.program.emit(Instruction::Constant { value_id: UNSPECIFIED_ID }))
    }

    /// Compile (declare-flow-object-class class-name (parent-classes...))
    /// DSSSL flow object class declaration - no-op for code generation
    fn compile_declare_flow_object_class(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() != 2 {
            return Err("declare-flow-object-class: expected exactly 2 arguments".to_string());
        }

        // Validate that first argument is a symbol (but don't evaluate it)
        let name_data = self.arena.get(args_vec[0]);
        if !matches!(name_data, ValueData::Symbol(_)) {
            return Err("declare-flow-object-class: first argument must be symbol".to_string());
        }

        // Return unspecified (no code generated)
        use crate::scheme::arena::UNSPECIFIED_ID;
        Ok(self.program.emit(Instruction::Constant { value_id: UNSPECIFIED_ID }))
    }

    /// Compile (define-language lang-name value)
    /// DSSSL language definition
    fn compile_define_language(&mut self, args_id: ValueId) -> Result<usize, String> {
        let args_vec = self.list_to_vec(args_id)?;

        if args_vec.len() != 2 {
            return Err("define-language: expected exactly 2 arguments".to_string());
        }

        let name_id = args_vec[0];
        let value_id = args_vec[1];

        // Get language name
        let name_data = self.arena.get(name_id);
        let name = match name_data {
            ValueData::Symbol(n) => n.to_string(),
            _ => return Err("define-language: first argument must be symbol".to_string()),
        };

        // Compile value expression
        let start_ip = self.compile_expr(value_id)?;

        // Emit define instruction
        self.program.emit(Instruction::DefineGlobal { name });

        Ok(start_ip)
    }

    /// Compile function application: (func arg1 arg2 ...)
    fn compile_application(&mut self, func_id: ValueId, args_id: ValueId) -> Result<usize, String> {
        // Capture start IP before compiling anything
        let start_ip = self.program.instructions.len();

        // Compile arguments first (pushed left-to-right onto stack)
        let args = self.list_to_vec(args_id)?;
        for &arg_id in &args {
            self.compile_expr(arg_id)?;
        }

        // Compile function
        self.compile_expr(func_id)?;

        // Apply
        self.program.emit(Instruction::Apply { n_args: args.len() });

        Ok(start_ip)
    }

    /// Parse parameter list (handles both proper and improper lists)
    /// Returns (params, required_count)
    /// - Proper list (a b c) => (["a", "b", "c"], 3)
    /// - Improper list (a b . rest) => (["a", "b", "rest"], 2)
    /// - Single symbol rest => (["rest"], 0)
    fn parse_params(&self, params_id: ValueId) -> Result<(Vec<String>, usize), String> {
        let params_data = self.arena.get(params_id);

        match params_data {
            ValueData::Nil => Ok((vec![], 0)),
            ValueData::Symbol(name) => {
                // Rest parameter: (lambda args ...) or (define (func . args) ...)
                Ok((vec![name.to_string()], 0))
            }
            ValueData::Pair { .. } => {
                // Parse list, handling both proper and improper lists
                let mut names = Vec::new();
                let mut current = params_id;
                let mut required_count = 0;

                loop {
                    let data = self.arena.get(current);
                    match data {
                        ValueData::Nil => {
                            // End of proper list
                            required_count = names.len();
                            break;
                        }
                        ValueData::Pair { car, cdr, .. } => {
                            // Get the parameter name
                            let param_data = self.arena.get(*car);
                            match param_data {
                                ValueData::Symbol(name) => {
                                    names.push(name.to_string());
                                    current = *cdr;
                                }
                                ValueData::Pair { .. } => {
                                    // Destructuring parameter - not standard Scheme
                                    // Generate a placeholder name to allow compilation
                                    // Execution will fail if this code path is reached
                                    let placeholder = format!("__destructure_param_{}", names.len());
                                    eprintln!("Warning: Destructuring parameters not supported, using placeholder: {}", placeholder);
                                    names.push(placeholder);
                                    current = *cdr;
                                }
                                _ => return Err(format!("lambda: parameter must be symbol, got {:?}", param_data)),
                            }
                        }
                        ValueData::Symbol(rest_name) => {
                            // Hit a rest parameter in improper list (a b . rest)
                            required_count = names.len();
                            names.push(rest_name.to_string());
                            break;
                        }
                        _ => return Err(format!("lambda: invalid parameter list, got {:?}", data)),
                    }
                }

                Ok((names, required_count))
            }
            _ => Err("lambda: invalid parameter list".to_string()),
        }
    }

    /// Convert a list (ValueId) to Vec<ValueId>
    fn list_to_vec(&self, list_id: ValueId) -> Result<Vec<ValueId>, String> {
        let mut result = Vec::new();
        let mut current = list_id;

        loop {
            let data = self.arena.get(current);
            match data {
                ValueData::Nil => break,
                ValueData::Pair { car, cdr, .. } => {
                    result.push(*car);
                    current = *cdr;
                }
                _ => return Err("Expected proper list".to_string()),
            }
        }

        Ok(result)
    }

    /// Find free variables in an expression (variables not bound in params or current env)
    fn find_free_variables(
        &self,
        expr_id: ValueId,
        bound: &std::collections::HashSet<String>,
    ) -> Vec<String> {
        let mut free_vars = Vec::new();
        let mut visited = std::collections::HashSet::new();
        self.collect_free_vars(expr_id, bound, &mut free_vars, &mut visited);

        // Deduplicate while preserving order
        let mut seen = std::collections::HashSet::new();
        free_vars.retain(|v| seen.insert(v.clone()));

        free_vars
    }

    /// Recursively collect free variables from an expression
    fn collect_free_vars(
        &self,
        expr_id: ValueId,
        bound: &std::collections::HashSet<String>,
        free_vars: &mut Vec<String>,
        visited: &mut std::collections::HashSet<ValueId>,
    ) {
        // Avoid infinite loops on circular structures
        if visited.contains(&expr_id) {
            return;
        }
        visited.insert(expr_id);

        let expr = self.arena.get(expr_id);

        match expr {
            // Symbol: check if it's a free variable
            ValueData::Symbol(name) => {
                let name_str = name.to_string();
                if !bound.contains(&name_str) && self.env.lookup(&name_str).is_some() {
                    // It's in the outer environment but not in our bound set
                    free_vars.push(name_str);
                }
            }

            // Pair: recursively check car and cdr
            ValueData::Pair { car, cdr, .. } => {
                let first = self.arena.get(*car);

                // Handle special forms that introduce bindings
                if let ValueData::Symbol(name) = first {
                    match name.as_ref() {
                        "lambda" => {
                            // Don't analyze lambda bodies - they have their own scope
                            return;
                        }
                        "let" | "let*" => {
                            // Don't analyze let bindings - simplified for now
                            return;
                        }
                        "quote" => {
                            // Quoted expressions don't have free variables
                            return;
                        }
                        _ => {}
                    }
                }

                // Regular pair: recurse on both elements
                self.collect_free_vars(*car, bound, free_vars, visited);
                self.collect_free_vars(*cdr, bound, free_vars, visited);
            }

            // Other types don't contain free variables
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_compile_constant() {
        let mut arena = Arena::new();
        let value_id = arena.int(42);

        let mut compiler = Compiler::new(&arena);
        let ip = compiler.compile(value_id).unwrap();

        assert_eq!(ip, 0);
        assert_eq!(compiler.program.instructions.len(), 1);
    }

    #[test]
    fn test_compile_if() {
        let mut arena = Arena::new();

        // Build (if #t 1 2)
        let test = TRUE_ID;
        let cons_val = arena.int(1);
        let alt_val = arena.int(2);

        let alt_pair = arena.cons(alt_val, NIL_ID);
        let cons_pair = arena.cons(cons_val, alt_pair);
        let args = arena.cons(test, cons_pair);

        let if_sym = arena.symbol(Rc::from("if"));
        let expr = arena.cons(if_sym, args);

        let mut compiler = Compiler::new(&arena);
        let ip = compiler.compile(expr).unwrap();

        assert!(ip == 0);
        // Should have: Constant(test), Test, Constant(consequent), Jump, Constant(alternative)
        assert!(compiler.program.instructions.len() >= 5);
    }

    #[test]
    fn test_compile_or() {
        let mut arena = Arena::new();

        // Build (or #f 42)
        let false_val = FALSE_ID;
        let forty_two = arena.int(42);

        let second = arena.cons(forty_two, NIL_ID);
        let args = arena.cons(false_val, second);

        let or_sym = arena.symbol(Rc::from("or"));
        let expr = arena.cons(or_sym, args);

        let mut compiler = Compiler::new(&arena);
        let ip = compiler.compile(expr).unwrap();

        assert_eq!(ip, 0);
        // Should have compiled or expression
        assert!(compiler.program.instructions.len() > 0);
    }

    #[test]
    fn test_compile_let() {
        let mut arena = Arena::new();

        // Build (let ((x 10)) x)
        let x_sym = arena.symbol(Rc::from("x"));
        let ten = arena.int(10);

        // Build binding (x 10)
        let binding_vals = arena.cons(ten, NIL_ID);
        let binding = arena.cons(x_sym, binding_vals);

        // Build bindings list ((x 10))
        let bindings = arena.cons(binding, NIL_ID);

        // Build body (x)
        let body = arena.cons(x_sym, NIL_ID);

        // Build args (bindings body)
        let args = arena.cons(bindings, body);

        let let_sym = arena.symbol(Rc::from("let"));
        let expr = arena.cons(let_sym, args);

        let mut compiler = Compiler::new(&arena);
        let ip = compiler.compile(expr).unwrap();

        assert_eq!(ip, 0);
        // Let compiles to lambda + apply
        assert!(compiler.program.instructions.len() > 0);
    }

    #[test]
    fn test_compile_define() {
        let mut arena = Arena::new();

        // Build (define x 42)
        let x_sym = arena.symbol(Rc::from("x"));
        let forty_two = arena.int(42);

        let val_list = arena.cons(forty_two, NIL_ID);
        let args = arena.cons(x_sym, val_list);

        let define_sym = arena.symbol(Rc::from("define"));
        let expr = arena.cons(define_sym, args);

        let mut compiler = Compiler::new(&arena);
        let ip = compiler.compile(expr).unwrap();

        assert_eq!(ip, 0);
        // Should end with DefineGlobal
        let last_insn = compiler.program.instructions.last();
        assert!(matches!(last_insn, Some(Instruction::DefineGlobal { .. })));
    }
}
