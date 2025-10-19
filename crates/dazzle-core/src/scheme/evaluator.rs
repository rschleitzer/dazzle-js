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
use crate::scheme::value::{Procedure, Value};
use gc::Gc;

// =============================================================================
// Evaluation Error
// =============================================================================

/// Evaluation error
#[derive(Debug, Clone)]
pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(message: String) -> Self {
        EvalError { message }
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Eval error: {}", self.message)
    }
}

impl std::error::Error for EvalError {}

pub type EvalResult = Result<Value, EvalError>;

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
    // Future: tail call optimization state, macro table, etc.
}

impl Evaluator {
    /// Create a new evaluator
    pub fn new() -> Self {
        Evaluator {}
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
        match expr {
            // Self-evaluating literals
            Value::Nil => Ok(Value::Nil),
            Value::Bool(_) => Ok(expr),
            Value::Integer(_) => Ok(expr),
            Value::Real(_) => Ok(expr),
            Value::Char(_) => Ok(expr),
            Value::String(_) => Ok(expr),
            Value::Procedure(_) => Ok(expr),
            Value::Vector(_) => Ok(expr), // Vectors are self-evaluating in R4RS
            Value::Unspecified => Ok(expr),
            Value::Error => Ok(expr),

            // DSSSL types (self-evaluating for now)
            Value::NodeList => Ok(expr),
            Value::Sosofo => Ok(expr),

            // Symbols: variable lookup
            Value::Symbol(ref name) => env
                .lookup(name)
                .ok_or_else(|| EvalError::new(format!("Undefined variable: {}", name))),

            // Keywords are self-evaluating
            Value::Keyword(_) => Ok(expr),

            // Lists: special forms or function application
            Value::Pair(_) => self.eval_list(expr, env),
        }
    }

    /// Evaluate a list (special form or function call)
    fn eval_list(&mut self, expr: Value, env: Gc<Environment>) -> EvalResult {
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

    /// Convert a list to a Vec of elements
    fn list_to_vec(&self, list: Value) -> Result<Vec<Value>, EvalError> {
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

        let test = self.eval(args_vec[0].clone(), env.clone())?;

        if test.is_true() {
            self.eval(args_vec[1].clone(), env)
        } else if args_vec.len() == 3 {
            self.eval(args_vec[2].clone(), env)
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
                let value = self.eval(args_vec[1].clone(), env.clone())?;
                env.define(name, value);
                Ok(Value::Unspecified)
            }

            Value::Pair(_) => {
                // Function definition: (define (name params...) body...)
                // This is syntactic sugar for: (define name (lambda (params...) body...))
                let (name_val, params) = self.list_car_cdr(&args_vec[0])?;

                if let Value::Symbol(ref name) = name_val {
                    // Build lambda: (lambda params body...)
                    let lambda_body = args_vec[1..].to_vec();
                    let mut body_list = Value::Nil;
                    for expr in lambda_body.into_iter().rev() {
                        body_list = Value::cons(expr, body_list);
                    }
                    let lambda_expr = Value::cons(
                        Value::symbol("lambda"),
                        Value::cons(params, body_list),
                    );

                    let lambda_value = self.eval(lambda_expr, env.clone())?;
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
    fn eval_lambda(&mut self, _args: Value, _env: Gc<Environment>) -> EvalResult {
        // TODO: Implement lambda (needs closure representation)
        Err(EvalError::new("lambda not yet implemented".to_string()))
    }

    /// (let ((var val)...) body...)
    fn eval_let(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;
        if args_vec.len() < 2 {
            return Err(EvalError::new(
                "let requires at least 2 arguments".to_string(),
            ));
        }

        // Parse bindings: ((var1 val1) (var2 val2) ...)
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
                let value = self.eval(binding_vec[1].clone(), env.clone())?;
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
                let value = self.eval(binding_vec[1].clone(), current_env.clone())?;
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
    fn eval_letrec(&mut self, _args: Value, _env: Gc<Environment>) -> EvalResult {
        // TODO: Implement letrec (needs special handling for recursive definitions)
        Err(EvalError::new("letrec not yet implemented".to_string()))
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
            let test = self.eval(clause_vec[0].clone(), env.clone())?;
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
    fn eval_case(&mut self, _args: Value, _env: Gc<Environment>) -> EvalResult {
        // TODO: Implement case
        Err(EvalError::new("case not yet implemented".to_string()))
    }

    /// (and expr...)
    fn eval_and(&mut self, args: Value, env: Gc<Environment>) -> EvalResult {
        let args_vec = self.list_to_vec(args)?;

        if args_vec.is_empty() {
            return Ok(Value::bool(true));
        }

        let mut result = Value::bool(true);
        for expr in args_vec {
            result = self.eval(expr, env.clone())?;
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
            let result = self.eval(expr, env.clone())?;
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
            result = self.eval(expr.clone(), env.clone())?;
        }

        Ok(result)
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
        // Evaluate operator
        let proc = self.eval(operator, env.clone())?;

        // Evaluate arguments
        let args_vec = self.list_to_vec(args)?;
        let mut evaled_args = Vec::new();
        for arg in args_vec {
            evaled_args.push(self.eval(arg, env.clone())?);
        }

        // Apply procedure
        self.apply(proc, evaled_args)
    }

    /// Apply a procedure to evaluated arguments
    fn apply(&mut self, proc: Value, args: Vec<Value>) -> EvalResult {
        if let Value::Procedure(ref p) = proc {
            match &**p {
                Procedure::Primitive { func, .. } => {
                    func(&args).map_err(|e| EvalError::new(e))
                }
                Procedure::Lambda { .. } => {
                    // TODO: Implement lambda application
                    Err(EvalError::new("Lambda application not yet implemented".to_string()))
                }
            }
        } else {
            Err(EvalError::new(format!(
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
}
