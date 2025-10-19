//! Integration tests for the complete Scheme interpreter
//!
//! These tests verify the full pipeline: Parse → Eval → Result
//!
//! This demonstrates that all components work together:
//! - Parser: Source code → Value (AST)
//! - Evaluator: Value (AST) → Value (result)
//! - Primitives: Built-in procedures accessible from Scheme code

#[cfg(test)]
mod tests {
    use crate::scheme::environment::Environment;
    use crate::scheme::evaluator::Evaluator;
    use crate::scheme::parser::Parser;
    use crate::scheme::primitives;
    use crate::scheme::value::Value;

    /// Helper: Parse, eval, and return result
    fn eval_string(code: &str) -> Value {
        // Parse
        let mut parser = Parser::new(code);
        let expr = parser.parse().expect("Parse error");

        // Create environment with primitives
        let env = Environment::new_global();
        primitives::register_list_primitives(&env);
        primitives::register_number_primitives(&env);
        primitives::register_string_primitives(&env);

        // Eval
        let mut evaluator = Evaluator::new();
        evaluator.eval(expr, env).expect("Eval error")
    }

    // =========================================================================
    // Self-evaluating literals
    // =========================================================================

    #[test]
    fn test_eval_integer() {
        let result = eval_string("42");
        assert!(matches!(result, Value::Integer(42)));
    }

    #[test]
    fn test_eval_string() {
        let result = eval_string(r#""hello""#);
        assert!(result.is_string());
    }

    #[test]
    fn test_eval_boolean() {
        let result = eval_string("#t");
        assert!(result.is_true());

        let result = eval_string("#f");
        assert!(!result.is_true());
    }

    // =========================================================================
    // Quote
    // =========================================================================

    #[test]
    fn test_eval_quote() {
        let result = eval_string("(quote (1 2 3))");
        assert!(result.is_list());
    }

    #[test]
    fn test_eval_quote_shorthand() {
        let result = eval_string("'(1 2 3)");
        assert!(result.is_list());
    }

    // =========================================================================
    // Define and variable lookup
    // =========================================================================

    #[test]
    fn test_eval_define_and_lookup() {
        let mut parser = Parser::new("(define x 42)");
        let define_expr = parser.parse().unwrap();

        let env = Environment::new_global();
        let mut evaluator = Evaluator::new();

        // Define x
        evaluator.eval(define_expr, env.clone()).unwrap();

        // Look up x
        let mut parser = Parser::new("x");
        let lookup_expr = parser.parse().unwrap();
        let result = evaluator.eval(lookup_expr, env).unwrap();

        assert!(matches!(result, Value::Integer(42)));
    }

    // =========================================================================
    // List primitives
    // =========================================================================

    #[test]
    fn test_eval_car() {
        let result = eval_string("(car '(1 2 3))");
        assert!(matches!(result, Value::Integer(1)));
    }

    #[test]
    fn test_eval_cdr() {
        let result = eval_string("(cdr '(1 2 3))");
        assert!(result.is_list());
        // Should be (2 3)
    }

    #[test]
    fn test_eval_cons() {
        let result = eval_string("(cons 1 2)");
        assert!(result.is_pair());
    }

    #[test]
    fn test_eval_list() {
        let result = eval_string("(list 1 2 3)");
        assert!(result.is_list());
    }

    #[test]
    fn test_eval_length() {
        let result = eval_string("(length '(1 2 3 4 5))");
        assert!(matches!(result, Value::Integer(5)));
    }

    #[test]
    fn test_eval_append() {
        let result = eval_string("(append '(1 2) '(3 4))");
        assert!(result.is_list());

        // Check length is 4
        let code = "(length (append '(1 2) '(3 4)))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(4)));
    }

    #[test]
    fn test_eval_reverse() {
        let result = eval_string("(car (reverse '(1 2 3)))");
        assert!(matches!(result, Value::Integer(3)));
    }

    #[test]
    fn test_eval_list_ref() {
        let result = eval_string("(list-ref '(10 20 30) 1)");
        assert!(matches!(result, Value::Integer(20)));
    }

    // =========================================================================
    // Type predicates
    // =========================================================================

    #[test]
    fn test_eval_null_p() {
        let result = eval_string("(null? '())");
        assert!(result.is_true());

        let result = eval_string("(null? '(1 2 3))");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_pair_p() {
        let result = eval_string("(pair? '(1 2))");
        assert!(result.is_true());

        let result = eval_string("(pair? '())");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_list_p() {
        let result = eval_string("(list? '(1 2 3))");
        assert!(result.is_true());

        let result = eval_string("(list? '())");
        assert!(result.is_true());
    }

    // =========================================================================
    // Conditionals
    // =========================================================================

    #[test]
    fn test_eval_if() {
        let result = eval_string("(if #t 1 2)");
        assert!(matches!(result, Value::Integer(1)));

        let result = eval_string("(if #f 1 2)");
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_eval_cond() {
        let code = r#"
            (cond
              (#f 1)
              (#t 2)
              (else 3))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(2)));
    }

    // =========================================================================
    // Let bindings
    // =========================================================================

    #[test]
    fn test_eval_let() {
        let code = r#"
            (let ((x 1)
                  (y 2))
              (cons x y))
        "#;
        let result = eval_string(code);
        assert!(result.is_pair());
    }

    #[test]
    fn test_eval_let_star() {
        let code = r#"
            (let* ((x 1)
                   (y (car '(2 3))))
              (cons x y))
        "#;
        let result = eval_string(code);
        assert!(result.is_pair());
    }

    // =========================================================================
    // Complex nested expressions
    // =========================================================================

    #[test]
    fn test_eval_nested_list_operations() {
        // (car (cdr (cdr '(1 2 3 4))))
        let result = eval_string("(car (cdr (cdr '(1 2 3 4))))");
        assert!(matches!(result, Value::Integer(3)));
    }

    #[test]
    fn test_eval_multiline_let_THE_CRITICAL_TEST() {
        // THE TEST THAT BREAKS STEEL!
        // Multi-line let bindings with arbitrary whitespace
        let code = r#"
            (let ((x 1)
                  (y 2)
                  (z 3))
              (list x y z))
        "#;

        let result = eval_string(code);
        assert!(result.is_list());

        // Verify it's a 3-element list
        let len_code = r#"
            (length (let ((x 1)
                          (y 2)
                          (z 3))
                      (list x y z)))
        "#;
        let len_result = eval_string(len_code);
        assert!(matches!(len_result, Value::Integer(3)));
    }

    #[test]
    fn test_eval_begin() {
        let code = r#"
            (begin
              (cons 1 2)
              (list 1 2 3)
              42)
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(42)));
    }

    // =========================================================================
    // Number primitives
    // =========================================================================

    #[test]
    fn test_eval_arithmetic() {
        let result = eval_string("(+ 1 2 3)");
        assert!(matches!(result, Value::Integer(6)));

        let result = eval_string("(- 10 3)");
        assert!(matches!(result, Value::Integer(7)));

        let result = eval_string("(* 2 3 4)");
        assert!(matches!(result, Value::Integer(24)));

        let result = eval_string("(/ 10 2)");
        assert!(result.is_real());
    }

    #[test]
    fn test_eval_comparison() {
        let result = eval_string("(< 1 2 3)");
        assert!(result.is_true());

        let result = eval_string("(> 3 2 1)");
        assert!(result.is_true());

        let result = eval_string("(= 5 5)");
        assert!(result.is_true());

        let result = eval_string("(<= 1 2 2)");
        assert!(result.is_true());

        let result = eval_string("(>= 3 2 2)");
        assert!(result.is_true());
    }

    #[test]
    fn test_eval_number_predicates() {
        let result = eval_string("(number? 42)");
        assert!(result.is_true());

        let result = eval_string("(integer? 42)");
        assert!(result.is_true());

        let result = eval_string("(zero? 0)");
        assert!(result.is_true());

        let result = eval_string("(positive? 5)");
        assert!(result.is_true());

        let result = eval_string("(negative? -5)");
        assert!(result.is_true());

        let result = eval_string("(odd? 3)");
        assert!(result.is_true());

        let result = eval_string("(even? 4)");
        assert!(result.is_true());
    }

    #[test]
    fn test_eval_math_functions() {
        let result = eval_string("(abs -5)");
        assert!(matches!(result, Value::Integer(5)));

        let result = eval_string("(max 1 5 3)");
        assert!(matches!(result, Value::Integer(5)));

        let result = eval_string("(min 5 1 3)");
        assert!(matches!(result, Value::Integer(1)));

        let result = eval_string("(floor 3.7)");
        assert!(matches!(result, Value::Integer(3)));

        let result = eval_string("(ceiling 3.2)");
        assert!(matches!(result, Value::Integer(4)));
    }

    #[test]
    fn test_eval_complex_arithmetic() {
        // Nested arithmetic expressions
        let result = eval_string("(+ (* 2 3) (- 10 5))");
        assert!(matches!(result, Value::Integer(11))); // (+ 6 5) = 11

        // Using arithmetic with let
        let code = r#"
            (let ((x 10)
                  (y 5))
              (+ x y))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(15)));

        // Using if with comparison
        let code = "(if (< 1 2) 100 200)";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(100)));
    }

    #[test]
    fn test_eval_arithmetic_with_lists() {
        // Calculate sum using length
        let code = "(+ (length '(1 2 3)) (length '(4 5)))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(5))); // 3 + 2 = 5

        // Use arithmetic result as list-ref index
        let code = "(list-ref '(10 20 30) (- 2 1))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(20))); // index 1
    }

    // =========================================================================
    // String primitives
    // =========================================================================

    #[test]
    fn test_eval_string_operations() {
        let result = eval_string(r#"(string-length "hello")"#);
        assert!(matches!(result, Value::Integer(5)));

        let result = eval_string(r#"(string-append "hello" " " "world")"#);
        assert!(result.is_string());

        let result = eval_string(r#"(substring "hello" 1 4)"#);
        assert!(result.is_string());
    }

    #[test]
    fn test_eval_string_comparison() {
        let result = eval_string(r#"(string=? "hello" "hello")"#);
        assert!(result.is_true());

        let result = eval_string(r#"(string<? "abc" "def")"#);
        assert!(result.is_true());

        let result = eval_string(r#"(string>? "xyz" "abc")"#);
        assert!(result.is_true());
    }

    #[test]
    fn test_eval_string_predicates() {
        let result = eval_string(r#"(string? "hello")"#);
        assert!(result.is_true());

        let result = eval_string(r#"(symbol? 'foo)"#);
        assert!(result.is_true());

        let result = eval_string(r#"(char? #\a)"#);
        assert!(result.is_true());
    }

    #[test]
    fn test_eval_string_conversions() {
        let result = eval_string(r#"(symbol->string 'foo)"#);
        assert!(result.is_string());

        let result = eval_string(r#"(string->symbol "bar")"#);
        assert!(matches!(result, Value::Symbol(_)));

        let result = eval_string(r#"(string->list "hi")"#);
        assert!(result.is_list());

        let result = eval_string(r#"(list->string '(#\h #\i))"#);
        assert!(result.is_string());
    }

    #[test]
    fn test_eval_char_operations() {
        let result = eval_string(r#"(char=? #\a #\a)"#);
        assert!(result.is_true());

        let result = eval_string(r#"(char<? #\a #\b)"#);
        assert!(result.is_true());

        let result = eval_string(r#"(char-upcase #\a)"#);
        assert!(matches!(result, Value::Char('A')));

        let result = eval_string(r#"(char-downcase #\Z)"#);
        assert!(matches!(result, Value::Char('z')));
    }

    #[test]
    fn test_eval_complex_string_operations() {
        // String append in let binding
        let code = r#"
            (let ((s1 "hello")
                  (s2 "world"))
              (string-append s1 " " s2))
        "#;
        let result = eval_string(code);
        assert!(result.is_string());

        // Using string-length with comparison
        let code = r#"(< (string-length "hi") (string-length "hello"))"#;
        let result = eval_string(code);
        assert!(result.is_true());

        // Round-trip conversion
        let code = r#"(list->string (string->list "test"))"#;
        let result = eval_string(code);
        assert!(result.is_string());
    }
}
