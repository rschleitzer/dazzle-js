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
        primitives::register_boolean_primitives(&env);
        primitives::register_io_primitives(&env);
        primitives::register_conversion_primitives(&env);
        primitives::register_keyword_primitives(&env);
        primitives::register_dsssl_type_primitives(&env);
        primitives::register_format_primitives(&env);
        primitives::register_grove_primitives(&env);
        primitives::register_sosofo_primitives(&env);
        primitives::register_utility_primitives(&env);

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

    #[test]
    fn test_eval_hex_numbers() {
        let result = eval_string("#xff");
        assert!(matches!(result, Value::Integer(255)));

        let result = eval_string("#x10");
        assert!(matches!(result, Value::Integer(16)));

        // Can use in expressions
        let result = eval_string("(+ #x10 #x20)");
        assert!(matches!(result, Value::Integer(48))); // 16 + 32
    }

    #[test]
    fn test_eval_octal_numbers() {
        let result = eval_string("#o77");
        assert!(matches!(result, Value::Integer(63)));

        let result = eval_string("#o10");
        assert!(matches!(result, Value::Integer(8)));

        // Can use in expressions
        let result = eval_string("(+ #o10 #o20)");
        assert!(matches!(result, Value::Integer(24))); // 8 + 16
    }

    #[test]
    fn test_eval_binary_numbers() {
        let result = eval_string("#b1010");
        assert!(matches!(result, Value::Integer(10)));

        let result = eval_string("#b1111");
        assert!(matches!(result, Value::Integer(15)));

        // Can use in expressions
        let result = eval_string("(+ #b10 #b100)");
        assert!(matches!(result, Value::Integer(6))); // 2 + 4
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

    #[test]
    fn test_eval_case() {
        // Test basic case with symbols
        let code = r#"
            (case 'b
              ((a) 1)
              ((b) 2)
              ((c) 3))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(2)));

        // Test case with multiple datums in a clause
        let code = r#"
            (case 3
              ((1 2) 'first)
              ((3 4) 'second)
              ((5 6) 'third))
        "#;
        let result = eval_string(code);
        if let Value::Symbol(ref sym) = result {
            assert_eq!(&**sym, "second");
        } else {
            panic!("Expected symbol 'second");
        }

        // Test case with else clause
        let code = r#"
            (case 99
              ((1 2) 'first)
              ((3 4) 'second)
              (else 'default))
        "#;
        let result = eval_string(code);
        if let Value::Symbol(ref sym) = result {
            assert_eq!(&**sym, "default");
        } else {
            panic!("Expected symbol 'default");
        }

        // Test case with no match and no else (returns unspecified)
        let code = r#"
            (case 99
              ((1 2) 'first)
              ((3 4) 'second))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Unspecified));

        // Test case with expression as key
        let code = r#"
            (case (car '(a b c))
              ((x y) 'not-this)
              ((a b) 'this-one)
              (else 'fallback))
        "#;
        let result = eval_string(code);
        if let Value::Symbol(ref sym) = result {
            assert_eq!(&**sym, "this-one");
        } else {
            panic!("Expected symbol 'this-one");
        }
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

    // =========================================================================
    // Lambda and closures
    // =========================================================================

    #[test]
    fn test_eval_lambda_simple() {
        // Simple identity function
        let code = "((lambda (x) x) 42)";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(42)));
    }

    #[test]
    fn test_eval_lambda_with_operations() {
        // Lambda that uses primitives
        let code = "((lambda (x y) (+ x y)) 10 20)";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(30)));
    }

    #[test]
    fn test_eval_lambda_closure() {
        // Lambda captures outer variable
        let code = r#"
            (let ((x 10))
              ((lambda (y) (+ x y)) 20))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(30)));
    }

    #[test]
    fn test_eval_define_function_shorthand() {
        // Define function using shorthand syntax
        let code = r#"
            (begin
              (define (add x y) (+ x y))
              (add 5 7))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(12)));
    }

    #[test]
    fn test_eval_lambda_nested_closures() {
        // Lambda returning lambda (currying)
        let code = r#"
            (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
              (let ((add5 (make-adder 5)))
                (add5 10)))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(15)));
    }

    #[test]
    fn test_eval_lambda_with_list_operations() {
        // Lambda with list operations
        let code = "((lambda (lst) (car (cdr lst))) '(1 2 3))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_eval_lambda_multiple_body_expressions() {
        // Lambda with multiple body expressions
        let code = r#"
            ((lambda (x)
               (+ x 1)
               (+ x 2)
               (+ x 3))
             10)
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(13))); // Only last expression returned
    }

    #[test]
    fn test_eval_higher_order_function() {
        // Simple higher-order function usage
        let code = r#"
            (begin
              (define (apply-twice f x)
                (f (f x)))
              (define (inc x) (+ x 1))
              (apply-twice inc 10))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(12)));
    }

    #[test]
    fn test_eval_lambda_with_conditionals() {
        // Lambda with if expression
        let code = r#"
            ((lambda (x)
               (if (< x 10)
                   "small"
                   "large"))
             5)
        "#;
        let result = eval_string(code);
        assert!(result.is_string());
    }

    #[test]
    fn test_eval_recursive_function() {
        // Simple recursive function
        let code = r#"
            (begin
              (define (factorial n)
                (if (<= n 1)
                    1
                    (* n (factorial (- n 1)))))
              (factorial 5))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(120)));
    }

    // =========================================================================
    // letrec (recursive bindings)
    // =========================================================================

    #[test]
    fn test_eval_letrec_simple() {
        // Simple letrec with recursive function
        let code = r#"
            (letrec ((fact (lambda (n)
                             (if (<= n 1)
                                 1
                                 (* n (fact (- n 1)))))))
              (fact 5))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(120)));
    }

    #[test]
    fn test_eval_letrec_mutually_recursive() {
        // Mutually recursive functions (even?/odd?)
        let code = r#"
            (letrec ((even? (lambda (n)
                              (if (= n 0)
                                  #t
                                  (odd? (- n 1)))))
                     (odd? (lambda (n)
                             (if (= n 0)
                                 #f
                                 (even? (- n 1))))))
              (even? 10))
        "#;
        let result = eval_string(code);
        assert!(result.is_true());
    }

    // =========================================================================
    // Higher-order functions (map, for-each, apply)
    // =========================================================================

    #[test]
    fn test_eval_map_simple() {
        // Map with lambda doubling each element
        let code = "(map (lambda (x) (* x 2)) '(1 2 3))";
        let result = eval_string(code);
        assert!(result.is_list());

        // Check first element is 2
        let code = "(car (map (lambda (x) (* x 2)) '(1 2 3)))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_eval_map_with_defined_function() {
        // Map with defined function
        let code = r#"
            (begin
              (define (square x) (* x x))
              (map square '(1 2 3 4)))
        "#;
        let result = eval_string(code);
        assert!(result.is_list());

        // Check length is 4
        let code = r#"
            (begin
              (define (square x) (* x x))
              (length (map square '(1 2 3 4))))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(4)));
    }

    #[test]
    fn test_eval_apply_with_list() {
        // Apply + to a list
        let code = "(apply + '(1 2 3 4 5))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(15)));
    }

    #[test]
    fn test_eval_apply_with_lambda() {
        // Apply a lambda to a list
        let code = "(apply (lambda (x y) (+ x y)) '(10 20))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(30)));
    }

    #[test]
    fn test_eval_for_each() {
        // for-each returns unspecified
        // We can't easily test side effects, but we can verify it runs without error
        let code = "(for-each (lambda (x) (+ x 1)) '(1 2 3))";
        let result = eval_string(code);
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_eval_map_nested() {
        // Nested map operations
        let code = r#"
            (map (lambda (x) (+ x 1))
                 (map (lambda (x) (* x 2))
                      '(1 2 3)))
        "#;
        let result = eval_string(code);
        assert!(result.is_list());

        // First element should be (1*2)+1 = 3
        let code = r#"
            (car (map (lambda (x) (+ x 1))
                      (map (lambda (x) (* x 2))
                           '(1 2 3))))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(3)));
    }

    #[test]
    fn test_eval_complex_higher_order() {
        // Complex use of higher-order functions
        let code = r#"
            (begin
              (define (compose f g)
                (lambda (x) (f (g x))))
              (define (inc x) (+ x 1))
              (define (double x) (* x 2))
              (define inc-then-double (compose double inc))
              (map inc-then-double '(1 2 3)))
        "#;
        let result = eval_string(code);
        assert!(result.is_list());

        // First element: (1+1)*2 = 4
        let code = r#"
            (begin
              (define (compose f g)
                (lambda (x) (f (g x))))
              (define (inc x) (+ x 1))
              (define (double x) (* x 2))
              (define inc-then-double (compose double inc))
              (car (map inc-then-double '(1 2 3))))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Integer(4)));
    }

    // =========================================================================
    // Boolean and equality primitives
    // =========================================================================

    #[test]
    fn test_eval_not() {
        let result = eval_string("(not #t)");
        assert!(!result.is_true());

        let result = eval_string("(not #f)");
        assert!(result.is_true());

        // Only #f is false in Scheme
        let result = eval_string("(not 0)");
        assert!(!result.is_true());

        let result = eval_string(r#"(not "")"#);
        assert!(!result.is_true());

        let result = eval_string("(not '())");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_boolean_p() {
        let result = eval_string("(boolean? #t)");
        assert!(result.is_true());

        let result = eval_string("(boolean? #f)");
        assert!(result.is_true());

        let result = eval_string("(boolean? 42)");
        assert!(!result.is_true());

        let result = eval_string(r#"(boolean? "hello")"#);
        assert!(!result.is_true());

        let result = eval_string("(boolean? '())");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_equal_p() {
        // Numbers
        let result = eval_string("(equal? 42 42)");
        assert!(result.is_true());

        let result = eval_string("(equal? 42 43)");
        assert!(!result.is_true());

        // Strings - deep comparison
        let result = eval_string(r#"(equal? "hello" "hello")"#);
        assert!(result.is_true());

        let result = eval_string(r#"(equal? "hello" "world")"#);
        assert!(!result.is_true());

        // Lists - deep structural comparison
        let result = eval_string("(equal? '(1 2 3) '(1 2 3))");
        assert!(result.is_true());

        let result = eval_string("(equal? '(1 2 3) '(1 2 4))");
        assert!(!result.is_true());

        // Nested lists
        let result = eval_string("(equal? '(1 (2 3) 4) '(1 (2 3) 4))");
        assert!(result.is_true());

        let result = eval_string("(equal? '(1 (2 3) 4) '(1 (2 4) 4))");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_eqv_p() {
        // Numbers - value equality
        let result = eval_string("(eqv? 42 42)");
        assert!(result.is_true());

        let result = eval_string("(eqv? 42 43)");
        assert!(!result.is_true());

        // Symbols - value equality
        let result = eval_string("(eqv? 'foo 'foo)");
        assert!(result.is_true());

        let result = eval_string("(eqv? 'foo 'bar)");
        assert!(!result.is_true());

        // Booleans
        let result = eval_string("(eqv? #t #t)");
        assert!(result.is_true());

        let result = eval_string("(eqv? #t #f)");
        assert!(!result.is_true());

        // Different types
        let result = eval_string(r#"(eqv? 42 "42")"#);
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_eq_p() {
        // Numbers - value equality
        let result = eval_string("(eq? 42 42)");
        assert!(result.is_true());

        // Symbols
        let result = eval_string("(eq? 'foo 'foo)");
        assert!(result.is_true());

        // Same pair should be eq?
        let result = eval_string(r#"
            (let ((x '(1 2 3)))
              (eq? x x))
        "#);
        assert!(result.is_true());

        // Different pairs with same values are not eq?
        let result = eval_string("(eq? '(1 2 3) '(1 2 3))");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_procedure_p() {
        // Primitive procedure
        let result = eval_string("(procedure? +)");
        assert!(result.is_true());

        let result = eval_string("(procedure? car)");
        assert!(result.is_true());

        // Lambda
        let result = eval_string("(procedure? (lambda (x) x))");
        assert!(result.is_true());

        // Not procedures
        let result = eval_string("(procedure? 42)");
        assert!(!result.is_true());

        let result = eval_string(r#"(procedure? "hello")"#);
        assert!(!result.is_true());

        let result = eval_string("(procedure? '(1 2 3))");
        assert!(!result.is_true());
    }

    #[test]
    fn test_eval_boolean_logic_combinations() {
        // not with comparisons
        let result = eval_string("(not (< 5 3))");
        assert!(result.is_true());

        // equal? with computation
        let result = eval_string("(equal? (+ 2 2) (* 2 2))");
        assert!(result.is_true());

        // Complex boolean logic
        let code = r#"
            (if (and (not (null? '(1 2 3)))
                     (equal? (car '(1 2 3)) 1))
                "yes"
                "no")
        "#;
        let result = eval_string(code);
        assert!(result.is_string());
    }

    // =========================================================================
    // I/O and utility primitives
    // =========================================================================

    #[test]
    fn test_eval_display() {
        let result = eval_string(r#"(display "hello")"#);
        assert!(matches!(result, Value::Unspecified));

        let result = eval_string("(display 42)");
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_eval_newline() {
        let result = eval_string("(newline)");
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_eval_write() {
        let result = eval_string(r#"(write "test")"#);
        assert!(matches!(result, Value::Unspecified));

        let result = eval_string("(write '(1 2 3))");
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_eval_display_sequence() {
        // Test sequence of display calls with begin
        let code = r#"
            (begin
              (display "Hello, ")
              (display "World!")
              (newline))
        "#;
        let result = eval_string(code);
        assert!(matches!(result, Value::Unspecified));
    }
}
