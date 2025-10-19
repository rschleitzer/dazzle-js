//! Test named let support

use dazzle_core::scheme::environment::Environment;
use dazzle_core::scheme::evaluator::Evaluator;
use dazzle_core::scheme::parser::Parser;
use dazzle_core::scheme::primitives;
use dazzle_core::scheme::value::Value;

#[test]
fn test_named_let_simple_loop() {
    let mut evaluator = Evaluator::new();
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);

    let code = r#"
(let loop ((i 0) (sum 0))
  (if (< i 10)
      (loop (+ i 1) (+ sum i))
      sum))
"#;

    let mut parser = Parser::new(code);
    let expr = parser.parse().unwrap();
    let result = evaluator.eval(expr, env).unwrap();

    // Should compute 0+1+2+...+9 = 45
    assert!(matches!(result, Value::Integer(45)));
}

#[test]
fn test_named_let_factorial() {
    let mut evaluator = Evaluator::new();
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);

    let code = r#"
(let fact ((n 5) (acc 1))
  (if (= n 0)
      acc
      (fact (- n 1) (* n acc))))
"#;

    let mut parser = Parser::new(code);
    let expr = parser.parse().unwrap();
    let result = evaluator.eval(expr, env).unwrap();

    // 5! = 120
    assert!(matches!(result, Value::Integer(120)));
}

#[test]
fn test_named_let_countdown() {
    let mut evaluator = Evaluator::new();
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);

    let code = r#"
(let countdown ((n 5))
  (if (= n 0)
      "done"
      (countdown (- n 1))))
"#;

    let mut parser = Parser::new(code);
    let expr = parser.parse().unwrap();
    let result = evaluator.eval(expr, env).unwrap();

    match result {
        Value::String(s) => assert_eq!(&**s, "done"),
        _ => panic!("Expected string, got {:?}", result),
    }
}
