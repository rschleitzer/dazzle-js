//! Integration tests for Grove + Scheme evaluator
//!
//! These tests verify the complete pipeline:
//! XML → libxml2 parsing → Grove implementation → Scheme primitives → Results

use dazzle_core::grove::Grove;
use dazzle_core::scheme::environment::Environment;
use dazzle_core::scheme::evaluator::Evaluator;
use dazzle_core::scheme::parser::Parser;
use dazzle_core::scheme::primitives;
use dazzle_core::scheme::value::Value;
use dazzle_grove_libxml2::LibXml2Grove;
use std::rc::Rc;

/// Helper function to set up an evaluator with a grove
fn setup_evaluator_with_xml(xml: &str) -> (Evaluator, gc::Gc<Environment>) {
    // Parse XML
    let grove = LibXml2Grove::parse(xml, false).expect("Failed to parse XML");
    let grove_rc = Rc::new(grove) as Rc<dyn Grove>;

    // Create evaluator with grove
    let mut evaluator = Evaluator::with_grove(grove_rc.clone());

    // Set current node to root
    let root = grove_rc.root();
    evaluator.set_current_node(root);

    // Create environment with all primitives
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);

    (evaluator, env)
}

/// Helper function to evaluate Scheme code
fn eval_scheme(evaluator: &mut Evaluator, env: gc::Gc<Environment>, code: &str) -> Value {
    let mut parser = Parser::new(code);
    let expr = parser.parse().expect("Failed to parse Scheme code");

    evaluator
        .eval(expr, env)
        .expect("Failed to evaluate")
}

/// Helper to check if a value is a boolean with a specific value
fn is_bool(value: &Value, expected: bool) -> bool {
    matches!(value, Value::Bool(b) if *b == expected)
}

/// Helper to check if a value is an integer with a specific value
fn is_integer(value: &Value, expected: i64) -> bool {
    matches!(value, Value::Integer(i) if *i == expected)
}

/// Helper to check if a value is a string with a specific value
fn is_string(value: &Value, expected: &str) -> bool {
    matches!(value, Value::String(s) if s.as_str() == expected)
}

/// Helper to check if a value is a symbol with a specific value
fn is_symbol(value: &Value, expected: &str) -> bool {
    matches!(value, Value::Symbol(s) if s.as_ref() == expected)
}

#[test]
fn test_current_node_and_gi() {
    let xml = r#"<?xml version="1.0"?><root><child>text</child></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test current-node returns a node
    let result = eval_scheme(&mut evaluator, env.clone(), "(node? (current-node))");
    assert!(is_bool(&result, true));

    // Test gi of current node (returns symbol, not string)
    let result = eval_scheme(&mut evaluator, env.clone(), "(gi (current-node))");
    assert!(is_symbol(&result, "root"));
}

#[test]
fn test_children_and_navigation() {
    let xml = r#"<?xml version="1.0"?><root><child1/><child2/><child3/></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test children returns a node-list
    let result = eval_scheme(&mut evaluator, env.clone(), "(node-list? (children (current-node)))");
    assert!(is_bool(&result, true));

    // Test children count
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(node-list-length (children (current-node)))",
    );
    assert!(is_integer(&result, 3));

    // Test gi of first child
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(gi (node-list-first (children (current-node))))",
    );
    assert!(is_symbol(&result, "child1"));
}

#[test]
fn test_attribute_string() {
    let xml = r#"<?xml version="1.0"?><root id="r1" name="test"><child/></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test attribute-string with existing attribute
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        r#"(attribute-string "id" (current-node))"#,
    );
    assert!(is_string(&result, "r1"));

    // Test attribute-string with another attribute
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        r#"(attribute-string "name" (current-node))"#,
    );
    assert!(is_string(&result, "test"));

    // Test attribute-string with non-existent attribute
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        r#"(attribute-string "missing" (current-node))"#,
    );
    assert!(is_bool(&result, false));
}

#[test]
fn test_data() {
    let xml = r#"<?xml version="1.0"?><root><child>Hello World</child></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Get first child
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(data (node-list-first (children (current-node))))",
    );
    assert!(is_string(&result, "Hello World"));
}

#[test]
fn test_parent() {
    let xml = r#"<?xml version="1.0"?><root><child><grandchild/></child></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Navigate: root -> child -> check parent is root
    let code = r#"
        (let ((child (node-list-first (children (current-node)))))
          (gi (parent child)))
    "#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_symbol(&result, "root"));

    // Root node should have no parent
    let result = eval_scheme(&mut evaluator, env.clone(), "(parent (current-node))");
    assert!(is_bool(&result, false));
}

#[test]
fn test_id() {
    let xml = r#"<?xml version="1.0"?><root><child id="c1">text</child><child id="c2">text</child></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test id of first child
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(id (node-list-first (children (current-node))))",
    );
    assert!(is_string(&result, "c1"));

    // Test node without id
    let result = eval_scheme(&mut evaluator, env.clone(), "(id (current-node))");
    assert!(is_bool(&result, false));
}

#[test]
fn test_complex_navigation() {
    let xml = r#"<?xml version="1.0"?>
<book id="b1">
    <title>DSSSL Guide</title>
    <chapter id="ch1">
        <title>Introduction</title>
        <para>First paragraph</para>
    </chapter>
    <chapter id="ch2">
        <title>Grove Model</title>
        <para>Second paragraph</para>
    </chapter>
</book>"#;

    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test: Get root element name
    let result = eval_scheme(&mut evaluator, env.clone(), "(gi (current-node))");
    assert!(is_symbol(&result, "book"));

    // Test: Count direct children of book (title + 2 chapters = 3)
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(node-list-length (children (current-node)))",
    );
    assert!(is_integer(&result, 3));

    // Test: Get title text
    let code = r#"
        (data (node-list-first (children (current-node))))
    "#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_string(&result, "DSSSL Guide"));

    // Test: Get first chapter id
    let code = r#"
        (let ((children-list (children (current-node))))
          (let ((second-child (node-list-first (node-list-rest children-list))))
            (id second-child)))
    "#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_string(&result, "ch1"));

    // Test: Get second chapter's title
    let code = r#"
        (let* ((chapters (node-list-rest (children (current-node))))
               (chapter2 (node-list-first (node-list-rest chapters)))
               (chapter2-title (node-list-first (children chapter2))))
          (data chapter2-title))
    "#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_string(&result, "Grove Model"));
}

#[test]
fn test_empty_elements() {
    let xml = r#"<?xml version="1.0"?><root><empty/><also-empty></also-empty></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Both empty elements should exist as children
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(node-list-length (children (current-node)))",
    );
    assert!(is_integer(&result, 2));

    // Empty elements should have no children
    let result = eval_scheme(
        &mut evaluator,
        env.clone(),
        "(node-list-empty? (children (node-list-first (children (current-node)))))",
    );
    assert!(is_bool(&result, true));
}

#[test]
fn test_node_list_operations() {
    let xml = r#"<?xml version="1.0"?><root><a/><b/><c/><d/><e/></root>"#;
    let (mut evaluator, env) = setup_evaluator_with_xml(xml);

    // Test node-list-ref with different indices
    let code = r#"(gi (node-list-ref (children (current-node)) 0))"#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_symbol(&result, "a"));

    let code = r#"(gi (node-list-ref (children (current-node)) 2))"#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_symbol(&result, "c"));

    let code = r#"(gi (node-list-ref (children (current-node)) 4))"#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_symbol(&result, "e"));

    // Test out of bounds
    let code = r#"(node-list-ref (children (current-node)) 10)"#;
    let result = eval_scheme(&mut evaluator, env.clone(), code);
    assert!(is_bool(&result, false));
}
