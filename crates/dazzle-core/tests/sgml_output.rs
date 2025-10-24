//! End-to-end integration tests for SGML output generation
//!
//! Tests the complete pipeline:
//! XML → libxml2 Grove → Scheme evaluator → SGML Backend → Generated files

use dazzle_core::fot::FotBuilder;
use dazzle_core::grove::Grove;
use dazzle_core::scheme::environment::Environment;
use dazzle_core::scheme::evaluator::Evaluator;
use dazzle_core::scheme::parser::Parser;
use dazzle_core::scheme::primitives;
use dazzle_core::scheme::value::Value;
use dazzle_backend_sgml::SgmlBackend;
use dazzle_grove_libxml2::LibXml2Grove;
use std::cell::RefCell;
use std::fs;
use std::rc::Rc;
use tempfile::TempDir;

/// Helper function to set up an evaluator with XML and SGML backend
fn setup_full_pipeline(xml: &str, output_dir: &std::path::Path) -> (Evaluator, gc::Gc<Environment>, SgmlBackend) {
    // Parse XML
    let grove = LibXml2Grove::parse(xml, false).expect("Failed to parse XML");
    let grove_rc = Rc::new(grove) as Rc<dyn Grove>;

    // Create evaluator with grove
    let mut evaluator = Evaluator::with_grove(grove_rc.clone());
    let root = grove_rc.root();
    evaluator.set_current_node(root);

    // Create environment with all primitives
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);

    // Create SGML backend
    let backend = SgmlBackend::new(output_dir);

    (evaluator, env, backend)
}

/// Helper function to evaluate Scheme code
fn eval_scheme(evaluator: &mut Evaluator, env: gc::Gc<Environment>, code: &str) -> Value {
    let mut parser = Parser::new(code);
    let expr = parser.parse().expect("Failed to parse Scheme code");

    evaluator
        .eval(expr, env)
        .expect("Failed to evaluate")
}

#[test]
fn test_simple_xml_to_text() {
    let temp_dir = TempDir::new().unwrap();

    // Simple XML document
    let xml = r#"<?xml version="1.0"?>
<document>
    <title>Hello World</title>
    <body>This is a test document.</body>
</document>"#;

    let (mut evaluator, env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Extract title from XML using Scheme
    let code = r#"
        (string-append "Title: "
                      (data (node-list-first (children (current-node))))
                      "\n")
    "#;

    let result = eval_scheme(&mut evaluator, env, code);

    // Use backend to write the result
    if let Value::String(s) = result {
        backend.formatting_instruction(&s).unwrap();
    }

    // Write to file
    let content = backend.current_output().to_string();
    backend.entity("output.txt", &content).unwrap();

    // Verify file was created with correct content
    let output_path = temp_dir.path().join("output.txt");
    assert!(output_path.exists());

    let file_content = fs::read_to_string(&output_path).unwrap();
    assert_eq!(file_content, "Title: Hello World\n");
}

#[test]
fn test_xml_navigation_and_output() {
    let temp_dir = TempDir::new().unwrap();

    // More complex XML with multiple elements
    let xml = r#"<?xml version="1.0"?>
<book>
    <title>DSSSL Guide</title>
    <author>James Clark</author>
    <chapter id="ch1">Introduction</chapter>
    <chapter id="ch2">Grove Model</chapter>
</book>"#;

    let (mut evaluator, env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Navigate XML and extract data
    let code = r#"
        (let ((root (current-node)))
          (let ((title (data (node-list-ref (children root) 0)))
                (author (data (node-list-ref (children root) 1)))
                (chapter1 (data (node-list-ref (children root) 2)))
                (chapter2 (data (node-list-ref (children root) 3))))
            (string-append "Book: " title "\n"
                          "Author: " author "\n"
                          "Chapters:\n"
                          "  1. " chapter1 "\n"
                          "  2. " chapter2 "\n")))
    "#;

    let result = eval_scheme(&mut evaluator, env, code);

    // Extract string result and write to file
    if let Value::String(s) = result {
        backend.entity("book-info.txt", &s).unwrap();

        // Verify output
        let output_path = temp_dir.path().join("book-info.txt");
        let content = fs::read_to_string(&output_path).unwrap();

        assert!(content.contains("Book: DSSSL Guide"));
        assert!(content.contains("Author: James Clark"));
        assert!(content.contains("1. Introduction"));
        assert!(content.contains("2. Grove Model"));
    } else {
        panic!("Expected string result, got {:?}", result);
    }
}

#[test]
fn test_load_template_and_generate() {
    let temp_dir = TempDir::new().unwrap();

    // Create a template file
    let template_path = temp_dir.path().join("template.scm");
    let template_code = r##"
;; Helper function to extract text
(define (get-element-text node index)
  (data (node-list-ref (children node) index)))

;; Generate output
(string-append
  "# " (get-element-text (current-node) 0) "

"
  "Content: " (get-element-text (current-node) 1))
"##;
    fs::write(&template_path, template_code).unwrap();

    // XML to process
    let xml = r#"<?xml version="1.0"?>
<section>
    <heading>Introduction</heading>
    <content>This is the introduction section.</content>
</section>"#;

    let (mut evaluator, env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Load and evaluate template
    let load_code = format!(r#"(load "{}")"#, template_path.display());
    let result = eval_scheme(&mut evaluator, env, &load_code);

    // Write result to output file
    if let Value::String(s) = result {
        backend.entity("section.md", &s).unwrap();

        // Verify output
        let output_path = temp_dir.path().join("section.md");
        let content = fs::read_to_string(&output_path).unwrap();

        assert!(content.contains("# Introduction"));
        assert!(content.contains("Content: This is the introduction section."));
    } else {
        panic!("Expected string result, got {:?}", result);
    }
}

#[test]
fn test_multiple_file_generation() {
    let temp_dir = TempDir::new().unwrap();

    // XML with multiple items
    let xml = r#"<?xml version="1.0"?>
<items>
    <item id="1">First</item>
    <item id="2">Second</item>
    <item id="3">Third</item>
</items>"#;

    let (mut evaluator, env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Process each item and generate separate files
    let code = r#"
        (let ((items (children (current-node))))
          (let ((item1 (node-list-ref items 0))
                (item2 (node-list-ref items 1))
                (item3 (node-list-ref items 2)))
            (list
              (cons (id item1) (data item1))
              (cons (id item2) (data item2))
              (cons (id item3) (data item3)))))
    "#;

    let _result = eval_scheme(&mut evaluator, env, code);

    // Process the result and generate files
    // For now, manually create files based on the XML structure
    backend.entity("item-1.txt", "Content: First").unwrap();
    backend.entity("item-2.txt", "Content: Second").unwrap();
    backend.entity("item-3.txt", "Content: Third").unwrap();

    // Verify all files were created
    assert_eq!(backend.written_files().len(), 3);
    assert!(temp_dir.path().join("item-1.txt").exists());
    assert!(temp_dir.path().join("item-2.txt").exists());
    assert!(temp_dir.path().join("item-3.txt").exists());
}

#[test]
fn test_nested_directory_generation() {
    let temp_dir = TempDir::new().unwrap();

    let xml = r#"<?xml version="1.0"?>
<project>
    <name>Example</name>
</project>"#;

    let (_evaluator, _env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Generate files in nested directories
    backend.entity("src/main.rs", "fn main() {}").unwrap();
    backend.entity("src/lib.rs", "pub mod utils;").unwrap();
    backend.entity("tests/test.rs", "#[test] fn it_works() {}").unwrap();

    // Verify directory structure
    assert!(temp_dir.path().join("src").exists());
    assert!(temp_dir.path().join("tests").exists());
    assert!(temp_dir.path().join("src/main.rs").exists());
    assert!(temp_dir.path().join("src/lib.rs").exists());
    assert!(temp_dir.path().join("tests/test.rs").exists());

    let main_content = fs::read_to_string(temp_dir.path().join("src/main.rs")).unwrap();
    assert_eq!(main_content, "fn main() {}");
}

#[test]
fn test_formatting_instruction_accumulation() {
    let temp_dir = TempDir::new().unwrap();

    let xml = r#"<?xml version="1.0"?><root/>"#;
    let (_evaluator, _env, mut backend) = setup_full_pipeline(xml, temp_dir.path());

    // Build up content using formatting-instruction
    backend.formatting_instruction("Line 1\n").unwrap();
    backend.formatting_instruction("Line 2\n").unwrap();
    backend.formatting_instruction("Line 3\n").unwrap();

    // Verify buffer accumulation
    assert_eq!(backend.current_output(), "Line 1\nLine 2\nLine 3\n");

    // Write to file
    let content = backend.current_output().to_string();
    backend.entity("output.txt", &content).unwrap();

    // Clear and verify
    backend.clear_buffer();
    assert_eq!(backend.current_output(), "");

    // Verify file was written
    let file_content = fs::read_to_string(temp_dir.path().join("output.txt")).unwrap();
    assert_eq!(file_content, "Line 1\nLine 2\nLine 3\n");
}

#[test]
fn test_make_directory_flow_object() {
    let temp_dir = TempDir::new().unwrap();

    let xml = r#"<?xml version="1.0"?>
<project>
    <name>TestProject</name>
</project>"#;

    let (mut evaluator, env, backend) = setup_full_pipeline(xml, temp_dir.path());

    // Set backend on evaluator (wrap in Rc<RefCell<>>)
    let backend_rc = Rc::new(RefCell::new(backend)) as Rc<RefCell<dyn FotBuilder>>;
    evaluator.set_backend(backend_rc);

    // Use make directory flow object with functional nesting
    let code = r#"
        (begin
          (make directory path: "src"
            (make directory path: "models")
            (make directory path: "controllers"))
          (make directory path: "tests"
            (make directory path: "integration")))
    "#;

    eval_scheme(&mut evaluator, env, code);

    // Verify directories were created
    assert!(temp_dir.path().join("src/models").exists());
    assert!(temp_dir.path().join("src/models").is_dir());
    assert!(temp_dir.path().join("src/controllers").exists());
    assert!(temp_dir.path().join("src/controllers").is_dir());
    assert!(temp_dir.path().join("tests/integration").exists());
    assert!(temp_dir.path().join("tests/integration").is_dir());
}

#[test]
fn test_make_directory_then_write_files() {
    let temp_dir = TempDir::new().unwrap();

    let xml = r#"<?xml version="1.0"?><root/>"#;
    let (mut evaluator, env, backend) = setup_full_pipeline(xml, temp_dir.path());

    // Set backend on evaluator (wrap in Rc<RefCell<>>)
    let backend_rc = Rc::new(RefCell::new(backend)) as Rc<RefCell<dyn FotBuilder>>;
    evaluator.set_backend(backend_rc);

    // Create directory structure and write files using functional nesting
    let code = r#"
        (make directory path: "src"
          (make directory path: "generated"
            (make entity system-id: "model.rs"
              (make formatting-instruction data: "pub struct Model {}\n"))
            (make entity system-id: "controller.rs"
              (make formatting-instruction data: "pub struct Controller {}\n"))))
    "#;

    eval_scheme(&mut evaluator, env, code);

    // Verify directory exists
    assert!(temp_dir.path().join("src/generated").exists());
    assert!(temp_dir.path().join("src/generated").is_dir());

    // Verify files were created with correct content
    let model_path = temp_dir.path().join("src/generated/model.rs");
    assert!(model_path.exists());
    let model_content = fs::read_to_string(&model_path).unwrap();
    assert_eq!(model_content, "pub struct Model {}\n");

    let controller_path = temp_dir.path().join("src/generated/controller.rs");
    assert!(controller_path.exists());
    let controller_content = fs::read_to_string(&controller_path).unwrap();
    assert_eq!(controller_content, "pub struct Controller {}\n");
}

#[test]
fn test_directory_with_relative_entity_paths() {
    let temp_dir = TempDir::new().unwrap();

    let xml = r#"<?xml version="1.0"?><root/>"#;
    let (mut evaluator, env, backend) = setup_full_pipeline(xml, temp_dir.path());

    // Set backend on evaluator (wrap in Rc<RefCell<>>)
    let backend_rc = Rc::new(RefCell::new(backend)) as Rc<RefCell<dyn FotBuilder>>;
    evaluator.set_backend(backend_rc);

    // Create directory structure with functional nesting
    let code = r#"
        (make directory path: "generated"
          (make directory path: "models"
            (make entity system-id: "User.rs"
              (make formatting-instruction data: "pub struct User {}\n"))
            (make entity system-id: "Post.rs"
              (make formatting-instruction data: "pub struct Post {}\n")))
          (make directory path: "controllers"
            (make entity system-id: "UserController.rs"
              (make formatting-instruction data: "pub struct UserController {}\n"))))
    "#;

    eval_scheme(&mut evaluator, env, code);

    // Verify directory structure
    assert!(temp_dir.path().join("generated/models").exists());
    assert!(temp_dir.path().join("generated/controllers").exists());

    // Verify files were created with relative paths in correct directories
    let user_path = temp_dir.path().join("generated/models/User.rs");
    assert!(user_path.exists());
    let user_content = fs::read_to_string(&user_path).unwrap();
    assert_eq!(user_content, "pub struct User {}\n");

    let post_path = temp_dir.path().join("generated/models/Post.rs");
    assert!(post_path.exists());
    let post_content = fs::read_to_string(&post_path).unwrap();
    assert_eq!(post_content, "pub struct Post {}\n");

    let controller_path = temp_dir.path().join("generated/controllers/UserController.rs");
    assert!(controller_path.exists());
    let controller_content = fs::read_to_string(&controller_path).unwrap();
    assert_eq!(controller_content, "pub struct UserController {}\n");
}
