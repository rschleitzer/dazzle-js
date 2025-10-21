//! Dazzle CLI entry point
//!
//! Command-line tool for template-driven code generation using Scheme and XML.

use anyhow::{Context, Result};
use clap::Parser;
use dazzle::Args;
use dazzle_backend_sgml::SgmlBackend;
use dazzle_core::fot::FotBuilder;
use dazzle_core::grove::Grove;
use dazzle_core::scheme::environment::Environment;
use dazzle_core::scheme::evaluator::Evaluator;
use dazzle_core::scheme::parser::Parser as SchemeParser;
use dazzle_core::scheme::primitives;
use dazzle_grove_libxml2::LibXml2Grove;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use tracing::{debug, info};

fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::WARN.into()),
        )
        .init();

    // Parse command-line arguments
    let args = Args::parse();

    if args.verbose {
        info!("Dazzle v{}", dazzle_core::VERSION);
        info!("Template: {}", args.template.display());
        info!("Input: {}", args.input.display());
        if !args.variables.is_empty() {
            info!("Variables: {:?}", args.variables);
        }
    }

    // Run the main process
    run(args)
}

fn run(args: Args) -> Result<()> {
    // 1. Load and parse input XML
    debug!("Loading XML: {}", args.input.display());

    // Use parse_file instead of parse_string to ensure proper DTD entity resolution
    // libxml2 can resolve relative DTD paths and external entities better when parsing from file
    let input_path = args.input.to_str().ok_or_else(|| anyhow::anyhow!("Invalid input path"))?;
    let grove = LibXml2Grove::parse_file(input_path, true)
        .map_err(|e| anyhow::anyhow!("Failed to parse XML: {}", e))?;
    let grove_rc = Rc::new(grove);

    debug!("XML parsed successfully");

    // 2. Determine output directory (same as input file directory, or current dir)
    let output_dir = args
        .input
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));

    // 3. Initialize SGML backend (wrapped in Rc<RefCell> for shared mutable access)
    let backend = std::rc::Rc::new(std::cell::RefCell::new(SgmlBackend::new(&output_dir)));
    debug!("Backend initialized: output_dir={}", output_dir.display());

    // 4. Create evaluator with grove
    let mut evaluator = Evaluator::with_grove(grove_rc.clone());
    let root = grove_rc.root();
    evaluator.set_current_node(root);
    evaluator.set_backend(backend.clone());
    debug!("Evaluator initialized with grove root and backend");

    // 5. Create environment and register all primitives
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);
    debug!("Primitives registered");

    // 6. Add variables to environment
    for (key, value) in &args.variables {
        env.define(key, dazzle_core::scheme::value::Value::string(value.clone()));
        debug!("Variable defined: {} = {}", key, value);
    }

    // 7. Add simple get-variable helper function
    // This returns a variable value or a default if not defined
    let get_var_code = r#"
(define (get-variable name . rest)
  (if (null? rest)
      name
      (if (null? name) (car rest) name)))
"#;

    let mut parser = SchemeParser::new(get_var_code);
    if let Ok(expr) = parser.parse() {
        let _ = evaluator.eval(expr, env.clone());
    }

    // 8. Load and evaluate template
    debug!("Loading template: {}", args.template.display());
    let template_path = find_template(&args.template, &args.search_dirs)?;
    let template_content = fs::read_to_string(&template_path)
        .with_context(|| format!("Failed to read template: {}", template_path.display()))?;

    // Check if this is an XML template wrapper (.dsl format)
    let scheme_code = if template_content.trim_start().starts_with("<!DOCTYPE")
        || template_content.trim_start().starts_with("<?xml") {
        debug!("Detected XML template wrapper, resolving entities...");
        resolve_xml_template(&template_content, &template_path, &args.search_dirs)?
    } else {
        template_content
    };

    debug!("Template loaded, evaluating...");
    evaluate_template(&mut evaluator, env.clone(), &scheme_code)?;

    // 9. Start DSSSL processing from root (OpenJade's ProcessContext::process)
    // After template loading, construction rules are defined.
    // Now trigger automatic tree processing from the root node.
    debug!("Starting DSSSL processing from root...");
    let processing_result = evaluator
        .process_root(env.clone())
        .map_err(|e| anyhow::anyhow!("Processing error: {}", e))?;

    // If processing returned a string, write it to backend
    if let dazzle_core::scheme::value::Value::String(s) = processing_result {
        backend
            .borrow_mut()
            .formatting_instruction(&s)
            .context("Failed to write processing result to backend")?;
    }

    // If there's accumulated output but no files were written, write to stdout
    let num_files = backend.borrow().written_files().len();
    if num_files == 0 && !backend.borrow().current_output().is_empty() {
        println!("{}", backend.borrow().current_output());
        debug!("Output written to stdout");
    } else {
        debug!("Code generation complete!");
        debug!("Generated {} file(s) in {}", num_files, output_dir.display());
    }

    Ok(())
}

/// Find template file in search paths
fn find_template(template: &PathBuf, search_dirs: &[PathBuf]) -> Result<PathBuf> {
    // First, try the template path as-is (absolute or relative to cwd)
    if template.exists() {
        return Ok(template.clone());
    }

    // Then try each search directory
    for dir in search_dirs {
        let path = dir.join(template);
        if path.exists() {
            return Ok(path);
        }
    }

    // Not found
    anyhow::bail!(
        "Template not found: {} (searched in {} locations)",
        template.display(),
        search_dirs.len() + 1
    )
}

/// Evaluate template code
fn evaluate_template(
    evaluator: &mut Evaluator,
    env: gc::Gc<Environment>,
    template: &str,
) -> Result<()> {
    let mut parser = SchemeParser::new(template);

    // Parse and evaluate all expressions in the template
    loop {
        match parser.parse() {
            Ok(expr) => {
                let _result = evaluator
                    .eval(expr, env.clone())
                    .map_err(|e| anyhow::anyhow!("Evaluation error: {}", e))?;

                // Results are now handled by `make` flow objects directly
                // No need to write strings to backend here
            }
            Err(e) => {
                // Check if we've reached end of input
                let msg = e.message.to_lowercase();
                if msg.contains("unexpected end of input") || msg.contains("eof") || msg.contains("end of input") {
                    break;
                }
                return Err(anyhow::anyhow!("Parse error: {}", e.message));
            }
        }
    }

    Ok(())
}

/// Resolve XML template wrapper (.dsl format) to plain Scheme code
///
/// Parses entity declarations from DOCTYPE and resolves entity references
/// by loading external .scm files, then concatenates all Scheme code.
fn resolve_xml_template(
    xml_content: &str,
    template_path: &PathBuf,
    _search_dirs: &[PathBuf],
) -> Result<String> {
    use std::collections::HashMap;

    // Extract entity declarations from DOCTYPE
    // Format: <!ENTITY name SYSTEM "file.scm">
    let mut entities = HashMap::new();

    for line in xml_content.lines() {
        let line = line.trim();
        if line.starts_with("<!ENTITY") && line.contains("SYSTEM") {
            // Parse: <!ENTITY syntax SYSTEM "syntax.scm">
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 4 {
                let entity_name = parts[1];
                // Extract filename from quotes
                if let Some(start) = line.find('"') {
                    if let Some(end) = line[start + 1..].find('"') {
                        let filename = &line[start + 1..start + 1 + end];
                        entities.insert(entity_name.to_string(), filename.to_string());
                        debug!("Found entity: {} -> {}", entity_name, filename);
                    }
                }
            }
        }
    }

    // Find the template's directory for resolving relative paths
    let template_dir = template_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));

    // Build result by resolving entity references
    let mut result = String::new();

    for line in xml_content.lines() {
        let line = line.trim();

        // Skip XML/DOCTYPE lines and DOCTYPE closing bracket
        if line.starts_with("<!") || line.starts_with("<?") || line.starts_with("<")
            || line.starts_with("]>") || line == "]" || line.is_empty() {
            continue;
        }

        // Check for entity reference: &name;
        if line.starts_with('&') && line.ends_with(';') {
            let entity_name = &line[1..line.len() - 1];

            if let Some(filename) = entities.get(entity_name) {
                // Try to load the entity file
                let entity_path = template_dir.join(filename);

                // Read file, trying UTF-8 first, then Latin-1 (ISO-8859-1)
                let content_result = fs::read(&entity_path).and_then(|bytes| {
                    // Try UTF-8 first
                    if let Ok(utf8_str) = String::from_utf8(bytes.clone()) {
                        Ok(utf8_str)
                    } else {
                        // Fall back to Latin-1 (ISO-8859-1) - never fails
                        // OpenJade uses Latin-1 for character literals in define-language
                        Ok(bytes.iter().map(|&b| b as char).collect())
                    }
                });

                match content_result {
                    Ok(mut content) => {
                        debug!("Resolved entity &{};  from {}", entity_name, entity_path.display());

                        // Strip CDATA wrappers if present
                        // Some .scm files are wrapped in <![CDATA[...]]> for XML embedding
                        let trimmed = content.trim();
                        if trimmed.starts_with("<![CDATA[") {
                            content = trimmed[9..].to_string(); // Strip "<![CDATA["
                            if content.ends_with("]]>") {
                                content = content[..content.len() - 3].to_string(); // Strip "]]>"
                            }
                        }

                        result.push_str(&content);
                        result.push('\n');
                    }
                    Err(e) => {
                        return Err(anyhow::anyhow!(
                            "Failed to load entity file {}: {}",
                            entity_path.display(),
                            e
                        ));
                    }
                }
            }
        }
    }

    if result.is_empty() {
        anyhow::bail!("No Scheme code extracted from XML template");
    }

    Ok(result)
}
