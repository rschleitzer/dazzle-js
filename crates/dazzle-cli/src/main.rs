//! Dazzle CLI entry point
//!
//! Command-line tool for template-driven code generation using Scheme and XML.

use anyhow::{Context, Result};
use clap::Parser;
use dazzle_backend_sgml::SgmlBackend;
use dazzle_cli::Args;
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
                .add_directive(tracing::Level::INFO.into()),
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
    info!("Loading XML: {}", args.input.display());
    let xml_content = fs::read_to_string(&args.input)
        .with_context(|| format!("Failed to read input file: {}", args.input.display()))?;

    let grove = LibXml2Grove::parse(&xml_content, true)
        .map_err(|e| anyhow::anyhow!("Failed to parse XML: {}", e))?;
    let grove_rc = Rc::new(grove);

    debug!("XML parsed successfully");

    // 2. Determine output directory (same as input file directory, or current dir)
    let output_dir = args
        .input
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));

    // 3. Initialize SGML backend
    let mut backend = SgmlBackend::new(&output_dir);
    debug!("Backend initialized: output_dir={}", output_dir.display());

    // 4. Create evaluator with grove
    let mut evaluator = Evaluator::with_grove(grove_rc.clone());
    let root = grove_rc.root();
    evaluator.set_current_node(root);
    debug!("Evaluator initialized with grove root");

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
    info!("Loading template: {}", args.template.display());
    let template_path = find_template(&args.template, &args.search_dirs)?;
    let template_content = fs::read_to_string(&template_path)
        .with_context(|| format!("Failed to read template: {}", template_path.display()))?;

    debug!("Template loaded, evaluating...");
    evaluate_template(&mut evaluator, env.clone(), &template_content, &mut backend)?;

    // If there's accumulated output but no files were written, write to stdout
    let num_files = backend.written_files().len();
    if num_files == 0 && !backend.current_output().is_empty() {
        println!("{}", backend.current_output());
        info!("Output written to stdout");
    } else {
        info!("Code generation complete!");
        info!("Generated {} file(s) in {}", num_files, output_dir.display());
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
    backend: &mut SgmlBackend,
) -> Result<()> {
    let mut parser = SchemeParser::new(template);

    // Parse and evaluate all expressions in the template
    loop {
        match parser.parse() {
            Ok(expr) => {
                let result = evaluator
                    .eval(expr, env.clone())
                    .map_err(|e| anyhow::anyhow!("Evaluation error: {}", e))?;

                // If result is a string, write it to backend
                if let dazzle_core::scheme::value::Value::String(s) = result {
                    backend
                        .formatting_instruction(&s)
                        .context("Failed to write to backend")?;
                }
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
