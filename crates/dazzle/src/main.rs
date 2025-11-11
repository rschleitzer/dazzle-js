//! Dazzle CLI entry point
//!
//! Command-line tool for template-driven code generation using Scheme and XML.

use anyhow::{Context, Result};
use clap::Parser;
use dazzle::Args;
use dazzle_backend_sgml::SgmlBackend;
use dazzle_backend_rtf::RtfBackend;
use dazzle_core::fot::FotBuilder;
use dazzle_core::grove::Grove;
use dazzle_core::scheme::environment::Environment;
use dazzle_core::scheme::evaluator::{Evaluator, LineMapping};
use dazzle_core::scheme::parser::Parser as SchemeParser;
use dazzle_core::scheme::primitives;
use dazzle_core::sgml_preprocess;
use dazzle_grove_libxml2::LibXml2Grove;
use regex::Regex;
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

    // Handle version flag
    if args.version {
        println!("dazzle {}", dazzle_core::VERSION);
        return Ok(());
    }

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

    // 2. Use current working directory for output (matches OpenJade behavior)
    let output_dir = PathBuf::from(".");

    // 3. Initialize backend based on -t flag
    match args.backend.as_str() {
        "text" | "xml" => {
            run_sgml_backend(args, grove_rc, output_dir)
        }
        "rtf" => {
            run_rtf_backend(args, grove_rc)
        }
        _ => {
            anyhow::bail!("Unknown backend: {} (supported: text, xml, rtf)", args.backend);
        }
    }
}

fn run_sgml_backend(args: Args, grove_rc: Rc<LibXml2Grove>, output_dir: PathBuf) -> Result<()> {
    // Initialize SGML backend (wrapped in Rc<RefCell> for shared mutable access)
    let backend = std::rc::Rc::new(std::cell::RefCell::new(SgmlBackend::new(&output_dir)));
    debug!("SGML backend initialized: output_dir={}", output_dir.display());

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

    // 6. Add simple get-variable helper function
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
    let (scheme_code, line_mappings, external_specs) = if template_content.trim_start().starts_with("<!DOCTYPE")
        || template_content.trim_start().starts_with("<?xml") {
        debug!("Detected XML template wrapper, resolving entities...");
        resolve_xml_template(&template_content, &template_path, &args.search_dirs)?
    } else {
        (template_content, Vec::new(), Vec::new())
    };

    // DEBUG: Write resolved content to file for inspection
    if let Ok(debug_path) = std::env::var("DAZZLE_DEBUG_RESOLVED") {
        if let Err(e) = std::fs::write(&debug_path, &scheme_code) {
            eprintln!("WARNING: Failed to write debug file {}: {}", debug_path, e);
        }
    }

    // Load external specifications first (e.g., dbparam.dsl, dblib.dsl)
    debug!("Loading {} external specifications...", external_specs.len());
    for (spec_id, spec_file) in &external_specs {
        debug!("Loading external specification: {} from {}", spec_id, spec_file);
        let spec_path = template_path.parent().unwrap_or(std::path::Path::new(".")).join(spec_file);
        if let Ok(spec_content) = fs::read_to_string(&spec_path) {
            // External specs may also be XML wrappers - resolve them recursively
            let (resolved_content, spec_mappings, _nested_specs) = if spec_content.trim_start().starts_with("<!DOCTYPE")
                || spec_content.trim_start().starts_with("<?xml") {
                debug!("  External spec is also an XML wrapper, resolving...");
                resolve_xml_template(&spec_content, &spec_path, &args.search_dirs)?
            } else {
                (spec_content, Vec::new(), Vec::new())
            };

            evaluator.set_source_file(spec_path.to_string_lossy().to_string());
            if !spec_mappings.is_empty() {
                evaluator.set_line_mappings(spec_mappings.clone());
            }
            evaluate_template(&mut evaluator, env.clone(), &resolved_content, &spec_mappings)?;
        } else {
            debug!("External specification not found: {}", spec_path.display());
        }
    }

    debug!("Template loaded, evaluating...");
    // Set source file for error reporting
    evaluator.set_source_file(template_path.to_string_lossy().to_string());
    // Set line mappings for accurate error reporting
    if !line_mappings.is_empty() {
        evaluator.set_line_mappings(line_mappings.clone());
    }
    evaluate_template(&mut evaluator, env.clone(), &scheme_code, &line_mappings)?;

    // 8. Override variables from command line (-V flags)
    // These take precedence over any (define ...) statements in the template
    for (key, value) in &args.variables {
        // Use set! to update existing bindings, or define if not exists
        match env.set(key, dazzle_core::scheme::value::Value::string(value.clone())) {
            Ok(_) => debug!("Variable overridden: {} = {}", key, value),
            Err(_) => {
                // Variable doesn't exist, define it
                env.define(key, dazzle_core::scheme::value::Value::string(value.clone()));
                debug!("Variable defined: {} = {}", key, value);
            }
        }
    }

    // 9. Start DSSSL processing from root (OpenJade's ProcessContext::process)
    // After template loading, construction rules are defined.
    // Now trigger automatic tree processing from the root node.
    debug!("Starting DSSSL processing from root...");
    let processing_result = evaluator
        .process_root(env.clone())
        .map_err(|e| anyhow::anyhow!("Processing error:\n{}", e))?;

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

fn run_rtf_backend(args: Args, grove_rc: Rc<LibXml2Grove>) -> Result<()> {
    // Derive output filename from input (e.g., dsssl.xml -> dsssl.rtf)
    let output_path = args.input.with_extension("rtf");
    let output_file = fs::File::create(&output_path)
        .with_context(|| format!("Failed to create output file: {}", output_path.display()))?;
    let output_writer = std::io::BufWriter::new(output_file);

    // Initialize RTF backend writing to file
    let backend = std::rc::Rc::new(std::cell::RefCell::new(
        RtfBackend::new(output_writer)
            .context("Failed to create RTF backend")?
    ));
    debug!("RTF backend initialized, writing to: {}", output_path.display());

    // Create evaluator with grove
    let mut evaluator = Evaluator::with_grove(grove_rc.clone());
    let root = grove_rc.root();
    evaluator.set_current_node(root);
    evaluator.set_backend(backend.clone());
    debug!("Evaluator initialized with grove root and backend");

    // Create environment and register all primitives
    let env = Environment::new_global();
    primitives::register_all_primitives(&env);
    debug!("Primitives registered");

    // Add simple get-variable helper function
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

    // Load and evaluate template
    debug!("Loading template: {}", args.template.display());
    let template_path = find_template(&args.template, &args.search_dirs)?;
    let template_content = fs::read_to_string(&template_path)
        .with_context(|| format!("Failed to read template: {}", template_path.display()))?;

    // Check if this is an XML template wrapper (.dsl format)
    let (scheme_code, line_mappings, external_specs) = if template_content.trim_start().starts_with("<!DOCTYPE")
        || template_content.trim_start().starts_with("<?xml") {
        debug!("Detected XML template wrapper, resolving entities...");
        resolve_xml_template(&template_content, &template_path, &args.search_dirs)?
    } else {
        (template_content, Vec::new(), Vec::new())
    };

    // DEBUG: Write resolved content to file for inspection
    if let Ok(debug_path) = std::env::var("DAZZLE_DEBUG_RESOLVED") {
        if let Err(e) = std::fs::write(&debug_path, &scheme_code) {
            eprintln!("WARNING: Failed to write debug file {}: {}", debug_path, e);
        }
    }

    // Load external specifications first (e.g., dbparam.dsl, dblib.dsl)
    debug!("Loading {} external specifications...", external_specs.len());
    for (spec_id, spec_file) in &external_specs {
        debug!("Loading external specification: {} from {}", spec_id, spec_file);
        let spec_path = template_path.parent().unwrap_or(std::path::Path::new(".")).join(spec_file);
        if let Ok(spec_content) = fs::read_to_string(&spec_path) {
            // External specs may also be XML wrappers - resolve them recursively
            let (resolved_content, spec_mappings, _nested_specs) = if spec_content.trim_start().starts_with("<!DOCTYPE")
                || spec_content.trim_start().starts_with("<?xml") {
                debug!("  External spec is also an XML wrapper, resolving...");
                resolve_xml_template(&spec_content, &spec_path, &args.search_dirs)?
            } else {
                (spec_content, Vec::new(), Vec::new())
            };

            evaluator.set_source_file(spec_path.to_string_lossy().to_string());
            if !spec_mappings.is_empty() {
                evaluator.set_line_mappings(spec_mappings.clone());
            }
            evaluate_template(&mut evaluator, env.clone(), &resolved_content, &spec_mappings)?;
        } else {
            debug!("External specification not found: {}", spec_path.display());
        }
    }

    debug!("Template loaded, evaluating...");
    // Set source file for error reporting
    evaluator.set_source_file(template_path.to_string_lossy().to_string());
    // Set line mappings for accurate error reporting
    if !line_mappings.is_empty() {
        evaluator.set_line_mappings(line_mappings.clone());
    }
    evaluate_template(&mut evaluator, env.clone(), &scheme_code, &line_mappings)?;

    // Override variables from command line (-V flags)
    for (key, value) in &args.variables {
        match env.set(key, dazzle_core::scheme::value::Value::string(value.clone())) {
            Ok(_) => debug!("Variable overridden: {} = {}", key, value),
            Err(_) => {
                env.define(key, dazzle_core::scheme::value::Value::string(value.clone()));
                debug!("Variable defined: {} = {}", key, value);
            }
        }
    }

    // Start DSSSL processing from root
    debug!("Starting DSSSL processing from root...");
    let processing_result = evaluator
        .process_root(env.clone())
        .map_err(|e| anyhow::anyhow!("Processing error:\n{}", e))?;

    // If processing returned a string, write it to backend
    if let dazzle_core::scheme::value::Value::String(s) = processing_result {
        backend
            .borrow_mut()
            .formatting_instruction(&s)
            .context("Failed to write processing result to backend")?;
    }

    // Drop will automatically call finish() on the backend
    debug!("RTF generation complete");
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

/// Evaluate template code with two-pass evaluation for top-level defines
///
/// R5RS semantics: All top-level defines create bindings first, then RHS expressions are evaluated.
/// This allows forward references between defines.
fn evaluate_template(
    evaluator: &mut Evaluator,
    env: gc::Gc<Environment>,
    template: &str,
    line_mappings: &[LineMapping],
) -> Result<()> {
    use dazzle_core::scheme::parser::Token;
    use dazzle_core::scheme::value::Value;

    let mut parser = SchemeParser::new(template);

    // TWO-PASS EVALUATION for R5RS top-level define semantics
    // Pass 1: Parse all expressions and collect defines
    let mut expressions = Vec::new();
    let mut positions = Vec::new();

    loop {
        let peek_result = parser.peek_token();
        match peek_result {
            Ok(tok) if matches!(*tok, Token::Eof) => break,
            Ok(_) => {
                let pos = parser.current_position();
                match parser.parse() {
                    Ok(expr) => {
                        expressions.push(expr);
                        positions.push(pos);
                    }
                    Err(e) => {
                        // Debug: Write preprocessed content to file if SGML_DEBUG is set
                        if std::env::var("SGML_DEBUG").is_ok() {
                            eprintln!("[SGML_DEBUG] Parse error at line {}: {}", e.position.line, e.message);
                            if let Err(write_err) = std::fs::write("/tmp/dazzle_preprocessed_template.scm", template) {
                                eprintln!("[SGML_DEBUG] Failed to write debug file: {}", write_err);
                            } else {
                                eprintln!("[SGML_DEBUG] Preprocessed template written to /tmp/dazzle_preprocessed_template.scm");
                            }
                        }

                        if !line_mappings.is_empty() {
                            if let Some(mapping) = line_mappings.iter().find(|m| m.output_line == e.position.line) {
                                return Err(anyhow::anyhow!(
                                    "{}:{}:{}: {}",
                                    mapping.source_file,
                                    mapping.source_line,
                                    e.position.column,
                                    e.message
                                ));
                            }
                        }
                        return Err(anyhow::anyhow!("Parse error at {}: {}", e.position, e.message));
                    }
                }
            }
            Err(e) => {
                if !line_mappings.is_empty() {
                    if let Some(mapping) = line_mappings.iter().find(|m| m.output_line == e.position.line) {
                        return Err(anyhow::anyhow!(
                            "{}:{}:{}: {}",
                            mapping.source_file,
                            mapping.source_line,
                            e.position.column,
                            e.message
                        ));
                    }
                }
                return Err(anyhow::anyhow!("Tokenizer error at {}: {}", e.position, e.message));
            }
        }
    }

    // Pass 2: Create bindings for all top-level defines with undefined placeholders
    for expr in expressions.iter() {
        if let Value::Pair(_) = expr {
            // Check if it's a (define name ...) form
            if let Ok(list) = evaluator.list_to_vec(expr.clone()) {
                if !list.is_empty() {
                    if let Value::Symbol(ref sym) = list[0] {
                        if sym.as_ref() == "define" && list.len() >= 2 {
                            // Extract the variable name
                            let var_name = match &list[1] {
                                Value::Symbol(name) => Some(name.clone()),
                                Value::Pair(_) => {
                                    // (define (func args...) body)
                                    if let Ok(func_list) = evaluator.list_to_vec(list[1].clone()) {
                                        if !func_list.is_empty() {
                                            if let Value::Symbol(name) = &func_list[0] {
                                                Some(name.clone())
                                            } else {
                                                None
                                            }
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            };

                            if let Some(name) = var_name {
                                // Create binding with unspecified value as placeholder
                                env.define(name.as_ref(), Value::Unspecified);
                            }
                        }
                    }
                }
            }
        }
    }

    // Pass 2.5: Evaluate function defines first (they don't have forward references)
    for (expr, pos) in expressions.iter().zip(positions.iter()) {
        if let Value::Pair(_) = expr {
            if let Ok(list) = evaluator.list_to_vec(expr.clone()) {
                if !list.is_empty() {
                    if let Value::Symbol(ref sym) = list[0] {
                        if sym.as_ref() == "define" && list.len() >= 2 {
                            if let Value::Pair(_) = &list[1] {
                                // Function define: (define (f x) body) - evaluate immediately
                                evaluator.set_position(pos.clone());
                                let _result = evaluator
                                    .eval(expr.clone(), env.clone())
                                    .map_err(|e| anyhow::anyhow!("Evaluation error: {}", e))?;
                            }
                        }
                    }
                }
            }
        }
    }

    // Pass 3: Iteratively evaluate simple variable defines until fixed point
    // This handles forward references by re-evaluating until all resolve
    let mut simple_defines: Vec<(usize, String, Value, dazzle_core::scheme::parser::Position)> = Vec::new();
    let mut non_define_exprs: Vec<(Value, dazzle_core::scheme::parser::Position)> = Vec::new();

    // Collect simple variable defines and non-define expressions
    for (idx, (expr, pos)) in expressions.iter().zip(positions.iter()).enumerate() {
        if let Value::Pair(_) = expr {
            if let Ok(list) = evaluator.list_to_vec(expr.clone()) {
                if !list.is_empty() {
                    if let Value::Symbol(ref sym) = list[0] {
                        if sym.as_ref() == "define" && list.len() >= 2 {
                            match &list[1] {
                                Value::Symbol(name) => {
                                    // Simple define: (define x value)
                                    simple_defines.push((idx, name.to_string(), list[2].clone(), pos.clone()));
                                    continue;
                                }
                                Value::Pair(_) => {
                                    // Function define: (define (f x) body)
                                    // Already evaluated in Pass 2.5, skip it
                                    continue;
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        // Not a simple variable define - evaluate normally later
        non_define_exprs.push((expr.clone(), pos.clone()));
    }

    // Pass 3: Evaluate simple variable defines once, in order
    // This matches OpenJade's behavior: evaluate sequentially, no iteration
    // Variables that can't be evaluated stay as #<unspecified>
    for (_idx, name, rhs_expr, pos) in simple_defines.iter() {
        evaluator.set_position(pos.clone());

        // Try to evaluate the RHS - if it fails, leave as unspecified
        if let Ok(value) = evaluator.eval(rhs_expr.clone(), env.clone()) {
            env.define(name, value);
        }
        // Errors are silently ignored - variable stays as #<unspecified>
        // This matches OpenJade behavior for forward references
    }

    // Pass 4: Evaluate non-define expressions (other top-level forms)
    for (expr, pos) in non_define_exprs {
        evaluator.set_position(pos);
        let _result = evaluator
            .eval(expr, env.clone())
            .map_err(|e| anyhow::anyhow!("Evaluation error: {}", e))?;
    }

    Ok(())
}

/// Resolve SGML character entity references in content
///
/// SGML uses entity references like `&#RE` for special characters.
/// These appear in character literals like `#\&#RE` and need to be resolved.
///
/// Handles two types of entities:
/// 1. Named entities: `&#RE`, `&#RS`, `&#SPACE`
/// 2. Numeric entities: `&#60;` (decimal), `&#x3C;` (hexadecimal)
///
/// Special handling for character literals:
/// - `#\&#RE` becomes `#\newline` (Scheme character literal syntax)
/// - `#\&#RS` becomes `#\return`
/// - `#\&#SPACE` becomes `#\space`
///
/// Outside character literals:
/// - `&#RE` becomes actual newline character
/// - `&#RS` becomes actual carriage return
/// - `&#SPACE` becomes actual space
/// - `&#60;` becomes `<`
/// - `&#38;` becomes `&`
fn resolve_sgml_entities(content: &str) -> String {
    // First, handle character literal contexts (#\&#ENTITY)
    // These need to become Scheme character names, not actual characters
    let mut result = content
        .replace("#\\&#RE", "#\\newline")
        .replace("#\\&#RS", "#\\return")
        .replace("#\\&#SPACE", "#\\space");

    // Then handle entity references outside character literals
    result = result
        .replace("&#RE", "\n")
        .replace("&#RS", "\r")
        .replace("&#SPACE", " ");

    // Now handle numeric character references: &#DIGITS; (decimal) or &#xHEX; (hexadecimal)
    // We need to parse these and replace with the actual character
    let mut final_result = String::with_capacity(result.len());
    let mut chars = result.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '&' && chars.peek() == Some(&'#') {
            // Found potential numeric entity: &#...;
            chars.next(); // consume '#'

            let mut entity_chars = String::new();
            let mut found_semicolon = false;

            // Collect characters until semicolon or invalid character
            while let Some(&next_ch) = chars.peek() {
                if next_ch == ';' {
                    chars.next(); // consume ';'
                    found_semicolon = true;
                    break;
                } else if next_ch.is_ascii_alphanumeric() || next_ch == 'x' {
                    entity_chars.push(next_ch);
                    chars.next();
                } else {
                    // Invalid entity reference, treat as literal text
                    break;
                }
            }

            if found_semicolon && !entity_chars.is_empty() {
                // Try to parse as decimal or hexadecimal
                let code: Option<u32> = if entity_chars.starts_with('x') || entity_chars.starts_with('X') {
                    // Hexadecimal: &#xHEX;
                    u32::from_str_radix(&entity_chars[1..], 16).ok()
                } else {
                    // Decimal: &#DIGITS;
                    entity_chars.parse().ok()
                };

                if let Some(code) = code {
                    if let Some(decoded_ch) = char::from_u32(code) {
                        final_result.push(decoded_ch);
                        continue;
                    }
                }
            }

            // If parsing failed, output the original text
            final_result.push('&');
            final_result.push('#');
            final_result.push_str(&entity_chars);
            if found_semicolon {
                final_result.push(';');
            }
        } else {
            final_result.push(ch);
        }
    }

    final_result
}

/// Expand SGML conditional section markers
///
/// SGML conditional sections like `<![%entity[ content ]]>` are used in DocBook stylesheets
/// for localization. This properly expands them based on entity declarations:
/// - <!ENTITY % l10n-en "INCLUDE"> → keeps content
/// - <!ENTITY % l10n-de "IGNORE"> → removes content
///
/// Example: With `<!ENTITY % l10n-en "INCLUDE">`:
/// `<![%l10n-en[ (define x 1) ]]>` → `(define x 1)`
///
/// If external_entities is provided, those parameter entity definitions are used
/// in addition to any found within the content itself.
fn strip_sgml_conditionals(content: &str, external_entities: Option<&std::collections::HashMap<String, String>>) -> String {
    // Use our proper SGML preprocessing that respects INCLUDE/IGNORE
    let result = if let Some(entities) = external_entities {
        sgml_preprocess::expand_marked_sections_with_entities(content, entities)
    } else {
        sgml_preprocess::expand_marked_sections(content)
    }.unwrap_or_else(|_| content.to_string());

    // Debug: Check if SGML_DEBUG env var is set
    if std::env::var("SGML_DEBUG").is_ok() {
        let before_count = content.matches("<![%").count();
        let after_count = result.matches("<![%").count();
        if before_count > 0 {
            eprintln!("[SGML_DEBUG] Preprocessing: {} -> {} marked sections", before_count, after_count);
            if after_count == before_count {
                eprintln!("[SGML_DEBUG] WARNING: No marked sections were expanded!");
            }
        }
    }

    result
}

/// Extract filename from an entity declaration (SYSTEM or PUBLIC)
///
/// Handles both formats:
/// - `<!ENTITY name SYSTEM "file.scm">`
/// - `<!ENTITY name PUBLIC "ID" "file.scm" CDATA DSSSL>`
fn extract_entity_filename(entity_decl: &str) -> Option<String> {
    // For SYSTEM entities, the filename is in the first quoted string after SYSTEM
    // For PUBLIC entities, the filename is in the second quoted string
    if entity_decl.contains("PUBLIC") {
        // PUBLIC format: <!ENTITY name PUBLIC "public-id" "filename" ...>
        // Find the second quoted string
        if let Some(first_quote) = entity_decl.find('"') {
            if let Some(first_close) = entity_decl[first_quote + 1..].find('"') {
                let after_first = first_quote + 1 + first_close + 1;
                if let Some(second_quote) = entity_decl[after_first..].find('"') {
                    if let Some(second_close) = entity_decl[after_first + second_quote + 1..].find('"') {
                        return Some(entity_decl[after_first + second_quote + 1..after_first + second_quote + 1 + second_close].to_string());
                    }
                }
            }
        }
    } else if entity_decl.contains("SYSTEM") {
        // SYSTEM format: <!ENTITY name SYSTEM "filename">
        // Find the first quoted string after SYSTEM
        if let Some(start) = entity_decl.find('"') {
            if let Some(end) = entity_decl[start + 1..].find('"') {
                return Some(entity_decl[start + 1..start + 1 + end].to_string());
            }
        }
    }
    None
}

/// Resolve XML template wrapper (.dsl format) to plain Scheme code
///
/// Parses entity declarations from DOCTYPE and resolves entity references
/// by loading external .scm files, then concatenates all Scheme code.
///
/// Returns (concatenated_code, line_mappings, external_specs)
fn resolve_xml_template(
    xml_content: &str,
    template_path: &PathBuf,
    _search_dirs: &[PathBuf],
) -> Result<(String, Vec<LineMapping>, Vec<(String, String)>)> {
    use std::collections::HashMap;

    // Extract entity declarations from DOCTYPE
    // Format: <!ENTITY name SYSTEM "file.scm"> or <!ENTITY % name SYSTEM "file.scm">
    let mut entities = HashMap::new();
    let mut parameter_entities = HashMap::new();

    // First pass: collect all entity declarations in the DOCTYPE
    for line in xml_content.lines() {
        let line = line.trim();
        if line.starts_with("<!ENTITY") && line.contains("SYSTEM") {
            // Parse: <!ENTITY syntax SYSTEM "syntax.scm">
            // Or:    <!ENTITY % syntax SYSTEM "syntax.scm"> (parameter entity)
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 4 {
                // Check if this is a parameter entity (has % after ENTITY)
                let (entity_name, is_parameter) = if parts[1] == "%" {
                    // Parameter entity: <!ENTITY % name SYSTEM "file">
                    (parts[2], true)
                } else {
                    // Regular entity: <!ENTITY name SYSTEM "file">
                    (parts[1], false)
                };

                // Extract filename from quotes
                if let Some(start) = line.find('"') {
                    if let Some(end) = line[start + 1..].find('"') {
                        let filename = &line[start + 1..start + 1 + end];
                        if is_parameter {
                            parameter_entities.insert(entity_name.to_string(), filename.to_string());
                        } else {
                            entities.insert(entity_name.to_string(), filename.to_string());
                        }
                    } else {
                        eprintln!("WARNING: Could not find closing quote for entity: {}", entity_name);
                    }
                } else {
                    eprintln!("WARNING: Could not find opening quote for entity on line: {}", line);
                }
            }
        }
    }

    // Second pass: load parameter entity files and parse them for more entity declarations
    // Parameter entities (like dbl10n.ent) contain more <!ENTITY> declarations
    let template_dir = template_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Template path has no parent directory"))?;

    // Global map of SGML parameter entity declarations (<!ENTITY % name "INCLUDE/IGNORE">)
    // These are collected from all parameter entity files and shared across all template files
    let mut sgml_parameter_entities = HashMap::new();

    for (_param_name, param_file) in &parameter_entities {
        let param_path = template_dir.join(param_file);

        match fs::read_to_string(&param_path) {
            Ok(param_content) => {
                // Extract SGML parameter entity declarations from this file
                // (e.g., <!ENTITY % l10n-en "INCLUDE">)
                // These will be available when preprocessing other files
                let local_params = sgml_preprocess::extract_parameter_entities(&param_content);

                sgml_parameter_entities.extend(local_params.clone());

                // Strip SGML conditional sections (e.g., <![%l10n-en[ ... ]]>)
                // Pass the extracted parameter entities so marked sections can be expanded
                let stripped_content = strip_sgml_conditionals(&param_content, Some(&local_params));

                // Parse entity declarations from the parameter entity file
                // Handle multi-line entity declarations by accumulating lines
                let mut current_entity = String::new();
                let mut entity_name = String::new();
                let mut in_entity_decl = false;

                for line in stripped_content.lines() {
                    let line = line.trim();
                    // Skip empty lines
                    if line.is_empty() {
                        continue;
                    }

                    if line.starts_with("<!ENTITY") && !line.contains('%') {
                        // Start of a new entity declaration
                        in_entity_decl = true;
                        current_entity = line.to_string();

                        // Extract entity name from first line
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 2 {
                            entity_name = parts[1].to_string();
                        }

                        // Check if it's a single-line declaration
                        if line.contains('>') {
                            in_entity_decl = false;
                            // Process this entity - but don't overwrite existing declarations
                            // (first declaration wins, so conditional sections take precedence)
                            if let Some(filename) = extract_entity_filename(&current_entity) {
                                if !entities.contains_key(&entity_name) {
                                    entities.insert(entity_name.clone(), filename.clone());
                                }
                            }
                            current_entity.clear();
                        }
                    } else if in_entity_decl {
                        // Continue accumulating multi-line entity declaration
                        current_entity.push(' ');
                        current_entity.push_str(line);

                        // Check if this line ends the declaration
                        if line.contains('>') {
                            in_entity_decl = false;
                            // Process this entity - but don't overwrite existing declarations
                            // (first declaration wins, so conditional sections take precedence)
                            if let Some(filename) = extract_entity_filename(&current_entity) {
                                if !entities.contains_key(&entity_name) {
                                    entities.insert(entity_name.clone(), filename.clone());
                                }
                            }
                            current_entity.clear();
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("WARNING: Could not load parameter entity file {}: {}", param_path.display(), e);
            }
        }
    }

    // Extract external-specification declarations
    // Format: <external-specification id="dbparam" document="dbparam.dsl">
    let mut external_specs = Vec::new();
    for line in xml_content.lines() {
        let line = line.trim();
        if line.starts_with("<external-specification") {
            if let Some(id_start) = line.find("id=\"") {
                if let Some(id_end) = line[id_start + 4..].find('"') {
                    let spec_id = &line[id_start + 4..id_start + 4 + id_end];
                    if let Some(doc_start) = line.find("document=\"") {
                        if let Some(doc_end) = line[doc_start + 10..].find('"') {
                            let spec_file = &line[doc_start + 10..doc_start + 10 + doc_end];
                            external_specs.push((spec_id.to_string(), spec_file.to_string()));
                            debug!("Found external spec: {} -> {}", spec_id, spec_file);
                        }
                    }
                }
            }
        }
    }

    // Extract use= attribute from <style-specification> to load external specs
    // The tag may span multiple lines, so accumulate lines until we find the closing >
    let mut use_specs = Vec::new();
    let mut inside_style_spec_tag = false;
    let mut accumulated_tag = String::new();

    for line in xml_content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with("<style-specification ") {
            inside_style_spec_tag = true;
            accumulated_tag.push_str(trimmed);
            accumulated_tag.push(' ');
        } else if inside_style_spec_tag {
            accumulated_tag.push_str(trimmed);
            accumulated_tag.push(' ');
        }

        if inside_style_spec_tag && accumulated_tag.contains('>') {
            // Found the complete tag, now parse the use= attribute
            if let Some(use_start) = accumulated_tag.find("use=\"") {
                if let Some(use_end) = accumulated_tag[use_start + 5..].find('"') {
                    let use_value = &accumulated_tag[use_start + 5..use_start + 5 + use_end];
                    // Split by whitespace to get individual spec IDs
                    use_specs = use_value.split_whitespace().map(|s| s.to_string()).collect();
                }
            }
            break;
        }
    }

    // Build result by resolving entity references
    let mut result = String::new();
    let mut line_mappings = Vec::new();
    let mut current_output_line = 1;
    let mut inside_body = false;
    let mut past_doctype = false;
    let mut inside_xml_tag = false;  // Track multi-line XML tags

    // Load external specifications referenced in use= attribute
    for spec_id in use_specs.iter() {
        // Try to find the spec file:
        // 1. Check if there's an entity named dbl1{spec_id} (for DocBook localization)
        // 2. Check if there's an <external-specification> declaration
        // 3. Try search paths
        let spec_file = if let Some(entity_file) = entities.get(&format!("dbl1{}", spec_id)) {
            entity_file.clone()
        } else if let Some((_id, spec_doc)) = external_specs.iter().find(|(id, _)| id == spec_id) {
            // The document= attribute may reference an entity name, resolve it first
            if let Some(entity_file) = entities.get(spec_doc.as_str()) {
                entity_file.clone()
            } else {
                // Not an entity, use as filename directly
                spec_doc.clone()
            }
        } else {
            // No entity or external-specification found - skip
            continue;
        };

        // Try multiple paths to find the spec file:
        // 1. Relative to template dir
        // 2. Relative to stylesheet root (../lib/ for DocBook)
        // 3. In search paths
        let mut spec_path = template_dir.join(&spec_file);
        if !spec_path.exists() {
            // Try ../lib/ for DocBook-style layout
            spec_path = template_dir.join("../lib").join(&spec_file);
        }
        if !spec_path.exists() {
            // Try search paths
            for search_dir in _search_dirs {
                let try_path = search_dir.join(&spec_file);
                if try_path.exists() {
                    spec_path = try_path;
                    break;
                }
            }
        }


        if let Ok(spec_bytes) = fs::read(&spec_path) {
                // Try UTF-8 first, then Latin-1
                let mut spec_content = if let Ok(utf8_str) = String::from_utf8(spec_bytes.clone()) {
                    utf8_str
                } else {
                    spec_bytes.iter().map(|&b| b as char).collect()
                };

                // Resolve SGML character entity references (&#RE, &#RS, etc.)
                spec_content = resolve_sgml_entities(&spec_content);

                // Strip SGML conditional sections (e.g., <![%l10n-en[ ... ]]>)
                // Pass the collected parameter entities from .ent files
                spec_content = strip_sgml_conditionals(&spec_content, Some(&sgml_parameter_entities));

                // Recursively resolve external spec if it's XML-wrapped
                let (resolved_content, mut resolved_mappings, _) = if spec_content.contains("<style-sheet>") || spec_content.contains("<style-specification>") {
                    // It's an XML-wrapped template, recursively resolve it
                    resolve_xml_template(&spec_content, &spec_path, _search_dirs)?
                } else {
                    // Plain DSSSL code, use as-is
                    let source_file = spec_path.to_string_lossy().to_string();
                    let mut mappings = Vec::new();
                    for (line_idx, _) in spec_content.lines().enumerate() {
                        mappings.push(LineMapping {
                            output_line: line_idx + 1,
                            source_file: source_file.clone(),
                            source_line: line_idx + 1,
                        });
                    }
                    (spec_content, mappings, Vec::new())
                };

                // Adjust line mappings to account for lines already in result
                for mapping in resolved_mappings.iter_mut() {
                    mapping.output_line += current_output_line - 1;
                }

                current_output_line += resolved_content.lines().count();
                line_mappings.extend(resolved_mappings);
                result.push_str(&resolved_content);
                result.push('\n'); // Add separator between specs
            } else {
            }
    }

    for line in xml_content.lines() {
        let trimmed = line.trim();

        // Track when DOCTYPE declaration ends (marked by ]> on its own line)
        // Only match lines that are JUST "]>" (not lines containing ]> in strings)
        if trimmed == "]>" {
            past_doctype = true;
            continue;
        }

        // Track when we enter/exit <style-specification-body> or <style-specification>
        // Note: Don't include '>' in the opening tag check because tags may have attributes
        if trimmed.starts_with("<style-specification-body") || trimmed.starts_with("<style-specification ") || trimmed == "<style-specification>" {
            inside_body = true;
            // Only set inside_xml_tag if the line doesn't end with '>' (multi-line tag)
            // This prevents skipping entity references after single-line tags like <style-specification-body>
            if !trimmed.ends_with('>') {
                inside_xml_tag = true;
            }
            continue;
        }
        if trimmed.starts_with("</style-specification-body>") || trimmed.starts_with("</style-specification>") {
            inside_body = false;
            continue;
        }

        // If we're inside a multi-line XML tag, skip until we see '>'
        if inside_xml_tag {
            if trimmed.contains('>') {
                inside_xml_tag = false;
            }
            continue;
        }

        // Skip other XML tags (like <style-sheet>, </style-sheet>)
        // BUT do NOT skip CDATA markers <![CDATA[ - they need to be processed
        if trimmed.starts_with('<') && !trimmed.starts_with("<![CDATA[") && trimmed != "" {
            continue;
        }

        // Process content when:
        // 1. Inside <style-specification-body> or <style-specification>, OR
        // 2. Past DOCTYPE and not inside any XML tags (root-level entity references)
        if !inside_body && !past_doctype {
            continue;
        }

        // Skip empty lines and comments
        if trimmed.is_empty() || trimmed.starts_with("<!--") {
            continue;
        }

        // Check for entity reference: &name; (may have trailing comment)
        if trimmed.starts_with('&') {
            // Extract entity reference, ignoring any trailing comment
            if let Some(end_pos) = trimmed.find(';') {
                let entity_name = &trimmed[1..end_pos];

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
                        // IMPORTANT: Strip SGML conditional section markers BEFORE stripping CDATA markers!
                        // SGML marked sections like <![%l10n-en[ ... ]]> also use ]]> as closing delimiter,
                        // so if we strip ]]> first, we'll break the marked sections.
                        //
                        // DocBook stylesheets use these for localization: <![%l10n-en[ ... ]]>
                        // Pass the collected parameter entities from .ent files so marked sections
                        // in .dsl files can be expanded correctly
                        // Pattern: <![%entity-name[  ->  empty string (if INCLUDE) or removed (if IGNORE)
                        content = strip_sgml_conditionals(&content, Some(&sgml_parameter_entities));

                        // Strip CDATA wrappers if present
                        // Some .scm files are wrapped in <![CDATA[...]]> for XML embedding
                        // IMPORTANT: Don't use trim() - it changes line counts!
                        if content.starts_with("<![CDATA[") {
                            content = content[9..].to_string(); // Strip "<![CDATA["

                            // Strip ]]> suffix - handle Windows (CRLF), Unix (LF), and no newline
                            if content.ends_with("]]>\r\n") {
                                content = content[..content.len() - 5].to_string(); // Strip "]]>\r\n"
                                content.push_str("\r\n"); // Keep the final newline for correct line counting
                            } else if content.ends_with("]]>\n") {
                                content = content[..content.len() - 4].to_string(); // Strip "]]>\n"
                                content.push('\n'); // Keep the final newline for correct line counting
                            } else if content.ends_with("]]>") {
                                content = content[..content.len() - 3].to_string(); // Strip "]]>"
                            }
                        }
                        // After stripping CDATA markers, line numbers are preserved:
                        // - Line 1 of stripped content = Line 1 of original file (content after <![CDATA[)
                        // - The ]]> line is removed, keeping line numbers aligned with the original file

                        // Now strip inline CDATA markers (common in .scm files that embed XML content)
                        // These appear as: ($<![CDATA[ ... ]]>)
                        // We replace COMPLETE CDATA sections, not just the markers independently
                        // This prevents removing ]]> closings that belong to marked sections
                        let cdata_re = Regex::new(r"<!\[CDATA\[(.*?)\]\]>").unwrap();
                        content = cdata_re.replace_all(&content, "$1").to_string();

                        // Resolve SGML character entity references (&#RE, &#RS, etc.)
                        content = resolve_sgml_entities(&content);

                        // Track line mappings for this entity file
                        let source_file = entity_path.to_string_lossy().to_string();
                        for (source_line_idx, _) in content.lines().enumerate() {
                            line_mappings.push(LineMapping {
                                output_line: current_output_line,
                                source_file: source_file.clone(),
                                source_line: source_line_idx + 1, // 1-indexed line numbers
                            });
                            current_output_line += 1;
                        }

                        result.push_str(&content);
                        // Don't add extra newline - it creates a line offset in the concatenated output
                    }
                    Err(e) => {
                        return Err(anyhow::anyhow!(
                            "Failed to load entity file {}: {}",
                            entity_path.display(),
                            e
                        ));
                    }
                }
                } else {
                    // Entity reference not found in declared entities - skip it
                    eprintln!("[WARNING] Undeclared entity reference: {}", entity_name);
                }
            }
        } else {
            // Not an entity reference - this is inline Scheme code
            // Process the line to remove SGML markers
            let mut processed_line = line.to_string();

            // Strip CDATA markers (may appear in inline code)
            // Only strip complete CDATA sections, not individual ]]> which may belong to marked sections
            let cdata_re = Regex::new(r"<!\[CDATA\[(.*?)\]\]>").unwrap();
            processed_line = cdata_re.replace_all(&processed_line, "$1").to_string();

            // Add it to the result
            result.push_str(&processed_line);
            result.push('\n');

            // Track line mapping for inline code
            line_mappings.push(LineMapping {
                output_line: current_output_line,
                source_file: template_path.to_string_lossy().to_string(),
                source_line: current_output_line, // Direct 1:1 mapping for inline code
            });
            current_output_line += 1;
        }
    }

    if result.is_empty() {
        anyhow::bail!("No Scheme code extracted from XML template");
    }

    // Apply SGML preprocessing to the final assembled template
    // This catches any SGML marked sections in inline code that weren't preprocessed during entity loading
    let result = strip_sgml_conditionals(&result, Some(&sgml_parameter_entities));

    Ok((result, line_mappings, external_specs))
}
