//! Dazzle CLI entry point

use anyhow::{Context, Result};
use clap::Parser;
use dazzle_cli::Args;
use dazzle_core::{SchemeEngine, XmlParser};
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    // Parse command-line arguments
    let args = Args::parse();

    // Set up logging
    let log_level = if args.verbose { Level::DEBUG } else { Level::INFO };
    let subscriber = FmtSubscriber::builder()
        .with_max_level(log_level)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .context("Failed to set up logging")?;

    info!("Dazzle v{}", dazzle_core::VERSION);
    info!("Template: {}", args.template.display());
    info!("Input: {}", args.input.display());

    // Step 1: Parse XML with DTD validation
    info!("Parsing XML input: {}", args.input.display());
    let parser = XmlParser::new();
    let grove = parser
        .parse_file(&args.input)
        .context("Failed to parse XML input")?;
    info!("XML parsed successfully");

    // Step 2: Create Scheme engine with primitives
    info!("Initializing Scheme engine");
    let mut engine = SchemeEngine::new().context("Failed to create Scheme engine")?;

    // Step 2.5: Add search directories from CLI
    for dir in &args.search_dirs {
        info!("Adding search directory: {}", dir.display());
        engine.add_search_path(dir.clone());
    }

    // Step 2.6: Add template's directory to search paths automatically
    if let Some(template_dir) = args.template.parent() {
        info!("Adding template directory to search paths: {}", template_dir.display());
        engine.add_search_path(template_dir.to_path_buf());
    }

    // Step 3: Set variables from CLI
    for (name, value) in &args.variables {
        info!("Setting variable: {} = {}", name, value);
        engine.set_variable(name.clone(), value.clone());
    }

    // Step 4: Load template FIRST (registers construction rules)
    info!("Loading template: {}", args.template.display());
    engine
        .load_file(args.template.to_str().unwrap())
        .context("Failed to load template")?;
    info!("Template loaded successfully");

    // Step 5: Set grove AFTER loading template (so it doesn't get reset)
    info!("Grove root element: {}", grove.root().gi());
    engine
        .set_current_grove(grove)
        .context("Failed to set current grove")?;
    info!("Grove root registered as 'current-root' in Scheme");

    // Step 6: Start processing (call process-root)
    info!("Starting template processing");
    engine
        .eval("(process-root)")
        .context("Failed to process document")?;
    info!("Processing complete");

    println!("\nDazzle processing complete!");
    println!("Template: {}", args.template.display());
    println!("Input: {}", args.input.display());
    if !args.variables.is_empty() {
        println!("Variables: {:?}", args.variables);
    }

    Ok(())
}
