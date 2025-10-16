//! Skeme CLI entry point

use anyhow::{Context, Result};
use clap::Parser;
use skeme_cli::Args;
use skeme_core::{SchemeEngine, XmlParser};
use skeme_template::{TemplateLoader, TemplateProcessor};
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

    info!("Skeme v{}", skeme_core::VERSION);
    info!("Template: {}", args.template.display());
    info!("Input: {}", args.input.display());

    // Phase 1 implementation will go here:
    // 1. Parse XML with DTD validation
    // 2. Create Scheme engine with primitives
    // 3. Set variables from CLI
    // 4. Load template
    // 5. Process template
    // 6. Generate output

    // For now, just a placeholder
    info!("Phase 1: Foundation - Implementation in progress");

    println!("Skeme CLI initialized successfully!");
    println!("Template: {}", args.template.display());
    println!("Input: {}", args.input.display());
    println!("Variables: {:?}", args.variables);

    Ok(())
}
