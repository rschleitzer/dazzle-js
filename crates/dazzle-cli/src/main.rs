//! Dazzle CLI entry point
//!
//! **Status**: Placeholder - full CLI implementation comes in Phase 6
//!
//! The final CLI will:
//! - Parse command-line arguments (-d, -V, -D, -t)
//! - Load and validate XML with DTD
//! - Initialize Scheme interpreter
//! - Load template files
//! - Process document and generate output

fn main() {
    println!("Dazzle v{}", dazzle_core::VERSION);
    println!();
    println!("Status: Core architecture defined (Phase 1)");
    println!("Next:   Scheme interpreter implementation (Phase 2)");
    println!();
    println!("Usage: dazzle -d TEMPLATE.scm [-V key=value]... INPUT.xml");
    println!();
    println!("This is a placeholder. Full CLI coming in Phase 6.");
}
