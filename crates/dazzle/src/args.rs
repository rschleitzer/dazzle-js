//! Command-line argument parsing

use clap::Parser;
use std::path::PathBuf;

/// Dazzle: A Rust-based code generation tool powered by Scheme templates
#[derive(Parser, Debug)]
#[command(name = "dazzle")]
#[command(about, long_about = None, version)]
#[command(disable_version_flag = true)]
pub struct Args {
    /// Print version information
    #[arg(long = "version")]
    pub version: bool,
    /// Template file (.scm)
    #[arg(short = 'd', long = "template", required_unless_present = "version")]
    pub template: Option<PathBuf>,

    /// Backend selection (text, xml, or rtf)
    #[arg(short = 't', long = "backend", default_value = "text")]
    pub backend: String,

    /// Template variables (key=value pairs)
    #[arg(short = 'V', long = "var", value_parser = parse_key_val)]
    pub variables: Vec<(String, String)>,

    /// Template search directories
    #[arg(short = 'D', long = "dir")]
    pub search_dirs: Vec<PathBuf>,

    /// Input XML file
    #[arg(required_unless_present = "version")]
    pub input: Option<PathBuf>,

    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,
}

/// Parse a single key=value pair
///
/// Supports OpenJade-compatible syntax:
/// - `key=value` - sets key to value
/// - `key` - sets key to "#t" (boolean true)
fn parse_key_val(s: &str) -> Result<(String, String), String> {
    if let Some(pos) = s.find('=') {
        Ok((s[..pos].to_string(), s[pos + 1..].to_string()))
    } else {
        // OpenJade compatibility: `-V name` sets name to #t
        Ok((s.to_string(), "#t".to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key_val() {
        let (k, v) = parse_key_val("foo=bar").unwrap();
        assert_eq!(k, "foo");
        assert_eq!(v, "bar");
    }

    #[test]
    fn test_parse_key_val_with_equals_in_value() {
        let (k, v) = parse_key_val("key=val=ue").unwrap();
        assert_eq!(k, "key");
        assert_eq!(v, "val=ue");
    }
}
