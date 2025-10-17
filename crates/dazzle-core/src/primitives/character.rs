//! Character primitives
//!
//! DSSSL character operations including case conversion.
//! These are simpler than full DSSSL language support - we just use
//! Rust's built-in Unicode character operations.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all character primitives
pub fn register_character_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // Character case conversion
    engine.register_fn("char-upcase", char_upcase);
    engine.register_fn("char-downcase", char_downcase);

    Ok(())
}

/// Convert a character to uppercase
/// Usage: (char-upcase #\a) => #\A
fn char_upcase(ch: char) -> char {
    ch.to_ascii_uppercase()
}

/// Convert a character to lowercase
/// Usage: (char-downcase #\A) => #\a
fn char_downcase(ch: char) -> char {
    ch.to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char_upcase() {
        assert_eq!(char_upcase('a'), 'A');
        assert_eq!(char_upcase('z'), 'Z');
        assert_eq!(char_upcase('A'), 'A'); // Already uppercase
    }

    #[test]
    fn test_char_downcase() {
        assert_eq!(char_downcase('A'), 'a');
        assert_eq!(char_downcase('Z'), 'z');
        assert_eq!(char_downcase('a'), 'a'); // Already lowercase
    }
}
