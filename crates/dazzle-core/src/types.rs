//! DSSSL type system
//!
//! Defines Rust types for DSSSL datatypes (mostly stubs for code generation use case).
//!
//! Types defined:
//! - Quantity (12pt, 2em, etc.) - STUB
//! - Color (RGB, CMYK) - STUB
//! - Address (links, cross-references) - STUB
//! - GlyphId (special characters) - STUB

use std::fmt;

/// DSSSL Quantity (length with unit)
///
/// STUB: Not needed for plain text code generation
#[derive(Debug, Clone)]
pub struct Quantity {
    value: f64,
    unit: String,
}

impl Quantity {
    pub fn new(value: f64, unit: impl Into<String>) -> Self {
        Self {
            value,
            unit: unit.into(),
        }
    }

    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn unit(&self) -> &str {
        &self.unit
    }
}

impl fmt::Display for Quantity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.value, self.unit)
    }
}

/// DSSSL Color
///
/// STUB: Not needed for plain text code generation
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    pub fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

/// DSSSL Address (for links and cross-references)
///
/// STUB: Rarely used in code generation
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Address {
    target: String,
}

impl Address {
    pub fn new(target: impl Into<String>) -> Self {
        Self {
            target: target.into(),
        }
    }
}

/// DSSSL GlyphId (special character representation)
///
/// STUB: Rarely used in code generation
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct GlyphId {
    name: String,
}

impl GlyphId {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quantity() {
        let q = Quantity::new(12.0, "pt");
        assert_eq!(q.value(), 12.0);
        assert_eq!(q.unit(), "pt");
        assert_eq!(q.to_string(), "12pt");
    }

    #[test]
    fn test_color() {
        let c = Color::rgb(255, 0, 0);
        assert!(matches!(c, Color { r: 255, g: 0, b: 0 }));
    }
}
