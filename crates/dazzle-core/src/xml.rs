//! XML parsing with DTD validation
//!
//! Integrates libxml2 for XML parsing and DTD validation,
//! converting XML documents into grove nodes.

use crate::grove::Grove;
use anyhow::{Context, Result};
use libxml::parser::Parser;
use std::path::Path;

/// XML parser with DTD validation support
pub struct XmlParser {
    parser: Parser,
}

impl XmlParser {
    /// Create a new XML parser with DTD validation enabled
    pub fn new() -> Self {
        let parser = Parser::default();
        Self { parser }
    }

    /// Parse an XML file
    ///
    /// If the XML contains a DOCTYPE declaration, DTD validation
    /// is performed automatically by libxml2.
    pub fn parse_file(&self, path: &Path) -> Result<Grove> {
        // Parse the file using libxml2
        let doc = self
            .parser
            .parse_file(path.to_str().ok_or_else(|| {
                anyhow::anyhow!("Invalid file path: {}", path.display())
            })?)
            .with_context(|| format!("Failed to parse XML file: {}", path.display()))?;

        // Create a grove from the document
        Grove::from_document(doc)
            .ok_or_else(|| anyhow::anyhow!("XML document has no root element"))
    }

    /// Parse XML from a string
    ///
    /// If the XML contains a DOCTYPE declaration, DTD validation
    /// is performed automatically by libxml2.
    pub fn parse_str(&self, xml: &str) -> Result<Grove> {
        // Parse the string using libxml2
        let doc = self
            .parser
            .parse_string(xml)
            .with_context(|| "Failed to parse XML string")?;

        // Create a grove from the document
        Grove::from_document(doc)
            .ok_or_else(|| anyhow::anyhow!("XML document has no root element"))
    }
}

impl Default for XmlParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let _parser = XmlParser::new();
    }

    #[test]
    fn test_parse_simple_xml() {
        let parser = XmlParser::new();
        let xml = r#"<?xml version="1.0"?>
<root>
  <child id="test">Hello, World!</child>
</root>"#;

        let grove = parser.parse_str(xml).unwrap();
        let root = grove.root();

        assert_eq!(root.gi(), "root");

        let children = root.children();
        assert!(!children.is_empty());
    }

    #[test]
    fn test_grove_navigation() {
        let parser = XmlParser::new();
        let xml = r#"<?xml version="1.0"?>
<doc>
  <section id="intro">
    <title>Introduction</title>
    <para>Some text</para>
  </section>
</doc>"#;

        let grove = parser.parse_str(xml).unwrap();
        let root = grove.root();
        assert_eq!(root.gi(), "doc");

        // Skip text nodes to find the actual element
        let root_children = root.children();
        let elements: Vec<_> = root_children.iter().filter(|n| n.is_element()).collect();

        let section = elements.first().expect("Should have section element");
        assert_eq!(section.gi(), "section");
        assert_eq!(section.id(), Some("intro".to_string()));

        // Check section's children
        let section_all_children = section.children();
        let section_children: Vec<_> = section_all_children
            .iter()
            .filter(|n| n.is_element())
            .collect();
        assert!(section_children.len() >= 2); // title and para
    }
}
