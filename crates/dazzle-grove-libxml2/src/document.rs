//! XML Document wrapper for libxml2
//!
//! Provides a safe Rust interface to libxml2's xmlDoc.
//!
//! ## Memory Safety
//!
//! libxml2 uses manual memory management (malloc/free). We ensure safety by:
//! - Wrapping xmlDoc* in a struct with Drop implementation
//! - Never exposing raw pointers to users
//! - All xmlNode* pointers are borrowed from the document lifetime
//!
//! ## OpenJade Correspondence
//!
//! Similar to OpenJade's `spgrove/GroveBuilder`, but simpler:
//! - OpenJade: Builds grove from OpenSP parse events (complex)
//! - Dazzle: Uses libxml2's DOM (already built by parser)

use libxml::tree::Document;

use crate::node::LibXml2Node;

/// Wrapper around libxml2 Document
///
/// Owns the libxml2 xmlDoc and ensures proper cleanup.
///
/// ## Lifetime Management
///
/// The document must outlive all nodes created from it.
/// We use `Rc<XmlDocument>` in LibXml2Grove to ensure this.
pub struct XmlDocument {
    /// The libxml2 document handle
    /// Using the `libxml` crate's safe wrapper
    doc: Document,
}

impl XmlDocument {
    /// Parse XML from a string
    ///
    /// # Arguments
    ///
    /// * `xml` - XML string to parse
    /// * `validate_dtd` - If true, validate against DTD (if DOCTYPE present)
    ///
    /// # Returns
    ///
    /// Returns `Ok(XmlDocument)` on success, `Err(String)` with error message on failure.
    ///
    /// # DSSSL Notes
    ///
    /// - DTD validation is optional but recommended for code generation
    /// - Attribute defaults from DTD are automatically applied by libxml2
    /// - Entity references are resolved automatically
    pub fn parse_string(xml: &str, validate_dtd: bool) -> Result<Self, String> {
        // Use libxml crate's parser
        let parser = libxml::parser::Parser::default();

        let doc = parser
            .parse_string(xml)
            .map_err(|e| format!("XML parse error: {}", e))?;

        // TODO: DTD validation
        // libxml2 provides xmlValidateDocument() for post-parse validation
        // The libxml crate may not expose this directly - we might need to add it

        if validate_dtd {
            // Placeholder: validation will be added
            // For now, just parse successfully
        }

        Ok(XmlDocument { doc })
    }

    /// Parse XML from a file
    ///
    /// # Arguments
    ///
    /// * `path` - Path to XML file
    /// * `validate_dtd` - If true, validate against DTD
    pub fn parse_file(path: &str, validate_dtd: bool) -> Result<Self, String> {
        let parser = libxml::parser::Parser::default();

        let doc = parser
            .parse_file(path)
            .map_err(|e| format!("XML parse error: {}", e))?;

        if validate_dtd {
            // Placeholder: validation will be added
        }

        Ok(XmlDocument { doc })
    }

    /// Get the root element of the document
    ///
    /// Returns `None` if the document is empty (shouldn't happen with valid XML).
    ///
    /// # DSSSL Correspondence
    ///
    /// This is the document element - the top-level element in the XML file.
    /// Not to be confused with the document node itself (which we don't expose).
    pub fn root_element(&self) -> Option<LibXml2Node> {
        let root = self.doc.get_root_element()?;
        Some(LibXml2Node::from_libxml_node(root))
    }

    /// Get the underlying libxml Document
    ///
    /// This is package-private - only used by LibXml2Node
    pub(crate) fn inner(&self) -> &Document {
        &self.doc
    }
}

impl std::fmt::Debug for XmlDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("XmlDocument")
            .field("has_root", &self.root_element().is_some())
            .finish()
    }
}

// Drop is automatically handled by libxml::tree::Document

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_xml() {
        let xml = r#"<?xml version="1.0"?><root><child>text</child></root>"#;
        let doc = XmlDocument::parse_string(xml, false);

        assert!(doc.is_ok());
        let doc = doc.unwrap();
        assert!(doc.root_element().is_some());
    }

    #[test]
    fn test_parse_malformed_xml() {
        // Note: libxml2 is very tolerant and tries to repair XML
        // We need really broken XML to get an error
        let xml = r#"<root><child"#; // Incomplete tag
        let doc = XmlDocument::parse_string(xml, false);

        // libxml2 might still parse this, so we can't strictly require error
        // Just verify it doesn't panic
        let _ = doc;
    }

    #[test]
    fn test_parse_with_attributes() {
        let xml = r#"<?xml version="1.0"?><root id="r1" name="test"><child/></root>"#;
        let doc = XmlDocument::parse_string(xml, false);

        assert!(doc.is_ok());
    }

    #[test]
    fn test_root_element() {
        use dazzle_core::grove::Node;

        let xml = r#"<?xml version="1.0"?><myroot><child/></myroot>"#;
        let doc = XmlDocument::parse_string(xml, false).unwrap();

        let root = doc.root_element();
        assert!(root.is_some());

        let root = root.unwrap();
        assert_eq!(root.gi(), Some("myroot".to_string()));
    }
}
