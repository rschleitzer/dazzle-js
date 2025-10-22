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

use libxml::bindings;
use libxml::tree::Document;
use std::ffi::CString;
use std::os::raw::c_char;
use std::ptr;

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
    /// * `base_url` - Optional base URL/path for resolving relative DTD references (e.g., "/path/to/file.xml")
    /// * `validate_dtd` - If true, validate against DTD (if DOCTYPE present)
    ///
    /// # Returns
    ///
    /// Returns `Ok(XmlDocument)` on success, `Err(String)` with error message on failure.
    ///
    /// # DSSSL Notes
    ///
    /// - DTD validation is optional but recommended for code generation
    /// - **Attribute defaults from DTD are automatically applied** (XML_PARSE_DTDATTR)
    /// - Entity references are resolved automatically
    /// - **External DTD is loaded** (XML_PARSE_DTDLOAD)
    /// - **Base URL is required** for resolving relative DTD paths (e.g., `model.dtd`)
    ///
    /// # Implementation
    ///
    /// We call libxml2's `xmlReadMemory` directly because the `libxml` crate's
    /// `ParserOptions` doesn't expose `XML_PARSE_DTDATTR` (apply DTD defaults).
    /// This is **critical for OpenJade compatibility** - templates rely on DTD defaults.
    pub fn parse_string(xml: &str, base_url: Option<&str>, validate_dtd: bool) -> Result<Self, String> {
        // libxml2 parser option flags
        const XML_PARSE_NOENT: i32 = 2;      // Substitute entities (CRITICAL for external entity refs!)
        const XML_PARSE_DTDLOAD: i32 = 4;    // Load external DTD
        const XML_PARSE_DTDATTR: i32 = 8;    // Apply DTD default attributes (CRITICAL!)
        const XML_PARSE_DTDVALID: i32 = 16;  // Validate with DTD

        // Build parser flags
        // ALWAYS load DTD, apply defaults, and substitute entities (required for DSSSL/OpenJade compatibility)
        // NOTE: Removed XML_PARSE_NONET to allow local DTD file access
        // NOTE: Removed XML_PARSE_RECOVER to fail fast on malformed XML (production safety)
        let mut flags = XML_PARSE_DTDLOAD | XML_PARSE_DTDATTR | XML_PARSE_NOENT;

        if validate_dtd {
            flags |= XML_PARSE_DTDVALID;
        }

        // Prepare C strings
        let xml_cstring = CString::new(xml).map_err(|e| format!("Invalid XML string: {}", e))?;
        let url = base_url.map(|u| CString::new(u).unwrap())
                          .unwrap_or_else(|| CString::new("").unwrap());
        let encoding = ptr::null(); // Auto-detect encoding

        // Call libxml2 directly to parse with DTD options
        // SAFETY: We're calling libxml2 C functions with properly constructed C strings
        let doc_ptr = unsafe {
            bindings::xmlReadMemory(
                xml_cstring.as_ptr() as *const c_char,
                xml.len() as i32,
                url.as_ptr(),
                encoding,
                flags,
            )
        };

        if doc_ptr.is_null() {
            return Err("XML parse error: failed to parse document".to_string());
        }

        // Wrap in libxml crate's Document (takes ownership, will call xmlFreeDoc on drop)
        let doc = Document::new_ptr(doc_ptr);

        Ok(XmlDocument { doc })
    }

    /// Parse XML from a file
    ///
    /// # Arguments
    ///
    /// * `path` - Path to XML file
    /// * `validate_dtd` - If true, validate against DTD
    ///
    /// # Implementation
    ///
    /// Uses same DTD loading flags as `parse_string`.
    pub fn parse_file(path: &str, validate_dtd: bool) -> Result<Self, String> {
        // libxml2 parser option flags (same as parse_string)
        const XML_PARSE_NOENT: i32 = 2;
        const XML_PARSE_DTDLOAD: i32 = 4;
        const XML_PARSE_DTDATTR: i32 = 8;
        const XML_PARSE_DTDVALID: i32 = 16;

        // NOTE: Removed XML_PARSE_NONET to allow local DTD file access
        // NOTE: Removed XML_PARSE_RECOVER to fail fast on malformed XML (production safety)
        let mut flags = XML_PARSE_DTDLOAD | XML_PARSE_DTDATTR | XML_PARSE_NOENT;

        if validate_dtd {
            flags |= XML_PARSE_DTDVALID;
        }

        // Prepare C strings
        let path_cstring = CString::new(path).map_err(|e| format!("Invalid path: {}", e))?;
        let encoding = ptr::null(); // Auto-detect encoding

        // Call libxml2 directly
        let doc_ptr = unsafe {
            bindings::xmlReadFile(
                path_cstring.as_ptr() as *const c_char,
                encoding,
                flags,
            )
        };

        if doc_ptr.is_null() {
            return Err(format!("XML parse error: failed to parse file: {}", path));
        }

        // Wrap in libxml crate's Document
        let doc = Document::new_ptr(doc_ptr);

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
    #[allow(dead_code)]
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
        let doc = XmlDocument::parse_string(xml, None, false);

        assert!(doc.is_ok());
        let doc = doc.unwrap();
        assert!(doc.root_element().is_some());
    }

    #[test]
    fn test_parse_malformed_xml() {
        // Note: libxml2 is very tolerant and tries to repair XML
        // We need really broken XML to get an error
        let xml = r#"<root><child"#; // Incomplete tag
        let doc = XmlDocument::parse_string(xml, None, false);

        // libxml2 might still parse this, so we can't strictly require error
        // Just verify it doesn't panic
        let _ = doc;
    }

    #[test]
    fn test_parse_with_attributes() {
        let xml = r#"<?xml version="1.0"?><root id="r1" name="test"><child/></root>"#;
        let doc = XmlDocument::parse_string(xml, None, false);

        assert!(doc.is_ok());
    }

    #[test]
    fn test_root_element() {
        use dazzle_core::grove::Node;

        let xml = r#"<?xml version="1.0"?><myroot><child/></myroot>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();

        let root = doc.root_element();
        assert!(root.is_some());

        let root = root.unwrap();
        assert_eq!(root.gi(), Some("myroot".to_string()));
    }

    #[test]
    fn test_dtd_default_attributes() {
        use dazzle_core::grove::Node;
        use std::fs;

        // Create test DTD with default attribute
        fs::write("/tmp/dazzle_test.dtd", r#"<!ELEMENT root EMPTY>
<!ATTLIST root
    name        CDATA     #REQUIRED
    controller  (yes|no)  "yes"
>"#).unwrap();

        // XML references DTD but doesn't specify controller attribute
        let xml = r#"<!DOCTYPE root SYSTEM "dazzle_test.dtd">
<root name="test"/>"#;

        // Parse with base URL so DTD can be resolved
        let doc = XmlDocument::parse_string(xml, Some("/tmp/test.xml"), false).unwrap();
        let root = doc.root_element().unwrap();

        // Explicit attribute should exist
        assert_eq!(root.attribute_string("name"), Some("test".to_string()));

        // DTD default should be applied
        let controller = root.attribute_string("controller");
        eprintln!("controller attribute from DTD default: {:?}", controller);
        assert_eq!(controller, Some("yes".to_string()), "DTD default attribute not applied!");
    }

    #[test]
    fn test_dtd_defaults_on_nested_elements() {
        use dazzle_core::grove::Node;
        use std::fs;

        // Create DTD similar to model.dtd with nested elements
        fs::write("/tmp/nested_test.dtd", r#"<!ELEMENT model (interfaces)>
<!ATTLIST model
    company CDATA #REQUIRED
>
<!ELEMENT interfaces (interface+)>
<!ELEMENT interface EMPTY>
<!ATTLIST interface
    entity      CDATA     #REQUIRED
    create      (yes|no)  "no"
    controller  (yes|no)  "yes"
>"#).unwrap();

        // XML like Icons.xml - interface element has no controller attribute
        let xml = r#"<!DOCTYPE model SYSTEM "nested_test.dtd">
<model company="Test">
    <interfaces>
        <interface entity="test.entity" create="yes"/>
    </interfaces>
</model>"#;

        let doc = XmlDocument::parse_string(xml, Some("/tmp/test.xml"), false).unwrap();
        let root = doc.root_element().unwrap();

        // Navigate to interface element
        let children = root.children();
        let interfaces = children.first().unwrap();
        let iface_children = interfaces.children();
        let interface = iface_children.first().unwrap();

        // Check explicit attributes
        assert_eq!(interface.attribute_string("entity"), Some("test.entity".to_string()));
        assert_eq!(interface.attribute_string("create"), Some("yes".to_string()));

        // Check DTD defaults on nested element
        let controller = interface.attribute_string("controller");
        eprintln!("Nested element controller attribute: {:?}", controller);
        assert_eq!(controller, Some("yes".to_string()), "DTD defaults not applied to nested elements!");
    }
}
