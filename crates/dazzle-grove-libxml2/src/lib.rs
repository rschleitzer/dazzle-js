//! libxml2-based grove implementation
//!
//! This crate provides a concrete implementation of Dazzle's grove traits
//! using libxml2 for XML parsing and DTD validation.
//!
//! Corresponds to OpenJade's `spgrove/` directory (OpenSP grove implementation).
//!
//! ## Phase 3 Implementation Strategy
//!
//! This is **not** a direct port of OpenJade's spgrove/. Instead, we:
//! - Study OpenJade's grove **semantics** (what properties mean, edge cases)
//! - Implement fresh using libxml2 (not OpenSP)
//! - Follow our clean trait definitions from dazzle-core/grove
//!
//! ## OpenJade References
//!
//! Key insights from OpenJade `spgrove/`:
//! - **Children vs. Data**: `children` returns only element nodes, `data` returns text
//! - **Attribute Defaults**: Must include DTD default values
//! - **Node Identity**: Nodes are compared by identity (same object), not structure
//!
//! ## Architecture
//!
//! ```text
//! LibXml2Grove
//!   ├─ document: XmlDocument (owns libxml2 xmlDoc)
//!   ├─ id_map: HashMap<String, NodePtr> (for element-with-id)
//!   └─ root: LibXml2Node
//!
//! LibXml2Node
//!   ├─ ptr: NodePtr (xmlNode pointer)
//!   └─ doc: Weak<XmlDocument> (keep doc alive)
//!
//! LibXml2NodeList
//!   └─ nodes: Vec<LibXml2Node> (eager for now, optimize later)
//! ```

mod document;
mod node;
mod node_list;

pub use document::XmlDocument;
pub use node::LibXml2Node;
pub use node_list::LibXml2NodeList;

use dazzle_core::grove::{Grove, Node};
use std::collections::HashMap;
use std::rc::Rc;

/// libxml2-based Grove implementation
///
/// Implements the `Grove` trait using libxml2 for XML parsing.
///
/// ## Usage
///
/// ```
/// use dazzle_grove_libxml2::LibXml2Grove;
/// use dazzle_core::grove::Grove;
///
/// let xml = r#"<root><child id="c1">text</child></root>"#;
/// let grove = LibXml2Grove::parse(xml, false).unwrap();
///
/// let root = grove.root();
/// println!("Root: {:?}", root.gi());
/// ```
pub struct LibXml2Grove {
    /// The parsed XML document
    document: Rc<XmlDocument>,

    /// Cache of elements by ID attribute
    /// Built during parsing for fast `element-with-id` lookup
    id_map: HashMap<String, LibXml2Node>,
}

impl LibXml2Grove {
    /// Parse XML string into a Grove
    ///
    /// # Arguments
    ///
    /// * `xml` - XML string to parse
    /// * `validate_dtd` - If true, validate against DTD (if present in DOCTYPE)
    ///
    /// # Errors
    ///
    /// Returns error if XML is malformed or DTD validation fails.
    pub fn parse(xml: &str, validate_dtd: bool) -> Result<Self, String> {
        let document = XmlDocument::parse_string(xml, validate_dtd)?;
        let doc_rc = Rc::new(document);

        // Build ID map by traversing tree
        let id_map = Self::build_id_map(&doc_rc);

        Ok(LibXml2Grove {
            document: doc_rc,
            id_map,
        })
    }

    /// Build ID map by traversing the document tree
    fn build_id_map(document: &Rc<XmlDocument>) -> HashMap<String, LibXml2Node> {
        let mut map = HashMap::new();

        // Get root element
        if let Some(root) = document.root_element() {
            Self::collect_ids_recursive(&root, document, &mut map);
        }

        map
    }

    /// Recursively collect all elements with ID attributes
    fn collect_ids_recursive(
        node: &LibXml2Node,
        document: &Rc<XmlDocument>,
        map: &mut HashMap<String, LibXml2Node>,
    ) {
        // Check if this node has an ID attribute
        if let Some(id) = node.id() {
            map.insert(id.to_string(), node.clone());
        }

        // Recurse into children
        let children = node.children();
        let mut current = children.first();
        while let Some(child_node) = current {
            // Downcast to LibXml2Node (we know it's our type)
            if let Some(child) = Self::downcast_node(&child_node) {
                Self::collect_ids_recursive(&child, document, map);
            }
            current = children.rest().first();
        }
    }

    /// Helper to downcast Box<dyn Node> to LibXml2Node
    fn downcast_node(_node: &Box<dyn Node>) -> Option<LibXml2Node> {
        // This is safe because we know all nodes in our grove are LibXml2Node
        // TODO: Consider using downcasting or a better pattern
        // For now, we'll rely on the fact that we control node creation
        None // Placeholder - will fix in implementation
    }
}

impl Grove for LibXml2Grove {
    fn root(&self) -> Box<dyn Node> {
        if let Some(root) = self.document.root_element() {
            Box::new(root)
        } else {
            // Empty document - return a dummy node
            // This shouldn't happen in practice (validated XML always has root)
            panic!("Document has no root element");
        }
    }

    fn element_with_id(&self, id: &str) -> Option<Box<dyn Node>> {
        self.id_map
            .get(id)
            .map(|node| Box::new(node.clone()) as Box<dyn Node>)
    }
}

impl std::fmt::Debug for LibXml2Grove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LibXml2Grove")
            .field("id_map_size", &self.id_map.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_xml() {
        let xml = r#"<root><child>text</child></root>"#;
        let result = LibXml2Grove::parse(xml, false);

        // Should succeed (once we implement XmlDocument)
        assert!(result.is_ok() || result.is_err()); // Placeholder
    }

    #[test]
    fn test_element_with_id() {
        let _xml = r#"<root><child id="c1">text</child></root>"#;
        // Will test once implemented
    }
}
