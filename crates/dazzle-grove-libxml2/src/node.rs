//! LibXml2Node implementation
//!
//! Implements the `Node` trait from dazzle-core using libxml2 nodes.
//!
//! ## OpenJade Semantics
//!
//! From studying OpenJade's `spgrove/`:
//!
//! 1. **`gi` (Generic Identifier)**:
//!    - Element nodes: return element name
//!    - Text/attribute nodes: return None
//!
//! 2. **`data`**:
//!    - Text nodes: return text content
//!    - Element nodes: return concatenated descendant text
//!    - Attribute nodes: return attribute value
//!
//! 3. **`children`**:
//!    - **CRITICAL**: Returns only ELEMENT children, not text nodes!
//!    - Text is accessed via `data` property
//!    - This is DSSSL semantics, different from DOM
//!
//! 4. **`id`**:
//!    - Returns value of ID-type attribute (from DTD)
//!    - If no DTD, checks for attribute named "id"
//!
//! 5. **`attribute-string`**:
//!    - Includes DTD default values
//!    - Returns None if attribute doesn't exist

use dazzle_core::grove::{Node, NodeList};
use libxml::tree::Node as LibxmlNode;
use std::rc::Rc;

use crate::node_list::LibXml2NodeList;

/// A node in the libxml2 grove
///
/// Wraps libxml2's xmlNode safely.
///
/// ## Memory Management
///
/// Nodes borrow from the document. The document must outlive all nodes.
/// We don't need Rc<XmlDocument> in each node because:
/// - Nodes are created on-demand from LibXml2Grove
/// - The grove owns the document
/// - Nodes are short-lived (query results)
#[derive(Clone)]
pub struct LibXml2Node {
    /// The underlying libxml node
    ///
    /// We use the `libxml` crate's Node wrapper which handles memory safety
    node: Rc<LibxmlNode>,
}

impl LibXml2Node {
    /// Create a LibXml2Node from a libxml Node
    ///
    /// This is package-private - only called from XmlDocument
    pub(crate) fn from_libxml_node(node: LibxmlNode) -> Self {
        LibXml2Node {
            node: Rc::new(node),
        }
    }

    /// Get the underlying libxml node (for internal use)
    #[allow(dead_code)]
    pub(crate) fn inner(&self) -> &LibxmlNode {
        &self.node
    }
}

impl Node for LibXml2Node {
    fn clone_node(&self) -> Box<dyn Node> {
        Box::new(self.clone())
    }

    fn gi(&self) -> Option<String> {
        // Only element nodes have a generic identifier
        if self.is_element() {
            Some(self.node.get_name())
        } else {
            None
        }
    }

    fn id(&self) -> Option<String> {
        // DSSSL: ID is the value of an ID-type attribute
        // For now, check for "id" attribute (proper DTD-based ID lookup needs more work)
        if !self.is_element() {
            return None;
        }

        // Check for "id" attribute
        self.node.get_attribute("id")
    }

    fn data(&self) -> Option<String> {
        // DSSSL semantics:
        // - Text nodes: return text content
        // - Element nodes: return concatenated descendant text
        // - Other nodes: None

        if self.is_text() {
            // Text node: return content
            Some(self.node.get_content())
        } else if self.is_element() {
            // Element: get all descendant text
            // libxml's get_content() does this automatically
            Some(self.node.get_content())
        } else {
            None
        }
    }

    fn children(&self) -> Box<dyn NodeList> {
        // DSSSL CRITICAL SEMANTICS:
        // `children` returns ONLY element nodes, not text nodes!
        //
        // This is different from DOM's childNodes which includes text.
        // Text content is accessed via the `data` property.

        if !self.is_element() {
            // Non-elements have no children in DSSSL
            return Box::new(LibXml2NodeList::empty());
        }

        let mut child_elements = Vec::new();

        // Iterate through child nodes, keeping only elements
        for child in self.node.get_child_nodes() {
            if child.get_type() == Some(libxml::tree::NodeType::ElementNode) {
                child_elements.push(LibXml2Node::from_libxml_node(child));
            }
        }

        Box::new(LibXml2NodeList::from_vec(child_elements))
    }

    fn parent(&self) -> Option<Box<dyn Node>> {
        // Get parent, but filter out document nodes
        // In DSSSL, the root element has no parent (document node is not exposed)
        self.node.get_parent().and_then(|p| {
            // Check if parent is a document node
            if p.get_type() == Some(libxml::tree::NodeType::DocumentNode) {
                None
            } else {
                Some(Box::new(LibXml2Node::from_libxml_node(p)) as Box<dyn Node>)
            }
        })
    }

    fn attribute_string(&self, name: &str) -> Option<String> {
        // DSSSL: Includes DTD default values
        // libxml2's get_attribute() automatically includes defaults

        if !self.is_element() {
            return None;
        }

        self.node.get_attribute(name)
    }

    fn is_element(&self) -> bool {
        self.node.get_type() == Some(libxml::tree::NodeType::ElementNode)
    }

    fn is_text(&self) -> bool {
        self.node.get_type() == Some(libxml::tree::NodeType::TextNode)
    }

    fn node_eq(&self, other: &dyn Node) -> bool {
        // Node equality: same node in the document tree
        // Try to downcast to LibXml2Node and compare underlying xmlNodePtr pointers
        if let Some(other_node) = (other as &dyn std::any::Any).downcast_ref::<LibXml2Node>() {
            // Compare the underlying xmlNodePtr C pointers
            // libxml's Node::node_ptr() returns the raw xmlNodePtr
            std::ptr::eq(self.node.node_ptr(), other_node.node.node_ptr())
        } else {
            // Different node implementation - fall back to ID comparison
            self.node_id() == other.node_id()
        }
    }

    fn node_id(&self) -> usize {
        // Use the underlying xmlNodePtr as a unique node ID
        // This is stable across multiple Rc wrappers of the same libxml node
        self.node.node_ptr() as usize
    }
}

impl std::fmt::Debug for LibXml2Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LibXml2Node")
            .field("gi", &self.gi())
            .field("id", &self.id())
            .field("is_element", &self.is_element())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::XmlDocument;

    #[test]
    fn test_node_gi() {
        let xml = r#"<?xml version="1.0"?><root><child/></root>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
        let root = doc.root_element().unwrap();

        assert_eq!(root.gi(), Some("root".to_string()));
    }

    #[test]
    fn test_node_children_only_elements() {
        // CRITICAL TEST: children should only return elements, not text!
        let xml = r#"<?xml version="1.0"?><root>text1<child/>text2</root>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();

        // Should have only 1 child (the <child> element)
        // NOT 3 children (text1, child, text2)
        assert_eq!(children.length(), 1);

        let first = children.first().unwrap();
        assert_eq!(first.gi(), Some("child".to_string()));
    }

    #[test]
    fn test_node_data() {
        let xml = r#"<?xml version="1.0"?><root><child>Hello World</child></root>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();
        let child = children.first().unwrap();

        // data() on element returns descendant text
        assert!(child.data().is_some());
        assert!(child.data().unwrap().contains("Hello World"));
    }

    #[test]
    fn test_node_attributes() {
        let xml = r#"<?xml version="1.0"?><root id="r1" name="test"/>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
        let root = doc.root_element().unwrap();

        assert_eq!(root.id(), Some("r1".to_string()));
        assert_eq!(root.attribute_string("name"), Some("test".to_string()));
        assert_eq!(root.attribute_string("missing"), None);
    }

    #[test]
    fn test_node_parent() {
        let xml = r#"<?xml version="1.0"?><root><child/></root>"#;
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();
        let child = children.first().unwrap();

        let parent = child.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().gi(), Some("root".to_string()));
    }
}
