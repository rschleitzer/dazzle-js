//! LibXml2NodeList implementation
//!
//! Implements the `NodeList` trait from dazzle-core.
//!
//! ## OpenJade Approach
//!
//! OpenJade uses lazy evaluation for node lists - they don't materialize
//! all nodes immediately. Instead, they act like iterators/generators.
//!
//! For Phase 3, we use an eager approach (Vec-based) for simplicity:
//! - Easier to implement and debug
//! - Adequate performance for code generation workloads
//! - Can optimize later if profiling shows it matters
//!
//! ## DSSSL Semantics
//!
//! Node lists are immutable and functional (like Scheme lists):
//! - `first()` returns first node
//! - `rest()` returns new node list without first element
//! - Lists are persistent (no mutation)

use dazzle_core::grove::{Node, NodeList};

use crate::node::LibXml2Node;

/// A list of nodes
///
/// Implemented as Vec for simplicity. Can be optimized later if needed.
///
/// ## Memory Management
///
/// Nodes are cloned (Rc-based, so cheap) when creating sublists.
/// This maintains functional semantics without excessive copying.
pub struct LibXml2NodeList {
    /// The nodes in this list
    ///
    /// Using Vec for eager evaluation. Alternative approaches:
    /// - Iterator-based (lazy, like OpenJade)
    /// - Arc<[LibXml2Node]> (cheaper cloning)
    /// - Rc slice (functional persistent data structure)
    nodes: Vec<LibXml2Node>,
}

impl LibXml2NodeList {
    /// Create an empty node list
    pub fn empty() -> Self {
        LibXml2NodeList { nodes: Vec::new() }
    }

    /// Create a node list from a Vec of nodes
    pub(crate) fn from_vec(nodes: Vec<LibXml2Node>) -> Self {
        LibXml2NodeList { nodes }
    }

    /// Get the underlying Vec (for internal use)
    pub(crate) fn nodes(&self) -> &[LibXml2Node] {
        &self.nodes
    }
}

impl NodeList for LibXml2NodeList {
    fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    fn first(&self) -> Option<Box<dyn Node>> {
        self.nodes
            .first()
            .map(|node| Box::new(node.clone()) as Box<dyn Node>)
    }

    fn rest(&self) -> Box<dyn NodeList> {
        if self.nodes.len() <= 1 {
            Box::new(LibXml2NodeList::empty())
        } else {
            Box::new(LibXml2NodeList::from_vec(self.nodes[1..].to_vec()))
        }
    }

    fn length(&self) -> usize {
        self.nodes.len()
    }

    fn get(&self, index: usize) -> Option<Box<dyn Node>> {
        self.nodes
            .get(index)
            .map(|node| Box::new(node.clone()) as Box<dyn Node>)
    }
}

impl std::fmt::Debug for LibXml2NodeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LibXml2NodeList")
            .field("length", &self.nodes.len())
            .finish()
    }
}

// Implement Clone for LibXml2NodeList
impl Clone for LibXml2NodeList {
    fn clone(&self) -> Self {
        LibXml2NodeList {
            nodes: self.nodes.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::XmlDocument;

    #[test]
    fn test_empty_node_list() {
        let list = LibXml2NodeList::empty();
        assert!(list.is_empty());
        assert_eq!(list.length(), 0);
        assert!(list.first().is_none());
    }

    #[test]
    fn test_node_list_operations() {
        let xml = r#"<?xml version="1.0"?><root><a/><b/><c/></root>"#;
        let doc = XmlDocument::parse_string(xml, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();

        assert!(!children.is_empty());
        assert_eq!(children.length(), 3);

        // Test first
        let first = children.first().unwrap();
        assert_eq!(first.gi(), Some("a".to_string()));

        // Test rest
        let rest = children.rest();
        assert_eq!(rest.length(), 2);

        let second = rest.first().unwrap();
        assert_eq!(second.gi(), Some("b".to_string()));

        // Test get
        let third = children.get(2).unwrap();
        assert_eq!(third.gi(), Some("c".to_string()));
    }

    #[test]
    fn test_node_list_rest_on_single_element() {
        let xml = r#"<?xml version="1.0"?><root><child/></root>"#;
        let doc = XmlDocument::parse_string(xml, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();
        assert_eq!(children.length(), 1);

        let rest = children.rest();
        assert!(rest.is_empty());
        assert_eq!(rest.length(), 0);
    }

    #[test]
    fn test_node_list_iteration() {
        let xml = r#"<?xml version="1.0"?><root><a/><b/><c/></root>"#;
        let doc = XmlDocument::parse_string(xml, false).unwrap();
        let root = doc.root_element().unwrap();

        let children = root.children();

        // Iterate using first/rest pattern (Scheme style)
        let mut current = children;
        let mut names = Vec::new();

        while let Some(node) = current.first() {
            if let Some(gi) = node.gi() {
                names.push(gi.to_string());
            }
            current = current.rest();
        }

        assert_eq!(names, vec!["a", "b", "c"]);
    }
}
