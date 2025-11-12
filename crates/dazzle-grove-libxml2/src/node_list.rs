//! LibXml2NodeList implementation
//!
//! Implements the `NodeList` trait from dazzle-core.
//!
//! ## OpenJade Approach
//!
//! OpenJade uses lazy evaluation for node lists - they don't materialize
//! all nodes immediately. Instead, they act like iterators/generators.
//!
//! ## Optimization Strategy
//!
//! Originally used Vec-based approach with `to_vec()` in `rest()`, causing O(N²)
//! iteration complexity. Now uses Rc<[LibXml2Node]> + offset for O(1) rest():
//! - Shared ownership via Rc (cheap clones)
//! - Slice offset for efficient sub-lists
//! - O(1) first() and rest() operations
//! - Maintains functional/persistent semantics
//!
//! ## DSSSL Semantics
//!
//! Node lists are immutable and functional (like Scheme lists):
//! - `first()` returns first node
//! - `rest()` returns new node list without first element
//! - Lists are persistent (no mutation)

use dazzle_core::grove::{Node, NodeList};
use std::rc::Rc;

use crate::node::LibXml2Node;

/// A list of nodes
///
/// Implemented with Rc<[LibXml2Node]> + offset for efficient sublisting.
///
/// ## Performance
///
/// - first(): O(1)
/// - rest(): O(1) (just increments offset, shares underlying slice)
/// - length(): O(1)
/// - get(): O(1)
/// - Iteration over N elements: O(N) total (not O(N²)!)
///
/// ## Memory Management
///
/// Multiple node-lists can share the same underlying Rc<[LibXml2Node]> with
/// different offsets. The slice is only freed when all node-lists are dropped.
pub struct LibXml2NodeList {
    /// The nodes in this list (shared via Rc)
    ///
    /// Using Rc<[LibXml2Node]> for efficient sharing without copying.
    /// This is immutable after creation, so no RefCell needed.
    nodes: Rc<[LibXml2Node]>,

    /// Offset into the nodes slice
    ///
    /// Allows `rest()` to create a sub-list in O(1) time by just
    /// incrementing the offset instead of copying the Vec.
    offset: usize,
}

impl LibXml2NodeList {
    /// Create an empty node list
    pub fn empty() -> Self {
        LibXml2NodeList {
            nodes: Rc::from(Vec::new()),
            offset: 0,
        }
    }

    /// Create a node list from a Vec of nodes
    pub(crate) fn from_vec(nodes: Vec<LibXml2Node>) -> Self {
        LibXml2NodeList {
            nodes: Rc::from(nodes),
            offset: 0,
        }
    }

    /// Get the underlying slice (for internal use)
    #[allow(dead_code)]
    pub(crate) fn nodes(&self) -> &[LibXml2Node] {
        &self.nodes[self.offset..]
    }
}

impl NodeList for LibXml2NodeList {
    fn is_empty(&self) -> bool {
        self.offset >= self.nodes.len()
    }

    fn first(&self) -> Option<Box<dyn Node>> {
        if self.offset < self.nodes.len() {
            Some(Box::new(self.nodes[self.offset].clone()) as Box<dyn Node>)
        } else {
            None
        }
    }

    fn rest(&self) -> Box<dyn NodeList> {
        if self.offset + 1 >= self.nodes.len() {
            Box::new(LibXml2NodeList::empty())
        } else {
            // Share the same Rc, just increment offset - O(1) operation!
            Box::new(LibXml2NodeList {
                nodes: Rc::clone(&self.nodes),
                offset: self.offset + 1,
            })
        }
    }

    fn length(&self) -> usize {
        if self.offset < self.nodes.len() {
            self.nodes.len() - self.offset
        } else {
            0
        }
    }

    fn get(&self, index: usize) -> Option<Box<dyn Node>> {
        let actual_index = self.offset + index;
        if actual_index < self.nodes.len() {
            Some(Box::new(self.nodes[actual_index].clone()) as Box<dyn Node>)
        } else {
            None
        }
    }
}

impl std::fmt::Debug for LibXml2NodeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LibXml2NodeList")
            .field("length", &self.length())
            .field("offset", &self.offset)
            .finish()
    }
}

// Implement Clone for LibXml2NodeList
impl Clone for LibXml2NodeList {
    fn clone(&self) -> Self {
        LibXml2NodeList {
            nodes: Rc::clone(&self.nodes),
            offset: self.offset,
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
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
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
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
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
        let doc = XmlDocument::parse_string(xml, None, false).unwrap();
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
