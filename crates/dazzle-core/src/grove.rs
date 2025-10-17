//! Grove model: representation of XML document trees
//!
//! This module implements the DSSSL grove model, which is an abstract
//! representation of document structure independent of the actual markup.
//!
//! ## Key Types
//!
//! - `Grove`: The document grove (keeps XML document alive)
//! - `Node`: A single node in the grove (element, text, etc.)
//! - `NodeList`: An ordered collection of nodes
//! - `NodeProperty`: Grove node properties (GI, ID, attributes, etc.)

use libxml::tree::{Document, Node as XmlNode};
use steel::rvals::Custom;
use std::sync::Arc;

/// A grove - the complete document tree
///
/// This holds the XML Document to keep it alive while nodes are in use.
#[derive(Clone)]
pub struct Grove {
    /// The underlying XML document (kept alive)
    /// This field is intentionally not accessed directly - its purpose is to keep the Document alive
    #[allow(dead_code)]
    document: Arc<Document>,
    /// The root node
    root: Node,
}

impl std::fmt::Debug for Grove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Grove")
            .field("root", &self.root)
            .finish()
    }
}

impl Grove {
    /// Create a new grove from a Document
    pub fn from_document(doc: Document) -> Option<Self> {
        let root_elem = doc.get_root_element()?;
        let root = Node::from_xml_node(root_elem);
        Some(Grove {
            document: Arc::new(doc),
            root,
        })
    }

    /// Get the root node
    pub fn root(&self) -> &Node {
        &self.root
    }
}

// Implement Steel's Custom trait for Grove
impl Custom for Grove {}

/// A node in the grove (document tree)
///
/// Wraps libxml::tree::Node and provides DSSSL grove semantics
#[derive(Debug, Clone)]
pub struct Node {
    /// The underlying libxml2 node
    inner: XmlNode,
}

impl Node {
    /// Create a Node from a libxml Node
    pub fn from_xml_node(xml_node: XmlNode) -> Self {
        Node { inner: xml_node }
    }

    /// Get the underlying libxml Node
    pub fn xml_node(&self) -> &XmlNode {
        &self.inner
    }

    /// Get the generic identifier (element name)
    pub fn gi(&self) -> String {
        self.inner.get_name()
    }

    /// Get the ID attribute
    pub fn id(&self) -> Option<String> {
        self.inner.get_attribute("id")
    }

    /// Get child nodes
    pub fn children(&self) -> NodeList {
        let child_nodes = self.inner.get_child_nodes();
        NodeList::from_vec(child_nodes.into_iter().map(Node::from_xml_node).collect())
    }

    /// Get parent node
    pub fn parent(&self) -> Option<Node> {
        self.inner.get_parent().map(Node::from_xml_node)
    }

    /// Get attribute value
    pub fn attribute(&self, name: &str) -> Option<String> {
        self.inner.get_attribute(name)
    }

    /// Get text content
    pub fn data(&self) -> String {
        self.inner.get_content()
    }

    /// Check if this is an element node
    pub fn is_element(&self) -> bool {
        self.inner.is_element_node()
    }

    /// Check if this is a text node
    pub fn is_text(&self) -> bool {
        self.inner.is_text_node()
    }

    /// Check pointer equality (same node in memory)
    pub fn ptr_eq(&self, other: &Node) -> bool {
        // Compare underlying xmlNode pointers
        std::ptr::eq(
            self.inner.node_ptr() as *const (),
            other.inner.node_ptr() as *const ()
        )
    }
}

// Implement Steel's Custom trait for Node
impl Custom for Node {}

/// An ordered collection of nodes
#[derive(Debug, Clone)]
pub struct NodeList {
    nodes: Vec<Node>,
}

impl NodeList {
    /// Create an empty node list
    pub fn empty() -> Self {
        Self { nodes: Vec::new() }
    }

    /// Create a node list from a vector of nodes
    pub fn from_vec(nodes: Vec<Node>) -> Self {
        Self { nodes }
    }

    /// Check if the node list is empty
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Get the first node
    pub fn first(&self) -> Option<Node> {
        self.nodes.first().cloned()
    }

    /// Get the rest of the node list (all but first)
    pub fn rest(&self) -> NodeList {
        if self.nodes.is_empty() {
            NodeList::empty()
        } else {
            NodeList::from_vec(self.nodes[1..].to_vec())
        }
    }

    /// Get the length
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Get node at index
    pub fn get(&self, index: usize) -> Option<Node> {
        self.nodes.get(index).cloned()
    }

    /// Iterate over the nodes
    pub fn iter(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    /// Create a singleton node list
    pub fn singleton(node: Node) -> Self {
        NodeList { nodes: vec![node] }
    }
}

// Implement Steel's Custom trait for NodeList
impl Custom for NodeList {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node_list_empty() {
        let nl = NodeList::empty();
        assert!(nl.is_empty());
        assert_eq!(nl.len(), 0);
    }
}
