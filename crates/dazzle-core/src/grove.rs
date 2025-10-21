//! Grove trait definitions
//!
//! This module defines the abstract interface for document trees (groves),
//! following OpenJade's grove/ architecture.
//!
//! The grove model is defined by the DSSSL standard (ISO/IEC 10179:1996) as an
//! abstract representation of document structure, independent of markup syntax.
//!
//! ## Architecture
//!
//! Like OpenJade's separation between `grove/` (abstract interface) and `spgrove/`
//! (OpenSP implementation), Dazzle defines:
//!
//! - **This module (`dazzle-core/grove`)**: Abstract traits (`Node`, `NodeList`, `Grove`)
//! - **`dazzle-grove-libxml2`**: Concrete implementation using libxml2 (XML + DTD)
//! - **`dazzle-grove-opensp` (future)**: Concrete implementation using OpenSP (full SGML)
//!
//! This allows the Scheme interpreter to work with any grove implementation
//! without coupling to a specific parser.
//!
//! ## Key Traits
//!
//! - `Node`: A single node in the document tree (element, text, attribute, etc.)
//! - `NodeList`: An ordered collection of nodes
//! - `Grove`: The complete document grove (root + global operations)

use std::any::Any;
use std::fmt::Debug;

/// A node in the document tree
///
/// Corresponds to OpenJade's `GroveImpl::NodeImpl` interface.
///
/// ## DSSSL Node Properties
///
/// DSSSL defines numerous node properties. The core ones are:
///
/// - **gi**: Generic identifier (element name)
/// - **id**: ID attribute value
/// - **data**: Text content (for text nodes)
/// - **attributes**: Attribute nodes
/// - **children**: Child nodes (elements only, not text)
/// - **parent**: Parent node
///
/// See DSSSL spec Section 8 for complete property list.
pub trait Node: Debug + Any {
    /// Clone this node into a new Box
    ///
    /// This is required because trait objects (Box<dyn Node>) cannot implement Clone directly.
    /// Each Node implementation must provide its own cloning logic.
    fn clone_node(&self) -> Box<dyn Node>;

    /// Get the generic identifier (element name)
    ///
    /// Returns `None` for non-element nodes.
    ///
    /// DSSSL: `gi` property
    ///
    /// **Design Note**: Returns owned `String` rather than `&str` because
    /// XML parsers (libxml2) return owned strings. This avoids lifetime complexity.
    fn gi(&self) -> Option<String>;

    /// Get the ID attribute value
    ///
    /// Returns `None` if node has no ID attribute.
    ///
    /// DSSSL: `id` property
    fn id(&self) -> Option<String>;

    /// Get text content
    ///
    /// For text nodes, returns the text. For elements, returns
    /// concatenated descendant text.
    ///
    /// DSSSL: `data` property
    fn data(&self) -> Option<String>;

    /// Get child nodes
    ///
    /// **Important**: In DSSSL, `children` returns only element nodes,
    /// not text nodes. Text is accessed via `data` property.
    ///
    /// DSSSL: `children` property
    fn children(&self) -> Box<dyn NodeList>;

    /// Get parent node
    ///
    /// Returns `None` for the root node.
    ///
    /// DSSSL: `parent` property
    fn parent(&self) -> Option<Box<dyn Node>>;

    /// Get attribute value
    ///
    /// Includes DTD default values if defined.
    ///
    /// DSSSL: `attribute-string` primitive
    fn attribute_string(&self, name: &str) -> Option<String>;

    /// Check if this is an element node
    fn is_element(&self) -> bool;

    /// Check if this is a text node
    fn is_text(&self) -> bool;

    /// Check node equality (same node in document tree)
    ///
    /// Two node references are equal if they refer to the same
    /// node in the document, not just structurally similar nodes.
    fn node_eq(&self, other: &dyn Node) -> bool;

    /// Get a unique identifier for this node
    ///
    /// Returns a value that uniquely identifies this node within its document.
    /// Two nodes with the same `node_id` are the same node.
    fn node_id(&self) -> usize;
}

/// An ordered collection of nodes
///
/// Corresponds to OpenJade's `GroveImpl::NodeListImpl` interface.
///
/// ## Lazy Evaluation
///
/// Like OpenJade, node lists should support lazy evaluation - they don't
/// need to materialize all nodes immediately. Operations like `first()`
/// and `rest()` can be implemented efficiently as iterators.
///
/// DSSSL node lists are immutable and functional (cons-list style).
pub trait NodeList: Debug {
    /// Check if the node list is empty
    ///
    /// DSSSL: `node-list-empty?`
    fn is_empty(&self) -> bool;

    /// Get the first node
    ///
    /// Returns `None` if the list is empty.
    ///
    /// DSSSL: `node-list-first`
    fn first(&self) -> Option<Box<dyn Node>>;

    /// Get the rest of the node list (all but first)
    ///
    /// Returns an empty node list if this list has 0 or 1 elements.
    ///
    /// DSSSL: `node-list-rest`
    fn rest(&self) -> Box<dyn NodeList>;

    /// Get the length of the node list
    ///
    /// DSSSL: `node-list-length`
    fn length(&self) -> usize;

    /// Get node at index
    ///
    /// Returns `None` if index is out of bounds.
    ///
    /// DSSSL: `node-list-ref`
    fn get(&self, index: usize) -> Option<Box<dyn Node>>;
}

/// The complete document grove
///
/// Corresponds to OpenJade's `GroveImpl::Grove` interface.
///
/// A grove holds the entire document tree and provides global operations
/// like `element-with-id`.
pub trait Grove: Debug {
    /// Get the root node
    ///
    /// DSSSL: `grove-root` (or implicit root access)
    fn root(&self) -> Box<dyn Node>;

    /// Find element by ID
    ///
    /// Returns `None` if no element with the given ID exists.
    ///
    /// DSSSL: `element-with-id`
    fn element_with_id(&self, id: &str) -> Option<Box<dyn Node>>;
}

/// An empty node list implementation
///
/// This is a concrete implementation of NodeList that represents
/// an empty list. It's used by primitives like `empty-node-list`.
#[derive(Debug, Clone)]
pub struct EmptyNodeList;

impl EmptyNodeList {
    pub fn new() -> Self {
        EmptyNodeList
    }
}

impl Default for EmptyNodeList {
    fn default() -> Self {
        Self::new()
    }
}

impl NodeList for EmptyNodeList {
    fn is_empty(&self) -> bool {
        true
    }

    fn first(&self) -> Option<Box<dyn Node>> {
        None
    }

    fn rest(&self) -> Box<dyn NodeList> {
        Box::new(EmptyNodeList::new())
    }

    fn length(&self) -> usize {
        0
    }

    fn get(&self, _index: usize) -> Option<Box<dyn Node>> {
        None
    }
}

/// A node list backed by a vector with shared ownership
///
/// Used for creating filtered node lists (e.g., from select-elements).
/// Uses Rc to share the vector without cloning nodes.
#[derive(Debug)]
pub struct VecNodeList {
    nodes: std::rc::Rc<Vec<Box<dyn Node>>>,
    offset: usize,
}

impl VecNodeList {
    pub fn new(nodes: Vec<Box<dyn Node>>) -> Self {
        VecNodeList {
            nodes: std::rc::Rc::new(nodes),
            offset: 0,
        }
    }

    fn from_rc(nodes: std::rc::Rc<Vec<Box<dyn Node>>>, offset: usize) -> Self {
        VecNodeList { nodes, offset }
    }
}

impl NodeList for VecNodeList {
    fn is_empty(&self) -> bool {
        self.offset >= self.nodes.len()
    }

    fn first(&self) -> Option<Box<dyn Node>> {
        self.nodes.get(self.offset).map(|n| n.clone_node())
    }

    fn rest(&self) -> Box<dyn NodeList> {
        if self.offset + 1 >= self.nodes.len() {
            Box::new(EmptyNodeList::new())
        } else {
            Box::new(VecNodeList::from_rc(self.nodes.clone(), self.offset + 1))
        }
    }

    fn length(&self) -> usize {
        self.nodes.len().saturating_sub(self.offset)
    }

    fn get(&self, index: usize) -> Option<Box<dyn Node>> {
        self.nodes.get(self.offset + index).map(|n| n.clone_node())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_traits_defined() {
        // This test just ensures the traits compile
        // Actual tests will be in dazzle-grove-libxml2
    }

    #[test]
    fn test_empty_node_list() {
        let empty = EmptyNodeList::new();
        assert!(empty.is_empty());
        assert_eq!(empty.length(), 0);
        assert!(empty.first().is_none());
        assert!(empty.get(0).is_none());
        assert!(empty.rest().is_empty());
    }
}
