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
pub trait Node: Debug {
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

#[cfg(test)]
mod tests {
    // These tests will be implemented once we have a concrete grove implementation
    // For now, they serve as documentation of expected behavior

    #[test]
    fn test_traits_defined() {
        // This test just ensures the traits compile
        // Actual tests will be in dazzle-grove-libxml2
    }
}
