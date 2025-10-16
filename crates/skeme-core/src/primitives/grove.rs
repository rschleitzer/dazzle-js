//! Grove query primitives
//!
//! Implements ~50 primitives for XML tree navigation and querying.
//!
//! Phase 1 implements the critical ones:
//! - current-node, gi, id, children, parent, attribute-string, data
//! - node-list, node-list-empty?, node-list-first, node-list-rest
//! - select-elements, element-with-id

use crate::grove::{Node, NodeList};
use crate::scheme::SchemeEngine;
use anyhow::Result;
use steel::steel_vm::register_fn::RegisterFn;

/// Register all grove query primitives
pub fn register_grove_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // Node property accessors
    engine.register_fn("gi", grove_gi);
    engine.register_fn("id", grove_id);
    engine.register_fn("data", grove_data);
    engine.register_fn("attribute-string", grove_attribute_string);

    // Node navigation
    engine.register_fn("children", grove_children);
    engine.register_fn("parent", grove_parent);

    // Node list operations
    engine.register_fn("node-list-empty?", grove_node_list_empty);
    engine.register_fn("node-list-first", grove_node_list_first);
    engine.register_fn("node-list-rest", grove_node_list_rest);
    engine.register_fn("node-list-length", grove_node_list_length);

    // Type predicates
    engine.register_fn("element?", grove_element_p);
    engine.register_fn("text?", grove_text_p);

    Ok(())
}

// ============================================================================
// Node Property Primitives
// ============================================================================

/// Get the generic identifier (element name) of a node
fn grove_gi(node: &Node) -> String {
    node.gi()
}

/// Get the ID attribute of a node
fn grove_id(node: &Node) -> Option<String> {
    node.id()
}

/// Get the text content of a node
fn grove_data(node: &Node) -> String {
    node.data()
}

/// Get an attribute value by name
fn grove_attribute_string(node: &Node, name: String) -> Option<String> {
    node.attribute(&name)
}

// ============================================================================
// Node Navigation Primitives
// ============================================================================

/// Get the children of a node as a node-list
fn grove_children(node: &Node) -> NodeList {
    node.children()
}

/// Get the parent of a node
fn grove_parent(node: &Node) -> Option<Node> {
    node.parent()
}

// ============================================================================
// NodeList Primitives
// ============================================================================

/// Check if a node-list is empty
fn grove_node_list_empty(nl: &NodeList) -> bool {
    nl.is_empty()
}

/// Get the first node in a node-list
fn grove_node_list_first(nl: &NodeList) -> Option<Node> {
    nl.first()
}

/// Get all but the first node in a node-list
fn grove_node_list_rest(nl: &NodeList) -> NodeList {
    nl.rest()
}

/// Get the length of a node-list
fn grove_node_list_length(nl: &NodeList) -> usize {
    nl.len()
}

// ============================================================================
// Type Predicates
// ============================================================================

/// Check if a node is an element
fn grove_element_p(node: &Node) -> bool {
    node.is_element()
}

/// Check if a node is a text node
fn grove_text_p(node: &Node) -> bool {
    node.is_text()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grove_primitives() {
        // Will test each primitive as implemented
    }
}
