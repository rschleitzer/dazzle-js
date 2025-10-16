//! Grove query primitives
//!
//! Implements ~50 primitives for XML tree navigation and querying.
//!
//! Phase 1 implements the critical ones:
//! - current-node, gi, id, children, parent, attribute-string, data
//! - node-list, node-list-empty?, node-list-first, node-list-rest
//! - select-elements, element-with-id

use crate::grove::{Grove, Node, NodeList};
use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all grove query primitives
pub fn register_grove_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // Grove accessors
    engine.register_fn("grove-root", grove_root);

    // Node property accessors (register as -impl, wrapped in Scheme for optional params)
    engine.register_fn("grove-gi-impl", grove_gi);
    engine.register_fn("grove-id-impl", grove_id);
    engine.register_fn("grove-data-impl", grove_data);
    engine.register_fn("grove-attribute-string-impl", grove_attribute_string_impl);

    // Node navigation
    engine.register_fn("children", grove_children);
    engine.register_fn("parent", grove_parent);

    // Node list operations (registered as -from-vec, wrapped in Scheme for variadic support)
    engine.register_fn("node-list-from-vec", grove_node_list);
    engine.register_fn("node-list-empty?", grove_node_list_empty);
    engine.register_fn("node-list-first", grove_node_list_first);
    engine.register_fn("node-list-rest", grove_node_list_rest);
    engine.register_fn("node-list-length", grove_node_list_length);
    engine.register_fn("node-list->list", grove_node_list_to_list);

    // Type predicates
    engine.register_fn("element?", grove_element_p);
    engine.register_fn("text?", grove_text_p);

    // Node list filtering
    engine.register_fn("select-elements", grove_select_elements);
    engine.register_fn("select-children", grove_select_children);
    engine.register_fn("descendants", grove_descendants);

    // Node list operations
    engine.register_fn("empty-node-list", grove_empty_node_list);
    engine.register_fn("node-list-reverse", grove_node_list_reverse);

    // Element lookup (registered as grove-element-with-id, wrapped in Scheme as element-with-id)
    engine.register_fn("grove-element-with-id", grove_element_with_id);

    // Child numbering
    engine.register_fn("child-number", grove_child_number);

    Ok(())
}

// ============================================================================
// Grove Accessors
// ============================================================================

/// Get the root node from a grove
fn grove_root(grove: &Grove) -> Node {
    grove.root().clone()
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

/// Get an attribute value by name (Rust implementation - always 2 args)
/// Wrapped in Scheme to support optional node parameter
fn grove_attribute_string_impl(name: String, node: &Node) -> Option<String> {
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

/// Create a node-list from a list of nodes
/// Usage: (node-list node1 node2 node3)
/// Note: This is typically called via (apply node-list list-of-nodes)
fn grove_node_list(nodes: Vec<Node>) -> NodeList {
    NodeList::from_vec(nodes)
}

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

/// Convert a node-list to a Scheme list
fn grove_node_list_to_list(nl: &NodeList) -> Vec<Node> {
    nl.iter().cloned().collect()
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

// ============================================================================
// Node List Filtering
// ============================================================================

/// Select elements with a specific GI from a node-list
/// Usage: (select-elements node-list "field")
fn grove_select_elements(nl: &NodeList, gi: String) -> NodeList {
    let filtered: Vec<Node> = nl.iter()
        .filter(|node| node.is_element() && node.gi() == gi)
        .cloned()
        .collect();
    NodeList::from_vec(filtered)
}

/// Select immediate children with a specific GI
/// Usage: (select-children node "field")
fn grove_select_children(node: &Node, gi: String) -> NodeList {
    let children = node.children();
    grove_select_elements(&children, gi)
}

/// Get all descendant nodes (depth-first traversal)
/// Usage: (descendants node)
fn grove_descendants(node: &Node) -> NodeList {
    fn collect_descendants(node: &Node, acc: &mut Vec<Node>) {
        let children = node.children();
        for child in children.iter() {
            acc.push(child.clone());
            collect_descendants(child, acc);
        }
    }

    let mut all_descendants = Vec::new();
    collect_descendants(node, &mut all_descendants);
    NodeList::from_vec(all_descendants)
}

/// Create an empty node-list
/// Usage: (empty-node-list)
fn grove_empty_node_list() -> NodeList {
    NodeList::from_vec(vec![])
}

/// Reverse a node-list
/// Usage: (node-list-reverse node-list)
fn grove_node_list_reverse(nl: &NodeList) -> NodeList {
    let mut nodes: Vec<Node> = nl.iter().cloned().collect();
    nodes.reverse();
    NodeList::from_vec(nodes)
}

/// Find element by ID attribute anywhere in the tree
/// Usage: (element-with-id grove "my-id")
fn grove_element_with_id(grove: &Grove, id: String) -> Option<Node> {
    fn search_for_id(node: &Node, target_id: &str) -> Option<Node> {
        // Check current node
        if node.is_element() {
            if let Some(node_id) = node.id() {
                if node_id == target_id {
                    return Some(node.clone());
                }
            }
        }

        // Search children recursively
        let children = node.children();
        for child in children.iter() {
            if let Some(found) = search_for_id(child, target_id) {
                return Some(found);
            }
        }

        None
    }

    search_for_id(&grove.root(), &id)
}

/// Get the 1-based index of a node among its SAME-TYPE siblings
/// Usage: (child-number node)
///
/// DSSSL behavior (per OpenJade): counts position among siblings of the SAME element type.
/// For example, if <grammar> has children: <syntax>, <syntax>, <keyword>, <keyword>
/// - First <syntax> has child-number 1
/// - Second <syntax> has child-number 2
/// - First <keyword> has child-number 1 (not 3!)
/// - Second <keyword> has child-number 2 (not 4!)
fn grove_child_number(node: &Node) -> usize {
    // Get the parent
    if let Some(parent) = node.parent() {
        let siblings = parent.children();
        let node_gi = node.gi();  // Element type of this node

        // Count position among same-type siblings only
        let mut count = 0;
        for sibling in siblings.iter() {
            if sibling.gi() == node_gi {
                count += 1;
                if sibling.ptr_eq(node) {
                    return count;  // 1-based index among same-type siblings
                }
            }
        }

        // Should not reach here if node is actually a child of parent
        1
    } else {
        // Root node - return 1
        1
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_grove_primitives() {
        // Will test each primitive as implemented
    }
}
