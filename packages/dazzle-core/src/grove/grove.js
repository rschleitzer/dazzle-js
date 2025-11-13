/**
 * Grove Interface - Abstract grove model for DSSSL
 *
 * Port from: OpenJade grove/Node.{h,cxx} (2,400 lines)
 *
 * This defines the abstract interface for grove nodes, matching
 * the DSSSL grove model (ISO/IEC 10179:1996 §8-9).
 *
 * ARCHITECTURE NOTE:
 * OpenJade uses C++ virtual functions for polymorphic dispatch.
 * We replicate this with TypeScript interfaces and duck typing.
 * Modern JS engines optimize method calls through prototype chains
 * with inline caches, giving performance similar to C++ vtables.
 *
 * USAGE PATTERN (matches OpenJade):
 * - Call methods directly: `const name = node.gi()`
 * - Methods return null when not applicable
 * - No need to check nodeType() before calling methods
 * - Only use nodeType() when you explicitly need to branch on type
 *
 * Example (OpenJade style):
 *   const gi = node.gi();  // Returns null for non-elements
 *   if (gi) { ... }
 *
 * NOT (unnecessary overhead):
 *   if (node.nodeType() === NodeType.Element) {
 *     const gi = node.gi();  // Type check not needed!
 *   }
 */
/**
 * Node type enumeration
 * Port from: grove/Node.h NodeType enum
 *
 * Used for debugging and explicit type checking (rare).
 * Most code should just call methods directly without checking type.
 */
export var NodeType;
(function (NodeType) {
    NodeType["Element"] = "element";
    NodeType["Text"] = "text";
    NodeType["ProcessingInstruction"] = "processing-instruction";
    NodeType["Comment"] = "comment";
    NodeType["Entity"] = "entity";
    NodeType["Notation"] = "notation";
    NodeType["Document"] = "document";
    NodeType["DocumentType"] = "document-type";
    NodeType["Attribute"] = "attribute";
    NodeType["AttributeDefinition"] = "attribute-definition";
    NodeType["ElementType"] = "element-type";
})(NodeType || (NodeType = {}));
/**
 * Empty node list singleton
 * Port from: grove/Node.cxx emptyNodeList
 *
 * Shared instance for efficiency - node lists are immutable
 */
export const EMPTY_NODE_LIST = {
    first: () => null,
    rest: () => null,
    length: () => 0,
    isEmpty: () => true,
    nth: () => null,
    toArray: () => [],
};
/**
 * Helper: Create a node list from an array
 * Port from: grove/Node.cxx (various constructors)
 *
 * Creates an immutable node list from an array.
 * The array is copied, so mutations won't affect the list.
 */
export function nodeListFromArray(nodes) {
    if (nodes.length === 0) {
        return EMPTY_NODE_LIST;
    }
    // Copy array to prevent mutations
    const nodesCopy = [...nodes];
    return {
        first: () => nodesCopy[0] || null,
        rest: () => (nodesCopy.length > 1 ? nodeListFromArray(nodesCopy.slice(1)) : null),
        length: () => nodesCopy.length,
        isEmpty: () => false,
        nth: (index) => nodesCopy[index] || null,
        toArray: () => [...nodesCopy],
    };
}
//# sourceMappingURL=grove.js.map