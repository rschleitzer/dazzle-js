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
export enum NodeType {
  Element = 'element',
  Text = 'text',
  ProcessingInstruction = 'processing-instruction',
  Comment = 'comment',
  Entity = 'entity',
  Notation = 'notation',
  Document = 'document',
  DocumentType = 'document-type',
  Attribute = 'attribute',
  AttributeDefinition = 'attribute-definition',
  ElementType = 'element-type',
}

/**
 * Node interface - represents a node in the grove
 * Port from: grove/Node.h class Node
 *
 * This is the core interface for all grove nodes. All node types
 * implement all methods. Methods return null when not applicable
 * to the node type (e.g., text.gi() returns null).
 *
 * OpenJade Note: In OpenJade, this is a pure virtual base class
 * with ~50 virtual methods. Implementations override all methods,
 * returning null for methods that don't apply to that node type.
 *
 * PERFORMANCE: Call methods directly, don't check nodeType() first.
 * Modern JS engines optimize polymorphic method calls efficiently.
 */
export interface Node {
  /**
   * Get the node type
   * Port from: Node.h nodeType()
   *
   * NOTE: Only use this for debugging or when you explicitly need
   * to branch on type. For property access, just call the method
   * directly (it returns null if not applicable).
   */
  nodeType(): NodeType;

  // ============ Basic Properties (DSSSL §8.3) ============

  /**
   * Generic identifier (element name)
   * Port from: Node.h gi()
   *
   * Returns the element type name (e.g., "chapter", "para")
   * Returns null for non-element nodes
   *
   * Usage: const gi = node.gi(); // Just call it, no type check needed
   */
  gi(): string | null;

  /**
   * ID attribute value
   * Port from: Node.h id()
   *
   * Returns the value of the ID attribute (if any)
   * Returns null if node has no ID or is not an element
   */
  id(): string | null;

  /**
   * Character data content
   * Port from: Node.h data()
   *
   * Returns the character data for text nodes
   * Returns null for non-text nodes
   */
  data(): string | null;

  /**
   * System data (PI target or entity name)
   * Port from: Node.h systemData()
   *
   * Returns target for processing instructions
   * Returns entity name for entity nodes
   * Returns null for other node types
   */
  systemData(): string | null;

  // ============ Navigation (DSSSL §9.2) ============

  /**
   * Parent node
   * Port from: Node.h parent()
   *
   * Returns the parent node in the grove
   * Returns null for the root node
   */
  parent(): Node | null;

  /**
   * Child nodes
   * Port from: Node.h children()
   *
   * Returns a node list of all children
   * Returns empty node list if no children
   */
  children(): NodeList;

  /**
   * Following sibling
   * Port from: Node.h followingSibling()
   *
   * Returns the next sibling node
   * Returns null if this is the last sibling
   */
  followingSibling(): Node | null;

  /**
   * Preceding sibling
   * Port from: Node.h precedingSibling()
   *
   * Returns the previous sibling node
   * Returns null if this is the first sibling
   */
  precedingSibling(): Node | null;

  /**
   * Ancestor nodes (parent, grandparent, etc.)
   * Port from: Node.h ancestors()
   *
   * Returns node list from parent to root
   * Empty if node has no parent
   */
  ancestors(): NodeList;

  /**
   * Descendant nodes (all descendants in document order)
   * Port from: Node.h descendants()
   *
   * Returns all descendants in document order
   * Empty if node has no descendants
   */
  descendants(): NodeList;

  /**
   * Following nodes (all nodes after this one in document order)
   * Port from: Node.h following()
   *
   * Returns all nodes that appear after this node
   * Empty if this is the last node
   */
  following(): NodeList;

  /**
   * Preceding nodes (all nodes before this one in document order)
   * Port from: Node.h preceding()
   *
   * Returns all nodes that appear before this node
   * Empty if this is the first node
   */
  preceding(): NodeList;

  // ============ Attributes (DSSSL §8.3.2) ============

  /**
   * Attribute nodes
   * Port from: Node.h attributes()
   *
   * Returns node list of attribute nodes
   * Empty for non-element nodes
   */
  attributes(): NodeList;

  /**
   * Get attribute value by name
   * Port from: Node.h attributeString()
   *
   * @param name Attribute name
   * @returns Attribute value or null if not present or not an element
   */
  attributeString(name: string): string | null;

  /**
   * Get inherited attribute value
   * Port from: Node.h inheritedAttributeString()
   *
   * Searches this node and ancestors for attribute
   * Implements DSSSL inherited-attribute-string
   *
   * @param name Attribute name
   * @returns Attribute value or null if not found
   */
  inheritedAttributeString(name: string): string | null;

  // ============ Position Information (DSSSL §9.3) ============

  /**
   * Check if this is the first sibling
   * Port from: Node.h isFirstSibling()
   */
  isFirstSibling(): boolean;

  /**
   * Check if this is the last sibling
   * Port from: Node.h isLastSibling()
   */
  isLastSibling(): boolean;

  /**
   * Child number (1-based index among siblings)
   * Port from: Node.h childNumber()
   *
   * Returns 1 for first child, 2 for second, etc.
   * Returns 0 if no parent
   */
  childNumber(): number;

  /**
   * Element number (1-based index among element siblings)
   * Port from: Node.h elementNumber()
   *
   * Like childNumber but only counts element siblings
   * Returns 0 if not an element or no parent
   */
  elementNumber(): number;

  // ============ Type Predicates ============
  // These are convenience methods, mostly for readability
  // Still faster to just call gi() and check for null

  /**
   * Check if this is an element node
   * Equivalent to: node.gi() !== null
   */
  isElement(): boolean;

  /**
   * Check if this is a text node
   * Equivalent to: node.data() !== null && node.gi() === null
   */
  isText(): boolean;

  /**
   * Check if this is a processing instruction
   */
  isProcessingInstruction(): boolean;

  /**
   * Check if this is a comment node
   */
  isComment(): boolean;

  // ============ Entity/Notation Information (DSSSL §8.4) ============

  /**
   * Entity system identifier
   * Port from: Node.h entitySystemId()
   *
   * Returns system ID for entity/notation nodes
   * Returns null for other node types
   */
  entitySystemId(): string | null;

  /**
   * Entity public identifier
   * Port from: Node.h entityPublicId()
   *
   * Returns public ID for entity/notation nodes
   * Returns null for other node types
   */
  entityPublicId(): string | null;

  /**
   * Entity text content (for internal entities)
   * Port from: Node.h entityText()
   *
   * Returns replacement text for internal entities
   * Returns null for external entities or non-entity nodes
   */
  entityText(): string | null;

  /**
   * Notation system identifier
   * Port from: Node.h notationSystemId()
   */
  notationSystemId(): string | null;

  /**
   * Notation public identifier
   * Port from: Node.h notationPublicId()
   */
  notationPublicId(): string | null;

  /**
   * Notation name (for entity nodes)
   * Port from: Node.h notationName()
   *
   * Returns the name of the notation for this entity
   * Returns null for non-entity nodes
   */
  notationName(): string | null;

  // ============ Additional Properties ============

  /**
   * Get generic node property by name
   * Port from: Node.h property()
   *
   * Provides access to any DSSSL property by name.
   * Mainly used by the node-property primitive.
   *
   * @param name Property name (e.g., "gi", "id", "data")
   * @returns Property value or null
   */
  property(name: string): unknown;

  /**
   * Document node (root of the grove)
   * Port from: Node.h documentNode()
   *
   * Returns the document node (root of grove)
   * Never returns null
   */
  documentNode(): Node;

  /**
   * Document element (root element, not document node)
   * Port from: Node.h documentElement()
   *
   * Returns the root element (e.g., <book> in a DocBook document)
   * Returns null if grove has no document element
   */
  documentElement(): Node | null;

  /**
   * Origin (for SGML subdocuments)
   * Port from: Node.h origin()
   *
   * Returns the original location for subdocument entities
   * Returns null for nodes that aren't in subdocuments
   */
  origin(): Node | null;
}

/**
 * NodeList interface - represents a list of nodes
 * Port from: grove/Node.h class NodeList
 *
 * DSSSL node lists are immutable and use functional-style
 * access (first/rest pattern, like Lisp).
 *
 * OpenJade Note: NodeList is also a pure virtual interface
 * with different implementations for efficiency (e.g.,
 * singleton lists, filtered lists, computed lists).
 *
 * PERFORMANCE: Node lists may be lazily evaluated. Don't
 * convert to arrays unless necessary.
 */
export interface NodeList {
  /**
   * First node in the list
   * Port from: NodeList.h first()
   *
   * Returns null if list is empty
   */
  first(): Node | null;

  /**
   * Rest of the list (all but first)
   * Port from: NodeList.h rest()
   *
   * Returns null if list is empty or has only one element
   */
  rest(): NodeList | null;

  /**
   * Length of the list
   * Port from: NodeList.h length()
   *
   * NOTE: May be O(n) for some implementations (e.g., filtered lists)
   */
  length(): number;

  /**
   * Check if list is empty
   * Equivalent to: list.first() === null
   */
  isEmpty(): boolean;

  /**
   * Get node at index (0-based)
   * Port from: NodeList.h nth()
   *
   * Returns null if index out of bounds
   */
  nth(index: number): Node | null;

  /**
   * Convert to array (for implementation convenience)
   * Not in OpenJade, but useful for TypeScript
   *
   * CAUTION: Forces evaluation of lazy lists. May be expensive.
   */
  toArray(): Node[];
}

/**
 * Grove interface - represents the entire document grove
 * Port from: grove/Node.h class Grove
 *
 * The grove is the top-level container for the document tree.
 * It provides access to the root node and global lookup operations.
 */
export interface Grove {
  /**
   * Root node of the grove (document node)
   * Port from: Grove.h root()
   *
   * Returns the document node (not the document element)
   */
  root(): Node;

  /**
   * Document element (root element, not document node)
   * Port from: Grove.h documentElement()
   *
   * Returns the root element (e.g., <book> in DocBook)
   * Returns null if grove has no document element
   */
  documentElement(): Node | null;

  /**
   * Find element by ID attribute
   * Port from: Grove.h elementWithId()
   *
   * Implements DSSSL element-with-id
   * @param id ID value to search for
   * @returns Element node with that ID, or null if not found
   */
  elementWithId(id: string): Node | null;

  /**
   * Get all entity declarations
   * Port from: Grove.h entities()
   *
   * Returns node list of all declared entities
   */
  entities(): NodeList;

  /**
   * Get entity by name
   * Port from: Grove.h entity()
   *
   * @param name Entity name
   * @returns Entity node or null if not found
   */
  entity(name: string): Node | null;

  /**
   * Get all notation declarations
   * Port from: Grove.h notations()
   *
   * Returns node list of all declared notations
   */
  notations(): NodeList;

  /**
   * Get notation by name
   * Port from: Grove.h notation()
   *
   * @param name Notation name
   * @returns Notation node or null if not found
   */
  notation(name: string): Node | null;
}

/**
 * Empty node list singleton
 * Port from: grove/Node.cxx emptyNodeList
 *
 * Shared instance for efficiency - node lists are immutable
 */
export const EMPTY_NODE_LIST: NodeList = {
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
export function nodeListFromArray(nodes: Node[]): NodeList {
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
    nth: (index: number) => nodesCopy[index] || null,
    toArray: () => [...nodesCopy],
  };
}
