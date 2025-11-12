/**
 * Grove interfaces - Direct port from OpenJade grove/Node.{h,cxx} (2,400 lines)
 *
 * The grove is an abstract representation of a document tree following the
 * DSSSL grove model (ISO/IEC 10179:1996, §8).
 */

/**
 * Node interface - Represents a single node in the grove
 * Port from: grove/Node.h
 */
export interface Node {
  // Element properties (DSSSL §8.4.1)
  gi(): string | null;                    // Generic identifier (element name)
  id(): string | null;                    // ID attribute value

  // Character data (DSSSL §8.4.2)
  data(): string | null;                  // Character data content

  // Tree navigation (DSSSL §8.4.3)
  parent(): Node | null;                  // Parent node
  children(): NodeList;                   // Child nodes
  follow(): NodeList;                     // Following siblings
  preced(): NodeList;                     // Preceding siblings

  // Attributes (DSSSL §8.4.4)
  attributes(): NodeList;                 // Attribute nodes
  attributeString(name: string): string | null;  // Get attribute value

  // Entity/notation properties (DSSSL §8.4.5)
  entitySystemId(): string | null;
  entityPublicId(): string | null;
  entityType(): string | null;
  notationSystemId(): string | null;
  notationPublicId(): string | null;

  // Position queries (DSSSL §8.4.6)
  childNumber(): number;                  // Position among siblings
  firstSibling(): boolean;
  lastSibling(): boolean;

  // Type queries
  nodeType(): NodeType;
}

/**
 * NodeList interface - Represents a sequence of nodes
 * Port from: grove/Node.h NodeListObj
 */
export interface NodeList {
  first(): Node | null;                   // First node in list
  rest(): NodeList | null;                // Remaining nodes
  length(): number;                       // Total number of nodes
  isEmpty(): boolean;                     // Check if empty
}

/**
 * Grove interface - Root of the document grove
 * Port from: grove/Grove.h
 */
export interface Grove {
  root(): Node;                           // Document root
  elementWithId(id: string): Node | null; // Lookup by ID
}

/**
 * Node types following DSSSL specification
 */
export enum NodeType {
  Element = 'element',
  Text = 'text',
  ProcessingInstruction = 'processing-instruction',
  Attribute = 'attribute',
  Entity = 'entity',
  Notation = 'notation',
  Document = 'document',
  DocumentType = 'document-type',
}
