/**
 * Grove module - Abstract grove interface for DSSSL
 *
 * Port from: OpenJade grove/ directory
 *
 * This module defines the abstract grove interface that separates
 * the DSSSL interpreter from specific grove implementations
 * (e.g., libxmljs2 for Node.js, DOM for browser).
 */
export { type Node, type NodeList, type Grove, NodeType, nodeListFromArray, } from './grove.js';
export declare const EMPTY_NODE_LIST: import("./grove.js").NodeList;
export { LibxmljsGrove, parseXmlGrove, } from './libxmljs2-grove.js';
//# sourceMappingURL=index.d.ts.map