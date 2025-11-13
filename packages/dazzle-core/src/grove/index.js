/**
 * Grove module - Abstract grove interface for DSSSL
 *
 * Port from: OpenJade grove/ directory
 *
 * This module defines the abstract grove interface that separates
 * the DSSSL interpreter from specific grove implementations
 * (e.g., libxmljs2 for Node.js, DOM for browser).
 */
export { NodeType, nodeListFromArray, } from './grove.js';
// Re-export EMPTY_NODE_LIST from grove.ts
import { EMPTY_NODE_LIST as _EMPTY_NODE_LIST } from './grove.js';
export const EMPTY_NODE_LIST = _EMPTY_NODE_LIST;
export { LibxmljsGrove, parseXmlGrove, } from './libxmljs2-grove.js';
//# sourceMappingURL=index.js.map