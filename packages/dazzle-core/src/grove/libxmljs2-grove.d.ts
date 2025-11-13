/**
 * libxmljs2 Grove Implementation
 *
 * Concrete grove implementation using libxmljs2 for Node.js
 *
 * PERFORMANCE CRITICAL:
 * This implementation is designed for multi-megabyte XML files and
 * complex DSSSL stylesheets (e.g., Norman Walsh's DocBook stylesheets).
 *
 * Key optimizations:
 * 1. Lightweight wrappers - Just hold reference to native node
 * 2. Wrapper caching - WeakMap to reuse wrappers for same native node
 * 3. Lazy NodeLists - Compute on demand, not eagerly
 * 4. Cache expensive traversals - ancestors, descendants, following, preceding
 * 5. Minimize allocations - Reuse objects where possible
 *
 * Based on: OpenJade spgrove/ (7K lines)
 */
import * as libxmljs2 from 'libxmljs2';
import { type Node, type NodeList, type Grove } from './grove.js';
/**
 * LibxmljsGrove - Concrete Grove implementation
 *
 * PERFORMANCE: Caches expensive lookups (element by ID)
 */
export declare class LibxmljsGrove implements Grove {
    private rootNode;
    private idCache?;
    constructor(doc: libxmljs2.Document);
    root(): Node;
    documentElement(): Node | null;
    elementWithId(id: string): Node | null;
    private buildIdCache;
    entities(): NodeList;
    entity(_name: string): Node | null;
    notations(): NodeList;
    notation(_name: string): Node | null;
}
/**
 * Parse XML and create a grove
 *
 * PERFORMANCE: Uses libxmljs2's native parser (libxml2)
 * which is very fast for large documents.
 *
 * @param xml XML string or Buffer
 * @param options Parse options
 * @returns Grove for the document
 */
export declare function parseXmlGrove(xml: string | Buffer, options?: {
    baseUrl?: string;
    dtdload?: boolean;
    dtdvalid?: boolean;
    noent?: boolean;
    nocdata?: boolean;
}): Grove;
//# sourceMappingURL=libxmljs2-grove.d.ts.map