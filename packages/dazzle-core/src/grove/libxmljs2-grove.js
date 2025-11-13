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
import { NodeType, EMPTY_NODE_LIST, nodeListFromArray, } from './grove.js';
/**
 * Wrapper cache - Reuse Node wrappers for same native node
 *
 * PERFORMANCE: WeakMap ensures wrappers are GC'd when native nodes are.
 * This prevents creating multiple wrappers for the same node.
 */
const nodeWrapperCache = new WeakMap();
/**
 * Wrap a native libxmljs2 node
 *
 * PERFORMANCE: Always use this instead of `new LibxmljsNode()` to
 * ensure we reuse wrappers.
 */
function wrapNode(native) {
    if (!native)
        return null;
    let wrapper = nodeWrapperCache.get(native);
    if (!wrapper) {
        wrapper = new LibxmljsNode(native);
        nodeWrapperCache.set(native, wrapper);
    }
    return wrapper;
}
/**
 * Wrap an array of native nodes
 *
 * PERFORMANCE: Creates wrappers lazily as nodes are accessed
 */
function wrapNodes(natives) {
    return natives.map(n => wrapNode(n)).filter(n => n !== null);
}
/**
 * LibxmljsNode - Concrete Node implementation using libxmljs2
 *
 * PERFORMANCE: This class is designed to be lightweight.
 * - Only stores reference to native node (8 bytes)
 * - Methods delegate to native node (no intermediate data structures)
 * - Expensive operations are cached (memoization)
 * - No eager computation of properties
 */
class LibxmljsNode {
    native;
    // Cached expensive operations (lazy-initialized)
    _ancestors;
    _descendants;
    _following;
    _preceding;
    /**
     * Create a node wrapper
     *
     * IMPORTANT: Don't call this directly. Use wrapNode() to get cached wrapper.
     */
    constructor(native) {
        this.native = native;
    }
    // ============ Basic Properties ============
    nodeType() {
        const type = this.native.type();
        switch (type) {
            case 'element': return NodeType.Element;
            case 'text': return NodeType.Text;
            case 'comment': return NodeType.Comment;
            case 'attribute': return NodeType.Attribute;
            case 'pi': return NodeType.ProcessingInstruction;
            default: return NodeType.Element; // fallback
        }
    }
    gi() {
        return this.native.type() === 'element'
            ? this.native.name()
            : null;
    }
    id() {
        if (this.native.type() !== 'element')
            return null;
        // Try standard ID attributes
        const elem = this.native;
        return elem.attr('id')?.value()
            || elem.attr('xml:id')?.value()
            || null;
    }
    data() {
        return this.native.type() === 'text'
            ? this.native.text() // Text node has text() method
            : null;
    }
    systemData() {
        if (this.native.type() === 'pi') {
            // PI name is system data
            return this.native.name(); // PI node has name() method
        }
        return null;
    }
    // ============ Navigation ============
    parent() {
        return wrapNode(this.native.parent());
    }
    children() {
        if (this.native.type() !== 'element') {
            return EMPTY_NODE_LIST;
        }
        const elem = this.native;
        const childNodes = elem.childNodes();
        if (!childNodes || childNodes.length === 0) {
            return EMPTY_NODE_LIST;
        }
        // Create lazy node list - don't wrap all children upfront
        return new LazyNodeList(() => wrapNodes(childNodes));
    }
    followingSibling() {
        const parent = this.native.parent();
        if (!parent)
            return null;
        const siblings = parent.childNodes();
        if (!siblings)
            return null;
        const index = siblings.indexOf(this.native);
        if (index === -1 || index === siblings.length - 1) {
            return null;
        }
        return wrapNode(siblings[index + 1]);
    }
    precedingSibling() {
        const parent = this.native.parent();
        if (!parent)
            return null;
        const siblings = parent.childNodes();
        if (!siblings)
            return null;
        const index = siblings.indexOf(this.native);
        if (index <= 0) {
            return null;
        }
        return wrapNode(siblings[index - 1]);
    }
    ancestors() {
        // PERFORMANCE: Cache result - ancestors don't change
        if (!this._ancestors) {
            const ancestorNodes = [];
            let current = this.parent();
            while (current) {
                ancestorNodes.push(current);
                current = current.parent();
            }
            this._ancestors = nodeListFromArray(ancestorNodes);
        }
        return this._ancestors;
    }
    descendants() {
        // PERFORMANCE: Cache result - expensive operation
        if (!this._descendants) {
            this._descendants = new LazyNodeList(() => {
                const result = [];
                const collectDescendants = (node) => {
                    let children = node.children(); // Changed to let for reassignment
                    let child = children.first();
                    while (child) {
                        result.push(child);
                        collectDescendants(child);
                        const rest = children.rest();
                        child = rest ? rest.first() : null;
                        if (child) {
                            // Move to next in list
                            children = rest;
                        }
                    }
                };
                collectDescendants(this);
                return result;
            });
        }
        return this._descendants;
    }
    following() {
        // PERFORMANCE: Cache result - very expensive operation
        if (!this._following) {
            this._following = new LazyNodeList(() => {
                const result = [];
                // Get all nodes after this one in document order
                // Strategy: Go up to ancestors, collect following siblings and their descendants
                let current = this;
                while (current) {
                    let sibling = current.followingSibling();
                    while (sibling) {
                        result.push(sibling);
                        // Add all descendants of this sibling
                        const descendants = sibling.descendants().toArray();
                        result.push(...descendants);
                        sibling = sibling.followingSibling();
                    }
                    current = current.parent();
                }
                return result;
            });
        }
        return this._following;
    }
    preceding() {
        // PERFORMANCE: Cache result - very expensive operation
        if (!this._preceding) {
            this._preceding = new LazyNodeList(() => {
                const result = [];
                // Get all nodes before this one in document order
                let current = this;
                while (current) {
                    let sibling = current.precedingSibling();
                    while (sibling) {
                        // Add descendants first (they come before sibling in doc order)
                        const descendants = sibling.descendants().toArray();
                        result.unshift(...descendants);
                        result.unshift(sibling);
                        sibling = sibling.precedingSibling();
                    }
                    current = current.parent();
                }
                return result;
            });
        }
        return this._preceding;
    }
    // ============ Attributes ============
    attributes() {
        if (this.native.type() !== 'element') {
            return EMPTY_NODE_LIST;
        }
        const elem = this.native;
        const attrs = elem.attrs();
        if (!attrs || attrs.length === 0) {
            return EMPTY_NODE_LIST;
        }
        // PERFORMANCE: Lazy list - don't wrap attributes until accessed
        return new LazyNodeList(() => wrapNodes(attrs));
    }
    attributeString(name) {
        if (this.native.type() !== 'element') {
            return null;
        }
        const elem = this.native;
        const attr = elem.attr(name);
        return attr ? attr.value() : null;
    }
    inheritedAttributeString(name) {
        // Check this node first
        const value = this.attributeString(name);
        if (value !== null) {
            return value;
        }
        // Search ancestors
        let current = this.parent();
        while (current) {
            const value = current.attributeString(name);
            if (value !== null) {
                return value;
            }
            current = current.parent();
        }
        return null;
    }
    // ============ Position Information ============
    isFirstSibling() {
        return this.precedingSibling() === null;
    }
    isLastSibling() {
        return this.followingSibling() === null;
    }
    childNumber() {
        const parent = this.native.parent();
        if (!parent)
            return 0;
        const siblings = parent.childNodes();
        if (!siblings)
            return 0;
        const index = siblings.indexOf(this.native);
        return index === -1 ? 0 : index + 1; // 1-based
    }
    elementNumber() {
        if (this.native.type() !== 'element')
            return 0;
        const parent = this.native.parent();
        if (!parent)
            return 0;
        const siblings = parent.childNodes();
        if (!siblings)
            return 0;
        // Count element siblings before this one
        let count = 0;
        for (const sibling of siblings) {
            if (sibling === this.native) {
                return count + 1; // 1-based
            }
            if (sibling.type() === 'element') {
                count++;
            }
        }
        return 0;
    }
    // ============ Type Predicates ============
    isElement() {
        return this.native.type() === 'element';
    }
    isText() {
        return this.native.type() === 'text';
    }
    isProcessingInstruction() {
        return this.native.type() === 'pi';
    }
    isComment() {
        return this.native.type() === 'comment';
    }
    // ============ Entity/Notation Information ============
    // libxmljs2 doesn't expose entity/notation nodes directly
    // These return null for now
    entitySystemId() {
        return null;
    }
    entityPublicId() {
        return null;
    }
    entityText() {
        return null;
    }
    notationSystemId() {
        return null;
    }
    notationPublicId() {
        return null;
    }
    notationName() {
        return null;
    }
    // ============ Additional Properties ============
    property(name) {
        // Implement DSSSL property access
        switch (name) {
            case 'gi': return this.gi();
            case 'id': return this.id();
            case 'data': return this.data();
            case 'system-data': return this.systemData();
            case 'parent': return this.parent();
            case 'children': return this.children();
            case 'attributes': return this.attributes();
            default: return null;
        }
    }
    documentNode() {
        let current = this.native;
        while (current.parent()) {
            current = current.parent();
        }
        return wrapNode(current);
    }
    documentElement() {
        const doc = this.native.doc();
        if (!doc)
            return null;
        return wrapNode(doc.root());
    }
    origin() {
        // Not supported by libxmljs2
        return null;
    }
}
/**
 * LazyNodeList - NodeList that computes nodes on demand
 *
 * PERFORMANCE CRITICAL:
 * - Doesn't compute full list until needed
 * - Caches result after first computation
 * - Minimizes allocations for operations like first() that don't need full list
 */
class LazyNodeList {
    compute;
    cached;
    constructor(compute) {
        this.compute = compute;
    }
    ensureCached() {
        if (!this.cached) {
            this.cached = this.compute();
        }
        return this.cached;
    }
    first() {
        if (this.cached) {
            return this.cached[0] || null;
        }
        // Optimize: Could compute just first element for some cases
        // For now, compute full list (simplicity vs optimization trade-off)
        return this.ensureCached()[0] || null;
    }
    rest() {
        const all = this.ensureCached();
        if (all.length <= 1)
            return null;
        return nodeListFromArray(all.slice(1));
    }
    length() {
        return this.ensureCached().length;
    }
    isEmpty() {
        return this.length() === 0;
    }
    nth(index) {
        return this.ensureCached()[index] || null;
    }
    toArray() {
        return [...this.ensureCached()];
    }
}
/**
 * LibxmljsGrove - Concrete Grove implementation
 *
 * PERFORMANCE: Caches expensive lookups (element by ID)
 */
export class LibxmljsGrove {
    rootNode;
    idCache;
    constructor(doc) {
        const nativeRoot = doc.root();
        if (!nativeRoot) {
            throw new Error('Document has no root element');
        }
        this.rootNode = wrapNode(nativeRoot);
    }
    root() {
        return this.rootNode;
    }
    documentElement() {
        return this.rootNode;
    }
    elementWithId(id) {
        // PERFORMANCE: Build ID cache on first lookup
        if (!this.idCache) {
            this.idCache = new Map();
            this.buildIdCache(this.rootNode);
        }
        return this.idCache.get(id) || null;
    }
    buildIdCache(node) {
        const nodeId = node.id();
        if (nodeId) {
            this.idCache.set(nodeId, node);
        }
        // Recurse into children
        const children = node.children();
        let child = children.first();
        let currentList = children;
        while (child) {
            this.buildIdCache(child);
            const rest = currentList.rest();
            if (!rest)
                break;
            child = rest.first();
            currentList = rest;
        }
    }
    entities() {
        // libxmljs2 doesn't expose entity declarations
        return EMPTY_NODE_LIST;
    }
    entity(_name) {
        return null;
    }
    notations() {
        // libxmljs2 doesn't expose notation declarations
        return EMPTY_NODE_LIST;
    }
    notation(_name) {
        return null;
    }
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
export function parseXmlGrove(xml, options) {
    // Convert Buffer to string if needed
    const xmlString = typeof xml === 'string' ? xml : xml.toString('utf-8');
    const doc = libxmljs2.parseXml(xmlString, {
        baseUrl: options?.baseUrl,
        dtdload: options?.dtdload ?? false,
        dtdvalid: options?.dtdvalid ?? false,
        noent: options?.noent ?? true, // Expand entities
        nocdata: options?.nocdata ?? false,
    });
    return new LibxmljsGrove(doc);
}
//# sourceMappingURL=libxmljs2-grove.js.map