/**
 * Scheme value system - Direct port from OpenJade style/ELObj.h
 *
 * ELObj is the base class for all Scheme values in OpenJade.
 * We port this to TypeScript using classes.
 */
/**
 * Base class for all Scheme values
 * Port from: ELObj.h class ELObj
 */
export class ELObj {
    /** Type predicates - return specific subclass or null */
    asNil() { return null; }
    asBoolean() { return null; }
    asNumber() { return null; }
    asString() { return null; }
    asSymbol() { return null; }
    asChar() { return null; }
    asKeyword() { return null; }
    asPair() { return null; }
    asVector() { return null; }
    asFunction() { return null; }
    asNode() { return null; }
    asNodeList() { return null; }
    asSosofo() { return null; }
    /** Check if this is a proper list */
    isList() {
        return this.asNil() !== null;
    }
    /** Check if this is true (everything except #f is true in Scheme) */
    isTrue() {
        const b = this.asBoolean();
        return !(b && !b.value);
    }
    /** Get exact integer value */
    exactIntegerValue() { return null; }
    /** Get real value */
    realValue() { return null; }
    /** Get string data */
    stringData() { return null; }
}
/**
 * Nil (empty list)
 * Port from: ELObj.h (nil is special singleton)
 */
export class NilObj extends ELObj {
    asNil() { return this; }
    isList() { return true; }
}
/**
 * Boolean (#t or #f)
 * Port from: ELObj.h (booleans are singletons)
 */
export class BooleanObj extends ELObj {
    value;
    constructor(value) {
        super();
        this.value = value;
    }
    asBoolean() { return this; }
}
/**
 * Number (integer or real)
 * Port from: ELObj.h exactIntegerValue(), realValue()
 */
export class NumberObj extends ELObj {
    value;
    exact;
    constructor(value, exact = true) {
        super();
        this.value = value;
        this.exact = exact;
    }
    asNumber() { return this; }
    exactIntegerValue() {
        return this.exact ? this.value : null;
    }
    realValue() {
        return this.value;
    }
}
/**
 * String
 * Port from: ELObj.h StringObj
 */
export class StringObj extends ELObj {
    value;
    constructor(value) {
        super();
        this.value = value;
    }
    asString() { return this; }
    stringData() { return this.value; }
}
/**
 * Character
 * Port from: ELObj.h CharObj
 */
export class CharObj extends ELObj {
    value;
    constructor(value) {
        super();
        this.value = value;
    }
    asChar() { return this; }
}
/**
 * Symbol
 * Port from: ELObj.h SymbolObj
 */
export class SymbolObj extends ELObj {
    name;
    constructor(name) {
        super();
        this.name = name;
    }
    asSymbol() { return this; }
    stringData() { return this.name; }
}
/**
 * Keyword (DSSSL trailing colon syntax: system-id:)
 * Port from: ELObj.h KeywordObj
 */
export class KeywordObj extends ELObj {
    name;
    constructor(name) {
        super();
        this.name = name;
    }
    asKeyword() { return this; }
}
/**
 * Pair (cons cell)
 * Port from: ELObj.h PairObj
 */
export class PairObj extends ELObj {
    car;
    cdr;
    constructor(car, cdr) {
        super();
        this.car = car;
        this.cdr = cdr;
    }
    asPair() { return this; }
    isList() {
        return this.cdr.isList();
    }
}
/**
 * Vector
 * Port from: ELObj.h VectorObj
 */
export class VectorObj extends ELObj {
    elements;
    constructor(elements) {
        super();
        this.elements = elements;
    }
    asVector() { return this; }
}
/**
 * Function (primitive or lambda)
 * Port from: ELObj.h FunctionObj, PrimitiveObj, ClosureObj
 */
export class FunctionObj extends ELObj {
    name;
    primitive;
    code;
    signature;
    display;
    constructor(name, primitive, code, // For closures/lambdas - will be Insn
    signature, display // Captured variables for closures
    ) {
        super();
        this.name = name;
        this.primitive = primitive;
        this.code = code;
        this.signature = signature;
        this.display = display;
    }
    asFunction() { return this; }
    /**
     * Check if this is a primitive function
     */
    isPrimitive() {
        return this.primitive !== undefined;
    }
    /**
     * Check if this is a closure (user-defined function)
     */
    isClosure() {
        return this.code !== undefined && this.primitive === undefined;
    }
    /**
     * Call the primitive function
     * Port from: ELObj.h PrimitiveObj::primitiveCall()
     */
    callPrimitive(args) {
        if (!this.primitive) {
            throw new Error('Not a primitive function');
        }
        return this.primitive(args);
    }
}
/**
 * Node (DSSSL grove node)
 * Port from: ELObj.h NodeObj
 */
export class NodeObj extends ELObj {
    node;
    constructor(node) {
        super();
        this.node = node;
    }
    asNode() { return this; }
}
/**
 * NodeList (DSSSL node list)
 * Port from: ELObj.h NodeListObj
 */
export class NodeListObj extends ELObj {
    nodes;
    constructor(nodes) {
        super();
        this.nodes = nodes;
    }
    asNodeList() { return this; }
}
/**
 * Sosofo (Specification of a Sequence of Flow Objects)
 * Port from: ELObj.h SosofoObj
 */
export class SosofoObj extends ELObj {
    asSosofo() { return this; }
}
/**
 * Singleton constants
 */
export const theNilObj = new NilObj();
export const theTrueObj = new BooleanObj(true);
export const theFalseObj = new BooleanObj(false);
/**
 * Convenience constructors
 */
export function makeBoolean(value) {
    return value ? theTrueObj : theFalseObj;
}
export function makeNumber(value, exact = true) {
    return new NumberObj(value, exact);
}
export function makeString(value) {
    return new StringObj(value);
}
export function makeChar(value) {
    return new CharObj(value);
}
export function makeSymbol(name) {
    return new SymbolObj(name);
}
export function makeKeyword(name) {
    return new KeywordObj(name);
}
export function makePair(car, cdr) {
    return new PairObj(car, cdr);
}
export function makeVector(elements) {
    return new VectorObj(elements);
}
export function makeNode(node) {
    return new NodeObj(node);
}
export function makeNodeList(nodes) {
    return new NodeListObj(nodes);
}
//# sourceMappingURL=elobj.js.map