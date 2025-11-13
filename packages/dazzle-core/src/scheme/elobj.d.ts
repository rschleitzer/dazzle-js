/**
 * Scheme value system - Direct port from OpenJade style/ELObj.h
 *
 * ELObj is the base class for all Scheme values in OpenJade.
 * We port this to TypeScript using classes.
 */
import type { Node, NodeList } from '../grove/index.js';
/**
 * Base class for all Scheme values
 * Port from: ELObj.h class ELObj
 */
export declare abstract class ELObj {
    /** Type predicates - return specific subclass or null */
    asNil(): NilObj | null;
    asBoolean(): BooleanObj | null;
    asNumber(): NumberObj | null;
    asString(): StringObj | null;
    asSymbol(): SymbolObj | null;
    asChar(): CharObj | null;
    asKeyword(): KeywordObj | null;
    asPair(): PairObj | null;
    asVector(): VectorObj | null;
    asFunction(): FunctionObj | null;
    asNode(): NodeObj | null;
    asNodeList(): NodeListObj | null;
    asSosofo(): SosofoObj | null;
    /** Check if this is a proper list */
    isList(): boolean;
    /** Check if this is true (everything except #f is true in Scheme) */
    isTrue(): boolean;
    /** Get exact integer value */
    exactIntegerValue(): number | null;
    /** Get real value */
    realValue(): number | null;
    /** Get string data */
    stringData(): string | null;
}
/**
 * Nil (empty list)
 * Port from: ELObj.h (nil is special singleton)
 */
export declare class NilObj extends ELObj {
    asNil(): NilObj;
    isList(): boolean;
}
/**
 * Boolean (#t or #f)
 * Port from: ELObj.h (booleans are singletons)
 */
export declare class BooleanObj extends ELObj {
    value: boolean;
    constructor(value: boolean);
    asBoolean(): BooleanObj;
}
/**
 * Number (integer or real)
 * Port from: ELObj.h exactIntegerValue(), realValue()
 */
export declare class NumberObj extends ELObj {
    value: number;
    exact: boolean;
    constructor(value: number, exact?: boolean);
    asNumber(): NumberObj;
    exactIntegerValue(): number | null;
    realValue(): number | null;
}
/**
 * String
 * Port from: ELObj.h StringObj
 */
export declare class StringObj extends ELObj {
    value: string;
    constructor(value: string);
    asString(): StringObj;
    stringData(): string;
}
/**
 * Character
 * Port from: ELObj.h CharObj
 */
export declare class CharObj extends ELObj {
    value: string;
    constructor(value: string);
    asChar(): CharObj;
}
/**
 * Symbol
 * Port from: ELObj.h SymbolObj
 */
export declare class SymbolObj extends ELObj {
    name: string;
    constructor(name: string);
    asSymbol(): SymbolObj;
    stringData(): string;
}
/**
 * Keyword (DSSSL trailing colon syntax: system-id:)
 * Port from: ELObj.h KeywordObj
 */
export declare class KeywordObj extends ELObj {
    name: string;
    constructor(name: string);
    asKeyword(): KeywordObj;
}
/**
 * Pair (cons cell)
 * Port from: ELObj.h PairObj
 */
export declare class PairObj extends ELObj {
    car: ELObj;
    cdr: ELObj;
    constructor(car: ELObj, cdr: ELObj);
    asPair(): PairObj;
    isList(): boolean;
}
/**
 * Vector
 * Port from: ELObj.h VectorObj
 */
export declare class VectorObj extends ELObj {
    elements: ELObj[];
    constructor(elements: ELObj[]);
    asVector(): VectorObj;
}
/**
 * Primitive function callback type
 * Port from: ELObj.h PrimitiveObj::primitiveCall()
 */
export type PrimitiveFunction = (args: ELObj[]) => ELObj;
/**
 * Function signature - describes expected arguments
 * Port from: Insn.h struct Signature
 */
export interface Signature {
    nRequiredArgs: number;
    nOptionalArgs: number;
    restArg: boolean;
    nKeyArgs: number;
}
/**
 * Function (primitive or lambda)
 * Port from: ELObj.h FunctionObj, PrimitiveObj, ClosureObj
 */
export declare class FunctionObj extends ELObj {
    name: string | null;
    primitive?: PrimitiveFunction | undefined;
    code?: any | undefined;
    signature?: Signature | undefined;
    display?: ELObj[] | undefined;
    constructor(name: string | null, primitive?: PrimitiveFunction | undefined, code?: any | undefined, // For closures/lambdas - will be Insn
    signature?: Signature | undefined, display?: ELObj[] | undefined);
    asFunction(): FunctionObj;
    /**
     * Check if this is a primitive function
     */
    isPrimitive(): boolean;
    /**
     * Check if this is a closure (user-defined function)
     */
    isClosure(): boolean;
    /**
     * Call the primitive function
     * Port from: ELObj.h PrimitiveObj::primitiveCall()
     */
    callPrimitive(args: ELObj[]): ELObj;
}
/**
 * Node (DSSSL grove node)
 * Port from: ELObj.h NodeObj
 */
export declare class NodeObj extends ELObj {
    node: Node;
    constructor(node: Node);
    asNode(): NodeObj;
}
/**
 * NodeList (DSSSL node list)
 * Port from: ELObj.h NodeListObj
 */
export declare class NodeListObj extends ELObj {
    nodes: NodeList;
    constructor(nodes: NodeList);
    asNodeList(): NodeListObj;
}
/**
 * Sosofo (Specification of a Sequence of Flow Objects)
 * Port from: ELObj.h SosofoObj
 */
export declare class SosofoObj extends ELObj {
    asSosofo(): SosofoObj;
}
/**
 * Singleton constants
 */
export declare const theNilObj: NilObj;
export declare const theTrueObj: BooleanObj;
export declare const theFalseObj: BooleanObj;
/**
 * Convenience constructors
 */
export declare function makeBoolean(value: boolean): BooleanObj;
export declare function makeNumber(value: number, exact?: boolean): NumberObj;
export declare function makeString(value: string): StringObj;
export declare function makeChar(value: string): CharObj;
export declare function makeSymbol(name: string): SymbolObj;
export declare function makeKeyword(name: string): KeywordObj;
export declare function makePair(car: ELObj, cdr: ELObj): PairObj;
export declare function makeVector(elements: ELObj[]): VectorObj;
export declare function makeNode(node: Node): NodeObj;
export declare function makeNodeList(nodes: NodeList): NodeListObj;
//# sourceMappingURL=elobj.d.ts.map