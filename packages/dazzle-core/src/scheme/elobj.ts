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
export abstract class ELObj {
  /** Type predicates - return specific subclass or null */
  asNil(): NilObj | null { return null; }
  asBoolean(): BooleanObj | null { return null; }
  asNumber(): NumberObj | null { return null; }
  asString(): StringObj | null { return null; }
  asSymbol(): SymbolObj | null { return null; }
  asChar(): CharObj | null { return null; }
  asKeyword(): KeywordObj | null { return null; }
  asPair(): PairObj | null { return null; }
  asVector(): VectorObj | null { return null; }
  asFunction(): FunctionObj | null { return null; }
  asNode(): NodeObj | null { return null; }
  asNodeList(): NodeListObj | null { return null; }
  asSosofo(): SosofoObj | null { return null; }

  /** Check if this is a proper list */
  isList(): boolean {
    return this.asNil() !== null;
  }

  /** Check if this is true (everything except #f is true in Scheme) */
  isTrue(): boolean {
    const b = this.asBoolean();
    return !(b && !b.value);
  }

  /** Get exact integer value */
  exactIntegerValue(): number | null { return null; }

  /** Get real value */
  realValue(): number | null { return null; }

  /** Get string data */
  stringData(): string | null { return null; }
}

/**
 * Nil (empty list)
 * Port from: ELObj.h (nil is special singleton)
 */
export class NilObj extends ELObj {
  asNil(): NilObj { return this; }
  isList(): boolean { return true; }
}

/**
 * Boolean (#t or #f)
 * Port from: ELObj.h (booleans are singletons)
 */
export class BooleanObj extends ELObj {
  constructor(public value: boolean) {
    super();
  }
  asBoolean(): BooleanObj { return this; }
}

/**
 * Number (integer or real)
 * Port from: ELObj.h exactIntegerValue(), realValue()
 */
export class NumberObj extends ELObj {
  constructor(
    public value: number,
    public exact: boolean = true
  ) {
    super();
  }

  asNumber(): NumberObj { return this; }

  exactIntegerValue(): number | null {
    return this.exact ? this.value : null;
  }

  realValue(): number | null {
    return this.value;
  }
}

/**
 * String
 * Port from: ELObj.h StringObj
 */
export class StringObj extends ELObj {
  constructor(public value: string) {
    super();
  }

  asString(): StringObj { return this; }
  stringData(): string { return this.value; }
}

/**
 * Character
 * Port from: ELObj.h CharObj
 */
export class CharObj extends ELObj {
  constructor(public value: string) {
    super();
  }

  asChar(): CharObj { return this; }
}

/**
 * Symbol
 * Port from: ELObj.h SymbolObj
 */
export class SymbolObj extends ELObj {
  constructor(public name: string) {
    super();
  }

  asSymbol(): SymbolObj { return this; }
  stringData(): string { return this.name; }
}

/**
 * Keyword (DSSSL trailing colon syntax: system-id:)
 * Port from: ELObj.h KeywordObj
 */
export class KeywordObj extends ELObj {
  constructor(public name: string) {
    super();
  }

  asKeyword(): KeywordObj { return this; }
}

/**
 * Pair (cons cell)
 * Port from: ELObj.h PairObj
 */
export class PairObj extends ELObj {
  constructor(
    public car: ELObj,
    public cdr: ELObj
  ) {
    super();
  }

  asPair(): PairObj { return this; }

  isList(): boolean {
    return this.cdr.isList();
  }
}

/**
 * Vector
 * Port from: ELObj.h VectorObj
 */
export class VectorObj extends ELObj {
  constructor(public elements: ELObj[]) {
    super();
  }

  asVector(): VectorObj { return this; }
}

// Forward declaration for VM (avoid circular dependency)
import type { VM } from './vm.js';

/**
 * Primitive function callback type
 * Port from: ELObj.h PrimitiveObj::primitiveCall(int nArgs, ELObj **args, EvalContext &context)
 */
export type PrimitiveFunction = (args: ELObj[], vm: VM) => ELObj;

/**
 * Function signature - describes expected arguments
 * Port from: Insn.h struct Signature
 */
export interface Signature {
  nRequiredArgs: number;   // Number of required arguments
  nOptionalArgs: number;   // Number of optional arguments
  restArg: boolean;        // Whether there's a rest argument
  nKeyArgs: number;        // Number of keyword arguments
}

/**
 * Function (primitive or lambda)
 * Port from: ELObj.h FunctionObj, PrimitiveObj, ClosureObj
 */
export class FunctionObj extends ELObj {
  constructor(
    public name: string | null,
    public primitive?: PrimitiveFunction,
    public code?: any, // For closures/lambdas - will be Insn
    public signature?: Signature,
    public display?: ELObj[] // Captured variables for closures
  ) {
    super();
  }

  asFunction(): FunctionObj { return this; }

  /**
   * Check if this is a primitive function
   */
  isPrimitive(): boolean {
    return this.primitive !== undefined;
  }

  /**
   * Check if this is a closure (user-defined function)
   */
  isClosure(): boolean {
    return this.code !== undefined && this.primitive === undefined;
  }

  /**
   * Call the primitive function
   * Port from: ELObj.h PrimitiveObj::primitiveCall()
   */
  callPrimitive(args: ELObj[], vm: VM): ELObj {
    if (!this.primitive) {
      throw new Error('Not a primitive function');
    }
    return this.primitive(args, vm);
  }
}

/**
 * Node (DSSSL grove node)
 * Port from: ELObj.h NodeObj
 */
export class NodeObj extends ELObj {
  constructor(public node: Node) {
    super();
  }

  asNode(): NodeObj { return this; }
}

/**
 * NodeList (DSSSL node list)
 * Port from: ELObj.h NodeListObj
 */
export class NodeListObj extends ELObj {
  constructor(public nodes: NodeList) {
    super();
  }

  asNodeList(): NodeListObj { return this; }
}

/**
 * Sosofo (Specification of a Sequence of Flow Objects)
 * Port from: ELObj.h SosofoObj
 *
 * A sosofo represents the result of DSSSL processing - a specification
 * of formatting objects that can be passed to a backend for output.
 */
export class SosofoObj extends ELObj {
  constructor(
    public type: 'empty' | 'append' | 'entity' | 'formatting-instruction' | 'literal',
    public data?: unknown
  ) {
    super();
  }

  asSosofo(): SosofoObj { return this; }

  /**
   * Check if this is an empty sosofo
   */
  isEmpty(): boolean {
    return this.type === 'empty';
  }

  /**
   * Get children for append sosofo
   */
  children(): SosofoObj[] {
    if (this.type === 'append' && Array.isArray(this.data)) {
      return this.data as SosofoObj[];
    }
    return [];
  }

  /**
   * Get literal text content
   */
  literalText(): string | null {
    if (this.type === 'literal' && typeof this.data === 'string') {
      return this.data;
    }
    return null;
  }
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
export function makeBoolean(value: boolean): BooleanObj {
  return value ? theTrueObj : theFalseObj;
}

export function makeNumber(value: number, exact = true): NumberObj {
  return new NumberObj(value, exact);
}

export function makeString(value: string): StringObj {
  return new StringObj(value);
}

export function makeChar(value: string): CharObj {
  return new CharObj(value);
}

export function makeSymbol(name: string): SymbolObj {
  return new SymbolObj(name);
}

export function makeKeyword(name: string): KeywordObj {
  return new KeywordObj(name);
}

export function makePair(car: ELObj, cdr: ELObj): PairObj {
  return new PairObj(car, cdr);
}

export function makeVector(elements: ELObj[]): VectorObj {
  return new VectorObj(elements);
}

export function makeNode(node: Node): NodeObj {
  return new NodeObj(node);
}

export function makeNodeList(nodes: NodeList): NodeListObj {
  return new NodeListObj(nodes);
}

export function makeSosofo(type: 'empty' | 'append' | 'entity' | 'formatting-instruction' | 'literal', data?: unknown): SosofoObj {
  return new SosofoObj(type, data);
}
