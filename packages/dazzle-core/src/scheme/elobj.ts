/**
 * Scheme value system - Direct port from OpenJade style/ELObj.h
 *
 * ELObj is the base class for all Scheme values in OpenJade.
 * We port this to TypeScript using classes.
 */

import type { Node, NodeList } from '../grove';

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
  asKeyword(): KeywordObj | null { return null; }
  asPair(): PairObj | null { return null; }
  asVector(): VectorObj | null { return null; }
  asFunction(): FunctionObj | null { return null; }
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

/**
 * Primitive function callback type
 * Port from: ELObj.h PrimitiveObj::primitiveCall()
 */
export type PrimitiveFunction = (args: ELObj[]) => ELObj;

/**
 * Function (primitive or lambda)
 * Port from: ELObj.h FunctionObj, PrimitiveObj
 */
export class FunctionObj extends ELObj {
  constructor(
    public name: string | null,
    public primitive?: PrimitiveFunction,
    public code?: any // For closures/lambdas - will be Insn later
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
   * Call the primitive function
   * Port from: ELObj.h PrimitiveObj::primitiveCall()
   */
  callPrimitive(args: ELObj[]): ELObj {
    if (!this.primitive) {
      throw new Error('Not a primitive function');
    }
    return this.primitive(args);
  }
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
 */
export class SosofoObj extends ELObj {
  asSosofo(): SosofoObj { return this; }
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
