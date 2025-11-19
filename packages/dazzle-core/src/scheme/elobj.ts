/**
 * Scheme value system - Direct port from OpenJade style/ELObj.h
 *
 * ELObj is the base class for all Scheme values in OpenJade.
 * We port this to TypeScript using classes.
 */

import type { Node, NodeList } from '../grove/index.js';

/**
 * Source location (forward declaration from insn.ts to avoid circular dependency)
 */
export interface Location {
  file: string;
  line: number;
  column: number;
}

/**
 * Base class for all Scheme values
 * Port from: ELObj.h class ELObj
 */
export abstract class ELObj {
  /**
   * Source location where this value was parsed
   * Port from: OpenJade ELObj location tracking
   */
  public location: Location = { file: '<unknown>', line: 0, column: 0 };

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
  asQuantity(): QuantityObj | null { return null; }
  asBox(): BoxObj | null { return null; }
  asColor(): ColorObj | null { return null; }
  asColorSpace(): ColorSpaceObj | null { return null; }
  asAddress(): AddressObj | null { return null; }

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
  optionalDefaults?: any[];  // Compiled instructions for optional parameter default values (Insn[])
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
 * Quantity - length with unit
 * Port from: DSSSL spec - quantities (lengths, etc.)
 *
 * Represents a dimension with a unit (e.g., 10cm, 12pt, 1in)
 */
export class QuantityObj extends ELObj {
  constructor(
    public value: number,
    public unit: string  // 'cm', 'mm', 'in', 'pt', 'px', 'em', etc.
  ) {
    super();
  }

  asQuantity(): QuantityObj { return this; }

  toString(): string {
    return `${this.value}${this.unit}`;
  }
}

/**
 * Unresolved Quantity - Port from OpenJade UnresolvedQuantityObj
 *
 * Represents a quantity literal like "10cm" before unit resolution.
 * OpenJade stores these and resolves them later via resolveQuantities().
 *
 * Port from: ELObj.h class UnresolvedQuantityObj
 */
export class UnresolvedQuantityObj extends ELObj {
  constructor(
    public value: number,
    public unitName: string,
    public unitExp: number = 1
  ) {
    super();
  }

  /**
   * Resolve this unresolved quantity to a QuantityObj
   * Port from: OpenJade UnresolvedQuantityObj::resolveQuantities
   *
   * @param unitRegistry Map of unit names to conversion factors (to inches)
   */
  resolve(unitRegistry: Map<string, number>): QuantityObj | null {
    const unitValue = unitRegistry.get(this.unitName);
    if (unitValue === undefined) {
      return null;  // Unknown unit
    }

    // Calculate value in inches (OpenJade's internal representation)
    const valueInInches = this.value * Math.pow(unitValue, this.unitExp);

    // Return QuantityObj with original unit for display
    return new QuantityObj(valueInInches, this.unitName);
  }

  toString(): string {
    if (this.unitExp === 1) {
      return `${this.value}${this.unitName}`;
    }
    return `${this.value}${this.unitName}${this.unitExp}`;
  }
}

/**
 * ColorSpace - Port from OpenJade ColorSpaceObj
 *
 * Represents a color space (Device RGB, Gray, CMYK, etc.)
 * Port from: ELObj.h class ColorSpaceObj
 */
export class ColorSpaceObj extends ELObj {
  constructor(
    public family: string  // 'Device RGB', 'Device Gray', 'Device CMYK', etc.
  ) {
    super();
  }

  asColorSpace(): ColorSpaceObj { return this; }

  /**
   * Create a color in this color space
   * Port from: OpenJade ColorSpaceObj::makeColor
   */
  makeColor(components: number[]): ColorObj {
    return new ColorObj(this, components);
  }

  toString(): string {
    return `<color-space ${this.family}>`;
  }
}

/**
 * Color - Port from OpenJade ColorObj
 *
 * Represents a color value in a specific color space
 * Port from: ELObj.h class ColorObj
 */
export class ColorObj extends ELObj {
  constructor(
    public colorSpace: ColorSpaceObj,
    public components: number[]  // RGB, Gray, or CMYK values
  ) {
    super();
  }

  asColor(): ColorObj { return this; }

  toString(): string {
    return `<color ${this.components.join(' ')}>`;
  }
}

/**
 * Sosofo (Specification of a Sequence of Flow Objects)
 * Port from: OpenJade style/SosofoObj.h class SosofoObj : public ELObj
 *
 * A sosofo represents the result of DSSSL processing - a specification
 * of formatting objects that can be passed to a backend for output.
 *
 * In OpenJade, this is an abstract base class. FlowObj extends SosofoObj.
 * Here we use it both as a base class and for simple sosofo types.
 */
export class SosofoObj extends ELObj {
  public type?: 'empty' | 'append' | 'entity' | 'directory' | 'formatting-instruction' | 'literal' | 'page-number';
  public data?: unknown;

  constructor(type?: 'empty' | 'append' | 'entity' | 'directory' | 'formatting-instruction' | 'literal' | 'page-number', data?: unknown) {
    super();
    this.type = type;
    this.data = data;
  }

  asSosofo(): SosofoObj { return this; }

  /**
   * Process this sosofo with a ProcessContext
   * Port from: OpenJade SosofoObj::process()
   *
   * Must be implemented by subclasses (like FlowObj)
   * Base implementation handles simple types
   */
  process(_context: any): void {
    // Base implementation - subclasses override
    // Simple sosofos (empty, literal, etc.) are handled by ProcessContext
  }

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

/**
 * Symbol table for interning symbols
 * Port from: OpenJade Interpreter::symbolTable_
 * Ensures that symbols with the same name are the same object (for === equality)
 */
const symbolTable = new Map<string, SymbolObj>();

/**
 * Create or retrieve an interned symbol
 * Port from: OpenJade Interpreter::makeSymbol
 */
export function makeSymbol(name: string): SymbolObj {
  // Check if symbol already exists in table
  let sym = symbolTable.get(name);
  if (!sym) {
    // Create new symbol and intern it
    sym = new SymbolObj(name);
    symbolTable.set(name, sym);
  }
  return sym;
}

export function makeKeyword(name: string): KeywordObj {
  return new KeywordObj(name);
}

/**
 * Box object - Mutable container for values
 * Port from: OpenJade BoxObj
 * Used for letrec variables that need to be captured before initialization
 */
export class BoxObj extends ELObj {
  constructor(public value: ELObj) {
    super();
  }

  asBox(): BoxObj | null {
    return this;
  }
}

/**
 * Address type enumeration
 * Port from: OpenJade style/FOTBuilder.h struct Address::Type
 */
export enum AddressType {
  // An address of #f
  none = 'none',
  // An address that was resolved by the front-end to a node
  // Only the node member is valid.
  resolvedNode = 'resolvedNode',
  // node contains current node, params[0] is string.
  idref = 'idref',
  entity = 'entity',
  sgmlDocument = 'sgmlDocument',
  hytimeLinkend = 'hytimeLinkend',
  tei = 'tei',
  html = 'html'
}

/**
 * Address object - Reference to a document location
 * Port from: OpenJade style/ELObj.h class AddressObj
 *
 * Used for creating links and references in DSSSL output.
 */
export class AddressObj extends ELObj {
  constructor(
    public addressType: AddressType,
    public node: Node | null = null,
    public params: [string, string, string] = ['', '', '']
  ) {
    super();
  }

  asAddress(): AddressObj | null {
    return this;
  }
}

export function makeBox(value: ELObj): BoxObj {
  return new BoxObj(value);
}

export function makeAddress(
  addressType: AddressType,
  node: Node | null = null,
  params: [string, string, string] = ['', '', '']
): AddressObj {
  return new AddressObj(addressType, node, params);
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

export function makeQuantity(value: number, unit: string): QuantityObj {
  return new QuantityObj(value, unit);
}

export function makeUnresolvedQuantity(value: number, unitName: string, unitExp: number = 1): UnresolvedQuantityObj {
  return new UnresolvedQuantityObj(value, unitName, unitExp);
}

export function makeSosofo(type: 'empty' | 'append' | 'entity' | 'directory' | 'formatting-instruction' | 'literal' | 'page-number', data?: unknown): SosofoObj {
  return new SosofoObj(type, data);
}

export function makeColorSpace(family: string): ColorSpaceObj {
  return new ColorSpaceObj(family);
}

export function makeColor(colorSpace: ColorSpaceObj, components: number[]): ColorObj {
  return new ColorObj(colorSpace, components);
}
