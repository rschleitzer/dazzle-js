/**
 * Scheme Primitives - Basic built-in functions
 * Port from: OpenJade style/primitive.cxx (5,704 lines)
 *
 * Starting with essential R4RS primitives to make the VM usable.
 * Will expand to full 260 primitives as needed.
 */

import {
  type ELObj,
  type PrimitiveFunction,
  FunctionObj,
  PairObj,
  SosofoObj,
  makeNumber,
  makeBoolean,
  makePair,
  makeString,
  makeSymbol,
  makeVector,
  makeChar,
  makeNode,
  makeNodeList,
  makeSosofo,
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj.js';

import type { VM } from './vm.js';
import type { Node, NodeList } from '../grove/index.js';
import { EMPTY_NODE_LIST, nodeListFromArray } from '../grove/index.js';
import { CallInsn } from './insn.js';

/**
 * Helper: Make file path relative to current working directory
 */
function makeRelativePath(filePath: string): string {
  // Handle special case for unknown files
  if (filePath === '<unknown>' || !filePath) {
    return filePath;
  }

  // For Node.js environment
  if (typeof process !== 'undefined' && process.cwd) {
    try {
      // Simple relative path calculation
      const cwd = process.cwd();
      if (filePath.startsWith(cwd)) {
        const relative = filePath.slice(cwd.length);
        // Remove leading slash
        return relative.startsWith('/') ? relative.slice(1) : relative;
      }
    } catch (e) {
      // Fall back to absolute path if something goes wrong
    }
  }

  // Return absolute path as fallback
  return filePath;
}

/**
 * Helper: Call a closure from within a primitive
 * Creates necessary bytecode to invoke the closure with arguments
 */
function callClosure(func: FunctionObj, args: ELObj[], vm: VM): ELObj {
  if (!func.isClosure()) {
    throw new Error('callClosure requires a closure');
  }

  // Port from: OpenJade - must save/restore stack state
  // Record initial stack size to restore after call
  const initialStackSize = vm.stackSize();

  // Push arguments onto stack
  for (const arg of args) {
    vm.push(arg);
  }

  // Push function onto stack
  vm.push(func);

  // Create and execute CallInsn
  const callInsn = new CallInsn(args.length, { file: '<apply>', line: 0, column: 0 }, null);
  let next = callInsn.execute(vm);

  // Execute the continuation until completion
  // (The closure body will execute and return via ReturnInsn)
  while (next) {
    next = next.execute(vm);
  }

  // Result is now on top of stack
  const result = vm.pop();

  // CRITICAL FIX: Restore stack to initial state
  // The closure execution might have left values on the stack (e.g. in nested calls)
  // We need to pop everything down to the initial stack size
  while (vm.stackSize() > initialStackSize) {
    vm.pop();
  }

  return result;
}

/**
 * Helper: Convert argument to optional singleton node
 * Port from: OpenJade ELObj::optSingletonNodeList
 *
 * Accepts:
 * - A node → returns the node
 * - A singleton node-list → returns the single node
 * - An empty node-list → returns null
 * - Multiple nodes → throws error
 *
 * This matches OpenJade's optSingletonNodeList pattern:
 * - Returns false (error) for multi-node lists
 * - Returns true (ok) for singleton/empty, with node possibly null
 */
function optSingletonNode(obj: ELObj): Node | null {
  // Direct node
  const node = obj.asNode();
  if (node) {
    return node.node;
  }

  // Node-list
  const nl = obj.asNodeList();
  if (nl) {
    const first = nl.nodes.first();
    if (!first) {
      // Empty node-list - return null (not an error)
      return null;
    }

    const rest = nl.nodes.rest();
    if (rest && rest.first() !== null) {
      // Multiple nodes - ERROR
      throw new Error('expected singleton node-list, got multiple nodes');
    }

    // Singleton node-list
    return first;
  }

  // Not a node or node-list - error (OpenJade behavior)
  // Port from: OpenJade returns false from optSingletonNodeList, causing argError
  throw new Error(`not an optional singleton node: ${obj.constructor.name}`);
}

/**
 * Arithmetic primitive: +
 * Port from: primitive.cxx Plus::primitiveCall
 */
const plusPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  let sum = 0;
  let exact = true;

  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('+ requires numeric arguments');
    }
    sum += num.value;
    if (!num.exact) {
      exact = false;
    }
  }

  return makeNumber(sum, exact);
};

/**
 * Arithmetic primitive: -
 * Port from: primitive.cxx Minus::primitiveCall
 */
const minusPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    throw new Error('- requires at least 1 argument');
  }

  const first = args[0].asNumber();
  if (!first) {
    throw new Error('- requires numeric arguments');
  }

  if (args.length === 1) {
    // Unary negation
    return makeNumber(-first.value, first.exact);
  }

  // Subtraction
  let result = first.value;
  let exact = first.exact;

  for (let i = 1; i < args.length; i++) {
    const num = args[i].asNumber();
    if (!num) {
      throw new Error('- requires numeric arguments');
    }
    result -= num.value;
    if (!num.exact) {
      exact = false;
    }
  }

  return makeNumber(result, exact);
};

/**
 * Arithmetic primitive: *
 * Port from: primitive.cxx Times::primitiveCall
 */
const timesPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  let product = 1;
  let exact = true;

  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('* requires numeric arguments');
    }
    product *= num.value;
    if (!num.exact) {
      exact = false;
    }
  }

  return makeNumber(product, exact);
};

/**
 * Arithmetic primitive: /
 * Port from: primitive.cxx Divide::primitiveCall
 */
const dividePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    throw new Error('/ requires at least 1 argument');
  }

  const first = args[0].asNumber();
  if (!first) {
    throw new Error('/ requires numeric arguments');
  }

  if (args.length === 1) {
    // Reciprocal
    if (first.value === 0) {
      throw new Error('Division by zero');
    }
    return makeNumber(1 / first.value, false); // Always inexact for division
  }

  // Division
  let result = first.value;

  for (let i = 1; i < args.length; i++) {
    const num = args[i].asNumber();
    if (!num) {
      throw new Error('/ requires numeric arguments');
    }
    if (num.value === 0) {
      throw new Error('Division by zero');
    }
    result /= num.value;
  }

  return makeNumber(result, false); // Always inexact for division
};

/**
 * Comparison primitive: <
 * Port from: primitive.cxx LessThan::primitiveCall
 */
const lessThanPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('< requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const a = args[i].asNumber();
    const b = args[i + 1].asNumber();
    if (!a || !b) {
      throw new Error('< requires numeric arguments');
    }
    if (!(a.value < b.value)) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Comparison primitive: >
 * Port from: primitive.cxx GreaterThan::primitiveCall
 */
const greaterThanPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('> requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const a = args[i].asNumber();
    const b = args[i + 1].asNumber();
    if (!a || !b) {
      throw new Error('> requires numeric arguments');
    }
    if (!(a.value > b.value)) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Comparison primitive: =
 * Port from: primitive.cxx NumericEqual::primitiveCall
 */
const numericEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('= requires at least 2 arguments');
  }

  const first = args[0].asNumber();
  if (!first) {
    throw new Error('= requires numeric arguments');
  }

  for (let i = 1; i < args.length; i++) {
    const num = args[i].asNumber();
    if (!num) {
      throw new Error('= requires numeric arguments');
    }
    if (num.value !== first.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * List primitive: cons
 * Port from: primitive.cxx Cons::primitiveCall
 */
const consPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('cons requires exactly 2 arguments');
  }
  return makePair(args[0], args[1]);
};

/**
 * List primitive: car
 * Port from: primitive.cxx Car::primitiveCall
 */
const carPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('car requires exactly 1 argument');
  }

  const pair = args[0].asPair();
  if (!pair) {
    throw new Error('car requires a pair');
  }

  return pair.car;
};

/**
 * List primitive: cdr
 * Port from: primitive.cxx Cdr::primitiveCall
 */
const cdrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdr requires exactly 1 argument');
  }

  const pair = args[0].asPair();
  if (!pair) {
    throw new Error('cdr requires a pair');
  }

  return pair.cdr;
};

/**
 * List primitive: null?
 * Port from: primitive.cxx Null::primitiveCall
 */
const nullPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('null? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asNil() !== null);
};

/**
 * Type predicate: pair?
 * Port from: primitive.cxx Pair::primitiveCall
 */
const pairPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('pair? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asPair() !== null);
};

/**
 * Type predicate: number?
 * Port from: primitive.cxx Number::primitiveCall
 */
const numberPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('number? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asNumber() !== null);
};

/**
 * Logic primitive: not
 * Port from: primitive.cxx Not::primitiveCall
 */
const notPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('not requires exactly 1 argument');
  }

  return makeBoolean(!args[0].isTrue());
};

/**
 * Comparison primitive: <=
 * Port from: primitive.cxx LessThanOrEqual::primitiveCall
 */
const lessThanOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('<= requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const a = args[i].asNumber();
    const b = args[i + 1].asNumber();
    if (!a || !b) {
      throw new Error('<= requires numeric arguments');
    }
    if (!(a.value <= b.value)) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Comparison primitive: >=
 * Port from: primitive.cxx GreaterThanOrEqual::primitiveCall
 */
const greaterThanOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('>= requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const a = args[i].asNumber();
    const b = args[i + 1].asNumber();
    if (!a || !b) {
      const aType = !a ? args[i].constructor.name : 'Number';
      const bType = !b ? args[i + 1].constructor.name : 'Number';
      throw new Error(`>= requires numeric arguments, got ${aType} and ${bType}`);
    }
    if (!(a.value >= b.value)) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * List primitive: length
 * Port from: primitive.cxx Length::primitiveCall
 */
const lengthPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('length requires exactly 1 argument');
  }

  let count = 0;
  let current = args[0];

  while (!current.asNil()) {
    const pair = current.asPair();
    if (!pair) {
      throw new Error('length requires a proper list');
    }
    count++;
    current = pair.cdr;
  }

  return makeNumber(count, true);
};

/**
 * List primitive: list
 * Port from: primitive.cxx List::primitiveCall
 */
const listPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  let result: ELObj = theNilObj;

  // Build list from right to left
  for (let i = args.length - 1; i >= 0; i--) {
    result = makePair(args[i], result);
  }

  return result;
};

/**
 * List primitive: append
 * Port from: primitive.cxx Append::primitiveCall
 */
const appendPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  if (args.length === 1) {
    return args[0];
  }

  // Helper to copy a list
  const copyList = (list: ELObj): ELObj => {
    if (list.asNil()) {
      return theNilObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('append requires proper lists');
    }

    return makePair(pair.car, copyList(pair.cdr));
  };

  // Build result from right to left
  let result = args[args.length - 1];

  for (let i = args.length - 2; i >= 0; i--) {
    let current = args[i];

    if (current.asNil()) {
      continue;
    }

    // Copy this list and append to result
    const copied = copyList(current);

    // Find the last pair in copied list
    let last = copied;
    while (true) {
      const pair = last.asPair();
      if (!pair) break;

      const cdrPair = pair.cdr.asPair();
      if (!cdrPair) {
        // Last pair found, set its cdr to result
        pair.cdr = result;
        break;
      }
      last = pair.cdr;
    }

    result = copied;
  }

  return result;
};

/**
 * List primitive: reverse
 * Port from: primitive.cxx Reverse::primitiveCall
 */
const reversePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('reverse requires exactly 1 argument');
  }

  let result: ELObj = theNilObj;
  let current = args[0];

  while (!current.asNil()) {
    const pair = current.asPair();
    if (!pair) {
      throw new Error('reverse requires a proper list');
    }
    result = makePair(pair.car, result);
    current = pair.cdr;
  }

  return result;
};

/**
 * Type predicate: list?
 * Port from: primitive.cxx List::primitiveCall
 */
const listPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('list? requires exactly 1 argument');
  }

  return makeBoolean(args[0].isList());
};

/**
 * Type predicate: string?
 * Port from: primitive.cxx String::primitiveCall
 */
const stringPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asString() !== null);
};

/**
 * Type predicate: symbol?
 * Port from: primitive.cxx Symbol::primitiveCall
 */
const symbolPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('symbol? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asSymbol() !== null);
};

/**
 * Type predicate: boolean?
 * Port from: primitive.cxx Boolean::primitiveCall
 */
const booleanPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('boolean? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asBoolean() !== null);
};

/**
 * Type predicate: procedure?
 * Port from: primitive.cxx Procedure::primitiveCall
 */
const procedurePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('procedure? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asFunction() !== null);
};

/**
 * Type predicate: real?
 * Port from: primitive.cxx RealP::primitiveCall
 */
const realPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('real? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(Number.isFinite(num.value));
};

/**
 * Type predicate: rational?
 * Port from: primitive.cxx RationalP::primitiveCall
 */
const rationalPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('rational? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(Number.isFinite(num.value));
};

/**
 * Type predicate: complex?
 * Port from: primitive.cxx ComplexP::primitiveCall
 */
const complexPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('complex? requires exactly 1 argument');
  }

  // In our implementation, all numbers are real (no complex support)
  return makeBoolean(args[0].asNumber() !== null);
};

/**
 * Equality primitive: eq?
 * Port from: primitive.cxx Eq::primitiveCall
 */
const eqPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('eq? requires exactly 2 arguments');
  }

  // eq? tests identity (same object)
  return makeBoolean(args[0] === args[1]);
};

/**
 * Equality primitive: eqv?
 * Port from: primitive.cxx Eqv::primitiveCall
 */
const eqvPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('eqv? requires exactly 2 arguments');
  }

  const a = args[0];
  const b = args[1];

  // Identity
  if (a === b) {
    return theTrueObj;
  }

  // Numbers
  const numA = a.asNumber();
  const numB = b.asNumber();
  if (numA && numB) {
    return makeBoolean(numA.exact === numB.exact && numA.value === numB.value);
  }

  // Booleans
  const boolA = a.asBoolean();
  const boolB = b.asBoolean();
  if (boolA && boolB) {
    return makeBoolean(boolA.value === boolB.value);
  }

  // Symbols
  const symA = a.asSymbol();
  const symB = b.asSymbol();
  if (symA && symB) {
    return makeBoolean(symA.name === symB.name);
  }

  // Characters
  const charA = a.asChar();
  const charB = b.asChar();
  if (charA && charB) {
    return makeBoolean(charA.value === charB.value);
  }

  return theFalseObj;
};

/**
 * Equality primitive: equal?
 * Port from: primitive.cxx Equal::primitiveCall
 */
const equalPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('equal? requires exactly 2 arguments');
  }

  const equal = (a: ELObj, b: ELObj): boolean => {
    // Try eqv? first
    if (a === b) return true;

    // Pairs
    const pairA = a.asPair();
    const pairB = b.asPair();
    if (pairA && pairB) {
      return equal(pairA.car, pairB.car) && equal(pairA.cdr, pairB.cdr);
    }

    // Strings
    const strA = a.asString();
    const strB = b.asString();
    if (strA && strB) {
      return strA.value === strB.value;
    }

    // Vectors
    const vecA = a.asVector();
    const vecB = b.asVector();
    if (vecA && vecB) {
      if (vecA.elements.length !== vecB.elements.length) {
        return false;
      }
      for (let i = 0; i < vecA.elements.length; i++) {
        if (!equal(vecA.elements[i], vecB.elements[i])) {
          return false;
        }
      }
      return true;
    }

    // Fall back to eqv?
    const numA = a.asNumber();
    const numB = b.asNumber();
    if (numA && numB) {
      return numA.exact === numB.exact && numA.value === numB.value;
    }

    return false;
  };

  return makeBoolean(equal(args[0], args[1]));
};

/**
 * String primitive: string-length
 * Port from: primitive.cxx StringLength::primitiveCall
 */
const stringLengthPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-length requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-length requires a string');
  }

  return makeNumber(str.value.length, true);
};

/**
 * String primitive: string-append
 * Port from: primitive.cxx StringAppend::primitiveCall
 */
const stringAppendPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  let result = '';

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    // Port from: OpenJade StringAppend - treats () and #f as empty string
    // This allows templates to use (if ...) and (case ...) without else clauses
    // AND allows attribute-string returning #f to be safely concatenated
    const argBool = arg.asBoolean();
    if (arg.asNil() || (argBool !== null && !argBool.value)) {  // FIX: use .value
      continue; // Treat () and #f as empty string
    }

    const str = arg.asString();
    if (!str) {
      const argType = arg.constructor.name;
      const argBool2 = arg.asBoolean();  // Check again for error message
      const argValue = argBool2 !== null ? (argBool2.value ? '#t' : '#f') :
                       arg.asNil() ? '()' : argType;
      // Debug: show all args
      const debugArgs = args.map((a, idx) => {
        const str = a.asString();
        const bool = a.asBoolean();
        return str ? `"${str.value.substring(0, 30)}..."` :
               bool !== null ? (bool.value ? '#t' : '#f') :
               a.asNil() ? '()' : a.constructor.name;
      }).join(', ');
      throw new Error(`string-append requires string arguments, got ${argValue} at position ${i}. Args: [${debugArgs}]`);
    }
    result += str.value;
  }

  return makeString(result);
};

// ============ More Arithmetic Operations ============

/**
 * Arithmetic primitive: abs
 * Port from: primitive.cxx Abs::primitiveCall
 */
const absPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('abs requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('abs requires a number argument');
  }

  return makeNumber(Math.abs(num.value), num.exact);
};

/**
 * Arithmetic primitive: quotient
 * Port from: primitive.cxx Quotient::primitiveCall
 */
const quotientPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('quotient requires exactly 2 arguments');
  }

  const num1 = args[0].asNumber();
  const num2 = args[1].asNumber();
  if (!num1 || !num2) {
    throw new Error('quotient requires number arguments');
  }

  if (num2.value === 0) {
    throw new Error('quotient: division by zero');
  }

  return makeNumber(Math.trunc(num1.value / num2.value), true);
};

/**
 * Arithmetic primitive: remainder
 * Port from: primitive.cxx Remainder::primitiveCall
 */
const remainderPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('remainder requires exactly 2 arguments');
  }

  const num1 = args[0].asNumber();
  const num2 = args[1].asNumber();
  if (!num1 || !num2) {
    throw new Error('remainder requires number arguments');
  }

  if (num2.value === 0) {
    throw new Error('remainder: division by zero');
  }

  return makeNumber(num1.value % num2.value, true);
};

/**
 * Arithmetic primitive: modulo
 * Port from: primitive.cxx Modulo::primitiveCall
 */
const moduloPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('modulo requires exactly 2 arguments');
  }

  const num1 = args[0].asNumber();
  const num2 = args[1].asNumber();
  if (!num1 || !num2) {
    throw new Error('modulo requires number arguments');
  }

  if (num2.value === 0) {
    throw new Error('modulo: division by zero');
  }

  // R4RS modulo: result has same sign as divisor
  const result = num1.value % num2.value;
  if ((result > 0 && num2.value < 0) || (result < 0 && num2.value > 0)) {
    return makeNumber(result + num2.value, true);
  }
  return makeNumber(result, true);
};

/**
 * Arithmetic primitive: floor
 * Port from: primitive.cxx Floor::primitiveCall
 */
const floorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('floor requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('floor requires a number argument');
  }

  return makeNumber(Math.floor(num.value), true);
};

/**
 * Arithmetic primitive: ceiling
 * Port from: primitive.cxx Ceiling::primitiveCall
 */
const ceilingPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('ceiling requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('ceiling requires a number argument');
  }

  return makeNumber(Math.ceil(num.value), true);
};

/**
 * Arithmetic primitive: truncate
 * Port from: primitive.cxx Truncate::primitiveCall
 */
const truncatePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('truncate requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('truncate requires a number argument');
  }

  return makeNumber(Math.trunc(num.value), true);
};

/**
 * Arithmetic primitive: round
 * Port from: primitive.cxx Round::primitiveCall
 */
const roundPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('round requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('round requires a number argument');
  }

  return makeNumber(Math.round(num.value), true);
};

/**
 * Arithmetic primitive: sqrt
 * Port from: primitive.cxx Sqrt::primitiveCall
 */
const sqrtPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('sqrt requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('sqrt requires a number argument');
  }

  return makeNumber(Math.sqrt(num.value), false);
};

/**
 * Arithmetic primitive: expt
 * Port from: primitive.cxx Expt::primitiveCall
 */
const exptPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('expt requires exactly 2 arguments');
  }

  const base = args[0].asNumber();
  const exp = args[1].asNumber();
  if (!base || !exp) {
    throw new Error('expt requires number arguments');
  }

  const result = Math.pow(base.value, exp.value);
  const exact = base.exact && exp.exact && Number.isInteger(exp.value) && exp.value >= 0;
  return makeNumber(result, exact);
};

/**
 * Arithmetic primitive: min
 * Port from: primitive.cxx Min::primitiveCall
 */
const minPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    throw new Error('min requires at least 1 argument');
  }

  let minVal = Infinity;
  let exact = true;

  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('min requires number arguments');
    }
    if (num.value < minVal) {
      minVal = num.value;
    }
    if (!num.exact) {
      exact = false;
    }
  }

  return makeNumber(minVal, exact);
};

/**
 * Arithmetic primitive: max
 * Port from: primitive.cxx Max::primitiveCall
 */
const maxPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    throw new Error('max requires at least 1 argument');
  }

  let maxVal = -Infinity;
  let exact = true;

  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('max requires number arguments');
    }
    if (num.value > maxVal) {
      maxVal = num.value;
    }
    if (!num.exact) {
      exact = false;
    }
  }

  return makeNumber(maxVal, exact);
};

// ============ Numeric Predicates ============

/**
 * Numeric predicate: integer?
 * Port from: primitive.cxx IntegerP::primitiveCall
 */
const integerPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('integer? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(Number.isInteger(num.value));
};

/**
 * Numeric predicate: exact?
 * Port from: primitive.cxx ExactP::primitiveCall
 */
const exactPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('exact? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('exact? requires a number argument');
  }

  return makeBoolean(num.exact);
};

/**
 * Numeric predicate: inexact?
 * Port from: primitive.cxx InexactP::primitiveCall
 */
const inexactPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('inexact? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('inexact? requires a number argument');
  }

  return makeBoolean(!num.exact);
};

/**
 * Numeric predicate: zero?
 * Port from: primitive.cxx ZeroP::primitiveCall
 */
const zeroPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('zero? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('zero? requires a number argument');
  }

  return makeBoolean(num.value === 0);
};

/**
 * Numeric predicate: positive?
 * Port from: primitive.cxx PositiveP::primitiveCall
 */
const positivePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('positive? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('positive? requires a number argument');
  }

  return makeBoolean(num.value > 0);
};

/**
 * Numeric predicate: negative?
 * Port from: primitive.cxx NegativeP::primitiveCall
 */
const negativePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('negative? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('negative? requires a number argument');
  }

  return makeBoolean(num.value < 0);
};

/**
 * Numeric predicate: odd?
 * Port from: primitive.cxx OddP::primitiveCall
 */
const oddPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('odd? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('odd? requires a number argument');
  }

  if (!Number.isInteger(num.value)) {
    throw new Error('odd? requires an integer argument');
  }

  return makeBoolean(num.value % 2 !== 0);
};

/**
 * Numeric predicate: even?
 * Port from: primitive.cxx EvenP::primitiveCall
 */
const evenPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('even? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('even? requires a number argument');
  }

  if (!Number.isInteger(num.value)) {
    throw new Error('even? requires an integer argument');
  }

  return makeBoolean(num.value % 2 === 0);
};

// ============ Numeric Conversions ============

/**
 * Numeric conversion: exact->inexact
 * Port from: primitive.cxx ExactToInexact::primitiveCall
 */
const exactToInexactPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('exact->inexact requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('exact->inexact requires a number argument');
  }

  return makeNumber(num.value, false);
};

/**
 * Numeric conversion: inexact->exact
 * Port from: primitive.cxx InexactToExact::primitiveCall
 */
const inexactToExactPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('inexact->exact requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('inexact->exact requires a number argument');
  }

  if (!Number.isFinite(num.value)) {
    throw new Error('inexact->exact: argument must be finite');
  }

  return makeNumber(num.value, true);
};

// ============ More Arithmetic Operations ============

/**
 * Arithmetic primitive: gcd
 * Port from: primitive.cxx Gcd::primitiveCall
 */
const gcdPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNumber(0, true);
  }

  const gcd = (a: number, b: number): number => {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  };

  let result = 0;
  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('gcd requires number arguments');
    }
    if (!Number.isInteger(num.value)) {
      throw new Error('gcd requires integer arguments');
    }
    result = gcd(result, num.value);
  }

  return makeNumber(result, true);
};

/**
 * Arithmetic primitive: lcm
 * Port from: primitive.cxx Lcm::primitiveCall
 */
const lcmPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNumber(1, true);
  }

  const gcd = (a: number, b: number): number => {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  };

  const lcm = (a: number, b: number): number => {
    if (a === 0 || b === 0) return 0;
    return Math.abs(a * b) / gcd(a, b);
  };

  let result = 1;
  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('lcm requires number arguments');
    }
    if (!Number.isInteger(num.value)) {
      throw new Error('lcm requires integer arguments');
    }
    result = lcm(result, num.value);
  }

  return makeNumber(result, true);
};

/**
 * Arithmetic primitive: numerator
 * Port from: primitive.cxx Numerator::primitiveCall
 *
 * In our implementation, since we don't have true rational numbers,
 * numerator returns the number itself for integers, or an approximation
 * for floating-point numbers.
 */
const numeratorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('numerator requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('numerator requires a number argument');
  }

  // For integers, numerator is the number itself
  if (Number.isInteger(num.value)) {
    return makeNumber(num.value, num.exact);
  }

  // For floating-point, approximate as rational
  // Simple approximation: multiply by a power of 10 to make it an integer
  const decimalPlaces = num.value.toString().split('.')[1]?.length || 0;
  const multiplier = Math.pow(10, Math.min(decimalPlaces, 10));
  const numerator = Math.round(num.value * multiplier);

  // Simplify by GCD
  const gcd = (a: number, b: number): number => {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  };

  const divisor = gcd(numerator, multiplier);
  return makeNumber(numerator / divisor, true);
};

/**
 * Arithmetic primitive: denominator
 * Port from: primitive.cxx Denominator::primitiveCall
 *
 * In our implementation, since we don't have true rational numbers,
 * denominator returns 1 for integers, or an approximation for
 * floating-point numbers.
 */
const denominatorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('denominator requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('denominator requires a number argument');
  }

  // For integers, denominator is 1
  if (Number.isInteger(num.value)) {
    return makeNumber(1, num.exact);
  }

  // For floating-point, approximate as rational
  const decimalPlaces = num.value.toString().split('.')[1]?.length || 0;
  const multiplier = Math.pow(10, Math.min(decimalPlaces, 10));
  const numerator = Math.round(num.value * multiplier);

  // Simplify by GCD
  const gcd = (a: number, b: number): number => {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  };

  const divisor = gcd(numerator, multiplier);
  return makeNumber(multiplier / divisor, true);
};

/**
 * Arithmetic primitive: rationalize
 * Port from: primitive.cxx Rationalize::primitiveCall
 *
 * Returns the simplest rational number within the given tolerance of x.
 */
const rationalizePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('rationalize requires exactly 2 arguments');
  }

  const x = args[0].asNumber();
  const tolerance = args[1].asNumber();

  if (!x || !tolerance) {
    throw new Error('rationalize requires number arguments');
  }

  // Simple implementation: find the simplest fraction within tolerance
  const lower = x.value - Math.abs(tolerance.value);
  const upper = x.value + Math.abs(tolerance.value);

  // Try simple fractions first
  for (let denom = 1; denom <= 1000; denom++) {
    for (let numer = Math.floor(lower * denom); numer <= Math.ceil(upper * denom); numer++) {
      const candidate = numer / denom;
      if (candidate >= lower && candidate <= upper) {
        // Check if this fraction is in its simplest form
        const gcd = (a: number, b: number): number => {
          a = Math.abs(a);
          b = Math.abs(b);
          while (b !== 0) {
            const temp = b;
            b = a % b;
            a = temp;
          }
          return a;
        };

        if (gcd(Math.abs(numer), denom) === 1) {
          return makeNumber(candidate, true);
        }
      }
    }
  }

  // Fallback: return the original number
  return x;
};

// ============ More Math Functions ============

/**
 * Math function: exp
 * Port from: primitive.cxx Exp::primitiveCall
 */
const expPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('exp requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('exp requires a number argument');
  }

  return makeNumber(Math.exp(num.value), false);
};

/**
 * Math function: log
 * Port from: primitive.cxx Log::primitiveCall
 */
const logPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('log requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('log requires a number argument');
  }

  return makeNumber(Math.log(num.value), false);
};

/**
 * Math function: sin
 * Port from: primitive.cxx Sin::primitiveCall
 */
const sinPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('sin requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('sin requires a number argument');
  }

  return makeNumber(Math.sin(num.value), false);
};

/**
 * Math function: cos
 * Port from: primitive.cxx Cos::primitiveCall
 */
const cosPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cos requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('cos requires a number argument');
  }

  return makeNumber(Math.cos(num.value), false);
};

/**
 * Math function: tan
 * Port from: primitive.cxx Tan::primitiveCall
 */
const tanPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('tan requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('tan requires a number argument');
  }

  return makeNumber(Math.tan(num.value), false);
};

/**
 * Math function: asin
 * Port from: primitive.cxx Asin::primitiveCall
 */
const asinPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('asin requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('asin requires a number argument');
  }

  return makeNumber(Math.asin(num.value), false);
};

/**
 * Math function: acos
 * Port from: primitive.cxx Acos::primitiveCall
 */
const acosPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('acos requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('acos requires a number argument');
  }

  return makeNumber(Math.acos(num.value), false);
};

/**
 * Math function: atan
 * Port from: primitive.cxx Atan::primitiveCall
 */
const atanPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('atan requires 1 or 2 arguments');
  }

  const y = args[0].asNumber();
  if (!y) {
    throw new Error('atan requires number arguments');
  }

  if (args.length === 1) {
    return makeNumber(Math.atan(y.value), false);
  }

  const x = args[1].asNumber();
  if (!x) {
    throw new Error('atan requires number arguments');
  }

  return makeNumber(Math.atan2(y.value, x.value), false);
};

// ============ More String Operations ============

/**
 * String primitive: string-ref
 * Port from: primitive.cxx StringRef::primitiveCall
 */
const stringRefPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-ref requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const idx = args[1].asNumber();
  if (!str || !idx) {
    throw new Error('string-ref requires string and number arguments');
  }

  if (idx.value < 0 || idx.value >= str.value.length) {
    throw new Error(`string-ref: index ${idx.value} out of bounds for string of length ${str.value.length}`);
  }

  // Return as a character
  return { asChar: () => ({ value: str.value[Math.floor(idx.value)] }) } as any;
};

/**
 * String primitive: substring
 * Port from: primitive.cxx Substring::primitiveCall
 */
const substringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('substring requires exactly 3 arguments');
  }

  const str = args[0].asString();
  const start = args[1].asNumber();
  const end = args[2].asNumber();
  if (!str || !start || !end) {
    throw new Error('substring requires string and two number arguments');
  }

  const startIdx = Math.floor(start.value);
  const endIdx = Math.floor(end.value);

  if (startIdx < 0 || startIdx > str.value.length) {
    throw new Error(`substring: start index ${startIdx} out of bounds`);
  }
  if (endIdx < startIdx || endIdx > str.value.length) {
    throw new Error(`substring: end index ${endIdx} out of bounds`);
  }

  return makeString(str.value.substring(startIdx, endIdx));
};

/**
 * String primitive: string=?
 * Port from: primitive.cxx StringEqual::primitiveCall
 */
const stringEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string=? requires at least 2 arguments');
  }

  // Port from: OpenJade primitive.cxx StringEqual - treats #f as not equal
  // If any argument is #f or not a string, return #f instead of throwing
  const first = args[0].asString();
  if (!first) {
    return theFalseObj;
  }

  for (let i = 1; i < args.length; i++) {
    const str = args[i].asString();
    if (!str) {
      return theFalseObj;
    }
    if (str.value !== first.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String primitive: string<?
 * Port from: primitive.cxx StringLess::primitiveCall
 */
const stringLessPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string<? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string<? requires string arguments');
    }
    if (str1.value >= str2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

// ============ Type Conversions ============

/**
 * Conversion primitive: number->string
 * Port from: primitive.cxx NumberToString::primitiveCall
 */
const numberToStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('number->string requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('number->string requires a number argument');
  }

  return makeString(num.value.toString());
};

/**
 * Conversion primitive: string->number
 * Port from: primitive.cxx StringToNumber::primitiveCall
 */
const stringToNumberPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string->number requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string->number requires a string argument');
  }

  const num = parseFloat(str.value);
  if (isNaN(num)) {
    return theFalseObj; // R4RS: return #f if cannot parse
  }

  const exact = /^-?\d+$/.test(str.value.trim());
  return makeNumber(num, exact);
};

/**
 * Conversion primitive: symbol->string
 * Port from: primitive.cxx SymbolToString::primitiveCall
 */
const symbolToStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('symbol->string requires exactly 1 argument');
  }

  const sym = args[0].asSymbol();
  if (!sym) {
    throw new Error('symbol->string requires a symbol argument');
  }

  return makeString(sym.name);
};

/**
 * Conversion primitive: string->symbol
 * Port from: primitive.cxx StringToSymbol::primitiveCall
 */
const stringToSymbolPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string->symbol requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string->symbol requires a string argument');
  }

  return makeSymbol(str.value);
};

// ============ List Search Operations ============

/**
 * List search: member
 * Port from: primitive.cxx Member::primitiveCall
 */
const memberPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('member requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('member: second argument must be a list');
    }

    // Use equal? comparison
    const equal = (a: ELObj, b: ELObj): boolean => {
      if (a === b) return true;

      const pairA = a.asPair();
      const pairB = b.asPair();
      if (pairA && pairB) {
        return equal(pairA.car, pairB.car) && equal(pairA.cdr, pairB.cdr);
      }

      const strA = a.asString();
      const strB = b.asString();
      if (strA && strB) {
        return strA.value === strB.value;
      }

      const numA = a.asNumber();
      const numB = b.asNumber();
      if (numA && numB) {
        return numA.value === numB.value && numA.exact === numB.exact;
      }

      return false;
    };

    if (equal(obj, pair.car)) {
      return list;
    }

    list = pair.cdr;
  }
};

/**
 * List search: memq
 * Port from: primitive.cxx Memq::primitiveCall
 */
const memqPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('memq requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('memq: second argument must be a list');
    }

    // Use eq? comparison (identity)
    if (obj === pair.car) {
      return list;
    }

    list = pair.cdr;
  }
};

/**
 * List search: memv
 * Port from: primitive.cxx Memv::primitiveCall
 */
const memvPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('memv requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('memv: second argument must be a list');
    }

    // Use eqv? comparison
    const eqv = (a: ELObj, b: ELObj): boolean => {
      if (a === b) return true;

      const numA = a.asNumber();
      const numB = b.asNumber();
      if (numA && numB) {
        return numA.exact === numB.exact && numA.value === numB.value;
      }

      const symA = a.asSymbol();
      const symB = b.asSymbol();
      if (symA && symB) {
        return symA.name === symB.name;
      }

      return false;
    };

    if (eqv(obj, pair.car)) {
      return list;
    }

    list = pair.cdr;
  }
};

/**
 * List search: assoc
 * Port from: primitive.cxx Assoc::primitiveCall
 */
const assocPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('assoc requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  const equal = (a: ELObj, b: ELObj): boolean => {
    if (a === b) return true;

    const pairA = a.asPair();
    const pairB = b.asPair();
    if (pairA && pairB) {
      return equal(pairA.car, pairB.car) && equal(pairA.cdr, pairB.cdr);
    }

    const strA = a.asString();
    const strB = b.asString();
    if (strA && strB) {
      return strA.value === strB.value;
    }

    const numA = a.asNumber();
    const numB = b.asNumber();
    if (numA && numB) {
      return numA.value === numB.value && numA.exact === numB.exact;
    }

    return false;
  };

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('assoc: second argument must be an association list');
    }

    const entry = pair.car.asPair();
    if (!entry) {
      throw new Error('assoc: list element is not a pair');
    }

    if (equal(obj, entry.car)) {
      return pair.car;
    }

    list = pair.cdr;
  }
};

/**
 * List search: assq
 * Port from: primitive.cxx Assq::primitiveCall
 */
const assqPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('assq requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('assq: second argument must be an association list');
    }

    const entry = pair.car.asPair();
    if (!entry) {
      throw new Error('assq: list element is not a pair');
    }

    if (obj === entry.car) {
      return pair.car;
    }

    list = pair.cdr;
  }
};

/**
 * List search: assv
 * Port from: primitive.cxx Assv::primitiveCall
 */
const assvPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('assv requires exactly 2 arguments');
  }

  const obj = args[0];
  let list = args[1];

  const eqv = (a: ELObj, b: ELObj): boolean => {
    if (a === b) return true;

    const numA = a.asNumber();
    const numB = b.asNumber();
    if (numA && numB) {
      return numA.exact === numB.exact && numA.value === numB.value;
    }

    const symA = a.asSymbol();
    const symB = b.asSymbol();
    if (symA && symB) {
      return symA.name === symB.name;
    }

    return false;
  };

  while (true) {
    if (list.asNil()) {
      return theFalseObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('assv: second argument must be an association list');
    }

    const entry = pair.car.asPair();
    if (!entry) {
      throw new Error('assv: list element is not a pair');
    }

    if (eqv(obj, entry.car)) {
      return pair.car;
    }

    list = pair.cdr;
  }
};

// ============ Vector Operations ============

/**
 * Vector constructor: make-vector
 * Port from: primitive.cxx MakeVector::primitiveCall
 */
const makeVectorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('make-vector requires 1 or 2 arguments');
  }

  const size = args[0].asNumber();
  if (!size) {
    throw new Error('make-vector: size must be a number');
  }

  if (size.value < 0 || !Number.isInteger(size.value)) {
    throw new Error('make-vector: size must be a non-negative integer');
  }

  const fill = args.length === 2 ? args[1] : theNilObj;
  const elements = new Array(Math.floor(size.value)).fill(fill);

  return makeVector(elements);
};

/**
 * Vector constructor: vector
 * Port from: primitive.cxx Vector::primitiveCall
 */
const vectorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  return makeVector(args);
};

/**
 * Vector accessor: vector-ref
 * Port from: primitive.cxx VectorRef::primitiveCall
 */
const vectorRefPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('vector-ref requires exactly 2 arguments');
  }

  const vec = args[0].asVector();
  const idx = args[1].asNumber();
  if (!vec || !idx) {
    throw new Error('vector-ref requires vector and number arguments');
  }

  if (idx.value < 0 || idx.value >= vec.elements.length) {
    throw new Error(`vector-ref: index ${idx.value} out of bounds for vector of length ${vec.elements.length}`);
  }

  return vec.elements[Math.floor(idx.value)];
};

/**
 * Vector mutator: vector-set!
 * Port from: primitive.cxx VectorSet::primitiveCall
 */
const vectorSetPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('vector-set! requires exactly 3 arguments');
  }

  const vec = args[0].asVector();
  const idx = args[1].asNumber();
  const value = args[2];
  if (!vec || !idx) {
    throw new Error('vector-set! requires vector and number arguments');
  }

  if (idx.value < 0 || idx.value >= vec.elements.length) {
    throw new Error(`vector-set!: index ${idx.value} out of bounds for vector of length ${vec.elements.length}`);
  }

  vec.elements[Math.floor(idx.value)] = value;
  return theNilObj; // R4RS: unspecified return value
};

/**
 * Vector length: vector-length
 * Port from: primitive.cxx VectorLength::primitiveCall
 */
const vectorLengthPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('vector-length requires exactly 1 argument');
  }

  const vec = args[0].asVector();
  if (!vec) {
    throw new Error('vector-length requires a vector argument');
  }

  return makeNumber(vec.elements.length, true);
};

/**
 * Vector predicate: vector?
 * Port from: primitive.cxx VectorP::primitiveCall
 */
const vectorPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('vector? requires exactly 1 argument');
  }

  return makeBoolean(!!args[0].asVector());
};

/**
 * Vector mutator: vector-fill!
 * Port from: primitive.cxx VectorFill::primitiveCall
 */
const vectorFillPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('vector-fill! requires exactly 2 arguments');
  }

  const vec = args[0].asVector();
  const fill = args[1];
  if (!vec) {
    throw new Error('vector-fill! requires a vector argument');
  }

  for (let i = 0; i < vec.elements.length; i++) {
    vec.elements[i] = fill;
  }

  return theNilObj; // R4RS: unspecified return value
};

// ============ Character Operations ============

/**
 * Character predicate: char?
 * Port from: primitive.cxx CharP::primitiveCall
 */
const charPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char? requires exactly 1 argument');
  }

  return makeBoolean(!!args[0].asChar());
};

/**
 * Character comparison: char=?
 * Port from: primitive.cxx CharEqual::primitiveCall
 */
const charEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char=? requires at least 2 arguments');
  }

  const first = args[0].asChar();
  if (!first) {
    throw new Error('char=? requires character arguments');
  }

  for (let i = 1; i < args.length; i++) {
    const ch = args[i].asChar();
    if (!ch) {
      throw new Error('char=? requires character arguments');
    }
    if (ch.value !== first.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char<?
 * Port from: primitive.cxx CharLess::primitiveCall
 */
const charLessPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char<? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char<? requires character arguments');
    }
    if (ch1.value >= ch2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character conversion: char-upcase
 * Port from: primitive.cxx CharUpcase::primitiveCall
 */
const charUpcasePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-upcase requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-upcase requires a character argument');
  }

  return { asChar: () => ({ value: ch.value.toUpperCase() }) } as any;
};

/**
 * Character conversion: char-downcase
 * Port from: primitive.cxx CharDowncase::primitiveCall
 */
const charDowncasePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-downcase requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-downcase requires a character argument');
  }

  return { asChar: () => ({ value: ch.value.toLowerCase() }) } as any;
};

/**
 * Character conversion: char->integer
 * Port from: primitive.cxx CharToInteger::primitiveCall
 */
const charToIntegerPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char->integer requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char->integer requires a character argument');
  }

  return makeNumber(ch.value.charCodeAt(0), true);
};

/**
 * Character conversion: integer->char
 * Port from: primitive.cxx IntegerToChar::primitiveCall
 */
const integerToCharPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('integer->char requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('integer->char requires a number argument');
  }

  if (!Number.isInteger(num.value) || num.value < 0 || num.value > 0x10ffff) {
    throw new Error('integer->char: argument out of range');
  }

  return { asChar: () => ({ value: String.fromCharCode(num.value) }) } as any;
};

// ============ More List Operations ============

/**
 * List accessor: list-ref
 * Port from: primitive.cxx ListRef::primitiveCall
 */
const listRefPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('list-ref requires exactly 2 arguments');
  }

  let list = args[0];
  const idx = args[1].asNumber();
  if (!idx) {
    throw new Error('list-ref requires a number as second argument');
  }

  if (idx.value < 0 || !Number.isInteger(idx.value)) {
    throw new Error('list-ref: index must be a non-negative integer');
  }

  const index = Math.floor(idx.value);
  for (let i = 0; i < index; i++) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list-ref: index out of bounds');
    }
    list = pair.cdr;
  }

  const pair = list.asPair();
  if (!pair) {
    throw new Error('list-ref: index out of bounds');
  }

  return pair.car;
};

/**
 * List accessor: list-tail
 * Port from: primitive.cxx ListTail::primitiveCall
 */
const listTailPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('list-tail requires exactly 2 arguments');
  }

  let list = args[0];
  const idx = args[1].asNumber();
  if (!idx) {
    throw new Error('list-tail requires a number as second argument');
  }

  if (idx.value < 0 || !Number.isInteger(idx.value)) {
    throw new Error('list-tail: index must be a non-negative integer');
  }

  const index = Math.floor(idx.value);
  for (let i = 0; i < index; i++) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list-tail: index out of bounds');
    }
    list = pair.cdr;
  }

  return list;
};

/**
 * Higher-order: map
 * Port from: primitive.cxx Map::primitiveCall
 */
const mapPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('map requires at least 2 arguments');
  }

  const proc = args[0].asFunction();
  if (!proc) {
    throw new Error('map: first argument must be a procedure');
  }

  const lists = args.slice(1);
  const result: ELObj[] = [];

  // Check all lists are proper lists
  for (const list of lists) {
    let current = list;
    while (!current.asNil()) {
      const pair = current.asPair();
      if (!pair) {
        throw new Error('map: arguments must be proper lists');
      }
      current = pair.cdr;
    }
  }

  // Apply proc to corresponding elements
  while (true) {
    // Check if any list is empty
    for (const list of lists) {
      if (list.asNil()) {
        // Build result list
        let resultList: ELObj = theNilObj;
        for (let i = result.length - 1; i >= 0; i--) {
          resultList = makePair(result[i], resultList);
        }
        return resultList;
      }
    }

    // Get first element from each list
    const elements: ELObj[] = [];
    for (let i = 0; i < lists.length; i++) {
      const pair = lists[i].asPair();
      if (!pair) {
        throw new Error('map: lists have different lengths');
      }
      elements.push(pair.car);
      lists[i] = pair.cdr;
    }

    // Apply procedure (works for both primitives and closures)
    const value = proc.isPrimitive()
      ? proc.callPrimitive(elements, vm)
      : callClosure(proc, elements, vm);
    result.push(value);
  }
};

/**
 * Higher-order: for-each
 * Port from: primitive.cxx ForEach::primitiveCall
 */
const forEachPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('for-each requires at least 2 arguments');
  }

  const proc = args[0].asFunction();
  if (!proc) {
    throw new Error('for-each: first argument must be a procedure');
  }

  const lists = args.slice(1);

  // Check all lists are proper lists
  for (const list of lists) {
    let current = list;
    while (!current.asNil()) {
      const pair = current.asPair();
      if (!pair) {
        throw new Error('for-each: arguments must be proper lists');
      }
      current = pair.cdr;
    }
  }

  // Apply proc to corresponding elements
  while (true) {
    // Check if any list is empty
    for (const list of lists) {
      if (list.asNil()) {
        return theNilObj; // R4RS: unspecified return value
      }
    }

    // Get first element from each list
    const elements: ELObj[] = [];
    for (let i = 0; i < lists.length; i++) {
      const pair = lists[i].asPair();
      if (!pair) {
        throw new Error('for-each: lists have different lengths');
      }
      elements.push(pair.car);
      lists[i] = pair.cdr;
    }

    // Apply procedure (ignore result)
    if (proc.isPrimitive()) {
      proc.callPrimitive(elements, vm);
    } else {
      callClosure(proc, elements, vm);
    }
  }
};

/**
 * Higher-order: apply
 * Port from: primitive.cxx Apply::primitiveCall
 */
const applyPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('apply requires at least 2 arguments');
  }

  const proc = args[0].asFunction();
  if (!proc) {
    throw new Error('apply: first argument must be a procedure');
  }

  // Collect all arguments except the last, which must be a list
  const argsList: ELObj[] = [];

  // Add intermediate arguments
  for (let i = 1; i < args.length - 1; i++) {
    argsList.push(args[i]);
  }

  // Last argument must be a list - convert to array
  let list = args[args.length - 1];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('apply: last argument must be a proper list');
    }
    argsList.push(pair.car);
    list = pair.cdr;
  }

  return proc.isPrimitive()
    ? proc.callPrimitive(argsList, vm)
    : callClosure(proc, argsList, vm);
};

// ============ More List Utilities ============

/**
 * List utility: last
 * Port from: Common Lisp/Scheme extension
 */
const lastPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('last requires exactly 1 argument');
  }

  let list = args[0];
  if (list.asNil()) {
    throw new Error('last: argument must be a non-empty list');
  }

  let prev = list;
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('last: argument must be a proper list');
    }
    prev = list;
    list = pair.cdr;
  }

  return prev.asPair()!.car;
};

/**
 * List utility: butlast
 * Port from: Common Lisp extension
 */
const butlastPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('butlast requires exactly 1 argument');
  }

  let list = args[0];
  if (list.asNil()) {
    return theNilObj;
  }

  // Check if list has only one element
  const firstPair = list.asPair();
  if (!firstPair) {
    throw new Error('butlast: argument must be a list');
  }
  if (firstPair.cdr.asNil()) {
    return theNilObj;
  }

  // Build result list without last element
  const result: ELObj[] = [];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('butlast: argument must be a proper list');
    }
    // Don't add if this is the last element
    if (!pair.cdr.asNil()) {
      result.push(pair.car);
    }
    list = pair.cdr;
  }

  let resultList: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    resultList = makePair(result[i], resultList);
  }
  return resultList;
};

/**
 * List utility: nthcdr
 * Port from: Common Lisp
 */
const nthcdrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('nthcdr requires exactly 2 arguments');
  }

  const n = args[0].asNumber();
  let list = args[1];
  if (!n) {
    throw new Error('nthcdr: first argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('nthcdr: index must be a non-negative integer');
  }

  const index = Math.floor(n.value);
  for (let i = 0; i < index; i++) {
    if (list.asNil()) {
      return theNilObj;
    }
    const pair = list.asPair();
    if (!pair) {
      throw new Error('nthcdr: second argument must be a list');
    }
    list = pair.cdr;
  }

  return list;
};

// ============ I/O Primitives (Stubs) ============

/**
 * I/O primitive: display
 * Port from: primitive.cxx Display::primitiveCall
 *
 * Note: In OpenJade this writes to output port.
 * For now, this is a stub that returns unspecified.
 */
const displayPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('display requires 1 or 2 arguments');
  }

  // In a full implementation, this would write to a port
  // For now, just return unspecified
  return theNilObj;
};

/**
 * I/O primitive: write
 * Port from: primitive.cxx Write::primitiveCall
 *
 * Note: In OpenJade this writes to output port with quotes.
 * For now, this is a stub that returns unspecified.
 */
const writePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('write requires 1 or 2 arguments');
  }

  // In a full implementation, this would write to a port with quotes
  // For now, just return unspecified
  return theNilObj;
};

/**
 * I/O primitive: newline
 * Port from: primitive.cxx Newline::primitiveCall
 *
 * Note: In OpenJade this writes a newline to output port.
 * For now, this is a stub that returns unspecified.
 */
const newlinePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length > 1) {
    throw new Error('newline requires 0 or 1 arguments');
  }

  // In a full implementation, this would write a newline to a port
  // For now, just return unspecified
  return theNilObj;
};

// ============ Port Predicates (Stubs) ============

/**
 * Port predicate: input-port?
 * Port from: primitive.cxx InputPort::primitiveCall
 *
 * Returns #f for now since we don't have port objects yet
 */
const inputPortPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('input-port? requires exactly 1 argument');
  }
  // No port objects implemented yet
  return theFalseObj;
};

/**
 * Port predicate: output-port?
 * Port from: primitive.cxx OutputPort::primitiveCall
 *
 * Returns #f for now since we don't have port objects yet
 */
const outputPortPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('output-port? requires exactly 1 argument');
  }
  // No port objects implemented yet
  return theFalseObj;
};

// ============ String Case Conversion ============

/**
 * String case conversion: string-upcase
 * Port from: primitive.cxx StringUpcase::primitiveCall
 */
const stringUpcasePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-upcase requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-upcase requires a string argument');
  }

  return makeString(str.value.toUpperCase());
};

/**
 * String case conversion: string-downcase
 * Port from: primitive.cxx StringDowncase::primitiveCall
 */
const stringDowncasePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-downcase requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-downcase requires a string argument');
  }

  return makeString(str.value.toLowerCase());
};

// ============ Additional List Operations ============

/**
 * List operation: reverse!
 * Port from: primitive.cxx ReverseInPlace::primitiveCall
 *
 * Destructively reverses a list. In our implementation, we return a new list
 * since JavaScript doesn't have true mutable pairs in the same way.
 */
const reverseInPlacePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('reverse! requires exactly 1 argument');
  }

  // For now, just use reverse (non-destructive)
  // A true in-place reverse would require mutable pair support
  let list = args[0];
  let result: ELObj = theNilObj;

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('reverse!: argument must be a list');
    }
    result = makePair(pair.car, result);
    list = pair.cdr;
  }

  return result;
};

/**
 * List operation: list-copy
 * Port from: primitive.cxx ListCopy::primitiveCall
 *
 * Creates a shallow copy of a list
 */
const listCopyPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('list-copy requires exactly 1 argument');
  }

  let list = args[0];

  // Handle nil
  if (list.asNil()) {
    return theNilObj;
  }

  // Build a new list with the same elements
  const elements: ELObj[] = [];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list-copy: argument must be a proper list');
    }
    elements.push(pair.car);
    list = pair.cdr;
  }

  // Reconstruct the list
  let result: ELObj = theNilObj;
  for (let i = elements.length - 1; i >= 0; i--) {
    result = makePair(elements[i], result);
  }

  return result;
};

// ============ Additional Predicates ============

/**
 * Predicate: eof-object?
 * Port from: primitive.cxx EofObject::primitiveCall
 *
 * Stub for now - we don't have EOF objects yet
 */
const eofObjectPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('eof-object? requires exactly 1 argument');
  }
  // No EOF objects implemented yet
  return theFalseObj;
};

// ============ More List Operations ============

/**
 * List operation: make-list
 * Port from: OpenJade extensions
 *
 * Creates a list of N elements, optionally filled with a value
 */
const makeListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('make-list requires 1 or 2 arguments');
  }

  const n = args[0].asNumber();
  if (!n) {
    throw new Error('make-list: first argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('make-list: size must be a non-negative integer');
  }

  const fillValue = args.length === 2 ? args[1] : theNilObj;
  const size = Math.floor(n.value);

  let result: ELObj = theNilObj;
  for (let i = 0; i < size; i++) {
    result = makePair(fillValue, result);
  }

  return result;
};

/**
 * List operation: iota
 * Port from: OpenJade extensions
 *
 * Creates a list of integers from 0 to n-1
 */
const iotaPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 3) {
    throw new Error('iota requires 1, 2, or 3 arguments');
  }

  const count = args[0].asNumber();
  if (!count || count.value < 0 || !Number.isInteger(count.value)) {
    throw new Error('iota: count must be a non-negative integer');
  }

  const start = args.length >= 2 ? args[1].asNumber() : null;
  const startValue = start ? start.value : 0;

  const step = args.length >= 3 ? args[2].asNumber() : null;
  const stepValue = step ? step.value : 1;

  let result: ELObj = theNilObj;
  for (let i = Math.floor(count.value) - 1; i >= 0; i--) {
    const value = startValue + i * stepValue;
    result = makePair(makeNumber(value, true), result);
  }

  return result;
};

// ============ Vector Copy Operations ============

/**
 * Vector operation: vector-copy
 * Port from: R7RS vector-copy
 *
 * Creates a copy of a vector
 */
const vectorCopyPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('vector-copy requires exactly 1 argument');
  }

  const vec = args[0].asVector();
  if (!vec) {
    throw new Error('vector-copy requires a vector argument');
  }

  // Create a shallow copy of the elements array
  const newElements = [...vec.elements];
  return makeVector(newElements);
};

// ============ Additional Math Operations ============

/**
 * Math operation: square
 * Port from: OpenJade extensions
 */
const squarePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('square requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('square requires a number argument');
  }

  return makeNumber(num.value * num.value, num.exact);
};

// ============ Boolean Operations ============

/**
 * Boolean operation: boolean=?
 * Port from: OpenJade extensions
 */
const booleanEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('boolean=? requires at least 2 arguments');
  }

  const first = args[0].asBoolean();
  if (!first) {
    throw new Error('boolean=? requires boolean arguments');
  }

  for (let i = 1; i < args.length; i++) {
    const bool = args[i].asBoolean();
    if (!bool || bool.value !== first.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Boolean operation: symbol=?
 * Port from: OpenJade extensions
 */
const symbolEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('symbol=? requires at least 2 arguments');
  }

  const first = args[0].asSymbol();
  if (!first) {
    throw new Error('symbol=? requires symbol arguments');
  }

  for (let i = 1; i < args.length; i++) {
    const sym = args[i].asSymbol();
    if (!sym || sym.name !== first.name) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

// ============ Additional Number Operations ============

/**
 * Number operation: number->string with radix
 * Extended version supporting radix (base) argument
 */
const numberToStringRadixPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('number->string requires 1 or 2 arguments');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('number->string: first argument must be a number');
  }

  if (args.length === 1) {
    return makeString(num.value.toString());
  }

  const radix = args[1].asNumber();
  if (!radix) {
    throw new Error('number->string: radix must be a number');
  }

  if (![2, 8, 10, 16].includes(radix.value)) {
    throw new Error('number->string: radix must be 2, 8, 10, or 16');
  }

  return makeString(Math.floor(num.value).toString(radix.value));
};

// ============ Additional String Operations ============

/**
 * String operation: string-append
 * Note: This is a duplicate registration fix for the existing implementation
 */

// ============ List Filtering and Searching ============

/**
 * List operation: filter
 * Port from: SRFI-1 filter
 *
 * Returns a list containing only elements that satisfy the predicate
 */
const filterPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('filter requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('filter: first argument must be a procedure');
  }

  let list = args[1];
  const result: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('filter: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car; // Simplified for non-primitive functions

    if (testResult.isTrue()) {
      result.push(pair.car);
    }

    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: remove
 * Port from: SRFI-1 remove
 *
 * Returns a list with elements that don't satisfy the predicate
 */
const removePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('remove requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('remove: first argument must be a procedure');
  }

  let list = args[1];
  const result: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('remove: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (!testResult.isTrue()) {
      result.push(pair.car);
    }

    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

// ============ List Folding Operations ============

/**
 * List operation: fold-left (also known as foldl or reduce)
 * Port from: SRFI-1 fold
 *
 * Folds a list from the left
 */
const foldLeftPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('fold-left requires exactly 3 arguments');
  }

  const func = args[0].asFunction();
  if (!func) {
    throw new Error('fold-left: first argument must be a procedure');
  }

  let accumulator = args[1];
  let list = args[2];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('fold-left: third argument must be a list');
    }

    // Call function with accumulator and current element
    if (func.isPrimitive()) {
      accumulator = func.callPrimitive([accumulator, pair.car], vm);
    }

    list = pair.cdr;
  }

  return accumulator;
};

/**
 * List operation: fold-right (also known as foldr)
 * Port from: SRFI-1 fold-right
 *
 * Folds a list from the right
 */
const foldRightPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('fold-right requires exactly 3 arguments');
  }

  const func = args[0].asFunction();
  if (!func) {
    throw new Error('fold-right: first argument must be a procedure');
  }

  const accumulator = args[1];
  let list = args[2];

  // Collect all elements
  const elements: ELObj[] = [];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('fold-right: third argument must be a list');
    }
    elements.push(pair.car);
    list = pair.cdr;
  }

  // Fold from right to left
  let result = accumulator;
  for (let i = elements.length - 1; i >= 0; i--) {
    if (func.isPrimitive()) {
      result = func.callPrimitive([elements[i], result], vm);
    }
  }

  return result;
};

// ============ List Slicing Operations ============

/**
 * List operation: take
 * Port from: SRFI-1 take
 *
 * Returns the first n elements of a list
 */
const takePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('take requires exactly 2 arguments');
  }

  let list = args[0];
  const n = args[1].asNumber();

  if (!n) {
    throw new Error('take: second argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('take: count must be a non-negative integer');
  }

  const count = Math.floor(n.value);
  const result: ELObj[] = [];

  for (let i = 0; i < count; i++) {
    if (list.asNil()) {
      throw new Error('take: list too short');
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('take: argument must be a list');
    }

    result.push(pair.car);
    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: drop
 * Port from: SRFI-1 drop
 *
 * Returns the list without the first n elements
 */
const dropPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('drop requires exactly 2 arguments');
  }

  let list = args[0];
  const n = args[1].asNumber();

  if (!n) {
    throw new Error('drop: second argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('drop: count must be a non-negative integer');
  }

  const count = Math.floor(n.value);

  for (let i = 0; i < count; i++) {
    if (list.asNil()) {
      return theNilObj;
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('drop: argument must be a list');
    }

    list = pair.cdr;
  }

  return list;
};

/**
 * List operation: split-at
 * Port from: SRFI-1 split-at
 *
 * Splits a list at position n, returning both parts as a pair
 */
const splitAtPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('split-at requires exactly 2 arguments');
  }

  let list = args[0];
  const n = args[1].asNumber();

  if (!n) {
    throw new Error('split-at: second argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('split-at: count must be a non-negative integer');
  }

  const count = Math.floor(n.value);
  const firstPart: ELObj[] = [];

  for (let i = 0; i < count; i++) {
    if (list.asNil()) {
      throw new Error('split-at: list too short');
    }

    const pair = list.asPair();
    if (!pair) {
      throw new Error('split-at: argument must be a list');
    }

    firstPart.push(pair.car);
    list = pair.cdr;
  }

  // Reconstruct first part
  let first: ELObj = theNilObj;
  for (let i = firstPart.length - 1; i >= 0; i--) {
    first = makePair(firstPart[i], first);
  }

  // Return as a pair (first-part . second-part)
  return makePair(first, list);
};

/**
 * List operation: take-while
 * Port from: SRFI-1 take-while
 *
 * Returns the longest initial prefix whose elements all satisfy predicate
 */
const takeWhilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('take-while requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('take-while: first argument must be a procedure');
  }

  let list = args[1];
  const result: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('take-while: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (!testResult.isTrue()) {
      break;
    }

    result.push(pair.car);
    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: drop-while
 * Port from: SRFI-1 drop-while
 *
 * Drops the longest initial prefix whose elements all satisfy predicate
 */
const dropWhilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('drop-while requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('drop-while: first argument must be a procedure');
  }

  let list = args[1];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('drop-while: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (!testResult.isTrue()) {
      break;
    }

    list = pair.cdr;
  }

  return list;
};

// ============ List Predicates (SRFI-1) ============

/**
 * List predicate: any?
 * Port from: SRFI-1 any
 *
 * Tests if any element satisfies the predicate
 */
const anyPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('any? requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('any?: first argument must be a procedure');
  }

  let list = args[1];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('any?: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (testResult.isTrue()) {
      return testResult; // Return the true value, not just #t
    }

    list = pair.cdr;
  }

  return theFalseObj;
};

/**
 * List predicate: every?
 * Port from: SRFI-1 every
 *
 * Tests if all elements satisfy the predicate
 */
const everyPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('every? requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('every?: first argument must be a procedure');
  }

  let list = args[1];
  let lastResult: ELObj = theTrueObj;

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('every?: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (!testResult.isTrue()) {
      return theFalseObj;
    }

    lastResult = testResult;
    list = pair.cdr;
  }

  return lastResult; // Return the last true value
};

/**
 * List operation: find
 * Port from: SRFI-1 find
 *
 * Returns the first element that satisfies the predicate, or #f
 */
const findPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('find requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('find: first argument must be a procedure');
  }

  let list = args[1];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('find: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (testResult.isTrue()) {
      return pair.car; // Return the element itself
    }

    list = pair.cdr;
  }

  return theFalseObj;
};

/**
 * List operation: count
 * Port from: SRFI-1 count
 *
 * Counts elements that satisfy the predicate
 */
const countPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('count requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('count: first argument must be a procedure');
  }

  let list = args[1];
  let count = 0;

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('count: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (testResult.isTrue()) {
      count++;
    }

    list = pair.cdr;
  }

  return makeNumber(count, true);
};

/**
 * List operation: partition
 * Port from: SRFI-1 partition
 *
 * Splits a list into two lists based on a predicate
 * Returns (values satisfied unsatisfied)
 */
const partitionPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('partition requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('partition: first argument must be a procedure');
  }

  let list = args[1];
  const satisfied: ELObj[] = [];
  const unsatisfied: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('partition: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (testResult.isTrue()) {
      satisfied.push(pair.car);
    } else {
      unsatisfied.push(pair.car);
    }

    list = pair.cdr;
  }

  // Reconstruct lists
  let satisfiedList: ELObj = theNilObj;
  for (let i = satisfied.length - 1; i >= 0; i--) {
    satisfiedList = makePair(satisfied[i], satisfiedList);
  }

  let unsatisfiedList: ELObj = theNilObj;
  for (let i = unsatisfied.length - 1; i >= 0; i--) {
    unsatisfiedList = makePair(unsatisfied[i], unsatisfiedList);
  }

  // Return as a pair (both lists)
  return makePair(satisfiedList, makePair(unsatisfiedList, theNilObj));
};

/**
 * List operation: last-pair
 * Port from: SRFI-1 last-pair
 *
 * Returns the last pair in a list (not the last element)
 */
const lastPairPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('last-pair requires exactly 1 argument');
  }

  let list = args[0];
  const pair = list.asPair();
  if (!pair) {
    throw new Error('last-pair: argument must be a pair');
  }

  let current = list;
  while (true) {
    const p = current.asPair();
    if (!p) {
      throw new Error('last-pair: improper list');
    }

    // Check if cdr is nil (we're at the last pair)
    if (p.cdr.asNil()) {
      return current;
    }

    current = p.cdr;
  }
};

/**
 * List operation: zip
 * Port from: SRFI-1 zip
 *
 * Combines multiple lists into a list of lists
 * (zip '(1 2) '(a b)) => ((1 a) (2 b))
 */
const zipPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  // Convert all arguments to arrays
  const lists: ELObj[][] = [];
  let minLength = Infinity;

  for (const arg of args) {
    const elements: ELObj[] = [];
    let list = arg;

    while (!list.asNil()) {
      const pair = list.asPair();
      if (!pair) {
        throw new Error('zip: all arguments must be lists');
      }
      elements.push(pair.car);
      list = pair.cdr;
    }

    lists.push(elements);
    minLength = Math.min(minLength, elements.length);
  }

  // Build result list
  const result: ELObj[] = [];
  for (let i = 0; i < minLength; i++) {
    let tuple: ELObj = theNilObj;
    for (let j = lists.length - 1; j >= 0; j--) {
      tuple = makePair(lists[j][i], tuple);
    }
    result.push(tuple);
  }

  // Reconstruct final list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: append-map
 * Port from: SRFI-1 append-map
 *
 * Maps a function over a list and appends the results
 */
const appendMapPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('append-map requires exactly 2 arguments');
  }

  const func = args[0].asFunction();
  if (!func) {
    throw new Error('append-map: first argument must be a procedure');
  }

  let list = args[1];
  const results: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('append-map: second argument must be a list');
    }

    // Call function
    const result = func.isPrimitive()
      ? func.callPrimitive([pair.car], vm)
      : pair.car;

    results.push(result);
    list = pair.cdr;
  }

  // Collect all elements from all result lists
  const allElements: ELObj[] = [];
  for (let i = 0; i < results.length; i++) {
    let current = results[i];

    // Each result should be a list
    while (!current.asNil()) {
      const p = current.asPair();
      if (!p) {
        throw new Error('append-map: function must return lists');
      }
      allElements.push(p.car);
      current = p.cdr;
    }
  }

  // Build final result list
  let finalResult: ELObj = theNilObj;
  for (let i = allElements.length - 1; i >= 0; i--) {
    finalResult = makePair(allElements[i], finalResult);
  }

  return finalResult;
};

// ============ Additional Numeric Predicates ============

/**
 * Numeric predicate: finite?
 * Port from: R6RS finite?
 *
 * Tests if a number is finite (not NaN or infinite)
 */
const finitePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('finite? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(Number.isFinite(num.value));
};

/**
 * Numeric predicate: infinite?
 * Port from: R6RS infinite?
 *
 * Tests if a number is infinite
 */
const infinitePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('infinite? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(!Number.isFinite(num.value) && !Number.isNaN(num.value));
};

/**
 * Numeric predicate: nan?
 * Port from: R6RS nan?
 *
 * Tests if a number is NaN (Not a Number)
 */
const nanPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('nan? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(Number.isNaN(num.value));
};

// ============ Pair Mutation Operations ============

/**
 * Mutation: set-car!
 * Port from: primitive.cxx SetCar::primitiveCall
 *
 * Mutates the car of a pair
 */
const setCarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('set-car! requires exactly 2 arguments');
  }

  const pair = args[0].asPair();
  if (!pair) {
    throw new Error('set-car!: first argument must be a pair');
  }

  // Mutation: update the car
  pair.car = args[1];

  return theNilObj; // Return unspecified (nil)
};

/**
 * Mutation: set-cdr!
 * Port from: primitive.cxx SetCdr::primitiveCall
 *
 * Mutates the cdr of a pair
 */
const setCdrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('set-cdr! requires exactly 2 arguments');
  }

  const pair = args[0].asPair();
  if (!pair) {
    throw new Error('set-cdr!: first argument must be a pair');
  }

  // Mutation: update the cdr
  pair.cdr = args[1];

  return theNilObj; // Return unspecified (nil)
};

/**
 * Mutation: list-set!
 * Port from: SRFI-1 list-set!
 *
 * Sets the element at index in a list
 */
const listSetPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('list-set! requires exactly 3 arguments');
  }

  let list = args[0];
  const index = args[1].asNumber();
  if (!index) {
    throw new Error('list-set!: second argument must be a number');
  }

  if (index.value < 0 || !Number.isInteger(index.value)) {
    throw new Error('list-set!: index must be a non-negative integer');
  }

  const idx = Math.floor(index.value);
  const newValue = args[2];

  // Navigate to the element at index
  let current = list;
  for (let i = 0; i < idx; i++) {
    const pair = current.asPair();
    if (!pair) {
      throw new Error('list-set!: index out of bounds');
    }
    current = pair.cdr;
  }

  const pair = current.asPair();
  if (!pair) {
    throw new Error('list-set!: index out of bounds');
  }

  // Mutate the car
  pair.car = newValue;

  return theNilObj; // Return unspecified (nil)
};

// ============ Additional List Operations (SRFI-1) ============

/**
 * List operation: delete
 * Port from: SRFI-1 delete
 *
 * Removes all elements equal? to x from the list
 */
const deletePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2 || args.length > 3) {
    throw new Error('delete requires 2 or 3 arguments');
  }

  const x = args[0];
  let list = args[1];
  // Third argument would be comparison function (not implemented)

  const result: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('delete: second argument must be a list');
    }

    // Check equality using equal? semantics
    if (!areEqual(pair.car, x)) {
      result.push(pair.car);
    }

    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: delete-duplicates
 * Port from: SRFI-1 delete-duplicates
 *
 * Removes duplicate elements from the list
 */
const deleteDuplicatesPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('delete-duplicates requires exactly 1 argument');
  }

  let list = args[0];
  const seen: ELObj[] = [];
  const result: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('delete-duplicates: argument must be a list');
    }

    // Check if we've seen this element before
    let duplicate = false;
    for (const item of seen) {
      if (areEqual(pair.car, item)) {
        duplicate = true;
        break;
      }
    }

    if (!duplicate) {
      seen.push(pair.car);
      result.push(pair.car);
    }

    list = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: concatenate
 * Port from: SRFI-1 concatenate
 *
 * Concatenates a list of lists into a single list
 */
const concatenatePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('concatenate requires exactly 1 argument');
  }

  let listOfLists = args[0];
  const allElements: ELObj[] = [];

  // Iterate over the list of lists
  while (!listOfLists.asNil()) {
    const pair = listOfLists.asPair();
    if (!pair) {
      throw new Error('concatenate: argument must be a list');
    }

    // Each element should be a list
    let sublist = pair.car;
    while (!sublist.asNil()) {
      const subpair = sublist.asPair();
      if (!subpair) {
        throw new Error('concatenate: elements must be lists');
      }
      allElements.push(subpair.car);
      sublist = subpair.cdr;
    }

    listOfLists = pair.cdr;
  }

  // Build final result
  let finalResult: ELObj = theNilObj;
  for (let i = allElements.length - 1; i >= 0; i--) {
    finalResult = makePair(allElements[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: flatten
 * Flattens a nested list structure into a single list
 */
const flattenPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('flatten requires exactly 1 argument');
  }

  const allElements: ELObj[] = [];

  const flattenHelper = (obj: ELObj): void => {
    if (obj.asNil()) {
      return;
    }

    const pair = obj.asPair();
    if (pair) {
      flattenHelper(pair.car);
      flattenHelper(pair.cdr);
    } else {
      // Atom - add it to results
      allElements.push(obj);
    }
  };

  flattenHelper(args[0]);

  // Build result list
  let result: ELObj = theNilObj;
  for (let i = allElements.length - 1; i >= 0; i--) {
    result = makePair(allElements[i], result);
  }

  return result;
};

/**
 * Conversion: string->vector
 * Converts a string to a vector of characters
 */
const stringToVectorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string->vector requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string->vector requires a string argument');
  }

  const elements: ELObj[] = [];
  for (let i = 0; i < str.value.length; i++) {
    elements.push(makeChar(str.value[i]));
  }

  return makeVector(elements);
};

/**
 * Conversion: vector->string
 * Converts a vector of characters to a string
 */
const vectorToStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('vector->string requires exactly 1 argument');
  }

  const vec = args[0].asVector();
  if (!vec) {
    throw new Error('vector->string requires a vector argument');
  }

  let result = '';
  for (const elem of vec.elements) {
    const ch = elem.asChar();
    if (!ch) {
      throw new Error('vector->string: vector elements must be characters');
    }
    result += ch.value;
  }

  return makeString(result);
};

// ============ Helper Functions ============

/**
 * Helper: equal? predicate (deep structural equality)
 * Port from: primitive.cxx Equal::primitiveCall
 */
function areEqual(obj1: ELObj, obj2: ELObj): boolean {
  // Identity check
  if (obj1 === obj2) return true;

  // Nil check
  if (obj1.asNil() && obj2.asNil()) return true;

  // Number equality
  const num1 = obj1.asNumber();
  const num2 = obj2.asNumber();
  if (num1 && num2) {
    return num1.value === num2.value && num1.exact === num2.exact;
  }

  // String equality
  const str1 = obj1.asString();
  const str2 = obj2.asString();
  if (str1 && str2) {
    return str1.value === str2.value;
  }

  // Symbol equality
  const sym1 = obj1.asSymbol();
  const sym2 = obj2.asSymbol();
  if (sym1 && sym2) {
    return sym1.name === sym2.name;
  }

  // Boolean equality
  const bool1 = obj1.asBoolean();
  const bool2 = obj2.asBoolean();
  if (bool1 && bool2) {
    return bool1.value === bool2.value;
  }

  // Character equality
  const char1 = obj1.asChar();
  const char2 = obj2.asChar();
  if (char1 && char2) {
    return char1.value === char2.value;
  }

  // Pair equality (recursive)
  const pair1 = obj1.asPair();
  const pair2 = obj2.asPair();
  if (pair1 && pair2) {
    return areEqual(pair1.car, pair2.car) && areEqual(pair1.cdr, pair2.cdr);
  }

  // Vector equality (recursive)
  const vec1 = obj1.asVector();
  const vec2 = obj2.asVector();
  if (vec1 && vec2) {
    if (vec1.elements.length !== vec2.elements.length) return false;
    for (let i = 0; i < vec1.elements.length; i++) {
      if (!areEqual(vec1.elements[i], vec2.elements[i])) return false;
    }
    return true;
  }

  return false;
}

// ============ Additional List Predicates (SRFI-1) ============

/**
 * List predicate: null-list?
 * Port from: SRFI-1 null-list?
 *
 * Returns #t if argument is an empty list
 */
const nullListPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('null-list? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asNil() !== null);
};

/**
 * List predicate: proper-list?
 * Port from: SRFI-1 proper-list?
 *
 * Returns #t if argument is a proper list (finite, nil-terminated)
 */
const properListPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('proper-list? requires exactly 1 argument');
  }

  let current = args[0];
  const seen = new Set<ELObj>();

  while (true) {
    if (current.asNil()) {
      return theTrueObj;
    }

    // Check for cycles
    if (seen.has(current)) {
      return theFalseObj; // Circular list
    }
    seen.add(current);

    const pair = current.asPair();
    if (!pair) {
      return theFalseObj; // Dotted list
    }

    current = pair.cdr;
  }
};

/**
 * List predicate: circular-list?
 * Port from: SRFI-1 circular-list?
 *
 * Returns #t if argument is a circular list
 */
const circularListPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('circular-list? requires exactly 1 argument');
  }

  let current = args[0];
  const seen = new Set<ELObj>();

  while (true) {
    if (current.asNil()) {
      return theFalseObj;
    }

    // Check for cycles
    if (seen.has(current)) {
      return theTrueObj; // Found a cycle
    }
    seen.add(current);

    const pair = current.asPair();
    if (!pair) {
      return theFalseObj; // Not a pair
    }

    current = pair.cdr;
  }
};

/**
 * List predicate: dotted-list?
 * Port from: SRFI-1 dotted-list?
 *
 * Returns #t if argument is a finite, non-nil-terminated (dotted) list
 */
const dottedListPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('dotted-list? requires exactly 1 argument');
  }

  let current = args[0];
  const seen = new Set<ELObj>();

  while (true) {
    if (current.asNil()) {
      return theFalseObj; // Proper list
    }

    // Check for cycles
    if (seen.has(current)) {
      return theFalseObj; // Circular list
    }
    seen.add(current);

    const pair = current.asPair();
    if (!pair) {
      return theTrueObj; // Found a non-nil atom - dotted list
    }

    current = pair.cdr;
  }
};

/**
 * List predicate: not-pair?
 * Opposite of pair?
 */
const notPairPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('not-pair? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asPair() === null);
};

/**
 * List operation: list-index
 * Port from: SRFI-1 list-index
 *
 * Returns the index of the first element that satisfies predicate
 */
const listIndexPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('list-index requires exactly 2 arguments');
  }

  const pred = args[0].asFunction();
  if (!pred) {
    throw new Error('list-index: first argument must be a procedure');
  }

  let list = args[1];
  let index = 0;

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list-index: second argument must be a list');
    }

    // Call predicate
    const testResult = pred.isPrimitive()
      ? pred.callPrimitive([pair.car], vm)
      : pair.car;

    if (testResult.isTrue()) {
      return makeNumber(index, true);
    }

    index++;
    list = pair.cdr;
  }

  return theFalseObj; // Not found
};

/**
 * List operation: take-right
 * Port from: SRFI-1 take-right
 *
 * Returns the last n elements of the list
 */
const takeRightPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('take-right requires exactly 2 arguments');
  }

  let list = args[0];
  const n = args[1].asNumber();

  if (!n) {
    throw new Error('take-right: second argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('take-right: count must be a non-negative integer');
  }

  const count = Math.floor(n.value);

  // Collect all elements
  const elements: ELObj[] = [];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('take-right: first argument must be a list');
    }
    elements.push(pair.car);
    list = pair.cdr;
  }

  // Take last n elements
  const start = Math.max(0, elements.length - count);
  const result = elements.slice(start);

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: drop-right
 * Port from: SRFI-1 drop-right
 *
 * Returns all but the last n elements of the list
 */
const dropRightPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('drop-right requires exactly 2 arguments');
  }

  let list = args[0];
  const n = args[1].asNumber();

  if (!n) {
    throw new Error('drop-right: second argument must be a number');
  }

  if (n.value < 0 || !Number.isInteger(n.value)) {
    throw new Error('drop-right: count must be a non-negative integer');
  }

  const count = Math.floor(n.value);

  // Collect all elements
  const elements: ELObj[] = [];
  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('drop-right: first argument must be a list');
    }
    elements.push(pair.car);
    list = pair.cdr;
  }

  // Drop last n elements
  const end = Math.max(0, elements.length - count);
  const result = elements.slice(0, end);

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

// ============ Bitwise Operations ============

/**
 * Bitwise operation: bitwise-and
 * Port from: R6RS bitwise-and
 */
const bitwiseAndPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNumber(-1, true); // All bits set
  }

  let result = -1;
  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('bitwise-and requires integer arguments');
    }
    if (!Number.isInteger(num.value)) {
      throw new Error('bitwise-and requires integer arguments');
    }
    result = result & Math.floor(num.value);
  }

  return makeNumber(result, true);
};

/**
 * Bitwise operation: bitwise-ior (inclusive or)
 * Port from: R6RS bitwise-ior
 */
const bitwiseIorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNumber(0, true);
  }

  let result = 0;
  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('bitwise-ior requires integer arguments');
    }
    if (!Number.isInteger(num.value)) {
      throw new Error('bitwise-ior requires integer arguments');
    }
    result = result | Math.floor(num.value);
  }

  return makeNumber(result, true);
};

/**
 * Bitwise operation: bitwise-xor
 * Port from: R6RS bitwise-xor
 */
const bitwiseXorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNumber(0, true);
  }

  let result = 0;
  for (const arg of args) {
    const num = arg.asNumber();
    if (!num) {
      throw new Error('bitwise-xor requires integer arguments');
    }
    if (!Number.isInteger(num.value)) {
      throw new Error('bitwise-xor requires integer arguments');
    }
    result = result ^ Math.floor(num.value);
  }

  return makeNumber(result, true);
};

/**
 * Bitwise operation: bitwise-not
 * Port from: R6RS bitwise-not
 */
const bitwiseNotPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('bitwise-not requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('bitwise-not requires an integer argument');
  }
  if (!Number.isInteger(num.value)) {
    throw new Error('bitwise-not requires an integer argument');
  }

  return makeNumber(~Math.floor(num.value), true);
};

/**
 * Bitwise operation: arithmetic-shift
 * Port from: R6RS arithmetic-shift
 *
 * Shifts n left by count bits (or right if count is negative)
 */
const arithmeticShiftPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('arithmetic-shift requires exactly 2 arguments');
  }

  const n = args[0].asNumber();
  const count = args[1].asNumber();

  if (!n || !count) {
    throw new Error('arithmetic-shift requires integer arguments');
  }
  if (!Number.isInteger(n.value) || !Number.isInteger(count.value)) {
    throw new Error('arithmetic-shift requires integer arguments');
  }

  const nVal = Math.floor(n.value);
  const countVal = Math.floor(count.value);

  let result: number;
  if (countVal >= 0) {
    result = nVal << countVal;
  } else {
    result = nVal >> (-countVal);
  }

  return makeNumber(result, true);
};

// ============ Additional Math Operations ============

/**
 * Math operation: quotient+remainder
 * Returns both quotient and remainder as a list
 */
const quotientRemainderPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('quotient+remainder requires exactly 2 arguments');
  }

  const n1 = args[0].asNumber();
  const n2 = args[1].asNumber();

  if (!n1 || !n2) {
    throw new Error('quotient+remainder requires numeric arguments');
  }

  if (n2.value === 0) {
    throw new Error('quotient+remainder: division by zero');
  }

  const quotient = Math.trunc(n1.value / n2.value);
  const remainder = n1.value - quotient * n2.value;

  // Return as a list (quotient remainder)
  return makePair(
    makeNumber(quotient, n1.exact && n2.exact),
    makePair(
      makeNumber(remainder, n1.exact && n2.exact),
      theNilObj
    )
  );
};

// ============ Additional String Operations ============

/**
 * String operation: string-null?
 * Tests if a string is empty
 */
const stringNullPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-null? requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-null? requires a string argument');
  }

  return makeBoolean(str.value.length === 0);
};

/**
 * String operation: string-contains?
 * Tests if string contains substring
 */
const stringContainsPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-contains? requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const substr = args[1].asString();

  if (!str || !substr) {
    throw new Error('string-contains? requires string arguments');
  }

  return makeBoolean(str.value.includes(substr.value));
};

/**
 * String operation: string-prefix?
 * Tests if string starts with prefix
 */
const stringPrefixPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-prefix? requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const prefix = args[1].asString();

  if (!str || !prefix) {
    throw new Error('string-prefix? requires string arguments');
  }

  return makeBoolean(str.value.startsWith(prefix.value));
};

/**
 * String operation: string-suffix?
 * Tests if string ends with suffix
 */
const stringSuffixPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-suffix? requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const suffix = args[1].asString();

  if (!str || !suffix) {
    throw new Error('string-suffix? requires string arguments');
  }

  return makeBoolean(str.value.endsWith(suffix.value));
};

/**
 * String operation: string-index
 * Returns the index of the first occurrence of char in string
 */
const stringIndexPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-index requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const ch = args[1].asChar();

  if (!str || !ch) {
    throw new Error('string-index requires a string and a character');
  }

  const index = str.value.indexOf(ch.value);
  return index >= 0 ? makeNumber(index, true) : theFalseObj;
};

/**
 * String operation: string-reverse
 * Returns a reversed copy of the string
 */
const stringReversePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-reverse requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-reverse requires a string argument');
  }

  return makeString(str.value.split('').reverse().join(''));
};

/**
 * String operation: string-trim
 * Removes whitespace from both ends of string
 */
const stringTrimPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-trim requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-trim requires a string argument');
  }

  return makeString(str.value.trim());
};

// ============ Additional Numeric Operations ============

/**
 * Numeric operation: clamp
 * Clamps a value between min and max
 */
const clampPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('clamp requires exactly 3 arguments');
  }

  const x = args[0].asNumber();
  const min = args[1].asNumber();
  const max = args[2].asNumber();

  if (!x || !min || !max) {
    throw new Error('clamp requires numeric arguments');
  }

  let result: number;
  if (x.value < min.value) {
    result = min.value;
  } else if (x.value > max.value) {
    result = max.value;
  } else {
    result = x.value;
  }

  return makeNumber(result, x.exact && min.exact && max.exact);
};

/**
 * Numeric operation: sgn (sign)
 * Returns -1, 0, or 1 depending on sign of number
 */
const sgnPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('sgn requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('sgn requires a numeric argument');
  }

  if (num.value > 0) return makeNumber(1, true);
  if (num.value < 0) return makeNumber(-1, true);
  return makeNumber(0, true);
};

/**
 * Numeric operation: exact-integer?
 * Tests if argument is an exact integer
 */
const exactIntegerPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('exact-integer? requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    return theFalseObj;
  }

  return makeBoolean(num.exact && Number.isInteger(num.value));
};

/**
 * Numeric operation: make-rectangular
 * Creates a complex number (stub - returns first arg)
 */
const makeRectangularPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('make-rectangular requires exactly 2 arguments');
  }

  const real = args[0].asNumber();
  const imag = args[1].asNumber();

  if (!real || !imag) {
    throw new Error('make-rectangular requires numeric arguments');
  }

  // Simplified: return real part only (full complex number support not implemented)
  return real;
};

/**
 * Numeric operation: make-polar
 * Creates a complex number from polar coordinates (stub)
 */
const makePolarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('make-polar requires exactly 2 arguments');
  }

  const magnitude = args[0].asNumber();
  const angle = args[1].asNumber();

  if (!magnitude || !angle) {
    throw new Error('make-polar requires numeric arguments');
  }

  // Simplified: return magnitude only (full complex number support not implemented)
  return magnitude;
};

/**
 * Numeric operation: real-part
 * Returns real part of number (stub - returns number itself)
 */
const realPartPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('real-part requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('real-part requires a numeric argument');
  }

  return num;
};

/**
 * Numeric operation: imag-part
 * Returns imaginary part of number (stub - returns 0)
 */
const imagPartPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('imag-part requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('imag-part requires a numeric argument');
  }

  // Simplified: return 0 (full complex number support not implemented)
  return makeNumber(0, true);
};

/**
 * Numeric operation: magnitude
 * Returns magnitude of number (stub - returns abs)
 */
const magnitudePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('magnitude requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('magnitude requires a numeric argument');
  }

  return makeNumber(Math.abs(num.value), num.exact);
};

/**
 * Numeric operation: angle
 * Returns angle of number (stub - returns 0 or pi)
 */
const anglePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('angle requires exactly 1 argument');
  }

  const num = args[0].asNumber();
  if (!num) {
    throw new Error('angle requires a numeric argument');
  }

  // Simplified: return 0 for positive, pi for negative
  return makeNumber(num.value < 0 ? Math.PI : 0, false);
};

// ============ Additional List Operations ============

/**
 * List operation: circular-list
 * Creates a circular list (simplified - creates proper list)
 */
const circularListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  // Simplified: create proper list instead of circular
  // (true circular lists require mutation)
  let result: ELObj = theNilObj;
  for (let i = args.length - 1; i >= 0; i--) {
    result = makePair(args[i], result);
  }

  return result;
};

/**
 * List operation: append!
 * Destructive append (mutates last cdr of first list)
 */
const appendInPlacePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  if (args.length === 1) {
    return args[0];
  }

  // Find last pair of first list
  let first = args[0];
  if (first.asNil()) {
    // If first is nil, return append of remaining lists
    return appendInPlacePrimitive(args.slice(1), vm);
  }

  let current = first;
  let lastPair: PairObj | null = null;

  while (!current.asNil()) {
    const pair = current.asPair();
    if (!pair) {
      throw new Error('append!: arguments must be lists');
    }
    lastPair = pair;
    current = pair.cdr;
  }

  if (lastPair) {
    // Mutate the cdr of the last pair
    lastPair.cdr = appendInPlacePrimitive(args.slice(1), vm);
  }

  return first;
};

/**
 * List operation: reverse!
 * Destructive reverse (already implemented, this is an alias)
 */
// Already implemented as reverseInPlacePrimitive

/**
 * List operation: lset-union
 * Union of lists (treating them as sets)
 */
const lsetUnionPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  const seen: ELObj[] = [];
  const result: ELObj[] = [];

  for (const listArg of args) {
    let list = listArg;
    while (!list.asNil()) {
      const pair = list.asPair();
      if (!pair) {
        throw new Error('lset-union: arguments must be lists');
      }

      // Check if we've seen this element
      let found = false;
      for (const item of seen) {
        if (areEqual(pair.car, item)) {
          found = true;
          break;
        }
      }

      if (!found) {
        seen.push(pair.car);
        result.push(pair.car);
      }

      list = pair.cdr;
    }
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

/**
 * List operation: lset-intersection
 * Intersection of two lists (treating them as sets)
 */
const lsetIntersectionPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }

  if (args.length === 1) {
    return args[0];
  }

  // Get all elements from second list
  const secondSet: ELObj[] = [];
  let list2 = args[1];
  while (!list2.asNil()) {
    const pair = list2.asPair();
    if (!pair) {
      throw new Error('lset-intersection: arguments must be lists');
    }
    secondSet.push(pair.car);
    list2 = pair.cdr;
  }

  // Keep elements from first list that are in second list
  const result: ELObj[] = [];
  let list1 = args[0];
  while (!list1.asNil()) {
    const pair = list1.asPair();
    if (!pair) {
      throw new Error('lset-intersection: arguments must be lists');
    }

    // Check if element is in second set
    let found = false;
    for (const item of secondSet) {
      if (areEqual(pair.car, item)) {
        found = true;
        break;
      }
    }

    if (found) {
      result.push(pair.car);
    }

    list1 = pair.cdr;
  }

  // Reconstruct list
  let finalResult: ELObj = theNilObj;
  for (let i = result.length - 1; i >= 0; i--) {
    finalResult = makePair(result[i], finalResult);
  }

  return finalResult;
};

// ============ Utility Functions ============

/**
 * Utility: identity
 * Returns its argument unchanged
 */
const identityPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('identity requires exactly 1 argument');
  }
  return args[0];
};

/**
 * Utility: values
 * Port from: R5RS values
 *
 * Note: Full implementation would support multiple return values.
 * For now, single value implementation.
 */
const valuesPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return theNilObj;
  }
  if (args.length === 1) {
    return args[0];
  }
  // For multiple values, return as a list
  let result: ELObj = theNilObj;
  for (let i = args.length - 1; i >= 0; i--) {
    result = makePair(args[i], result);
  }
  return result;
};

/**
 * Utility: call-with-values
 * Port from: R5RS call-with-values
 *
 * Note: Simplified implementation for single values.
 */
const callWithValuesPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('call-with-values requires exactly 2 arguments');
  }

  const producer = args[0].asFunction();
  const consumer = args[1].asFunction();
  if (!producer || !consumer) {
    throw new Error('call-with-values: arguments must be procedures');
  }

  // Call producer with no arguments
  const values = producer.callPrimitive([], vm);

  // If values is a list, unpack it
  const valuesList: ELObj[] = [];
  let current = values;
  while (current.asPair()) {
    const pair = current.asPair()!;
    valuesList.push(pair.car);
    current = pair.cdr;
  }

  // If we didn't unpack anything, it's a single value
  if (valuesList.length === 0) {
    return consumer.callPrimitive([values], vm);
  }

  // Call consumer with the values
  return consumer.callPrimitive(valuesList, vm);
};

// ============ Error Handling ============

/**
 * Error handling: error
 * Port from: primitive.cxx Error::primitiveCall
 */
const errorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1) {
    throw new Error('error requires at least 1 argument');
  }

  const msg = args[0].asString();
  if (!msg) {
    throw new Error('error: first argument must be a string');
  }

  // Collect additional arguments for error context
  const irritants: string[] = [];
  for (let i = 1; i < args.length; i++) {
    const str = args[i].asString();
    if (str) {
      irritants.push(str.value);
    } else {
      irritants.push(String(args[i]));
    }
  }

  const fullMsg = irritants.length > 0
    ? `${msg.value}: ${irritants.join(', ')}`
    : msg.value;

  throw new Error(fullMsg);
};

/**
 * debug - Output debug information and return the value
 * Port from: OpenJade external procedure debug (primitive.cxx line 4407)
 * Port from: ELObjMessageArg calls obj->print() to convert any type
 *
 * This is used for debugging DSSSL code. It outputs the value to stderr
 * with location information and returns the value unchanged (pass-through).
 *
 * Usage: (debug expr) - outputs expr's value and returns expr
 */
const debugPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('debug requires exactly 1 argument');
  }

  const value = args[0];

  // Format the value using print representation (like OpenJade's obj->print())
  // Port from: ELObjMessageArg::convert calls obj->print(interp, os)
  let valueStr: string;

  const str = value.asString();
  if (str) {
    // Strings are shown with surrounding quotes
    valueStr = `"${str.value}"`;
  } else {
    const num = value.asNumber();
    const bool = value.asBoolean();
    const sym = value.asSymbol();
    const node = value.asNode();
    const nodeList = value.asNodeList();

    if (num) {
      valueStr = num.value.toString();
    } else if (bool !== null) {
      valueStr = bool.value ? '#t' : '#f';
    } else if (sym) {
      valueStr = sym.name;
    } else if (value.asNil()) {
      valueStr = '()';
    } else if (node) {
      // Show node with gi
      const gi = node.node.gi();
      valueStr = gi ? `#<node ${gi}>` : '#<node>';
    } else if (nodeList) {
      valueStr = `#<node-list length=${nodeList.nodes.length()}>`;
    } else {
      // For other types, show type name
      valueStr = `#<${value.constructor.name}>`;
    }
  }

  // Output in OpenJade format: dazzle:file:line:col:I: debug "value"
  // Port from: OpenJade outputs "openjade:file:line:col:I: debug value"
  const file = makeRelativePath(vm.currentFile);
  const line = vm.currentLine;
  const col = vm.currentColumn;
  console.error(`dazzle:${file}:${line}:${col}:I: debug ${valueStr}`);

  // Return the value unchanged (pass-through)
  // Port from: OpenJade returns argv[0]
  return value;
};

/**
 * external-procedure - Look up an external procedure by public ID
 * Port from: OpenJade primitive.cxx line 2115
 *
 * External procedures are registered with public IDs like:
 * - "ISO/IEC 10179:1996//Procedure::<name>" for DSSSL standard procedures
 * - "UNREGISTERED::James Clark//Procedure::<name>" for James Clark extensions
 * - "UNREGISTERED::OpenJade//Procedure::<name>" for OpenJade extensions
 *
 * Usage: (external-procedure "UNREGISTERED::James Clark//Procedure::debug")
 * Returns: The function object if found, #f otherwise
 */
const externalProcedurePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('external-procedure requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('external-procedure requires a string argument');
  }

  const pubid = str.value;

  // Look up the procedure in the external procedure table
  // Port from: OpenJade lookupExternalProc (Interpreter.h line 704)
  const func = externalProcTable.get(pubid);

  if (func) {
    return func;
  }

  // Return #f if not found
  // Port from: OpenJade returns 0 (null), which becomes #f
  return makeBoolean(false);
};

// External procedure table
// Port from: OpenJade externalProcTable_ (Interpreter.h line 483)
// Maps public IDs to function objects
const externalProcTable = new Map<string, FunctionObj>();

// ============ Additional List Utilities ============

/**
 * List utility: unfold
 * Port from: SRFI-1 unfold
 *
 * Constructs a list by repeatedly applying a function
 * (unfold stop? mapper successor seed [tail-gen])
 */
const unfoldPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 4 || args.length > 5) {
    throw new Error('unfold requires 4 or 5 arguments');
  }

  const stopPred = args[0].asFunction();
  const mapper = args[1].asFunction();
  const successor = args[2].asFunction();

  if (!stopPred || !mapper || !successor) {
    throw new Error('unfold: first three arguments must be procedures');
  }

  let seed = args[3];
  const tailGen = args.length === 5 ? args[4].asFunction() : null;

  const elements: ELObj[] = [];

  // Generate elements
  while (true) {
    // Check stop condition
    const shouldStop = stopPred.isPrimitive()
      ? stopPred.callPrimitive([seed], vm)
      : seed;

    if (shouldStop.isTrue()) {
      break;
    }

    // Map the seed to an element
    const element = mapper.isPrimitive()
      ? mapper.callPrimitive([seed], vm)
      : seed;

    elements.push(element);

    // Get next seed
    seed = successor.isPrimitive()
      ? successor.callPrimitive([seed], vm)
      : seed;
  }

  // Build result list
  let result: ELObj = tailGen && tailGen.isPrimitive()
    ? tailGen.callPrimitive([seed], vm)
    : theNilObj;

  for (let i = elements.length - 1; i >= 0; i--) {
    result = makePair(elements[i], result);
  }

  return result;
};

/**
 * List utility: fold
 * Port from: SRFI-1 fold (left fold)
 *
 * (fold kons knil list)
 */
const foldPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('fold requires exactly 3 arguments');
  }

  const kons = args[0].asFunction();
  if (!kons) {
    throw new Error('fold: first argument must be a procedure');
  }

  let accumulator = args[1];
  let list = args[2];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('fold: third argument must be a list');
    }

    accumulator = kons.isPrimitive()
      ? kons.callPrimitive([pair.car, accumulator], vm)
      : accumulator;

    list = pair.cdr;
  }

  return accumulator;
};

/**
 * List utility: unfold-right
 * Port from: SRFI-1 unfold-right
 *
 * Like unfold but builds list from right to left
 */
const unfoldRightPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 4 || args.length > 5) {
    throw new Error('unfold-right requires 4 or 5 arguments');
  }

  const stopPred = args[0].asFunction();
  const mapper = args[1].asFunction();
  const successor = args[2].asFunction();

  if (!stopPred || !mapper || !successor) {
    throw new Error('unfold-right: first three arguments must be procedures');
  }

  let seed = args[3];
  let result: ELObj = args.length === 5 ? args[4] : theNilObj;

  // Generate elements from right to left
  while (true) {
    // Check stop condition
    const shouldStop = stopPred.isPrimitive()
      ? stopPred.callPrimitive([seed], vm)
      : seed;

    if (shouldStop.isTrue()) {
      break;
    }

    // Map the seed to an element
    const element = mapper.isPrimitive()
      ? mapper.callPrimitive([seed], vm)
      : seed;

    // Cons onto result
    result = makePair(element, result);

    // Get next seed
    seed = successor.isPrimitive()
      ? successor.callPrimitive([seed], vm)
      : seed;
  }

  return result;
};

// ============ Additional I/O Operations (Stubs) ============

/**
 * I/O operation: open-input-file
 * Port from: R5RS open-input-file
 *
 * Stub implementation (no real file I/O yet)
 */
const openInputFilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('open-input-file requires exactly 1 argument');
  }

  const filename = args[0].asString();
  if (!filename) {
    throw new Error('open-input-file requires a string argument');
  }

  // Stub: return a dummy port object (nil for now)
  return theNilObj;
};

/**
 * I/O operation: open-output-file
 * Port from: R5RS open-output-file
 *
 * Stub implementation (no real file I/O yet)
 */
const openOutputFilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('open-output-file requires exactly 1 argument');
  }

  const filename = args[0].asString();
  if (!filename) {
    throw new Error('open-output-file requires a string argument');
  }

  // Stub: return a dummy port object (nil for now)
  return theNilObj;
};

/**
 * I/O operation: close-input-port
 * Port from: R5RS close-input-port
 *
 * Stub implementation
 */
const closeInputPortPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('close-input-port requires exactly 1 argument');
  }

  // Stub: do nothing
  return theNilObj;
};

/**
 * I/O operation: close-output-port
 * Port from: R5RS close-output-port
 *
 * Stub implementation
 */
const closeOutputPortPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('close-output-port requires exactly 1 argument');
  }

  // Stub: do nothing
  return theNilObj;
};

/**
 * I/O operation: read
 * Port from: R5RS read
 *
 * Stub implementation (no real reading yet)
 */
const readPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length > 1) {
    throw new Error('read requires 0 or 1 arguments');
  }

  // Stub: return nil
  return theNilObj;
};

/**
 * I/O operation: read-char
 * Port from: R5RS read-char
 *
 * Stub implementation
 */
const readCharPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length > 1) {
    throw new Error('read-char requires 0 or 1 arguments');
  }

  // Stub: return nil
  return theNilObj;
};

/**
 * I/O operation: peek-char
 * Port from: R5RS peek-char
 *
 * Stub implementation
 */
const peekCharPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length > 1) {
    throw new Error('peek-char requires 0 or 1 arguments');
  }

  // Stub: return nil
  return theNilObj;
};

/**
 * I/O operation: write-char
 * Port from: R5RS write-char
 *
 * Stub implementation
 */
const writeCharPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('write-char requires 1 or 2 arguments');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('write-char requires a character argument');
  }

  // Stub: do nothing (would write to output port)
  return theNilObj;
};

// ============ Additional String/Symbol Operations ============

/**
 * String operation: string-append
 * Already implemented, but ensuring it's exported
 */

/**
 * Symbol operation: symbol->string
 * Already implemented
 */

/**
 * Symbol operation: string->symbol
 * Already implemented
 */

/**
 * Symbol operation: gensym
 * Port from: Common Lisp gensym
 *
 * Generates a unique symbol
 */
let gensymCounter = 0;

const gensymPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length > 1) {
    throw new Error('gensym requires 0 or 1 arguments');
  }

  const prefix = args.length === 1
    ? (args[0].asString()?.value ?? args[0].asSymbol()?.name ?? 'g')
    : 'g';

  const name = `${prefix}${gensymCounter++}`;
  return makeSymbol(name);
};

// ============ Additional I/O Operations (Stubs) ============

/**
 * I/O operation: call-with-input-file
 * Port from: R5RS call-with-input-file
 *
 * Stub implementation (no real file I/O yet)
 */
const callWithInputFilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('call-with-input-file requires exactly 2 arguments');
  }

  const filename = args[0].asString();
  const proc = args[1].asFunction();

  if (!filename) {
    throw new Error('call-with-input-file requires a string as first argument');
  }

  if (!proc) {
    throw new Error('call-with-input-file requires a procedure as second argument');
  }

  // Stub: return nil (no real file I/O)
  return theNilObj;
};

/**
 * I/O operation: call-with-output-file
 * Port from: R5RS call-with-output-file
 *
 * Stub implementation (no real file I/O yet)
 */
const callWithOutputFilePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('call-with-output-file requires exactly 2 arguments');
  }

  const filename = args[0].asString();
  const proc = args[1].asFunction();

  if (!filename) {
    throw new Error('call-with-output-file requires a string as first argument');
  }

  if (!proc) {
    throw new Error('call-with-output-file requires a procedure as second argument');
  }

  // Stub: return nil (no real file I/O)
  return theNilObj;
};

/**
 * I/O operation: load
 * Port from: R5RS load
 *
 * Stub implementation (no real file I/O yet)
 */
const loadPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('load requires exactly 1 argument');
  }

  const filename = args[0].asString();

  if (!filename) {
    throw new Error('load requires a string argument');
  }

  // Stub: return nil (no real file loading)
  return theNilObj;
};

// ============ Grove Primitives (DSSSL §9) ============
// Port from: OpenJade primitive.h/primitive.cxx
//
// Architecture: Primitives are thin wrappers around Node interface methods.
// The real work happens in the Node implementation (e.g., LibxmljsNode).

/**
 * Grove: gi (generic identifier - element name)
 * Port from: primitive.h PRIMITIVE(Gi, "gi", 0, 1, 0)
 *
 * Returns a string (not a symbol) containing the element name.
 * OpenJade returns a string for gi.
 */
const giPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('gi requires exactly 1 argument');
  }

  // Port from: OpenJade Gi primitive uses optSingletonNodeList
  // Accepts nodes, singleton node-lists, and empty node-lists
  const node = optSingletonNode(args[0]);
  if (!node) {
    // Empty node-list -> return #f (not an error)
    // Port from: OpenJade returns makeFalse() when node is null
    return theFalseObj;
  }

  const gi = node.gi();
  // Port from: OpenJade returns StringObj even for empty string, only #f for null
  return gi !== null ? makeString(gi) : theFalseObj;
};

/**
 * Grove: id (ID attribute value)
 * Port from: primitive.h PRIMITIVE(Id, "id", 0, 1, 0)
 */
const idPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('id requires exactly 1 argument');
  }

  // Port from: OpenJade Id primitive uses optSingletonNodeList
  // Accepts nodes, singleton node-lists, and empty node-lists
  const node = optSingletonNode(args[0]);
  if (!node) {
    // Empty node-list -> return #f (not an error)
    // Port from: OpenJade returns makeFalse() when node is null
    return theFalseObj;
  }

  const id = node.id();
  // Port from: OpenJade returns StringObj even for empty string, only #f for null
  return id !== null ? makeString(id) : theFalseObj;
};

/**
 * Grove: element-with-id
 * Port from: primitive.cxx DEFPRIMITIVE(ElementWithId)
 *
 * Looks up an element by ID in the document.
 * Takes an ID string and optional node (defaults to current node).
 * The node parameter determines which grove to search (for multi-document scenarios).
 * Returns singleton node-list containing the element, or empty node-list if not found.
 */
const elementWithIdPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('element-with-id requires 1 or 2 arguments');
  }

  const idStr = args[0].asString();
  if (!idStr) {
    const argType = args[0].constructor.name;
    const argValue = args[0].asBoolean() !== null ? (args[0].asBoolean() ? '#t' : '#f') :
                     args[0].asNil() ? '()' : argType;
    throw new Error(`element-with-id requires a string as first argument, got ${argValue}`);
  }

  // Get the node to search from (defaults to current node)
  // Port from: OpenJade ElementWithId - argc > 1 check
  let searchNode: Node;
  if (args.length === 2) {
    const nodeArg = args[1].asNode();
    if (!nodeArg) {
      throw new Error('element-with-id requires a node as second argument');
    }
    searchNode = nodeArg.node;
  } else {
    if (!vm.currentNode) {
      throw new Error('element-with-id: no current node');
    }
    searchNode = vm.currentNode;
  }

  // Get the grove root from the node
  // Port from: OpenJade node->getGroveRoot(node)
  const groveRoot = searchNode.documentNode();

  // Get the grove to perform ID lookup
  // Port from: OpenJade node->getElements(elements) && elements->namedNode(id, node)
  // In our architecture, the Grove object has elementWithId()
  if (!vm.grove) {
    throw new Error('element-with-id: no grove available');
  }

  // Look up element by ID
  const element = vm.grove.elementWithId(idStr.value);
  if (element) {
    // Return singleton node-list
    // Port from: OpenJade new (interp) NodePtrNodeListObj(node)
    return makeNodeList(nodeListFromArray([element]));
  }

  // Port from: OpenJade interp.makeEmptyNodeList()
  return makeNodeList(EMPTY_NODE_LIST);
};

/**
 * Grove: data (character data content)
 * Port from: primitive.h PRIMITIVE(Data, "data", 0, 1, 0)
 */
const dataPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  // Port from: OpenJade PRIMITIVE(Data, "data", 0, 1, 0)
  // 0 required, 1 optional - can be called with 0 or 1 arguments
  if (args.length > 1) {
    throw new Error('data takes at most 1 argument');
  }

  let nodeObj: ELObj;
  if (args.length === 0) {
    // Use current-node from VM context
    const groveNode = vm.currentNode;
    if (!groveNode) {
      throw new Error('data: no current node');
    }
    nodeObj = makeNode(groveNode);
  } else {
    nodeObj = args[0];
  }

  // Check if it's a node
  let node = nodeObj.asNode();

  // If it's a node-list, concatenate data from all nodes (DSSSL spec)
  if (!node) {
    const nodeList = nodeObj.asNodeList();
    if (nodeList) {
      // Empty node-list returns #f
      if (nodeList.nodes.isEmpty()) {
        return theFalseObj;
      }

      // Concatenate data from all nodes in the list
      // Per DSSSL spec: "Returns a string containing the concatenation of the data of each member of nl"
      const parts: string[] = [];
      let current: NodeList | null = nodeList.nodes;

      while (current && !current.isEmpty()) {
        const firstNode = current.first();
        if (firstNode) {
          const data = firstNode.data();
          if (data !== null) {
            parts.push(data);
          }
        }
        current = current.rest();
      }

      return parts.length > 0 ? makeString(parts.join('')) : theFalseObj;
    }
  }

  if (!node) {
    throw new Error('data requires a node or node-list argument');
  }

  const data = node.node.data();
  // Port from: OpenJade returns StringObj even for empty string, only #f for null
  return data !== null ? makeString(data) : theFalseObj;
};

/**
 * Grove: parent
 * Port from: primitive.h PRIMITIVE(Parent, "parent", 0, 1, 0)
 */
const parentPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  // Port from: OpenJade PRIMITIVE(Parent, "parent", 0, 1, 0)
  // 0 required, 1 optional - can be called with 0 or 1 arguments
  if (args.length > 1) {
    throw new Error('parent takes at most 1 argument');
  }

  let nodeObj: ELObj;
  if (args.length === 0) {
    // Use current-node from VM context
    const groveNode = vm.currentNode;
    if (!groveNode) {
      throw new Error('parent: no current node');
    }
    nodeObj = makeNode(groveNode);
  } else {
    nodeObj = args[0];
  }

  // Check if it's a node
  let node = nodeObj.asNode();

  // If it's a node-list, extract the first node
  if (!node) {
    const nodeList = nodeObj.asNodeList();
    if (nodeList) {
      const first = nodeList.nodes.first();
      if (!first) {
        // Empty node-list
        return theFalseObj;
      }
      nodeObj = makeNode(first);
      node = nodeObj.asNode();
    }
  }

  if (!node) {
    throw new Error('parent requires a node or node-list argument');
  }

  const parent = node.node.parent();
  return parent ? makeNode(parent) : theFalseObj;
};

/**
 * Grove: children
 * Port from: primitive.h PRIMITIVE(Children, "children", 0, 1, 0)
 */
const childrenPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  // Takes 0 or 1 argument - if 0, uses current-node
  // Argument can be a node or node-list
  if (args.length > 1) {
    throw new Error('children takes at most 1 argument');
  }

  if (args.length === 0) {
    // Use current-node from VM context
    const groveNode = vm.currentNode;
    if (!groveNode) {
      throw new Error('children: no current node');
    }
    return makeNodeList(groveNode.children());
  }

  // Check if argument is a node
  const nodeObj = args[0].asNode();
  if (nodeObj) {
    return makeNodeList(nodeObj.node.children());
  }

  // Check if argument is a node-list
  const nodeListObj = args[0].asNodeList();
  if (nodeListObj) {
    // Return children of all nodes in the list
    const allChildren: Node[] = [];
    const nodeArray = nodeListObj.nodes.toArray();
    for (const node of nodeArray) {
      const children = node.children().toArray();
      allChildren.push(...children);
    }
    return makeNodeList(nodeListFromArray(allChildren));
  }

  throw new Error('children requires a node or node-list argument');
};

/**
 * Grove: node-list? predicate
 * Port from: primitive.h PRIMITIVE(NodeListP, "node-list?", 0, 1, 0)
 */
const nodeListPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list? requires exactly 1 argument');
  }

  return args[0].asNodeList() ? theTrueObj : theFalseObj;
};

/**
 * Grove: node-list-length
 * Port from: primitive.h PRIMITIVE(NodeListLength, "node-list-length", 0, 1, 0)
 */
const nodeListLengthPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list-length requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-length requires a node-list argument');
  }

  return makeNumber(nl.nodes.length());
};

/**
 * Grove: node-list->list
 * Convert a node-list to a Scheme list
 * Port from: OpenJade extension for interoperability
 */
const nodeListToListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list->list requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list->list requires a node-list argument');
  }

  // Convert node-list to proper Scheme list
  let result: ELObj = theNilObj;
  const nodesArray = nl.nodes.toArray();

  // Build list in reverse order (cons from end to start)
  for (let i = nodesArray.length - 1; i >= 0; i--) {
    result = makePair(makeNode(nodesArray[i]), result);
  }

  return result;
};

/**
 * Grove: empty-node-list
 * Port from: primitive.h PRIMITIVE(EmptyNodeList, "empty-node-list", 0, 0, 0)
 */
const emptyNodeListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 0) {
    throw new Error('empty-node-list requires no arguments');
  }

  return makeNodeList(EMPTY_NODE_LIST);
};

/**
 * Grove: node-list-empty?
 * Port from: primitive.h PRIMITIVE(IsNodeListEmpty, "node-list-empty?", 1, 0, 0)
 */
const nodeListEmptyPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list-empty? requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-empty? requires a node-list argument');
  }

  return makeBoolean(nl.nodes.first() === null);
};

/**
 * Grove: node-list-first
 * Port from: primitive.h PRIMITIVE(NodeListFirst, "node-list-first", 0, 1, 0)
 * Port from: primitive.cxx line 3828-3836
 *
 * Returns a node-list (singleton or empty), NOT #f
 * OpenJade: return new (interp) NodePtrNodeListObj(nd);
 */
const nodeListFirstPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list-first requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-first requires a node-list argument');
  }

  // Port from: OpenJade returns NodePtrNodeListObj(nd) where nd might be null
  // If null, it's an empty node-list, not #f
  const first = nl.nodes.first();
  if (first) {
    // Return singleton node-list containing the first node
    return makeNodeList(nodeListFromArray([first]));
  } else {
    // Return empty node-list (NOT #f)
    return makeNodeList(EMPTY_NODE_LIST);
  }
};

/**
 * Grove: node-list-rest
 * Port from: primitive.h PRIMITIVE(NodeListRest, "node-list-rest", 0, 1, 0)
 */
const nodeListRestPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list-rest requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-rest requires a node-list argument');
  }

  const rest = nl.nodes.rest();
  return rest ? makeNodeList(rest) : makeNodeList(EMPTY_NODE_LIST);
};

/**
 * Grove: node-list-reverse
 * Port from: primitive.h PRIMITIVE(NodeListReverse, "node-list-reverse", 0, 1, 0)
 */
const nodeListReversePrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('node-list-reverse requires exactly 1 argument');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-reverse requires a node-list argument');
  }

  // Convert to array, reverse it, and convert back to node-list
  const nodesArray = nl.nodes.toArray();
  const reversed = nodesArray.reverse();
  return makeNodeList(nodeListFromArray(reversed));
};

/**
 * Grove: node-list
 * Port from: primitive.h PRIMITIVE(NodeList, "node-list", 0, 0, 1)
 *
 * Concatenates multiple node-lists into a single node-list.
 * With 0 arguments, returns empty node-list.
 * With 1+ arguments, concatenates them from left to right.
 */
const nodeListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeNodeList(EMPTY_NODE_LIST);
  }

  // Port from: OpenJade primitive.cxx NodeList - accepts both nodes and node-lists
  // Arguments can be either single nodes or node-lists
  const nodeLists: NodeList[] = [];
  for (let i = 0; i < args.length; i++) {
    const nl = args[i].asNodeList();
    if (nl) {
      // It's a node-list, use it directly
      nodeLists.push(nl.nodes);
    } else {
      // Try as a single node - wrap it in a singleton node-list
      const node = args[i].asNode();
      if (node) {
        nodeLists.push(nodeListFromArray([node.node]));
      } else {
        throw new Error(`node-list argument ${i} must be a node or node-list`);
      }
    }
  }

  // Concatenate all node-lists by converting to arrays and merging
  let combined: Node[] = [];
  for (const nl of nodeLists) {
    combined = combined.concat(nl.toArray());
  }

  return makeNodeList(nodeListFromArray(combined));
};

/**
 * Grove: select-elements
 * Port from: primitive.h PRIMITIVE(SelectElements, "select-elements", 0, 2, 0)
 *
 * Filters a node-list to include only elements matching the pattern.
 * For now, only supports simple element name (GI) patterns.
 * TODO: Full pattern support for attributes, etc.
 */
const selectElementsPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('select-elements requires exactly 2 arguments');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('select-elements requires a node-list as first argument');
  }

  // Pattern can be a string (element name) or symbol
  const pattern = args[1].asString() || args[1].asSymbol();
  if (!pattern) {
    throw new Error('select-elements requires a string or symbol as second argument (element name pattern)');
  }

  const elementName = args[1].asString()?.value || args[1].asSymbol()?.name || '';

  // Filter nodes by element name
  const nodesArray = nl.nodes.toArray();
  const filtered = nodesArray.filter((node: Node) => {
    // Only match element nodes with the specified GI
    return node.isElement() && node.gi() === elementName;
  });

  return makeNodeList(nodeListFromArray(filtered));
};

/**
 * Grove: node-list-map
 * Maps a function over a node-list, returning a new node-list
 * Port from: OpenJade extension (not in DSSSL standard)
 */
const nodeListMapPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('node-list-map requires exactly 2 arguments');
  }

  const func = args[0].asFunction();
  if (!func) {
    throw new Error('node-list-map requires a function as first argument');
  }

  const nl = args[1].asNodeList();
  if (!nl) {
    throw new Error('node-list-map requires a node-list as second argument');
  }

  // Map function over each node in the list
  const results: Node[] = [];
  let current = nl.nodes;
  while (true) {
    const first = current.first();
    if (!first) {
      break;
    }

    // Call function with node as argument
    const nodeObj = makeNode(first);
    const result = func.isPrimitive()
      ? func.callPrimitive([nodeObj], vm)
      : callClosure(func, [nodeObj], vm);

    // Result must be a node-list (OpenJade MapNodeListObj::mapNext behavior)
    // Port from: OpenJade checks mapped_ = ret->asNodeList() and errors if null
    const resultNodeList = result.asNodeList();
    if (!resultNodeList) {
      // Port from: OpenJade logs InterpreterMessages::returnNotNodeList and stops
      throw new Error('node-list-map: function must return a node-list, got ' + result.constructor.name);
    }

    // Add all nodes from the returned node-list (flattening)
    const nodeArray = resultNodeList.nodes.toArray();
    results.push(...nodeArray);

    const rest = current.rest();
    if (!rest) {
      break;
    }
    current = rest;
  }

  return makeNodeList(nodeListFromArray(results));
};

/**
 * Grove: node-list-union
 * Merges multiple node-lists, removing duplicates
 * Port from: OpenJade extension
 */
const nodeListUnionPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('node-list-union requires at least 2 arguments');
  }

  // Collect all nodes from all lists, avoiding duplicates
  const allNodes: Node[] = [];
  const seenNodes = new Set<Node>();

  for (const arg of args) {
    const nl = arg.asNodeList();
    if (!nl) {
      throw new Error('node-list-union requires node-list arguments');
    }

    // Add nodes from this list, skipping duplicates
    let current = nl.nodes;
    while (true) {
      const first = current.first();
      if (!first) break;

      // Only add if not seen before
      if (!seenNodes.has(first)) {
        seenNodes.add(first);
        allNodes.push(first);
      }

      const rest = current.rest();
      if (!rest) break;
      current = rest;
    }
  }

  return makeNodeList(nodeListFromArray(allNodes));
};

/**
 * Grove: node-list-some?
 * Tests if any node in a node-list satisfies a predicate
 * Port from: OpenJade extension (similar to Scheme's any)
 */
const nodeListSomePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('node-list-some? requires exactly 2 arguments');
  }

  const func = args[0].asFunction();
  if (!func) {
    throw new Error('node-list-some? requires a function as first argument');
  }

  const nl = args[1].asNodeList();
  if (!nl) {
    throw new Error('node-list-some? requires a node-list as second argument');
  }

  // Test each node with the predicate
  let current = nl.nodes;
  while (true) {
    const first = current.first();
    if (!first) {
      // No more nodes, none matched
      return theFalseObj;
    }

    // Call predicate with node
    const nodeObj = makeNode(first);
    const result = func.isPrimitive()
      ? func.callPrimitive([nodeObj], vm)
      : callClosure(func, [nodeObj], vm);

    // If result is true, return true immediately
    if (result !== theFalseObj) {
      return theTrueObj;
    }

    const rest = current.rest();
    if (!rest) {
      return theFalseObj;
    }
    current = rest;
  }
};

/**
 * Grove: node-list=?
 * Tests if two node-lists contain the same nodes in the same order
 * Port from: OpenJade extension
 */
const nodeListEqualPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('node-list=? requires exactly 2 arguments');
  }

  // Arguments can be nodes or node-lists
  const obj1 = args[0];
  const obj2 = args[1];

  // Convert to node-lists if needed
  let nl1: NodeList;
  let nl2: NodeList;

  const node1 = obj1.asNode();
  if (node1) {
    nl1 = nodeListFromArray([node1.node]);
  } else {
    const list1 = obj1.asNodeList();
    if (!list1) {
      throw new Error('node-list=? requires node or node-list as first argument');
    }
    nl1 = list1.nodes;
  }

  const node2 = obj2.asNode();
  if (node2) {
    nl2 = nodeListFromArray([node2.node]);
  } else {
    const list2 = obj2.asNodeList();
    if (!list2) {
      throw new Error('node-list=? requires node or node-list as second argument');
    }
    nl2 = list2.nodes;
  }

  // Compare node-lists element by element
  let current1 = nl1;
  let current2 = nl2;

  while (true) {
    const first1 = current1.first();
    const first2 = current2.first();

    // Both empty - equal
    if (!first1 && !first2) {
      return theTrueObj;
    }

    // One empty, one not - not equal
    if (!first1 || !first2) {
      return theFalseObj;
    }

    // Different nodes - not equal
    if (first1 !== first2) {
      return theFalseObj;
    }

    // Move to next elements
    const rest1 = current1.rest();
    const rest2 = current2.rest();
    if (!rest1 || !rest2) {
      // One has more elements - check if both ended
      return (!rest1 && !rest2) ? theTrueObj : theFalseObj;
    }
    current1 = rest1;
    current2 = rest2;
  }
};

/**
 * Grove: node-list-contains?
 * Helper primitive for checking if a node is in a node-list
 * Used in SchwebNet templates
 */
const nodeListContainsPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('node-list-contains? requires exactly 2 arguments');
  }

  const nl = args[0].asNodeList();
  if (!nl) {
    throw new Error('node-list-contains? requires a node-list as first argument');
  }

  // Second argument can be a node or node-list
  let searchNodeObj = args[1];
  let searchNode = searchNodeObj.asNode();
  if (!searchNode) {
    const searchNodeList = searchNodeObj.asNodeList();
    if (searchNodeList) {
      const first = searchNodeList.nodes.first();
      if (!first) {
        // Empty node-list - not found
        return theFalseObj;
      }
      searchNodeObj = makeNode(first);
      searchNode = searchNodeObj.asNode();
      if (!searchNode) {
        throw new Error('node-list-contains?: failed to extract node from node-list');
      }
    } else {
      throw new Error('node-list-contains? requires a node or node-list as second argument');
    }
  }

  // Search for the node in the list
  let current = nl.nodes;
  while (true) {
    const first = current.first();
    if (!first) {
      // End of list, not found
      return theFalseObj;
    }

    // Compare nodes - check if they're the same node
    // In grove model, nodes have identity - compare underlying native nodes
    if (first === searchNode.node) {
      return theTrueObj;
    }

    const rest = current.rest();
    if (!rest) {
      return theFalseObj;
    }
    current = rest;
  }
};

/**
 * Grove: attributes
 * Port from: primitive.h PRIMITIVE(Attributes, "attributes", 0, 1, 0)
 */
const attributesPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('attributes requires exactly 1 argument');
  }

  const node = args[0].asNode();
  if (!node) {
    throw new Error('attributes requires a node argument');
  }

  return makeNodeList(node.node.attributes());
};

/**
 * Grove: attribute-string
 * Port from: primitive.h PRIMITIVE(AttributeString, "attribute-string", 2, 0, 0)
 */
const attributeStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('attribute-string requires exactly 2 arguments');
  }

  const name = args[0].asString() || args[0].asSymbol();
  if (!name) {
    throw new Error('attribute-string requires string or symbol as first argument');
  }

  // Port from: primitive.cxx line 3009 - optSingletonNodeList
  const node = optSingletonNode(args[1]);
  if (!node) {
    // Empty node-list -> return #f (line 3012-3013)
    return theFalseObj;
  }

  const attrName = args[0].asString()?.value || args[0].asSymbol()?.name || '';
  const value = node.attributeString(attrName);

  // Port from: OpenJade returns StringObj even for empty string, only #f for missing attribute
  // IMPORTANT: Empty string "" is different from missing attribute (null)
  return value !== null ? makeString(value) : theFalseObj;
};

/**
 * Grove: ancestors
 * Port from: primitive.h PRIMITIVE(Ancestors, "ancestors", 0, 1, 0)
 */
const ancestorsPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('ancestors requires exactly 1 argument');
  }

  const node = args[0].asNode();
  if (!node) {
    throw new Error('ancestors requires a node argument');
  }

  return makeNodeList(node.node.ancestors());
};

/**
 * Grove: ancestor
 * Port from: primitive.h PRIMITIVE(Ancestor, "ancestor", 1, 1, 0)
 * Returns first ancestor with matching GI
 */
const ancestorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('ancestor requires exactly 2 arguments');
  }

  const giStr = args[0].asString();
  if (!giStr) {
    throw new Error('ancestor requires a string as first argument');
  }

  const node = args[1].asNode();
  if (!node) {
    throw new Error('ancestor requires a node as second argument');
  }

  const gi = giStr.value;
  let ancestorList = node.node.ancestors();

  // Search for first ancestor with matching GI
  while (true) {
    const first = ancestorList.first();
    if (!first) {
      // No more ancestors
      break;
    }

    if (first.gi() === gi) {
      return makeNode(first);
    }

    const rest = ancestorList.rest();
    if (!rest) {
      break;
    }
    ancestorList = rest;
  }

  // No matching ancestor found
  return makeNodeList(EMPTY_NODE_LIST);
};

/**
 * Grove: descendants
 * Port from: primitive.h PRIMITIVE(Descendants, "descendants", 0, 1, 0)
 */
const descendantsPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('descendants requires exactly 1 argument');
  }

  const node = args[0].asNode();
  if (!node) {
    throw new Error('descendants requires a node argument');
  }

  return makeNodeList(node.node.descendants());
};

/**
 * Grove: child-number
 * Port from: primitive.h PRIMITIVE(ChildNumber, "child-number", 0, 1, 0)
 * Port from: primitive.cxx ChildNumber - uses optSingletonNodeList
 */
const childNumberPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('child-number requires exactly 1 argument');
  }

  // Port from: OpenJade optSingletonNodeList - accepts node or singleton node-list
  let nodeObj = args[0].asNode();
  if (!nodeObj) {
    // Try as a singleton node-list
    const nodeList = args[0].asNodeList();
    if (nodeList) {
      const first = nodeList.nodes.first();
      const rest = nodeList.nodes.rest();

      // Must be singleton (exactly one element)
      if (first && (!rest || rest.length() === 0)) {
        nodeObj = makeNode(first).asNode();
      } else {
        throw new Error('child-number requires a singleton node-list (got empty or multi-element list)');
      }
    }

    if (!nodeObj) {
      throw new Error('child-number requires a node or singleton node-list argument');
    }
  }

  return makeNumber(nodeObj.node.childNumber());
};

/**
 * Grove: first-sibling?
 * Port from: DSSSL standard (§9.6.5)
 * Returns true if node is the first among its siblings
 */
const firstSiblingPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('first-sibling? requires exactly 1 argument');
  }

  const nodeObj = optSingletonNode(args[0]);
  if (!nodeObj) {
    throw new Error('first-sibling? requires a node or singleton node-list argument');
  }

  // Node is first sibling if child-number is 1
  const childNum = nodeObj.childNumber();
  return makeBoolean(childNum === 1);
};

/**
 * Grove: last-sibling?
 * Port from: DSSSL standard (§9.6.5)
 * Returns true if node is the last among its siblings
 */
const lastSiblingPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('last-sibling? requires exactly 1 argument');
  }

  const nodeObj = optSingletonNode(args[0]);
  if (!nodeObj) {
    throw new Error('last-sibling? requires a node or singleton node-list argument');
  }

  // Get parent and check if this is the last child
  const parent = nodeObj.parent();
  if (!parent) {
    // No parent - can't determine
    return theFalseObj;
  }

  const siblings = parent.children().toArray();
  if (siblings.length === 0) {
    return theFalseObj;
  }

  // Check if this node is the last sibling
  return makeBoolean(siblings[siblings.length - 1] === nodeObj);
};

// ============ Common List Accessors ============

/**
 * List accessor: caar
 * Port from: primitive.cxx Caar::primitiveCall
 */
const caarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caar requires exactly 1 argument');
  }

  const pair1 = args[0].asPair();
  if (!pair1) {
    throw new Error('caar requires a pair argument');
  }

  const pair2 = pair1.car.asPair();
  if (!pair2) {
    throw new Error('caar: car is not a pair');
  }

  return pair2.car;
};

/**
 * List accessor: cadr
 * Port from: primitive.cxx Cadr::primitiveCall
 */
const cadrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cadr requires exactly 1 argument');
  }

  const pair1 = args[0].asPair();
  if (!pair1) {
    throw new Error('cadr requires a pair argument');
  }

  const pair2 = pair1.cdr.asPair();
  if (!pair2) {
    throw new Error('cadr: cdr is not a pair');
  }

  return pair2.car;
};

/**
 * List accessor: cdar
 * Port from: primitive.cxx Cdar::primitiveCall
 */
const cdarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdar requires exactly 1 argument');
  }

  const pair1 = args[0].asPair();
  if (!pair1) {
    throw new Error('cdar requires a pair argument');
  }

  const pair2 = pair1.car.asPair();
  if (!pair2) {
    throw new Error('cdar: car is not a pair');
  }

  return pair2.cdr;
};

/**
 * List accessor: cddr
 * Port from: primitive.cxx Cddr::primitiveCall
 */
const cddrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cddr requires exactly 1 argument');
  }

  const pair1 = args[0].asPair();
  if (!pair1) {
    throw new Error('cddr requires a pair argument');
  }

  const pair2 = pair1.cdr.asPair();
  if (!pair2) {
    throw new Error('cddr: cdr is not a pair');
  }

  return pair2.cdr;
};

// ============ Extended List Accessors (c[ad]{3}r) ============

/**
 * List accessor: caaar
 * Port from: primitive.cxx Caaar::primitiveCall
 */
const caaarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caaar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caaar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('caaar: car is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('caaar: caar is not a pair');
  return p3.car;
};

/**
 * List accessor: caadr
 * Port from: primitive.cxx Caadr::primitiveCall
 */
const caadrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caadr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caadr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('caadr: cdr is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('caadr: cadr is not a pair');
  return p3.car;
};

/**
 * List accessor: cadar
 * Port from: primitive.cxx Cadar::primitiveCall
 */
const cadarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cadar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cadar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('cadar: car is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cadar: cdar is not a pair');
  return p3.car;
};

/**
 * List accessor: caddr
 * Port from: primitive.cxx Caddr::primitiveCall
 */
const caddrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caddr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caddr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('caddr: cdr is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('caddr: cddr is not a pair');
  return p3.car;
};

/**
 * List accessor: cdaar
 * Port from: primitive.cxx Cdaar::primitiveCall
 */
const cdaarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdaar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cdaar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('cdaar: car is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('cdaar: caar is not a pair');
  return p3.cdr;
};

/**
 * List accessor: cdadr
 * Port from: primitive.cxx Cdadr::primitiveCall
 */
const cdadrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdadr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cdadr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('cdadr: cdr is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('cdadr: cadr is not a pair');
  return p3.cdr;
};

/**
 * List accessor: cddar
 * Port from: primitive.cxx Cddar::primitiveCall
 */
const cddarPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cddar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cddar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('cddar: car is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cddar: cdar is not a pair');
  return p3.cdr;
};

/**
 * List accessor: cdddr
 * Port from: primitive.cxx Cdddr::primitiveCall
 */
const cdddrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdddr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cdddr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('cdddr: cdr is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cdddr: cddr is not a pair');
  return p3.cdr;
};

// ============ Extended List Accessors (c[ad]{4}r) ============

/**
 * List accessor: caaaar
 * Port from: primitive.cxx Caaaar::primitiveCall
 */
const caaaaRPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caaaar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caaaar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('caaaar: car is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('caaaar: caar is not a pair');
  const p4 = p3.car.asPair();
  if (!p4) throw new Error('caaaar: caaar is not a pair');
  return p4.car;
};

/**
 * List accessor: caaadr
 * Port from: primitive.cxx Caaadr::primitiveCall
 */
const caaaDrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caaadr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caaadr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('caaadr: cdr is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('caaadr: cadr is not a pair');
  const p4 = p3.car.asPair();
  if (!p4) throw new Error('caaadr: caadr is not a pair');
  return p4.car;
};

/**
 * List accessor: caaddr
 * Port from: primitive.cxx Caaddr::primitiveCall
 */
const caadDrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('caaddr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('caaddr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('caaddr: cdr is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('caaddr: cddr is not a pair');
  const p4 = p3.car.asPair();
  if (!p4) throw new Error('caaddr: caddr is not a pair');
  return p4.car;
};

/**
 * List accessor: cadddr
 * Port from: primitive.cxx Cadddr::primitiveCall
 */
const caddDrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cadddr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cadddr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('cadddr: cdr is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cadddr: cddr is not a pair');
  const p4 = p3.cdr.asPair();
  if (!p4) throw new Error('cadddr: cdddr is not a pair');
  return p4.car;
};

/**
 * List accessor: cdaaar
 * Port from: primitive.cxx Cdaaar::primitiveCall
 */
const cdaaArPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdaaar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cdaaar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('cdaaar: car is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('cdaaar: caar is not a pair');
  const p4 = p3.car.asPair();
  if (!p4) throw new Error('cdaaar: caaar is not a pair');
  return p4.cdr;
};

/**
 * List accessor: cdaadr
 * Port from: primitive.cxx Cdaadr::primitiveCall
 */
const cdaaDrPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cdaadr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cdaadr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('cdaadr: cdr is not a pair');
  const p3 = p2.car.asPair();
  if (!p3) throw new Error('cdaadr: cadr is not a pair');
  const p4 = p3.car.asPair();
  if (!p4) throw new Error('cdaadr: caadr is not a pair');
  return p4.cdr;
};

/**
 * List accessor: cddaar
 * Port from: primitive.cxx Cddaar::primitiveCall
 */
const cddaArPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cddaar requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cddaar: not a pair');
  const p2 = p1.car.asPair();
  if (!p2) throw new Error('cddaar: car is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cddaar: cdar is not a pair');
  const p4 = p3.cdr.asPair();
  if (!p4) throw new Error('cddaar: cddar is not a pair');
  return p4.car;
};

/**
 * List accessor: cddddr
 * Port from: primitive.cxx Cddddr::primitiveCall
 */
const cddddRPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('cddddr requires exactly 1 argument');
  }

  const p1 = args[0].asPair();
  if (!p1) throw new Error('cddddr: not a pair');
  const p2 = p1.cdr.asPair();
  if (!p2) throw new Error('cddddr: cdr is not a pair');
  const p3 = p2.cdr.asPair();
  if (!p3) throw new Error('cddddr: cddr is not a pair');
  const p4 = p3.cdr.asPair();
  if (!p4) throw new Error('cddddr: cdddr is not a pair');
  return p4.cdr;
};

// ============ String/List Conversions ============

/**
 * String to list conversion: string->list
 * Port from: primitive.cxx StringToList::primitiveCall
 */
const stringToListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string->list requires exactly 1 argument');
  }

  // Port from: OpenJade - handle #f gracefully (from attribute-string returning #f)
  const str = args[0].asString();
  if (!str) {
    // Return empty list for #f (similar to how string=? returns #f for non-strings)
    return theNilObj;
  }

  let result: ELObj = theNilObj;
  for (let i = str.value.length - 1; i >= 0; i--) {
    const char = makeChar(str.value[i]);
    result = makePair(char, result);
  }

  return result;
};

/**
 * List to string conversion: list->string
 * Port from: primitive.cxx ListToString::primitiveCall
 */
const listToStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('list->string requires exactly 1 argument');
  }

  let list = args[0];
  let result = '';

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list->string: argument must be a proper list');
    }

    const ch = pair.car.asChar();
    if (!ch) {
      throw new Error('list->string: list elements must be characters');
    }

    result += ch.value;
    list = pair.cdr;
  }

  return makeString(result);
};

// ============ More String Operations ============

/**
 * String constructor: string
 * Port from: primitive.cxx String::primitiveCall
 */
const stringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  let result = '';
  for (const arg of args) {
    const ch = arg.asChar();
    if (!ch) {
      throw new Error('string requires character arguments');
    }
    result += ch.value;
  }
  return makeString(result);
};

/**
 * String constructor: make-string
 * Port from: primitive.cxx MakeString::primitiveCall
 */
const makeStringPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 1 || args.length > 2) {
    throw new Error('make-string requires 1 or 2 arguments');
  }

  const size = args[0].asNumber();
  if (!size) {
    throw new Error('make-string: size must be a number');
  }

  if (size.value < 0 || !Number.isInteger(size.value)) {
    throw new Error('make-string: size must be a non-negative integer');
  }

  const fillChar = args.length === 2 ? args[1].asChar() : null;
  if (args.length === 2 && !fillChar) {
    throw new Error('make-string: fill must be a character');
  }

  const ch = fillChar ? fillChar.value : ' ';
  return makeString(ch.repeat(Math.floor(size.value)));
};

/**
 * String mutator: string-set!
 * Port from: primitive.cxx StringSet::primitiveCall
 */
const stringSetPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 3) {
    throw new Error('string-set! requires exactly 3 arguments');
  }

  const str = args[0].asString();
  const idx = args[1].asNumber();
  const ch = args[2].asChar();
  if (!str || !idx || !ch) {
    throw new Error('string-set! requires string, number, and character arguments');
  }

  if (idx.value < 0 || idx.value >= str.value.length) {
    throw new Error(`string-set!: index ${idx.value} out of bounds for string of length ${str.value.length}`);
  }

  const index = Math.floor(idx.value);
  // JavaScript strings are immutable, but we can modify the StringObj's value
  str.value = str.value.substring(0, index) + ch.value + str.value.substring(index + 1);
  return theNilObj; // R4RS: unspecified return value
};

/**
 * String mutator: string-fill!
 * Port from: primitive.cxx StringFill::primitiveCall
 */
const stringFillPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 2) {
    throw new Error('string-fill! requires exactly 2 arguments');
  }

  const str = args[0].asString();
  const ch = args[1].asChar();
  if (!str || !ch) {
    throw new Error('string-fill! requires string and character arguments');
  }

  str.value = ch.value.repeat(str.value.length);
  return theNilObj; // R4RS: unspecified return value
};

/**
 * String utility: string-copy
 * Port from: primitive.cxx StringCopy::primitiveCall
 */
const stringCopyPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('string-copy requires exactly 1 argument');
  }

  const str = args[0].asString();
  if (!str) {
    throw new Error('string-copy requires a string argument');
  }

  return makeString(str.value);
};

// ============ More String Comparisons ============

/**
 * String comparison: string>?
 * Port from: primitive.cxx StringGreater::primitiveCall
 */
const stringGreaterPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string>? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string>? requires string arguments');
    }
    if (str1.value <= str2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string<=?
 * Port from: primitive.cxx StringLessOrEqual::primitiveCall
 */
const stringLessOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string<=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string<=? requires string arguments');
    }
    if (str1.value > str2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string>=?
 * Port from: primitive.cxx StringGreaterOrEqual::primitiveCall
 */
const stringGreaterOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string>=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string>=? requires string arguments');
    }
    if (str1.value < str2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string-ci=? (case-insensitive)
 * Port from: primitive.cxx StringCiEqual::primitiveCall
 */
const stringCiEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string-ci=? requires at least 2 arguments');
  }

  const first = args[0].asString();
  if (!first) {
    throw new Error('string-ci=? requires string arguments');
  }

  const firstLower = first.value.toLowerCase();
  for (let i = 1; i < args.length; i++) {
    const str = args[i].asString();
    if (!str) {
      throw new Error('string-ci=? requires string arguments');
    }
    if (str.value.toLowerCase() !== firstLower) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string-ci<? (case-insensitive)
 * Port from: primitive.cxx StringCiLess::primitiveCall
 */
const stringCiLessPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string-ci<? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string-ci<? requires string arguments');
    }
    if (str1.value.toLowerCase() >= str2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string-ci>? (case-insensitive)
 * Port from: primitive.cxx StringCiGreater::primitiveCall
 */
const stringCiGreaterPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string-ci>? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string-ci>? requires string arguments');
    }
    if (str1.value.toLowerCase() <= str2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string-ci<=? (case-insensitive)
 * Port from: primitive.cxx StringCiLessOrEqual::primitiveCall
 */
const stringCiLessOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string-ci<=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string-ci<=? requires string arguments');
    }
    if (str1.value.toLowerCase() > str2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * String comparison: string-ci>=? (case-insensitive)
 * Port from: primitive.cxx StringCiGreaterOrEqual::primitiveCall
 */
const stringCiGreaterOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('string-ci>=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const str1 = args[i].asString();
    const str2 = args[i + 1].asString();
    if (!str1 || !str2) {
      throw new Error('string-ci>=? requires string arguments');
    }
    if (str1.value.toLowerCase() < str2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

// ============ More Character Comparisons ============

/**
 * Character comparison: char>?
 * Port from: primitive.cxx CharGreater::primitiveCall
 */
const charGreaterPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char>? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char>? requires character arguments');
    }
    if (ch1.value <= ch2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char<=?
 * Port from: primitive.cxx CharLessOrEqual::primitiveCall
 */
const charLessOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char<=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char<=? requires character arguments');
    }
    if (ch1.value > ch2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char>=?
 * Port from: primitive.cxx CharGreaterOrEqual::primitiveCall
 */
const charGreaterOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char>=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char>=? requires character arguments');
    }
    if (ch1.value < ch2.value) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char-ci=? (case-insensitive)
 * Port from: primitive.cxx CharCiEqual::primitiveCall
 */
const charCiEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char-ci=? requires at least 2 arguments');
  }

  const first = args[0].asChar();
  if (!first) {
    throw new Error('char-ci=? requires character arguments');
  }

  const firstLower = first.value.toLowerCase();
  for (let i = 1; i < args.length; i++) {
    const ch = args[i].asChar();
    if (!ch) {
      throw new Error('char-ci=? requires character arguments');
    }
    if (ch.value.toLowerCase() !== firstLower) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char-ci<? (case-insensitive)
 * Port from: primitive.cxx CharCiLess::primitiveCall
 */
const charCiLessPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char-ci<? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char-ci<? requires character arguments');
    }
    if (ch1.value.toLowerCase() >= ch2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char-ci>? (case-insensitive)
 * Port from: primitive.cxx CharCiGreater::primitiveCall
 */
const charCiGreaterPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char-ci>? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char-ci>? requires character arguments');
    }
    if (ch1.value.toLowerCase() <= ch2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char-ci<=? (case-insensitive)
 * Port from: primitive.cxx CharCiLessOrEqual::primitiveCall
 */
const charCiLessOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char-ci<=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char-ci<=? requires character arguments');
    }
    if (ch1.value.toLowerCase() > ch2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

/**
 * Character comparison: char-ci>=? (case-insensitive)
 * Port from: primitive.cxx CharCiGreaterOrEqual::primitiveCall
 */
const charCiGreaterOrEqualPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length < 2) {
    throw new Error('char-ci>=? requires at least 2 arguments');
  }

  for (let i = 0; i < args.length - 1; i++) {
    const ch1 = args[i].asChar();
    const ch2 = args[i + 1].asChar();
    if (!ch1 || !ch2) {
      throw new Error('char-ci>=? requires character arguments');
    }
    if (ch1.value.toLowerCase() < ch2.value.toLowerCase()) {
      return theFalseObj;
    }
  }

  return theTrueObj;
};

// ============ Character Predicates ============

/**
 * Character predicate: char-alphabetic?
 * Port from: primitive.cxx CharAlphabetic::primitiveCall
 */
const charAlphabeticPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-alphabetic? requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-alphabetic? requires a character argument');
  }

  return makeBoolean(/[a-zA-Z]/.test(ch.value));
};

/**
 * Character predicate: char-numeric?
 * Port from: primitive.cxx CharNumeric::primitiveCall
 */
const charNumericPredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-numeric? requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-numeric? requires a character argument');
  }

  return makeBoolean(/[0-9]/.test(ch.value));
};

/**
 * Character predicate: char-whitespace?
 * Port from: primitive.cxx CharWhitespace::primitiveCall
 */
const charWhitespacePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-whitespace? requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-whitespace? requires a character argument');
  }

  return makeBoolean(/\s/.test(ch.value));
};

/**
 * Character predicate: char-upper-case?
 * Port from: primitive.cxx CharUpperCase::primitiveCall
 */
const charUpperCasePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-upper-case? requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-upper-case? requires a character argument');
  }

  return makeBoolean(/[A-Z]/.test(ch.value));
};

/**
 * Character predicate: char-lower-case?
 * Port from: primitive.cxx CharLowerCase::primitiveCall
 */
const charLowerCasePredicate: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('char-lower-case? requires exactly 1 argument');
  }

  const ch = args[0].asChar();
  if (!ch) {
    throw new Error('char-lower-case? requires a character argument');
  }

  return makeBoolean(/[a-z]/.test(ch.value));
};

// ============ Vector/List Conversions ============

/**
 * Conversion: vector->list
 * Port from: primitive.cxx VectorToList::primitiveCall
 */
const vectorToListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('vector->list requires exactly 1 argument');
  }

  const vec = args[0].asVector();
  if (!vec) {
    throw new Error('vector->list requires a vector argument');
  }

  let result: ELObj = theNilObj;
  for (let i = vec.elements.length - 1; i >= 0; i--) {
    result = makePair(vec.elements[i], result);
  }

  return result;
};

/**
 * Conversion: list->vector
 * Port from: primitive.cxx ListToVector::primitiveCall
 */
const listToVectorPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('list->vector requires exactly 1 argument');
  }

  let list = args[0];
  const elements: ELObj[] = [];

  while (!list.asNil()) {
    const pair = list.asPair();
    if (!pair) {
      throw new Error('list->vector requires a proper list');
    }
    elements.push(pair.car);
    list = pair.cdr;
  }

  return makeVector(elements);
};

/**
 * ========================================================================
 * DSSSL Processing Primitives (§10)
 * Port from: OpenJade style/primitive.cxx
 * ========================================================================
 */

/**
 * current-node - Returns the current node being processed
 * Port from: primitive.cxx CurrentNode::primitiveCall
 */
const currentNodePrimitive: PrimitiveFunction = (_args: ELObj[], vm: VM): ELObj => {
  if (!vm.currentNode) {
    throw new Error('current-node: no current node in processing context');
  }
  return makeNode(vm.currentNode);
};

/**
 * empty-sosofo - Returns an empty sosofo
 * Port from: primitive.cxx EmptySosofo::primitiveCall
 */
const emptySosofoPrimitive: PrimitiveFunction = (_args: ELObj[], vm: VM): ELObj => {
  return makeSosofo('empty');
};

/**
 * literal - Creates a literal text sosofo
 * Port from: primitive.cxx Literal::primitiveCall
 *
 * Takes 0+ string arguments and concatenates them into literal text.
 * With 0 args: returns empty sosofo
 * With 1+ args: concatenates strings and returns literal sosofo
 */
const literalPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    return makeSosofo('empty');
  }

  // Concatenate all string arguments
  let text = '';
  for (let i = 0; i < args.length; i++) {
    const str = args[i].asString();
    if (!str) {
      throw new Error(`literal: argument ${i + 1} must be a string`);
    }
    text += str.value;
  }

  return makeSosofo('literal', text);
};

/**
 * sosofo-append - Appends sosofos into a sequence
 * Port from: primitive.cxx SosofoAppend::primitiveCall
 */
const sosofoAppendPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  const sosofos: SosofoObj[] = [];

  for (const arg of args) {
    const sosofo = arg.asSosofo();
    if (!sosofo) {
      throw new Error('sosofo-append: all arguments must be sosofos');
    }
    if (!sosofo.isEmpty()) {
      sosofos.push(sosofo);
    }
  }

  if (sosofos.length === 0) {
    return makeSosofo('empty');
  }
  if (sosofos.length === 1) {
    return sosofos[0];
  }

  return makeSosofo('append', sosofos);
};

/**
 * process-root - Start DSSSL processing from the document root
 * Port from: primitive.cxx ProcessRoot::primitiveCall
 *
 * Finds and executes the "root" construction rule.
 */
const processRootPrimitive: PrimitiveFunction = (_args: ELObj[], vm: VM): ELObj => {
  if (!vm.grove) {
    throw new Error('process-root: no grove in processing context');
  }
  if (!vm.globals) {
    throw new Error('process-root: no global environment');
  }

  // Find the root rule (lazy compilation with globals)
  const mode = vm.processingMode || '';
  const rule = vm.globals.ruleRegistry.findRule('root', mode, vm.globals);

  // Save current node and set to root
  const savedNode = vm.currentNode;
  const rootNode = vm.grove.root();
  vm.currentNode = rootNode;

  try {
    if (rule) {
      // Explicit root rule exists - use it
      const func = rule.func!.asFunction();
      if (!func || !func.isClosure()) {
        throw new Error('process-root: root rule must be a function');
      }

      // Call the rule with no arguments
      const result = callClosure(func, [], vm);

      // Result should be a sosofo
      const sosofo = result.asSosofo();
      if (!sosofo) {
        throw new Error('process-root: root rule must return a sosofo');
      }

      return result;
    } else {
      // No explicit root rule - default to processing document element
      // Port from: OpenJade default behavior when no root rule exists
      const docElement = rootNode.gi();
      if (docElement) {
        const elementRule = vm.globals.ruleRegistry.findRule(docElement, mode, vm.globals);
        if (elementRule) {
          const func = elementRule.func!.asFunction();
          if (!func || !func.isClosure()) {
            throw new Error(`process-root: rule for '${docElement}' must be a function`);
          }

          const result = callClosure(func, [], vm);
          return result;
        }
      }

      // No rule found - return empty sosofo
      return makeSosofo('empty');
    }
  } finally {
    // Restore current node
    vm.currentNode = savedNode;
  }
};

/**
 * process-children - Process the children of the current node
 * Port from: primitive.cxx ProcessChildren::primitiveCall
 *
 * Optionally takes a mode name to process in a different mode.
 */
const processChildrenPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (!vm.currentNode) {
    throw new Error('process-children: no current node in processing context');
  }

  // Optional mode argument
  let mode = vm.processingMode || '';
  if (args.length > 0) {
    const modeArg = args[0].asSymbol();
    if (!modeArg) {
      throw new Error('process-children: mode argument must be a symbol');
    }
    mode = modeArg.name;
  }

  // Save current mode and switch to new mode
  const savedMode = vm.processingMode;
  vm.processingMode = mode;

  try {
    const children = vm.currentNode.children();
    return processNodeListHelper(children, vm);
  } finally {
    // Restore mode
    vm.processingMode = savedMode;
  }
};

/**
 * process-node-list - Process a node list
 * Port from: primitive.cxx ProcessNodeList::primitiveCall
 */
const processNodeListPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length !== 1) {
    throw new Error('process-node-list requires exactly 1 argument');
  }

  const nodeListObj = args[0].asNodeList();
  if (!nodeListObj) {
    throw new Error('process-node-list requires a node-list argument');
  }

  return processNodeListHelper(nodeListObj.nodes, vm);
};

/**
 * next-match - Continue to next matching rule
 * Port from: primitive.cxx NextMatch::primitiveCall
 *
 * In DSSSL, next-match allows a rule to explicitly call the next rule
 * that would match the current node (typically with lower priority).
 *
 * For now, this is a stub that returns empty-sosofo.
 * Full implementation would require rule priorities and a rule stack.
 */
const nextMatchPrimitive: PrimitiveFunction = (_args: ELObj[], vm: VM): ELObj => {
  // TODO: Implement full next-match with priorities
  // For now, just return empty sosofo
  return makeSosofo('empty');
};

/**
 * Helper: Process a node list and return a sosofo
 * Port from: Interpreter.cxx Interpreter::processNodeList()
 *
 * Iterates through nodes, finds matching rules, executes them, and combines results.
 */
function processNodeListHelper(nodeList: NodeList, vm: VM): ELObj {
  if (!vm.globals) {
    throw new Error('processNodeList: no global environment');
  }

  const sosofos: ELObj[] = [];
  const mode = vm.processingMode || '';

  // Iterate through node list
  let current = nodeList.first();
  while (current) {
    // Find construction rule for this element
    const gi = current.gi();
    if (gi) {
      const rule = vm.globals.ruleRegistry.findRule(gi, mode, vm.globals);

      if (rule) {
        // Save current node
        const savedNode = vm.currentNode;
        vm.currentNode = current;

        try {
          // Execute the rule's lambda
          const func = rule.func!.asFunction();
          if (!func || !func.isClosure()) {
            throw new Error(`processNodeList: rule for '${gi}' must be a function`);
          }

          // Call the rule with no arguments
          const result = callClosure(func, [], vm);

          // Result should be a sosofo
          const sosofo = result.asSosofo();
          if (!sosofo) {
            throw new Error(`processNodeList: rule for '${gi}' must return a sosofo`);
          }

          sosofos.push(result);
        } finally {
          // Restore current node
          vm.currentNode = savedNode;
        }
      }
      // If no rule found, skip this node (default behavior)
    }

    // Move to next node
    const rest = nodeList.rest();
    if (!rest) break;
    current = rest.first();
    nodeList = rest;
  }

  // Combine all sosofos
  if (sosofos.length === 0) {
    return makeSosofo('empty');
  } else if (sosofos.length === 1) {
    return sosofos[0];
  } else {
    return makeSosofo('append', sosofos);
  }
}

/**
 * make-flow-object - Create a DSSSL flow object
 * Port from: primitive.cxx MakeFlowObject::primitiveCall
 *
 * Syntax: (make type keyword: value ... content)
 * Examples:
 *   (make entity system-id: "file.txt")
 *   (make formatting-instruction data: "text")
 */
const makeFlowObjectPrimitive: PrimitiveFunction = (args: ELObj[], vm: VM): ELObj => {
  if (args.length === 0) {
    throw new Error('make requires at least a flow object type');
  }

  // First argument is the flow object type
  const typeSym = args[0].asSymbol();
  if (!typeSym) {
    throw new Error('make requires a symbol as first argument (flow object type)');
  }

  const flowObjectType = typeSym.name;

  // Parse keyword arguments
  const characteristics: Record<string, unknown> = {};
  let contentStart = 1;

  // Scan for keyword: value pairs
  for (let i = 1; i < args.length; i++) {
    const keyword = args[i].asKeyword();
    if (keyword) {
      // Next argument is the value
      if (i + 1 >= args.length) {
        throw new Error(`make: keyword ${keyword.name}: requires a value`);
      }
      i++;
      const value = args[i];

      // Store characteristic (for now, store the raw value)
      characteristics[keyword.name] = value;
      contentStart = i + 1;
    } else {
      // No more keywords, rest is content
      break;
    }
  }

  // Content arguments (if any)
  const content = args.slice(contentStart);

  // Create flow object based on type
  switch (flowObjectType) {
    case 'entity': {
      // (make entity system-id: "file.txt" content...)
      const systemIdValue = characteristics['system-id'];
      if (!systemIdValue) {
        throw new Error('make entity requires system-id: characteristic');
      }
      const systemId = (systemIdValue as ELObj).asString();
      if (!systemId) {
        throw new Error('make entity system-id: must be a string');
      }

      return makeSosofo('entity', {
        systemId: systemId.value,
        content,
      });
    }

    case 'formatting-instruction': {
      // (make formatting-instruction data: "text")
      const dataValue = characteristics['data'];
      if (!dataValue) {
        throw new Error('make formatting-instruction requires data: characteristic');
      }
      const data = (dataValue as ELObj).asString();
      if (!data) {
        throw new Error('make formatting-instruction data: must be a string');
      }

      return makeSosofo('formatting-instruction', {
        data: data.value,
      });
    }

    default:
      // For now, unsupported flow object types return empty sosofo
      // TODO: Implement other flow object types (paragraph, sequence, etc.)
      return makeSosofo('empty');
  }
};

/**
 * Standard primitive registry
 * Maps primitive names to function objects
 */
export const standardPrimitives: Record<string, FunctionObj> = {
  // Arithmetic - basic
  '+': new FunctionObj('+', plusPrimitive),
  '-': new FunctionObj('-', minusPrimitive),
  '*': new FunctionObj('*', timesPrimitive),
  '/': new FunctionObj('/', dividePrimitive),

  // Arithmetic - advanced
  'abs': new FunctionObj('abs', absPrimitive),
  'quotient': new FunctionObj('quotient', quotientPrimitive),
  'remainder': new FunctionObj('remainder', remainderPrimitive),
  'modulo': new FunctionObj('modulo', moduloPrimitive),
  'floor': new FunctionObj('floor', floorPrimitive),
  'ceiling': new FunctionObj('ceiling', ceilingPrimitive),
  'truncate': new FunctionObj('truncate', truncatePrimitive),
  'round': new FunctionObj('round', roundPrimitive),
  'sqrt': new FunctionObj('sqrt', sqrtPrimitive),
  'square': new FunctionObj('square', squarePrimitive),
  'expt': new FunctionObj('expt', exptPrimitive),
  'min': new FunctionObj('min', minPrimitive),
  'max': new FunctionObj('max', maxPrimitive),
  'gcd': new FunctionObj('gcd', gcdPrimitive),
  'lcm': new FunctionObj('lcm', lcmPrimitive),
  'numerator': new FunctionObj('numerator', numeratorPrimitive),
  'denominator': new FunctionObj('denominator', denominatorPrimitive),
  'rationalize': new FunctionObj('rationalize', rationalizePrimitive),

  // Arithmetic - math functions
  'exp': new FunctionObj('exp', expPrimitive),
  'log': new FunctionObj('log', logPrimitive),
  'sin': new FunctionObj('sin', sinPrimitive),
  'cos': new FunctionObj('cos', cosPrimitive),
  'tan': new FunctionObj('tan', tanPrimitive),
  'asin': new FunctionObj('asin', asinPrimitive),
  'acos': new FunctionObj('acos', acosPrimitive),
  'atan': new FunctionObj('atan', atanPrimitive),
  'quotient+remainder': new FunctionObj('quotient+remainder', quotientRemainderPrimitive),

  // Bitwise operations
  'bitwise-and': new FunctionObj('bitwise-and', bitwiseAndPrimitive),
  'bitwise-ior': new FunctionObj('bitwise-ior', bitwiseIorPrimitive),
  'bitwise-xor': new FunctionObj('bitwise-xor', bitwiseXorPrimitive),
  'bitwise-not': new FunctionObj('bitwise-not', bitwiseNotPrimitive),
  'arithmetic-shift': new FunctionObj('arithmetic-shift', arithmeticShiftPrimitive),

  // Numeric predicates
  'integer?': new FunctionObj('integer?', integerPredicate),
  'exact?': new FunctionObj('exact?', exactPredicate),
  'inexact?': new FunctionObj('inexact?', inexactPredicate),
  'zero?': new FunctionObj('zero?', zeroPredicate),
  'positive?': new FunctionObj('positive?', positivePredicate),
  'negative?': new FunctionObj('negative?', negativePredicate),
  'odd?': new FunctionObj('odd?', oddPredicate),
  'even?': new FunctionObj('even?', evenPredicate),
  'finite?': new FunctionObj('finite?', finitePredicate),
  'infinite?': new FunctionObj('infinite?', infinitePredicate),
  'nan?': new FunctionObj('nan?', nanPredicate),

  // Comparison
  '<': new FunctionObj('<', lessThanPrimitive),
  '>': new FunctionObj('>', greaterThanPrimitive),
  '<=': new FunctionObj('<=', lessThanOrEqualPrimitive),
  '>=': new FunctionObj('>=', greaterThanOrEqualPrimitive),
  '=': new FunctionObj('=', numericEqualPrimitive),

  // Equality
  'eq?': new FunctionObj('eq?', eqPrimitive),
  'eqv?': new FunctionObj('eqv?', eqvPrimitive),
  'equal?': new FunctionObj('equal?', equalPrimitive),
  'boolean=?': new FunctionObj('boolean=?', booleanEqualPrimitive),
  'symbol=?': new FunctionObj('symbol=?', symbolEqualPrimitive),

  // List operations - basic
  'cons': new FunctionObj('cons', consPrimitive),
  'car': new FunctionObj('car', carPrimitive),
  'cdr': new FunctionObj('cdr', cdrPrimitive),
  'list': new FunctionObj('list', listPrimitive),
  'length': new FunctionObj('length', lengthPrimitive),
  'append': new FunctionObj('append', appendPrimitive),
  'reverse': new FunctionObj('reverse', reversePrimitive),
  'list-ref': new FunctionObj('list-ref', listRefPrimitive),
  'list-tail': new FunctionObj('list-tail', listTailPrimitive),

  // List operations - accessors (c[ad]{2}r)
  'caar': new FunctionObj('caar', caarPrimitive),
  'cadr': new FunctionObj('cadr', cadrPrimitive),
  'cdar': new FunctionObj('cdar', cdarPrimitive),
  'cddr': new FunctionObj('cddr', cddrPrimitive),

  // List operations - extended accessors (c[ad]{3}r)
  'caaar': new FunctionObj('caaar', caaarPrimitive),
  'caadr': new FunctionObj('caadr', caadrPrimitive),
  'cadar': new FunctionObj('cadar', cadarPrimitive),
  'caddr': new FunctionObj('caddr', caddrPrimitive),
  'cdaar': new FunctionObj('cdaar', cdaarPrimitive),
  'cdadr': new FunctionObj('cdadr', cdadrPrimitive),
  'cddar': new FunctionObj('cddar', cddarPrimitive),
  'cdddr': new FunctionObj('cdddr', cdddrPrimitive),

  // List operations - extended accessors (c[ad]{4}r)
  'caaaar': new FunctionObj('caaaar', caaaaRPrimitive),
  'caaadr': new FunctionObj('caaadr', caaaDrPrimitive),
  'caaddr': new FunctionObj('caaddr', caadDrPrimitive),
  'cadddr': new FunctionObj('cadddr', caddDrPrimitive),
  'cdaaar': new FunctionObj('cdaaar', cdaaArPrimitive),
  'cdaadr': new FunctionObj('cdaadr', cdaaDrPrimitive),
  'cddaar': new FunctionObj('cddaar', cddaArPrimitive),
  'cddddr': new FunctionObj('cddddr', cddddRPrimitive),

  // List operations - search
  'member': new FunctionObj('member', memberPrimitive),
  'memq': new FunctionObj('memq', memqPrimitive),
  'memv': new FunctionObj('memv', memvPrimitive),
  'assoc': new FunctionObj('assoc', assocPrimitive),
  'assq': new FunctionObj('assq', assqPrimitive),
  'assv': new FunctionObj('assv', assvPrimitive),

  // List operations - higher-order
  'map': new FunctionObj('map', mapPrimitive),
  'for-each': new FunctionObj('for-each', forEachPrimitive),
  'apply': new FunctionObj('apply', applyPrimitive),

  // List operations - utilities
  'last': new FunctionObj('last', lastPrimitive),
  'butlast': new FunctionObj('butlast', butlastPrimitive),
  'nthcdr': new FunctionObj('nthcdr', nthcdrPrimitive),
  'reverse!': new FunctionObj('reverse!', reverseInPlacePrimitive),
  'list-copy': new FunctionObj('list-copy', listCopyPrimitive),
  'make-list': new FunctionObj('make-list', makeListPrimitive),
  'iota': new FunctionObj('iota', iotaPrimitive),

  // List operations - filtering and folding
  'filter': new FunctionObj('filter', filterPrimitive),
  'remove': new FunctionObj('remove', removePrimitive),
  'fold-left': new FunctionObj('fold-left', foldLeftPrimitive),
  'fold-right': new FunctionObj('fold-right', foldRightPrimitive),

  // List operations - slicing
  'take': new FunctionObj('take', takePrimitive),
  'drop': new FunctionObj('drop', dropPrimitive),
  'split-at': new FunctionObj('split-at', splitAtPrimitive),
  'take-while': new FunctionObj('take-while', takeWhilePrimitive),
  'drop-while': new FunctionObj('drop-while', dropWhilePrimitive),
  'take-right': new FunctionObj('take-right', takeRightPrimitive),
  'drop-right': new FunctionObj('drop-right', dropRightPrimitive),
  'list-index': new FunctionObj('list-index', listIndexPrimitive),

  // List operations - predicates (SRFI-1)
  'any?': new FunctionObj('any?', anyPredicate),
  'every?': new FunctionObj('every?', everyPredicate),
  'find': new FunctionObj('find', findPrimitive),
  'count': new FunctionObj('count', countPrimitive),
  'partition': new FunctionObj('partition', partitionPrimitive),
  'last-pair': new FunctionObj('last-pair', lastPairPrimitive),
  'zip': new FunctionObj('zip', zipPrimitive),
  'append-map': new FunctionObj('append-map', appendMapPrimitive),
  'delete': new FunctionObj('delete', deletePrimitive),
  'delete-duplicates': new FunctionObj('delete-duplicates', deleteDuplicatesPrimitive),
  'concatenate': new FunctionObj('concatenate', concatenatePrimitive),
  'flatten': new FunctionObj('flatten', flattenPrimitive),

  // Pair mutation operations
  'set-car!': new FunctionObj('set-car!', setCarPrimitive),
  'set-cdr!': new FunctionObj('set-cdr!', setCdrPrimitive),
  'list-set!': new FunctionObj('list-set!', listSetPrimitive),

  // Vector operations
  'make-vector': new FunctionObj('make-vector', makeVectorPrimitive),
  'vector': new FunctionObj('vector', vectorPrimitive),
  'vector-ref': new FunctionObj('vector-ref', vectorRefPrimitive),
  'vector-set!': new FunctionObj('vector-set!', vectorSetPrimitive),
  'vector-length': new FunctionObj('vector-length', vectorLengthPrimitive),
  'vector-fill!': new FunctionObj('vector-fill!', vectorFillPrimitive),
  'vector-copy': new FunctionObj('vector-copy', vectorCopyPrimitive),

  // Type predicates
  'null?': new FunctionObj('null?', nullPrimitive),
  'pair?': new FunctionObj('pair?', pairPrimitive),
  'list?': new FunctionObj('list?', listPredicate),
  'number?': new FunctionObj('number?', numberPrimitive),
  'string?': new FunctionObj('string?', stringPredicate),
  'symbol?': new FunctionObj('symbol?', symbolPredicate),
  'boolean?': new FunctionObj('boolean?', booleanPredicate),
  'procedure?': new FunctionObj('procedure?', procedurePredicate),
  'vector?': new FunctionObj('vector?', vectorPredicate),
  'char?': new FunctionObj('char?', charPredicate),
  'real?': new FunctionObj('real?', realPredicate),
  'rational?': new FunctionObj('rational?', rationalPredicate),
  'complex?': new FunctionObj('complex?', complexPredicate),
  'input-port?': new FunctionObj('input-port?', inputPortPredicate),
  'output-port?': new FunctionObj('output-port?', outputPortPredicate),
  'eof-object?': new FunctionObj('eof-object?', eofObjectPredicate),
  'null-list?': new FunctionObj('null-list?', nullListPredicate),
  'proper-list?': new FunctionObj('proper-list?', properListPredicate),
  'circular-list?': new FunctionObj('circular-list?', circularListPredicate),
  'dotted-list?': new FunctionObj('dotted-list?', dottedListPredicate),
  'not-pair?': new FunctionObj('not-pair?', notPairPredicate),

  // String operations
  'string': new FunctionObj('string', stringPrimitive),
  'make-string': new FunctionObj('make-string', makeStringPrimitive),
  'string-length': new FunctionObj('string-length', stringLengthPrimitive),
  'string-append': new FunctionObj('string-append', stringAppendPrimitive),
  'string-ref': new FunctionObj('string-ref', stringRefPrimitive),
  'substring': new FunctionObj('substring', substringPrimitive),
  'string-set!': new FunctionObj('string-set!', stringSetPrimitive),
  'string-fill!': new FunctionObj('string-fill!', stringFillPrimitive),
  'string-copy': new FunctionObj('string-copy', stringCopyPrimitive),
  'string=?': new FunctionObj('string=?', stringEqualPrimitive),
  'string<?': new FunctionObj('string<?', stringLessPrimitive),
  'string>?': new FunctionObj('string>?', stringGreaterPrimitive),
  'string<=?': new FunctionObj('string<=?', stringLessOrEqualPrimitive),
  'string>=?': new FunctionObj('string>=?', stringGreaterOrEqualPrimitive),
  'string-ci=?': new FunctionObj('string-ci=?', stringCiEqualPrimitive),
  'string-ci<?': new FunctionObj('string-ci<?', stringCiLessPrimitive),
  'string-ci>?': new FunctionObj('string-ci>?', stringCiGreaterPrimitive),
  'string-ci<=?': new FunctionObj('string-ci<=?', stringCiLessOrEqualPrimitive),
  'string-ci>=?': new FunctionObj('string-ci>=?', stringCiGreaterOrEqualPrimitive),
  'string-upcase': new FunctionObj('string-upcase', stringUpcasePrimitive),
  'string-downcase': new FunctionObj('string-downcase', stringDowncasePrimitive),
  'string-null?': new FunctionObj('string-null?', stringNullPredicate),
  'string-contains?': new FunctionObj('string-contains?', stringContainsPredicate),
  'string-prefix?': new FunctionObj('string-prefix?', stringPrefixPredicate),
  'string-suffix?': new FunctionObj('string-suffix?', stringSuffixPredicate),
  'string-index': new FunctionObj('string-index', stringIndexPrimitive),
  'string-reverse': new FunctionObj('string-reverse', stringReversePrimitive),
  'string-trim': new FunctionObj('string-trim', stringTrimPrimitive),

  // Type conversions
  'number->string': new FunctionObj('number->string', numberToStringPrimitive),
  'string->number': new FunctionObj('string->number', stringToNumberPrimitive),
  'symbol->string': new FunctionObj('symbol->string', symbolToStringPrimitive),
  'string->symbol': new FunctionObj('string->symbol', stringToSymbolPrimitive),
  'char->integer': new FunctionObj('char->integer', charToIntegerPrimitive),
  'integer->char': new FunctionObj('integer->char', integerToCharPrimitive),
  'vector->list': new FunctionObj('vector->list', vectorToListPrimitive),
  'list->vector': new FunctionObj('list->vector', listToVectorPrimitive),
  'string->list': new FunctionObj('string->list', stringToListPrimitive),
  'list->string': new FunctionObj('list->string', listToStringPrimitive),
  'string->vector': new FunctionObj('string->vector', stringToVectorPrimitive),
  'vector->string': new FunctionObj('vector->string', vectorToStringPrimitive),
  'exact->inexact': new FunctionObj('exact->inexact', exactToInexactPrimitive),
  'inexact->exact': new FunctionObj('inexact->exact', inexactToExactPrimitive),

  // Character operations
  'char=?': new FunctionObj('char=?', charEqualPrimitive),
  'char<?': new FunctionObj('char<?', charLessPrimitive),
  'char>?': new FunctionObj('char>?', charGreaterPrimitive),
  'char<=?': new FunctionObj('char<=?', charLessOrEqualPrimitive),
  'char>=?': new FunctionObj('char>=?', charGreaterOrEqualPrimitive),
  'char-ci=?': new FunctionObj('char-ci=?', charCiEqualPrimitive),
  'char-ci<?': new FunctionObj('char-ci<?', charCiLessPrimitive),
  'char-ci>?': new FunctionObj('char-ci>?', charCiGreaterPrimitive),
  'char-ci<=?': new FunctionObj('char-ci<=?', charCiLessOrEqualPrimitive),
  'char-ci>=?': new FunctionObj('char-ci>=?', charCiGreaterOrEqualPrimitive),
  'char-upcase': new FunctionObj('char-upcase', charUpcasePrimitive),
  'char-downcase': new FunctionObj('char-downcase', charDowncasePrimitive),
  'char-alphabetic?': new FunctionObj('char-alphabetic?', charAlphabeticPredicate),
  'char-numeric?': new FunctionObj('char-numeric?', charNumericPredicate),
  'char-whitespace?': new FunctionObj('char-whitespace?', charWhitespacePredicate),
  'char-upper-case?': new FunctionObj('char-upper-case?', charUpperCasePredicate),
  'char-lower-case?': new FunctionObj('char-lower-case?', charLowerCasePredicate),

  // Logic
  'not': new FunctionObj('not', notPrimitive),

  // I/O primitives (stubs)
  'display': new FunctionObj('display', displayPrimitive),
  'write': new FunctionObj('write', writePrimitive),
  'newline': new FunctionObj('newline', newlinePrimitive),

  // Utility functions
  'identity': new FunctionObj('identity', identityPrimitive),
  'values': new FunctionObj('values', valuesPrimitive),
  'call-with-values': new FunctionObj('call-with-values', callWithValuesPrimitive),

  // Error handling
  'error': new FunctionObj('error', errorPrimitive),
  'debug': new FunctionObj('debug', debugPrimitive),
  'external-procedure': new FunctionObj('external-procedure', externalProcedurePrimitive),

  // Numeric utilities
  'clamp': new FunctionObj('clamp', clampPrimitive),
  'sgn': new FunctionObj('sgn', sgnPrimitive),
  'exact-integer?': new FunctionObj('exact-integer?', exactIntegerPredicate),

  // Complex number operations (stubs)
  'make-rectangular': new FunctionObj('make-rectangular', makeRectangularPrimitive),
  'make-polar': new FunctionObj('make-polar', makePolarPrimitive),
  'real-part': new FunctionObj('real-part', realPartPrimitive),
  'imag-part': new FunctionObj('imag-part', imagPartPrimitive),
  'magnitude': new FunctionObj('magnitude', magnitudePrimitive),
  'angle': new FunctionObj('angle', anglePrimitive),

  // List operations
  'circular-list': new FunctionObj('circular-list', circularListPrimitive),
  'append!': new FunctionObj('append!', appendInPlacePrimitive),
  'lset-union': new FunctionObj('lset-union', lsetUnionPrimitive),
  'lset-intersection': new FunctionObj('lset-intersection', lsetIntersectionPrimitive),

  // Additional list utilities (not already registered)
  'unfold': new FunctionObj('unfold', unfoldPrimitive),
  'fold': new FunctionObj('fold', foldPrimitive),
  'unfold-right': new FunctionObj('unfold-right', unfoldRightPrimitive),

  // Additional I/O operations (stubs)
  'open-input-file': new FunctionObj('open-input-file', openInputFilePrimitive),
  'open-output-file': new FunctionObj('open-output-file', openOutputFilePrimitive),
  'close-input-port': new FunctionObj('close-input-port', closeInputPortPrimitive),
  'close-output-port': new FunctionObj('close-output-port', closeOutputPortPrimitive),
  'read': new FunctionObj('read', readPrimitive),
  'read-char': new FunctionObj('read-char', readCharPrimitive),
  'peek-char': new FunctionObj('peek-char', peekCharPrimitive),
  'write-char': new FunctionObj('write-char', writeCharPrimitive),

  // Symbol operations
  'gensym': new FunctionObj('gensym', gensymPrimitive),

  // Additional I/O operations (stubs)
  'call-with-input-file': new FunctionObj('call-with-input-file', callWithInputFilePrimitive),
  'call-with-output-file': new FunctionObj('call-with-output-file', callWithOutputFilePrimitive),
  'load': new FunctionObj('load', loadPrimitive),

  // Grove primitives (DSSSL §9)
  'gi': new FunctionObj('gi', giPrimitive),
  'id': new FunctionObj('id', idPrimitive),
  'element-with-id': new FunctionObj('element-with-id', elementWithIdPrimitive),
  'data': new FunctionObj('data', dataPrimitive),
  'parent': new FunctionObj('parent', parentPrimitive),
  'children': new FunctionObj('children', childrenPrimitive),
  'node-list': new FunctionObj('node-list', nodeListPrimitive),
  'node-list?': new FunctionObj('node-list?', nodeListPredicate),
  'node-list-length': new FunctionObj('node-list-length', nodeListLengthPrimitive),
  'node-list-count': new FunctionObj('node-list-count', nodeListLengthPrimitive),  // Alias for node-list-length
  'node-list->list': new FunctionObj('node-list->list', nodeListToListPrimitive),
  'empty-node-list': new FunctionObj('empty-node-list', emptyNodeListPrimitive),
  'node-list-empty?': new FunctionObj('node-list-empty?', nodeListEmptyPredicate),
  'node-list-first': new FunctionObj('node-list-first', nodeListFirstPrimitive),
  'node-list-rest': new FunctionObj('node-list-rest', nodeListRestPrimitive),
  'node-list-reverse': new FunctionObj('node-list-reverse', nodeListReversePrimitive),
  'select-elements': new FunctionObj('select-elements', selectElementsPrimitive),
  'node-list-map': new FunctionObj('node-list-map', nodeListMapPrimitive),
  'node-list-union': new FunctionObj('node-list-union', nodeListUnionPrimitive),
  'node-list-some?': new FunctionObj('node-list-some?', nodeListSomePredicate),
  'node-list=?': new FunctionObj('node-list=?', nodeListEqualPredicate),
  'node-list-contains?': new FunctionObj('node-list-contains?', nodeListContainsPredicate),
  'attributes': new FunctionObj('attributes', attributesPrimitive),
  'attribute-string': new FunctionObj('attribute-string', attributeStringPrimitive),
  'ancestors': new FunctionObj('ancestors', ancestorsPrimitive),
  'ancestor': new FunctionObj('ancestor', ancestorPrimitive),
  'descendants': new FunctionObj('descendants', descendantsPrimitive),
  'child-number': new FunctionObj('child-number', childNumberPrimitive),
  'first-sibling?': new FunctionObj('first-sibling?', firstSiblingPredicate),
  'last-sibling?': new FunctionObj('last-sibling?', lastSiblingPredicate),

  // DSSSL processing primitives (§10)
  'current-node': new FunctionObj('current-node', currentNodePrimitive),
  'process-root': new FunctionObj('process-root', processRootPrimitive),
  'process-children': new FunctionObj('process-children', processChildrenPrimitive),
  'process-node-list': new FunctionObj('process-node-list', processNodeListPrimitive),
  'next-match': new FunctionObj('next-match', nextMatchPrimitive),
  'empty-sosofo': new FunctionObj('empty-sosofo', emptySosofoPrimitive),
  'literal': new FunctionObj('literal', literalPrimitive),
  'sosofo-append': new FunctionObj('sosofo-append', sosofoAppendPrimitive),
  'make-flow-object': new FunctionObj('make-flow-object', makeFlowObjectPrimitive),
};

// Register external procedures in the external procedure table
// Port from: OpenJade installXPrimitive (primitive.cxx line 5326-5328)
// XPRIMITIVE procedures use "UNREGISTERED::James Clark//Procedure::" prefix
externalProcTable.set('UNREGISTERED::James Clark//Procedure::debug', standardPrimitives['debug']);

/**
 * Get a primitive by name
 */
export function getPrimitive(name: string): FunctionObj | undefined {
  return standardPrimitives[name];
}
