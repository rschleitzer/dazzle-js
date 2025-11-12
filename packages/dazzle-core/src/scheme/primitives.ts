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
  makeNumber,
  makeBoolean,
  makePair,
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj';

/**
 * Arithmetic primitive: +
 * Port from: primitive.cxx Plus::primitiveCall
 */
const plusPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const minusPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const timesPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const dividePrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const lessThanPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const greaterThanPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const numericEqualPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const consPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
  if (args.length !== 2) {
    throw new Error('cons requires exactly 2 arguments');
  }
  return makePair(args[0], args[1]);
};

/**
 * List primitive: car
 * Port from: primitive.cxx Car::primitiveCall
 */
const carPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const cdrPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
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
const nullPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
  if (args.length !== 1) {
    throw new Error('null? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asNil() !== null);
};

/**
 * Type predicate: pair?
 * Port from: primitive.cxx Pair::primitiveCall
 */
const pairPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
  if (args.length !== 1) {
    throw new Error('pair? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asPair() !== null);
};

/**
 * Type predicate: number?
 * Port from: primitive.cxx Number::primitiveCall
 */
const numberPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
  if (args.length !== 1) {
    throw new Error('number? requires exactly 1 argument');
  }

  return makeBoolean(args[0].asNumber() !== null);
};

/**
 * Logic primitive: not
 * Port from: primitive.cxx Not::primitiveCall
 */
const notPrimitive: PrimitiveFunction = (args: ELObj[]): ELObj => {
  if (args.length !== 1) {
    throw new Error('not requires exactly 1 argument');
  }

  return makeBoolean(!args[0].isTrue());
};

/**
 * Standard primitive registry
 * Maps primitive names to function objects
 */
export const standardPrimitives: Record<string, FunctionObj> = {
  '+': new FunctionObj('+', plusPrimitive),
  '-': new FunctionObj('-', minusPrimitive),
  '*': new FunctionObj('*', timesPrimitive),
  '/': new FunctionObj('/', dividePrimitive),
  '<': new FunctionObj('<', lessThanPrimitive),
  '>': new FunctionObj('>', greaterThanPrimitive),
  '=': new FunctionObj('=', numericEqualPrimitive),
  'cons': new FunctionObj('cons', consPrimitive),
  'car': new FunctionObj('car', carPrimitive),
  'cdr': new FunctionObj('cdr', cdrPrimitive),
  'null?': new FunctionObj('null?', nullPrimitive),
  'pair?': new FunctionObj('pair?', pairPrimitive),
  'number?': new FunctionObj('number?', numberPrimitive),
  'not': new FunctionObj('not', notPrimitive),
};

/**
 * Get a primitive by name
 */
export function getPrimitive(name: string): FunctionObj | undefined {
  return standardPrimitives[name];
}
