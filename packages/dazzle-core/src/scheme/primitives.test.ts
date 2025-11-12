/**
 * Primitives tests - Test basic Scheme primitives
 */

import { describe, it, expect } from 'vitest';
import { VM } from './vm';
import { ConstantInsn, PrimitiveCallInsn } from './insn';
import { makeNumber, makeBoolean, makePair, theNilObj, theTrueObj, theFalseObj } from './elobj';
import { getPrimitive } from './primitives';

describe('Arithmetic Primitives', () => {
  it('should add numbers with +', () => {
    const vm = new VM();
    const addFn = getPrimitive('+')!;

    // (+ 1 2 3) = 6
    const call = new PrimitiveCallInsn(3, addFn, null);
    const push3 = new ConstantInsn(makeNumber(3), call);
    const push2 = new ConstantInsn(makeNumber(2), push3);
    const push1 = new ConstantInsn(makeNumber(1), push2);

    const result = vm.eval(push1);

    expect(result.asNumber()?.value).toBe(6);
  });

  it('should add zero arguments with +', () => {
    const vm = new VM();
    const addFn = getPrimitive('+')!;

    // (+) = 0
    const call = new PrimitiveCallInsn(0, addFn, null);
    const result = vm.eval(call);

    expect(result.asNumber()?.value).toBe(0);
  });

  it('should subtract numbers with -', () => {
    const vm = new VM();
    const subFn = getPrimitive('-')!;

    // (- 10 3 2) = 5
    const call = new PrimitiveCallInsn(3, subFn, null);
    const push2 = new ConstantInsn(makeNumber(2), call);
    const push3 = new ConstantInsn(makeNumber(3), push2);
    const push10 = new ConstantInsn(makeNumber(10), push3);

    const result = vm.eval(push10);

    expect(result.asNumber()?.value).toBe(5);
  });

  it('should negate with unary -', () => {
    const vm = new VM();
    const subFn = getPrimitive('-')!;

    // (- 42) = -42
    const call = new PrimitiveCallInsn(1, subFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result.asNumber()?.value).toBe(-42);
  });

  it('should multiply numbers with *', () => {
    const vm = new VM();
    const mulFn = getPrimitive('*')!;

    // (* 2 3 4) = 24
    const call = new PrimitiveCallInsn(3, mulFn, null);
    const push4 = new ConstantInsn(makeNumber(4), call);
    const push3 = new ConstantInsn(makeNumber(3), push4);
    const push2 = new ConstantInsn(makeNumber(2), push3);

    const result = vm.eval(push2);

    expect(result.asNumber()?.value).toBe(24);
  });

  it('should divide numbers with /', () => {
    const vm = new VM();
    const divFn = getPrimitive('/')!;

    // (/ 20 2 2) = 5
    const call = new PrimitiveCallInsn(3, divFn, null);
    const push2b = new ConstantInsn(makeNumber(2), call);
    const push2a = new ConstantInsn(makeNumber(2), push2b);
    const push20 = new ConstantInsn(makeNumber(20), push2a);

    const result = vm.eval(push20);

    expect(result.asNumber()?.value).toBe(5);
  });

  it('should compute reciprocal with unary /', () => {
    const vm = new VM();
    const divFn = getPrimitive('/')!;

    // (/ 4) = 0.25
    const call = new PrimitiveCallInsn(1, divFn, null);
    const push4 = new ConstantInsn(makeNumber(4), call);

    const result = vm.eval(push4);

    expect(result.asNumber()?.value).toBe(0.25);
  });
});

describe('Comparison Primitives', () => {
  it('should compare numbers with <', () => {
    const vm = new VM();
    const ltFn = getPrimitive('<')!;

    // (< 1 2 3) = #t
    const call = new PrimitiveCallInsn(3, ltFn, null);
    const push3 = new ConstantInsn(makeNumber(3), call);
    const push2 = new ConstantInsn(makeNumber(2), push3);
    const push1 = new ConstantInsn(makeNumber(1), push2);

    const result = vm.eval(push1);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-ascending <', () => {
    const vm = new VM();
    const ltFn = getPrimitive('<')!;

    // (< 1 3 2) = #f
    const call = new PrimitiveCallInsn(3, ltFn, null);
    const push2 = new ConstantInsn(makeNumber(2), call);
    const push3 = new ConstantInsn(makeNumber(3), push2);
    const push1 = new ConstantInsn(makeNumber(1), push3);

    const result = vm.eval(push1);

    expect(result).toBe(theFalseObj);
  });

  it('should compare numbers with >', () => {
    const vm = new VM();
    const gtFn = getPrimitive('>')!;

    // (> 3 2 1) = #t
    const call = new PrimitiveCallInsn(3, gtFn, null);
    const push1 = new ConstantInsn(makeNumber(1), call);
    const push2 = new ConstantInsn(makeNumber(2), push1);
    const push3 = new ConstantInsn(makeNumber(3), push2);

    const result = vm.eval(push3);

    expect(result).toBe(theTrueObj);
  });

  it('should check numeric equality with =', () => {
    const vm = new VM();
    const eqFn = getPrimitive('=')!;

    // (= 5 5 5) = #t
    const call = new PrimitiveCallInsn(3, eqFn, null);
    const push5c = new ConstantInsn(makeNumber(5), call);
    const push5b = new ConstantInsn(makeNumber(5), push5c);
    const push5a = new ConstantInsn(makeNumber(5), push5b);

    const result = vm.eval(push5a);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for unequal numbers with =', () => {
    const vm = new VM();
    const eqFn = getPrimitive('=')!;

    // (= 5 5 6) = #f
    const call = new PrimitiveCallInsn(3, eqFn, null);
    const push6 = new ConstantInsn(makeNumber(6), call);
    const push5b = new ConstantInsn(makeNumber(5), push6);
    const push5a = new ConstantInsn(makeNumber(5), push5b);

    const result = vm.eval(push5a);

    expect(result).toBe(theFalseObj);
  });
});

describe('List Primitives', () => {
  it('should create pairs with cons', () => {
    const vm = new VM();
    const consFn = getPrimitive('cons')!;

    // (cons 1 2) = (1 . 2)
    const call = new PrimitiveCallInsn(2, consFn, null);
    const push2 = new ConstantInsn(makeNumber(2), call);
    const push1 = new ConstantInsn(makeNumber(1), push2);

    const result = vm.eval(push1);

    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asNumber()?.value).toBe(1);
    expect(pair?.cdr.asNumber()?.value).toBe(2);
  });

  it('should get car of a pair', () => {
    const vm = new VM();
    const carFn = getPrimitive('car')!;

    // (car (1 . 2)) = 1
    const pair = makePair(makeNumber(1), makeNumber(2));
    const call = new PrimitiveCallInsn(1, carFn, null);
    const pushPair = new ConstantInsn(pair, call);

    const result = vm.eval(pushPair);

    expect(result.asNumber()?.value).toBe(1);
  });

  it('should get cdr of a pair', () => {
    const vm = new VM();
    const cdrFn = getPrimitive('cdr')!;

    // (cdr (1 . 2)) = 2
    const pair = makePair(makeNumber(1), makeNumber(2));
    const call = new PrimitiveCallInsn(1, cdrFn, null);
    const pushPair = new ConstantInsn(pair, call);

    const result = vm.eval(pushPair);

    expect(result.asNumber()?.value).toBe(2);
  });

  it('should check for null with null?', () => {
    const vm = new VM();
    const nullFn = getPrimitive('null?')!;

    // (null? '()) = #t
    const call = new PrimitiveCallInsn(1, nullFn, null);
    const pushNil = new ConstantInsn(theNilObj, call);

    const result = vm.eval(pushNil);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-null with null?', () => {
    const vm = new VM();
    const nullFn = getPrimitive('null?')!;

    // (null? 42) = #f
    const call = new PrimitiveCallInsn(1, nullFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });
});

describe('Type Predicates', () => {
  it('should check for pairs with pair?', () => {
    const vm = new VM();
    const pairFn = getPrimitive('pair?')!;

    // (pair? (1 . 2)) = #t
    const pair = makePair(makeNumber(1), makeNumber(2));
    const call = new PrimitiveCallInsn(1, pairFn, null);
    const pushPair = new ConstantInsn(pair, call);

    const result = vm.eval(pushPair);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-pairs with pair?', () => {
    const vm = new VM();
    const pairFn = getPrimitive('pair?')!;

    // (pair? 42) = #f
    const call = new PrimitiveCallInsn(1, pairFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });

  it('should check for numbers with number?', () => {
    const vm = new VM();
    const numberFn = getPrimitive('number?')!;

    // (number? 42) = #t
    const call = new PrimitiveCallInsn(1, numberFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-numbers with number?', () => {
    const vm = new VM();
    const numberFn = getPrimitive('number?')!;

    // (number? '()) = #f
    const call = new PrimitiveCallInsn(1, numberFn, null);
    const pushNil = new ConstantInsn(theNilObj, call);

    const result = vm.eval(pushNil);

    expect(result).toBe(theFalseObj);
  });
});

describe('Logic Primitives', () => {
  it('should negate true with not', () => {
    const vm = new VM();
    const notFn = getPrimitive('not')!;

    // (not #t) = #f
    const call = new PrimitiveCallInsn(1, notFn, null);
    const pushTrue = new ConstantInsn(theTrueObj, call);

    const result = vm.eval(pushTrue);

    expect(result).toBe(theFalseObj);
  });

  it('should negate false with not', () => {
    const vm = new VM();
    const notFn = getPrimitive('not')!;

    // (not #f) = #t
    const call = new PrimitiveCallInsn(1, notFn, null);
    const pushFalse = new ConstantInsn(theFalseObj, call);

    const result = vm.eval(pushFalse);

    expect(result).toBe(theTrueObj);
  });

  it('should treat non-false values as true in not', () => {
    const vm = new VM();
    const notFn = getPrimitive('not')!;

    // (not 42) = #f
    const call = new PrimitiveCallInsn(1, notFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });
});
