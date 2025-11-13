/**
 * Primitives tests - Test basic Scheme primitives
 */

import { describe, it, expect } from 'vitest';
import { VM } from './vm';
import { ConstantInsn, PrimitiveCallInsn } from './insn';
import { makeNumber, makeBoolean, makePair, makeString, makeSymbol, theNilObj, theTrueObj, theFalseObj } from './elobj';
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

  it('should compare numbers with <=', () => {
    const vm = new VM();
    const leFn = getPrimitive('<=')!;

    // (<= 1 2 2 3) = #t
    const call = new PrimitiveCallInsn(4, leFn, null);
    const push3 = new ConstantInsn(makeNumber(3), call);
    const push2b = new ConstantInsn(makeNumber(2), push3);
    const push2a = new ConstantInsn(makeNumber(2), push2b);
    const push1 = new ConstantInsn(makeNumber(1), push2a);

    const result = vm.eval(push1);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-ascending <=', () => {
    const vm = new VM();
    const leFn = getPrimitive('<=')!;

    // (<= 1 3 2) = #f
    const call = new PrimitiveCallInsn(3, leFn, null);
    const push2 = new ConstantInsn(makeNumber(2), call);
    const push3 = new ConstantInsn(makeNumber(3), push2);
    const push1 = new ConstantInsn(makeNumber(1), push3);

    const result = vm.eval(push1);

    expect(result).toBe(theFalseObj);
  });

  it('should compare numbers with >=', () => {
    const vm = new VM();
    const geFn = getPrimitive('>=')!;

    // (>= 3 2 2 1) = #t
    const call = new PrimitiveCallInsn(4, geFn, null);
    const push1 = new ConstantInsn(makeNumber(1), call);
    const push2b = new ConstantInsn(makeNumber(2), push1);
    const push2a = new ConstantInsn(makeNumber(2), push2b);
    const push3 = new ConstantInsn(makeNumber(3), push2a);

    const result = vm.eval(push3);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-descending >=', () => {
    const vm = new VM();
    const geFn = getPrimitive('>=')!;

    // (>= 3 1 2) = #f
    const call = new PrimitiveCallInsn(3, geFn, null);
    const push2 = new ConstantInsn(makeNumber(2), call);
    const push1 = new ConstantInsn(makeNumber(1), push2);
    const push3 = new ConstantInsn(makeNumber(3), push1);

    const result = vm.eval(push3);

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

  it('should create list with list', () => {
    const vm = new VM();
    const listFn = getPrimitive('list')!;

    // (list 1 2 3) = (1 2 3)
    const call = new PrimitiveCallInsn(3, listFn, null);
    const push3 = new ConstantInsn(makeNumber(3), call);
    const push2 = new ConstantInsn(makeNumber(2), push3);
    const push1 = new ConstantInsn(makeNumber(1), push2);

    const result = vm.eval(push1);

    const pair1 = result.asPair();
    expect(pair1).not.toBeNull();
    expect(pair1?.car.asNumber()?.value).toBe(1);

    const pair2 = pair1?.cdr.asPair();
    expect(pair2).not.toBeNull();
    expect(pair2?.car.asNumber()?.value).toBe(2);

    const pair3 = pair2?.cdr.asPair();
    expect(pair3).not.toBeNull();
    expect(pair3?.car.asNumber()?.value).toBe(3);

    expect(pair3?.cdr).toBe(theNilObj);
  });

  it('should create empty list with list', () => {
    const vm = new VM();
    const listFn = getPrimitive('list')!;

    // (list) = '()
    const call = new PrimitiveCallInsn(0, listFn, null);

    const result = vm.eval(call);

    expect(result).toBe(theNilObj);
  });

  it('should get length of list', () => {
    const vm = new VM();
    const lengthFn = getPrimitive('length')!;

    // (length '(1 2 3)) = 3
    const list = makePair(makeNumber(1), makePair(makeNumber(2), makePair(makeNumber(3), theNilObj)));
    const call = new PrimitiveCallInsn(1, lengthFn, null);
    const pushList = new ConstantInsn(list, call);

    const result = vm.eval(pushList);

    expect(result.asNumber()?.value).toBe(3);
  });

  it('should get length of empty list', () => {
    const vm = new VM();
    const lengthFn = getPrimitive('length')!;

    // (length '()) = 0
    const call = new PrimitiveCallInsn(1, lengthFn, null);
    const pushNil = new ConstantInsn(theNilObj, call);

    const result = vm.eval(pushNil);

    expect(result.asNumber()?.value).toBe(0);
  });

  it('should append lists', () => {
    const vm = new VM();
    const appendFn = getPrimitive('append')!;

    // (append '(1 2) '(3 4)) = (1 2 3 4)
    const list1 = makePair(makeNumber(1), makePair(makeNumber(2), theNilObj));
    const list2 = makePair(makeNumber(3), makePair(makeNumber(4), theNilObj));
    const call = new PrimitiveCallInsn(2, appendFn, null);
    const pushList2 = new ConstantInsn(list2, call);
    const pushList1 = new ConstantInsn(list1, pushList2);

    const result = vm.eval(pushList1);

    const pair1 = result.asPair();
    expect(pair1?.car.asNumber()?.value).toBe(1);
    const pair2 = pair1?.cdr.asPair();
    expect(pair2?.car.asNumber()?.value).toBe(2);
    const pair3 = pair2?.cdr.asPair();
    expect(pair3?.car.asNumber()?.value).toBe(3);
    const pair4 = pair3?.cdr.asPair();
    expect(pair4?.car.asNumber()?.value).toBe(4);
    expect(pair4?.cdr).toBe(theNilObj);
  });

  it('should append with empty lists', () => {
    const vm = new VM();
    const appendFn = getPrimitive('append')!;

    // (append '() '(1 2) '()) = (1 2)
    const list = makePair(makeNumber(1), makePair(makeNumber(2), theNilObj));
    const call = new PrimitiveCallInsn(3, appendFn, null);
    const pushNil2 = new ConstantInsn(theNilObj, call);
    const pushList = new ConstantInsn(list, pushNil2);
    const pushNil1 = new ConstantInsn(theNilObj, pushList);

    const result = vm.eval(pushNil1);

    const pair1 = result.asPair();
    expect(pair1?.car.asNumber()?.value).toBe(1);
    const pair2 = pair1?.cdr.asPair();
    expect(pair2?.car.asNumber()?.value).toBe(2);
    expect(pair2?.cdr).toBe(theNilObj);
  });

  it('should reverse list', () => {
    const vm = new VM();
    const reverseFn = getPrimitive('reverse')!;

    // (reverse '(1 2 3)) = (3 2 1)
    const list = makePair(makeNumber(1), makePair(makeNumber(2), makePair(makeNumber(3), theNilObj)));
    const call = new PrimitiveCallInsn(1, reverseFn, null);
    const pushList = new ConstantInsn(list, call);

    const result = vm.eval(pushList);

    const pair1 = result.asPair();
    expect(pair1?.car.asNumber()?.value).toBe(3);
    const pair2 = pair1?.cdr.asPair();
    expect(pair2?.car.asNumber()?.value).toBe(2);
    const pair3 = pair2?.cdr.asPair();
    expect(pair3?.car.asNumber()?.value).toBe(1);
    expect(pair3?.cdr).toBe(theNilObj);
  });

  it('should reverse empty list', () => {
    const vm = new VM();
    const reverseFn = getPrimitive('reverse')!;

    // (reverse '()) = '()
    const call = new PrimitiveCallInsn(1, reverseFn, null);
    const pushNil = new ConstantInsn(theNilObj, call);

    const result = vm.eval(pushNil);

    expect(result).toBe(theNilObj);
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

  it('should check for lists with list?', () => {
    const vm = new VM();
    const listFn = getPrimitive('list?')!;

    // (list? '(1 2 3)) = #t
    const list = makePair(makeNumber(1), makePair(makeNumber(2), makePair(makeNumber(3), theNilObj)));
    const call = new PrimitiveCallInsn(1, listFn, null);
    const pushList = new ConstantInsn(list, call);

    const result = vm.eval(pushList);

    expect(result).toBe(theTrueObj);
  });

  it('should return true for empty list with list?', () => {
    const vm = new VM();
    const listFn = getPrimitive('list?')!;

    // (list? '()) = #t
    const call = new PrimitiveCallInsn(1, listFn, null);
    const pushNil = new ConstantInsn(theNilObj, call);

    const result = vm.eval(pushNil);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for improper list with list?', () => {
    const vm = new VM();
    const listFn = getPrimitive('list?')!;

    // (list? '(1 . 2)) = #f
    const pair = makePair(makeNumber(1), makeNumber(2));
    const call = new PrimitiveCallInsn(1, listFn, null);
    const pushPair = new ConstantInsn(pair, call);

    const result = vm.eval(pushPair);

    expect(result).toBe(theFalseObj);
  });

  it('should check for strings with string?', () => {
    const vm = new VM();
    const stringFn = getPrimitive('string?')!;

    // (string? "hello") = #t
    const call = new PrimitiveCallInsn(1, stringFn, null);
    const pushStr = new ConstantInsn(makeString('hello'), call);

    const result = vm.eval(pushStr);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-strings with string?', () => {
    const vm = new VM();
    const stringFn = getPrimitive('string?')!;

    // (string? 42) = #f
    const call = new PrimitiveCallInsn(1, stringFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });

  it('should check for symbols with symbol?', () => {
    const vm = new VM();
    const symbolFn = getPrimitive('symbol?')!;

    // (symbol? 'foo) = #t
    const call = new PrimitiveCallInsn(1, symbolFn, null);
    const pushSym = new ConstantInsn(makeSymbol('foo'), call);

    const result = vm.eval(pushSym);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-symbols with symbol?', () => {
    const vm = new VM();
    const symbolFn = getPrimitive('symbol?')!;

    // (symbol? 42) = #f
    const call = new PrimitiveCallInsn(1, symbolFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });

  it('should check for booleans with boolean?', () => {
    const vm = new VM();
    const booleanFn = getPrimitive('boolean?')!;

    // (boolean? #t) = #t
    const call = new PrimitiveCallInsn(1, booleanFn, null);
    const pushTrue = new ConstantInsn(theTrueObj, call);

    const result = vm.eval(pushTrue);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-booleans with boolean?', () => {
    const vm = new VM();
    const booleanFn = getPrimitive('boolean?')!;

    // (boolean? 42) = #f
    const call = new PrimitiveCallInsn(1, booleanFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

    expect(result).toBe(theFalseObj);
  });

  it('should check for procedures with procedure?', () => {
    const vm = new VM();
    const procedureFn = getPrimitive('procedure?')!;
    const addFn = getPrimitive('+')!;

    // (procedure? +) = #t
    const call = new PrimitiveCallInsn(1, procedureFn, null);
    const pushAdd = new ConstantInsn(addFn, call);

    const result = vm.eval(pushAdd);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for non-procedures with procedure?', () => {
    const vm = new VM();
    const procedureFn = getPrimitive('procedure?')!;

    // (procedure? 42) = #f
    const call = new PrimitiveCallInsn(1, procedureFn, null);
    const push42 = new ConstantInsn(makeNumber(42), call);

    const result = vm.eval(push42);

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

describe('Equality Primitives', () => {
  it('should check identity with eq?', () => {
    const vm = new VM();
    const eqFn = getPrimitive('eq?')!;

    // (eq? 'foo 'foo) = #t (symbols are interned)
    const sym = makeSymbol('foo');
    const call = new PrimitiveCallInsn(2, eqFn, null);
    const pushSym2 = new ConstantInsn(sym, call);
    const pushSym1 = new ConstantInsn(sym, pushSym2);

    const result = vm.eval(pushSym1);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for different objects with eq?', () => {
    const vm = new VM();
    const eqFn = getPrimitive('eq?')!;

    // (eq? '(1) '(1)) = #f (different objects)
    const list1 = makePair(makeNumber(1), theNilObj);
    const list2 = makePair(makeNumber(1), theNilObj);
    const call = new PrimitiveCallInsn(2, eqFn, null);
    const pushList2 = new ConstantInsn(list2, call);
    const pushList1 = new ConstantInsn(list1, pushList2);

    const result = vm.eval(pushList1);

    expect(result).toBe(theFalseObj);
  });

  it('should check equivalence with eqv?', () => {
    const vm = new VM();
    const eqvFn = getPrimitive('eqv?')!;

    // (eqv? 5 5) = #t
    const call = new PrimitiveCallInsn(2, eqvFn, null);
    const push5b = new ConstantInsn(makeNumber(5), call);
    const push5a = new ConstantInsn(makeNumber(5), push5b);

    const result = vm.eval(push5a);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for different values with eqv?', () => {
    const vm = new VM();
    const eqvFn = getPrimitive('eqv?')!;

    // (eqv? 5 6) = #f
    const call = new PrimitiveCallInsn(2, eqvFn, null);
    const push6 = new ConstantInsn(makeNumber(6), call);
    const push5 = new ConstantInsn(makeNumber(5), push6);

    const result = vm.eval(push5);

    expect(result).toBe(theFalseObj);
  });

  it('should check structural equality with equal?', () => {
    const vm = new VM();
    const equalFn = getPrimitive('equal?')!;

    // (equal? '(1 2) '(1 2)) = #t
    const list1 = makePair(makeNumber(1), makePair(makeNumber(2), theNilObj));
    const list2 = makePair(makeNumber(1), makePair(makeNumber(2), theNilObj));
    const call = new PrimitiveCallInsn(2, equalFn, null);
    const pushList2 = new ConstantInsn(list2, call);
    const pushList1 = new ConstantInsn(list1, pushList2);

    const result = vm.eval(pushList1);

    expect(result).toBe(theTrueObj);
  });

  it('should return false for structurally different lists with equal?', () => {
    const vm = new VM();
    const equalFn = getPrimitive('equal?')!;

    // (equal? '(1 2) '(1 3)) = #f
    const list1 = makePair(makeNumber(1), makePair(makeNumber(2), theNilObj));
    const list2 = makePair(makeNumber(1), makePair(makeNumber(3), theNilObj));
    const call = new PrimitiveCallInsn(2, equalFn, null);
    const pushList2 = new ConstantInsn(list2, call);
    const pushList1 = new ConstantInsn(list1, pushList2);

    const result = vm.eval(pushList1);

    expect(result).toBe(theFalseObj);
  });

  it('should check string equality with equal?', () => {
    const vm = new VM();
    const equalFn = getPrimitive('equal?')!;

    // (equal? "hello" "hello") = #t
    const call = new PrimitiveCallInsn(2, equalFn, null);
    const pushStr2 = new ConstantInsn(makeString('hello'), call);
    const pushStr1 = new ConstantInsn(makeString('hello'), pushStr2);

    const result = vm.eval(pushStr1);

    expect(result).toBe(theTrueObj);
  });
});

describe('String Primitives', () => {
  it('should get string length', () => {
    const vm = new VM();
    const lenFn = getPrimitive('string-length')!;

    // (string-length "hello") = 5
    const call = new PrimitiveCallInsn(1, lenFn, null);
    const pushStr = new ConstantInsn(makeString('hello'), call);

    const result = vm.eval(pushStr);

    expect(result.asNumber()?.value).toBe(5);
  });

  it('should get length of empty string', () => {
    const vm = new VM();
    const lenFn = getPrimitive('string-length')!;

    // (string-length "") = 0
    const call = new PrimitiveCallInsn(1, lenFn, null);
    const pushStr = new ConstantInsn(makeString(''), call);

    const result = vm.eval(pushStr);

    expect(result.asNumber()?.value).toBe(0);
  });

  it('should append strings', () => {
    const vm = new VM();
    const appendFn = getPrimitive('string-append')!;

    // (string-append "hello" " " "world") = "hello world"
    const call = new PrimitiveCallInsn(3, appendFn, null);
    const pushWorld = new ConstantInsn(makeString('world'), call);
    const pushSpace = new ConstantInsn(makeString(' '), pushWorld);
    const pushHello = new ConstantInsn(makeString('hello'), pushSpace);

    const result = vm.eval(pushHello);

    expect(result.asString()?.value).toBe('hello world');
  });

  it('should append with empty strings', () => {
    const vm = new VM();
    const appendFn = getPrimitive('string-append')!;

    // (string-append "" "hello" "") = "hello"
    const call = new PrimitiveCallInsn(3, appendFn, null);
    const pushEmpty2 = new ConstantInsn(makeString(''), call);
    const pushHello = new ConstantInsn(makeString('hello'), pushEmpty2);
    const pushEmpty1 = new ConstantInsn(makeString(''), pushHello);

    const result = vm.eval(pushEmpty1);

    expect(result.asString()?.value).toBe('hello');
  });

  it('should append zero strings', () => {
    const vm = new VM();
    const appendFn = getPrimitive('string-append')!;

    // (string-append) = ""
    const call = new PrimitiveCallInsn(0, appendFn, null);

    const result = vm.eval(call);

    expect(result.asString()?.value).toBe('');
  });
});
