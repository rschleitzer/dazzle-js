/**
 * VM tests - Basic bytecode execution
 */

import { describe, it, expect } from 'vitest';
import { VM } from './vm';
import {
  type Insn,
  ConstantInsn,
  TestInsn,
  PopInsn,
  ConsInsn,
  CaseInsn,
  FrameRefInsn,
  StackRefInsn,
  ClosureRefInsn,
} from './insn';
import { makeNumber, makeBoolean, makeSymbol, theTrueObj, theFalseObj } from './elobj';

describe('VM', () => {
  it('should execute a constant instruction', () => {
    const vm = new VM();
    const value = makeNumber(42);
    const insn = new ConstantInsn(value, null);

    const result = vm.eval(insn);

    expect(result).toBe(value);
    expect(result.asNumber()?.value).toBe(42);
  });

  it('should execute a sequence of instructions through TestInsn', () => {
    const vm = new VM();
    // Test: if true then 10 else 20
    const consequent = new ConstantInsn(makeNumber(10), null);
    const alternative = new ConstantInsn(makeNumber(20), null);
    const test = new TestInsn(consequent, alternative);
    const pushTest = new ConstantInsn(theTrueObj, test);

    const result = vm.eval(pushTest);

    // Should take true branch
    expect(result.asNumber()?.value).toBe(10);
  });

  it('should execute a test instruction with true branch', () => {
    const vm = new VM();
    const consequent = new ConstantInsn(makeNumber(1), null);
    const alternative = new ConstantInsn(makeNumber(2), null);
    const test = new TestInsn(consequent, alternative);
    const pushTest = new ConstantInsn(theTrueObj, test);

    const result = vm.eval(pushTest);

    expect(result.asNumber()?.value).toBe(1);
  });

  it('should execute a test instruction with false branch', () => {
    const vm = new VM();
    const consequent = new ConstantInsn(makeNumber(1), null);
    const alternative = new ConstantInsn(makeNumber(2), null);
    const test = new TestInsn(consequent, alternative);
    const pushTest = new ConstantInsn(theFalseObj, test);

    const result = vm.eval(pushTest);

    expect(result.asNumber()?.value).toBe(2);
  });

  it('should execute PopInsn to remove top value', () => {
    const vm = new VM();
    // Push 42, push 99, pop, result should be 42
    const pop = new PopInsn(null);
    const push99 = new ConstantInsn(makeNumber(99), pop);
    const push42 = new ConstantInsn(makeNumber(42), push99);

    const result = vm.eval(push42);

    expect(result.asNumber()?.value).toBe(42);
  });

  it('should execute ConsInsn to create pairs', () => {
    const vm = new VM();
    // Push 1, push 2, cons -> (1 . 2)
    const cons = new ConsInsn(null);
    const push2 = new ConstantInsn(makeNumber(2), cons);
    const push1 = new ConstantInsn(makeNumber(1), push2);

    const result = vm.eval(push1);

    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asNumber()?.value).toBe(1);
    expect(pair?.cdr.asNumber()?.value).toBe(2);
  });

  it('should execute CaseInsn with matching value', () => {
    const vm = new VM();
    const matchBranch = new ConstantInsn(makeNumber(100), null);
    const failBranch = new ConstantInsn(makeNumber(200), null);
    const caseTest = new CaseInsn(makeSymbol('foo'), matchBranch, failBranch);
    const pushValue = new ConstantInsn(makeSymbol('foo'), caseTest);

    const result = vm.eval(pushValue);

    expect(result.asNumber()?.value).toBe(100);
  });

  it('should execute CaseInsn with non-matching value', () => {
    const vm = new VM();
    const matchBranch = new ConstantInsn(makeNumber(100), null);
    // Fail branch must pop the unmatched value before pushing result
    const failBranch = new PopInsn(new ConstantInsn(makeNumber(200), null));
    const caseTest = new CaseInsn(makeSymbol('foo'), matchBranch, failBranch);
    const pushValue = new ConstantInsn(makeSymbol('bar'), caseTest);

    const result = vm.eval(pushValue);

    expect(result.asNumber()?.value).toBe(200);
  });

  it('should execute CaseInsn with numeric values', () => {
    const vm = new VM();
    const matchBranch = new ConstantInsn(makeNumber(100), null);
    const failBranch = new ConstantInsn(makeNumber(200), null);
    const caseTest = new CaseInsn(makeNumber(42), matchBranch, failBranch);
    const pushValue = new ConstantInsn(makeNumber(42), caseTest);

    const result = vm.eval(pushValue);

    expect(result.asNumber()?.value).toBe(100);
  });

  it('should access frame variables with FrameRefInsn', () => {
    const vm = new VM();

    // Simple test: push 10, 20, then access frame[1]
    // Stack: [10, 20] then FrameRef[1] pushes stack[0+1]=20
    // Final stack: [10, 20, 20] - need to pop first two values
    const frameRef = new FrameRefInsn(1, null);
    const push20 = new ConstantInsn(makeNumber(20), frameRef);
    const push10 = new ConstantInsn(makeNumber(10), push20);

    // Don't use eval - test the instruction directly
    const insn = push10;
    let current: Insn | null = insn;
    while (current) {
      current = current.execute(vm);
    }

    // Stack should be [10, 20, 20]
    expect(vm.stackSize()).toBe(3);
    const result = vm.pop(); // Get the accessed value
    expect(result.asNumber()?.value).toBe(20);
  });

  it('should access stack variables with StackRefInsn', () => {
    const vm = new VM();

    // Push 10, 20, 30, then access sp[-2]
    // After 3 pushes, sp=3, so sp[-2] = stack[1] = 20
    const stackRef = new StackRefInsn(-2, 0, null);
    const push30 = new ConstantInsn(makeNumber(30), stackRef);
    const push20 = new ConstantInsn(makeNumber(20), push30);
    const push10 = new ConstantInsn(makeNumber(10), push20);

    // Execute directly without eval
    let current: Insn | null = push10;
    while (current) {
      current = current.execute(vm);
    }

    // Stack should be [10, 20, 30, 20]
    expect(vm.stackSize()).toBe(4);
    const result = vm.pop(); // Get the accessed value
    expect(result.asNumber()?.value).toBe(20);
  });

  it('should access closure variables with ClosureRefInsn', () => {
    const vm = new VM();

    // Set up a closure with captured variables
    vm.closure = [makeNumber(100), makeNumber(200), makeNumber(300)];

    // Access second closure variable (index 1)
    const closureRef = new ClosureRefInsn(1, null);
    const result = vm.eval(closureRef);

    expect(result.asNumber()?.value).toBe(200);
  });
});
