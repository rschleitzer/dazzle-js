/**
 * VM tests - Basic bytecode execution
 */

import { describe, it, expect } from 'vitest';
import { VM } from './vm';
import { ConstantInsn, TestInsn } from './insn';
import { makeNumber, makeBoolean, theTrueObj, theFalseObj } from './elobj';

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
});
