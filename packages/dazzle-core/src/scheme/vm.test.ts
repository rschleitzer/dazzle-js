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

  it('should execute a sequence of constant instructions', () => {
    const vm = new VM();
    const insn2 = new ConstantInsn(makeNumber(10), null);
    const insn1 = new ConstantInsn(makeNumber(5), insn2);

    const result = vm.eval(insn1);

    // Last value should be on stack
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
