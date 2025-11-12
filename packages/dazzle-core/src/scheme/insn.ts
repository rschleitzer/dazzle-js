/**
 * Instruction system - Direct port from OpenJade style/Insn.h
 *
 * OpenJade compiles Scheme expressions into bytecode instructions
 * for efficient evaluation. Each instruction returns the next instruction
 * to execute, forming a linked chain.
 */

import type { ELObj } from './elobj';
import type { VM } from './vm';

/**
 * Base class for all instructions
 * Port from: Insn.h class Insn
 */
export abstract class Insn {
  /**
   * Execute this instruction
   * Port from: Insn.h virtual const Insn *execute(VM &) const = 0;
   *
   * @param vm The virtual machine
   * @returns The next instruction to execute, or null if done
   */
  abstract execute(vm: VM): Insn | null;

  /**
   * Check if this is a return instruction
   * Port from: Insn.h virtual bool isReturn(int &nArgs) const;
   */
  isReturn(): { isReturn: false } | { isReturn: true; nArgs: number } {
    return { isReturn: false };
  }
}

/**
 * Constant instruction - Push a constant value onto stack
 * Port from: Insn.h ConstantInsn
 */
export class ConstantInsn extends Insn {
  constructor(
    private value: ELObj,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    vm.push(this.value);
    return this.next;
  }
}

/**
 * Test instruction - Conditional branch (if expression)
 * Port from: Insn.h TestInsn
 */
export class TestInsn extends Insn {
  constructor(
    private consequent: Insn | null,
    private alternative: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const test = vm.pop();
    return test.isTrue() ? this.consequent : this.alternative;
  }
}

/**
 * Or instruction - Short-circuit OR
 * Port from: Insn.h OrInsn
 */
export class OrInsn extends Insn {
  constructor(
    private nextTest: Insn | null,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.peek(); // Don't pop yet
    if (value.isTrue()) {
      return this.next; // Short-circuit: value is already on stack
    }
    vm.pop(); // Pop the false value
    return this.nextTest; // Try next test
  }
}

/**
 * And instruction - Short-circuit AND
 * Port from: Insn.h AndInsn
 */
export class AndInsn extends Insn {
  constructor(
    private nextTest: Insn | null,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.peek(); // Don't pop yet
    if (!value.isTrue()) {
      return this.next; // Short-circuit: false value is already on stack
    }
    vm.pop(); // Pop the true value
    return this.nextTest; // Continue to next test
  }
}

/**
 * Case instruction - Pattern matching
 * Port from: Insn.h CaseInsn
 */
export class CaseInsn extends Insn {
  constructor(
    private _key: ELObj,
    private match: Insn | null,
    private fail: Insn | null
  ) {
    super();
  }

  execute(_vm: VM): Insn | null {
    // TODO: implement proper equality check between value on stack and key
    // For now, always fail
    return this.fail;
  }
}

// More instruction types will be added as we implement the compiler:
// - CallInsn (function calls)
// - LetInsn (let bindings)
// - DefineInsn (define)
// - LambdaInsn (lambda)
// - SetInsn (set!)
// - etc.
