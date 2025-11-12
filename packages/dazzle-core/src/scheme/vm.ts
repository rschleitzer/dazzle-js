/**
 * Virtual Machine - Direct port from OpenJade style/VM.h and Insn.cxx
 *
 * The VM executes bytecode instructions. It maintains:
 * - A value stack for computation
 * - A control stack for function calls
 * - A closure for lexical variable access
 */

import type { ELObj } from './elobj';
import type { Insn } from './insn';

/**
 * VM class - Virtual machine for executing bytecode
 * Port from: VM.h class VM
 */
export class VM {
  /** Value stack pointer - points to next free slot */
  private sp: number = 0;

  /** Value stack */
  private stack: ELObj[] = [];

  /** Stack limit */
  private stackLimit: number = 10000;

  /** Closure - for accessing lexical variables */
  public closure: ELObj[] | null = null;

  /** Frame pointer - for function arguments */
  public frame: number = 0;

  /** Number of actual arguments in current call */
  public nActualArgs: number = 0;

  constructor() {
    this.initStack();
  }

  /**
   * Initialize the stack
   * Port from: Insn.cxx VM::initStack()
   */
  private initStack(): void {
    this.sp = 0;
    this.stack = new Array(100); // Start with reasonable size
  }

  /**
   * Ensure stack has space for n more values
   * Port from: VM.h needStack()
   */
  private needStack(n: number): void {
    if (this.sp + n >= this.stack.length) {
      this.growStack(n);
    }
  }

  /**
   * Grow the stack
   * Port from: Insn.cxx VM::growStack()
   */
  private growStack(n: number): void {
    const newSize = Math.max(this.stack.length * 2, this.sp + n + 100);
    if (newSize > this.stackLimit) {
      throw new Error('Stack overflow');
    }
    const newStack = new Array(newSize);
    for (let i = 0; i < this.sp; i++) {
      newStack[i] = this.stack[i];
    }
    this.stack = newStack;
  }

  /**
   * Push a value onto the stack
   */
  push(value: ELObj): void {
    this.needStack(1);
    this.stack[this.sp++] = value;
  }

  /**
   * Pop a value from the stack
   */
  pop(): ELObj {
    if (this.sp <= 0) {
      throw new Error('Stack underflow');
    }
    return this.stack[--this.sp];
  }

  /**
   * Peek at top of stack without removing
   */
  peek(): ELObj {
    if (this.sp <= 0) {
      throw new Error('Stack underflow');
    }
    return this.stack[this.sp - 1];
  }

  /**
   * Get current stack size
   */
  stackSize(): number {
    return this.sp;
  }

  /**
   * Evaluate a bytecode instruction sequence
   * Port from: Insn.cxx VM::eval()
   *
   * This is the core execution loop:
   * ```cpp
   * while (insn)
   *   insn = insn->execute(*this);
   * ```
   *
   * @param insn Starting instruction
   * @param arg Optional initial argument to push on stack
   * @returns Result value
   */
  eval(insn: Insn | null, arg?: ELObj): ELObj {
    this.initStack();

    // Push initial argument if provided
    if (arg) {
      this.push(arg);
    }

    // The inner loop - keep executing instructions until done
    while (insn) {
      insn = insn.execute(this);
    }

    // Pop and return result
    if (this.sp > 0) {
      const result = this.pop();
      if (this.sp !== 0) {
        throw new Error('Stack not empty after evaluation');
      }
      return result;
    }

    throw new Error('No result on stack');
  }
}
