/**
 * Instruction system - Direct port from OpenJade style/Insn.h
 *
 * OpenJade compiles Scheme expressions into bytecode instructions
 * for efficient evaluation. Each instruction returns the next instruction
 * to execute, forming a linked chain.
 */

import type { ELObj } from './elobj.js';
import type { VM } from './vm.js';
import { FunctionObj, type Signature } from './elobj.js';

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
 * Pop instruction - Pop top value from stack
 * Port from: Insn.h PopInsn
 */
export class PopInsn extends Insn {
  constructor(private next: Insn | null) {
    super();
  }

  execute(vm: VM): Insn | null {
    vm.pop();
    return this.next;
  }
}

/**
 * Cons instruction - Create a pair from top two stack values
 * Port from: Insn.h ConsInsn
 */
export class ConsInsn extends Insn {
  constructor(private next: Insn | null) {
    super();
  }

  execute(vm: VM): Insn | null {
    const cdr = vm.pop();
    const car = vm.pop();
    const pair = vm.makePair(car, cdr);
    vm.push(pair);
    return this.next;
  }
}

/**
 * Case instruction - Pattern matching for case expression
 * Port from: Insn.h CaseInsn
 */
export class CaseInsn extends Insn {
  constructor(
    private obj: ELObj,
    private match: Insn | null,
    private fail: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.peek();
    if (this.eqv(value, this.obj)) {
      vm.pop(); // Pop the matched value
      return this.match;
    }
    return this.fail;
  }

  /**
   * R4RS eqv? predicate
   * Port from: ELObj.h ELObj::eqv()
   */
  private eqv(obj1: ELObj, obj2: ELObj): boolean {
    // Identity check
    if (obj1 === obj2) return true;

    // Type-specific equivalence
    const num1 = obj1.asNumber();
    const num2 = obj2.asNumber();
    if (num1 && num2) {
      return num1.exact === num2.exact && num1.value === num2.value;
    }

    const bool1 = obj1.asBoolean();
    const bool2 = obj2.asBoolean();
    if (bool1 && bool2) {
      return bool1.value === bool2.value;
    }

    const sym1 = obj1.asSymbol();
    const sym2 = obj2.asSymbol();
    if (sym1 && sym2) {
      return sym1.name === sym2.name;
    }

    // Characters would go here when implemented

    return false;
  }
}

/**
 * FrameRef instruction - Access variable from current stack frame
 * Port from: Insn.h FrameRefInsn
 *
 * Accesses function arguments: frame[index]
 */
export class FrameRefInsn extends Insn {
  constructor(
    private index: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.getFrame(this.index);
    vm.push(value);
    return this.next;
  }
}

/**
 * StackRef instruction - Access variable relative to stack pointer
 * Port from: Insn.h StackRefInsn
 *
 * Accesses stack at: sp[index] where index is negative
 */
export class StackRefInsn extends Insn {
  constructor(
    private index: number,        // negative offset from sp
    private frameIndex: number,   // for debugging/validation
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.getStackRelative(this.index);
    vm.push(value);
    return this.next;
  }
}

/**
 * ClosureRef instruction - Access captured variable from closure
 * Port from: Insn.h ClosureRefInsn
 *
 * Accesses closure[index]
 */
export class ClosureRefInsn extends Insn {
  constructor(
    private index: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.getClosure(this.index);
    vm.push(value);
    return this.next;
  }
}

/**
 * TestNull instruction - Branch based on whether value is null/undefined
 * Port from: Insn.h TestNullInsn
 *
 * Tests stack[offset] for null
 */
export class TestNullInsn extends Insn {
  constructor(
    private offset: number,
    private ifNull: Insn | null,
    private ifNotNull: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.getStackValue(this.offset);
    // In Scheme/DSSSL, we test for nil (empty list) not JavaScript null
    if (value.asNil()) {
      return this.ifNull;
    }
    return this.ifNotNull;
  }
}

/**
 * Return instruction - Return from function call
 * Port from: Insn.h ReturnInsn
 *
 * Pops result from stack, removes function arguments, restores caller frame,
 * and pushes result back on stack.
 */
export class ReturnInsn extends Insn {
  constructor(private totalArgs: number) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Pop the return value
    const result = vm.pop();

    // Remove function arguments from stack
    for (let i = 0; i < this.totalArgs; i++) {
      vm.pop();
    }

    // Pop the call frame and get the return address
    const next = vm.popFrame();

    // Push result back on stack for caller
    vm.push(result);

    return next;
  }

  isReturn(): { isReturn: false } | { isReturn: true; nArgs: number } {
    return { isReturn: true, nArgs: this.totalArgs };
  }
}

/**
 * PrimitiveCall instruction - Call a primitive function
 * Port from: Insn.h PrimitiveCallInsn
 *
 * Calls a primitive with nArgs arguments from the stack.
 * Arguments are replaced with the result.
 */
export class PrimitiveCallInsn extends Insn {
  constructor(
    private nArgs: number,
    private primitive: ELObj, // Must be FunctionObj with primitive
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Get the function object
    const func = this.primitive.asFunction();
    if (!func || !func.isPrimitive()) {
      throw new Error('PrimitiveCallInsn requires a primitive function');
    }

    // Gather arguments from stack (they're at sp - nArgs .. sp - 1)
    const args: ELObj[] = [];
    for (let i = this.nArgs - 1; i >= 0; i--) {
      args.unshift(vm.getStackValue(vm.stackSize() - this.nArgs + i));
    }

    // Call the primitive with VM context
    const result = func.callPrimitive(args, vm);

    // Pop all arguments
    for (let i = 0; i < this.nArgs; i++) {
      vm.pop();
    }

    // Push result
    vm.push(result);

    return this.next;
  }
}

/**
 * Generic call instruction - Call any function (primitive or closure)
 * Port from: Insn.h CallInsn
 *
 * Stack layout on entry:
 * [arg1, arg2, ..., argN, function]
 *
 * This instruction:
 * 1. Pops function from stack
 * 2. If primitive: calls it directly
 * 3. If closure: sets up frame and executes body
 */
export class CallInsn extends Insn {
  constructor(
    private nArgs: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Pop function from top of stack
    const funcObj = vm.pop();
    const func = funcObj.asFunction();

    if (!func) {
      throw new Error('Cannot call non-function');
    }

    // Gather arguments from stack (they're at sp - nArgs .. sp - 1)
    const args: ELObj[] = [];
    for (let i = this.nArgs - 1; i >= 0; i--) {
      args.unshift(vm.getStackValue(vm.stackSize() - this.nArgs + i));
    }

    // Handle primitive functions
    if (func.isPrimitive()) {
      // Pop arguments
      for (let i = 0; i < this.nArgs; i++) {
        vm.pop();
      }

      // Call primitive with VM context
      const result = func.callPrimitive(args, vm);

      // Push result
      vm.push(result);

      return this.next;
    }

    // Handle closures
    if (func.isClosure()) {
      // Validate argument count
      const sig = func.signature!;
      if (this.nArgs !== sig.nRequiredArgs + sig.nOptionalArgs && !sig.restArg) {
        throw new Error(`Function expects ${sig.nRequiredArgs} arguments, got ${this.nArgs}`);
      }

      // Set up frame for closure call
      // Arguments are already on the stack in the right order
      vm.frameIndex = vm.stackSize() - this.nArgs;
      vm.nActualArgs = this.nArgs;

      // Set closure display
      vm.closure = func.display || null;

      // Push call frame
      vm.pushFrame(this.next, this.nArgs);

      // Execute closure body
      // The closure's code will end with a ReturnInsn that pops the frame
      return func.code;
    }

    throw new Error('Function is neither primitive nor closure');
  }
}

/**
 * Closure instruction - Create a closure (lambda function)
 * Port from: Insn.h ClosureInsn
 *
 * Creates a closure by capturing variables from the stack.
 * The top displayLength values on the stack become the closure's display
 * (captured environment).
 */
export class ClosureInsn extends Insn {
  constructor(
    private signature: Signature,
    private code: Insn | null,
    private displayLength: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Capture variables from stack for the closure's display
    const display: ELObj[] = [];
    for (let i = 0; i < this.displayLength; i++) {
      // Get values from bottom of the display region to top
      display.push(vm.getStackValue(vm.stackSize() - this.displayLength + i));
    }

    // Create the closure object
    const closure = new FunctionObj(
      null, // anonymous lambda
      undefined, // not a primitive
      this.code, // bytecode to execute
      this.signature, // function signature
      display // captured variables
    );

    // Pop the display values from stack
    for (let i = 0; i < this.displayLength; i++) {
      vm.pop();
    }

    // Push the closure onto the stack
    vm.push(closure);

    return this.next;
  }
}

/**
 * StackSet instruction - Mutate stack variable
 * Port from: Insn.h StackSetInsn
 *
 * Sets a stack variable to the value on top of stack.
 * The old value is returned (left on stack).
 */
export class StackSetInsn extends Insn {
  constructor(
    private index: number,        // negative offset from sp
    private frameIndex: number,   // for debugging/validation
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Get the old value at the target location
    const oldValue = vm.getStackRelative(this.index);

    // Get the new value from top of stack
    const newValue = vm.peek();

    // Set the target location to the new value
    vm.setStackRelative(this.index, newValue);

    // Leave the old value on stack (this is what OpenJade does)
    // Actually, pop new value and push old value
    vm.pop();
    vm.push(oldValue);

    return this.next;
  }
}

/**
 * ClosureSet instruction - Mutate closure variable
 * Port from: Insn.h (simplified without Box)
 *
 * Sets a closure variable to the value on top of stack.
 * The old value is returned (left on stack).
 */
export class ClosureSetInsn extends Insn {
  constructor(
    private index: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Get the old value from closure
    const oldValue = vm.getClosure(this.index);

    // Get the new value from top of stack
    const newValue = vm.peek();

    // Set the closure variable
    vm.setClosure(this.index, newValue);

    // Leave the old value on stack
    vm.pop();
    vm.push(oldValue);

    return this.next;
  }
}

/**
 * PopBindings instruction - Pop N bindings while preserving result
 * Port from: Insn.h PopBindingsInsn
 *
 * Stack before: [binding1, binding2, ..., bindingN, result]
 * Stack after: [result]
 */
export class PopBindingsInsn extends Insn {
  constructor(
    private nBindings: number,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Pop the result first
    const result = vm.pop();

    // Pop all the bindings
    for (let i = 0; i < this.nBindings; i++) {
      vm.pop();
    }

    // Push result back
    vm.push(result);

    return this.next;
  }

  isPopBindings(): { is: false } | { is: true; nBindings: number; next: Insn | null } {
    return { is: true, nBindings: this.nBindings, next: this.next };
  }
}

// More instruction types will be added as we implement the compiler:
// - AppendInsn (list append - more complex, will add when needed)
// - FunctionCallInsn (user-defined function calls)
// - ApplyInsn (generic apply)
// - DefineInsn (define)
// - etc.
