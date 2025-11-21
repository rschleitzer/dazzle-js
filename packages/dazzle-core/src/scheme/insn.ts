/**
 * Instruction system - Direct port from OpenJade style/Insn.h
 *
 * OpenJade compiles Scheme expressions into bytecode instructions
 * for efficient evaluation. Each instruction returns the next instruction
 * to execute, forming a linked chain.
 */

import type { ELObj } from './elobj.js';
import type { VM } from './vm.js';
import { FunctionObj, type Signature, theNilObj, theFalseObj, makePair, makeSymbol } from './elobj.js';
import { createFlowObj, CompoundFlowObj, FlowObj } from '../flowobj.js';

/**
 * Source location information
 * Port from: OpenJade Location.h struct Location
 */
export interface Location {
  file: string;
  line: number;
  column: number;
}

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
    const result = test.isTrue() ? this.consequent : this.alternative;
    if (process.env.DEBUG_FOT) {
      console.error(`[TestInsn] test=${test.isTrue()}, returning ${result?.constructor.name || 'null'}, stackSize=${vm.stackSize()}`);
    }
    return result;
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
   *
   * Extended for DSSSL: Strings are compared by content for case matching
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

    // DSSSL extension: Compare strings by content for case matching
    // Port from: OpenJade behavior - enables case expressions with string literals
    const str1 = obj1.asString();
    const str2 = obj2.asString();
    if (str1 && str2) {
      return str1.value === str2.value;
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
    private next: Insn | null,
    private forCapture: boolean = false // true if capturing for closure
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    let value = vm.getFrame(this.index);

    // Auto-unbox if this is a boxed value (for letrec variables)
    // BUT don't unbox if we're capturing for a closure - closures need the box itself
    // Port from: OpenJade - frame refs auto-unbox, but closures capture boxes
    if (!this.forCapture && value && typeof value.asBox === 'function') {
      const box = value.asBox();
      if (box) {
        value = box.value;
      }
    }

    if (process.env.DEBUG_FOT && this.next?.constructor.name === 'CallInsn') {
      console.error(`[FrameRefInsn] index=${this.index}, value=${value.constructor.name}, next=CallInsn, stackSize=${vm.stackSize()}`);
    }

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
    private next: Insn | null,
    private forCapture: boolean = false, // true if capturing for closure
    private varName?: string  // for debugging
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Port from: OpenJade Insn.cxx StackRefInsn::execute
    // ASSERT(vm.sp - vm.frame == frameIndex_ - index_);
    // *vm.sp = vm.sp[index_];
    //
    // OpenJade uses frame-relative addressing:
    // - index is negative offset from current sp
    // - frameIndex is positive offset from frame base
    // - vm.sp[index] is equivalent to vm.frame[frameIndex]
    //
    // Use frame-relative access for correctness across different call depths
    if (process.env.DEBUG_INSN) {
      console.error(`[StackRefInsn] Accessing frame[${this.frameIndex}], frameIndex=${vm.frameIndex}`);
    }
    let value = vm.getFrame(this.frameIndex);

    // Debug: Track stack reads
    if (process.env.DEBUG_CLOSURES) {
      const sosofo = value?.asSosofo?.();
      const isFlowObj = sosofo && (sosofo.type === undefined || sosofo.type === null);

      // Log all stack reads, with extra detail for FlowObjs
      if (isFlowObj || (globalThis as any)._debugStackReads) {
        console.error(`[StackRefInsn] Reading from frame[${this.frameIndex}]${this.varName ? ` (var: ${this.varName})` : ''}`);
        console.error(`  this.index (offset): ${this.index}`);
        console.error(`  this.frameIndex: ${this.frameIndex}`);
        console.error(`  Value type: ${value?.constructor.name || 'null'}`);
        console.error(`  vm.frameIndex: ${vm.frameIndex}`);
        console.error(`  vm.stackSize: ${vm.stackSize()}`);
        if (isFlowObj) {
          console.error(`  *** FLOW OBJECT ***`);
        }

        // Count reads but keep logging (removed auto-shutoff)
        (globalThis as any)._debugStackReadCount = ((globalThis as any)._debugStackReadCount || 0) + 1;
      }
    }

    // Auto-unbox if this is a boxed value (for letrec variables)
    // Port from: OpenJade - stack refs auto-unbox
    // CRITICAL: Do NOT unbox if forCapture=true (for closure capture)
    // When capturing for closure, we need to capture the box itself,
    // not the value inside it, so that updates to the box are visible
    if (!this.forCapture && value && typeof value.asBox === 'function') {
      const box = value.asBox();
      if (box) {
        value = box.value;
      }
    }

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
    let value = vm.getClosure(this.index);

    // Safety check: ensure we have a valid ELObj
    if (!value || typeof value.asBox !== 'function') {
      // Debug: inspect what's in the closure
      console.error(`ClosureRefInsn: Invalid value at closure index ${this.index}`);
      console.error(`  Type: ${typeof value}`);
      console.error(`  Constructor: ${value?.constructor?.name || 'unknown'}`);
      console.error(`  Keys:`, Object.keys(value || {}));
      console.error(`  Has asBox:`, typeof value?.asBox);
      console.error(`  Value:`, value);
      throw new Error(`ClosureRefInsn: Invalid value in closure at index ${this.index}: ${typeof value} ${value?.constructor?.name || 'unknown'}`);
    }

    // Auto-unbox if this is a boxed value (for letrec variables)
    const box = value.asBox();
    if (box) {
      value = box.value;
    }

    // Debug: Track if we're reading a FlowObj from closure
    if (process.env.DEBUG_CLOSURES) {
      const sosofo = value?.asSosofo?.();
      if (sosofo && (sosofo.type === undefined || sosofo.type === null)) {
        console.error(`ClosureRefInsn: Reading FLOW OBJECT from closure[${this.index}]`);
        console.error(`  Value type: ${value.constructor.name}`);
        console.error(`  vm.currentNode: ${vm.currentNode ? vm.currentNode.gi() : 'null'}`);
      }
    }

    vm.push(value);
    return this.next;
  }
}

/**
 * GlobalRef instruction - Lookup global variable by name at runtime
 * Port from: OpenJade pattern for deferred global lookup
 *
 * This allows forward references to globals that will be defined later.
 */
export class GlobalRefInsn extends Insn {
  constructor(
    private name: string,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    if (!vm.globals) {
      throw new Error(`GlobalRefInsn: no globals available for ${this.name}`);
    }

    const value = vm.globals.lookup(this.name);
    if (!value) {
      throw new Error(`Undefined variable: ${this.name}\n  location: ${vm.currentFile}:${vm.currentLine}`);
    }

    // Debug: Check if we're looking up ancestor-member and it's a FlowObj
    if (process.env.DEBUG_CLOSURES && this.name === 'ancestor-member') {
      console.error(`GlobalRefInsn: Looking up 'ancestor-member'`);
      console.error(`  Value type: ${value.constructor.name}`);
      const sosofo = value.asSosofo?.();
      if (sosofo) {
        console.error(`  ERROR: ancestor-member is a SOSOFO!`);
        console.error(`  Sosofo type: ${sosofo.type || 'FlowObj'}`);
      }
      const func = value.asFunction();
      if (func) {
        console.error(`  Is function: ${func.isClosure() ? 'closure' : func.isPrimitive() ? 'primitive' : 'other'}`);
      }
    }

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
    // DEBUG: Check stack before return
    const stackBefore = vm.stackSize();

    if (process.env.DEBUG_FOT) {
      console.error(`[ReturnInsn] Executing return, totalArgs=${this.totalArgs}, stackSize=${stackBefore}, frameIndex=${vm.frameIndex}`);
    }

    // Pop the return value
    const result = vm.pop();

    if (process.env.DEBUG_FOT) {
      console.error(`  Return value type: ${result.constructor.name}, will pop ${this.totalArgs} args`);
    }

    // Debug: Track if we're returning a sosofo
    if (process.env.DEBUG_CLOSURES) {
      // Check if we should print debug for this return (set by CallInsn)
      if ((globalThis as any)._debugNextReturn) {
        console.error(`[ReturnInsn] Return value type: ${result.constructor.name}`);
        const sosofo = result.asSosofo();
        if (sosofo) {
          console.error(`  Is sosofo: YES (type: ${sosofo.type || 'FlowObj'})`);
        }
        (globalThis as any)._debugNextReturn = false;
      }

      const sosofo = result.asSosofo();
      if (sosofo) {
        // Only log FlowObj returns (not built-in sosofo types)
        const isFlowObj = sosofo.type === undefined || sosofo.type === null;
        if (isFlowObj) {
          console.error(`\n===== ReturnInsn: returning FLOW OBJECT from function =====`);
          console.error(`  Result type: ${result.constructor.name}`);
          console.error(`  Total args: ${this.totalArgs}`);
          console.error(`  vm.currentNode: ${vm.currentNode ? vm.currentNode.gi() : 'null'}`);
          console.error(`  Stack size before: ${stackBefore}`);
          console.error(`  Frame index: ${vm.frameIndex}`);
          console.error(`  Closure display depth: ${vm.closure ? 'has-closure' : 'no-closure'}`);
          // Get some stack trace context
          const error = new Error();
          const stack = error.stack?.split('\n').slice(2, 8).join('\n');
          console.error(`  Stack:\n${stack}`);
          console.error(`=============================================================\n`);
        }
      }
    }

    // Remove function arguments from stack
    for (let i = 0; i < this.totalArgs; i++) {
      vm.pop();
    }

    // Pop the call frame and get the return address
    const next = vm.popFrame();

    if (process.env.DEBUG_FOT) {
      console.error(`  Returning to: ${next ? next.constructor.name : 'null'}`);
    }

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
    private loc: Location,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Update VM location for error reporting and debug output
    // Port from: OpenJade PrimitiveCallInsn passes loc_ to primitiveCall
    vm.currentFile = this.loc.file;
    vm.currentLine = this.loc.line;
    vm.currentColumn = this.loc.column;

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

    // Debug: Track attribute-string primitive calls
    if (process.env.DEBUG_CLOSURES && func.name === 'attribute-string') {
      console.error(`\n[PrimitiveCallInsn] Calling attribute-string`);
      console.error(`  nArgs: ${this.nArgs}`);
      for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        const argType = arg.constructor.name;
        console.error(`  arg[${i}] type: ${argType}`);

        const argStr = arg.asString();
        const argSym = arg.asSymbol();
        const argNode = arg.asNode();
        const argSosofo = arg.asSosofo();

        if (argStr) {
          console.error(`    string: "${argStr.value}"`);
        } else if (argSym) {
          console.error(`    symbol: ${argSym.name}`);
        } else if (argNode) {
          console.error(`    node: gi="${argNode.node.gi()}"`);
        } else if (argSosofo) {
          console.error(`    sosofo: type="${argSosofo.type || 'FlowObj'}"`);
        }
      }
      console.error(`  vm.currentNode: ${vm.currentNode ? vm.currentNode.gi() : 'null'}`);
      console.error(`  vm.frameIndex: ${vm.frameIndex}, stackSize: ${vm.stackSize()}`);
      console.error(`  Stack contents:`);
      for (let i = Math.max(0, vm.stackSize() - 10); i < vm.stackSize(); i++) {
        const val = vm.getStackValue(i);
        console.error(`    stack[${i}]: ${val.constructor.name}`);
      }
    }

    // Call the primitive with VM context
    // Port from: OpenJade passes location to primitiveCall
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
    private loc: Location,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Update VM location for error reporting and debug output
    // Port from: OpenJade CallInsn has location
    vm.currentFile = this.loc.file;
    vm.currentLine = this.loc.line;
    vm.currentColumn = this.loc.column;

    // Pop function from top of stack
    const funcObj = vm.pop();
    const func = funcObj.asFunction();

    if (!func) {
      const str = funcObj.asString();
      const num = funcObj.asNumber();
      const bool = funcObj.asBoolean();
      const type = str ? `string "${str.value}"` :
                   num ? `number ${num.value}` :
                   bool !== null ? `boolean ${bool.value}` :
                   funcObj.constructor.name;

      // Add debug info to help trace the issue
      const stackInfo = vm.stackSize() > 0 ? `Stack size: ${vm.stackSize()}, top 3 items: ${
        [0, 1, 2].map(i => {
          if (vm.stackSize() > i) {
            const val = vm.getStackValue(vm.stackSize() - 1 - i);
            return val.constructor.name;
          }
          return 'N/A';
        }).join(', ')
      }` : 'Stack empty';

      throw new Error(`Cannot call non-function: got ${type} at ${this.loc.file}:${this.loc.line}. ${stackInfo}`);
    }

    // Gather arguments from stack (they're at sp - nArgs .. sp - 1)
    const args: ELObj[] = [];
    for (let i = this.nArgs - 1; i >= 0; i--) {
      args.unshift(vm.getStackValue(vm.stackSize() - this.nArgs + i));
    }

    // Handle primitive functions
    if (func.isPrimitive()) {
      // Debug: Track attribute-string calls
      if (process.env.DEBUG_CLOSURES) {
        // Check if this is attribute-string
        const isAttrStr = func.name === 'attribute-string';
        const arg1Type = args.length >= 2 ? args[1].constructor.name : 'N/A';
        const arg1IsSosofo = args.length >= 2 && args[1].asSosofo() !== null;

        if (isAttrStr || arg1IsSosofo) {
          console.error(`\n[CallInsn] Primitive: ${func.name}`);
          console.error(`  func.name === 'attribute-string': ${isAttrStr}`);
          console.error(`  nArgs: ${this.nArgs}`);
          if (args.length >= 2) {
            console.error(`  arg[1] type: ${arg1Type}, is sosofo: ${arg1IsSosofo}`);
          }
        }
      }

      if (process.env.DEBUG_CLOSURES && func.name === 'attribute-string') {
        console.error(`\n[CallInsn] Calling primitive: attribute-string`);
        console.error(`  arg[0] type: ${args[0].constructor.name}`);
        console.error(`  arg[1] type: ${args[1].constructor.name}`);
        const arg0Str = args[0].asString();
        const arg0Sym = args[0].asSymbol();
        if (arg0Str) {
          console.error(`  arg[0] string: "${arg0Str.value}"`);
        } else if (arg0Sym) {
          console.error(`  arg[0] symbol: ${arg0Sym.name}`);
        }
        const arg1Node = args[1].asNode();
        const arg1Sosofo = args[1].asSosofo();
        if (arg1Node) {
          console.error(`  arg[1] node: ${arg1Node.node.gi()}`);
        } else if (arg1Sosofo) {
          console.error(`  arg[1] sosofo: type=${arg1Sosofo.type || 'FlowObj'}`);
        }
        console.error(`  vm.currentNode: ${vm.currentNode ? vm.currentNode.gi() : 'null'}`);
        console.error(`  vm.frameIndex: ${vm.frameIndex}, stackSize: ${vm.stackSize()}`);
      }

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
      // Debug: Track what next instruction is being saved for FOT debugging
      if (process.env.DEBUG_FOT) {
        console.error(`[CallInsn] Calling closure: ${func.name || '<anonymous>'}, nArgs=${this.nArgs}, saving next=${this.next?.constructor.name || 'null'}, stackSize=${vm.stackSize()}`);
      }

      // Debug: Track function calls with 1 argument
      if (process.env.DEBUG_CLOSURES && this.nArgs === 1) {
        const arg = args[0];
        const argIsPair = arg?.asPair?.();
        // Only log when arg is a PairObj (the suspicious case)
        if (argIsPair) {
          // Build a string representation of the pair list
          let pairListStr = '';
          let current = argIsPair;
          let count = 0;
          const maxShow = 5;
          while (current && count < maxShow) {
            const car = current.car;
            const carStr = car?.asString?.();
            const carSym = car?.asSymbol?.();
            const carPair = car?.asPair?.();
            if (carStr) {
              pairListStr += `"${carStr.value}" `;
            } else if (carSym) {
              pairListStr += `${carSym.name} `;
            } else if (carPair) {
              // Nested pair - show first element
              const nestedCar = carPair.car;
              const nestedCarStr = nestedCar?.asString?.();
              const nestedCarSym = nestedCar?.asSymbol?.();
              if (nestedCarStr) {
                pairListStr += `("${nestedCarStr.value}" ...) `;
              } else if (nestedCarSym) {
                pairListStr += `(${nestedCarSym.name} ...) `;
              } else {
                pairListStr += `(${nestedCar?.constructor.name || 'null'} ...) `;
              }
            } else {
              pairListStr += `${car?.constructor.name || 'null'} `;
            }
            const cdr = current.cdr;
            const cdrPair = cdr?.asPair?.();
            if (!cdrPair) {
              break;
            }
            current = cdrPair;
            count++;
          }

          console.error(`\n[CallInsn] Calling closure with 1 argument (PairObj)`);
          console.error(`  Function name: ${func.name || '<anonymous>'}`);
          console.error(`  vm.currentNode: ${vm.currentNode ? vm.currentNode.gi() : 'null'}`);
          console.error(`  List elements: ${pairListStr}${count >= maxShow ? '...' : ''}`);

          // Mark that we want to see the return value and subsequent stack reads
          (globalThis as any)._debugNextReturn = true;
          (globalThis as any)._debugStackReads = true;
        }
      }

      // Port from: OpenJade Insn.cxx ClosureObj::call lines 755-764
      // Note: OpenJade doesn't validate argument count here - VarargsInsn handles it
      // IMPORTANT: Order matters! Must match OpenJade exactly:
      // 1. Set nActualArgs
      // 2. Push frame (saves previous frameIndex)
      // 3. Set new frameIndex
      // 4. Set closure display
      vm.nActualArgs = this.nArgs;

      // Push call frame (saves current frame state)
      vm.pushFrame(this.next, this.nArgs);

      // Set up frame for closure call (arguments are already on stack)
      vm.frameIndex = vm.stackSize() - this.nArgs;

      // Set closure display
      vm.closure = func.display || null;

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
      const value = vm.getStackValue(vm.stackSize() - this.displayLength + i);

      // Debug: Check if we're capturing a broken object
      if (!value || typeof value.asBox !== 'function') {
        console.error(`ClosureInsn: WARNING - capturing non-ELObj at display index ${i}`);
        console.error(`  Type: ${typeof value}`);
        console.error(`  Constructor: ${value?.constructor?.name}`);
        console.error(`  Keys:`, Object.keys(value || {}));
        console.error(`  Stack size: ${vm.stackSize()}, displayLength: ${this.displayLength}`);
      }

      display.push(value);
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
 * Varargs instruction - Handle variable-arity functions using entry points
 * Port from: OpenJade Insn.h VarargsInsn and Insn.cxx VarargsInsn::execute
 *
 * Uses pre-compiled entry points (instruction chains) for each arity.
 * Each entry point evaluates the necessary defaults and jumps to the body.
 * NO nested loops - just returns the appropriate entry point!
 *
 * Entry points array:
 * - entryPoints[0]: Only required args provided → eval all defaults → body
 * - entryPoints[1]: Required + 1 optional → eval remaining defaults → body
 * - entryPoints[n]: Required + n optionals → eval remaining defaults → body
 * - entryPoints[last]: All args provided (or rest args) → body
 */
export class VarargsInsn extends Insn {
  constructor(
    private signature: Signature,
    private entryPoints: Insn[]
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Port from: OpenJade Insn.cxx VarargsInsn::execute lines 689-735
    const nActual = vm.nActualArgs;
    const nRequired = this.signature.nRequiredArgs;
    const nOptional = this.signature.nOptionalArgs;

    // Calculate how many optional args were provided
    // n = number of args beyond required
    const n = nActual - nRequired;

    if (process.env.DEBUG_FOT) {
      console.error(`[VarargsInsn] nActualArgs=${nActual}, required=${nRequired}, optional=${nOptional}, n=${n}, entryPoints.length=${this.entryPoints.length}`);
    }

    // Port from: OpenJade Insn.cxx lines 692-733
    // Handle rest args case
    if ((this.signature.restArg || this.signature.nKeyArgs)
        && n > this.entryPoints.length - 2) {
      // Too many args - need to cons up rest list
      // Port from: OpenJade lines 694-702
      let restList: ELObj = theNilObj;
      const nRest = n - (this.entryPoints.length - 2);

      for (let i = 0; i < nRest; i++) {
        const arg = vm.pop();
        restList = makePair(arg, restList);
      }

      // Push rest list onto stack
      if (this.signature.restArg) {
        vm.push(restList);
      }

      if (process.env.DEBUG_FOT) {
        console.error(`  VarargsInsn: Packed ${nRest} rest args, returning last entry point`);
      }

      // Return last entry point (for rest args case)
      // Port from: OpenJade line 732
      return this.entryPoints[this.entryPoints.length - 1];
    }

    // Normal case: return entry point for this arity
    // Port from: OpenJade line 734
    if (process.env.DEBUG_FOT) {
      console.error(`  VarargsInsn: Returning entryPoints[${n}] (${nOptional - n} optionals missing), stackSize=${vm.stackSize()}`);
      if (n < this.entryPoints.length - 1) {
        console.error(`    Entry point ${n} will evaluate ${nOptional - n} default(s) then jump to body`);
      } else {
        console.error(`    Entry point ${n} is the body (all optionals provided)`);
      }
    }

    return this.entryPoints[n];
  }
}

/**
 * StackSet instruction - Mutate stack variable
 * Port from: Insn.h StackSetInsn
 *
 * Sets a stack variable to the value on top of stack.
 * The old value is returned (left on stack).
 */
/**
 * SetImmediateInsn - For letrec initialization
 * Port from: OpenJade Insn.h SetImmediateInsn
 *
 * Pops a value from stack and stores it at a relative offset.
 * Used by letrec to initialize recursive bindings.
 */
export class SetImmediateInsn extends Insn {
  constructor(
    private offset: number,  // positive offset from current stack position
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Port from: OpenJade Insn.cxx SetImmediateInsn::execute
    // --vm.sp;
    // vm.sp[-n_] = *vm.sp;
    //
    // Pop the value to store
    const value = vm.pop();

    // Get the target location (sp-relative, not absolute!)
    // OpenJade uses vm.sp[-n_] which is sp-relative addressing
    const target = vm.getStackRelative(-this.offset);

    // Check if target is a box (for letrec variables)
    // Port from: OpenJade SetBoxInsn
    const box = target.asBox();
    if (box) {
      // Update the box contents instead of replacing the box
      box.value = value;
    } else {
      // Normal case: replace the stack value
      vm.setStackRelative(-this.offset, value);
    }

    return this.next;
  }
}

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
    if (process.env.DEBUG_FOT) {
      console.error(`[PopBindingsInsn] nBindings=${this.nBindings}, stackSize=${vm.stackSize()}`);
      console.error(`  Stack before pop:`);
      for (let i = Math.max(0, vm.stackSize() - 5); i < vm.stackSize(); i++) {
        const val = vm.getStackValue(i);
        console.error(`    [${i}]: ${val.constructor.name}`);
      }
    }

    // Pop the result first
    const result = vm.pop();

    if (process.env.DEBUG_FOT) {
      console.error(`  Popped result: ${result.constructor.name}`);
    }

    // Pop all the bindings
    for (let i = 0; i < this.nBindings; i++) {
      const binding = vm.pop();
      if (process.env.DEBUG_FOT) {
        console.error(`  Popped binding ${i}: ${binding.constructor.name}`);
      }
    }

    // Push result back
    vm.push(result);

    if (process.env.DEBUG_FOT) {
      console.error(`  Pushed result back, stackSize=${vm.stackSize()}`);
      console.error(`  Stack after push:`);
      for (let i = Math.max(0, vm.stackSize() - 5); i < vm.stackSize(); i++) {
        const val = vm.getStackValue(i);
        console.error(`    [${i}]: ${val.constructor.name}`);
      }
    }

    return this.next;
  }

  isPopBindings(): { is: false } | { is: true; nBindings: number; next: Insn | null } {
    return { is: true, nBindings: this.nBindings, next: this.next };
  }
}

/**
 * Error instruction - throws an error
 * Used for case expressions without else clause when no clause matches
 */
/**
 * StoreUnitInsn - Pop quantity from stack and store in unit registry
 * Port from: OpenJade Interpreter.cxx Unit::tryCompute()
 *
 * This instruction is chained after the value expression in DefineUnitInsn.
 */
export class StoreUnitInsn extends Insn {
  constructor(
    private unitName: string,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Pop the result - can be a quantity or unresolved quantity
    const value = vm.pop();

    // Try to get a resolved quantity
    let quantity = value.asQuantity();

    // If it's an unresolved quantity, resolve it using the unit registry
    if (!quantity) {
      const unresolvedQuantity = value as any;
      if (unresolvedQuantity.resolve && typeof unresolvedQuantity.resolve === 'function') {
        // Port from: OpenJade UnresolvedQuantityObj::resolve()
        quantity = unresolvedQuantity.resolve(vm.unitRegistry);
      }
    }

    if (!quantity) {
      throw new Error(`define-unit: value must be a quantity, got ${value.constructor.name}`);
    }

    // Register the unit with the VM
    vm.defineUnit(this.unitName, quantity);

    // Push nil (define-unit returns nothing useful)
    vm.push(theNilObj);

    return this.next;
  }
}

/**
 * WithMode instruction - Execute expression with temporary processing mode
 * Port from: OpenJade Insn.h WithModeInsn (similar pattern)
 *
 * Syntax: (with-mode mode-name expr)
 * Temporarily sets the processing mode to mode-name while evaluating expr,
 * then restores the previous mode.
 */
/**
 * PushMode instruction - Push processing mode onto mode stack
 * Port from: OpenJade Insn2.h PushModeInsn
 */
export class PushModeInsn extends Insn {
  constructor(
    private modeName: string,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Push current mode onto mode stack
    vm.modeStack.push(vm.processingMode);

    // Set new mode
    vm.processingMode = this.modeName;

    if (process.env.DEBUG_PROCESS) {
      console.error(`[PushModeInsn] Set mode to '${this.modeName}', prev='${vm.modeStack[vm.modeStack.length - 1] || ''}'`);
    }

    return this.next;
  }
}

/**
 * PopMode instruction - Pop processing mode from mode stack
 * Port from: OpenJade Insn2.h PopModeInsn
 */
export class PopModeInsn extends Insn {
  constructor(
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const prevMode = vm.processingMode;
    // Restore previous mode from mode stack
    if (vm.modeStack.length > 0) {
      vm.processingMode = vm.modeStack.pop()!;
    }

    if (process.env.DEBUG_PROCESS) {
      console.error(`[PopModeInsn] Restored mode from '${prevMode}' to '${vm.processingMode}'`);
    }

    return this.next;
  }
}

export class ErrorInsn extends Insn {
  constructor(
    public message: string,
    public includeStackTop: boolean = false
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    if (this.includeStackTop) {
      // For case errors, show what value didn't match
      const value = vm.peek();
      const valueStr = value.asString()?.value ||
                       value.asSymbol()?.name ||
                       value.asBoolean()?.value.toString() ||
                       value.asNumber()?.value.toString() ||
                       value.constructor.name;
      throw new Error(`${this.message}: ${valueStr}`);
    }
    throw new Error(this.message);
  }
}

/**
 * DSSSL Flow Object Instructions
 * Port from: OpenJade Insn2.h
 *
 * These specialized instructions handle DSSSL make expressions.
 * Unlike the generic PrimitiveCallInsn approach, these instructions
 * build flow objects step-by-step, avoiding stack corruption issues
 * with nested make expressions.
 */

/**
 * Copy flow object instruction - Create a copy of a flow object template
 * Port from: Insn2.h CopyFlowObjInsn
 *
 * Pushes a copy of the flow object onto the stack.
 * The flow object template is created at compile time and stored in the instruction.
 */
export class CopyFlowObjInsn extends Insn {
  constructor(
    private flowObjType: string,  // e.g., 'sequence', 'simple-page-sequence'
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Create the flow object using the factory
    // Port from: OpenJade CopyFlowObjInsn::execute() - vm.sp++ = flowObj_->copy(*vm.interp)
    const flowObj = createFlowObj(this.flowObjType);

    if (!flowObj) {
      throw new Error(`Unknown flow object type: ${this.flowObjType}`);
    }

    // Push a copy of the flow object (OpenJade copies, not shares)
    vm.push(flowObj.copy());

    return this.next;
  }
}

/**
 * Check sosofo instruction - Verify that top of stack is a sosofo
 * Port from: Insn2.h CheckSosofoInsn
 *
 * Validates that the value on top of the stack is a sosofo (flow object).
 * Throws an error if not.
 */
export class CheckSosofoInsn extends Insn {
  constructor(
    private loc: Location,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    const value = vm.peek();
    const sosofo = value.asSosofo();
    if (process.env.DEBUG_FOT) {
      console.error(`CheckSosofoInsn at ${this.loc.file}:${this.loc.line}: value type=${value.constructor.name}, asSosofo=${sosofo ? 'yes' : 'no'}, stackSize=${vm.stackSize()}, peeking at stack[${vm.stackSize()-1}]`);
    }
    if (!sosofo) {
      vm.currentFile = this.loc.file;
      vm.currentLine = this.loc.line;
      vm.currentColumn = this.loc.column;
      throw new Error(`Expected sosofo, got ${value.constructor.name} at ${this.loc.file}:${this.loc.line}`);
    }
    return this.next;
  }
}

/**
 * Sosofo append instruction - Combine multiple sosofos into one
 * Port from: Insn2.h SosofoAppendInsn
 *
 * Pops n sosofos from the stack and combines them into a single sosofo.
 * Uses sosofo-append primitive.
 */
export class SosofoAppendInsn extends Insn {
  constructor(
    private n: number,  // Number of sosofos to append
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Gather n sosofos from top of stack
    const sosofos: ELObj[] = [];
    if (process.env.DEBUG_FOT) {
      console.error(`SosofoAppendInsn: n=${this.n}, stackSize=${vm.stackSize()}`);
      for (let i = 0; i < this.n; i++) {
        const idx = vm.stackSize() - this.n + i;
        const val = vm.getStackValue(idx);
        console.error(`  pos ${i}: stack[${idx}] = ${val.constructor.name}, asSosofo=${val.asSosofo() ? 'yes' : 'no'}`);
      }
    }
    for (let i = 0; i < this.n; i++) {
      const sosofo = vm.getStackValue(vm.stackSize() - this.n + i);
      if (!sosofo.asSosofo()) {
        if (process.env.DEBUG_FOT) {
          console.error(`\nDEBUG: SosofoAppendInsn found non-sosofo:`);
          console.error(`  Position: ${i} of ${this.n}`);
          console.error(`  Stack index: ${vm.stackSize() - this.n + i}`);
          console.error(`  Value type: ${sosofo.constructor.name}`);
          console.error(`  Stack dump:`);
          for (let j = 0; j < vm.stackSize(); j++) {
            const val = vm.getStackValue(j);
            console.error(`    [${j}]: ${val.constructor.name}`);
          }
        }
        throw new Error(`SosofoAppendInsn: expected sosofo at position ${i}, got ${sosofo.constructor.name}`);
      }
      sosofos.push(sosofo);
    }

    // Pop all sosofos except the first
    for (let i = 1; i < this.n; i++) {
      vm.pop();
    }

    // Call sosofo-append primitive
    const sosofoAppend = vm.globals.lookup('sosofo-append');
    if (!sosofoAppend) {
      throw new Error('sosofo-append primitive not found');
    }

    const func = sosofoAppend.asFunction();
    if (!func || !func.isPrimitive()) {
      throw new Error('sosofo-append must be a primitive function');
    }

    const result = func.callPrimitive(sosofos, vm);

    // Replace first sosofo with appended result
    vm.pop();
    vm.push(result);

    return this.next;
  }
}

/**
 * Set non-inherited characteristic instruction
 * Port from: Insn2.h SetNonInheritedCInsn
 *
 * Stack on entry: [flow-object, characteristic-value]
 * Pops value, sets characteristic on flow object, pushes flow object back.
 */
export class SetNonInheritedCInsn extends Insn {
  constructor(
    private characteristicName: string,
    private loc: Location,
    private next: Insn | null
  ) {
    super();
  }

  execute(vm: VM): Insn | null {
    // Stack: [flow-obj, char-value]
    const value = vm.pop();
    const flowObj = vm.peek();  // Peek, don't pop - we'll replace in place

    // Port from: OpenJade SetNonInheritedCInsn - calls flowObj->setNonInheritedC()
    if (!flowObj.asSosofo()) {
      vm.currentFile = this.loc.file;
      vm.currentLine = this.loc.line;
      vm.currentColumn = this.loc.column;
      throw new Error(`SetNonInheritedCInsn: expected flow object, got ${flowObj.constructor.name}`);
    }

    // Cast to FlowObj to access setNonInheritedC
    const flowObjInstance = flowObj as FlowObj;
    if (!(flowObj instanceof FlowObj)) {
      throw new Error(`SetNonInheritedCInsn: object is sosofo but not a FlowObj`);
    }

    // Set the characteristic
    flowObjInstance.setNonInheritedC(this.characteristicName, value);

    return this.next;
  }
}

/**
 * Set content instruction - Attach content sosofo to a flow object
 * Port from: Insn2.h SetContentInsn
 *
 * Stack on entry: [content-sosofo]
 * The flow object template is stored in the instruction (compile-time).
 * Pops content-sosofo, creates copy of template, sets content, pushes result.
 */
export class SetContentInsn extends Insn {
  private flowObjTemplate: CompoundFlowObj;

  constructor(
    private flowObjType: string,  // Type of the compound flow object
    private next: Insn | null
  ) {
    super();
    // Create the flow object template at compile time
    // Port from: OpenJade SetContentInsn stores flowObj_ member
    const template = createFlowObj(flowObjType);
    if (!template) {
      throw new Error(`Unknown flow object type: ${flowObjType}`);
    }
    if (!(template instanceof CompoundFlowObj)) {
      throw new Error(`SetContentInsn: ${flowObjType} is not a compound flow object`);
    }
    this.flowObjTemplate = template as CompoundFlowObj;
  }

  execute(vm: VM): Insn | null {
    // Stack: [content-sosofo]
    // Port from: OpenJade SetContentInsn::execute() lines 1333-1336
    // CompoundFlowObj *copy = (CompoundFlowObj *)flowObj_->copy(*vm.interp);
    // copy->setContent((SosofoObj *)vm.sp[-1]);
    // vm.sp[-1] = copy;

    const contentSosofo = vm.pop();  // Pop content from stack

    if (!contentSosofo.asSosofo()) {
      throw new Error(`SetContentInsn: expected content sosofo, got ${contentSosofo.constructor.name}`);
    }

    // Create a copy of the template flow object
    const copy = this.flowObjTemplate.copy() as CompoundFlowObj;
    const sosofo = contentSosofo.asSosofo();
    if (!sosofo) {
      throw new Error(`SetContentInsn: content is not a sosofo`);
    }
    copy.setContent(sosofo);

    // Push the completed flow object
    vm.push(copy);

    return this.next;
  }
}

// More instruction types will be added as we implement the compiler:
// - AppendInsn (list append - more complex, will add when needed)
// - FunctionCallInsn (user-defined function calls)
// - ApplyInsn (generic apply)
// - DefineInsn (define)
// - etc.
