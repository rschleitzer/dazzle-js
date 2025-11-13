/**
 * Instruction system - Direct port from OpenJade style/Insn.h
 *
 * OpenJade compiles Scheme expressions into bytecode instructions
 * for efficient evaluation. Each instruction returns the next instruction
 * to execute, forming a linked chain.
 */
import type { ELObj } from './elobj.js';
import type { VM } from './vm.js';
import { type Signature } from './elobj.js';
/**
 * Base class for all instructions
 * Port from: Insn.h class Insn
 */
export declare abstract class Insn {
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
    isReturn(): {
        isReturn: false;
    } | {
        isReturn: true;
        nArgs: number;
    };
}
/**
 * Constant instruction - Push a constant value onto stack
 * Port from: Insn.h ConstantInsn
 */
export declare class ConstantInsn extends Insn {
    private value;
    private next;
    constructor(value: ELObj, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Test instruction - Conditional branch (if expression)
 * Port from: Insn.h TestInsn
 */
export declare class TestInsn extends Insn {
    private consequent;
    private alternative;
    constructor(consequent: Insn | null, alternative: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Or instruction - Short-circuit OR
 * Port from: Insn.h OrInsn
 */
export declare class OrInsn extends Insn {
    private nextTest;
    private next;
    constructor(nextTest: Insn | null, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * And instruction - Short-circuit AND
 * Port from: Insn.h AndInsn
 */
export declare class AndInsn extends Insn {
    private nextTest;
    private next;
    constructor(nextTest: Insn | null, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Pop instruction - Pop top value from stack
 * Port from: Insn.h PopInsn
 */
export declare class PopInsn extends Insn {
    private next;
    constructor(next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Cons instruction - Create a pair from top two stack values
 * Port from: Insn.h ConsInsn
 */
export declare class ConsInsn extends Insn {
    private next;
    constructor(next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Case instruction - Pattern matching for case expression
 * Port from: Insn.h CaseInsn
 */
export declare class CaseInsn extends Insn {
    private obj;
    private match;
    private fail;
    constructor(obj: ELObj, match: Insn | null, fail: Insn | null);
    execute(vm: VM): Insn | null;
    /**
     * R4RS eqv? predicate
     * Port from: ELObj.h ELObj::eqv()
     */
    private eqv;
}
/**
 * FrameRef instruction - Access variable from current stack frame
 * Port from: Insn.h FrameRefInsn
 *
 * Accesses function arguments: frame[index]
 */
export declare class FrameRefInsn extends Insn {
    private index;
    private next;
    constructor(index: number, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * StackRef instruction - Access variable relative to stack pointer
 * Port from: Insn.h StackRefInsn
 *
 * Accesses stack at: sp[index] where index is negative
 */
export declare class StackRefInsn extends Insn {
    private index;
    private frameIndex;
    private next;
    constructor(index: number, // negative offset from sp
    frameIndex: number, // for debugging/validation
    next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * ClosureRef instruction - Access captured variable from closure
 * Port from: Insn.h ClosureRefInsn
 *
 * Accesses closure[index]
 */
export declare class ClosureRefInsn extends Insn {
    private index;
    private next;
    constructor(index: number, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * TestNull instruction - Branch based on whether value is null/undefined
 * Port from: Insn.h TestNullInsn
 *
 * Tests stack[offset] for null
 */
export declare class TestNullInsn extends Insn {
    private offset;
    private ifNull;
    private ifNotNull;
    constructor(offset: number, ifNull: Insn | null, ifNotNull: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Return instruction - Return from function call
 * Port from: Insn.h ReturnInsn
 *
 * Pops result from stack, removes function arguments, restores caller frame,
 * and pushes result back on stack.
 */
export declare class ReturnInsn extends Insn {
    private totalArgs;
    constructor(totalArgs: number);
    execute(vm: VM): Insn | null;
    isReturn(): {
        isReturn: false;
    } | {
        isReturn: true;
        nArgs: number;
    };
}
/**
 * PrimitiveCall instruction - Call a primitive function
 * Port from: Insn.h PrimitiveCallInsn
 *
 * Calls a primitive with nArgs arguments from the stack.
 * Arguments are replaced with the result.
 */
export declare class PrimitiveCallInsn extends Insn {
    private nArgs;
    private primitive;
    private next;
    constructor(nArgs: number, primitive: ELObj, // Must be FunctionObj with primitive
    next: Insn | null);
    execute(vm: VM): Insn | null;
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
export declare class CallInsn extends Insn {
    private nArgs;
    private next;
    constructor(nArgs: number, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * Closure instruction - Create a closure (lambda function)
 * Port from: Insn.h ClosureInsn
 *
 * Creates a closure by capturing variables from the stack.
 * The top displayLength values on the stack become the closure's display
 * (captured environment).
 */
export declare class ClosureInsn extends Insn {
    private signature;
    private code;
    private displayLength;
    private next;
    constructor(signature: Signature, code: Insn | null, displayLength: number, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * StackSet instruction - Mutate stack variable
 * Port from: Insn.h StackSetInsn
 *
 * Sets a stack variable to the value on top of stack.
 * The old value is returned (left on stack).
 */
export declare class StackSetInsn extends Insn {
    private index;
    private frameIndex;
    private next;
    constructor(index: number, // negative offset from sp
    frameIndex: number, // for debugging/validation
    next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * ClosureSet instruction - Mutate closure variable
 * Port from: Insn.h (simplified without Box)
 *
 * Sets a closure variable to the value on top of stack.
 * The old value is returned (left on stack).
 */
export declare class ClosureSetInsn extends Insn {
    private index;
    private next;
    constructor(index: number, next: Insn | null);
    execute(vm: VM): Insn | null;
}
/**
 * PopBindings instruction - Pop N bindings while preserving result
 * Port from: Insn.h PopBindingsInsn
 *
 * Stack before: [binding1, binding2, ..., bindingN, result]
 * Stack after: [result]
 */
export declare class PopBindingsInsn extends Insn {
    private nBindings;
    private next;
    constructor(nBindings: number, next: Insn | null);
    execute(vm: VM): Insn | null;
    isPopBindings(): {
        is: false;
    } | {
        is: true;
        nBindings: number;
        next: Insn | null;
    };
}
//# sourceMappingURL=insn.d.ts.map