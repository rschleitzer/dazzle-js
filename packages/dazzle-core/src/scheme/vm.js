/**
 * Virtual Machine - Direct port from OpenJade style/VM.h and Insn.cxx
 *
 * The VM executes bytecode instructions. It maintains:
 * - A value stack for computation
 * - A control stack for function calls
 * - A closure for lexical variable access
 */
import { makePair as makeELObjPair } from './elobj.js';
/**
 * VM class - Virtual machine for executing bytecode
 * Port from: VM.h class VM
 */
export class VM {
    /** Value stack pointer - points to next free slot */
    sp = 0;
    /** Value stack */
    stack = [];
    /** Stack limit */
    stackLimit = 10000;
    /**
     * Closure - for accessing lexical variables (captured from outer scope)
     * Port from: VM.h ELObj **closure
     */
    closure = null;
    /**
     * Frame pointer - points to current function's arguments on the stack
     * Port from: VM.h ELObj **frame
     */
    frameIndex = 0;
    /**
     * Number of actual arguments in current call
     * Port from: VM.h int nActualArgs
     */
    nActualArgs = 0;
    /**
     * Control stack - manages function call frames
     * Port from: VM.h ControlStackEntry *csp, *csbase, *cslim
     */
    controlStack = [];
    controlStackLimit = 1000;
    constructor() {
        this.initStack();
    }
    /**
     * Initialize the stack
     * Port from: Insn.cxx VM::initStack()
     */
    initStack() {
        this.sp = 0;
        this.stack = new Array(100); // Start with reasonable size
    }
    /**
     * Ensure stack has space for n more values
     * Port from: VM.h needStack()
     */
    needStack(n) {
        if (this.sp + n >= this.stack.length) {
            this.growStack(n);
        }
    }
    /**
     * Grow the stack
     * Port from: Insn.cxx VM::growStack()
     */
    growStack(n) {
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
    push(value) {
        this.needStack(1);
        this.stack[this.sp++] = value;
    }
    /**
     * Pop a value from the stack
     */
    pop() {
        if (this.sp <= 0) {
            throw new Error('Stack underflow');
        }
        return this.stack[--this.sp];
    }
    /**
     * Peek at top of stack without removing
     */
    peek() {
        if (this.sp <= 0) {
            throw new Error('Stack underflow');
        }
        return this.stack[this.sp - 1];
    }
    /**
     * Get current stack size
     */
    stackSize() {
        return this.sp;
    }
    /**
     * Create a pair (cons cell)
     * Port from: Interpreter.cxx Interpreter::makePair()
     */
    makePair(car, cdr) {
        return makeELObjPair(car, cdr);
    }
    /**
     * Access frame variable by index
     * Port from: VM.h vm.frame[index]
     */
    getFrame(index) {
        if (index < 0 || this.frameIndex + index >= this.sp) {
            throw new Error(`Frame index ${index} out of bounds`);
        }
        return this.stack[this.frameIndex + index];
    }
    /**
     * Access stack variable relative to sp (index is negative)
     * Port from: VM.h vm.sp[index]
     */
    getStackRelative(index) {
        if (index >= 0) {
            throw new Error(`Stack relative index must be negative, got ${index}`);
        }
        const absoluteIndex = this.sp + index;
        if (absoluteIndex < 0) {
            throw new Error(`Stack relative index ${index} out of bounds (stack size: ${this.sp})`);
        }
        return this.stack[absoluteIndex];
    }
    /**
     * Set stack variable relative to sp (index is negative)
     * Port from: VM.h vm.sp[index] = value
     */
    setStackRelative(index, value) {
        if (index >= 0) {
            throw new Error(`Stack relative index must be negative, got ${index}`);
        }
        const absoluteIndex = this.sp + index;
        if (absoluteIndex < 0) {
            throw new Error(`Stack relative index ${index} out of bounds (stack size: ${this.sp})`);
        }
        this.stack[absoluteIndex] = value;
    }
    /**
     * Access closure variable by index
     * Port from: VM.h vm.closure[index]
     */
    getClosure(index) {
        if (!this.closure) {
            throw new Error('No closure available');
        }
        if (index < 0 || index >= this.closure.length) {
            throw new Error(`Closure index ${index} out of bounds`);
        }
        return this.closure[index];
    }
    /**
     * Set closure variable by index
     * Port from: VM.h vm.closure[index] = value
     */
    setClosure(index, value) {
        if (!this.closure) {
            throw new Error('No closure available');
        }
        if (index < 0 || index >= this.closure.length) {
            throw new Error(`Closure index ${index} out of bounds`);
        }
        this.closure[index] = value;
    }
    /**
     * Access stack value by absolute index
     * Port from: VM.h vm.sp[offset] (where offset can be positive or negative)
     */
    getStackValue(offset) {
        const index = offset >= 0 ? offset : this.sp + offset;
        if (index < 0 || index >= this.sp) {
            throw new Error(`Stack index ${offset} out of bounds`);
        }
        return this.stack[index];
    }
    /**
     * Push a new call frame onto the control stack
     * Port from: Insn.cxx VM::pushFrame()
     *
     * @param next The instruction to return to after the call
     * @param argsPushed Number of arguments already pushed on stack
     */
    pushFrame(next, argsPushed) {
        if (this.controlStack.length >= this.controlStackLimit) {
            throw new Error('Control stack overflow');
        }
        // Calculate frame size (stack size before arguments were pushed)
        const frameSize = this.sp - this.frameIndex - argsPushed;
        // Push control stack entry
        this.controlStack.push({
            frameSize,
            closure: this.closure,
            next,
        });
    }
    /**
     * Pop a call frame from the control stack
     * Port from: Insn.cxx VM::popFrame()
     *
     * @returns The instruction to continue execution with
     */
    popFrame() {
        if (this.controlStack.length === 0) {
            throw new Error('Control stack underflow');
        }
        const entry = this.controlStack.pop();
        // Restore closure from saved frame
        this.closure = entry.closure;
        // Restore frame pointer
        this.frameIndex = this.sp - entry.frameSize;
        return entry.next;
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
    eval(insn, arg) {
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
//# sourceMappingURL=vm.js.map