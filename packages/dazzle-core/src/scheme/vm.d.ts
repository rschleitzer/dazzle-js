/**
 * Virtual Machine - Direct port from OpenJade style/VM.h and Insn.cxx
 *
 * The VM executes bytecode instructions. It maintains:
 * - A value stack for computation
 * - A control stack for function calls
 * - A closure for lexical variable access
 */
import type { ELObj } from './elobj.js';
import type { Insn } from './insn.js';
/**
 * VM class - Virtual machine for executing bytecode
 * Port from: VM.h class VM
 */
export declare class VM {
    /** Value stack pointer - points to next free slot */
    private sp;
    /** Value stack */
    private stack;
    /** Stack limit */
    private stackLimit;
    /**
     * Closure - for accessing lexical variables (captured from outer scope)
     * Port from: VM.h ELObj **closure
     */
    closure: ELObj[] | null;
    /**
     * Frame pointer - points to current function's arguments on the stack
     * Port from: VM.h ELObj **frame
     */
    frameIndex: number;
    /**
     * Number of actual arguments in current call
     * Port from: VM.h int nActualArgs
     */
    nActualArgs: number;
    /**
     * Control stack - manages function call frames
     * Port from: VM.h ControlStackEntry *csp, *csbase, *cslim
     */
    private controlStack;
    private controlStackLimit;
    constructor();
    /**
     * Initialize the stack
     * Port from: Insn.cxx VM::initStack()
     */
    private initStack;
    /**
     * Ensure stack has space for n more values
     * Port from: VM.h needStack()
     */
    private needStack;
    /**
     * Grow the stack
     * Port from: Insn.cxx VM::growStack()
     */
    private growStack;
    /**
     * Push a value onto the stack
     */
    push(value: ELObj): void;
    /**
     * Pop a value from the stack
     */
    pop(): ELObj;
    /**
     * Peek at top of stack without removing
     */
    peek(): ELObj;
    /**
     * Get current stack size
     */
    stackSize(): number;
    /**
     * Create a pair (cons cell)
     * Port from: Interpreter.cxx Interpreter::makePair()
     */
    makePair(car: ELObj, cdr: ELObj): ELObj;
    /**
     * Access frame variable by index
     * Port from: VM.h vm.frame[index]
     */
    getFrame(index: number): ELObj;
    /**
     * Access stack variable relative to sp (index is negative)
     * Port from: VM.h vm.sp[index]
     */
    getStackRelative(index: number): ELObj;
    /**
     * Set stack variable relative to sp (index is negative)
     * Port from: VM.h vm.sp[index] = value
     */
    setStackRelative(index: number, value: ELObj): void;
    /**
     * Access closure variable by index
     * Port from: VM.h vm.closure[index]
     */
    getClosure(index: number): ELObj;
    /**
     * Set closure variable by index
     * Port from: VM.h vm.closure[index] = value
     */
    setClosure(index: number, value: ELObj): void;
    /**
     * Access stack value by absolute index
     * Port from: VM.h vm.sp[offset] (where offset can be positive or negative)
     */
    getStackValue(offset: number): ELObj;
    /**
     * Push a new call frame onto the control stack
     * Port from: Insn.cxx VM::pushFrame()
     *
     * @param next The instruction to return to after the call
     * @param argsPushed Number of arguments already pushed on stack
     */
    pushFrame(next: Insn | null, argsPushed: number): void;
    /**
     * Pop a call frame from the control stack
     * Port from: Insn.cxx VM::popFrame()
     *
     * @returns The instruction to continue execution with
     */
    popFrame(): Insn | null;
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
    eval(insn: Insn | null, arg?: ELObj): ELObj;
}
//# sourceMappingURL=vm.d.ts.map