/**
 * Scheme Compiler - Compile S-expressions to bytecode
 *
 * Port from: OpenJade style/Expression.cxx and Interpreter.cxx
 * Simplified version focusing on core R4RS Scheme features.
 */
import { type ELObj } from './elobj.js';
import { type Insn } from './insn.js';
/**
 * Variable binding in environment
 */
interface Binding {
    name: string;
    kind: 'frame' | 'stack' | 'closure';
    index: number;
}
/**
 * Compilation environment - tracks variable bindings
 * Port from: Interpreter.h class Environment
 */
export declare class Environment {
    private stackDepth;
    private bindings;
    /**
     * Create environment
     */
    constructor(stackDepth?: number);
    /**
     * Look up a variable
     */
    lookup(name: string): Binding | null;
    /**
     * Extend environment with new stack variables (for let)
     */
    extendStack(vars: string[]): Environment;
    /**
     * Create new environment for lambda body
     */
    enterLambda(lambdaParams: string[], capturedVars: string[]): Environment;
}
/**
 * Global environment - top-level bindings
 */
export declare class GlobalEnvironment {
    private bindings;
    constructor();
    define(name: string, value: ELObj): void;
    lookup(name: string): ELObj | null;
}
/**
 * Compiler - compiles S-expressions to bytecode
 */
export declare class Compiler {
    private globals;
    constructor(globals: GlobalEnvironment);
    /**
     * Compile an S-expression to bytecode
     *
     * @param expr - S-expression to compile
     * @param env - Compilation environment
     * @param stackPos - Current stack position (depth)
     * @param next - Next instruction to execute
     * @returns Head of instruction chain
     */
    compile(expr: ELObj, env: Environment, stackPos?: number, next?: Insn | null): Insn;
    /**
     * Compile variable reference
     */
    private compileVariable;
    /**
     * Compile quote special form
     */
    private compileQuote;
    /**
     * Compile if special form
     */
    private compileIf;
    /**
     * Compile lambda special form
     */
    private compileLambda;
    /**
     * Compile define special form (top-level only for now)
     */
    private compileDefine;
    /**
     * Compile set! special form
     */
    private compileSet;
    /**
     * Compile let special form
     */
    private compileLet;
    /**
     * Compile begin special form
     */
    private compileBegin;
    /**
     * Compile and special form
     */
    private compileAnd;
    /**
     * Compile or special form
     */
    private compileOr;
    /**
     * Compile function call
     */
    private compileCall;
    /**
     * Convert list to array
     */
    private listToArray;
    /**
     * Create begin form from array of expressions
     */
    private makeBegin;
}
export {};
//# sourceMappingURL=compiler.d.ts.map