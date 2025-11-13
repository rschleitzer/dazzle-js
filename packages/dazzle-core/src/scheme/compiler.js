/**
 * Scheme Compiler - Compile S-expressions to bytecode
 *
 * Port from: OpenJade style/Expression.cxx and Interpreter.cxx
 * Simplified version focusing on core R4RS Scheme features.
 */
import { PairObj, makeSymbol, theNilObj, theTrueObj, theFalseObj, } from './elobj.js';
import { ConstantInsn, TestInsn, OrInsn, AndInsn, PopInsn, PopBindingsInsn, FrameRefInsn, StackRefInsn, ClosureRefInsn, ReturnInsn, PrimitiveCallInsn, CallInsn, ClosureInsn, } from './insn.js';
import { standardPrimitives } from './primitives.js';
/**
 * Compilation environment - tracks variable bindings
 * Port from: Interpreter.h class Environment
 */
export class Environment {
    stackDepth;
    bindings = new Map();
    /**
     * Create environment
     */
    constructor(stackDepth = 0 // Current stack depth for let bindings
    ) {
        this.stackDepth = stackDepth;
    }
    /**
     * Look up a variable
     */
    lookup(name) {
        return this.bindings.get(name) || null;
    }
    /**
     * Extend environment with new stack variables (for let)
     */
    extendStack(vars) {
        const baseStackPos = this.stackDepth;
        const newEnv = new Environment(baseStackPos + vars.length);
        // Copy existing bindings unchanged (stack indices are absolute positions)
        this.bindings.forEach((binding, name) => {
            newEnv.bindings.set(name, binding);
        });
        // Add new stack variables at absolute positions
        // We push inits right-to-left, so vars[n-1] is at baseStackPos+0, vars[0] is at baseStackPos+n-1
        for (let i = 0; i < vars.length; i++) {
            newEnv.bindings.set(vars[i], {
                name: vars[i],
                kind: 'stack',
                index: baseStackPos + (vars.length - 1 - i), // Absolute stack position
            });
        }
        return newEnv;
    }
    /**
     * Create new environment for lambda body
     */
    enterLambda(lambdaParams, capturedVars) {
        const newEnv = new Environment(0);
        // Closure variables
        for (let i = 0; i < capturedVars.length; i++) {
            newEnv.bindings.set(capturedVars[i], {
                name: capturedVars[i],
                kind: 'closure',
                index: i,
            });
        }
        // Lambda parameters as frame variables
        for (let i = 0; i < lambdaParams.length; i++) {
            newEnv.bindings.set(lambdaParams[i], {
                name: lambdaParams[i],
                kind: 'frame',
                index: i,
            });
        }
        return newEnv;
    }
}
/**
 * Global environment - top-level bindings
 */
export class GlobalEnvironment {
    bindings = new Map();
    constructor() {
        // Install standard primitives
        for (const [name, func] of Object.entries(standardPrimitives)) {
            this.bindings.set(name, func);
        }
    }
    define(name, value) {
        this.bindings.set(name, value);
    }
    lookup(name) {
        return this.bindings.get(name) || null;
    }
}
/**
 * Compiler - compiles S-expressions to bytecode
 */
export class Compiler {
    globals;
    constructor(globals) {
        this.globals = globals;
    }
    /**
     * Compile an S-expression to bytecode
     *
     * @param expr - S-expression to compile
     * @param env - Compilation environment
     * @param stackPos - Current stack position (depth)
     * @param next - Next instruction to execute
     * @returns Head of instruction chain
     */
    compile(expr, env, stackPos = 0, next = null) {
        // Self-evaluating constants
        if (expr.asNumber() || expr.asString() || expr.asBoolean() || expr.asChar()) {
            return new ConstantInsn(expr, next);
        }
        // Nil is self-evaluating
        if (expr.asNil()) {
            return new ConstantInsn(theNilObj, next);
        }
        // Symbols are variable references
        const sym = expr.asSymbol();
        if (sym) {
            return this.compileVariable(sym.name, env, stackPos, next);
        }
        // Lists are either special forms or function calls
        const pair = expr.asPair();
        if (pair) {
            const car = pair.car;
            const carSym = car.asSymbol();
            if (carSym) {
                // Check for special forms
                switch (carSym.name) {
                    case 'quote':
                        return this.compileQuote(pair.cdr, env, stackPos, next);
                    case 'if':
                        return this.compileIf(pair.cdr, env, stackPos, next);
                    case 'lambda':
                        return this.compileLambda(pair.cdr, env, stackPos, next);
                    case 'define':
                        return this.compileDefine(pair.cdr, env, stackPos, next);
                    case 'set!':
                        return this.compileSet(pair.cdr, env, stackPos, next);
                    case 'let':
                        return this.compileLet(pair.cdr, env, stackPos, next);
                    case 'begin':
                        return this.compileBegin(pair.cdr, env, stackPos, next);
                    case 'and':
                        return this.compileAnd(pair.cdr, env, stackPos, next);
                    case 'or':
                        return this.compileOr(pair.cdr, env, stackPos, next);
                }
            }
            // Function call
            return this.compileCall(car, this.listToArray(pair.cdr), env, stackPos, next);
        }
        throw new Error(`Cannot compile expression: ${expr}`);
    }
    /**
     * Compile variable reference
     */
    compileVariable(name, env, stackPos, next) {
        const binding = env.lookup(name);
        if (binding) {
            // Local variable
            switch (binding.kind) {
                case 'frame':
                    return new FrameRefInsn(binding.index, next);
                case 'stack':
                    // Convert absolute position to relative offset from current stackPos
                    const offset = binding.index - stackPos;
                    return new StackRefInsn(offset, binding.index, next);
                case 'closure':
                    return new ClosureRefInsn(binding.index, next);
            }
        }
        // Global variable
        const value = this.globals.lookup(name);
        if (value) {
            return new ConstantInsn(value, next);
        }
        throw new Error(`Undefined variable: ${name}`);
    }
    /**
     * Compile quote special form
     */
    compileQuote(args, env, stackPos, next) {
        const argsArray = this.listToArray(args);
        if (argsArray.length !== 1) {
            throw new Error('quote requires exactly 1 argument');
        }
        return new ConstantInsn(argsArray[0], next);
    }
    /**
     * Compile if special form
     */
    compileIf(args, env, stackPos, next) {
        const argsArray = this.listToArray(args);
        if (argsArray.length < 2 || argsArray.length > 3) {
            throw new Error('if requires 2 or 3 arguments');
        }
        const test = argsArray[0];
        const consequent = argsArray[1];
        const alternative = argsArray[2] || theNilObj; // Unspecified if missing
        const conseqInsn = this.compile(consequent, env, stackPos, next);
        const altInsn = this.compile(alternative, env, stackPos, next);
        return this.compile(test, env, stackPos, new TestInsn(conseqInsn, altInsn));
    }
    /**
     * Compile lambda special form
     */
    compileLambda(args, env, stackPos, next) {
        const argsArray = this.listToArray(args);
        if (argsArray.length < 2) {
            throw new Error('lambda requires at least 2 arguments');
        }
        const params = this.listToArray(argsArray[0]);
        const paramNames = params.map((p) => {
            const sym = p.asSymbol();
            if (!sym) {
                throw new Error('lambda parameter must be a symbol');
            }
            return sym.name;
        });
        // Body is remaining arguments (implicit begin)
        const bodyExprs = argsArray.slice(1);
        const body = bodyExprs.length === 1
            ? bodyExprs[0]
            : this.makeBegin(bodyExprs);
        // TODO: Analyze which variables from outer scope are captured
        const capturedVars = [];
        const lambdaEnv = env.enterLambda(paramNames, capturedVars);
        const bodyInsn = this.compile(body, lambdaEnv, 0, new ReturnInsn(paramNames.length));
        const signature = {
            nRequiredArgs: paramNames.length,
            nOptionalArgs: 0,
            restArg: false,
            nKeyArgs: 0,
        };
        return new ClosureInsn(signature, bodyInsn, capturedVars.length, next);
    }
    /**
     * Compile define special form (top-level only for now)
     */
    compileDefine(args, env, stackPos, next) {
        const argsArray = this.listToArray(args);
        if (argsArray.length !== 2) {
            throw new Error('define requires exactly 2 arguments');
        }
        const nameExpr = argsArray[0];
        const sym = nameExpr.asSymbol();
        if (!sym) {
            throw new Error('define name must be a symbol');
        }
        // For now, just return unspecified (real define would update global env at runtime)
        throw new Error('define not yet implemented');
    }
    /**
     * Compile set! special form
     */
    compileSet(args, env, stackPos, next) {
        throw new Error('set! not yet implemented');
    }
    /**
     * Compile let special form
     */
    compileLet(args, env, stackPos, next) {
        const argsArray = this.listToArray(args);
        if (argsArray.length < 2) {
            throw new Error('let requires at least 2 arguments');
        }
        const bindings = this.listToArray(argsArray[0]);
        const vars = [];
        const inits = [];
        for (const binding of bindings) {
            const pair = binding.asPair();
            if (!pair) {
                throw new Error('let binding must be a list');
            }
            const bindingList = this.listToArray(binding);
            if (bindingList.length !== 2) {
                throw new Error('let binding must have exactly 2 elements');
            }
            const name = bindingList[0].asSymbol();
            if (!name) {
                throw new Error('let binding name must be a symbol');
            }
            vars.push(name.name);
            inits.push(bindingList[1]);
        }
        // Body is remaining arguments (implicit begin)
        const bodyExprs = argsArray.slice(1);
        const body = bodyExprs.length === 1
            ? bodyExprs[0]
            : this.makeBegin(bodyExprs);
        // Compile body with extended environment
        const bodyEnv = env.extendStack(vars);
        const bodyStackPos = stackPos + vars.length;
        let result = this.compile(body, bodyEnv, bodyStackPos, new PopBindingsInsn(vars.length, next));
        // Compile initializers - last init compiled executes first
        // We want to push right-to-left: inits[n-1], ..., inits[0]
        // So compile them left-to-right: inits[0], ..., inits[n-1]
        for (let i = 0; i < inits.length; i++) {
            // Init i will execute with (inits.length - 1 - i) values already on stack
            const numPushed = inits.length - 1 - i;
            result = this.compile(inits[i], env, stackPos + numPushed, result);
        }
        return result;
    }
    /**
     * Compile begin special form
     */
    compileBegin(args, env, stackPos, next) {
        const exprs = this.listToArray(args);
        if (exprs.length === 0) {
            return new ConstantInsn(theNilObj, next);
        }
        // Compile expressions from right to left
        let result = this.compile(exprs[exprs.length - 1], env, stackPos, next);
        for (let i = exprs.length - 2; i >= 0; i--) {
            result = this.compile(exprs[i], env, stackPos, new PopInsn(result));
        }
        return result;
    }
    /**
     * Compile and special form
     */
    compileAnd(args, env, stackPos, next) {
        const exprs = this.listToArray(args);
        if (exprs.length === 0) {
            return new ConstantInsn(theTrueObj, next);
        }
        if (exprs.length === 1) {
            return this.compile(exprs[0], env, stackPos, next);
        }
        // Compile from right to left
        let result = this.compile(exprs[exprs.length - 1], env, stackPos, next);
        for (let i = exprs.length - 2; i >= 0; i--) {
            result = this.compile(exprs[i], env, stackPos, new AndInsn(result, next));
        }
        return result;
    }
    /**
     * Compile or special form
     */
    compileOr(args, env, stackPos, next) {
        const exprs = this.listToArray(args);
        if (exprs.length === 0) {
            return new ConstantInsn(theFalseObj, next);
        }
        if (exprs.length === 1) {
            return this.compile(exprs[0], env, stackPos, next);
        }
        // Compile from right to left
        let result = this.compile(exprs[exprs.length - 1], env, stackPos, next);
        for (let i = exprs.length - 2; i >= 0; i--) {
            result = this.compile(exprs[i], env, stackPos, new OrInsn(result, next));
        }
        return result;
    }
    /**
     * Compile function call
     */
    compileCall(fn, args, env, stackPos, next) {
        // Check if fn is a known primitive at compile time
        const fnSym = fn.asSymbol();
        if (fnSym) {
            const primitive = this.globals.lookup(fnSym.name);
            if (primitive && primitive.asFunction()?.isPrimitive()) {
                // Compile as primitive call
                let result = new PrimitiveCallInsn(args.length, primitive, next);
                // Compile arguments - last arg compiled executes first
                // We want to evaluate left-to-right: args[0], ..., args[n-1]
                // So compile them right-to-left: args[n-1], ..., args[0]
                for (let i = args.length - 1; i >= 0; i--) {
                    // Arg i will execute with i values already on stack
                    const numPushed = i;
                    result = this.compile(args[i], env, stackPos + numPushed, result);
                }
                return result;
            }
        }
        // Generic function call
        // Stack layout: [arg1, arg2, ..., argN, function]
        // CallInsn will pop function, execute it with args, push result
        let result = new CallInsn(args.length, next);
        // Compile function expression - will be on top of stack after args
        result = this.compile(fn, env, stackPos + args.length, result);
        // Compile arguments right-to-left so they evaluate left-to-right
        for (let i = args.length - 1; i >= 0; i--) {
            result = this.compile(args[i], env, stackPos + i, result);
        }
        return result;
    }
    // ============ Helpers ============
    /**
     * Convert list to array
     */
    listToArray(list) {
        const result = [];
        let current = list;
        while (true) {
            if (current.asNil()) {
                break;
            }
            const pair = current.asPair();
            if (!pair) {
                throw new Error('Invalid list - not a proper list');
            }
            result.push(pair.car);
            current = pair.cdr;
        }
        return result;
    }
    /**
     * Create begin form from array of expressions
     */
    makeBegin(exprs) {
        if (exprs.length === 0) {
            return theNilObj;
        }
        if (exprs.length === 1) {
            return exprs[0];
        }
        // Build (begin expr1 expr2 ...)
        let list = theNilObj;
        for (let i = exprs.length - 1; i >= 0; i--) {
            list = new PairObj(exprs[i], list);
        }
        return new PairObj(makeSymbol('begin'), list);
    }
}
//# sourceMappingURL=compiler.js.map