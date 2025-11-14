/**
 * Scheme Compiler - Compile S-expressions to bytecode
 *
 * Port from: OpenJade style/Expression.cxx and Interpreter.cxx
 * Simplified version focusing on core R4RS Scheme features.
 */

import {
  type ELObj,
  type Signature,
  PairObj,
  makeSymbol,
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj.js';

import {
  type Insn,
  ConstantInsn,
  TestInsn,
  OrInsn,
  AndInsn,
  PopInsn,
  PopBindingsInsn,
  ConsInsn,
  FrameRefInsn,
  StackRefInsn,
  ClosureRefInsn,
  ReturnInsn,
  PrimitiveCallInsn,
  CallInsn,
  ClosureInsn,
} from './insn.js';

import { VM } from './vm.js';
import { standardPrimitives } from './primitives.js';
import { RuleRegistry } from '../dsssl/rules.js';

/**
 * Variable binding in environment
 */
interface Binding {
  name: string;
  kind: 'frame' | 'stack' | 'closure';  // location kind
  index: number;     // index (positive for frame, ABSOLUTE for stack, positive for closure)
}

/**
 * Compilation environment - tracks variable bindings
 * Port from: Interpreter.h class Environment
 */
export class Environment {
  private bindings: Map<string, Binding> = new Map();

  /**
   * Create environment
   */
  constructor(
    private stackDepth: number = 0  // Current stack depth for let bindings
  ) {}

  /**
   * Look up a variable
   */
  lookup(name: string): Binding | null {
    return this.bindings.get(name) || null;
  }

  /**
   * Extend environment with new stack variables (for let)
   */
  extendStack(vars: string[]): Environment {
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
        index: baseStackPos + (vars.length - 1 - i),  // Absolute stack position
      });
    }

    return newEnv;
  }

  /**
   * Create new environment for lambda body
   */
  enterLambda(lambdaParams: string[], capturedVars: string[]): Environment {
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
 * Global environment - top-level bindings and DSSSL rules
 */
export class GlobalEnvironment {
  private bindings: Map<string, ELObj> = new Map();
  public ruleRegistry: RuleRegistry = new RuleRegistry();

  constructor() {
    // Install standard primitives
    for (const [name, func] of Object.entries(standardPrimitives)) {
      this.bindings.set(name, func);
    }
  }

  define(name: string, value: ELObj): void {
    this.bindings.set(name, value);
  }

  lookup(name: string): ELObj | null {
    return this.bindings.get(name) || null;
  }
}

/**
 * Compiler - compiles S-expressions to bytecode
 */
export class Compiler {
  constructor(private globals: GlobalEnvironment) {}

  /**
   * Compile an S-expression to bytecode
   *
   * @param expr - S-expression to compile
   * @param env - Compilation environment
   * @param stackPos - Current stack position (depth)
   * @param next - Next instruction to execute
   * @returns Head of instruction chain
   */
  compile(expr: ELObj, env: Environment, stackPos: number = 0, next: Insn | null = null): Insn {
    // Self-evaluating constants
    if (expr.asNumber() || expr.asString() || expr.asBoolean() || expr.asChar() || expr.asKeyword()) {
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
          case 'make':
            return this.compileMake(pair.cdr, env, stackPos, next);
          case 'element':
            return this.compileElement(pair.cdr, env, stackPos, next);
          case 'root':
            return this.compileRoot(pair.cdr, env, stackPos, next);
          case 'mode':
            return this.compileMode(pair.cdr, env, stackPos, next);
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
  private compileVariable(name: string, env: Environment, stackPos: number, next: Insn | null): Insn {
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
  private compileQuote(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length !== 1) {
      throw new Error('quote requires exactly 1 argument');
    }
    return new ConstantInsn(argsArray[0], next);
  }

  /**
   * Compile if special form
   */
  private compileIf(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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
  private compileLambda(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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

    // Analyze which variables from outer scope are captured
    const freeVars = this.findFreeVariables(body, new Set(paramNames));
    const capturedVars: string[] = [];

    // Capture all free variables that are in the outer environment
    // (frame, stack, or closure variables - anything that's not global)
    for (const varName of freeVars) {
      const binding = env.lookup(varName);
      if (binding) {
        capturedVars.push(varName);
      }
    }

    const lambdaEnv = env.enterLambda(paramNames, capturedVars);

    const bodyInsn = this.compile(body, lambdaEnv, 0, new ReturnInsn(paramNames.length));

    const signature: Signature = {
      nRequiredArgs: paramNames.length,
      nOptionalArgs: 0,
      restArg: false,
      nKeyArgs: 0,
    };

    // Build instruction chain:
    // 1. Push captured variables onto stack (right-to-left)
    // 2. Create closure with those values
    let result: Insn = new ClosureInsn(signature, bodyInsn, capturedVars.length, next);

    // Push captured variables onto stack (right-to-left so they're in order)
    for (let i = capturedVars.length - 1; i >= 0; i--) {
      const varName = capturedVars[i];
      const binding = env.lookup(varName);
      if (!binding) {
        throw new Error(`Variable ${varName} not found in environment`);
      }

      // Compile a reference to this variable
      switch (binding.kind) {
        case 'frame':
          result = new FrameRefInsn(binding.index, result);
          break;
        case 'stack':
          const offset = binding.index - (stackPos + (capturedVars.length - 1 - i));
          result = new StackRefInsn(offset, binding.index, result);
          break;
        case 'closure':
          result = new ClosureRefInsn(binding.index, result);
          break;
      }
    }

    return result;
  }

  /**
   * Compile define special form (top-level only for now)
   */
  private compileDefine(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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
  private compileSet(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    throw new Error('set! not yet implemented');
  }

  /**
   * Compile let special form
   */
  private compileLet(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 2) {
      throw new Error('let requires at least 2 arguments');
    }

    const bindings = this.listToArray(argsArray[0]);
    const vars: string[] = [];
    const inits: ELObj[] = [];

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
  private compileBegin(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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
  private compileAnd(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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
  private compileOr(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
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
   * Compile make special form (DSSSL flow object construction)
   * Port from: OpenJade Interpreter.cxx compileMake()
   *
   * Syntax: (make flow-object-type keyword: value ... content)
   *
   * The flow object type is not evaluated (like a symbol in quote).
   * Keywords and their values are compiled as normal arguments.
   */
  private compileMake(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length === 0) {
      throw new Error('make requires at least a flow object type');
    }

    // Get the make-flow-object primitive
    const primitive = this.globals.lookup('make-flow-object');
    if (!primitive || !primitive.asFunction()?.isPrimitive()) {
      throw new Error('make-flow-object primitive not found');
    }

    // Build instruction chain from back to front:
    // 1. PrimitiveCallInsn (calls make-flow-object)
    // 2. Push all arguments (right-to-left)
    // 3. Return first instruction in chain

    // Start with the call instruction
    let insn: Insn | null = new PrimitiveCallInsn(argsArray.length, primitive, next);

    // Compile arguments in reverse order (right-to-left for stack)
    for (let i = argsArray.length - 1; i >= 1; i--) {
      insn = this.compile(argsArray[i], env, stackPos + i, insn);
    }

    // First argument (flow object type) is a quoted symbol
    insn = new ConstantInsn(argsArray[0], insn);

    return insn;
  }

  /**
   * Compile function call
   */
  private compileCall(fn: ELObj, args: ELObj[], env: Environment, stackPos: number, next: Insn | null): Insn {
    // Check if fn is a known primitive at compile time
    const fnSym = fn.asSymbol();
    if (fnSym) {
      const primitive = this.globals.lookup(fnSym.name);
      if (primitive && primitive.asFunction()?.isPrimitive()) {
        // Compile as primitive call
        let result: Insn = new PrimitiveCallInsn(args.length, primitive, next);

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

    let result: Insn = new CallInsn(args.length, next);

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
   * Find free variables in an expression
   * Port from: OpenJade style/Expression.cxx freeVariables analysis
   *
   * @param expr Expression to analyze
   * @param bound Set of bound variables (parameters, let bindings)
   * @returns Set of free variable names
   */
  private findFreeVariables(expr: ELObj, bound: Set<string>): Set<string> {
    const free = new Set<string>();

    const analyze = (e: ELObj, b: Set<string>) => {
      // Symbol - check if it's free
      const sym = e.asSymbol();
      if (sym) {
        if (!b.has(sym.name) && !this.globals.lookup(sym.name)) {
          free.add(sym.name);
        }
        return;
      }

      // Not a list - no variables
      const pair = e.asPair();
      if (!pair) {
        return;
      }

      const car = pair.car;
      const carSym = car.asSymbol();

      // Special forms that introduce bindings
      if (carSym) {
        switch (carSym.name) {
          case 'lambda': {
            const args = this.listToArray(pair.cdr);
            if (args.length < 2) return;

            const params = this.listToArray(args[0]);
            const newBound = new Set(b);
            for (const p of params) {
              const pSym = p.asSymbol();
              if (pSym) newBound.add(pSym.name);
            }

            // Analyze body with extended bindings
            for (let i = 1; i < args.length; i++) {
              analyze(args[i], newBound);
            }
            return;
          }

          case 'let': {
            const args = this.listToArray(pair.cdr);
            if (args.length < 2) return;

            const bindings = this.listToArray(args[0]);
            const newBound = new Set(b);

            // Analyze init expressions with current bindings
            for (const binding of bindings) {
              const bindingList = this.listToArray(binding);
              if (bindingList.length === 2) {
                analyze(bindingList[1], b);
                const nameSym = bindingList[0].asSymbol();
                if (nameSym) newBound.add(nameSym.name);
              }
            }

            // Analyze body with extended bindings
            for (let i = 1; i < args.length; i++) {
              analyze(args[i], newBound);
            }
            return;
          }

          case 'quote':
            // Don't analyze quoted expressions
            return;

          case 'define':
          case 'set!':
            // Analyze the value but not the name
            const args = this.listToArray(pair.cdr);
            if (args.length === 2) {
              analyze(args[1], b);
            }
            return;
        }
      }

      // Default: analyze all elements of the list
      let current = e;
      while (current.asPair()) {
        const p = current.asPair()!;
        analyze(p.car, b);
        current = p.cdr;
      }
    };

    analyze(expr, bound);
    return free;
  }

  /**
   * Convert list to array
   */
  private listToArray(list: ELObj): ELObj[] {
    const result: ELObj[] = [];
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
  private makeBegin(exprs: ELObj[]): ELObj {
    if (exprs.length === 0) {
      return theNilObj;
    }
    if (exprs.length === 1) {
      return exprs[0];
    }

    // Build (begin expr1 expr2 ...)
    let list: ELObj = theNilObj;
    for (let i = exprs.length - 1; i >= 0; i--) {
      list = new PairObj(exprs[i], list);
    }
    return new PairObj(makeSymbol('begin'), list);
  }

  /**
   * Compile element construction rule
   * Port from: Interpreter.cxx compileElement()
   *
   * Syntax: (element gi body...)
   *
   * This doesn't generate runtime code - it registers a construction rule.
   * Returns empty instruction that does nothing.
   */
  private compileElement(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length < 1) {
      throw new Error('element requires at least a GI');
    }

    // First arg is element name (symbol or string)
    const giArg = argsArray[0];
    let elementName: string;

    const giSym = giArg.asSymbol();
    const giStr = giArg.asString();

    if (giSym) {
      elementName = giSym.name;
    } else if (giStr) {
      elementName = giStr.value;
    } else {
      throw new Error('element requires a symbol or string for GI');
    }

    // Body is rest of arguments (wrapped in begin if multiple)
    const body = argsArray.slice(1);
    const bodyExpr = body.length === 1 ? body[0] : this.makeBegin(body);

    // Compile body to a lambda (takes no args, accesses current-node via VM)
    const lambdaBody = new PairObj(bodyExpr, theNilObj);
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(theNilObj, lambdaBody) // Empty arg list
    );

    // Compile lambda to bytecode
    const lambdaInsn = this.compile(lambdaExpr, env, stackPos, null);

    // Execute to get the closure
    const vm = new VM();
    const closure = vm.eval(lambdaInsn);

    // Register rule (mode is empty string for default)
    this.globals.ruleRegistry.addRule(elementName, "", closure);

    // Return no-op instruction (element forms don't execute at template load time)
    return new ConstantInsn(theNilObj, next);
  }

  /**
   * Compile root construction rule
   * Port from: Interpreter.cxx compileRoot()
   *
   * Syntax: (root body...)
   *
   * Registers a rule for the document root.
   */
  private compileRoot(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length === 0) {
      throw new Error('root requires a body');
    }

    // Body (wrapped in begin if multiple expressions)
    const bodyExpr = argsArray.length === 1 ? argsArray[0] : this.makeBegin(argsArray);

    // Compile to lambda
    const lambdaBody = new PairObj(bodyExpr, theNilObj);
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(theNilObj, lambdaBody)
    );

    // Compile the lambda to bytecode
    const lambdaInsn = this.compile(lambdaExpr, env, stackPos, null);

    // Execute to get the closure
    const vm = new VM();
    const closure = vm.eval(lambdaInsn);

    // Register as root rule
    this.globals.ruleRegistry.addRule("root", "", closure);

    // Return no-op
    return new ConstantInsn(theNilObj, next);
  }

  /**
   * Compile mode declaration
   * Port from: Interpreter.cxx compileMode()
   *
   * Syntax: (mode name (element gi body...)...)
   *
   * For now, this is a stub. Full implementation would set current mode context.
   */
  private compileMode(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length < 2) {
      throw new Error('mode requires a name and at least one rule');
    }

    // First arg is mode name
    const modeNameArg = argsArray[0];
    let modeName: string;

    const modeSym = modeNameArg.asSymbol();
    if (modeSym) {
      modeName = modeSym.name;
    } else {
      throw new Error('mode requires a symbol for mode name');
    }

    // Rest are element definitions
    for (let i = 1; i < argsArray.length; i++) {
      const ruleExpr = argsArray[i];
      const rulePair = ruleExpr.asPair();

      if (!rulePair) {
        throw new Error('mode body must contain element expressions');
      }

      const ruleOp = rulePair.car.asSymbol();
      if (ruleOp && ruleOp.name === 'element') {
        // Parse element rule
        const elementArgs = this.listToArray(rulePair.cdr);
        if (elementArgs.length < 1) {
          throw new Error('element in mode requires GI');
        }

        const giArg = elementArgs[0];
        let elementName: string;

        const giSym = giArg.asSymbol();
        const giStr = giArg.asString();

        if (giSym) {
          elementName = giSym.name;
        } else if (giStr) {
          elementName = giStr.value;
        } else {
          throw new Error('element requires symbol or string for GI');
        }

        // Body
        const body = elementArgs.slice(1);
        const bodyExpr = body.length === 1 ? body[0] : this.makeBegin(body);

        // Compile to lambda
        const lambdaBody = new PairObj(bodyExpr, theNilObj);
        const lambdaExpr = new PairObj(
          makeSymbol('lambda'),
          new PairObj(theNilObj, lambdaBody)
        );

        // Compile lambda to bytecode and execute to get closure
        const lambdaInsn = this.compile(lambdaExpr, env, stackPos, null);
        const vm = new VM();
        const closure = vm.eval(lambdaInsn);

        // Register with mode
        this.globals.ruleRegistry.addRule(elementName, modeName, closure);
      }
    }

    // Return no-op
    return new ConstantInsn(theNilObj, next);
  }
}
