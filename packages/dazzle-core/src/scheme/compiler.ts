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
  makePair,
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
  CaseInsn,
  PopInsn,
  PopBindingsInsn,
  ConsInsn,
  SetImmediateInsn,
  FrameRefInsn,
  StackRefInsn,
  ClosureRefInsn,
  GlobalRefInsn,
  ReturnInsn,
  PrimitiveCallInsn,
  CallInsn,
  ClosureInsn,
  VarargsInsn,
} from './insn.js';

import { VM } from './vm.js';
import { standardPrimitives } from './primitives.js';
import { RuleRegistry } from '../dsssl/rules.js';
import { parse } from './parser.js';

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

  /**
   * Get all binding names (for debugging)
   */
  getBindingNames(): string[] {
    return Array.from(this.bindings.keys());
  }
}

/**
 * Definition entry - stores either compiled value or uncompiled expression
 * Port from: OpenJade Interpreter.h Identifier (def_ and value_ fields)
 */
interface Definition {
  value?: ELObj;          // Compiled value (if computed)
  expr?: ELObj;          // Uncompiled expression AST (if not yet computed)
  beingComputed?: boolean; // Loop detection
}

/**
 * Global environment - top-level bindings and DSSSL rules
 * Port from: OpenJade Interpreter.h Identifier management
 */
export class GlobalEnvironment {
  private bindings: Map<string, Definition> = new Map();
  public ruleRegistry: RuleRegistry = new RuleRegistry();

  constructor() {
    // Install standard primitives
    for (const [name, func] of Object.entries(standardPrimitives)) {
      this.bindings.set(name, { value: func });
    }

    // Install Scheme helper functions from OpenJade builtins.dsl
    // These are commonly-used utilities built on primitives
    this.installBuiltins();
  }

  /**
   * Install Scheme helper functions
   * Port from: OpenJade dsssl/builtins.dsl
   */
  private installBuiltins(): void {
    // Parse and install helper functions as uncompiled expressions
    // They will be compiled lazily on first use

    // node-list-reduce - DSSSL §10.2.2
    // Fundamental node-list iterator
    const nodeListReduceCode = `
      (define (node-list-reduce nl combine init)
        (if (node-list-empty? nl)
            init
            (node-list-reduce (node-list-rest nl)
                              combine
                              (combine init (node-list-first nl)))))
    `;

    // node-list->list - Convert node-list to Scheme list
    // Used with map, apply, etc.
    const nodeListToListCode = `
      (define (node-list->list nl)
        (reverse (node-list-reduce nl
                                   (lambda (result snl)
                                     (cons snl result))
                                   '())))
    `;

    // node-list-filter - Filter node-list by predicate
    // Port from: OpenJade builtins.dsl
    const nodeListFilterCode = `
      (define (node-list-filter proc nl)
        (node-list-reduce nl
                          (lambda (result snl)
                            (if (proc snl)
                                (node-list result snl)
                                result))
                          (empty-node-list)))
    `;

    // map - R4RS map with multi-list support
    // NOTE: We use the primitive map from primitives.ts instead of the Scheme-level version
    // The primitive version is more efficient and fully working
    // const mapCode = `...` - REMOVED

    try {
      const exprs = parse(`
        ${nodeListReduceCode}
        ${nodeListToListCode}
        ${nodeListFilterCode}
      `);

      // Compile and execute immediately to avoid AST corruption
      const compiler = new Compiler(this);
      const vm = new VM();
      vm.globals = this;

      for (const expr of exprs) {
        const bytecode = compiler.compile(expr, new Environment(), 0, null);
        vm.eval(bytecode); // Execute the define
      }
    } catch (e) {
      // Ignore errors during builtin installation
      // They will surface as undefined variable errors if actually needed
    }
  }

  /**
   * Define a value (already computed)
   * Port from: Identifier::setValue
   */
  define(name: string, value: ELObj): void {
    this.bindings.set(name, { value });
  }

  /**
   * Deep copy an AST to prevent shared structure corruption
   */
  private deepCopyAST(obj: ELObj): ELObj {
    // Primitives and immutables
    if (obj.asNil() || obj.asBoolean() !== null || obj.asNumber() || obj.asString() || obj.asSymbol()) {
      return obj; // These are immutable, safe to share
    }

    // Pairs - recursively copy
    const pair = obj.asPair();
    if (pair) {
      const carSym = pair.car.asSymbol();
      if (carSym && carSym.name === 'lambda') {
        // DEBUG: Show lambda params BEFORE copying
        const params = pair.cdr.asPair()?.car;
        const paramsDebug = [];
        let curr = params;
        while (curr?.asPair()) {
          const sym = curr.asPair()!.car.asSymbol();
          if (sym) paramsDebug.push(sym.name);
          curr = curr.asPair()!.cdr;
        }
      }

      const newCar = this.deepCopyAST(pair.car);
      const newCdr = this.deepCopyAST(pair.cdr);
      const result = makePair(newCar, newCdr);

      // DEBUG: Check if this was a lambda, show params AFTER copying
      if (carSym && carSym.name === 'lambda') {
        const params = result.cdr.asPair()?.car;
        const paramsDebug = [];
        let curr = params;
        while (curr?.asPair()) {
          const sym = curr.asPair()!.car.asSymbol();
          if (sym) paramsDebug.push(sym.name);
          curr = curr.asPair()!.cdr;
        }
      }

      return result;
    }

    // Other types (functions, sosofos, etc.) - return as-is
    // These shouldn't appear in uncompiled ASTs
    return obj;
  }

  /**
   * Define an expression (to be compiled lazily)
   * Port from: Identifier::setDefinition
   */
  setDefinition(name: string, expr: ELObj): void {
    // NOTE: OpenJade doesn't deep copy - it stores the expression as-is
    // Deep copying was causing corruption of shared AST structures
    this.bindings.set(name, { expr });
  }

  /**
   * Check if identifier is defined
   * Port from: Identifier::defined
   */
  defined(name: string): boolean {
    const def = this.bindings.get(name);
    return def !== undefined && (def.value !== undefined || def.expr !== undefined);
  }

  /**
   * Lookup value, computing if necessary
   * Port from: Identifier::computeValue
   */
  lookup(name: string): ELObj | null {
    const def = this.bindings.get(name);
    if (!def) return null;

    // Already computed
    if (def.value) {
      return def.value;
    }

    // Need to compute from expression
    if (def.expr) {
      // Loop detection - return null to allow recursive functions
      // Port from: OpenJade Identifier::computeValue - returns NULL when beingComputed
      // This allows forward/self references to compile as GlobalRefInsn (runtime lookup)
      if (def.beingComputed) {
        return null;
      }

      def.beingComputed = true;
      try {
        // DEBUG: Check if this is map function
        if (name === 'map') {
          // Check map1 params in stored expr
          try {
            const lambda = def.expr.asPair();
            const lambdaBody = lambda?.cdr.asPair()?.cdr.asPair()?.car;
            const letBindings = lambdaBody?.asPair()?.cdr.asPair()?.car;
            const map1Binding = letBindings?.asPair()?.car.asPair();
            const map1Lambda = map1Binding?.cdr.asPair()?.car;
            const map1Params = map1Lambda?.asPair()?.cdr.asPair()?.car;

            const paramsArray = [];
            let current = map1Params;
            while (current?.asPair()) {
              const sym = current.asPair()!.car.asSymbol();
              if (sym) paramsArray.push(sym.name);
              current = current.asPair()!.cdr;
            }
          } catch (e) {
          }
        }

        // Compile and evaluate the expression
        const compiler = new Compiler(this);
        const insn = compiler.compile(def.expr, new Environment(), 0, null);
        const vm = new VM();
        vm.globals = this;
        const value = vm.eval(insn);

        // Cache the computed value
        def.value = value;
        delete def.expr;  // Free the AST
        delete def.beingComputed;

        return value;
      } catch (e) {
        delete def.beingComputed;
        throw e;
      }
    }

    return null;
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
          case 'letrec':
            return this.compileLetrec(pair.cdr, env, stackPos, next);
          case 'begin':
            return this.compileBegin(pair.cdr, env, stackPos, next);
          case 'and':
            return this.compileAnd(pair.cdr, env, stackPos, next);
          case 'or':
            return this.compileOr(pair.cdr, env, stackPos, next);
          case 'case':
            return this.compileCase(pair.cdr, env, stackPos, next);
          case 'cond':
            return this.compileCond(pair.cdr, env, stackPos, next);
          case 'make':
            return this.compileMake(pair.cdr, env, stackPos, next);
          case 'element':
            return this.compileElement(pair.cdr, env, stackPos, next);
          case 'root':
            return this.compileRoot(pair.cdr, env, stackPos, next);
          case 'mode':
            return this.compileMode(pair.cdr, env, stackPos, next);
          // DSSSL declaration forms - no-ops for SGML backend (code generation)
          // Port from: OpenJade SchemeParser.cxx doDeclareCharacteristic, doDeclareFlowObjectClass, etc.
          // TODO: Implement when adding RTF/document formatting backends
          // These register characteristics, flow object classes, and language definitions
          // needed for document formatting but not code generation
          case 'declare-flow-object-class':
          case 'declare-characteristic':
          case 'declare-default-language':
          case 'define-language':
          case 'declare-initial-value':
          case 'declare-class-attribute':
          case 'declare-id-attribute':
          case 'declare-flow-object-macro':
          case 'declare-char-property':
          case 'add-char-properties':
          case 'declare-char-characteristic+property':
          case 'declare-reference-value-type':
            return new ConstantInsn(theNilObj, next);
        }
      }

      // Function call
      return this.compileCall(car, this.listToArray(pair.cdr), env, stackPos, next);
    }

    throw new Error(`Cannot compile expression: ${expr}`);
  }

  /**
   * Compile variable reference
   * Port from: OpenJade Expression.cxx VariableExpression::compile
   */
  private compileVariable(name: string, env: Environment, stackPos: number, next: Insn | null): Insn {
    const binding = env.lookup(name);

    if (binding) {
      // Local variable (frame, stack, or closure)
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

    // Global variable - check if defined
    // Port from: OpenJade VariableExpression::compile global lookup
    if (!this.globals.defined(name)) {
      throw new Error(`Undefined variable: ${name}`);
    }

    // Use runtime lookup (like OpenJade's TopRefInsn)
    // This allows forward references
    return new GlobalRefInsn(name, next);
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
    const paramNames: string[] = [];
    let hasRestArg = false;
    let restArgName: string | null = null;

    // Parse parameters, handling #!rest
    for (let i = 0; i < params.length; i++) {
      const p = params[i];
      const sym = p.asSymbol();
      if (!sym) {
        throw new Error('lambda parameter must be a symbol');
      }

      // Check for #!rest keyword
      if (sym.name === '#!rest') {
        if (i === params.length - 1) {
          throw new Error('#!rest must be followed by a parameter name');
        }
        hasRestArg = true;
        // Next parameter is the rest arg name
        i++;
        const restSym = params[i].asSymbol();
        if (!restSym) {
          throw new Error('rest parameter must be a symbol');
        }
        restArgName = restSym.name;
        paramNames.push(restArgName);
        // #!rest must be the last parameter specification
        if (i !== params.length - 1) {
          throw new Error('#!rest must be the last parameter specification');
        }
        break;
      }

      paramNames.push(sym.name);
    }

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

    // DEBUG
    if (paramNames.includes('xs')) {
    }

    const lambdaEnv = env.enterLambda(paramNames, capturedVars);

    const bodyInsn = this.compile(body, lambdaEnv, 0, new ReturnInsn(paramNames.length));

    // Calculate required args (excluding rest arg if present)
    const nRequiredArgs = hasRestArg ? paramNames.length - 1 : paramNames.length;

    const signature: Signature = {
      nRequiredArgs,
      nOptionalArgs: 0,
      restArg: hasRestArg,
      nKeyArgs: 0,
    };

    // Port from: OpenJade Expression.cxx lines 602-636
    // If function has rest arg, wrap body with VarargsInsn
    let closureCode: Insn | null = bodyInsn;
    if (hasRestArg) {
      closureCode = new VarargsInsn(signature, bodyInsn);
    }

    // Build instruction chain:
    // 1. Push captured variables onto stack (right-to-left)
    // 2. Create closure with those values
    let result: Insn = new ClosureInsn(signature, closureCode, capturedVars.length, next);

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
   * Compile define special form
   * Port from: OpenJade SchemeParser.cxx doDefine()
   *
   * Supports two forms:
   * 1. (define name value)
   * 2. (define (name args...) body...) - lambda shorthand
   *
   * Stores the UNCOMPILED expression for lazy evaluation (like OpenJade)
   */
  private compileDefine(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 2) {
      throw new Error('define requires at least 2 arguments');
    }

    const nameExpr = argsArray[0];

    // DEBUG: Check if this is the map function
    const nameExprPair = nameExpr.asPair();
    if (nameExprPair) {
      const funcName = nameExprPair.car.asSymbol();
      if (funcName && funcName.name === 'map') {
        // Check if body contains map1 lambda
        try {
          const body = argsArray[1];
          const letExpr = body.asPair();
          if (letExpr) {
            const letBindings = letExpr.cdr.asPair()?.car;
            const map1Binding = letBindings?.asPair()?.car;
            const map1Lambda = map1Binding?.asPair()?.cdr.asPair()?.car;
            const map1Params = map1Lambda?.asPair()?.cdr.asPair()?.car;

            const paramsArray = [];
            let current = map1Params;
            while (current?.asPair()) {
              const sym = current.asPair()!.car.asSymbol();
              if (sym) paramsArray.push(sym.name);
              current = current.asPair()!.cdr;
            }
          }
        } catch (e) {
        }
      }
    }

    // Check for lambda shorthand: (define (name args...) body...)
    const namePair = nameExpr.asPair();
    if (namePair) {
      // Lambda shorthand form
      const funcNameSym = namePair.car.asSymbol();
      if (!funcNameSym) {
        throw new Error('define: function name must be a symbol');
      }

      // Body is rest of define args
      const body = argsArray.slice(1);

      // Transform to: (lambda (args...) body...)
      // IMPORTANT: Use namePair.cdr directly (the original param list) instead of extracting and rebuilding
      // This avoids potential issues with shared structure
      const lambdaBody = body.length === 1 ? body[0] : this.makeBegin(body);
      const lambdaExpr = new PairObj(
        makeSymbol('lambda'),
        new PairObj(
          namePair.cdr,  // Use original params list directly
          new PairObj(lambdaBody, theNilObj)
        )
      );

      // Store UNCOMPILED expression (lazy compilation like OpenJade)
      this.globals.setDefinition(funcNameSym.name, lambdaExpr);

      // Return no-op (define doesn't produce a value)
      return new ConstantInsn(theNilObj, next);
    }

    // Simple form: (define name value)
    if (argsArray.length !== 2) {
      throw new Error('define requires exactly 2 arguments');
    }

    const sym = nameExpr.asSymbol();
    if (!sym) {
      throw new Error('define name must be a symbol');
    }

    const valueExpr = argsArray[1];

    // Store UNCOMPILED expression (lazy compilation like OpenJade)
    this.globals.setDefinition(sym.name, valueExpr);

    // Return no-op
    return new ConstantInsn(theNilObj, next);
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

    // Check for named let: (let name ((var init) ...) body)
    const firstArg = argsArray[0];
    const namedLetSym = firstArg.asSymbol();
    if (namedLetSym) {
      // Named let - direct implementation without letrec
      if (argsArray.length < 3) {
        throw new Error('named let requires at least 3 arguments');
      }
      const name = namedLetSym.name;
      const bindings = this.listToArray(argsArray[1]);
      const bodyExprs = argsArray.slice(2);

      // Extract vars and inits from bindings
      const vars: string[] = [];
      const inits: ELObj[] = [];
      for (const binding of bindings) {
        const bindingList = this.listToArray(binding);
        if (bindingList.length !== 2) {
          throw new Error('let binding must have exactly 2 elements');
        }
        const varSym = bindingList[0].asSymbol();
        if (!varSym) {
          throw new Error('let binding name must be a symbol');
        }
        vars.push(varSym.name);
        inits.push(bindingList[1]);
      }

      // Named let implementation (Port from OpenJade SchemeParser.cxx parseLet)
      // OpenJade transforms: (let name ((v1 e1) ...) body) to:
      //   ((letrec ((name (lambda (v1 ...) body))) name) e1 ...)
      //
      // This creates a local recursive binding using letrec, not a global definition.
      // This is critical because the lambda needs to capture the current environment.

      // Build lambda: (lambda (v1 ...) body)
      const lambdaParams = vars.reduceRight((acc: ELObj, v) => makePair(makeSymbol(v), acc), theNilObj);
      const lambdaBody = bodyExprs.length === 1 ? bodyExprs[0] : this.makeBegin(bodyExprs);
      const lambda = makePair(makeSymbol('lambda'), makePair(lambdaParams, makePair(lambdaBody, theNilObj)));

      // Build: (letrec ((name (lambda ...))) name)
      const letrecBinding = makePair(makeSymbol(name), makePair(lambda, theNilObj));
      const letrecBindings = makePair(letrecBinding, theNilObj);
      const letrecBody = makeSymbol(name);  // Just the variable reference
      const letrec = makePair(makeSymbol('letrec'), makePair(letrecBindings, makePair(letrecBody, theNilObj)));

      // Build call: ((letrec ...) e1 ...)
      const callArgs = inits.reduceRight((acc: ELObj, init) => makePair(init, acc), theNilObj);
      const call = makePair(letrec, callArgs);

      // Compile the call expression
      return this.compile(call, env, stackPos, next);
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
      const init = bindingList[1];

      // DEBUG: Check map1's lambda params BEFORE pushing to inits
      if (name.name === 'map1') {
        const lambdaPair = init.asPair();
        if (lambdaPair) {
          const params = lambdaPair.cdr.asPair()?.car;
          const paramsDebug = [];
          let curr = params;
          while (curr?.asPair()) {
            const sym = curr.asPair()!.car.asSymbol();
            if (sym) paramsDebug.push(sym.name);
            curr = curr.asPair()!.cdr;
          }
        }
      }

      inits.push(init);

      // DEBUG: Check map1's lambda params AFTER pushing to inits
      if (name.name === 'map1') {
        const lambdaPair = init.asPair();
        if (lambdaPair) {
          const params = lambdaPair.cdr.asPair()?.car;
          const firstPair = params?.asPair();
          const paramsDebug = [];
          let curr = params;
          while (curr?.asPair()) {
            const sym = curr.asPair()!.car.asSymbol();
            if (sym) paramsDebug.push(sym.name);
            curr = curr.asPair()!.cdr;
          }
          // Store reference to check later
          (globalThis as any).__map1ParamsFirstPair = firstPair;
        }
      }
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
      // DEBUG: Check map1's lambda params BEFORE compiling this init
      if (vars[i] === 'map1') {
        const lambdaPair = inits[i].asPair();
        if (lambdaPair) {
          const params = lambdaPair.cdr.asPair()?.car;
          const paramsDebug = [];
          let curr = params;
          while (curr?.asPair()) {
            const sym = curr.asPair()!.car.asSymbol();
            if (sym) paramsDebug.push(sym.name);
            curr = curr.asPair()!.cdr;
          }
        }
      }

      // DEBUG: Check map1's lambda params RIGHT BEFORE compiling this init
      if (vars[i] === 'map1') {
        const lambdaPair = inits[i].asPair();
        if (lambdaPair) {
          const params = lambdaPair.cdr.asPair()?.car;
          const firstPair = params?.asPair();
        }
      }

      // Init i will execute with (inits.length - 1 - i) values already on stack
      const numPushed = inits.length - 1 - i;
      result = this.compile(inits[i], env, stackPos + numPushed, result);

      // DEBUG: Check map1's lambda params AFTER compiling this init
      if (vars[i] === 'map1') {
        const lambdaPair = inits[i].asPair();
        if (lambdaPair) {
          const params = lambdaPair.cdr.asPair()?.car;
          const paramsDebug = [];
          let curr = params;
          while (curr?.asPair()) {
            const sym = curr.asPair()!.car.asSymbol();
            if (sym) paramsDebug.push(sym.name);
            curr = curr.asPair()!.cdr;
          }
        }
      }
    }

    return result;
  }

  /**
   * Compile letrec special form
   * Port from: OpenJade Expression.cxx LetrecExpression::compile
   *
   * Letrec allows recursive bindings by:
   * 1. Allocating stack slots for all variables (initially undefined)
   * 2. Evaluating inits with those slots visible in the environment
   * 3. Storing the computed values into those slots
   * 4. Evaluating the body
   */
  private compileLetrec(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 2) {
      throw new Error('letrec requires at least 2 arguments');
    }

    const bindings = this.listToArray(argsArray[0]);
    const vars: string[] = [];
    const inits: ELObj[] = [];

    for (const binding of bindings) {
      const bindingList = this.listToArray(binding);
      if (bindingList.length !== 2) {
        throw new Error('letrec binding must have exactly 2 elements');
      }

      const name = bindingList[0].asSymbol();
      if (!name) {
        throw new Error('letrec binding name must be a symbol');
      }

      vars.push(name.name);
      inits.push(bindingList[1]);
    }

    const nVars = vars.length;

    // Body is remaining arguments (implicit begin)
    const bodyExprs = argsArray.slice(1);
    const body = bodyExprs.length === 1
      ? bodyExprs[0]
      : this.makeBegin(bodyExprs);

    // DEBUG
    const bodySym = body.asSymbol();
    if (bodySym) {
    }

    // Create extended environment with all variables visible
    const bodyEnv = env.extendStack(vars);
    const bodyStackPos = stackPos + nVars;

    // Compile body with extended environment
    let result: Insn = this.compile(body, bodyEnv, bodyStackPos, new PopBindingsInsn(nVars, next));

    // Add SetImmediateInsn for each variable (in reverse order)
    // These will pop init values and store them into the reserved slots
    for (let i = 0; i < nVars; i++) {
      result = new SetImmediateInsn(nVars, result);
    }

    // Compile inits in reverse order
    // Each init is compiled with all variables visible (for recursion)
    for (let i = nVars - 1; i >= 0; i--) {
      result = this.compile(inits[i], bodyEnv, bodyStackPos, result);
    }

    // Push placeholder values for all variables (to reserve stack slots)
    for (let i = nVars - 1; i >= 0; i--) {
      result = new ConstantInsn(theNilObj, result);
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
   * Compile case special form
   * Port from: OpenJade Interpreter.cxx (case uses CaseInsn for matching)
   *
   * Syntax: (case <key>
   *           ((<datum1> ...) <expr> ...)
   *           ((<datum2> ...) <expr> ...)
   *           (else <expr> ...))
   *
   * Semantics: Evaluate <key>, then check each clause to see if the result
   * is eqv? to any datum in the clause. If so, evaluate that clause's expressions.
   */
  private compileCase(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 2) {
      throw new Error('case requires at least key and one clause');
    }

    const keyExpr = argsArray[0];
    const clauses = argsArray.slice(1);

    // Compile failure case (no match) - return unspecified
    let failInsn: Insn = new ConstantInsn(theNilObj, next);

    // Compile clauses from right to left
    for (let i = clauses.length - 1; i >= 0; i--) {
      const clause = clauses[i];
      const clauseList = this.listToArray(clause);

      if (clauseList.length < 2) {
        throw new Error('case clause must have at least datum list and one expression');
      }

      const selector = clauseList[0];
      const clauseBody = clauseList.slice(1);

      // Check for else clause
      const selectorSym = selector.asSymbol();
      if (selectorSym && selectorSym.name === 'else') {
        // Else clause - compile body
        const bodyExpr = clauseBody.length === 1
          ? clauseBody[0]
          : this.makeBegin(clauseBody);
        failInsn = this.compile(bodyExpr, env, stackPos - 1, new PopInsn(next));
        continue;
      }

      // Regular clause - compile datum list and body
      const datums = this.listToArray(selector);
      const bodyExpr = clauseBody.length === 1
        ? clauseBody[0]
        : this.makeBegin(clauseBody);

      // Compile the clause body
      // Note: After matching, we need to pop the key from stack before executing body
      const bodyInsn = this.compile(bodyExpr, env, stackPos - 1, new PopInsn(next));

      // Build chain of CaseInsn for each datum (right to left)
      let clauseInsn: Insn = failInsn;
      for (let j = datums.length - 1; j >= 0; j--) {
        clauseInsn = new CaseInsn(datums[j], bodyInsn, clauseInsn);
      }

      failInsn = clauseInsn;
    }

    // Compile key expression - result will be on stack for CaseInsn to test
    return this.compile(keyExpr, env, stackPos, failInsn);
  }

  /**
   * Compile cond special form
   * Port from: R4RS cond specification
   */
  private compileCond(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 1) {
      throw new Error('cond requires at least one clause');
    }

    // Compile clauses from right to left
    let result: Insn = new ConstantInsn(theNilObj, next); // Default if no clause matches

    for (let i = argsArray.length - 1; i >= 0; i--) {
      const clause = argsArray[i];
      const clauseList = this.listToArray(clause);

      if (clauseList.length < 1) {
        throw new Error('cond clause must have at least a test');
      }

      const test = clauseList[0];
      const testSym = test.asSymbol();

      // Check for else clause
      if (testSym && testSym.name === 'else') {
        if (clauseList.length < 2) {
          throw new Error('else clause must have at least one expression');
        }
        const bodyExprs = clauseList.slice(1);
        const body = bodyExprs.length === 1
          ? bodyExprs[0]
          : this.makeBegin(bodyExprs);
        result = this.compile(body, env, stackPos, next);
        continue;
      }

      // Regular clause
      if (clauseList.length === 1) {
        // (test) with no body - return test result if true
        const thenBranch = new PopInsn(next); // Pop test result and continue
        const elseBranch = result;
        const testInsn = this.compile(test, env, stackPos, new TestInsn(thenBranch, elseBranch));
        result = testInsn;
      } else {
        // (test expr ...) - evaluate body if test is true
        const bodyExprs = clauseList.slice(1);
        const body = bodyExprs.length === 1
          ? bodyExprs[0]
          : this.makeBegin(bodyExprs);
        const thenBranch = this.compile(body, env, stackPos - 1, new PopInsn(next));
        const elseBranch = result;
        result = this.compile(test, env, stackPos, new TestInsn(thenBranch, elseBranch));
      }
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
    // Stack layout when CallInsn executes: [arg0, arg1, ..., argN-1, function]
    // Function must be on top of stack for CallInsn to pop it
    //
    // Instruction chain: last compiled executes first
    // To get stack [arg0, ..., argN-1, fn]:
    //   Compile: arg0 first (pushes first) → ... → argN-1 → fn last (pushes last) → CallInsn
    // Build chain: CallInsn ← fn ← argN-1 ← ... ← arg0

    let result: Insn = new CallInsn(args.length, next);

    // Compile function - it will execute after all args, so will be on top
    result = this.compile(fn, env, stackPos + args.length, result);

    // Compile arguments in reverse (n-1 down to 0)
    // They execute first-to-last (0 to n-1), pushing in that order
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

            // Check for named let
            const firstArg = args[0];
            const namedLetSym = firstArg.asSymbol();
            if (namedLetSym) {
              // Named let: (let name ((v1 e1) ...) body)
              if (args.length < 3) return;
              const bindings = this.listToArray(args[1]);
              const newBound = new Set(b);
              newBound.add(namedLetSym.name); // The name is bound in the body

              // Analyze init expressions with current bindings
              for (const binding of bindings) {
                const bindingList = this.listToArray(binding);
                if (bindingList.length === 2) {
                  analyze(bindingList[1], b);
                  const varSym = bindingList[0].asSymbol();
                  if (varSym) newBound.add(varSym.name);
                }
              }

              // Analyze body with extended bindings
              for (let i = 2; i < args.length; i++) {
                analyze(args[i], newBound);
              }
              return;
            }

            // Regular let
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

          case 'letrec': {
            // letrec: variables are bound during init evaluation
            const args = this.listToArray(pair.cdr);
            if (args.length < 2) return;

            const bindings = this.listToArray(args[0]);
            const newBound = new Set(b);

            // First, add all variable names to bindings
            for (const binding of bindings) {
              const bindingList = this.listToArray(binding);
              if (bindingList.length === 2) {
                const nameSym = bindingList[0].asSymbol();
                if (nameSym) newBound.add(nameSym.name);
              }
            }

            // KEY DIFFERENCE FROM LET:
            // Analyze init expressions with extended bindings (allows recursive references)
            for (const binding of bindings) {
              const bindingList = this.listToArray(binding);
              if (bindingList.length === 2) {
                analyze(bindingList[1], newBound);
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
   * Convert array to list
   */
  private arrayToList(arr: ELObj[]): ELObj {
    let result: ELObj = theNilObj;

    // Build list from right to left
    for (let i = arr.length - 1; i >= 0; i--) {
      result = new PairObj(arr[i], result);
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

    // Create lambda expression (takes no args, accesses current-node via VM)
    const lambdaBody = new PairObj(bodyExpr, theNilObj);
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(theNilObj, lambdaBody) // Empty arg list
    );

    // Store UNCOMPILED lambda expression (lazy compilation like define)
    // This allows rules to reference variables defined later in the template
    // The lambda will be compiled when the rule is first matched
    this.globals.ruleRegistry.addRuleLambdaExpr(elementName, "", lambdaExpr, this.globals);

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

    // Create lambda expression
    const lambdaBody = new PairObj(bodyExpr, theNilObj);
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(theNilObj, lambdaBody)
    );

    // Store UNCOMPILED lambda expression (lazy compilation)
    this.globals.ruleRegistry.addRuleLambdaExpr("root", "", lambdaExpr, this.globals);

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

        // Create lambda expression
        const lambdaBody = new PairObj(bodyExpr, theNilObj);
        const lambdaExpr = new PairObj(
          makeSymbol('lambda'),
          new PairObj(theNilObj, lambdaBody)
        );

        // Store UNCOMPILED lambda expression (lazy compilation)
        this.globals.ruleRegistry.addRuleLambdaExpr(elementName, modeName, lambdaExpr, this.globals);
      }
    }

    // Return no-op
    return new ConstantInsn(theNilObj, next);
  }
}
