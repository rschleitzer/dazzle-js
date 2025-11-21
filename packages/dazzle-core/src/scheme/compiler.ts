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
  UnresolvedQuantityObj,
  makeSymbol,
  makePair,
  makeBox,
  makeStyle,
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj.js';

import {
  type Insn,
  type Location,
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
  ErrorInsn,
  StoreUnitInsn,
  PushModeInsn,
  PopModeInsn,
  CopyFlowObjInsn,
  CheckSosofoInsn,
  SosofoAppendInsn,
  SetContentInsn,
  SetNonInheritedCInsn,
  SetNonInheritedCsSosofoInsn,
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
  private static nextId = 0;
  private id: number;

  /**
   * Create environment
   */
  constructor(
    private stackDepth: number = 0  // Current stack depth for let bindings
  ) {
    this.id = Environment.nextId++;
    if (process.env.DEBUG_FRAME) {
      console.error(`[Environment] Created env#${this.id} with stackDepth=${stackDepth}`);
    }
  }

  /**
   * Look up a variable
   */
  lookup(name: string): Binding | null {
    return this.bindings.get(name) || null;
  }

  /**
   * Get debug info about bindings
   */
  getDebugInfo(): Array<[string, Binding]> {
    return Array.from(this.bindings.entries());
  }

  /**
   * Extend environment with new stack variables (for let)
   * Port from: OpenJade Environment::augmentFrame
   */
  extendStack(vars: string[], stackPos: number): Environment {
    const baseStackPos = stackPos;
    const newEnv = new Environment(baseStackPos + vars.length);

    if (process.env.DEBUG_FRAME) {
      console.error(`[extendStack] env#${this.id} -> env#${newEnv.id}: Adding ${vars.length} vars at stackPos=${stackPos}`);
      console.error(`  Parent has ${this.bindings.size} bindings`);
    }

    // Copy existing bindings unchanged
    this.bindings.forEach((binding, name) => {
      newEnv.bindings.set(name, binding);
      if (process.env.DEBUG_FRAME) {
        console.error(`  Copied: ${name} -> ${binding.kind}[${binding.index}]`);
      }
    });

    // Add new stack variables at frame-relative positions
    // Port from: OpenJade uses frame-relative positions, not absolute
    // vars[0] at frame position stackPos+0, vars[1] at stackPos+1, etc.
    if (process.env.DEBUG_CLOSURES) {
      console.error(`[extendStack] Adding ${vars.length} vars at stackPos=${stackPos}`);
    }
    for (let i = 0; i < vars.length; i++) {
      const framePos = baseStackPos + i;
      if (process.env.DEBUG_FRAME) {
        console.error(`  New: ${vars[i]} -> stack[${framePos}]`);
      }
      newEnv.bindings.set(vars[i], {
        name: vars[i],
        kind: 'stack',
        index: framePos,  // Frame-relative position (NOT reversed!)
      });
    }

    return newEnv;
  }

  /**
   * Create new environment for lambda body
   */
  enterLambda(lambdaParams: string[], capturedVars: string[]): Environment {
    const newEnv = new Environment(0);

    if (process.env.DEBUG_FRAME) {
      console.error(`[enterLambda] env#${this.id} -> env#${newEnv.id}: ${lambdaParams.length} params, ${capturedVars.length} captured`);
    }

    // Closure variables
    for (let i = 0; i < capturedVars.length; i++) {
      if (process.env.DEBUG_FRAME) {
        console.error(`  Captured: ${capturedVars[i]} -> closure[${i}]`);
      }
      newEnv.bindings.set(capturedVars[i], {
        name: capturedVars[i],
        kind: 'closure',
        index: i,
      });
    }

    // Lambda parameters as frame variables
    for (let i = 0; i < lambdaParams.length; i++) {
      if (process.env.DEBUG_FRAME) {
        console.error(`  Param: ${lambdaParams[i]} -> frame[${i}]`);
      }
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
   * Format S-expression for debugging
   */
  private formatSExpr(obj: ELObj | null): string {
    if (!obj) return '()';
    const sym = obj.asSymbol();
    if (sym) return sym.name;
    const pair = obj.asPair();
    if (pair) {
      const items: string[] = [];
      let current: ELObj | null = obj;
      while (current?.asPair()) {
        items.push(this.formatSExpr(current.asPair()!.car));
        current = current.asPair()!.cdr;
      }
      if (current && !current.asPair()) {
        return `(${items.join(' ')} . ${this.formatSExpr(current)})`;
      }
      return `(${items.join(' ')})`;
    }
    return obj.constructor.name;
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
        // DEBUG: Track runtime compilation
        if (process.env.DEBUG_FRAME) {
          console.error(`[GlobalEnvironment.lookup] Runtime compiling global '${name}'`);
          console.error(`  Expression type: ${def.expr.constructor.name}`);
          // If it's a lambda, show basic structure
          const pair = def.expr.asPair();
          if (pair && pair.car.asSymbol()?.name === 'lambda') {
            const params = pair.cdr.asPair()?.car ?? null;
            console.error(`  Is lambda with params: ${this.formatSExpr(params)}`);
          }
        }

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
        const env = new Environment();
        if (process.env.DEBUG_FRAME) {
          console.error(`[GlobalEnvironment.lookup] Compiling with clean environment`);
          const bindings = env.getDebugInfo();
          console.error(`  Environment bindings: ${bindings.length} (should be 0)`);
          if (bindings.length > 0) {
            console.error(`  ERROR: Environment is not empty!`);
            for (const [k, v] of bindings) {
              console.error(`    ${k}: ${v.kind}[${v.index}]`);
            }
          }
        }
        const insn = compiler.compile(def.expr, env, 0, null);
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
  /** Current source file being compiled */
  public currentFile: string = '<unknown>';
  /** Current line number being compiled */
  public currentLine: number = 0;

  constructor(private globals: GlobalEnvironment) {}

  /**
   * Create location for current compilation position
   * Port from: OpenJade tracks location through compilation
   *
   * If expr is provided and has location info, use that.
   * Otherwise fall back to compiler's current file/line.
   */
  private makeLocation(expr?: ELObj): Location {
    if (expr && expr.location.file !== '<unknown>') {
      return expr.location;
    }
    return {
      file: this.currentFile,
      line: this.currentLine,
      column: 0,
    };
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
  compile(expr: ELObj, env: Environment, stackPos: number = 0, next: Insn | null = null): Insn {
    // Self-evaluating constants
    // Port from: OpenJade - UnresolvedQuantityObj is a literal that evaluates to itself
    if (expr.asNumber() || expr.asString() || expr.asBoolean() || expr.asChar() || expr.asKeyword() || expr instanceof UnresolvedQuantityObj) {
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
          case 'define-unit':
            return this.compileDefineUnit(pair.cdr, env, stackPos, next);
          case 'set!':
            return this.compileSet(pair.cdr, env, stackPos, next);
          case 'let':
            return this.compileLet(pair.cdr, env, stackPos, next);
          case 'let*':
            return this.compileLetStar(pair.cdr, env, stackPos, next);
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
          case 'style':
            return this.compileStyle(pair.cdr, env, stackPos, next);
          case 'element':
            return this.compileElement(pair.cdr, env, stackPos, next);
          case 'default':
            return this.compileDefault(pair.cdr, env, stackPos, next);
          case 'root':
            return this.compileRoot(pair.cdr, env, stackPos, next);
          case 'mode':
            return this.compileMode(pair.cdr, env, stackPos, next);
          case 'with-mode':
            return this.compileWithMode(pair.cdr, env, stackPos, next);
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
      return this.compileCall(car, this.listToArray(pair.cdr), env, stackPos, next, expr);
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
          if (process.env.DEBUG_CLOSURES && (name === 'component' || name === 'chaporapp')) {
            console.error(`[compileVariable] ${name}: frame[${binding.index}]`);
          }
          if (process.env.DEBUG_FRAME) {
            console.error(`[compileVariable] Generating FrameRefInsn for '${name}' at index ${binding.index}`);
            // Show environment chain
            const envBindings = env.getDebugInfo();
            console.error(`  Environment has ${envBindings.length} local bindings:`);
            for (const [k, v] of envBindings) {
              console.error(`    ${k}: ${v.kind}[${v.index}]`);
            }
          }
          return new FrameRefInsn(binding.index, next);
        case 'stack':
          // Convert absolute position to relative offset from current stackPos
          const offset = binding.index - stackPos;
          if (process.env.DEBUG_CLOSURES && (name === 'component' || name === 'chaporapp')) {
            console.error(`[compileVariable] ${name}: binding.index=${binding.index}, stackPos=${stackPos}, offset=${offset}`);
          }
          if (process.env.DEBUG_FOT && (name === 'itemcontent' || name === 'spacing' || name === 'first-child')) {
            console.error(`[compileVariable] ${name}: binding.index=${binding.index}, stackPos=${stackPos}, offset=${offset}`);
          }
          return new StackRefInsn(offset, binding.index, next, false, name);
        case 'closure':
          if (process.env.DEBUG_CLOSURES && (name === 'component' || name === 'chaporapp')) {
            console.error(`[compileVariable] ${name}: closure[${binding.index}]`);
          }
          return new ClosureRefInsn(binding.index, next);
      }
    }

    // Global variable - check if defined
    // Port from: OpenJade VariableExpression::compile global lookup
    if (!this.globals.defined(name)) {
      throw new Error(`Undefined variable: ${name}\n  file: ${this.currentFile}:${this.currentLine}`);
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

    if (process.env.DEBUG_FOT && next?.constructor.name === 'CheckSosofoInsn') {
      console.error(`  compileIf: Compiling if with CheckSosofoInsn as next, stackPos=${stackPos}`);
      console.error(`    consequent type: ${consequent.constructor.name}`);
      console.error(`    alternative type: ${alternative.constructor.name}`);
    }

    const conseqInsn = this.compile(consequent, env, stackPos, next);
    const altInsn = this.compile(alternative, env, stackPos, next);

    if (process.env.DEBUG_FOT && next?.constructor.name === 'CheckSosofoInsn') {
      console.error(`    conseqInsn: ${conseqInsn.constructor.name}, altInsn: ${altInsn.constructor.name}`);
    }

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
    const optionalDefaults: (ELObj | null)[] = [];
    let nRequiredParams = 0;
    let nOptionalParams = 0;
    let hasRestArg = false;
    let restArgName: string | null = null;
    let inOptional = false;

    // Parse parameters, handling #!optional and #!rest
    // Port from: OpenJade Expression.cxx parseFormalArguments
    for (let i = 0; i < params.length; i++) {
      const p = params[i];

      // Check for keyword first
      const sym = p.asSymbol();
      if (sym) {
        // Check for #!optional keyword
        if (sym.name === '#!optional') {
          if (inOptional) {
            throw new Error('duplicate #!optional in parameter list');
          }
          if (hasRestArg) {
            throw new Error('#!optional cannot follow #!rest');
          }
          inOptional = true;
          continue;
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

        // Regular parameter name
        if (inOptional) {
          // Optional parameter (defaults to #f)
          paramNames.push(sym.name);
          optionalDefaults.push(theFalseObj);
          nOptionalParams++;
        } else {
          // Required parameter
          paramNames.push(sym.name);
          nRequiredParams++;
        }
        continue;
      }

      // Check for optional parameter with default value: (name default)
      const pair = p.asPair();
      if (pair) {
        if (!inOptional) {
          throw new Error('parameter default values require #!optional');
        }

        // Parse (name default-value)
        const nameArg = pair.car.asSymbol();
        if (!nameArg) {
          throw new Error('optional parameter name must be a symbol');
        }

        const defaultValueList = this.listToArray(pair.cdr);
        if (defaultValueList.length !== 1) {
          throw new Error('optional parameter must have exactly one default value');
        }

        paramNames.push(nameArg.name);
        // Store the default value expression (will be evaluated later)
        optionalDefaults.push(defaultValueList[0]);
        nOptionalParams++;
        continue;
      }

      throw new Error('lambda parameter must be a symbol or (name default) pair');
    }

    // Body is remaining arguments (implicit begin)
    const bodyExprs = argsArray.slice(1);
    const body = bodyExprs.length === 1
      ? bodyExprs[0]
      : this.makeBegin(bodyExprs);

    // Analyze which variables from outer scope are captured
    const freeVars = this.findFreeVariables(body, new Set(paramNames));
    const capturedVars: string[] = [];

    // Debug: log free variables found
    if (process.env.DEBUG_FRAME) {
      console.error(`[compileLambda] findFreeVariables found: ${Array.from(freeVars).join(', ') || '(none)'}`);
    }
    if (process.env.DEBUG_CLOSURE && freeVars.size > 0) {
      console.error(`findFreeVariables found: ${Array.from(freeVars).join(', ')}`);
    }

    // Capture all free variables that are in the outer environment
    // (frame, stack, or closure variables - anything that's not global)
    for (const varName of freeVars) {
      const binding = env.lookup(varName);
      if (binding) {
        capturedVars.push(varName);
        if (process.env.DEBUG_FRAME) {
          console.error(`[compileLambda] Capturing '${varName}' from outer env: ${binding.kind}[${binding.index}]`);
        }
      } else if (process.env.DEBUG_CLOSURE || process.env.DEBUG_FRAME) {
        console.error(`  ${varName}: NOT FOUND in environment (skipped)`);
      }
    }

    if (process.env.DEBUG_CLOSURE && capturedVars.length > 0) {
      console.error(`  Capturing: ${capturedVars.join(', ')}`);
    }

    const lambdaEnv = env.enterLambda(paramNames, capturedVars);

    // CRITICAL: Compile lambda body with stackPos = nParams (frame-relative addressing)
    // Port from: OpenJade Expression.cxx LambdaExpression::compile
    // Parameters are at frame positions 0..nParams-1, so body starts at nParams
    if (process.env.DEBUG_FOT && (nOptionalParams > 0 || hasRestArg)) {
      console.error(`[compileLambda] Creating ReturnInsn with totalArgs=${paramNames.length}`);
      console.error(`  paramNames: [${paramNames.join(', ')}]`);
      console.error(`  nRequiredParams: ${nRequiredParams}, nOptionalParams: ${nOptionalParams}, hasRestArg: ${hasRestArg}`);
    }
    const bodyInsn = this.compile(body, lambdaEnv, paramNames.length, new ReturnInsn(paramNames.length));

    // Build signature
    // Port from: OpenJade Expression.cxx parseFormalArguments signature building
    const signature: Signature = {
      nRequiredArgs: nRequiredParams,
      nOptionalArgs: nOptionalParams,
      restArg: hasRestArg,
      nKeyArgs: 0,
    };

    // DEBUG: For 0-arg lambdas, show what the body instruction is
    if (process.env.DEBUG_FRAME && nRequiredParams === 0 && nOptionalParams === 0 && !hasRestArg) {
      console.error(`[compileLambda] 0-arg lambda body starts with: ${bodyInsn.constructor.name}`);
      if (bodyInsn.constructor.name === 'FrameRefInsn') {
        console.error(`  ERROR: 0-arg lambda should not have FrameRefInsn in body!`);
        console.error(`  FrameRefInsn index: ${(bodyInsn as any).index}`);
        console.error(`  lambdaEnv bindings after enterLambda:`);
        const bindings = lambdaEnv.getDebugInfo();
        for (const [k, v] of bindings) {
          console.error(`    ${k}: ${v.kind}[${v.index}]`);
        }
      }
    }

    if (process.env.DEBUG_FRAME) {
      if (nRequiredParams === 0 && nOptionalParams === 0 && !hasRestArg) {
        console.error(`[compileLambda] Compiling 0-argument lambda with paramNames=[${paramNames.join(', ')}], capturedVars=[${capturedVars.join(', ')}]`);
        if (paramNames.length > 0) {
          console.error(`  BUG: signature has 0 params but paramNames has ${paramNames.length}`);
        }
      } else {
        console.error(`[compileLambda] Compiling lambda with paramNames=[${paramNames.join(', ')}], capturedVars=[${capturedVars.join(', ')}]`);
      }
    }

    // Port from: OpenJade Expression.cxx lines 545-636
    // Build entry points for VarargsInsn - one instruction chain per arity
    let closureCode: Insn | null = bodyInsn;
    if (hasRestArg || nOptionalParams > 0) {
      // entryPoints[0] = for only required args (all optionals missing)
      // entryPoints[1] = for required + 1 optional (remaining optionals missing)
      // ...
      // entryPoints[nOptionalArgs] = for all args provided
      // entryPoints[nOptionalArgs+1] = for rest args (if restArg)
      const nEntryPoints = nOptionalParams + (hasRestArg ? 1 : 0) + 1;
      const entryPoints: Insn[] = new Array(nEntryPoints);

      // Start with the last entry point (all optionals provided, or rest args)
      // Port from: Expression.cxx lines 557-607
      let code: Insn = bodyInsn;

      // If we have rest arg, push nil for it
      // Port from: Expression.cxx lines 602-606
      if (hasRestArg) {
        code = new ConstantInsn(theNilObj, code);
      }

      // Store the last entry point
      entryPoints[nOptionalParams] = code;

      // Build entry points for each arity, from most args to fewest
      // Port from: Expression.cxx lines 609-625
      for (let i = nOptionalParams - 1; i >= 0; i--) {
        // entryPoints[i] needs to:
        // 1. Evaluate default expression for optional param i
        // 2. Jump to entryPoints[i+1] (which handles remaining params)
        const nextEntry = entryPoints[i + 1];
        const defaultExpr = optionalDefaults[i];

        if (defaultExpr && defaultExpr !== theFalseObj) {
          // Compile default expression
          // Port from: Expression.cxx lines 614-620
          // Environment includes: required params + earlier optional params (not this one or later)
          // Plus captured variables from outer scope
          const paramsInScope = paramNames.slice(0, nRequiredParams + i);

          // Create environment: Environment(formalParams[0..n], boundVars)
          // Port from: Expression.cxx line 618
          const defaultEnv = new Environment();

          // Add captured variables as closure refs
          for (let j = 0; j < capturedVars.length; j++) {
            defaultEnv['bindings'].set(capturedVars[j], {
              name: capturedVars[j],
              kind: 'closure',
              index: j,
            });
          }

          // Add parameters in scope as frame refs
          for (let j = 0; j < paramsInScope.length; j++) {
            defaultEnv['bindings'].set(paramsInScope[j], {
              name: paramsInScope[j],
              kind: 'frame',
              index: j,
            });
          }

          // Stack position is the number of params in scope
          // Port from: Expression.cxx line 619 - f.size()
          if (process.env.DEBUG_FOT) {
            console.error(`[compileLambda] Compiling default for optional param ${i} at stackPos=${paramsInScope.length}`);
            console.error(`  paramsInScope: [${paramsInScope.join(', ')}]`);
            console.error(`  Will chain to entryPoints[${i + 1}]`);
          }
          const defaultInsn = this.compile(defaultExpr, defaultEnv, paramsInScope.length, nextEntry);
          entryPoints[i] = defaultInsn;
        } else {
          // No default expression, use #f
          // Port from: Expression.cxx lines 622-624
          entryPoints[i] = new ConstantInsn(theFalseObj, nextEntry);
        }
      }

      // Create VarargsInsn with entry points
      // Port from: Expression.cxx line 636
      closureCode = new VarargsInsn(signature, entryPoints);
    }

    // Build instruction chain:
    // 1. Push captured variables onto stack (right-to-left)
    // 2. Create closure with those values
    let result: Insn = new ClosureInsn(signature, closureCode, capturedVars.length, next);

    // Count how many stack/closure variables we'll push (frame vars don't get pushed)
    let numStackVarsToPush = 0;
    for (const varName of capturedVars) {
      const binding = env.lookup(varName);
      if (binding && (binding.kind === 'stack' || binding.kind === 'closure')) {
        numStackVarsToPush++;
      }
    }

    // Push captured variables onto stack (right-to-left so they're in order)
    let stackVarsPushedSoFar = 0;
    for (let i = capturedVars.length - 1; i >= 0; i--) {
      const varName = capturedVars[i];
      const binding = env.lookup(varName);
      if (!binding) {
        throw new Error(`Variable ${varName} not found in environment`);
      }

      // Debug: log binding details for path
      if (process.env.DEBUG_CLOSURE && varName === 'path') {
        console.error(`  Generating capture instruction for 'path': kind=${binding.kind}, index=${binding.index}, stackPos=${stackPos}, numStackVarsToPush=${numStackVarsToPush}, stackVarsPushedSoFar=${stackVarsPushedSoFar}`);
      }

      // Compile a reference to this variable for closure capture
      // Port from: OpenJade Expression.cxx compilePushVars
      // Frame variables use FrameRefInsn, stack variables need offset calculation
      switch (binding.kind) {
        case 'frame':
          // Frame variables (parameters) can be accessed directly
          // Use forCapture=true so boxes aren't unboxed
          result = new FrameRefInsn(binding.index, result, true);
          if (process.env.DEBUG_CLOSURE && varName === 'path') {
            console.error(`    Created FrameRefInsn[${binding.index}]`);
          }
          break;
        case 'stack':
          // Stack variables (let/letrec bindings) need offset from current stackPos
          // IMPORTANT: Only count stack/closure vars that have been pushed, not frame vars!
          const offset = binding.index - (stackPos + (numStackVarsToPush - 1 - stackVarsPushedSoFar));
          // Use forCapture=true so boxes aren't unboxed
          result = new StackRefInsn(offset, binding.index, result, true);
          if (process.env.DEBUG_CLOSURE && varName === 'path') {
            console.error(`    Created StackRefInsn[${offset}] (binding.index=${binding.index})`);
          }
          stackVarsPushedSoFar++;
          break;
        case 'closure':
          result = new ClosureRefInsn(binding.index, result);
          if (process.env.DEBUG_CLOSURE && varName === 'path') {
            console.error(`    Created ClosureRefInsn[${binding.index}]`);
          }
          stackVarsPushedSoFar++;
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
   * Compile define-unit special form
   * Port from: OpenJade SchemeParser.cxx doDefineUnit()
   *
   * Form: (define-unit name value)
   * Where name is a symbol (unit name) and value is an expression that evaluates to a quantity
   *
   * Creates instruction chain: valueInsn → StoreUnitInsn → next
   */
  private compileDefineUnit(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length !== 2) {
      throw new Error('define-unit requires exactly 2 arguments');
    }

    const nameExpr = argsArray[0];
    const sym = nameExpr.asSymbol();
    if (!sym) {
      throw new Error('define-unit: name must be a symbol');
    }

    // Validate unit name - must be all letters, not 'e'
    // Port from: OpenJade SchemeParser.cxx doDefineUnit() validation
    const unitName = sym.name;
    if (unitName === 'e') {
      throw new Error('define-unit: "e" is not a valid unit name');
    }
    if (!/^[a-zA-Z]+$/.test(unitName)) {
      throw new Error(`define-unit: invalid unit name "${unitName}" (must be all letters)`);
    }

    const valueExpr = argsArray[1];

    // Build chain: valueInsn → StoreUnitInsn → next
    // This avoids nested loop execution
    const storeInsn = new StoreUnitInsn(unitName, next);
    const valueInsn = this.compile(valueExpr, env, stackPos, storeInsn);

    return valueInsn;
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
          const elements = bindingList.map(e => {
            const sym = e.asSymbol();
            if (sym) return sym.name;
            const str = e.asString();
            if (str) return `"${str.value}"`;
            return e.constructor.name;
          }).join(', ');
          throw new Error(`let binding must have exactly 2 elements, got ${bindingList.length}: (${elements})`);
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
        throw new Error(`let binding must have exactly 2 elements, got ${bindingList.length}`);
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
    const bodyEnv = env.extendStack(vars, stackPos);
    const bodyStackPos = stackPos + vars.length;
    if (process.env.DEBUG_CLOSURES && vars.includes('component')) {
      console.error(`[compileLet] component let: stackPos=${stackPos}, vars.length=${vars.length}, bodyStackPos=${bodyStackPos}`);
    }
    let result = this.compile(body, bodyEnv, bodyStackPos, new PopBindingsInsn(vars.length, next));

    // Compile initializers in reverse order (like OpenJade's compileInits)
    // Port from: OpenJade Expression.cxx LetExpression::compileInits
    // Each init[i] is compiled at stackPos + i, with the chain of later inits as next
    // Compile from last to first so init[0] executes first
    for (let i = inits.length - 1; i >= 0; i--) {
      // Init i pushes its value to stackPos + i
      result = this.compile(inits[i], env, stackPos + i, result);
    }

    return result;
  }

  /**
   * Compile let* - sequential bindings
   * Port from: R4RS §4.2.2
   * (let* ((v1 e1) (v2 e2) ...) body) transforms to nested let forms
   */
  private compileLetStar(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);
    if (argsArray.length < 2) {
      throw new Error('let* requires at least 2 arguments');
    }

    const bindings = this.listToArray(argsArray[0]);
    const bodyExprs = argsArray.slice(1);

    if (process.env.DEBUG_CLOSURES) {
      const varNames = bindings.map(b => {
        const pair = b.asPair();
        if (pair) {
          const sym = pair.car.asSymbol();
          return sym ? sym.name : '?';
        }
        return '?';
      });
      console.error(`[compileLetStar] bindings: [${varNames.join(', ')}], stackPos=${stackPos}`);
    }

    // Empty bindings - just evaluate body
    if (bindings.length === 0) {
      const body = bodyExprs.length === 1
        ? bodyExprs[0]
        : this.makeBegin(bodyExprs);
      return this.compile(body, env, stackPos, next);
    }

    // Transform let* to nested let forms
    // (let* ((v1 e1) (v2 e2)) body) => (let ((v1 e1)) (let ((v2 e2)) body))
    let result: ELObj;

    // Start with the innermost let (last binding + body)
    const lastBinding = bindings[bindings.length - 1];
    const innerBody = bodyExprs.length === 1
      ? bodyExprs[0]
      : this.makeBegin(bodyExprs);

    // Build: (let ((last-binding)) body)
    result = makePair(
      makeSymbol('let'),
      makePair(
        makePair(lastBinding, theNilObj),  // Single binding list
        makePair(innerBody, theNilObj)      // Body
      )
    );

    // Wrap each previous binding as an outer let
    for (let i = bindings.length - 2; i >= 0; i--) {
      const binding = bindings[i];
      result = makePair(
        makeSymbol('let'),
        makePair(
          makePair(binding, theNilObj),  // Single binding list
          makePair(result, theNilObj)     // Previous let as body
        )
      );
    }

    // Compile the transformed expression
    return this.compile(result, env, stackPos, next);
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

    // Create extended environment with all variables visible
    const bodyEnv = env.extendStack(vars, stackPos);
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
    // Port from: OpenJade - letrec variables are boxed to allow updates after capture
    for (let i = nVars - 1; i >= 0; i--) {
      // Push a box containing nil as the placeholder
      result = new ConstantInsn(makeBox(theNilObj), result);
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

    // Compile failure case (no match) - throw error
    // Port from: OpenJade behavior - case without else clause throws error if no match
    // ErrorInsn will peek at stack to show the unmatched value, then PopInsn pops it
    let failInsn: Insn = new PopInsn(new ErrorInsn('case: no matching clause', true));

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
        // Port from: R4RS case - else clause must pop key before executing body
        const bodyExpr = clauseBody.length === 1
          ? clauseBody[0]
          : this.makeBegin(clauseBody);
        // Key will be popped before body executes, so body runs at original stackPos
        failInsn = new PopInsn(this.compile(bodyExpr, env, stackPos, next));
        continue;
      }

      // Regular clause - compile datum list and body
      const datums = this.listToArray(selector);
      const bodyExpr = clauseBody.length === 1
        ? clauseBody[0]
        : this.makeBegin(clauseBody);

      // Compile the clause body
      // Port from: OpenJade CaseInsn behavior - body executes after key is popped
      // Key was pushed to stackPos, then popped, so body runs with stack at stackPos
      const bodyInsn = this.compile(bodyExpr, env, stackPos, next);

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
        // Port from: OpenJade - test result stays on stack
        const thenBranch = next;
        const elseBranch = result;
        const testInsn = this.compile(test, env, stackPos, new TestInsn(thenBranch, elseBranch));
        result = testInsn;
      } else {
        // (test expr ...) - evaluate body if test is true
        // Port from: OpenJade IfExpression - both branches at same stackPos
        const bodyExprs = clauseList.slice(1);
        const body = bodyExprs.length === 1
          ? bodyExprs[0]
          : this.makeBegin(bodyExprs);
        const thenBranch = this.compile(body, env, stackPos, next);
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
  /**
   * Compile make special form using OpenJade's algorithm
   * Port from: OpenJade Expression.cxx MakeExpression::compile
   *
   * (make flow-obj-type content...)
   *
   * OpenJade's algorithm:
   * 1. Create flow object template (CopyFlowObjInsn)
   * 2. Compile content expressions, each wrapped in CheckSosofoInsn
   * 3. If multiple content, combine with SosofoAppendInsn
   * 4. Attach content to flow object with SetContentInsn
   *
   * This avoids stack corruption issues because:
   * - Flow object is created immediately (no type symbol on stack)
   * - Content expressions evaluate at same stackPos (replace test/branch values)
   * - Each nested make completes fully before returning to outer context
   */
  private compileMake(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length === 0) {
      throw new Error('make requires at least a flow object type');
    }

    // First argument must be a flow object type symbol
    // Port from: OpenJade SchemeParser.cxx - rejects non-symbols at parse time
    const flowObjType = argsArray[0].asSymbol();
    if (!flowObjType) {
      const loc = this.makeLocation(argsArray[0]);
      throw new Error(`${loc.file}:${loc.line}: make: flow object type must be a symbol, got: ${argsArray[0].constructor.name}`);
    }

    // Separate keyword arguments from content expressions
    // DSSSL syntax: (make type key1: val1 key2: val2 content1 content2 ...)
    // Keywords end with ':' and are followed by their values
    const keywords: Array<{name: string, valueExpr: ELObj, index: number}> = [];
    let contentStart = 1;  // Index where content expressions start

    for (let i = 1; i < argsArray.length; i++) {
      const keyword = argsArray[i].asKeyword();
      if (keyword) {
        // Found a keyword, next element is its value
        if (i + 1 >= argsArray.length) {
          throw new Error(`make: keyword ${keyword.name} missing value`);
        }
        keywords.push({
          name: keyword.name,
          valueExpr: argsArray[i + 1],
          index: i
        });
        i++;  // Skip the value
        contentStart = i + 1;
      } else {
        // Not a keyword, this is where content starts
        break;
      }
    }

    // Now we know: argsArray[1..contentStart-1] are keywords+values, argsArray[contentStart..] are content
    const nContent = argsArray.length - contentStart;

    // Port from: OpenJade MakeExpression::compile lines 1305-1326
    // Handle case with no content
    if (nContent === 0 && keywords.length === 0) {
      // No keywords, no content - just create and return the flow object
      // Port from: line 1309 - CopyFlowObjInsn
      return new CopyFlowObjInsn(flowObjType.name, next);
    }

    // If we have keywords but no content, we still need to set them
    if (nContent === 0) {
      // Build chain: CopyFlowObjInsn -> SetNonInheritedCInsn for each keyword
      let rest: Insn | null = next;
      // Compile keywords in order (they'll execute in reverse, which is what we want)
      for (const kw of keywords) {
        rest = this.compile(kw.valueExpr, env, stackPos + 1,
                            new SetNonInheritedCInsn(kw.name, this.makeLocation(kw.valueExpr), rest));
      }
      return new CopyFlowObjInsn(flowObjType.name, rest);
    }

    // Build instruction chain from back to front:
    let rest: Insn | null = next;

    // Port from: Expression.cxx MakeExpression::compileNonInheritedCs
    // Keyword values must be evaluated with captured variables, not frame variables
    // This is because they're evaluated lazily when the flow object is processed
    if (keywords.length > 0) {
      // Find all variables used in keyword expressions
      const boundVars = new Set<string>();
      for (const kw of keywords) {
        this.collectFreeVariables(kw.valueExpr, new Set(), boundVars);
      }

      // Filter to only variables that exist in current environment
      const capturedVars: string[] = [];
      for (const varName of boundVars) {
        const binding = env.lookup(varName);
        if (binding) {
          capturedVars.push(varName);
          if (process.env.DEBUG_FOT) {
            console.error(`[compileMake] Capturing variable '${varName}' for keyword: ${binding.kind}[${binding.index}]`);
          }
        }
      }

      // Create new environment with ONLY captured variables (as closure variables, not frame)
      const kwEnv = new Environment(0);
      for (let i = 0; i < capturedVars.length; i++) {
        if (process.env.DEBUG_FOT) {
          console.error(`[compileMake] kwEnv: ${capturedVars[i]} -> closure[${i}]`);
        }
        kwEnv['bindings'].set(capturedVars[i], {
          name: capturedVars[i],
          kind: 'closure',
          index: i,
        });
      }

      // Compile keyword value expressions with the new environment
      // Stack position is 1 (flow object is at position 0 in the new context)
      let kwCode: Insn | null = null;
      for (const kw of keywords) {
        if (process.env.DEBUG_FOT) {
          console.error(`[compileMake] Compiling keyword ${kw.name} with kwEnv (${capturedVars.length} closure vars)`);
        }
        kwCode = this.compile(kw.valueExpr, kwEnv, 1,
                             new SetNonInheritedCInsn(kw.name, this.makeLocation(kw.valueExpr), kwCode));
      }

      // Wrap in SetNonInheritedCsSosofoInsn that captures the variables
      rest = new SetNonInheritedCsSosofoInsn(kwCode, capturedVars.length, rest);

      // Push captured variables onto stack (right-to-left)
      for (let i = capturedVars.length - 1; i >= 0; i--) {
        const varName = capturedVars[i];
        const binding = env.lookup(varName);
        if (!binding) {
          throw new Error(`Variable ${varName} not found in environment`);
        }

        // Generate appropriate reference instruction based on binding type
        // Port from: OpenJade Expression.cxx compilePushVars
        // Frame and stack variables both use FrameRefInsn
        // Only closure variables use ClosureRefInsn
        if (binding.kind === 'frame' || binding.kind === 'stack') {
          // Both frame and stack variables use FrameRefInsn in OpenJade
          // binding.index is the frame-relative position
          if (process.env.DEBUG_FOT) {
            console.error(`[compileMake] Pushing captured ${binding.kind} var '${varName}': binding.index=${binding.index} (frame-relative)`);
          }
          rest = new FrameRefInsn(binding.index, rest);
        } else if (binding.kind === 'closure') {
          rest = new ClosureRefInsn(binding.index, rest);
        }
      }
    }

    // Port from: line 1311 - SetContentInsn attaches content to flow object
    rest = new SetContentInsn(flowObjType.name, rest);

    // Port from: lines 1318-1326 - Handle content compilation
    // When there's content, SetContentInsn contains the template, so we DON'T need CopyFlowObjInsn
    if (nContent === 1) {
      // Single content expression
      // Port from: line 1320 - compile at stackPos (not stackPos+1!)
      // Content replaces itself with a sosofo at the same stack position
      const contentExpr = argsArray[contentStart];
      return this.compile(contentExpr, env, stackPos,
                          new CheckSosofoInsn(this.makeLocation(contentExpr), rest));
    } else {
      // Multiple content expressions
      // Port from: lines 1322-1325
      rest = new SosofoAppendInsn(nContent, rest);

      // Compile each content expression in reverse order
      // Port from: line 1324 - compile at stackPos + nContent - i
      if (process.env.DEBUG_FOT) {
        console.error(`compileMake: nContent=${nContent}, contentStart=${contentStart}, stackPos=${stackPos}, argsArray.length=${argsArray.length}`);
      }
      for (let i = nContent - 1; i >= 0; i--) {
        const contentExpr = argsArray[contentStart + i];
        const loc = this.makeLocation(contentExpr);
        const checkInsn: Insn = new CheckSosofoInsn(loc, rest);
        if (process.env.DEBUG_FOT) {
          console.error(`  Loop i=${i}: Wrapping content ${i} (argsArray[${contentStart + i}]=${contentExpr ? contentExpr.constructor.name : 'undefined'}) with CheckSosofoInsn at stackPos ${stackPos + i}`);
          console.error(`    CheckSosofoInsn location: ${loc.file}:${loc.line}:${loc.column}`);
          // Show what the expression is if it's a pair
          const pair = contentExpr?.asPair();
          if (pair) {
            const carSym = pair.car?.asSymbol();
            if (carSym) {
              console.error(`    Expression is: (${carSym.name} ...)`);
            }
          }
        }
        const compiled = this.compile(contentExpr, env, stackPos + i, checkInsn);
        if (process.env.DEBUG_FOT && compiled !== checkInsn && !this.chainContainsInsn(compiled, checkInsn)) {
          console.error(`    WARNING: compile() returned instruction chain that doesn't contain CheckSosofoInsn!`);
          console.error(`    Returned: ${compiled?.constructor.name}, Expected chain to include: CheckSosofoInsn`);
        }
        rest = compiled;
      }
      if (process.env.DEBUG_FOT) {
        console.error(`  Loop completed, returning instruction chain`);
      }
      return rest;
    }
  }

  /**
   * Compile style special form
   * Port from: OpenJade StyleExpression::compile
   *
   * (style key1: val1 key2: val2 ...)
   * Creates a style object with the specified characteristics.
   * For now, this is a stub that just returns an empty style object.
   */
  private compileStyle(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    // For now, ignore all the keyword arguments and just return an empty style object
    // TODO: Properly parse and compile the keyword arguments
    const argsArray = this.listToArray(args);

    // Count keyword argument pairs
    let numPairs = 0;
    for (let i = 0; i < argsArray.length; i++) {
      const keyword = argsArray[i].asKeyword();
      if (keyword && i + 1 < argsArray.length) {
        numPairs++;
        i++; // Skip the value
      }
    }

    // For now, just create an empty style object
    // In a full implementation, we would compile the keyword values and create a proper style
    return new ConstantInsn(makeStyle(), next);
  }

  /**
   * Compile function call
   */
  private compileCall(fn: ELObj, args: ELObj[], env: Environment, stackPos: number, next: Insn | null, expr: ELObj): Insn {
    // Check if fn is a known primitive at compile time
    const fnSym = fn.asSymbol();
    if (fnSym) {
      const primitive = this.globals.lookup(fnSym.name);
      if (primitive && primitive.asFunction()?.isPrimitive()) {
        // Compile as primitive call with location from call site
        let result: Insn = new PrimitiveCallInsn(args.length, primitive, this.makeLocation(expr), next);

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

    if (process.env.DEBUG_FOT && fnSym && next?.constructor.name === 'CheckSosofoInsn') {
      console.error(`  compileCall: ${fnSym.name} with ${args.length} args, next=CheckSosofoInsn`);
    }

    let result: Insn = new CallInsn(args.length, this.makeLocation(expr), next);

    // Compile function - it will execute after all args, so will be on top
    result = this.compile(fn, env, stackPos + args.length, result);

    if (process.env.DEBUG_FOT && fnSym && next?.constructor.name === 'CheckSosofoInsn') {
      console.error(`    After compiling fn: result=${result.constructor.name}`);
    }

    // Compile arguments in reverse (n-1 down to 0)
    // They execute first-to-last (0 to n-1), pushing in that order
    for (let i = args.length - 1; i >= 0; i--) {
      result = this.compile(args[i], env, stackPos + i, result);
    }

    if (process.env.DEBUG_FOT && fnSym && next?.constructor.name === 'CheckSosofoInsn') {
      console.error(`    Final result=${result.constructor.name}`);
    }

    return result;
  }

  // ============ Helpers ============

  /**
   * Built-in keywords and primitives that can never be shadowed
   * These are excluded from free variable detection for performance
   *
   * Note: This list includes common primitives and helpers that are used frequently
   * but are extremely unlikely to be shadowed by parameter names in real code.
   */
  private static readonly UNSHADOWABLE_BUILTINS = new Set([
    // Special forms
    'quote', 'if', 'lambda', 'let', 'let*', 'letrec', 'define', 'set!',
    'begin', 'and', 'or', 'cond', 'case', 'make',
    // Common Scheme primitives
    'cons', 'car', 'cdr', 'cadr', 'caddr', 'cadddr',
    'list', 'null?', 'pair?', 'list?',
    'eq?', 'eqv?', 'equal?', 'not',
    '+', '-', '*', '/', 'quotient', 'remainder', 'modulo',
    '<', '>', '<=', '>=', '=',
    'string-append', 'string=?', 'string<?', 'string>?',
    'number?', 'string?', 'symbol?', 'boolean?', 'procedure?',
    'map', 'for-each', 'append', 'reverse', 'length', 'member', 'assoc',
    'substring', 'string-length', 'string-ref',
    'vector', 'vector-ref', 'vector-set!', 'vector-length',
    // DSSSL specific
    'process-children', 'process-node-list', 'process-root', 'next-match',
    'empty-sosofo', 'sosofo-append', 'literal',
    'current-node', 'node-list-first', 'node-list-rest', 'node-list-length',
    'empty-node-list', 'node-list',
    'gi', 'data', 'attribute-string', 'inherited-attribute-string',
    'children', 'parent', 'ancestors', 'descendants', 'siblings',
    'select-elements', 'select-children', 'element-with-id',
    'first-sibling?', 'last-sibling?',
    // Common helper functions from templates (these are rarely shadowed)
    '$', 'for', 'sql-go', 'file',
  ]);

  /**
   * Find free variables in an expression
   * Port from: OpenJade style/Expression.cxx freeVariables analysis
   *
   * CRITICAL: Proper lexical scoping with shadowing support
   *
   * We mark variables as free even if they're globals, UNLESS they're unshadowable built-ins.
   * This allows a parameter in an outer lambda to shadow a global function with the same name.
   *
   * The capture phase (in compileLambda) then checks if free variables exist in the environment:
   * - If found in environment: captured (shadows any global)
   * - If not in environment: looked up as global at runtime
   *
   * @param expr Expression to analyze
   * @param bound Set of bound variables (parameters, let bindings)
   * @returns Set of free variable names
   */
  /**
   * Collect all free variables in an expression (simpler than findFreeVariables)
   * Port from: OpenJade Expression::markBoundVars
   */
  private collectFreeVariables(expr: ELObj, bound: Set<string>, result: Set<string>): void {
    // Symbol - add if not bound
    const sym = expr.asSymbol();
    if (sym) {
      if (!bound.has(sym.name) && !Compiler.UNSHADOWABLE_BUILTINS.has(sym.name)) {
        result.add(sym.name);
      }
      return;
    }

    // Not a list - no variables
    const pair = expr.asPair();
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
          const newBound = new Set(bound);
          for (const p of params) {
            const pSym = p.asSymbol();
            if (pSym) newBound.add(pSym.name);
          }

          // Recurse into body with extended bindings
          for (let i = 1; i < args.length; i++) {
            this.collectFreeVariables(args[i], newBound, result);
          }
          return;
        }

        case 'let': {
          const args = this.listToArray(pair.cdr);
          if (args.length < 2) return;

          const firstArg = args[0];
          const namedLetSym = firstArg.asSymbol();
          if (namedLetSym) {
            // Named let
            if (args.length < 3) return;
            const bindings = this.listToArray(args[1]);
            const newBound = new Set(bound);
            newBound.add(namedLetSym.name);

            for (const binding of bindings) {
              const bindingList = this.listToArray(binding);
              if (bindingList.length === 2) {
                this.collectFreeVariables(bindingList[1], bound, result);
                const varSym = bindingList[0].asSymbol();
                if (varSym) newBound.add(varSym.name);
              }
            }

            for (let i = 2; i < args.length; i++) {
              this.collectFreeVariables(args[i], newBound, result);
            }
            return;
          }

          // Regular let
          const bindings = this.listToArray(args[0]);
          const newBound = new Set(bound);

          for (const binding of bindings) {
            const bindingList = this.listToArray(binding);
            if (bindingList.length === 2) {
              this.collectFreeVariables(bindingList[1], bound, result);
              const varSym = bindingList[0].asSymbol();
              if (varSym) newBound.add(varSym.name);
            }
          }

          for (let i = 1; i < args.length; i++) {
            this.collectFreeVariables(args[i], newBound, result);
          }
          return;
        }

        case 'define':
        case 'set!':
        case 'if':
        case 'quote':
          // These don't introduce new bindings, just recurse
          break;
      }
    }

    // Default: recurse into all parts of the list
    this.collectFreeVariables(car, bound, result);
    this.collectFreeVariables(pair.cdr, bound, result);
  }

  private findFreeVariables(expr: ELObj, bound: Set<string>): Set<string> {
    const free = new Set<string>();

    const analyze = (e: ELObj, b: Set<string>) => {
      // Symbol - check if it's free
      const sym = e.asSymbol();
      if (sym) {
        // Variable is free if:
        // 1. Not bound in this lambda's parameters/let bindings
        // 2. Not an unshadowable built-in
        // Note: We DON'T exclude all globals! This enables lexical shadowing.
        if (!b.has(sym.name) && !Compiler.UNSHADOWABLE_BUILTINS.has(sym.name)) {
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

          case 'let*': {
            // let*: bindings are evaluated sequentially, each sees previous
            const args = this.listToArray(pair.cdr);
            if (args.length < 2) return;

            const bindings = this.listToArray(args[0]);
            let currentBound = new Set(b);

            // Analyze each binding, extending bound set incrementally
            for (const binding of bindings) {
              const bindingList = this.listToArray(binding);
              if (bindingList.length === 2) {
                // Analyze init with current bindings
                analyze(bindingList[1], currentBound);
                // Add variable to bindings for next init
                const nameSym = bindingList[0].asSymbol();
                if (nameSym) {
                  currentBound = new Set(currentBound);
                  currentBound.add(nameSym.name);
                }
              }
            }

            // Analyze body with all bindings
            for (let i = 1; i < args.length; i++) {
              analyze(args[i], currentBound);
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

    // First arg is element name (symbol, string, or path)
    // Port from: OpenJade supports element paths like (orderedlist title)
    const giArg = argsArray[0];
    let elementName: string;

    const giSym = giArg.asSymbol();
    const giStr = giArg.asString();
    const giPair = giArg.asPair();

    if (giSym) {
      elementName = giSym.name;
    } else if (giStr) {
      elementName = giStr.value;
    } else if (giPair) {
      // Element path like (orderedlist title) - last element in path is the match
      // For now, just use the last element as the key (full path matching not yet implemented)
      const pathElements = this.listToArray(giPair);
      if (pathElements.length === 0) {
        throw new Error('element path cannot be empty');
      }
      const lastElem = pathElements[pathElements.length - 1];
      const lastSym = lastElem.asSymbol();
      if (!lastSym) {
        throw new Error('element path must contain symbols');
      }
      elementName = lastSym.name;
      // TODO: Store full path for proper matching
    } else {
      throw new Error(`element requires a symbol, string, or path for GI, got: ${giArg.constructor.name}`);
    }

    // Body is rest of arguments (wrapped in begin if multiple)
    const body = argsArray.slice(1);
    const bodyExpr = body.length === 1 ? body[0] : this.makeBegin(body);

    // Wrap rule body in a lambda (no parameters)
    // This creates a closure that will be called when the rule matches
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(
        theNilObj,  // No parameters
        new PairObj(bodyExpr, theNilObj)
      )
    );

    // Store uncompiled lambda expression - will be compiled lazily
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
   * Compile default construction rule
   * Port from: OpenJade default rule handling
   *
   * Syntax: (default body...)
   *
   * Registers a catch-all rule that matches any element without a specific rule.
   */
  private compileDefault(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length === 0) {
      throw new Error('default requires a body');
    }

    // Body (wrapped in begin if multiple expressions)
    const bodyExpr = argsArray.length === 1 ? argsArray[0] : this.makeBegin(argsArray);

    // Create lambda expression
    const lambdaBody = new PairObj(bodyExpr, theNilObj);
    const lambdaExpr = new PairObj(
      makeSymbol('lambda'),
      new PairObj(theNilObj, lambdaBody)
    );

    // Store UNCOMPILED lambda expression with special element name "#default"
    // Port from: OpenJade uses a special marker for default rules
    this.globals.ruleRegistry.addRuleLambdaExpr("#default", "", lambdaExpr, this.globals);

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

    // Rest are element or default definitions
    for (let i = 1; i < argsArray.length; i++) {
      const ruleExpr = argsArray[i];
      const rulePair = ruleExpr.asPair();

      if (!rulePair) {
        throw new Error('mode body must contain element or default expressions');
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
        const giPair = giArg.asPair();

        if (giSym) {
          elementName = giSym.name;
        } else if (giStr) {
          elementName = giStr.value;
        } else if (giPair) {
          // Element path - use last element as key
          const pathElements = this.listToArray(giPair);
          if (pathElements.length === 0) {
            throw new Error('element path cannot be empty');
          }
          const lastElem = pathElements[pathElements.length - 1];
          const lastSym = lastElem.asSymbol();
          if (!lastSym) {
            throw new Error('element path must contain symbols');
          }
          elementName = lastSym.name;
        } else {
          throw new Error('element requires symbol, string, or path for GI');
        }

        // Body
        const body = elementArgs.slice(1);
        const bodyExpr = body.length === 1 ? body[0] : this.makeBegin(body);

        // Wrap in lambda (no parameters)
        const lambdaExpr = new PairObj(
          makeSymbol('lambda'),
          new PairObj(
            theNilObj,  // No parameters
            new PairObj(bodyExpr, theNilObj)
          )
        );

        this.globals.ruleRegistry.addRuleLambdaExpr(elementName, modeName, lambdaExpr, this.globals);
      } else if (ruleOp && ruleOp.name === 'default') {
        // Parse default rule (catch-all for this mode)
        const defaultArgs = this.listToArray(rulePair.cdr);

        if (defaultArgs.length === 0) {
          throw new Error('default in mode requires a body');
        }

        // Body
        const bodyExpr = defaultArgs.length === 1 ? defaultArgs[0] : this.makeBegin(defaultArgs);

        // Wrap in lambda (no parameters)
        const lambdaExpr = new PairObj(
          makeSymbol('lambda'),
          new PairObj(
            theNilObj,  // No parameters
            new PairObj(bodyExpr, theNilObj)
          )
        );

        // Store with special element name "#default"
        this.globals.ruleRegistry.addRuleLambdaExpr("#default", modeName, lambdaExpr, this.globals);
      }
    }

    // Return no-op
    return new ConstantInsn(theNilObj, next);
  }

  /**
   * Compile with-mode special form
   * Port from: OpenJade Interpreter.cxx (similar pattern to process-node-list with mode)
   *
   * Syntax: (with-mode mode-name expr)
   *
   * Temporarily sets the processing mode to mode-name while evaluating expr.
   */
  private compileWithMode(args: ELObj, env: Environment, stackPos: number, next: Insn | null): Insn {
    const argsArray = this.listToArray(args);

    if (argsArray.length !== 2) {
      throw new Error('with-mode requires exactly 2 arguments: mode-name and expression');
    }

    // First arg is mode name (can be symbol or keyword)
    const modeNameArg = argsArray[0];
    let modeName: string;

    const modeSym = modeNameArg.asSymbol();
    const modeKeyword = modeNameArg.asKeyword();
    if (modeSym) {
      modeName = modeSym.name;
    } else if (modeKeyword) {
      modeName = modeKeyword.name;
    } else {
      throw new Error('with-mode requires a symbol or keyword for mode name');
    }

    // Second arg is the expression to evaluate with the mode set
    const bodyExpr = argsArray[1];

    // Port from: OpenJade WithModeExpression::compile lines 1089-1091
    // return new PushModeInsn(mode, compile(expr, new PopModeInsn(next)));
    //
    // Creates instruction chain: PushMode → body → PopMode → next
    // This ensures body is part of the normal instruction chain, not a nested loop
    return new PushModeInsn(
      modeName,
      this.compile(bodyExpr, env, stackPos, new PopModeInsn(next))
    );
  }

  /**
   * Helper: Check if instruction chain contains a specific instruction
   */
  private chainContainsInsn(start: Insn | null, target: Insn): boolean {
    let current = start;
    let depth = 0;
    while (current && depth < 100) {  // Limit depth to avoid infinite loops
      if (current === target) return true;
      // Try to get next instruction (this is a simplified check)
      current = (current as any).next || null;
      depth++;
    }
    return false;
  }
}
