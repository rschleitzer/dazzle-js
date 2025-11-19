/**
 * DSSSL Construction Rules
 * Port from: OpenJade style/Interpreter.{h,cxx}
 *
 * Construction rules define how elements are transformed during processing.
 * Rules are matched by element name and processing mode.
 */

import type { ELObj } from '../scheme/elobj.js';
import type { GlobalEnvironment } from '../scheme/compiler.js';
import { Compiler, Environment } from '../scheme/compiler.js';
import { VM } from '../scheme/vm.js';

/**
 * Construction rule - stores either compiled function or uncompiled expression
 * Port from: OpenJade Interpreter.h Identifier pattern (value_ and def_)
 */
export interface ConstructionRule {
  /**
   * Element name (e.g., "chapter") or "root" for root rule
   */
  elementName: string;

  /**
   * Processing mode (empty string for default mode)
   */
  mode: string;

  /**
   * Compiled function (closure) - cached after first compilation
   */
  func?: ELObj;

  /**
   * Compiled bytecode instruction - cached after first compilation
   * Port from: OpenJade stores both the closure (func) and the bytecode (insn)
   * The bytecode is re-evaluated each time to create a fresh closure with correct stack frames
   */
  insn?: any;

  /**
   * Uncompiled lambda expression - compiled on first match
   */
  lambdaExpr?: ELObj;

  /**
   * Circular compilation detection
   */
  beingCompiled?: boolean;
}

/**
 * Rule Registry - stores and matches construction rules
 * Port from: Interpreter.cxx rule management
 */
export class RuleRegistry {
  private rules: ConstructionRule[] = [];

  /**
   * Register a construction rule with compiled function
   * Port from: Interpreter::addConstructionRule()
   */
  addRule(elementName: string, mode: string, func: ELObj): void {
    // Check if rule already exists for this element+mode
    const existing = this.rules.findIndex(
      r => r.elementName === elementName && r.mode === mode
    );

    if (existing >= 0) {
      // Replace existing rule (later definition wins)
      this.rules[existing] = { elementName, mode, func };
    } else {
      // Add new rule
      this.rules.push({ elementName, mode, func });
    }
  }

  /**
   * Register a construction rule with uncompiled lambda expression
   * Port from: OpenJade Identifier::setDefinition pattern
   *
   * The lambda will be compiled lazily on first match, allowing forward references.
   */
  addRuleLambdaExpr(elementName: string, mode: string, lambdaExpr: ELObj, globals: any): void {
    // Check if rule already exists for this element+mode
    const existing = this.rules.findIndex(
      r => r.elementName === elementName && r.mode === mode
    );

    if (existing >= 0) {
      // Replace existing rule (later definition wins)
      this.rules[existing] = { elementName, mode, lambdaExpr };
    } else {
      // Add new rule
      this.rules.push({ elementName, mode, lambdaExpr });
    }
  }

  /**
   * Find matching rule for an element, compiling if necessary
   * Port from: OpenJade Identifier::computeValue pattern
   *
   * @param elementName Element name (or "root")
   * @param mode Processing mode (default: "")
   * @param globals Global environment for compilation (required if rule needs compilation)
   * @returns Matching rule or null
   */
  findRule(elementName: string, mode: string = "", globals?: GlobalEnvironment): ConstructionRule | null {
    // Try to find exact match
    let rule = this.rules.find(
      r => r.elementName === elementName && r.mode === mode
    );

    // If no exact match and this is NOT a root query, try default rule for this mode
    // Port from: OpenJade falls back to default rule when no specific rule matches
    // But root processing should NOT fall back to default rule
    if (!rule && elementName !== "root") {
      rule = this.rules.find(
        r => r.elementName === "#default" && r.mode === mode
      );
    }

    if (!rule) return null;

    // Already compiled
    if (rule.func) return rule;

    // Need to compile from lambda expression
    if (rule.lambdaExpr) {
      if (rule.beingCompiled) {
        throw new Error(`Circular reference in rule for ${elementName}/${mode}`);
      }

      if (!globals) {
        throw new Error(`Cannot compile rule for ${elementName}/${mode}: no globals provided`);
      }

      rule.beingCompiled = true;
      try {
        // Compile lambda expression to bytecode
        const compiler = new Compiler(globals);
        const insn = compiler.compile(rule.lambdaExpr, new Environment(), 0, null);

        // Execute to get the closure (for backwards compatibility)
        const vm = new VM();
        vm.globals = globals;
        const func = vm.eval(insn);

        // Cache BOTH the bytecode instruction AND the function
        // Port from: OpenJade caches the instruction so it can be re-evaluated
        // to create fresh closures with correct stack frames
        rule.insn = insn;
        rule.func = func;
        delete rule.lambdaExpr;  // Free the AST
        delete rule.beingCompiled;

        return rule;
      } catch (e) {
        delete rule.beingCompiled;
        throw e;
      }
    }

    return null;
  }

  /**
   * Get all rules (for debugging)
   */
  getAllRules(): ConstructionRule[] {
    return [...this.rules];
  }

  /**
   * Clear all rules
   */
  clear(): void {
    this.rules = [];
  }
}
