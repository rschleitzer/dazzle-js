/**
 * DSSSL Construction Rules
 * Port from: OpenJade style/Interpreter.{h,cxx}
 *
 * Construction rules define how elements are transformed during processing.
 * Rules are matched by element name and processing mode.
 */

import type { ELObj } from '../scheme/elobj.js';

/**
 * Construction rule - defines how to process an element or root
 * Port from: Interpreter.h struct ConstructionRule
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
   * Function to execute (returns sosofo)
   * In OpenJade this is compiled bytecode
   */
  func: ELObj;
}

/**
 * Rule Registry - stores and matches construction rules
 * Port from: Interpreter.cxx rule management
 */
export class RuleRegistry {
  private rules: ConstructionRule[] = [];

  /**
   * Register a construction rule
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
   * Find matching rule for an element
   * Port from: Interpreter::findConstructionRule()
   *
   * @param elementName Element name (or "root")
   * @param mode Processing mode (default: "")
   * @returns Matching rule or null
   */
  findRule(elementName: string, mode: string = ""): ConstructionRule | null {
    // Try to find exact match
    const rule = this.rules.find(
      r => r.elementName === elementName && r.mode === mode
    );

    return rule || null;
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
