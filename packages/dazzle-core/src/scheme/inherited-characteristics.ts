/**
 * DSSSL Inherited Characteristic Functions
 * Port from: OpenJade style/primitive.cxx inherited characteristic accessors
 *
 * These functions query the current inherited values of characteristics
 * during flow object processing. In DSSSL, characteristics can be inherited
 * from parent flow objects, and these functions retrieve those inherited values.
 *
 * TODO: Implement full characteristic inheritance system from OpenJade.
 * For now, these return sensible default values to unblock FOT backend development.
 */

import { type ELObj, type PrimitiveFunction, makeQuantity, makeNumber, makeSymbol, makeString, makePair, theNilObj, theFalseObj, theTrueObj } from './elobj.js';
import type { VM } from './vm.js';
import type { Node, NodeList } from '../grove/index.js';

/**
 * inherited-start-indent
 * Port from: OpenJade style/FlowObj.cxx
 *
 * Returns the inherited value of the start-indent characteristic.
 * In DSSSL, this is typically used to calculate nested indentation:
 *   start-indent: (+ (inherited-start-indent) 10pt)
 *
 * TODO: Query actual inherited value from current flow object context.
 * For now, returns 0pt as the default.
 */
export const inheritedStartIndentPrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // Stub: Return 0pt as default
  // In full implementation, this should:
  // 1. Query current ProcessContext for active flow object stack
  // 2. Look up start-indent characteristic from parent flow object
  // 3. Return that quantity value
  return makeQuantity(0, 'pt');
};

/**
 * element-number
 * Port from: OpenJade style/primitive.cxx DEFPRIMITIVE(ElementNumber)
 * DSSSL spec §10.2.3
 *
 * Returns the position number of an element among its siblings
 * with the same generic identifier (element name).
 *
 * Takes an optional node argument, defaults to current-node.
 */
export const elementNumberPrimitive: PrimitiveFunction = (
  args: ELObj[],
  vm: VM
): ELObj => {
  // Get the node (from args[0] or current-node)
  let node: Node | null = null;

  if (args.length > 0) {
    // Try to get node from argument
    const nodeListObj = args[0].asNodeList();
    if (!nodeListObj) {
      throw new Error('element-number: argument must be a node-list');
    }
    const nodeList = nodeListObj.nodes;
    node = nodeList.first();
    if (!node || nodeList.rest()?.first()) {
      throw new Error('element-number: argument must be a singleton node-list');
    }
  } else {
    // Get current node from VM context
    node = vm.currentNode;
    if (!node) {
      throw new Error('element-number: no current node');
    }
  }

  // Get the element's GI (generic identifier / element name)
  const gi = node.gi();
  if (!gi) {
    // Not an element node
    return theFalseObj;
  }

  // Count preceding siblings with the same GI
  let count = 1; // 1-based numbering
  const parent = node.parent();
  if (parent) {
    let currentList: NodeList | null = parent.children();

    while (currentList) {
      const current = currentList.first();
      if (!current) break;

      if (current === node) {
        break;
      }
      if (current.gi() === gi) {
        count++;
      }

      const rest = currentList.rest();
      currentList = rest;
    }
  }

  return makeNumber(count, true);
};

/**
 * inherited-font-size
 * Returns the inherited value of the font-size characteristic.
 * Default: 10pt
 */
export const inheritedFontSizePrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeQuantity(10, 'pt');
};

/**
 * inherited-end-indent
 * Returns the inherited value of the end-indent characteristic.
 * Default: 0pt
 */
export const inheritedEndIndentPrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeQuantity(0, 'pt');
};

/**
 * inherited-line-spacing
 * Returns the inherited value of the line-spacing characteristic.
 * Default: 12pt (120% of 10pt font)
 */
export const inheritedLineSpacingPrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeQuantity(12, 'pt');
};

/**
 * inherited-font-posture
 * Returns the inherited value of the font-posture characteristic.
 * Default: 'upright
 */
export const inheritedFontPosturePrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeSymbol('upright');
};

/**
 * inherited-font-weight
 * Returns the inherited value of the font-weight characteristic.
 * Default: 'medium
 */
export const inheritedFontWeightPrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeSymbol('medium');
};

/**
 * inherited-font-family-name
 * Returns the inherited value of the font-family-name characteristic.
 * Default: "serif"
 */
export const inheritedFontFamilyNamePrimitive: PrimitiveFunction = (
  _args: ELObj[],
  _vm: VM
): ELObj => {
  // TODO: Query actual inherited value from flow object context
  return makeString('serif');
};

/**
 * hierarchical-number-recursive
 * Port from: OpenJade style/primitive.cxx DEFPRIMITIVE(HierarchicalNumberRecursive)
 * DSSSL spec §10.2.3
 *
 * Returns a list of position numbers for all ancestors of the given element
 * that have the specified generic identifier.
 *
 * (hierarchical-number-recursive "section") with current node being the 3rd para
 * in the 2nd section might return (2 3) - meaning 2nd section, 3rd child.
 *
 * Takes: gi (element name), optional node (defaults to current-node)
 */
export const hierarchicalNumberRecursivePrimitive: PrimitiveFunction = (
  args: ELObj[],
  vm: VM
): ELObj => {
  if (args.length < 1) {
    throw new Error('hierarchical-number-recursive requires at least 1 argument');
  }

  // Get the target GI from first argument
  const giArg = args[0].asString() || args[0].asSymbol();
  if (!giArg) {
    throw new Error('hierarchical-number-recursive: first argument must be a string or symbol');
  }
  const targetGi = giArg.toString();

  // Get the node (from args[1] or current-node)
  let node: Node | null = null;
  if (args.length > 1) {
    const nodeListObj = args[1].asNodeList();
    if (!nodeListObj) {
      throw new Error('hierarchical-number-recursive: second argument must be a node-list');
    }
    const nodeList = nodeListObj.nodes;
    node = nodeList.first();
    if (!node || nodeList.rest()?.first()) {
      throw new Error('hierarchical-number-recursive: second argument must be a singleton node-list');
    }
  } else {
    node = vm.currentNode;
    if (!node) {
      throw new Error('hierarchical-number-recursive: no current node');
    }
  }

  // Build list of numbers by walking up ancestors
  const numbers: ELObj[] = [];
  let current: Node | null = node;

  while (current) {
    const parent = current.parent();
    if (!parent) break;

    // Check if this ancestor has the target GI
    if (current.gi() === targetGi) {
      // Count position among siblings
      let position = 1; // 1-based
      let siblingList: NodeList | null = parent.children();

      while (siblingList) {
        const sibling = siblingList.first();
        if (!sibling) break;
        if (sibling === current) break;
        position++;
        siblingList = siblingList.rest();
      }

      numbers.unshift(makeNumber(position, true)); // Prepend (building from leaf to root)
    }

    current = parent;
  }

  // Convert array to Scheme list (build from right to left)
  let result: ELObj = theNilObj;
  for (let i = numbers.length - 1; i >= 0; i--) {
    result = makePair(numbers[i], result);
  }

  return result;
};

/**
 * have-ancestor?
 * Port from: OpenJade style/primitive.cxx DEFPRIMITIVE(IsHaveAncestor)
 * DSSSL spec §10.2.3
 *
 * Returns #t if there is an ancestor with the specified generic identifier.
 *
 * Takes: gi (element name), optional node (defaults to current-node)
 */
export const haveAncestorPredicate: PrimitiveFunction = (
  args: ELObj[],
  vm: VM
): ELObj => {
  if (args.length < 1) {
    throw new Error('have-ancestor? requires at least 1 argument');
  }

  // Get the target GI from first argument
  const giArg = args[0].asString() || args[0].asSymbol();
  if (!giArg) {
    throw new Error('have-ancestor?: first argument must be a string or symbol');
  }
  const targetGi = giArg.toString();

  // Get the node (from args[1] or current-node)
  let node: Node | null = null;
  if (args.length > 1) {
    const nodeListObj = args[1].asNodeList();
    if (!nodeListObj) {
      throw new Error('have-ancestor?: second argument must be a node-list');
    }
    const nodeList = nodeListObj.nodes;
    node = nodeList.first();
    if (!node || nodeList.rest()?.first()) {
      throw new Error('have-ancestor?: second argument must be a singleton node-list');
    }
  } else {
    node = vm.currentNode;
    if (!node) {
      throw new Error('have-ancestor?: no current node');
    }
  }

  // Walk up ancestors looking for matching GI
  let current: Node | null = node.parent();
  while (current) {
    if (current.gi() === targetGi) {
      return theTrueObj;
    }
    current = current.parent();
  }

  return theFalseObj;
};

/**
 * Additional inherited characteristic functions to implement:
 * - element-number-list
 * - inherited-dbhtml-value (extension)
 * - inherited-pi-value (extension)
 * - etc. (see DSSSL spec §12.6 for full list)
 */
