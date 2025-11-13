/**
 * Dazzle Core - TypeScript port of OpenJade's DSSSL processor
 *
 * This is the core library containing:
 * - Grove interfaces (abstract document tree)
 * - FOT Builder interfaces (backend abstraction)
 * - Scheme interpreter (bytecode VM)
 * - DSSSL processing model
 * - Primitives (260 language features)
 */

export * from './grove/index.js';
export * from './fot.js';
export * from './dsssl/index.js';

// Scheme interpreter
export { parse, parseOne } from './scheme/parser.js';
export { Compiler, GlobalEnvironment, Environment } from './scheme/compiler.js';
export { VM } from './scheme/vm.js';
export type { ELObj } from './scheme/elobj.js';
