/**
 * Scheme Primitives - Basic built-in functions
 * Port from: OpenJade style/primitive.cxx (5,704 lines)
 *
 * Starting with essential R4RS primitives to make the VM usable.
 * Will expand to full 260 primitives as needed.
 */
import { FunctionObj } from './elobj.js';
/**
 * Standard primitive registry
 * Maps primitive names to function objects
 */
export declare const standardPrimitives: Record<string, FunctionObj>;
/**
 * Get a primitive by name
 */
export declare function getPrimitive(name: string): FunctionObj | undefined;
//# sourceMappingURL=primitives.d.ts.map