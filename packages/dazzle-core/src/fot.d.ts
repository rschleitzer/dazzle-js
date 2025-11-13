/**
 * FOT (Flow Object Tree) Builder interface
 * Direct port from OpenJade style/FOTBuilder.h
 *
 * The FOT builder is the backend abstraction that receives flow objects
 * from the DSSSL processor and generates output.
 *
 * This is the minimal interface needed for SGML backend (code generation).
 * Document formatting backends (RTF, PDF) will extend this.
 */
import type { Node } from './grove';
/**
 * FOTBuilder interface - Backend abstraction for output generation
 * Port from: style/FOTBuilder.h (lines 460-803)
 *
 * For SGML backend (code generation), we need these core methods.
 */
export interface FotBuilder {
    /**
     * Output character data
     * Port from: FOTBuilder.h:460
     */
    characters(data: string): void;
    /**
     * Output character data from a specific node
     * Port from: FOTBuilder.h:464
     * Default implementation can just call characters()
     */
    charactersFromNode(node: Node, data: string): void;
    /**
     * Formatting instruction - backend-specific raw output
     * Port from: FOTBuilder.h:472
     * "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction"
     */
    formattingInstruction(data: string): void;
    /**
     * Start a sequence flow object
     * Port from: FOTBuilder.h:474
     */
    startSequence(): void;
    /**
     * End a sequence flow object
     * Port from: FOTBuilder.h:475
     */
    endSequence(): void;
    /**
     * Start processing a node
     * Port from: FOTBuilder.h:802
     */
    startNode(node: Node, processingMode: string): void;
    /**
     * End processing a node
     * Port from: FOTBuilder.h:803
     */
    endNode(): void;
}
//# sourceMappingURL=fot.d.ts.map