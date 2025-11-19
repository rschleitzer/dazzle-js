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

  /**
   * Start an entity flow object (file output for code generation)
   * Port from: TransformFOTBuilder::startEntity()
   */
  startEntity(systemId: string): void;

  /**
   * End an entity flow object
   * Port from: TransformFOTBuilder::endEntity()
   */
  endEntity(): void;

  /**
   * Start a directory flow object (directory creation for code generation)
   * Custom extension for file system structure generation
   */
  startDirectory(path: string): void;

  /**
   * End a directory flow object
   * Custom extension for file system structure generation
   */
  endDirectory(): void;

  /**
   * Finish output and cleanup
   * Port from: FOTBuilder destructor
   */
  end(): void;

  // ============ Print Flow Objects (optional) ============
  // Port from: OpenJade jade/SgmlFOTBuilder.cxx
  // These are used by print backends (FOT, RTF, PDF) but not by SGML backend

  /**
   * Start a simple-page-sequence flow object
   * Port from: SgmlFOTBuilder.cxx
   */
  startSimplePageSequence?(properties?: Record<string, string>): void;

  /**
   * End a simple-page-sequence flow object
   */
  endSimplePageSequence?(): void;

  /**
   * Start a scroll flow object
   * Port from: SgmlFOTBuilder.cxx
   */
  startScroll?(): void;

  /**
   * End a scroll flow object
   */
  endScroll?(): void;

  /**
   * Start a paragraph flow object
   * Port from: SgmlFOTBuilder.cxx
   */
  startParagraph?(properties?: Record<string, string>): void;

  /**
   * End a paragraph flow object
   */
  endParagraph?(): void;

  /**
   * Start a display-group flow object
   * Port from: OpenJade FOTBuilder.h
   */
  startDisplayGroup?(properties?: Record<string, string>): void;

  /**
   * End a display-group flow object
   */
  endDisplayGroup?(): void;

  /**
   * Page number sosofo - outputs current page number
   * Port from: OpenJade ProcessContext.cxx PageNumberSosofoObj::process()
   * Port from: OpenJade FOTBuilder.h currentNodePageNumber
   */
  pageNumber?(): void;

  /**
   * Get the generated output (for backends that buffer output)
   * Used by FOT backend to retrieve the complete XML output
   */
  getOutput?(): string;
}
