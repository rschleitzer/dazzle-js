/**
 * FOT (Flow Object Tree) Backend for Dazzle - XML output of flow objects
 *
 * Port from: OpenJade jade/SgmlFOTBuilder.cxx
 * This outputs the Flow Object Tree in XML format for debugging and comparison.
 *
 * Goal: Byte-for-byte identical output to OpenJade's `-t fot` backend
 *
 * Key flow objects:
 * - simple-page-sequence: Page formatting container
 * - scroll: Continuous layout (no page breaks)
 * - paragraph: Block-level text
 * - text: Leaf text content
 * - sequence: Generic container
 */

import type { FotBuilder, Node } from 'dazzle-core';

/**
 * FOT Builder - Outputs flow object tree as XML
 *
 * Port from: OpenJade jade/SgmlFOTBuilder.cxx
 */
export class FotBackend implements FotBuilder {
  private output: string = '';
  private indentLevel: number = 0;
  private inText: boolean = false;
  private anchorCounter: number = 0;

  constructor() {
    // Write XML header and root element
    this.output += '<?xml version="1.0"?>\n';
    this.output += '<fot>\n';
  }

  /**
   * Output character data
   */
  characters(data: string): void {
    if (!data) return;

    // If not already in a text element, start one
    if (!this.inText) {
      this.writeIndent();
      this.output += '<text>';
      this.inText = true;
    }

    // Escape XML special characters
    this.output += this.escapeXml(data);
  }

  /**
   * Output character data from a specific node
   */
  charactersFromNode(_node: Node, data: string): void {
    this.characters(data);
  }

  /**
   * Formatting instruction - not used in FOT output
   */
  formattingInstruction(_data: string): void {
    // FOT backend doesn't use formatting instructions
  }

  /**
   * Start a sequence flow object
   */
  startSequence(): void {
    this.closeText();
    this.writeIndent();
    this.output += '<sequence>\n';
    this.indentLevel++;
  }

  /**
   * End a sequence flow object
   */
  endSequence(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</sequence>\n';
  }

  /**
   * Start processing a node - generate anchor
   */
  startNode(_node: Node, _processingMode: string): void {
    this.closeText();
    this.writeIndent();
    this.output += `<a name="${this.anchorCounter}"/>\n`;
    this.anchorCounter++;
  }

  /**
   * End processing a node
   */
  endNode(): void {
    // Nothing to do
  }

  /**
   * Start an entity flow object - not used in FOT output
   */
  startEntity(_systemId: string): void {
    // FOT backend doesn't handle entity file output
  }

  /**
   * End an entity flow object
   */
  endEntity(): void {
    // FOT backend doesn't handle entity file output
  }

  /**
   * Start a directory flow object - not used in FOT output
   */
  startDirectory(_path: string): void {
    // FOT backend doesn't handle directories
  }

  /**
   * End a directory flow object
   */
  endDirectory(): void {
    // FOT backend doesn't handle directories
  }

  /**
   * Start a simple-page-sequence flow object
   */
  startSimplePageSequence(properties?: Record<string, string>): void {
    this.closeText();
    this.writeIndent();
    this.output += '<simple-page-sequence';
    if (properties) {
      for (const [key, value] of Object.entries(properties)) {
        this.output += ` ${key}="${value}"`;
      }
    }
    this.output += '>\n';
    this.indentLevel++;
  }

  /**
   * End a simple-page-sequence flow object
   */
  endSimplePageSequence(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</simple-page-sequence>\n';
  }

  /**
   * Start a scroll flow object
   */
  startScroll(): void {
    this.closeText();
    this.writeIndent();
    this.output += '<scroll>\n';
    this.indentLevel++;
  }

  /**
   * End a scroll flow object
   */
  endScroll(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</scroll>\n';
  }

  /**
   * Start a paragraph flow object
   */
  startParagraph(properties?: Record<string, string>): void {
    this.closeText();
    this.writeIndent();
    this.output += '<paragraph';
    if (properties) {
      for (const [key, value] of Object.entries(properties)) {
        this.output += ` ${key}="${value}"`;
      }
    }
    this.output += '>\n';
    this.indentLevel++;
  }

  /**
   * End a paragraph flow object
   */
  endParagraph(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</paragraph>\n';
  }

  /**
   * Finish output and return result
   */
  end(): void {
    this.closeText();
    this.output += '</fot>\n';
  }

  /**
   * Get the generated FOT output
   */
  getOutput(): string {
    return this.output;
  }

  /**
   * Close any open text element
   */
  private closeText(): void {
    if (this.inText) {
      this.output += '</text>\n';
      this.inText = false;
    }
  }

  /**
   * Write indentation
   */
  private writeIndent(): void {
    // No indentation in OpenJade FOT output - it's flat
    // Keeping this method for potential future use
  }

  /**
   * Escape XML special characters
   */
  private escapeXml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }
}

/**
 * Create a FOT backend
 * Factory function matching OpenJade pattern
 */
export function createFotBackend(): FotBuilder {
  return new FotBackend();
}
