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
  private pendingNodes: Node[] = [];

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

    // Flush pending anchors before outputting text
    // Port from: SgmlFOTBuilder.cxx:633 characters()
    this.flushPendingNodes();

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
   * Start processing a node - add to pending anchors
   * Port from: SgmlFOTBuilder.cxx:2748 startNode()
   */
  startNode(node: Node, _processingMode: string): void {
    // Don't output anchor immediately - add to pending list
    // Anchors will be flushed when content is output
    this.pendingNodes.push(node);
  }

  /**
   * End processing a node
   */
  endNode(): void {
    // Nothing to do - pending anchors are managed by flushPendingNodes()
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
   * Start a leader flow object
   * Port from: OpenJade FOTBuilder
   */
  startLeader(): void {
    this.closeText();
    this.writeIndent();
    this.output += '<leader>\n';
    this.indentLevel++;
  }

  /**
   * End a leader flow object
   */
  endLeader(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</leader>\n';
  }

  /**
   * Start a link flow object
   * Port from: OpenJade FOTBuilder.h:484
   */
  startLink(destination?: string): void {
    this.closeText();
    this.writeIndent();
    this.output += '<link';
    if (destination) {
      this.output += ` destination="${this.escapeXml(destination)}"`;
    }
    this.output += '>\n';
    this.indentLevel++;
  }

  /**
   * End a link flow object
   * Port from: OpenJade FOTBuilder.h:485
   */
  endLink(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</link>\n';
  }

  /**
   * Start a line-field flow object
   * Port from: OpenJade FOTBuilder.h:476
   */
  startLineField(): void {
    this.closeText();
    this.writeIndent();
    this.output += '<line-field>\n';
    this.indentLevel++;
  }

  /**
   * End a line-field flow object
   * Port from: OpenJade FOTBuilder.h:477
   */
  endLineField(): void {
    this.closeText();
    this.indentLevel--;
    this.writeIndent();
    this.output += '</line-field>\n';
  }

  /**
   * Finish output and return result
   */
  end(): void {
    this.closeText();
    this.output += '</fot>';
  }

  /**
   * Get the generated FOT output
   */
  getOutput(): string {
    return this.output;
  }

  /**
   * Flush pending node anchors
   * Port from: SgmlFOTBuilder.cxx:2807 flushPendingElements()
   */
  private flushPendingNodes(): void {
    if (this.pendingNodes.length === 0) {
      return;
    }

    // Output anchor for each pending node
    for (const _node of this.pendingNodes) {
      this.writeIndent();
      this.output += `<a name="${this.anchorCounter}"/>\n`;
      this.anchorCounter++;
    }

    // Clear pending nodes
    this.pendingNodes = [];
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
