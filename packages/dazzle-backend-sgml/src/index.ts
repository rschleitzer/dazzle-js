/**
 * Transform Backend for Dazzle - Code generation output engine
 *
 * Port from: OpenJade jade/TransformFOTBuilder.cxx (641 lines)
 * This is the actual `-t sgml` backend for code generation,
 * NOT the SgmlFOTBuilder (which outputs FOT as SGML/XML).
 *
 * Key flow objects:
 * - entity (system-id): Opens a file for writing
 * - formatting-instruction (data): Writes raw text to current stream
 * - characters: Writes text with proper escaping
 */

import type { FotBuilder, Node } from 'dazzle-core';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Output stream abstraction
 * Port from: OpenJade OutputCharStream
 */
interface OutputStream {
  write(data: string): void;
  flush(): void;
  close(): void;
}

/**
 * Standard output stream (writes to stdout)
 */
class StdoutStream implements OutputStream {
  write(data: string): void {
    process.stdout.write(data);
  }

  flush(): void {
    // Node.js stdout auto-flushes
  }

  close(): void {
    // Don't close stdout
  }
}

/**
 * File output stream
 */
class FileStream implements OutputStream {
  private fd: number | null = null;
  private buffer: string = '';

  constructor(public readonly filePath: string) {}

  open(): boolean {
    try {
      // Ensure directory exists
      const dir = path.dirname(this.filePath);
      if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
      }

      // Open file for writing
      this.fd = fs.openSync(this.filePath, 'w');
      return true;
    } catch (err) {
      console.error(`Error opening file ${this.filePath}:`, err);
      return false;
    }
  }

  write(data: string): void {
    if (this.fd !== null) {
      this.buffer += data;
    }
  }

  flush(): void {
    if (this.fd !== null && this.buffer) {
      fs.writeSync(this.fd, this.buffer, undefined, 'utf-8');
      this.buffer = '';
    }
  }

  close(): void {
    if (this.fd !== null) {
      this.flush();
      fs.closeSync(this.fd);
      this.fd = null;
    }
  }
}

/**
 * Open file entry (for stack)
 * Port from: TransformFOTBuilder.cxx OpenFile struct
 */
interface OpenFile {
  systemId: string;
  stream: FileStream;
  savedStream: OutputStream;
}

/**
 * Transform FOT Builder - Code generation backend
 *
 * Port from: OpenJade jade/TransformFOTBuilder.cxx
 */
export class TransformFotBuilder implements FotBuilder {
  private currentStream: OutputStream;
  private defaultStream: OutputStream;
  private fileStack: OpenFile[] = [];
  private outputDir: string = '.';

  /**
   * Create Transform backend
   * Port from: TransformFOTBuilder::TransformFOTBuilder
   *
   * @param outputDir Base directory for output files (default: '.')
   * @param stream Output stream (default: stdout)
   */
  constructor(outputDir: string = '.', stream?: OutputStream) {
    this.outputDir = outputDir;
    this.defaultStream = stream || new StdoutStream();
    this.currentStream = this.defaultStream;
  }

  /**
   * Output character data
   * Port from: TransformFOTBuilder.cxx:443
   */
  characters(data: string): void {
    if (!data) return;
    this.currentStream.write(data);
  }

  /**
   * Output character data from a specific node
   * Port from: TransformFOTBuilder.cxx:431
   * Default implementation just calls characters()
   */
  charactersFromNode(_node: Node, data: string): void {
    this.characters(data);
  }

  /**
   * Formatting instruction - backend-specific raw output
   * Port from: TransformFOTBuilder.cxx:471
   *
   * For code generation, this writes text directly to the current stream.
   */
  formattingInstruction(data: string): void {
    this.currentStream.write(data);
  }

  /**
   * Start a sequence flow object
   */
  startSequence(): void {
    // Sequences don't need special handling in transform backend
  }

  /**
   * End a sequence flow object
   */
  endSequence(): void {
    // Sequences don't need special handling in transform backend
  }

  /**
   * Start processing a node
   */
  startNode(_node: Node, _processingMode: string): void {
    // Node tracking for debugging/error messages
  }

  /**
   * End processing a node
   */
  endNode(): void {
    // Node tracking cleanup
  }

  /**
   * Start an entity flow object (opens a file)
   * Port from: TransformFOTBuilder.cxx:483
   *
   * This is the key flow object for code generation.
   * Opens a file and switches output to it.
   */
  startEntity(systemId: string): void {
    const filePath = path.resolve(this.outputDir, systemId);
    const fileStream = new FileStream(filePath);

    if (!fileStream.open()) {
      // If file open fails, continue with current stream
      return;
    }

    // Push current stream to stack
    const openFile: OpenFile = {
      systemId,
      stream: fileStream,
      savedStream: this.currentStream,
    };
    this.fileStack.push(openFile);

    // Switch to new file stream
    this.currentStream = fileStream;
  }

  /**
   * End an entity flow object (closes file)
   * Port from: TransformFOTBuilder.cxx:514
   *
   * Closes the current file and restores previous stream.
   */
  endEntity(): void {
    if (this.fileStack.length === 0) {
      throw new Error('endEntity called without matching startEntity');
    }

    // Pop file from stack
    const openFile = this.fileStack.pop()!;

    // Close file
    openFile.stream.close();

    // Restore previous stream
    this.currentStream = openFile.savedStream;
  }

  /**
   * Finish output and cleanup
   * Port from: TransformFOTBuilder destructor
   */
  end(): void {
    // Close any remaining open files (shouldn't happen in well-formed stylesheets)
    while (this.fileStack.length > 0) {
      this.endEntity();
    }

    // Flush default stream
    this.defaultStream.flush();
  }
}

/**
 * Create a Transform backend
 * Factory function matching OpenJade pattern
 * Port from: makeTransformFOTBuilder
 */
export function createTransformBackend(outputDir: string = '.', stream?: OutputStream): FotBuilder {
  return new TransformFotBuilder(outputDir, stream);
}
