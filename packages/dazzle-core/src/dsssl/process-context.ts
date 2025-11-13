/**
 * DSSSL Processing Context
 * Port from: OpenJade style/ProcessContext.{h,cxx}
 *
 * The ProcessContext manages the execution of sosofos - it traverses
 * the sosofo tree and calls appropriate methods on the FOTBuilder backend.
 */

import type { FotBuilder } from '../fot.js';
import type { VM } from '../scheme/vm.js';
import { SosofoObj } from '../scheme/elobj.js';

/**
 * ProcessContext - manages sosofo processing
 * Port from: ProcessContext.h class ProcessContext
 */
export class ProcessContext {
  constructor(
    private fotBuilder: FotBuilder,
    private vm: VM
  ) {}

  /**
   * Get the current FOT builder
   */
  currentFOTBuilder(): FotBuilder {
    return this.fotBuilder;
  }

  /**
   * Get the VM
   */
  getVM(): VM {
    return this.vm;
  }

  /**
   * Process a sosofo - execute it against the backend
   * Port from: ProcessContext.cxx various SosofoObj::process() methods
   */
  process(sosofo: SosofoObj): void {
    switch (sosofo.type) {
      case 'empty':
        // Empty sosofo - nothing to do
        break;

      case 'literal': {
        // Literal text - call characters() on backend
        const text = sosofo.literalText();
        if (text) {
          this.fotBuilder.characters(text);
        }
        break;
      }

      case 'formatting-instruction': {
        // Raw backend output - call formattingInstruction()
        const data = sosofo.data as { data: string };
        if (data && data.data) {
          this.fotBuilder.formattingInstruction(data.data);
        }
        break;
      }

      case 'entity': {
        // Entity flow object - open file, process content, close file
        const entityData = sosofo.data as { systemId: string; content: any[] };
        if (entityData && entityData.systemId) {
          this.fotBuilder.startEntity(entityData.systemId);

          // Process content (if any)
          // Content items are ELObj values that may be sosofos
          if (entityData.content && Array.isArray(entityData.content)) {
            for (const item of entityData.content) {
              const sosofoItem = item.asSosofo?.();
              if (sosofoItem) {
                this.process(sosofoItem);
              } else {
                throw new Error(`make entity: content must be sosofos, got non-sosofo item`);
              }
            }
          }

          this.fotBuilder.endEntity();
        }
        break;
      }

      case 'append': {
        // Append sosofo - process children in sequence
        const children = sosofo.children();
        for (const child of children) {
          this.process(child);
        }
        break;
      }

      default:
        // Unknown sosofo type - ignore
        break;
    }
  }

  /**
   * Process children of the current node
   * Port from: ProcessContext.cxx ProcessChildrenSosofoObj::process()
   */
  processChildren(mode?: string): void {
    // TODO: Implement full rule-based processing
    // For now, this is a stub
  }
}
