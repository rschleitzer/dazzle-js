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
import { callClosure } from '../scheme/primitives.js';
import { NodeType } from '../grove/grove.js';

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
    if (process.env.DEBUG_FOT) {
      console.error(`ProcessContext.process: sosofo type: ${sosofo.type || 'FlowObj'}`);
    }

    // FlowObj sosofos don't have a type - they handle their own processing
    // Port from: OpenJade - FlowObj::process() is called polymorphically
    if (!sosofo.type) {
      // This is a FlowObj - call its process() method
      sosofo.process(this);
      return;
    }

    switch (sosofo.type) {
      case 'empty':
        // Empty sosofo - nothing to do
        break;

      case 'literal': {
        // Literal text - call characters() on backend
        const text = sosofo.literalText();
        if (process.env.DEBUG_FOT) {
          console.error(`Processing literal sosofo, text: "${text}"`);
        }
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

      case 'directory': {
        // Directory flow object - create directory, process content, restore context
        const dirData = sosofo.data as { path: string; content: any[] };
        if (dirData && dirData.path) {
          this.fotBuilder.startDirectory(dirData.path);

          // Process content (if any)
          // Content items are ELObj values that may be sosofos
          if (dirData.content && Array.isArray(dirData.content)) {
            for (const item of dirData.content) {
              const sosofoItem = item.asSosofo?.();
              if (sosofoItem) {
                this.process(sosofoItem);
              } else {
                throw new Error(`make directory: content must be sosofos, got non-sosofo item`);
              }
            }
          }

          this.fotBuilder.endDirectory();
        }
        break;
      }

      case 'append': {
        // Append sosofo - process children in sequence
        const children = sosofo.children();
        if (process.env.DEBUG_FOT) {
          console.error(`Processing append sosofo with ${children.length} children`);
        }
        for (const child of children) {
          if (process.env.DEBUG_FOT) {
            console.error(`  - child type: ${child.type || (child.constructor.name === 'FlowObj' ? 'FlowObj' : child.constructor.name)}`);
          }
          this.process(child);
        }
        break;
      }

      case 'page-number': {
        // Page number sosofo - call pageNumber() on backend
        // Port from: OpenJade ProcessContext.cxx PageNumberSosofoObj::process()
        if (this.fotBuilder.pageNumber) {
          this.fotBuilder.pageNumber();
        }
        break;
      }

      case 'process-children-trim': {
        // Process children with whitespace trimming
        // Port from: OpenJade ProcessContext.cxx ProcessChildrenTrimSosofoObj::process()
        const trimData = sosofo.data as { mode: string };
        const mode = trimData ? trimData.mode : '';
        this.processChildrenTrim(mode);
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
    // Get current node from VM
    const currentNode = this.vm.currentNode;
    if (!currentNode) {
      return;
    }

    // Get children of current node
    const children = currentNode.children();
    if (!children) {
      return;
    }

    // Process each child node
    const processingMode = mode || '';
    const previousNode = this.vm.currentNode;

    for (let i = 0; i < children.length(); i++) {
      const child = children.nth(i);
      if (!child) continue;

      // Set current node in VM
      this.vm.currentNode = child;

      try {
        // Port from: ProcessContext.cxx:66-73 processNode()
        // Check if this is a text node - if so, output characters directly
        const nodeType = child.nodeType();
        if (nodeType === NodeType.Text) {
          const text = child.data();
          if (text) {
            this.fotBuilder.charactersFromNode(child, text);
          }
          continue;
        }

        // Element node - call startNode, match rules, call endNode
        // Port from: ProcessContext.cxx:80
        this.fotBuilder.startNode(child, processingMode);

        try {
          // Find rule for this element
          const elementName = child.gi() || '';
          const rule = this.vm.globals.ruleRegistry.findRule(elementName, processingMode, this.vm.globals);

          if (rule) {
            // Execute rule to get sosofo
            const func = rule.func!.asFunction();
            if (!func || !func.isClosure()) {
              throw new Error(`Rule for '${elementName}' must be a function`);
            }

            // Call the rule with no arguments
            const sosofo = callClosure(func, [], this.vm);
            const sosofoObj = sosofo?.asSosofo();
            if (sosofoObj) {
              this.process(sosofoObj);
            }
          } else {
            // Port from: OpenJade - If no rule found, process children recursively
            // This ensures text content is still output even without explicit rules
            this.processChildren(processingMode);
          }
        } finally {
          this.fotBuilder.endNode();
        }
      } finally {
        // Restore previous node
        this.vm.currentNode = previousNode;
      }
    }
  }

  /**
   * Process children of the current node with whitespace trimming
   * Port from: ProcessContext.cxx ProcessChildrenTrimSosofoObj::process()
   *
   * Like processChildren, but trims leading/trailing whitespace from text nodes.
   */
  processChildrenTrim(mode?: string): void {
    // Get current node from VM
    const currentNode = this.vm.currentNode;
    if (!currentNode) {
      return;
    }

    // Get children of current node
    const children = currentNode.children();
    if (!children) {
      return;
    }

    // Process each child node
    const processingMode = mode || '';
    const previousNode = this.vm.currentNode;

    // Track if we've seen any non-whitespace content yet
    let seenContent = false;
    const childCount = children.length();

    // First pass: collect text nodes and determine if they should be trimmed
    const textNodes: Array<{ index: number; text: string; isFirst: boolean; isLast: boolean }> = [];
    for (let i = 0; i < childCount; i++) {
      const child = children.nth(i);
      if (!child) continue;

      const nodeType = child.nodeType();
      if (nodeType === NodeType.Text) {
        const text = child.data() || '';
        const isFirst = i === 0;
        const isLast = i === childCount - 1;
        textNodes.push({ index: i, text, isFirst, isLast });
      }
    }

    for (let i = 0; i < childCount; i++) {
      const child = children.nth(i);
      if (!child) continue;

      // Set current node in VM
      this.vm.currentNode = child;

      try {
        // Port from: ProcessContext.cxx:66-73 processNode()
        // Check if this is a text node - if so, output characters with trimming
        const nodeType = child.nodeType();
        if (nodeType === NodeType.Text) {
          let text = child.data() || '';

          // Find this text node in our list
          const textInfo = textNodes.find(t => t.index === i);
          if (textInfo) {
            // Trim leading whitespace from first text node
            if (textInfo.isFirst && !seenContent) {
              text = text.replace(/^\s+/, '');
            }
            // Trim trailing whitespace from last text node
            if (textInfo.isLast) {
              text = text.replace(/\s+$/, '');
            }
          }

          if (text.length > 0) {
            this.fotBuilder.charactersFromNode(child, text);
            seenContent = true;
          }
          continue;
        }

        // Element node - call startNode, match rules, call endNode
        // Port from: ProcessContext.cxx:80
        this.fotBuilder.startNode(child, processingMode);

        try {
          // Find rule for this element
          const elementName = child.gi() || '';
          const rule = this.vm.globals.ruleRegistry.findRule(elementName, processingMode, this.vm.globals);

          if (rule) {
            // Execute rule to get sosofo
            const func = rule.func!.asFunction();
            if (!func || !func.isClosure()) {
              throw new Error(`Rule for '${elementName}' must be a function`);
            }

            // Call the rule with no arguments
            const sosofo = callClosure(func, [], this.vm);
            const sosofoObj = sosofo?.asSosofo();
            if (sosofoObj) {
              this.process(sosofoObj);
              seenContent = true;
            }
          } else {
            // Port from: OpenJade - If no rule found, process children recursively
            // This ensures text content is still output even without explicit rules
            this.processChildrenTrim(processingMode);
          }
        } finally {
          this.fotBuilder.endNode();
        }
      } finally {
        // Restore previous node
        this.vm.currentNode = previousNode;
      }
    }
  }
}
