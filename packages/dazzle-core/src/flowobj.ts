/**
 * Flow Object System
 * Port from: OpenJade style/FlowObj.cxx (3,082 lines)
 *
 * Flow objects are created by the `make` primitive and represent
 * formatted output elements. They process content and call FOTBuilder
 * methods to generate output.
 */

import type { ELObj } from './scheme/elobj';
import type { VM } from './scheme/vm';
import type { FotBuilder } from './fot';

/**
 * Process context - manages processing state
 * Port from: OpenJade style/ProcessContext.h
 */
export class ProcessContext {
  private fotBuilder: FotBuilder;
  private vm: VM;

  constructor(fotBuilder: FotBuilder, vm: VM) {
    this.fotBuilder = fotBuilder;
    this.vm = vm;
  }

  currentFOTBuilder(): FotBuilder {
    return this.fotBuilder;
  }

  getVM(): VM {
    return this.vm;
  }
}

/**
 * Base FlowObj class
 * Port from: OpenJade style/FlowObj.cxx FlowObj class
 *
 * All flow objects inherit from this base class.
 */
export abstract class FlowObj {
  protected hasSubObjects: boolean = false;

  /**
   * Process this flow object
   * Port from: FlowObj::process()
   */
  process(context: ProcessContext): void {
    this.processInner(context);
  }

  /**
   * Process the inner content of this flow object
   * Must be implemented by subclasses
   * Port from: FlowObj::processInner()
   */
  protected abstract processInner(context: ProcessContext): void;

  /**
   * Check if this flow object has a non-inherited characteristic
   * Port from: FlowObj::hasNonInheritedC()
   */
  hasNonInheritedC(_name: string): boolean {
    return false;
  }

  /**
   * Set a non-inherited characteristic
   * Port from: FlowObj::setNonInheritedC()
   */
  setNonInheritedC(_name: string, _value: ELObj): void {
    throw new Error('setNonInheritedC not implemented for this flow object');
  }

  /**
   * Copy this flow object
   */
  abstract copy(): FlowObj;
}

/**
 * Compound FlowObj - has child content
 * Port from: OpenJade style/FlowObj.cxx CompoundFlowObj class
 */
export abstract class CompoundFlowObj extends FlowObj {
  protected content: ELObj | null = null;

  constructor() {
    super();
    this.hasSubObjects = true;
  }

  /**
   * Set the content (child sosofo) of this flow object
   */
  setContent(content: ELObj): void {
    this.content = content;
  }

  /**
   * Process the inner content
   * Port from: CompoundFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    if (this.content) {
      // Process child content
      const sosofo = this.content.asSosofo();
      if (sosofo) {
        sosofo.process(context);
      }
    }
  }
}

/**
 * SimplePageSequence FlowObj
 * Port from: OpenJade style/FlowObj.cxx SimplePageSequenceFlowObj
 *
 * Creates a page sequence for formatted output.
 * Simplified version without header/footer support initially.
 */
export class SimplePageSequenceFlowObj extends CompoundFlowObj {
  private leftMargin: string | null = null;
  private rightMargin: string | null = null;
  private topMargin: string | null = null;
  private bottomMargin: string | null = null;

  /**
   * Process this flow object
   * Port from: SimplePageSequenceFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Collect properties
    const properties: Record<string, string> = {};
    if (this.leftMargin) properties['left-margin'] = this.leftMargin;
    if (this.rightMargin) properties['right-margin'] = this.rightMargin;
    if (this.topMargin) properties['top-margin'] = this.topMargin;
    if (this.bottomMargin) properties['bottom-margin'] = this.bottomMargin;

    // Start simple page sequence
    if (fotb.startSimplePageSequence) {
      fotb.startSimplePageSequence(properties);
    }

    // Process child content
    super.processInner(context);

    // End simple page sequence
    if (fotb.endSimplePageSequence) {
      fotb.endSimplePageSequence();
    }
  }

  /**
   * Check if has characteristic
   */
  hasNonInheritedC(name: string): boolean {
    return name === 'left-margin' || name === 'right-margin' ||
           name === 'top-margin' || name === 'bottom-margin';
  }

  /**
   * Set characteristic value
   */
  setNonInheritedC(name: string, value: ELObj): void {
    const strValue = this.characteristicToString(value);
    if (!strValue) return;

    switch (name) {
      case 'left-margin':
        this.leftMargin = strValue;
        break;
      case 'right-margin':
        this.rightMargin = strValue;
        break;
      case 'top-margin':
        this.topMargin = strValue;
        break;
      case 'bottom-margin':
        this.bottomMargin = strValue;
        break;
    }
  }

  /**
   * Convert characteristic value to string (with unit conversion)
   */
  private characteristicToString(value: ELObj): string | null {
    // Check if it's a quantity (length with unit)
    const qty = value.asQuantity?.();
    if (qty) {
      // Convert to points (OpenJade standard unit)
      // 1cm = 28.3464pt, 1in = 72pt, 1mm = 2.83464pt
      const val = qty.value;
      const unit = qty.unit;

      let pts: number;
      switch (unit) {
        case 'cm':
          pts = val * 28.3464;
          break;
        case 'mm':
          pts = val * 2.83464;
          break;
        case 'in':
          pts = val * 72;
          break;
        case 'pt':
          pts = val;
          break;
        default:
          return null;
      }

      return `${pts}pt`;
    }

    // Try as number (assume points)
    const num = value.asNumber();
    if (num !== null) {
      return `${num}pt`;
    }

    return null;
  }

  copy(): FlowObj {
    const copy = new SimplePageSequenceFlowObj();
    copy.leftMargin = this.leftMargin;
    copy.rightMargin = this.rightMargin;
    copy.topMargin = this.topMargin;
    copy.bottomMargin = this.bottomMargin;
    copy.content = this.content;
    return copy;
  }
}

/**
 * Scroll FlowObj
 * Port from: OpenJade style/FlowObj.cxx ScrollFlowObj
 *
 * Continuous layout without page breaks.
 */
export class ScrollFlowObj extends CompoundFlowObj {
  /**
   * Process this flow object
   * Port from: ScrollFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Start scroll
    if (fotb.startScroll) {
      fotb.startScroll();
    }

    // Process child content
    super.processInner(context);

    // End scroll
    if (fotb.endScroll) {
      fotb.endScroll();
    }
  }

  copy(): FlowObj {
    const copy = new ScrollFlowObj();
    copy.content = this.content;
    return copy;
  }
}

/**
 * Paragraph FlowObj
 * Port from: OpenJade style/FlowObj.cxx ParagraphFlowObj
 *
 * Block-level paragraph element.
 */
export class ParagraphFlowObj extends CompoundFlowObj {
  /**
   * Process this flow object
   * Port from: ParagraphFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Start paragraph
    if (fotb.startParagraph) {
      fotb.startParagraph();
    }

    // Process child content
    super.processInner(context);

    // End paragraph
    if (fotb.endParagraph) {
      fotb.endParagraph();
    }
  }

  copy(): FlowObj {
    const copy = new ParagraphFlowObj();
    copy.content = this.content;
    return copy;
  }
}

/**
 * Flow object factory
 * Creates flow objects by class name
 */
export function createFlowObj(className: string): FlowObj | null {
  switch (className) {
    case 'simple-page-sequence':
      return new SimplePageSequenceFlowObj();
    case 'scroll':
      return new ScrollFlowObj();
    case 'paragraph':
      return new ParagraphFlowObj();
    default:
      return null;
  }
}
