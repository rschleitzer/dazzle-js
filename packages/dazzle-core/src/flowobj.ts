/**
 * Flow Object System
 * Port from: OpenJade style/FlowObj.cxx (3,082 lines)
 *
 * Flow objects are created by the `make` primitive and represent
 * formatted output elements. They process content and call FOTBuilder
 * methods to generate output.
 */

import { SosofoObj, type ELObj } from './scheme/elobj.js';
import type { ProcessContext } from './dsssl/process-context.js';

/**
 * Base FlowObj class
 * Port from: OpenJade style/SosofoObj.h:111 class FlowObj : public SosofoObj
 *
 * In OpenJade, FlowObj extends SosofoObj - flow objects ARE sosofos.
 * All flow objects inherit from this base class.
 */
export abstract class FlowObj extends SosofoObj {
  protected hasSubObjects: boolean = false;

  /**
   * Return this as a sosofo (it already is one)
   */
  asSosofo(): SosofoObj {
    return this;
  }

  /**
   * Process this flow object
   * Port from: FlowObj.cxx:32 FlowObj::process()
   */
  process(context: ProcessContext): void {
    // OpenJade calls: context.startFlowObj(), pushStyle(), processInner(), popStyle(), endFlowObj()
    // For now, simplified to just processInner()
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
 * Port from: OpenJade style/SosofoObj.h:135 class CompoundFlowObj : public FlowObj
 */
export abstract class CompoundFlowObj extends FlowObj {
  protected content_: SosofoObj | null = null;

  constructor() {
    super();
    this.hasSubObjects = true;
  }

  /**
   * Set the content (child sosofo) of this flow object
   * Port from: SosofoObj.h:140 void setContent(SosofoObj *content)
   */
  setContent(content: SosofoObj): void {
    this.content_ = content;
  }

  /**
   * Process the inner content
   * Port from: FlowObj.cxx:183 CompoundFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    if (this.content_) {
      this.content_.process(context);
    } else {
      // No explicit content - process children of current node
      // Port from: FlowObj.cxx:188 context.processChildren(...)
      context.processChildren();
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
    copy.content_ = this.content_;
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
    copy.content_ = this.content_;
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
    copy.content_ = this.content_;
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
