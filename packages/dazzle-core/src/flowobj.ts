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
    // Default: accept all characteristics (subclasses can override for specific handling)
    return true;
  }

  /**
   * Set a non-inherited characteristic
   * Port from: FlowObj::setNonInheritedC()
   *
   * Default implementation: silently accept characteristics
   * Subclasses override to actually store and use the values
   */
  setNonInheritedC(_name: string, _value: ELObj): void {
    // Default: silently accept but don't use
    // Subclasses override to store and use characteristics
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
    if (process.env.DEBUG_FOT) {
      console.error(`CompoundFlowObj.processInner: content_ is ${this.content_ ? 'set' : 'null'}, type: ${this.content_?.type || 'N/A'}`);
    }
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
           name === 'top-margin' || name === 'bottom-margin' ||
           name === 'page-n-columns' || name === 'input-whitespace-treatment' ||
           name === 'use';
  }

  /**
   * Set characteristic value
   */
  setNonInheritedC(name: string, value: ELObj): void {
    // Handle margin characteristics
    if (name.endsWith('-margin')) {
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
    // TODO: Handle other characteristics (page-n-columns, input-whitespace-treatment, use)
    // For now, silently accept them to allow compilation
  }

  /**
   * Convert characteristic value to string (with unit conversion)
   * Port from: OpenJade - QuantityObj stores values in inches internally
   */
  private characteristicToString(value: ELObj): string | null {
    // Check if it's a quantity (length with unit)
    const qty = value.asQuantity?.();
    if (qty) {
      // Port from: OpenJade stores all quantities as "units per inch" internally
      // After resolution, qty.value is already in inches, so we just convert to points
      const valueInInches = qty.value;
      const pts = valueInInches * 72;  // 1 inch = 72 points

      // Truncate to 3 decimal places (thousandths of a point) to match OpenJade
      const truncated = Math.floor(pts * 1000) / 1000;

      // Remove trailing zeros and unnecessary decimal point
      const formatted = truncated.toString().replace(/\.?0+$/, '');
      return `${formatted}pt`;
    }

    // Handle old code path (before proper unit resolution) - can be removed later
    // This is kept for backwards compatibility with any remaining cases
    if (false) {
      const val = qty!.value;
      const unit = qty!.unit;

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
 * Entity FlowObj - Opens a file for output
 * Port from: OpenJade TransformFOTBuilder entity handling
 *
 * Used by SGML backend for code generation - opens a file specified by system-id.
 */
export class EntityFlowObj extends CompoundFlowObj {
  private systemId: string | null = null;

  /**
   * Set the system-id characteristic (file path)
   */
  setNonInheritedC(name: string, value: ELObj): void {
    if (name === 'system-id') {
      const str = value.asString();
      this.systemId = str ? str.value : null;
    }
  }

  hasNonInheritedC(name: string): boolean {
    return name === 'system-id';
  }

  /**
   * Process this flow object
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    if (fotb.startEntity && this.systemId) {
      fotb.startEntity(this.systemId);
    }

    // Process child content
    super.processInner(context);

    if (fotb.endEntity) {
      fotb.endEntity();
    }
  }

  copy(): FlowObj {
    const copy = new EntityFlowObj();
    copy.systemId = this.systemId;
    copy.content_ = this.content_;
    return copy;
  }
}

/**
 * FormattingInstruction FlowObj - Writes raw text output
 * Port from: OpenJade TransformFOTBuilder formatting-instruction handling
 *
 * Used by SGML backend for code generation - writes raw text to current output stream.
 */
export class FormattingInstructionFlowObj extends FlowObj {
  private dataContent: string | null = null;

  /**
   * Set the data characteristic (text to write)
   */
  setNonInheritedC(name: string, value: ELObj): void {
    if (name === 'data') {
      const str = value.asString();
      this.dataContent = str ? str.value : null;
    }
  }

  hasNonInheritedC(name: string): boolean {
    return name === 'data';
  }

  /**
   * Process this flow object
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    if (fotb.formattingInstruction && this.dataContent) {
      fotb.formattingInstruction(this.dataContent);
    }
  }

  copy(): FlowObj {
    const copy = new FormattingInstructionFlowObj();
    copy.dataContent = this.dataContent;
    return copy;
  }
}

/**
 * Leader FlowObj
 * Port from: OpenJade style/FlowObj.cxx LeaderFlowObj (lines 1293-1369)
 *
 * Creates a leader (line or pattern connecting content, like dots in TOC).
 * Common characteristics: length, break-before-priority, break-after-priority
 */
export class LeaderFlowObj extends CompoundFlowObj {
  // For now, we just accept characteristics without storing them
  // TODO: Implement proper leader rendering with characteristics

  /**
   * Process this flow object
   * Port from: LeaderFlowObj::processInner() line 1319
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Start leader
    if (fotb.startLeader) {
      fotb.startLeader();
    }

    // Process child content
    super.processInner(context);

    // End leader
    if (fotb.endLeader) {
      fotb.endLeader();
    }
  }

  /**
   * Check if has characteristic
   * Port from: LeaderFlowObj::hasNonInheritedC() line 1350
   */
  hasNonInheritedC(name: string): boolean {
    return name === 'length' ||
           name === 'break-before-priority' ||
           name === 'break-after-priority';
  }

  /**
   * Set characteristic value
   * Port from: LeaderFlowObj::setNonInheritedC() line 1327
   */
  setNonInheritedC(_name: string, _value: ELObj): void {
    // TODO: Store and use characteristics
    // For now, silently accept to allow compilation
  }

  copy(): FlowObj {
    const copy = new LeaderFlowObj();
    copy.content_ = this.content_;
    return copy;
  }
}

/**
 * Link FlowObj
 * Port from: OpenJade style/FlowObj.cxx LinkFlowObj (lines 847-911)
 *
 * Creates a link (hyperlink) element. The destination characteristic
 * specifies the target address.
 */
export class LinkFlowObj extends CompoundFlowObj {
  private destination: string | null = null;

  /**
   * Process this flow object
   * Port from: LinkFlowObj::processInner() line 865
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Start link with destination (if provided)
    if (fotb.startLink) {
      fotb.startLink(this.destination || undefined);
    }

    // Process child content
    super.processInner(context);

    // End link
    if (fotb.endLink) {
      fotb.endLink();
    }
  }

  /**
   * Check if has characteristic
   * Port from: LinkFlowObj::hasNonInheritedC() line 890
   */
  hasNonInheritedC(name: string): boolean {
    return name === 'destination';
  }

  /**
   * Set characteristic value
   * Port from: LinkFlowObj::setNonInheritedC() line 896
   */
  setNonInheritedC(name: string, value: ELObj): void {
    if (name === 'destination') {
      // For now, handle destination as string
      // TODO: Implement proper AddressObj support
      const str = value.asString();
      this.destination = str ? str.value : null;
    }
  }

  copy(): FlowObj {
    const copy = new LinkFlowObj();
    copy.destination = this.destination;
    copy.content_ = this.content_;
    return copy;
  }
}

/**
 * Sequence FlowObj
 * Port from: OpenJade style/FlowObj.cxx SequenceFlowObj
 *
 * Generic sequence container - wraps child content without additional semantics.
 * This is the most common flow object in DSSSL output.
 */
export class SequenceFlowObj extends CompoundFlowObj {
  /**
   * Process this flow object
   * Port from: SequenceFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    if (process.env.DEBUG_FOT) {
      console.error('SequenceFlowObj.processInner called');
    }

    // Start sequence
    if (fotb.startSequence) {
      fotb.startSequence();
    }

    // Process child content
    super.processInner(context);

    // End sequence
    if (fotb.endSequence) {
      fotb.endSequence();
    }
  }

  copy(): FlowObj {
    const copy = new SequenceFlowObj();
    copy.content_ = this.content_;
    return copy;
  }
}

/**
 * DisplayGroup FlowObj
 * Port from: OpenJade style/FlowObj.cxx DisplayGroupFlowObj
 *
 * Display block container - like sequence but represents a display-level block.
 * Used for grouping content that should be kept together.
 */
export class DisplayGroupFlowObj extends CompoundFlowObj {
  /**
   * Process this flow object
   * Port from: DisplayGroupFlowObj::processInner()
   */
  protected processInner(context: ProcessContext): void {
    const fotb = context.currentFOTBuilder();

    // Start display group
    if (fotb.startDisplayGroup) {
      fotb.startDisplayGroup();
    }

    // Process child content
    super.processInner(context);

    // End display group
    if (fotb.endDisplayGroup) {
      fotb.endDisplayGroup();
    }
  }

  /**
   * DisplayGroupFlowObj supports various characteristics
   * For now, accept but ignore them (proper implementation would store and use them)
   */
  hasNonInheritedC(name: string): boolean {
    // Accept common display-group characteristics
    return name === 'break-before' || name === 'break-after' ||
           name === 'keep-with-previous?' || name === 'keep-with-next?' ||
           name === 'space-before' || name === 'space-after';
  }

  /**
   * Set characteristic value
   * For now, accept but ignore (proper implementation would store them)
   */
  setNonInheritedC(_name: string, _value: ELObj): void {
    // TODO: Store and use characteristics
    // For now, silently accept to allow compilation
  }

  copy(): FlowObj {
    const copy = new DisplayGroupFlowObj();
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
    case 'sequence':
      return new SequenceFlowObj();
    case 'simple-page-sequence':
      return new SimplePageSequenceFlowObj();
    case 'scroll':
      return new ScrollFlowObj();
    case 'paragraph':
      return new ParagraphFlowObj();
    case 'display-group':
      return new DisplayGroupFlowObj();
    case 'leader':
      return new LeaderFlowObj();
    case 'link':
      return new LinkFlowObj();
    case 'entity':
      return new EntityFlowObj();
    case 'formatting-instruction':
      return new FormattingInstructionFlowObj();
    default:
      return null;
  }
}
