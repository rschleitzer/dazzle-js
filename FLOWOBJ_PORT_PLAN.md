# FlowObj System Port Plan

## Overview

Port OpenJade's complete flow object system from C++ to TypeScript to achieve byte-for-byte identical FOT output.

## Source Files to Port

### Core Files
- **`style/FlowObj.cxx`** - 3,082 lines - Flow object classes and processing
- **`style/FOTBuilder.h`** - ~1,500 lines - Backend interface and NIC structures
- **`style/Style.cxx`** - Characteristic (property) system
- **`style/primitive.cxx`** - `make` primitive implementation

### Architecture Layers

```
DSSSL Code: (make simple-page-sequence left-margin: 10cm ...)
     ↓
Interpreter: make primitive evaluates characteristics
     ↓
FlowObj: SimplePageSequenceFlowObj stores characteristics
     ↓
ProcessContext: Calls flowObj.processInner()
     ↓
FOTBuilder: startSimplePageSequence(characteristics)
     ↓
Backend: SgmlFOTBuilder outputs <simple-page-sequence left-margin="283.464pt">
```

## Components to Port

### 1. Characteristic System (~500 lines)

**Inherited Characteristics** (from Style.cxx):
- Margins: `left-margin`, `right-margin`, `top-margin`, `bottom-margin`
- Font: `font-size`, `font-family-name`, `font-weight`, `font-posture`
- Line: `line-spacing`, `quadding` (alignment)
- Page: `page-width`, `page-height`
- Colors, spacing, etc.

**Non-Inherited Characteristics** (NIC structures in FOTBuilder.h):
- `DisplayNIC`: `space-before`, `space-after`, `keep-with-next`, etc.
- `ParagraphNIC`: paragraph-specific properties
- `SimplePageSequenceNIC`: page-specific properties

### 2. Base FlowObj Classes (~300 lines)

```typescript
abstract class FlowObj {
  abstract processInner(context: ProcessContext): void;
  hasNonInheritedC(ident: Identifier): boolean;
  setNonInheritedC(ident: Identifier, value: ELObj): void;
}

abstract class CompoundFlowObj extends FlowObj {
  content: SosofoObj | null;
  // Processes child content
}
```

### 3. Specific Flow Objects (~800 lines)

For demo.dsl and sps_css.dsl, we need:

**SimplePageSequenceFlowObj** (~150 lines):
- Header/footer support (6 parts: left/center/right × header/footer)
- Page type handling (first/last/odd/even pages)
- Characteristic handling for margins

**ScrollFlowObj** (~50 lines):
- Continuous layout (no page breaks)
- Simple wrapper around child content

**ParagraphFlowObj** (~100 lines):
- Block-level text container
- Handles alignment, spacing, indentation
- Font properties

### 4. ProcessContext (~200 lines)

Manages processing state:
- Current FOTBuilder
- Style stack (inherited characteristics)
- Page type state
- Principal port stack (for headers/footers)

### 5. FOTBuilder Interface Updates (~200 lines)

Port exact OpenJade signatures:

```typescript
interface FOTBuilder {
  // Simple page sequence
  startSimplePageSequence(headerFooter: FOTBuilder[]): void;
  endSimplePageSequenceHeaderFooter(): void;
  endSimplePageSequence(): void;

  // Paragraph
  startParagraph(nic: ParagraphNIC): void;
  endParagraph(): void;

  // Scroll
  startScroll(): void;
  endScroll(): void;

  // Characteristics must be passed to each flow object start method
}
```

### 6. Updated `make` Primitive (~300 lines)

Currently in primitives.ts, needs major update:
- Parse flow object class name
- Parse characteristics (keyword arguments)
- Create appropriate FlowObj instance
- Set characteristics
- Return as SosofoObj
- Process child content

### 7. SgmlFOTBuilder (FOT Backend) (~500 lines)

Port from `jade/SgmlFOTBuilder.cxx`:
- Receive flow objects with characteristics
- Convert characteristics to XML attributes
- Output flow object tree as XML
- Handle unit conversions (cm → pt, etc.)

## Test Cases

### demo.dsl - Basic Flow Objects (no characteristics)
```scheme
(root (make simple-page-sequence (make scroll)))
(element p (make paragraph))
```

Expected output: 16 lines, no characteristics

### sps_css.dsl - With Characteristics
```scheme
(element prova
  (make simple-page-sequence
    left-margin: 10cm
    top-margin: 10cm))
```

Expected output: 8 lines, margins converted to pt

## Phased Implementation Plan

### Phase 1: Minimal FlowObj System (Week 1-2)
- [ ] Port base FlowObj and CompoundFlowObj classes
- [ ] Port SimplePageSequenceFlowObj (no header/footer support yet)
- [ ] Port ScrollFlowObj
- [ ] Port ParagraphFlowObj
- [ ] Basic ProcessContext
- [ ] Update `make` primitive to create flow objects

### Phase 2: Characteristic System (Week 2-3)
- [ ] Port inherited characteristic definitions
- [ ] Port NIC structures
- [ ] Implement characteristic evaluation in `make`
- [ ] Pass characteristics to FOTBuilder methods

### Phase 3: FOT Backend (Week 3-4)
- [ ] Port SgmlFOTBuilder
- [ ] Implement XML output with attributes
- [ ] Unit conversion (cm → pt, etc.)
- [ ] XML escaping

### Phase 4: Integration & Testing (Week 4)
- [ ] Integrate with CLI (`-t fot`)
- [ ] Test demo.dsl (byte-for-byte match)
- [ ] Test sps_css.dsl (byte-for-byte match)
- [ ] Debug and fix differences

## Estimated Effort

- **Total Lines to Port**: ~3,500-4,000 lines of C++ → ~3,000-3,500 lines of TypeScript
- **Time Estimate**: 3-4 weeks part-time (assuming 10-15 hours/week)
- **Complexity**: High - requires deep understanding of DSSSL processing model

## Alternative: Incremental Approach

Start with simplified implementation for test cases only:

### Simplified Phase 1 (Week 1)
- [ ] Hardcode flow objects as special cases in `make` primitive
- [ ] Pass simple property dictionaries instead of NIC structures
- [ ] Minimal FOT backend that just outputs XML

This gets us to working test cases faster, but diverges from OpenJade architecture.

## Recommendation

Given the scope, I recommend:
1. **Start with full port** (proper approach for long-term maintainability)
2. **Focus on test case flow objects first** (SimplePageSequence, Scroll, Paragraph)
3. **Implement characteristics incrementally** (start with margins only)
4. **Expand to full DSSSL spec later** (tables, multi-column, etc.)

This balances correctness with practical progress.
