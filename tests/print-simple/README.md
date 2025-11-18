# Simple Print Flow Object Test Cases

Source: OpenJade `/dsssl/examples/`

These are minimal test cases to understand and implement DSSSL print flow objects.

## Test Case 1: demo

**Files**: `demo.sgm`, `demo.dsl`, `test-demo.sh`

**Input** (demo.sgm):
```xml
<demo>
  <p>The first paragraph.</p>
  <p>The second paragraph.</p>
</demo>
```

**Stylesheet** (demo.dsl):
```scheme
(root (make simple-page-sequence
        (make scroll)))

(element p (make paragraph))
```

**Flow Objects Demonstrated**:
- `simple-page-sequence` - Page sequence container
- `scroll` - Continuous layout (no page breaks)
- `paragraph` - Paragraph flow object
- `text` - Implicit text content
- `a` (anchors) - Automatic anchor generation

**FOT Output** (16 lines, 262 bytes):
```xml
<?xml version="1.0"?>
<fot>
<simple-page-sequence>
<scroll>
<paragraph>
<a name="0"/>
<a name="1"/>
<text>The first paragraph.</text>
</paragraph>
<paragraph>
<a name="2"/>
<text>The second paragraph.</text>
</paragraph>
</scroll>
</simple-page-sequence>
</fot>
```

**Complexity**: ⭐ Minimal
- 2 flow object types: `paragraph`, `scroll`
- No properties
- Pure text content

---

## Test Case 2: sps_css

**Files**: `sps_css.sgml`, `sps_css.dsl`, `test-sps-css.sh`

**Input** (sps_css.sgml):
```xml
<prova>
  <p>Hola aixo es una prova.</p>
</prova>
```

**Stylesheet** (sps_css.dsl):
```scheme
(element prova
  (make simple-page-sequence
    left-margin: 10cm
    top-margin: 10cm
  )
)
```

**Flow Objects Demonstrated**:
- `simple-page-sequence` with margin properties
- `left-margin` property (10cm → 283.464pt)
- `top-margin` property (10cm → 283.464pt)
- Implicit text processing (no explicit paragraph)

**FOT Output** (8 lines, 194 bytes):
```xml
<?xml version="1.0"?>
<fot>
<simple-page-sequence left-margin="283.464pt" top-margin="283.464pt">
<a name="0"/>
<a name="1"/>
<text>Hola aixo es una prova.</text>
</simple-page-sequence>
</fot>
```

**Complexity**: ⭐ Minimal
- 1 flow object type: `simple-page-sequence`
- 2 properties: margins
- Unit conversion: cm → pt (1cm = 28.3464pt)

---

## Running Tests

```bash
cd tests/print-simple

# Generate FOT baselines
./test-demo.sh > demo.fot
./test-sps-css.sh > sps_css.fot

# Compare with dazzle (future)
# node ../../packages/dazzle-cli/dist/cli.js -t fot -d demo.dsl demo.sgm > demo-dazzle.fot
# diff demo.fot demo-dazzle.fot
```

---

## Next Steps

1. Implement FOT backend in dazzle
2. Support basic flow objects:
   - `simple-page-sequence`
   - `scroll`
   - `paragraph`
   - `text`
3. Support basic properties:
   - `left-margin`, `right-margin`, `top-margin`, `bottom-margin`
   - Unit conversions (cm, pt, etc.)
4. Match OpenJade FOT output byte-for-byte
5. Expand to more complex examples

---

## Flow Object Reference

### simple-page-sequence
- Container for page-oriented content
- Properties: margins, page dimensions, headers/footers
- Children: paragraphs, display groups, sequences

### scroll
- Continuous layout without page breaks
- Used for screen display or continuous output
- Children: any flow objects

### paragraph
- Block-level paragraph
- Properties: alignment, spacing, indentation, font
- Children: text, inline sequences

### text
- Leaf node containing actual text content
- Generated implicitly from PCDATA in most cases
