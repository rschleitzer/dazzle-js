# CLAUDE.md - Dazzle Project Context

> **Context document for Claude Code and future development sessions**
>
> This document provides complete background on the Dazzle project, including motivation, design decisions, technical specifications, and implementation guidance.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Motivation & History](#motivation--history)
3. [DSSSL Lineage](#dsssl-lineage)
4. [OpenJade Analysis](#openjade-analysis)
5. [Primitive Catalog](#primitive-catalog)
6. [Feature Matrix](#feature-matrix)
7. [Technical Stack](#technical-stack)
8. [CLI Design](#cli-design)
9. [Distribution Strategy](#distribution-strategy)
10. [Implementation Roadmap](#implementation-roadmap)

---

## Project Overview

**Dazzle** is a Rust-based code generation tool powered by Scheme templates. It reimplements the essential functionality of OpenJade's SGML backend for modern systems.

### Key Facts

- **Name**: Dazzle (evocative of brilliance and transformation)
- **Purpose**: Template-driven code generation from XML/SGML input
- **Language**: Rust (host) + Scheme (templates)
- **Primary Use Case**: fire code generation

### Why "Dazzle"?

- Evokes transformation and brilliance - what code generation does
- Memorable and distinctive
- Works as imperative verb - "dazzle this template"
- Available everywhere (crates.io, all distros, Homebrew, MacPorts)
- No trademark conflicts (only libdazzle exists, a GNOME library - different domain)

---

## Motivation & History

### The Problem

OpenJade (and its predecessor Jade) are disappearing from package managers:

- **Homebrew**: Already dropped OpenJade
- **MacPorts**: Still available, but aging
- **Linux distros**: Maintenance declining
- **Codebase**: Unmaintained C++ from 1990s

### User's Context

- Using OpenJade since 1997 (28 years)
- Initially: SGML documentation generation
- Since 2003: **Code generation via SGML backend**
- Current project: **fire** (FHIR 5 server in Rust)
- Large existing codebase depending on OpenJade workflow

### The Solution: Dazzle

Preserve the workflow in a maintainable form:

- ✅ Pure Rust implementation (maintainable, cross-platform)
- ✅ Scheme-powered (keep template language)
- ✅ XML + DTD validation (libxml2)
- ✅ OpenJade CLI-compatible (zero retraining)
- ✅ Focus on code generation (not document formatting)
- ✅ Drop SGML-as-spec overhead (use pure .scm files)

---

## DSSSL Lineage

Understanding what Dazzle inherits and what it doesn't:

### 1. DSSSL Standard (ISO/IEC 10179:1996)

**Download**: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf

**The "Big Bang" of our information:**

- Full specification of DSSSL (Document Style Semantics and Specification Language)
- Two languages:
  - **Transformation Language**: SGML → SGML transformations
  - **Style Language**: SGML → formatted output (print, screen)
- Based on **R4RS Scheme** (side-effect-free subset)
- Defines:
  - SDQL (Standard Document Query Language)
  - Grove model (document tree representation)
  - Flow objects (formatting primitives)
  - Style specifications

**Important**: DSSSL does NOT standardize:
- File I/O (`load`, `read`, `write`)
- The SGML backend (that's an OpenJade extension)

### 2. DSSSL-online (Jon Bosak Proposal)

Simplified subset of DSSSL proposed for web use:

- "DSSSL Lite" for WWW Consortium consideration (~1994)
- Goal: Make DSSSL practical for web browsers
- Simpler than full ISO standard
- Eventually led to CSS, not DSSSL adoption

### 3. Jade (James Clark, ~1996)

**First major DSSSL implementation:**

- Implemented DSSSL-online with restrictions and extensions
- Written in C++
- Backends: RTF, TeX, MIF, HTML, SGML
- **SGML backend**: Key extension - SGML-to-SGML transformations
- Introduced "external procedures" concept
- Added `load` procedure (not in DSSSL standard!)

### 4. OpenJade (Community fork, 1999-2010)

**Evolution of Jade:**

- Fork maintained by DSSSL community
- More backends, bug fixes, extensions
- **SGML backend remains the killer feature**
- Last significant activity: ~2010
- Current status: Unmaintained, C++, hard to build

**OpenJade Extensions not in DSSSL:**
- `load` procedure (file loading)
- SGML/XML backend for transformations
- Various external procedures
- Platform-specific features

---

## OpenJade Analysis

**Source**: Analyzed actual OpenJade 1.3.2 codebase (April 2003)

### Codebase Size

**Total: ~72,000 lines of C++** across 117 files

**Key components:**
- `style/` (39,135 lines) - DSSSL interpreter, Scheme evaluator, primitives
- `jade/` (20,641 lines) - FOT builders (HTML, RTF, TeX, MIF, **SGML backend**)
- `spgrove/` (7,006 lines) - OpenSP grove integration
- `grove/` (2,393 lines) - Grove model
- `include/` (48 lines) - Headers (depends on external OpenSP library)

**Critical files for Dazzle:**
- `style/primitive.h` - **224 Scheme primitives** defined
- `style/primitive.cxx` - 5,704 lines - Primitive implementations
- `style/Interpreter.cxx` - 2,390 lines - Scheme interpreter core
- `style/SchemeParser.cxx` - ~2,300 lines - S-expression parser
- `jade/SgmlFOTBuilder.cxx` - 2,824 lines - **SGML backend implementation**
- `grove/Node.{h,cxx}` - ~2,400 lines - Grove node interface

### OpenSP Dependency

**Important discovery**: OpenJade does NOT include OpenSP sources!

- OpenSP was split from OpenJade in 2002 (version 1.3.2)
- OpenJade's `configure` looks for external OpenSP installation
- OpenSP is ~100-150K lines of C++ (SGML/XML parser)
- Dazzle replaces OpenSP with **libxml2** (XML only, DTD validation included)

### What Dazzle Must Implement

From OpenJade analysis, Dazzle needs:

1. **224 Scheme primitives** (detailed below)
   - ~90 from R5RS (Steel provides)
   - ~104 DSSSL-specific (implement in Rust)
   - ~30 DSSSL types (stubs only - not needed for code generation)
2. **Grove query engine** (~50 core primitives)
3. **Processing & output** (~20 primitives for text generation)
4. **SGML backend concept** (simplified: only `entity` + `formatting-instruction` flow objects)
5. **R5RS Scheme interpreter** (use Steel, not port)
6. **Template file parser** (pure .scm files, no SGML wrapper)

**Key simplification**: User only generates plain text code files (`.java`, `.rs`, etc.), not styled documents. This eliminates ~30 primitives (quantities, colors, spacing) - implement as stubs.

### What Dazzle Does NOT Need

- ❌ OpenSP parser (use libxml2 instead)
- ❌ Other FOT builders (HTML, RTF, TeX, MIF)
- ❌ DSSSL style language (flow objects, characteristics for document formatting)
- ❌ SGML-wrapped template parsing (`<style-specification>`)
- ❌ Document rendering features

---

## Primitive Catalog

**Total primitives in OpenJade**: 224 (from `style/primitive.h`)

### Steel Provides (R5RS Standard): ~90 primitives ✓

**Already implemented** in Steel Scheme:
- **Lists** (15): `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`, `list-tail`, `list-ref`, `member`, `memv`, `assoc`, `null?`, `pair?`, `list?`
- **Strings** (14): `string`, `string-length`, `string=?`, `string<?`, `string<=?`, `string-append`, `string-ref`, `substring`, `symbol->string`, `string->symbol`, `string->list`, `list->string`
- **Numbers** (42): Arithmetic, comparison, transcendental functions, conversions
- **Predicates** (14): Type checks for symbol, boolean, procedure, string, char, number, etc.
- **Logic** (3): `not`, `equal?`, `eqv?`
- **Characters** (5): Comparison and case conversion
- **Vectors** (8): `vector?`, `vector`, `vector-ref`, `vector-set!`, `make-vector`, conversions

### Dazzle Must Implement: ~134 primitives

Organized by priority and function:

#### **CRITICAL: Grove Query Functions** (~50 primitives)

XML tree navigation and querying - see DSSSL spec Section 9 (SDQL) for details:

- **Context**: `current-node`
- **Node lists**: `node-list?`, `node-list-empty?`, `node-list-first`, `node-list-rest`, `node-list`, `node-list-length`, `node-list-ref`, `node-list-reverse`, `node-list-map`, `node-list=?`, `empty-node-list`
- **Properties**: `gi` (element name), `id`, `data` (text), `node-property`
- **Navigation**: `parent`, `ancestor`, `children`, `descendants`, `follow`, `preced`, `attributes`
- **Selection**: `select-elements`, `select-by-class`, `element-with-id`, `match-element?`
- **Attributes**: `attribute-string`, `inherited-attribute-string`, `inherited-element-attribute-string`
- **Position**: `first-sibling?`, `last-sibling?`, `absolute-first-sibling?`, `absolute-last-sibling?`, `have-ancestor?`
- **Numbering**: `child-number`, `ancestor-child-number`, `element-number`, `element-number-list`, `hierarchical-number`, `hierarchical-number-recursive`, `first-child-gi`
- **Entities**: `entity-system-id`, `entity-public-id`, `entity-generated-system-id`, `entity-text`, `entity-notation`, `entity-type`, `entity-attribute-string`
- **Notations**: `notation-system-id`, `notation-public-id`, `notation-generated-system-id`
- **Normalization**: `general-name-normalize`, `entity-name-normalize`

#### **HIGH: Processing & Sosofo** (~20 primitives)

- **Processing**: `process-children`, `process-children-trim`, `process-node-list`, `process-element-with-id`, `process-matching-children`, `process-first-descendant`, `next-match`
- **Sosofo**: `sosofo?`, `empty-sosofo`, `sosofo-append`, `literal`, `sosofo-label`, `sosofo-discard-labeled`
- **Formatting**: `format-number` (I/II/III, 1/2/3, a/b/c), `format-number-list` (1.2.3)
- **Page numbers**: `current-node-page-number-sosofo`, `page-number-sosofo` (may not need)

#### **MEDIUM: DSSSL Types** (~30 primitives) - **MOSTLY STUBS**

Only `entity` and `formatting-instruction` flow objects needed for code generation. Most types implemented as stubs:

- **Quantities** (4): `quantity?`, `table-unit`, `quantity->number`, `quantity->string` - Return dummy values
- **Spacing** (5): `display-space?`, `display-space`, `inline-space?`, `inline-space`, `display-size` - Return dummy values
- **Colors** (4): `color?`, `color`, `color-space?`, `color-space` - Return dummy values
- **Addresses** (8): `address?`, `address-local?`, `address-visited?`, `current-node-address`, `idref-address`, `entity-address`, `sgml-document-address`, `node-list-address` - Return dummy values
- **Glyphs** (5): `glyph-id?`, `glyph-id`, `glyph-subst-table?`, `glyph-subst-table`, `glyph-subst` - Return dummy values
- **Character properties** (2): `char-property`, `char-script-case` - Implement if needed for case conversion

**Implementation**: All stubs initially (1-2 days), implement properly only if templates use them.

#### **LOW: Extensions & Utilities** (~20 primitives)

- **Keywords** (3): `keyword?`, `keyword->string`, `string->keyword`
- **Time** (6): `time`, `time->string`, `time<?`, `time>?`, `time<=?`, `time>=?`
- **Language** (4): `language?`, `current-language`, `with-language`, `language`
- **Style** (3): `style?`, `merge-style`, `map-constructor`
- **Parsing**: `sgml-parse`
- **Debug/error** (4): `error`, `external-procedure`, `read-entity`, `debug`
- **Named node lists** (6): `named-node-list?`, `named-node`, `named-node-list-names`, `named-node-list-normalize`, `node-list-no-order`, `node-list-error`
- **Page conditionals** (3): `if-first-page`, `if-front-page`, `all-element-number`
- **HyTime**: `hytime-linkend`
- **String utilities**: `string-equiv?`

---

## Feature Matrix

What Dazzle implements from each ancestor:

### From DSSSL Standard (ISO 10179)

**Core Expression Language** - ✅ Via Steel Scheme:
- ✅ R5RS Scheme (superset of R4RS/DSSSL)
- ✅ ~90 standard procedures (lists, strings, math, predicates)
- ✅ Basic data types (lists, strings, numbers, booleans, vectors)
- ✅ Procedures (`define`, `lambda`, `let`, `letrec`, `let*`)
- ✅ Conditionals (`if`, `cond`, `case`)
- ✅ Comments (line `;` and block `#| ... |#`)

**NOT implementing (document formatting):**
- ❌ Flow objects for document rendering
- ❌ Style specifications for pagination
- ❌ Formatting characteristics (fonts, spacing for print)
- ❌ SPDL output

**Grove Model** - ✅ Complete implementation:
- ✅ ~50 grove query primitives
- ✅ XML tree navigation (parent, children, ancestors, descendants)
- ✅ Node properties (GI, ID, attributes, data)
- ✅ Pattern matching and selection
- ✅ Entity and notation access

### From Jade/OpenJade

**Scheme Primitives** - ✅ Complete compatibility:
- ✅ All 224 OpenJade primitives
- ✅ ~90 from R5RS (Steel provides)
- ✅ ~134 DSSSL-specific (Dazzle implements)

**Processing & Code Generation:**
- ✅ `load` procedure (OpenJade extension, not DSSSL standard)
- ✅ SGML backend concept (text output generation)
- ✅ Template-based code generation
- ✅ Processing control (`process-children`, `process-node-list`, etc.)
- ✅ Sosofo operations (`literal`, `sosofo-append`, etc.)
- ✅ External procedures (Rust functions callable from Scheme)

**CLI Compatibility:**
- ✅ `-d` template file option
- ✅ `-V` variable definitions
- ✅ `-D` search directories
- ✅ `-t` backend selection: text, xml
- ❌ `-o` output file (template controls output via `write-file`)

**NOT implementing:**
- ❌ Document formatting backends (RTF, TeX, MIF, HTML)
- ❌ SPDL generation
- ❌ Full SGML parsing (XML only, via libxml2)
- ❌ SGML-wrapped templates (`<style-specification>` format)

### Dazzle-Specific Features

**New/Enhanced:**
- ✅ Pure Rust implementation
- ✅ libxml2 for XML + DTD validation
- ✅ Steel Scheme interpreter (R5RS)
- ✅ Pure .scm template files (no SGML overhead)
- ✅ Modern error messages
- ✅ File writing from templates (`write-file`)
- ✅ Multiple output file generation

**Simplified:**
- Stdout output removed (templates write files directly)
- Single backend (code generation only)
- No SGML entity loading (use `load` instead)

---

## Technical Stack

### Language Choices

**Host Language: Rust**
- Modern, maintainable, safe
- Excellent cross-platform support
- Good FFI for libxml2
- Active ecosystem

**Template Language: Scheme (via Steel)**
- **Steel**: R5RS Scheme implementation in Rust
- Chosen because:
  - ✅ R5RS compatible (superset of R4RS/DSSSL)
  - ✅ Actively maintained
  - ✅ Designed for Rust embedding
  - ✅ Production-ready
  - ✅ Good documentation
- Why not scheme-rs: Too new (2025), async-focused, less mature

**XML Parser: libxml2**
- Industry standard (GNOME project)
- Full DTD validation support
- Pure C (clean FFI, not C++)
- Used by xmllint, browsers, editors
- Much safer dependency than OpenSP

### Dependencies

```toml
[dependencies]
libxml = "0.3"              # libxml2 Rust bindings
steel-interpreter = "..."   # Steel Scheme runtime
clap = "4"                  # CLI argument parsing
```

**Philosophy**: Minimal dependencies for easier packaging.

### Project Structure

```
dazzle/
├── crates/
│   ├── dazzle-core/      # libxml2 + Steel integration
│   ├── dazzle-template/  # Template engine, file writing
│   └── dazzle-cli/       # Command-line interface
├── examples/        # Example templates
├── tests/           # Integration tests
└── docs/            # Documentation
```

---

## CLI Design

### Final Interface

```bash
dazzle -d template.scm [-t xml] [-V key=value]... [-D dir]... input.xml
```

**Flags:**
- `-t xml` - Optional XML backend
- `-d template.scm` - **Required**. Scheme template file
- `-V key=value` - Template variables (repeatable)
- `-D directory` - Template search paths (repeatable)
- `input.xml` - Input XML file(s)

**Automatic Features:**
- DTD validation if `<!DOCTYPE>` present in XML
- Output controlled by template (no `-o` flag)
- Search paths: current dir, `-D` dirs, system dirs

### Examples

```bash
# Basic usage
dazzle -d codegen.scm grammar.xml

# With variables
dazzle -d gen.scm -V package=com.example -V version=1.0 model.xml

# With search paths
dazzle -d template.scm -D /usr/share/dazzle/templates input.xml

# Multiple variables
dazzle -d gen.scm \
  -V package=smartonfhir \
  -V outdir=src/generated \
  -V debug=true \
  grammar.xml
```

### Template File Writing

Templates write files directly:

```scheme
;; In template.scm
(define outdir (get-variable "outdir" "generated"))

(define (generate-class node)
  (let ((name (xml-get-attribute node "name")))
    (write-file 
      (string-append outdir "/" name ".java")
      (generate-java-code name))))
```

---

## Distribution Strategy

### Package Availability: "dazzle"

**Verified Available:**
- ✅ crates.io
- ✅ Debian/Ubuntu repositories
- ✅ Fedora repositories
- ✅ Arch/AUR
- ✅ Homebrew
- ✅ MacPorts
- ✅ openSUSE

**No conflicts found.**

### Release Roadmap

**Phase 1: Foundation (Week 1)**
- Publish to crates.io
- GitHub releases with binaries
- Static musl builds

**Phase 2: Package Managers (Month 1-2)**
- Arch AUR (easiest, fastest)
- Homebrew tap (your own)
- MacPorts submission

**Phase 3: Official Repos (Month 3-6)**
- Homebrew core
- MacPorts official
- Fedora
- Debian (1+ year timeline)

### macOS Priority

**Critical for user:**
- Homebrew dropped OpenJade
- MacPorts still has it, but aging
- Need both for redundancy

**Focus:**
1. **MacPorts** - More conservative, won't drop packages easily
2. **Homebrew** - Larger user base

### Linux Priority

1. **Arch AUR** - Fastest acceptance, Rust-friendly
2. **Fedora** - Active Rust community
3. **openSUSE** - German connection, good for user
4. **Debian** - Long-term stability (slow process)

---

## Implementation Notes

### Scheme Features

**From R4RS (DSSSL base):**
```scheme
; Core language
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Lists
(map (lambda (x) (* x 2)) '(1 2 3))  ; => (2 4 6)

; Strings
(string-append "Hello" " " "World")  ; => "Hello World"
```

**From R5RS (Steel provides):**
```scheme
; Line comments (R4RS)
; This is a comment

; Block comments (R5RS)
#|
Multi-line block comment
Can nest: #| inner |#
|#

; S-expression comments (R5RS)
#; (this whole expression is ignored
     (even if multi-line))
```

**Extensions needed for Dazzle:**
```scheme
; load - OpenJade extension (not DSSSL standard)
(load "helpers.scm")

; File I/O - Dazzle additions
(write-file "Output.java" content)
(ensure-dir "src/generated")

; Variables from CLI
(get-variable "package" "com.default")

; XML navigation - Dazzle API
(xml-get-attribute node "name")
(xml-children node)
(xml-select node "//rule")
```

### Template Pattern

**Typical Dazzle template:**

```scheme
;; codegen.scm - Main template

; Load utilities
(load "lib/xml-helpers.scm")
(load "lib/java-naming.scm")

; Main generation function
(define (generate-class node)
  (let* ((name (xml-get-attribute node "name"))
         (filename (string-append outdir "/" name ".java")))
    (make entity system-id: filename
      (make formatting-instruction (string-append
        "package " package ";\n\n"
        "public class " name " {\n"
        "  // Generated code\n"
        "}\n")))))

### XML + DTD Handling

**Automatic DTD validation:**

```xml
<?xml version="1.0"?>
<!DOCTYPE grammar SYSTEM "grammar.dtd">
<grammar>
  <rule name="expression">
    <!-- ... -->
  </rule>
</grammar>
```

When Dazzle parses this:
1. libxml2 sees `<!DOCTYPE>`
2. Loads `grammar.dtd` automatically
3. Validates XML against DTD
4. Reports errors if validation fails
5. Only passes validated document to template

**No flag needed - it just works!**

### Bridge Functions (Rust ↔ Scheme)

**What Rust must expose to Steel:**

```rust
// File I/O
vm.register_fn("write-file", |path: String, content: String| {
    std::fs::write(path, content)
});

vm.register_fn("ensure-dir", |path: String| {
    std::fs::create_dir_all(path)
});

// CLI variables
vm.register_fn("get-variable", |key: String, default: String| {
    variables.get(&key).cloned().unwrap_or(default)
});

// XML navigation (simplified)
vm.register_fn("xml-get-attribute", |node: Node, name: String| {
    node.get_attribute(&name)
});

vm.register_fn("xml-children", |node: Node| {
    node.children()
});

vm.register_fn("xml-select", |node: Node, xpath: String| {
    node.select(&xpath)  // XPath queries
});
```

---

## Migration from OpenJade

### File Organization

**Before (OpenJade):**
```
project/
├── grammar.sgml        # SGML source
├── grammar.dtd         # DTD
└── codegen.dsl         # DSSSL spec (SGML format!)
    <!DOCTYPE style-sheet ...>
    <style-sheet>
      <style-specification>
        <![CDATA[
        (define (foo) ...)  ; Scheme in CDATA
        ]]>
      </style-specification>
    </style-sheet>
```

**After (Dazzle):**
```
project/
├── grammar.xml         # XML source (converted from SGML)
├── grammar.dtd         # DTD (unchanged!)
└── codegen.scm         # Pure Scheme template
    ;; No SGML overhead!
    (define (foo) ...)
```

### Command Migration

**Before:**
```bash
openjade -t sgml -d codegen.dsl grammar.sgml > Output.java
```

**After:**
```bash
dazzle -d codegen.scm grammar.xml
# Output.java written by template
```

### Benefits of Migration

**Technical:**
- ✅ No SGML escaping (`<` is just `<`, not `&lt;`)
- ✅ No CDATA sections
- ✅ Pure Scheme files (better editor support)
- ✅ Block comments available (`#| ... |#`)
- ✅ Cleaner, more maintainable code

**Operational:**
- ✅ Won't disappear from package managers
- ✅ Modern tooling (Rust ecosystem)
- ✅ Active maintenance
- ✅ Cross-platform binaries
- ✅ Better error messages

---

## Implementation Roadmap

### Overview

**Total scope**: ~134 Rust functions to implement (~104 real + ~30 stubs) + integration code

**Estimated timeline**: 3-6 months (solo, experienced Rust developer)

**Key simplification**: User only uses `entity` + `formatting-instruction` flow objects for plain text code generation. No document formatting, colors, spacing, or page layout needed.

### Phase 1: Foundation (2-3 weeks)

**Goal**: Basic project structure + minimal viable product

**Tasks:**
1. Set up Cargo workspace with three crates:
   - `dazzle-core`: libxml2 + Steel integration
   - `dazzle-template`: Template engine, file I/O
   - `dazzle-cli`: Command-line interface

2. Dependencies:
   ```toml
   libxml = "0.3"                 # libxml2 bindings
   steel-core = "0.5"             # Steel Scheme interpreter
   clap = { version = "4", features = ["derive"] }
   ```

3. Implement **~30 critical primitives**:
   - Grove basics: `current-node`, `gi`, `id`, `children`, `parent`, `attribute-string`, `data`
   - Node lists: `node-list`, `node-list-empty?`, `node-list-first`, `node-list-rest`
   - Processing: `process-children`, `literal`, `sosofo-append`, `empty-sosofo`
   - Selection: `select-elements`, `element-with-id`

4. Basic file I/O:
   - `write-file`, `ensure-dir`
   - `get-variable` (CLI variables)
   - `load` (load .scm files)

**Deliverable**: Can run simple templates that traverse XML and output text.

### Phase 2: Complete Grove Support (3-4 weeks)

**Goal**: Full XML navigation and querying

**Tasks:**
1. Implement **~50 grove primitives**:
   - Tree navigation: `ancestor`, `descendants`, `follow`, `preced`, `attributes`
   - Position predicates: `first-sibling?`, `last-sibling?`, `have-ancestor?`
   - Numbering: `child-number`, `element-number`, `hierarchical-number`
   - Entities: `entity-system-id`, `entity-public-id`, etc.
   - Notations: `notation-system-id`, `notation-public-id`, etc.

2. Pattern matching:
   - `select-elements` with pattern support
   - `match-element?` predicate
   - Pattern parser

3. Node list operations:
   - `node-list-map`, `node-list-ref`, `node-list-reverse`, `node-list-length`
   - Lazy evaluation where possible

**Deliverable**: Full OpenJade grove query compatibility.

### Phase 3: Processing & Sosofo (2-3 weeks)

**Goal**: Template processing control and output generation

**Tasks:**
1. Implement **~20 processing primitives**:
   - `process-node-list`, `process-element-with-id`
   - `process-matching-children`, `process-first-descendant`
   - `next-match` (template rule chaining)
   - `process-children-trim` (whitespace handling)

2. Sosofo system:
   - Sosofo data structure (concatenation of output fragments)
   - `sosofo-append`, `sosofo-label`, `sosofo-discard-labeled`
   - `literal` (text output)

3. Formatting helpers:
   - `format-number` (I, II, III, 1, 2, 3, a, b, c, etc.)
   - `format-number-list` (1.2.3 hierarchical numbering)

**Deliverable**: Complete code generation workflow working.

### Phase 4: DSSSL Types (1-2 days) - **SIMPLIFIED**

**Goal**: Stub implementations for DSSSL types (not needed for code generation)

**User's use case**: Only `entity` + `formatting-instruction` flow objects → plain text output

**Tasks:**
1. Implement **~30 type primitives as STUBS**:
   - Quantities: Return dummy values (not used for code generation)
   - Colors: Return dummy values (not used for code generation)
   - Addresses: Return dummy values (rarely used)
   - Glyphs: Return dummy values (rarely used)
   - Spacing: Return dummy values (not used for code generation)
   - Character properties: Stub `char-script-case`, maybe implement `char-property`

2. Stub implementation pattern:
   ```rust
   vm.register_fn("quantity?", |_obj| false);
   vm.register_fn("color?", |_obj| false);
   vm.register_fn("display-size", || vec![0, 0]);
   // etc.
   ```

3. **Only implement properly if templates actually use them** (unlikely)

**Deliverable**: All type primitives callable (return sensible defaults), templates don't error.

### Phase 5: Extensions & Utilities (1-2 weeks)

**Goal**: Remaining OpenJade primitives

**Tasks:**
1. Implement **~20 utility primitives**:
   - Keywords: `keyword?`, `keyword->string`, `string->keyword`
   - Time: `time`, `time->string`, `time<?`, etc.
   - Language: `language?`, `current-language`, `with-language`
   - Style: `style?`, `merge-style`
   - Debug: `error`, `debug`
   - Named node lists (if needed)

2. Optional features:
   - `sgml-parse` (parse nested documents)
   - `read-entity` (read entity content)
   - Page conditionals (if needed: `if-first-page`, `if-front-page`)

**Deliverable**: 100% OpenJade primitive compatibility.

### Phase 6: CLI & Integration (1-2 weeks)

**Goal**: Complete CLI tool

**Tasks:**
1. Command-line interface:
   - `-d template.scm` - Template file
   - `-V key=value` - Variables (repeatable)
   - `-D directory` - Search paths (repeatable)
   - `-t xml` - Backend selection
   - Input XML file(s)

2. Template loading:
   - Search path resolution
   - `load` procedure integration
   - Error reporting

3. XML parsing:
   - libxml2 integration
   - DTD validation (automatic if `<!DOCTYPE>` present)
   - Error reporting

**Deliverable**: Complete CLI tool, OpenJade-compatible interface.

### Phase 7: Testing & Documentation (2-4 weeks)

**Goal**: Production-ready release

**Tasks:**
1. Testing:
   - Unit tests for each primitive
   - Integration tests with real templates
   - Compare output with OpenJade
   - Test with user's actual templates

2. Documentation:
   - README with quick start
   - Primitive reference
   - Migration guide (OpenJade → Dazzle)
   - Template examples

3. Packaging:
   - Publish to crates.io
   - GitHub releases with binaries
   - Static musl builds for Linux
   - macOS universal binaries

**Deliverable**: v1.0 release.

### Phase 8: Distribution (Ongoing, 1-6 months)

**Goal**: Available in package managers

**Tasks:**
1. **Immediate** (Week 1-2):
   - Publish to crates.io
   - GitHub releases
   - Documentation site

2. **Short-term** (Month 1-2):
   - Arch AUR package
   - Homebrew tap (personal)
   - MacPorts submission

3. **Medium-term** (Month 3-6):
   - Homebrew core
   - MacPorts official
   - Fedora package
   - openSUSE package

4. **Long-term** (6+ months):
   - Debian package (slow process)
   - Ubuntu PPA

### Testing Strategy

**Test pyramid:**
```
Production Tests (user's actual templates)
        ↑
Integration Tests (synthetic templates)
        ↑
    Primitive Tests (unit tests)
        ↑
    Type Tests (Rust ↔ Steel conversions)
```

**Compatibility testing:**
1. Convert user's SGML → XML (one-time: `opensp -x`)
2. Run both OpenJade and Dazzle on same XML
3. Compare outputs
4. Fix discrepancies
5. Add regression test

### Success Criteria

**Phase completion checklist:**

- [ ] Phase 1: Can generate simple code from XML
- [ ] Phase 2: Full XML navigation works
- [ ] Phase 3: Complex templates with processing control work
- [ ] Phase 4: DSSSL types all functional
- [ ] Phase 5: All 224 primitives implemented
- [ ] Phase 6: CLI matches OpenJade interface
- [ ] Phase 7: User's templates work identically to OpenJade
- [ ] Phase 8: Available in at least 2 package managers

**v1.0 Release criteria:**
1. ✅ All 224 primitives working
2. ✅ User's actual templates generate identical output to OpenJade
3. ✅ DTD validation working
4. ✅ CLI compatible with OpenJade
5. ✅ Documentation complete
6. ✅ Published to crates.io
7. ✅ Binaries for macOS, Linux, Windows

---

## References

### Standards & Specs

- **DSSSL**: ISO/IEC 10179:1996
  - Online: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf
  - Local copy: `/Users/r.schleitzer/Documents/dsssl96b.pdf`
  - **Key sections for Dazzle implementation**:
    - Section 8: Grove architecture and node properties
    - Section 9: SDQL (Standard Document Query Language) - grove queries
    - Section 10: Processing model - rules, modes, `next-match`
    - Section 6: Data types - quantities, colors, addresses
- **R4RS Scheme**: IEEE Std 1178-1990 (DSSSL base)
- **R5RS Scheme**: http://www.schemers.org/Documents/Standards/R5RS/
- **XML**: W3C XML 1.0 Specification
- **DTD**: Part of XML spec

### Implementations

- **Jade**: https://www.jclark.com/jade/
- **OpenJade**: https://openjade.sourceforge.net/
- **Steel**: https://github.com/mattwparas/steel
- **libxml2**: https://gitlab.gnome.org/GNOME/libxml2

### User's Projects

- **Scaly.io**: Parser generator project
- Custom grammar DTD for parser specifications
- Large existing codebase using OpenJade

---

## Design Decisions Summary

**Language Decisions:**
- **Rust** (not C++) - Maintainability, safety, modern tooling, active ecosystem
- **Steel Scheme** (not scheme-rs, not port OpenJade interpreter) - Proven, R5RS-compliant, designed for embedding, actively maintained
- **libxml2** (not pure Rust, not OpenSP) - Only mature option for DTD validation, industry standard

**Scope Decisions:**
- **Code generation only** (not document formatting)
  - Implement SGML backend concept (text output)
  - No RTF, TeX, MIF, HTML backends
  - No flow objects for pagination
- **XML only** (not SGML input)
  - Use libxml2 for parsing
  - Keep DTD validation
  - One-time SGML → XML conversion acceptable
- **Pure .scm templates** (not SGML-wrapped `<style-specification>`)
  - R5RS Scheme files with block comments
  - No CDATA sections needed
  - Better editor support

**Primitive Decisions:**
- **All 224 OpenJade primitives** for full compatibility
- **~90 from R5RS** via Steel (lists, strings, math, etc.)
- **~134 custom implementations** (grove queries, processing, DSSSL types)
- **Priority**: Grove navigation > Processing > Types > Utilities

**CLI Decisions:**
- **Drop `-o` flag** - Template controls output via `write-file`
- **Keep `-d`, `-V`, `-D`** - OpenJade compatible interface
- **Automatic DTD validation** - No flag needed, triggered by `<!DOCTYPE>`
- **Search paths** - Current dir, `-D` dirs, system dirs

**Distribution Decisions:**
- **crates.io first** - Fast, foundational, developer audience
- **AUR + Homebrew tap** - Quick adoption, early feedback
- **MacPorts priority** - Stability, won't drop packages easily
- **Official repos later** - Once proven (Homebrew core, Fedora, Debian)

**Implementation Decisions:**
- **Use Steel, not port interpreter** - 3-4 months saved, R5RS compliance
- **Implement primitives as Rust functions** - Type safety, performance
- **libxml2 FFI** - Battle-tested, not reinvent parser
- **Phased development** - MVP first, then expand
- **Test against user's templates** - Real-world validation

---

## Success Metrics

**Project succeeds if:**

1. ✅ **Drop-in replacement** - User can replace OpenJade with Dazzle for their existing templates
2. ✅ **Full compatibility** - All 224 OpenJade primitives work identically
3. ✅ **Output identical** - Templates generate same code as OpenJade
4. ✅ **Available in package managers** - MacPorts + Homebrew minimum (won't disappear)
5. ✅ **Build from source anywhere** - Pure Rust + libxml2 (standard dependency)
6. ✅ **Easier to maintain** - Modern codebase, good documentation
7. ✅ **Community can contribute** - Rust vs C++, clear architecture

**Bonus success:**
- **Others adopt Dazzle** - Beyond original use case
- **Template libraries** - Shared, reusable code generators
- **Extended features** - REPL mode, watch mode, template debugger
- **Multiple backends** - S-expression input, JSON output, etc.

---

## Future Possibilities

**Not in scope for v1.0, but possible later:**

- **S-expression input**: Alternative to XML
- **More Scheme implementations**: Make backend pluggable
- **Pure Rust DTD**: If libxml2 becomes problematic
- **Template library**: Reusable code generators
- **REPL mode**: Interactive template development
- **Watch mode**: Auto-regenerate on file changes
- **Language bindings**: Call Dazzle from other languages

---

## Contact & Contributing

**Project Status**: In development
**Target**: v1.0 release when feature-complete
**License**: MIT

---

## Analysis Summary (Updated 2025-01-16)

### Key Findings from OpenJade Analysis

1. **OpenJade codebase**: ~72,000 lines C++ (not including OpenSP)
2. **OpenSP dependency**: Separate project, ~100-150K lines (not needed - use libxml2)
3. **Total primitives**: 224 Scheme functions
   - 90 from R5RS (Steel provides)
   - 134 DSSSL-specific (Dazzle must implement)
4. **SGML backend**: 2,824 lines (core feature to preserve)
5. **Critical files**:
   - `style/primitive.h` - All 224 primitives defined
   - `style/primitive.cxx` - 5,704 lines of implementations
   - `jade/SgmlFOTBuilder.cxx` - 2,824 lines backend logic

### Implementation Strategy

**Chosen approach**: Hybrid (reimplementation with Steel)

**What we use:**
- ✅ Steel Scheme (R5RS interpreter) - ~90 primitives free
- ✅ libxml2 (XML + DTD validation) - battle-tested
- ✅ Pure Rust (modern, safe, maintainable)

**What we implement:**
- ✅ ~134 DSSSL primitives as Rust functions
- ✅ Grove query engine (XML navigation)
- ✅ Processing control (sosofo, template rules)
- ✅ DSSSL types (quantities, colors, addresses, glyphs)
- ✅ File I/O and CLI integration

**What we drop:**
- ❌ OpenSP parser (use libxml2 instead)
- ❌ OpenJade interpreter (use Steel instead)
- ❌ Other backends (RTF, TeX, MIF, HTML)
- ❌ SGML-wrapped templates (pure .scm files)
- ❌ Document formatting features

### Effort Estimate

**Timeline**: 3-6 months (solo, experienced Rust developer)

**Breakdown**:
- Foundation: 2-3 weeks
- Grove support: 3-4 weeks
- Processing & Sosofo: 2-3 weeks
- DSSSL types: **1-2 days** (stubs only - major time saver!)
- Extensions: 1-2 weeks
- CLI integration: 1-2 weeks
- Testing & docs: 2-4 weeks
- Distribution: Ongoing (1-6 months)

**Key milestone**: MVP in ~6-8 weeks (Phases 1-3)
**Near-complete**: ~10-12 weeks (Phases 1-6)

### Risk Mitigation

**Low risks**:
- Steel Scheme integration - well-documented, designed for embedding
- libxml2 FFI - stable, widely used
- Primitive implementation - straightforward Rust functions

**Medium risks**:
- Pattern matching complexity - requires parser
- Node list lazy evaluation - performance critical
- Template rule chaining (`next-match`) - requires careful design

**Mitigation**:
- Start with user's actual templates (real-world validation)
- Compare output with OpenJade at each phase
- Build test suite from day one

### Next Steps

1. **Phase 1 start**: Set up Cargo workspace
2. **Implement MVP**: ~30 critical primitives
3. **Test with simple template**: Validate approach
4. **Iterate**: Add primitives based on user needs
5. **Full compatibility**: All 224 primitives working
