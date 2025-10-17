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
- ✅ Use DSSSL XML wrappers with entity references (OpenJade compatible)

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
6. **Template file parser** (XML wrapper with entity references to .scm modules)

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

---

## Template File Format: XML Wrappers with Entity References

**CRITICAL**: Templates must work with BOTH OpenJade AND Dazzle for migration compatibility.

### Why Entity References?

Steel Scheme has context isolation with `(load ...)` - each file creates a new context. Entity references let the parser concatenate `.scm` files before the interpreter sees them, following DSSSL tradition.

### Template Format

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY helpers SYSTEM "helpers.scm">
<!ENTITY rules   SYSTEM "rules.scm">
]>
<style-sheet>
<style-specification>
&helpers;
&rules;
</style-specification>
</style-sheet>
```

Both OpenJade/OpenSP and Dazzle/libxml2 parse this identically: resolve entities, extract content from `<style-specification>`, pass unified program to Scheme interpreter.

### CDATA Wrapping

Wrap `.scm` content in `<![CDATA[...]]>` if it contains `<` or `&` characters (Scheme predicates like `string<?`, generated code like `if (a < b)`, strings with `&`). Otherwise, no wrapping needed.

### Migration from OpenJade SGML

Add `<?xml version="1.0"?>` declaration, use lowercase element names (`<style-sheet>`, `<style-specification>`). The `.scm` files remain unchanged.

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

### Key Scheme Extensions for Dazzle

**Steel provides**: R5RS Scheme (core language, lists, strings, numbers, predicates, block comments `#| ... |#`)

**Dazzle must add**:
- `load` - Load .scm files (OpenJade extension)
- `write-file`, `ensure-dir` - File I/O
- `get-variable` - CLI variables
- Grove navigation - `current-node`, `gi`, `children`, `attribute-string`, etc.
- Processing - `process-children`, `literal`, `sosofo-append`, etc.

### DTD Validation

Automatic when `<!DOCTYPE>` present in XML. libxml2 loads and validates against DTD, reports errors before template runs.

### Rust ↔ Steel Bridge

Register Rust functions with Steel VM: file I/O (`write-file`, `ensure-dir`), CLI variables (`get-variable`), XML navigation (`xml-get-attribute`, `xml-children`, `xml-select` with XPath).

---

## Migration from OpenJade

**File changes**: Convert SGML → XML (one-time), pure `.scm` templates (no CDATA in wrapper), keep DTD unchanged.

**Command**: `openjade -t sgml -d codegen.dsl grammar.sgml > Output.java` → `dazzle -d codegen.scm grammar.xml` (output controlled by template)

**Benefits**: Modern tooling, won't disappear from repos, better editor support, cleaner code, cross-platform binaries.

---

## Implementation Roadmap

**Total**: ~134 Rust functions (~104 real + ~30 stubs). **Timeline**: 3-6 months.

### Phase 1: Foundation (2-3 weeks)
Cargo workspace (dazzle-core, dazzle-template, dazzle-cli). Deps: libxml, steel-core, clap. Implement ~30 critical primitives (grove basics, node lists, processing, file I/O). **MVP**: Simple templates work.

### Phase 2: Complete Grove (3-4 weeks)
~50 grove primitives (navigation, position, numbering, entities, notations). Pattern matching. Node list operations with lazy evaluation. **Complete OpenJade grove compatibility.**

### Phase 3: Processing & Sosofo (2-3 weeks)
~20 processing primitives (`process-node-list`, `next-match`, etc.). Sosofo data structure. Formatting helpers (`format-number`, `format-number-list`). **Full code generation workflow.**

### Phase 4: DSSSL Types (1-2 days)
~30 type primitives as **stubs** (quantities, colors, addresses, glyphs return dummy values). Implement properly only if templates use them.

### Phase 5: Extensions (1-2 weeks)
~20 utility primitives (keywords, time, language, style, debug). Optional: `sgml-parse`, `read-entity`, page conditionals. **100% OpenJade compatibility.**

### Phase 6: CLI (1-2 weeks)
Complete CLI (`-d`, `-V`, `-D`, `-t`). Template loading with search paths. libxml2 integration with automatic DTD validation. **Production CLI.**

### Phase 7: Testing & Docs (2-4 weeks)
Unit tests, integration tests, OpenJade comparison. Documentation (README, primitive reference, migration guide). Packaging (crates.io, binaries). **v1.0 release.**

### Phase 8: Distribution (Ongoing)
**Immediate**: crates.io, GitHub releases. **Short-term**: Arch AUR, Homebrew tap, MacPorts. **Medium-term**: Official repos (Homebrew core, Fedora, openSUSE). **Long-term**: Debian/Ubuntu.

### Testing Strategy
Test pyramid: Rust↔Steel types → Primitive units → Integration tests → Production (user templates). Compare Dazzle vs OpenJade output on identical XML.

### Success Criteria
**v1.0**: All 224 primitives, identical output to OpenJade, DTD validation, CLI compatible, complete docs, published to crates.io, cross-platform binaries.

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

**Stack**: Rust (host), Steel Scheme (templates), libxml2 (XML/DTD). **Scope**: Code generation only (no document formatting). **Primitives**: All 224 OpenJade (90 from Steel R5RS, 134 custom). **CLI**: OpenJade-compatible (`-d`, `-V`, `-D`), template controls output (no `-o`), auto DTD validation. **Distribution**: crates.io first, then AUR/Homebrew/MacPorts, official repos later. **Implementation**: Use Steel (not port), primitives as Rust functions, phased development, real-world validation.

---

## Success Metrics & Future

**Success**: Drop-in OpenJade replacement, 224 primitives identical, available in MacPorts/Homebrew, maintainable Rust codebase.

**Bonus**: REPL mode, watch mode, template debugger, S-expression input, JSON output, template libraries.

**Project Status**: In development | **License**: MIT

---

## Quick Reference

**OpenJade Analysis**: 72K lines C++ (224 primitives in `style/primitive.h`, 2,824-line SGML backend). OpenSP separate (100-150K lines) → replaced with libxml2.

**Implementation**: Hybrid approach - Steel provides R5RS (90 primitives), Dazzle implements 134 DSSSL primitives as Rust functions (grove queries, processing, types as stubs, utilities).

**Timeline**: 3-6 months. **MVP**: 6-8 weeks (Phases 1-3). **Near-complete**: 10-12 weeks (Phases 1-6).

**Risks**: Low (Steel/libxml2 proven). Medium (pattern matching, lazy evaluation, `next-match` chaining). **Mitigation**: Real-world templates, OpenJade comparison, tests from day one.
