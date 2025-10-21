# CLAUDE.md - Dazzle Project Context

> **Dazzle**: Rust port of OpenJade's SGML backend for modern code generation

## Table of Contents

1. [Critical Decision](#critical-decision---october-2025)
2. [Project Overview](#project-overview)
3. [DSSSL Lineage](#dsssl-lineage)
4. [OpenJade Analysis](#openjade-analysis)
5. [Primitive Catalog](#primitive-catalog)
6. [Technical Stack](#technical-stack)
7. [Architecture Design](#architecture-design)
8. [CLI Design](#cli-design)
9. [Distribution](#distribution)
10. [References](#references)
11. [Quick Reference](#quick-reference)
12. [Current Status](#current-status-october-20-2025)

---

## Critical Decision - October 2025

**Porting OpenJade's Scheme interpreter to Rust** (NOT using Steel Scheme)

**Steel Issues** (Oct 19, 2025):
- Parser bug: `let` bindings must be on same line (not R4RS compliant)
- Error reporting: spans (byte offsets), not line numbers
- User's existing DSSSL has multi-line `let` bindings (sql.scm:652, 819, 828)

**Solution**: Port OpenJade interpreter (~12K C++ → ~10K Rust)
- ✅ 100% compatible (25 years proven)
- ✅ Whitespace-agnostic (R4RS compliant)
- ✅ Human-friendly errors (line numbers)
- ✅ Reasonable scope (8-12 weeks part-time)

**Files**: `OPENJADE_INTERPRETER_ANALYSIS.md`, `/Users/r.schleitzer/repos/openjade/style/`

---

## Project Overview

**Dazzle**: Rust-based code generation tool using Scheme templates

- **Purpose**: Template-driven code generation from XML input
- **Language**: Rust + Scheme (ported from OpenJade)
- **Use Case**: fire (FHIR 5 server) code generation
- **Name**: Available everywhere, no conflicts

**Problem**: OpenJade disappearing from package managers (dropped from Homebrew, aging in MacPorts, unmaintained C++)

**Solution**: Pure Rust, libxml2 (UTF-8 support), OpenJade CLI-compatible, focus on code generation

---

## DSSSL Lineage

**1. DSSSL Standard (ISO/IEC 10179:1996)**
- Spec: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf (local: `/Users/r.schleitzer/Documents/dsssl96b.pdf`)
- Based on R4RS Scheme (side-effect-free subset)
- Defines: SDQL (grove queries), grove model, flow objects, style specs
- **Doesn't standardize**: File I/O (`load`), SGML backend

**Key Sections**:
- §8: Grove architecture, node properties
- §9: SDQL (grove queries)
- §10: Processing model (rules, modes, `next-match`)
- §6: Data types (quantities, colors, addresses)

**2. Jade (James Clark, ~1996)**
- First major DSSSL implementation (C++)
- Backends: RTF, TeX, MIF, HTML, **SGML**
- Added `load` procedure, external procedures (not in DSSSL standard)

**3. OpenJade (1999-2010)**
- Community fork of Jade
- More backends, bug fixes
- Last activity: ~2010
- Status: Unmaintained, hard to build

---

## OpenJade Analysis

**Source**: OpenJade 1.3.2 (April 2003)

**Codebase**: ~72K lines C++ (117 files)
- `style/` (39K lines) - Interpreter, evaluator, **224 primitives**
- `jade/` (20K lines) - FOT builders, **SGML backend**
- `spgrove/` (7K lines) - OpenSP integration
- `grove/` (2.4K lines) - Grove model

**Critical Files**:
- `primitive.h` - 224 primitives
- `primitive.cxx` - 5,704 lines (implementations)
- `Interpreter.cxx` - 2,390 lines (interpreter core)
- `SchemeParser.cxx` - 2,300 lines (parser)
- `SgmlFOTBuilder.cxx` - 2,824 lines (SGML backend)
- `Node.{h,cxx}` - 2,400 lines (grove interface)

**OpenSP**: ~100-150K lines C++ (separate library) → **Dazzle uses libxml2**

**Dazzle Must Implement**:
1. **224 primitives**: ~90 R4RS + ~50 grove + ~20 processing + ~30 type stubs + ~24 utilities
2. **R4RS interpreter** (port from OpenJade)
3. **Grove engine** (trait-based)
4. **SGML backend** (only `entity` + `formatting-instruction`)
5. **Template parser** (XML + entity references)

**Dazzle Does NOT Need**:
- Other FOT builders (RTF, TeX, MIF, HTML)
- Document formatting primitives (quantities, colors, spacing) - stub only
- OpenSP (use libxml2)

---

## Primitive Catalog

**Total**: 224 primitives (from OpenJade `primitive.h`)

**R4RS Primitives (~90)** - Port from OpenJade:
- Lists (15): `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`, `member`, `assoc`, etc.
- Strings (14): `string`, `string-append`, `substring`, `string-ref`, `string-length`, etc.
- Numbers (42): `+`, `-`, `*`, `/`, `<`, `>`, `min`, `max`, `floor`, `sqrt`, `sin`, `cos`, etc.
- Predicates (14): `null?`, `pair?`, `symbol?`, `number?`, `string?`, etc.
- Logic (3): `not`, `equal?`, `eqv?`
- Characters (5): `char=?`, `char<?`, `char-upcase`, `char-downcase`, etc.
- Vectors (8): `vector`, `vector-ref`, `vector-set!`, `make-vector`, etc.

**Grove Primitives (~50)** - Critical (DSSSL §9):
- **Context**: `current-node`
- **Node lists**: `node-list?`, `node-list-first`, `node-list-rest`, `node-list-length`, `empty-node-list`, etc.
- **Properties**: `gi`, `id`, `data`, `node-property`, `attribute-string`, `inherited-attribute-string`
- **Navigation**: `parent`, `ancestor`, `children`, `descendants`, `follow`, `preced`, `attributes`
- **Selection**: `select-elements`, `element-with-id`, `match-element?`
- **Position**: `first-sibling?`, `last-sibling?`, `child-number`, `element-number`
- **Entities/Notations**: `entity-system-id`, `entity-public-id`, `entity-text`, `notation-system-id`

**Processing & Sosofo (~20)** - High priority:
- `process-children`, `process-node-list`, `next-match`, `sosofo-append`, `literal`, `empty-sosofo`
- `format-number` (I/II/III, 1/2/3, a/b/c), `format-number-list` (1.2.3)

**DSSSL Types (~30)** - **Stubs only** (not needed for code gen):
- Quantities (4), Spacing (5), Colors (4), Addresses (8), Glyphs (5), Char properties (2)
- Return dummy values, implement if templates use them

**Extensions & Utilities (~24)**:
- Keywords (3), Time (6), Language (4), Style (3), Debug (4), Named node lists (6), etc.

---

## Technical Stack

**Host**: Rust (maintainable, cross-platform, good FFI)
**Scheme**: Ported from OpenJade (~12K C++ → ~10K Rust) - NOT Steel (parser bugs)
**XML**: libxml2 (industry standard, DTD validation, UTF-8 support, clean C FFI)

**Dependencies**:
```toml
libxml = "0.3"    # libxml2 bindings
clap = "4"        # CLI parsing
gc = "0.5"        # Garbage collection
```

**Template Format**: XML wrapper with entity references (OpenJade compatible)

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

Parser resolves entities, extracts `<style-specification>` content, passes to interpreter. Use `<![CDATA[...]]>` if `.scm` contains `<` or `&`.

---

## Architecture Design

**Goal**: Port OpenJade's trait-based architecture to Rust

**Workspace** (multi-crate):
```
dazzle-core/          → Interpreter + traits (scheme, dsssl, grove, fot)
dazzle-grove-libxml2/ → XML grove (priority 1)
dazzle-backend-sgml/  → Code gen backend (priority 1)
dazzle-cli/           → CLI
```

Future: `dazzle-grove-opensp`, `dazzle-backend-{rtf,tex,mif,html}`

**Key Traits**:

```rust
// Grove abstraction (port of grove/spgrove pattern)
pub trait Node {
    fn gi(&self) -> Option<&str>;  // Element name
    fn id(&self) -> Option<&str>;
    fn data(&self) -> Option<&str>;
    fn parent(&self) -> Option<Box<dyn Node>>;
    fn children(&self) -> Box<dyn NodeList>;
    fn attributes(&self) -> Box<dyn NodeList>;
    fn attribute_string(&self, name: &str) -> Option<String>;
    // ... all DSSSL node properties
}

pub trait NodeList {
    fn first(&self) -> Option<Box<dyn Node>>;
    fn rest(&self) -> Option<Box<dyn NodeList>>;
    fn length(&self) -> usize;
    // ... all DSSSL operations
}

pub trait Grove {
    fn root(&self) -> Box<dyn Node>;
    fn element_with_id(&self, id: &str) -> Option<Box<dyn Node>>;
}

// Backend abstraction (port of FOTBuilder pattern)
pub trait FotBuilder {
    fn entity(&mut self, system_id: &str, content: &str) -> Result<()>;
    fn formatting_instruction(&mut self, data: &str) -> Result<()>;
    // ... other flow objects for document formatting
}

// Interpreter (port of style/)
pub struct Interpreter {
    symbols: SymbolTable,
    identifiers: IdentifierTable,
    grove: Box<dyn Grove>,           // Pluggable
    backend: Box<dyn FotBuilder>,    // Pluggable
    collector: gc::Gc,
    // ...
}
```

**Principles**:
- Trait-based abstraction (groves, backends pluggable)
- Faithful port (preserve structure + optimizations)
- Clean separation (groves don't know backends, etc.)
- Testable components

**Migration**:
1. Phase 1: SGML backend + libxml2 (code gen)
2. Phase 2: RTF/TeX/MIF/HTML backends
3. Phase 3: OpenSP grove (full SGML)

---

## CLI Design

```bash
dazzle -d template.scm [-t xml] [-V key=value]... [-D dir]... input.xml
```

**Flags**: `-d` (template, required), `-V` (variables), `-D` (search paths), `-t` (backend: xml/text)
**Auto**: DTD validation if `<!DOCTYPE>`, output via template (no `-o`)

**Examples**:
```bash
dazzle -d codegen.scm grammar.xml
dazzle -d gen.scm -V package=com.example -V version=1.0 model.xml
dazzle -d gen.scm -V outdir=src/generated -D /usr/share/dazzle input.xml
```

**Template writes files**:
```scheme
(define outdir (get-variable "outdir" "generated"))
(write-file (string-append outdir "/" name ".java") (generate-code name))
```

---

## Distribution

**Target**: crates.io, Arch AUR, Homebrew, MacPorts, Fedora, openSUSE, Debian

**Release**:
1. Week 1: crates.io, GitHub releases, musl binaries
2. Month 1-2: Arch AUR, Homebrew tap, MacPorts submission
3. Month 3-6: Homebrew core, MacPorts official, Fedora
4. 6+ months: Debian/Ubuntu

**Priority**: MacPorts (stable) + Homebrew (popular) for macOS, Arch AUR (fast) for Linux

---

## References

**Standards**:
- DSSSL (ISO/IEC 10179:1996): ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf (local: `/Users/r.schleitzer/Documents/dsssl96b.pdf`)
  - §6: Data types; §8: Grove; §9: SDQL queries; §10: Processing model
- R4RS/R5RS Scheme: IEEE 1178-1990, http://www.schemers.org/Documents/Standards/R5RS/

**Projects**:
- OpenJade: https://openjade.sourceforge.net/ (72K C++, `/Users/r.schleitzer/repos/openjade/style/`)
- libxml2: https://gitlab.gnome.org/GNOME/libxml2

---

## Quick Reference

**What Dazzle Is**:
- Modern Rust port of OpenJade's DSSSL processor for code generation
- 10K lines of Rust (vs 72K C++ in OpenJade)
- 260 language features (258 primitives + 2 special forms)
- **~4x slower** than OpenJade (~110ms vs ~27ms on Icons test case)
- Full DSSSL processing model with automatic tree traversal
- Production-validated on real-world code generation (Icons + ADM test cases)

**OpenJade Comparison**:
- OpenJade: 72K C++ (117 files), 224 primitives, ~27ms
- Dazzle: 10K Rust (multi-crate), 260 features, ~110ms
- Compatibility: Full DSSSL + OpenJade extensions
- Performance: **~4x slower** but still fast (<200ms for 18-file generation)
- **UTF-8**: Dazzle handles modern XML with UTF-8 characters

**Architecture**:
- Trait-based (grove/backend pluggable), multi-crate workspace
- Faithful port with optimizations: instruction-based eval, string interning, lazy lists
- Pure Rust + libxml2 (no OpenSP dependency)

**Key Features**:
- ✅ Complete R4RS Scheme interpreter with named let
- ✅ Full DSSSL processing model (process-root, rules, modes, next-match)
- ✅ 50+ grove query primitives (XML + DTD validation)
- ✅ Flow objects (make entity, formatting-instruction)
- ✅ XML template wrapper support (.dsl format)
- ✅ 322 tests passing, zero warnings

**Install**: `cargo install dazzle`

**License**: MIT | **Status**: v0.2.0 Production Ready (Oct 2025)

---

## Current Status (October 21, 2025)

### 🚀 v0.2.0 - PRODUCTION READY

**Published to crates.io**: https://crates.io/crates/dazzle

**All 8 Phases Completed:**
- ✅ Phase 1: Architecture & Traits (Oct 17-19)
- ✅ Phase 2: Scheme Interpreter (Oct 17-19)
- ✅ Phase 3: libxml2 Grove (Oct 17-19)
- ✅ Phase 4: SGML Backend & Processing (Oct 19-20)
- ✅ Phase 5: 258 Primitives + 2 Special Forms (Oct 19-20)
- ✅ Phase 6: CLI & Loading (Oct 19)
- ✅ Phase 7: Testing & Documentation (Oct 19)
- ✅ Phase 8: Distribution (Oct 20 - **PUBLISHED TO CRATES.IO**)

**Implementation Stats:**
- **Lines of Code**: ~10,000 Rust (vs 72,000 C++ in OpenJade)
- **Language Features**: **260 total** (258 primitives + 2 special forms)
- **Tests**: 322 passing (100% success rate)
- **Build**: Zero warnings, zero errors
- **Performance**: **~4x slower than OpenJade** (~110ms vs ~27ms on Icons test case)

**v0.2.0 Major Features:**
- ✅ **DSSSL Processing Model** - Full OpenJade-compatible automatic tree traversal
  - `process-root`: Automatic DSSSL processing from document root
  - Rule matching: Element and root construction rules
  - Construction modes: Multiple processing passes
  - `next-match`: Explicit continuation to next rule
- ✅ **Flow Objects** - `make` special form for file generation
  - `make entity system-id: "file.txt"`: Write files
  - `make formatting-instruction data: "text"`: Append text
  - Nested flow objects with proper buffer management
- ✅ **XML Template Wrapper Support** - OpenJade-compatible `.dsl` format
  - Entity reference resolution: `<!ENTITY name SYSTEM "file.scm">`
  - DOCTYPE parsing and multi-file template loading
  - Auto-detection of XML vs plain Scheme templates
- ✅ **DSSSL Keywords** - Trailing colon syntax (`system-id:`, `data:`)
- ✅ **Multi-list operations** - R4RS-compliant `map` and `for-each`
- ✅ **Production Validated** - Generates 170KB Scaly parser (5,532 lines)

**Feature Highlights:**
- ✅ Complete R4RS Scheme interpreter with named let
- ✅ Full DSSSL grove query primitives (50+)
- ✅ Full DSSSL processing model (process-root, rules, modes, next-match)
- ✅ libxml2 integration with DTD validation
- ✅ **UTF-8 support**: Handles modern XML
- ✅ SGML backend with buffer management
- ✅ CLI with template loading, variables, search paths
- ✅ XML template wrapper (.dsl format) support
- ✅ **100% output compatibility** with OpenJade
- ✅ Real-world validation: production code generation (Icons + ADM)
- ✅ Comprehensive test coverage
- ✅ Full documentation (README, CHANGELOG, examples, primitive reference)

**Deliverables:**
- ✅ Published to crates.io (dazzle v0.2.0)
- ✅ 4 published crates (core, grove-libxml2, backend-sgml, cli)
- ✅ Working CLI tool (`dazzle`)
- ✅ Production validation: 2 test cases (27 files total, 100% identical to OpenJade)
  - Icons: 18 files, ASCII-clean XML
  - ADM: 9 files (5,983 lines), UTF-8 XML
- ✅ Performance benchmarks: ~110ms for 18-file generation (vs ~27ms OpenJade)
- ✅ Complete documentation (README, CHANGELOG, examples)
- ✅ Test suite (322 tests, 100% passing)

**Production Validation:**
- ✅ **Icons Test Case** - Generates C# interfaces, clients, services from XML
  - Input: Icons.xml with entity definitions
  - Output: 18 C# files (contracts, clients, repositories, services)
  - Template: 14 entity files with complex DSSSL rules
  - Performance: ~110ms (vs ~27ms in OpenJade)
  - Result: 100% byte-for-byte identical output to OpenJade

- ✅ **ADM Test Case** - Administrative module code generation with UTF-8 content
  - Input: ADM.xml with UTF-8 German characters (ü, ö, etc.)
  - Output: 9 C# files (5,983 lines total)
    - ADM/contracts/Interfaces.cs (825 lines)
    - ADM/module/generated/Context.cs (843 lines)
    - ADM/client/generated/Client.cs (2,863 lines)
    - 6 additional module files
  - Template: Same map.dsl as Icons
  - libxml2 handles UTF-8 correctly
  - Result: 100% byte-for-byte identical output to OpenJade (with SP_ENCODING=XML)

**Distribution Status:**
- ✅ **crates.io**: Published v0.2.0 (Oct 20, 2025)
  - dazzle v0.2.0
  - dazzle-core v0.2.0
  - dazzle-grove-libxml2 v0.2.0
  - dazzle-backend-sgml v0.2.0
- 🚧 **GitHub Release**: Pending (v0.2.0 tag)
- 🚧 **Homebrew**: Planned
- 🚧 **MacPorts**: Planned
- 🚧 **AUR**: Planned

**Install:**
```bash
cargo install dazzle
```

**Next Steps:**
- Create GitHub v0.2.0 release with binaries
- Submit to Homebrew core
- Submit to MacPorts
- Create AUR package
- Begin work on v0.3.0 (full flow object support)

---
