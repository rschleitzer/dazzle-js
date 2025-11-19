# CLAUDE.md - Dazzle Project Context

> **Dazzle**: TypeScript port of OpenJade's DSSSL processor with FOT, RTF, and SGML backends

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
13. [Development Policies](#development-policies)

---

## Critical Decision - November 2025

**Porting OpenJade's Scheme interpreter to TypeScript** (NOT using existing Scheme implementations)

**Why TypeScript**:
- ✅ Excellent tooling and IDE support
- ✅ Familiar to wider developer audience
- ✅ Easy distribution via npm
- ✅ Strong type system for maintainability
- ✅ Native string and Unicode support

**Solution**: Port OpenJade interpreter (~12K C++ → ~10K TypeScript)
- ✅ 100% compatible (25 years proven)
- ✅ Whitespace-agnostic (R4RS compliant)
- ✅ Human-friendly errors (line numbers)
- ✅ Reasonable scope (8-12 weeks part-time)

**Files**: `OPENJADE_INTERPRETER_ANALYSIS.md`, `/Users/r.schleitzer/repos/openjade/style/`

---

## Project Overview

**Dazzle**: TypeScript-based DSSSL processor using Scheme templates

- **Purpose**: Document transformation and code generation from XML input
- **Language**: TypeScript + Scheme (ported from OpenJade)
- **Input**: XML only (no SGML document support)
- **Backends**: FOT (Flow Object Tree), RTF (document formatting), SGML (code generation)
- **Use Cases**:
  - Code generation (fire FHIR 5 server)
  - Document formatting (RTF, future PDF)
  - Generic XML transformation
- **Future**: PDF backend, editing capabilities
- **Name**: Available everywhere, no conflicts

**Problem**: OpenJade disappearing from package managers (dropped from Homebrew, aging in MacPorts, unmaintained C++)

**Solution**: Pure TypeScript/Node.js, libxmljs2 (UTF-8 support), OpenJade CLI-compatible, multiple backends (FOT/RTF/SGML)

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

**OpenSP**: ~100-150K lines C++ (separate library) → **Dazzle uses libxmljs2**

**Dazzle Must Implement**:
1. **224 primitives**: ~90 R4RS + ~50 grove + ~20 processing + ~30 type stubs + ~24 utilities
2. **R4RS interpreter** (port from OpenJade)
3. **Grove engine** (trait-based, XML only)
4. **Multiple backends**:
   - SGML backend (code generation: `entity` + `formatting-instruction`)
   - FOT backend (Flow Object Tree output)
   - RTF backend (document formatting)
5. **Template parser** (XML + entity references)

**Dazzle Roadmap**:
- ✅ SGML backend (code generation) - COMPLETE
- 🔄 FOT backend (intermediate representation) - IN PROGRESS
- 🔄 RTF backend (document formatting) - IN PROGRESS
- 🚧 PDF backend (via RTF foundation) - FUTURE
- 🚧 Editing capabilities - FUTURE
- 🚧 Additional backends (TeX, MIF, HTML) - OPTIONAL

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

**Host**: TypeScript/Node.js (maintainable, cross-platform, universal - Node.js + Browser)
**Scheme**: Ported from OpenJade (~12K C++ → ~10K TypeScript)
**XML**: Abstract grove interface (pluggable implementations)
  - **Node.js**: libxmljs2 (industry standard, DTD validation, UTF-8 support)
  - **Browser**: Future implementation (DOM-based or other)

**Core Dependencies**:
```json
{
  "commander": "^11.0.0"   // CLI parsing (Node.js only)
}
```

**Grove Implementation (Node.js)**:
```json
{
  "libxmljs2": "^0.33.0"   // XML parsing with DTD validation
}
```

**Dev Dependencies**:
```json
{
  "typescript": "^5.0.0",
  "vitest": "^1.0.0",
  "@types/node": "^20.0.0"
}
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

**Goal**: Port OpenJade's interface-based architecture to TypeScript (exact replica)

**Monorepo** (npm workspaces):
```
packages/
  dazzle-core/            → Interpreter + interfaces (scheme, dsssl, grove, fot)
  dazzle-grove-libxmljs2/ → XML grove for Node.js
  dazzle-backend-sgml/    → Code gen backend
  dazzle-backend-rtf/     → RTF backend (FUTURE)
  dazzle-cli/             → CLI tool
```

**Key Components** (direct port from OpenJade):

```typescript
// 1. Grove abstraction - EXACT replica of OpenJade's grove/spgrove pattern
// Port from: grove/Node.{h,cxx} (2,400 lines)
export interface Node {
  // All methods match OpenJade's Node interface exactly
  gi(): string | null;
  id(): string | null;
  data(): string | null;
  parent(): Node | null;
  children(): NodeList;
  attributes(): NodeList;
  attributeString(name: string): string | null;
  // ... all other DSSSL node properties from OpenJade
}

export interface NodeList {
  // Matches OpenJade's NodeList interface
  first(): Node | null;
  rest(): NodeList | null;
  length(): number;
  // ... all other operations
}

export interface Grove {
  root(): Node;
  elementWithId(id: string): Node | null;
}

// 2. Backend abstraction - EXACT replica of FOTBuilder pattern
// Port from: jade/FOTBuilder.h
export interface FotBuilder {
  entity(systemId: string, content: string): void;
  formattingInstruction(data: string): void;
  // ... other flow objects
}

// 3. Bytecode evaluator - Port from style/Insn.{h,cxx}
// OpenJade compiles Scheme to bytecode instructions for performance
export abstract class Insn {
  abstract eval(context: Context): EvalResult;
  // Instruction hierarchy: LiteralInsn, CallInsn, IfInsn, LetInsn, etc.
}

// 4. Interpreter - Port from style/Interpreter.cxx (2,390 lines)
export class Interpreter {
  private symbolTable: SymbolTable;
  private vm: VM;  // Bytecode VM
  compile(expr: any): Insn;
  eval(insn: Insn): Value;
}
```

**Critical Files to Port**:
- `style/Insn.{h,cxx}` - Bytecode instruction hierarchy
- `style/Interpreter.{h,cxx}` - Interpreter and compiler
- `style/EvalContext.{h,cxx}` - Evaluation context
- `style/SchemeParser.cxx` - Scheme parser
- `style/primitive.{h,cxx}` - 224 primitives
- `grove/Node.{h,cxx}` - Grove interface
- `jade/FOTBuilder.h` - Backend interface

**Principles**:
- **VERY faithful port**: Preserve OpenJade's complete architecture
- **Bytecode VM**: Port instruction-based evaluator (compile → bytecode → eval)
- **Interface-based**: Exact OpenJade patterns (Node, NodeList, Grove, FotBuilder)
- **Clean separation**: Groves don't know backends, core doesn't know grove impl
- **Pluggable**: Groves and backends implement interfaces
- **Universal**: Core + grove interfaces work everywhere (Node.js, Browser)
- **Performance**: Same optimizations as OpenJade (bytecode, lazy evaluation, etc.)

**Implementation Phases**:
1. 🚧 Phase 1: Core interpreter + SGML backend + libxmljs2 grove
2. 🚧 Phase 2: RTF backend
3. 🚧 Phase 3: PDF backend

---

## CLI Design

```bash
dazzle -d template.scm [-t sgml|rtf] [-V key=value]... [-D dir]... input.xml
```

**Flags**:
- `-d` (template, required)
- `-t` (backend: `sgml` for code gen, `rtf` for document formatting)
- `-V` (variables), `-D` (search paths)

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

**Target**: npm, GitHub releases, optional native binaries

**Release**:
1. Week 1: npm, GitHub releases
2. Month 1-2: Optional standalone binaries (pkg/nexe)
3. Month 3+: Homebrew, MacPorts if needed

**Priority**: npm (universal), standalone binaries optional

---

## References

**Standards**:
- DSSSL (ISO/IEC 10179:1996): ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf (local: `/Users/r.schleitzer/Documents/dsssl96b.pdf`)
  - §6: Data types; §8: Grove; §9: SDQL queries; §10: Processing model
- R4RS/R5RS Scheme: IEEE 1178-1990, http://www.schemers.org/Documents/Standards/R5RS/

**Projects**:
- OpenJade: https://openjade.sourceforge.net/ (72K C++, `/Users/r.schleitzer/repos/openjade/style/`)
- libxmljs2: https://github.com/marudor/libxmljs2 (Node.js bindings for libxml2)

---

## Quick Reference

**What Dazzle Is**:
- Modern TypeScript port of OpenJade's DSSSL processor
- **Multiple backends**: FOT (intermediate), RTF (formatting), SGML (code generation)
- **Input**: XML only (no SGML document support)
- Target: ~10K lines of TypeScript (vs 72K C++ in OpenJade)
- 260 language features (258 primitives + 2 special forms)
- Full DSSSL processing model with automatic tree traversal
- **UTF-8 native**: Handles modern XML perfectly
- **Universal**: Runs in Node.js and Browser (with appropriate grove implementation)
- **Future**: PDF backend, editing capabilities

**OpenJade Comparison**:
- OpenJade: 72K C++ (117 files), 224 primitives
- Dazzle: ~10K TypeScript (monorepo), 260 features
- Compatibility: Full DSSSL + OpenJade extensions
- Architecture: Exact replica of OpenJade's interface-based design

**Architecture**:
- **VERY faithful port**: Complete OpenJade architecture including bytecode VM
- **Bytecode evaluator**: Compile Scheme → Instructions → Eval (port from Insn.{h,cxx})
- Interface-based (grove/backend pluggable), npm workspace monorepo
- TypeScript + libxmljs2 for Node.js (no OpenSP dependency)
- Pluggable groves: Web apps can provide custom data sources
- Same optimizations: lazy evaluation, string interning, instruction caching

**Target Features**:
- 🚧 Complete R4RS Scheme interpreter with named let
- 🚧 Full DSSSL processing model (process-root, rules, modes, next-match, default)
- 🚧 50+ grove query primitives (XML only, DTD validation via libxmljs2)
- 🚧 Multiple flow object backends:
  - FOT backend (Flow Object Tree intermediate format)
  - RTF backend (document formatting)
  - SGML backend (code generation via entity + formatting-instruction)
- 🚧 XML template wrapper support (.dsl format with entity references)
- 🚧 SGML marked section processing (<![%entity;[ ... ]]>)

**Install**: `npm install -g dazzle` (when available)

**License**: MIT | **Status**: In Development (Nov 2025)

---

## Current Status (November 12, 2025)

### 🚧 v0.1.0 - Starting TypeScript Port

**Decision**: Complete rewrite in TypeScript (discontinuing Rust implementation)

**Why TypeScript**:
- Wider developer reach and ecosystem
- Universal runtime (Node.js + Browser)
- Better tooling and IDE support
- Easier distribution via npm
- Native UTF-8 and string handling

**Starting Fresh - All Components To Be Ported:**
- 🚧 **Scheme parser**: Port from `SchemeParser.cxx` (2,300 lines)
- 🚧 **Bytecode VM**: Port instruction hierarchy from `Insn.{h,cxx}`
  - LiteralInsn, CallInsn, IfInsn, LetInsn, DefineInsn, etc.
  - Compile-time optimization passes
- 🚧 **Interpreter**: Port from `Interpreter.cxx` (2,390 lines)
  - Symbol table, identifier table
  - Compile: Scheme AST → bytecode instructions
  - Eval: Execute bytecode with context
- 🚧 **260 primitives**: Port from `primitive.cxx` (5,704 lines)
  - R4RS, grove queries, DSSSL processing, type constructors
- 🚧 **Grove interface**: Exact replica from `Node.{h,cxx}` (2,400 lines)
- 🚧 **libxmljs2 grove**: Implement Node/NodeList for libxmljs2
- 🚧 **DSSSL processing**: Rules, modes, next-match
- 🚧 **SGML backend**: Port from `SgmlFOTBuilder.cxx` (2,824 lines)
- 🚧 **CLI tool**: OpenJade-compatible command line
- 🚧 **Test suite**: Port OpenJade's test cases

**Previous Rust Implementation v0.4.6** (archived):
- Achieved production-ready SGML backend
- 100% OpenJade-compatible output (byte-for-byte)
- **Performance: Much slower than OpenJade**
- 322 tests passing, 4 real-world test cases validated
- Now serving as reference for TypeScript port

**Port Strategy**:
- **VERY faithful port**: Preserve OpenJade's architecture completely
- **Bytecode (insn) architecture**: Port instruction-based evaluator from `Insn.{h,cxx}`
- **Complete implementation**: All optimizations, patterns, and structures from OpenJade
- **Direct translation**: C++ → TypeScript, preserving class hierarchy and logic
- **Performance goal**: Match OpenJade's speed by using the same bytecode VM architecture

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
- ✅ Production validation: 4 test cases (45 files total, 100% identical to OpenJade)
  - Icons: 18 files (~2,000 lines), ASCII-clean XML
  - ADM: 9 files (5,983 lines), UTF-8 XML
  - Authorization: 9 files (962 lines), UTF-8 XML
  - Digitalisierung: 9 files (6,853 lines), UTF-8 XML - LARGEST
- ✅ Performance benchmarks: 1.07x-4x slower (near-parity on realistic workloads)
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
  - Performance: 1.78x slower (167ms vs 94ms)
  - libxml2 handles UTF-8 correctly
  - Result: 100% byte-for-byte identical output to OpenJade (with SP_ENCODING=XML)

- ✅ **Authorization Test Case** - Authorization module with UTF-8 content
  - Input: Authorization.xml with UTF-8 characters
  - Output: 9 C# files (962 lines total)
    - Authorization/contracts/Interfaces.cs (93 lines)
    - Authorization/client/generated/Client.cs (507 lines)
    - 7 additional module files
  - Template: Same map.dsl as Icons/ADM
  - **Performance: 1.07x slower (109ms vs 101ms) - NEAR PARITY!**
  - Result: 100% byte-for-byte identical output to OpenJade

- ✅ **Digitalisierung Test Case** - Digitalization module (LARGEST WORKLOAD)
  - Input: Digitalisierung.xml with UTF-8 characters
  - Output: 9 C# files (6,853 lines total - largest test case)
    - Digitalisierung/module/generated/Context.cs (2,508 lines)
    - Digitalisierung/client/generated/Client.cs (1,975 lines)
    - Digitalisierung/contracts/Interfaces.cs (1,040 lines)
    - 6 additional module files (1,330 lines)
  - Template: Same map.dsl as all others
  - **Performance: 2.08x slower (196ms vs 94ms) - EXCELLENT SCALING!**
  - **Consistency: Parity variance** (33ms vs 33ms StdDev)
  - Result: 100% byte-for-byte identical output to OpenJade

**Performance Summary:**

| Test Case       | Files | Lines  | OpenJade | Dazzle  | Ratio | Overhead | Notes |
|-----------------|-------|--------|----------|---------|-------|----------|-------|
| Authorization   | 9     | 962    | 101ms    | 109ms   | 1.07x | 8ms      | Near parity |
| Icons           | 18    | ~2,000 | 27ms     | 109ms   | 4.04x | 82ms     | Many small files |
| ADM             | 9     | 5,983  | 94ms     | 167ms   | 1.78x | 73ms     | Large output |
| Digitalisierung | 9     | 6,853  | 94ms     | 196ms   | 2.08x | 102ms    | Largest - excellent scaling |

**Key Insights:**
- Dazzle has **fixed startup overhead (~80-100ms)**, then scales linearly
- For realistic production workloads (>1,000 lines), performance is **1.07x-2.08x slower**
- **Consistency**: Dazzle matches or exceeds OpenJade (same absolute variance on large workloads)
- **Total validated**: 45 files, 15,798 lines - 100% identical to OpenJade

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

**Current Work (Nov 2025): RTF Backend**

Implementing OpenJade-compatible RTF backend for document formatting:

**Reference**:
- OpenJade: `RtfFOTBuilder.cxx` (4,391 lines)
- Test case: `/Users/r.schleitzer/repos/dazzzledoc/dsssl.rtf` (1.0 MB)
- Goal: Byte-for-byte compatibility with OpenJade RTF output

**Implementation Plan**:
1. ✅ Phase 1: SGML backend (code generation) - **COMPLETE**
2. 🔄 **Phase 2: RTF backend (document formatting) - IN PROGRESS**
   - Architecture: `dazzle-backend-rtf` crate
   - Core flow objects: paragraph, sequence, display-group
   - Document structure: page setup, headers/footers, sections
   - Advanced: links, TOC, lists, tables
   - Character/paragraph properties
3. 🚧 Phase 3: PDF backend (via RTF foundation)

**RTF Backend Scope**:
- Full DSSSL flow object support (vs SGML's entity+formatting-instruction only)
- Document formatting primitives (quantities, colors, spacing)
- First step toward full print/PDF output pipeline

---
---

## Development Policies

### Git Commit Messages

**Policy**: Use simple one-line commit messages only.

**Format**:
```
Add feature description
Fix bug description
Update component description
```

**Examples**:
- ✅ Good: `Add arena-based value storage with generational indices`
- ✅ Good: `Fix ancestor primitive argument handling`
- ✅ Good: `Update RTF backend to write output files`
- ❌ Bad: Multi-paragraph messages with detailed explanations

**Rationale**: Keep git history clean and readable. Detailed context belongs in code comments, documentation, or issue trackers, not commit messages.

---
