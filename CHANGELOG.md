# Changelog

All notable changes to Dazzle will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-10-19

### Added

#### Core Interpreter
- **R4RS Scheme interpreter** ported from OpenJade (~12K C++ → 10K Rust)
  - Full parser with S-expression support
  - Instruction-based evaluator with proper tail call optimization
  - Environment model with lexical scoping
  - Garbage collection using `gc` crate
  - **Named let** support (syntactic sugar for recursive iteration)

#### Primitives (256 total, exceeding OpenJade's 224)
- **R4RS Core (~90 primitives)**:
  - Lists: `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`, `member`, `assoc`, `map`, `for-each`
  - Strings: `string`, `string-append`, `substring`, `string-ref`, `string-length`, `string=?`, `string<?`
  - Numbers: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `min`, `max`, `floor`, `ceiling`, `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`, `expt`, `abs`, `quotient`, `remainder`, `modulo`, `gcd`, `lcm`
  - Predicates: `null?`, `pair?`, `symbol?`, `number?`, `string?`, `boolean?`, `char?`, `vector?`, `procedure?`, `equal?`, `eqv?`, `eq?`
  - Characters: `char=?`, `char<?`, `char>?`, `char-upcase`, `char-downcase`, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char->integer`, `integer->char`
  - Logic: `not`, `and`, `or`

- **DSSSL Grove Query (~50 primitives)**:
  - Context: `current-node`
  - Navigation: `parent`, `children`, `ancestor`, `descendants`, `follow`, `preced`, `attributes`
  - Properties: `gi`, `id`, `data`, `node-property`, `attribute-string`, `inherited-attribute-string`
  - Node Lists: `node-list?`, `node-list-first`, `node-list-rest`, `node-list-length`, `node-list-ref`, `node-list-empty?`, `node-list-head`, `node-list-tail`, `node-list-sublist`, `node-list-map`, `node-list-reduce`, `node-list-filter`, `node-list-union`, `node-list-intersection`, `node-list-difference`
  - Selection: `select-elements`, `element-with-id`, `match-element?`
  - Position: `child-number`, `element-number`, `first-sibling?`, `last-sibling?`, `absolute-first-sibling?`, `hierarchical-number`
  - Entities: `entity-system-id`, `entity-public-id`, `entity-text`, `entity-notation`
  - Notations: `notation-system-id`, `notation-public-id`

- **Processing & Sosofo (~20 primitives)**:
  - Processing: `process-children`, `process-node-list`, `next-match`
  - Sosofo: `sosofo?`, `sosofo-append`, `literal`, `empty-sosofo`, `make`, `make-entity`
  - Formatting: `format-number`, `format-number-list`

- **DSSSL Type System (~30 primitives, stubbed)**:
  - Quantities: `quantity?`, `number->quantity`, `quantity-convert`
  - Spacing: `display-space?`, `inline-space?`
  - Colors: `color?`, `color-rgb`
  - Addresses: `address?`, `current-node-address`
  - Glyphs: `glyph?`, `glyph-id`
  - Character properties: `char-property`, `char-script`

- **Utilities & Extensions (~66 primitives)**:
  - Keywords: `keyword?`, `keyword->string`, `string->keyword`
  - Time: `time`, `time<?`, `time>?`, `time=?`, `time<=?`, `time>=?`
  - Language: `language?`, `current-language`, `with-language`
  - Style: `style`, `mode`, `with-mode`
  - Debug: `debug`, `error`
  - Named node lists: `node-list-union-map`, `node-list-every?`, `node-list-some?`
  - DTD/Document: `declaration`, `dtd`, `sgml-declaration`, `document-element`, `prolog`, `epilog`

#### Grove Implementation
- **libxml2-based grove** (`dazzle-grove-libxml2`)
  - Full XML parsing with DTD validation
  - Entity reference resolution
  - DSSSL grove trait implementation
  - Safe Rust wrappers around libxml2 C API
  - Comprehensive node and node list support

#### SGML Backend
- **Code generation backend** (`dazzle-backend-sgml`)
  - File output via `make entity` flow object
  - Text output via `literal` and `formatting-instruction`
  - Multiple file generation from single template
  - Output directory management

#### CLI Tool
- **Command-line interface** (`dazzle-cli`)
  - Template loading: `-d/--template` (required)
  - Variable passing: `-V/--var KEY=VALUE` (multiple)
  - Search paths: `-D/--dir DIR` (multiple)
  - Backend selection: `-t/--backend TYPE` (xml, text)
  - Verbose logging: `-v/--verbose`
  - Auto DTD validation when `<!DOCTYPE>` present
  - Template file discovery in search paths
  - Environment variable passing to Scheme evaluator

#### Testing & Quality
- **322 tests passing**:
  - 276 core interpreter tests (lists, strings, numbers, predicates, control flow)
  - 15 libxml2 grove tests
  - 9 grove integration tests
  - 6 SGML output tests
  - 3 named let tests
  - 13 other tests
- **Zero warnings** in workspace build
- **Zero clippy warnings**

#### Documentation
- Comprehensive README.md with:
  - Installation and quick start
  - Complete command-line reference
  - Real-world code generation example
  - 256 primitives catalog
  - Architecture diagram
  - Use cases and examples
  - Comparison with OpenJade
  - Known limitations
- CLAUDE.md project context for AI assistants
- Working examples in `examples/rust-codegen/`
- Complete primitive reference in README

#### Infrastructure
- Multi-crate workspace:
  - `dazzle-core`: Scheme interpreter + traits
  - `dazzle-grove-libxml2`: XML grove implementation
  - `dazzle-backend-sgml`: Code generation backend
  - `dazzle-cli`: Command-line tool
- Optimized release profile (LTO, opt-level 3, stripped binaries)
- MIT License
- Proper crates.io metadata for all crates

### Performance
- Instruction-based evaluator for efficient execution
- String interning for symbols
- Lazy node list evaluation
- Minimal allocations in hot paths
- Release builds optimized with LTO

### Known Limitations
- Flow objects: Only `literal`, `formatting-instruction`, and `make entity` (full FOT in future)
- Processing modes: Not yet implemented (planned for v0.2)
- External procedures: Not yet implemented (planned for v0.2)
- SGML grove: Only XML via libxml2 (full SGML with OpenSP in future)

### Acknowledgments
- OpenJade project and maintainers for the original implementation
- James Clark for DSSSL specification and Jade
- DSSSL community for grove model and processing language design

---

## [0.2.0] - 2025-10-20

### Added

#### DSSSL Processing Model (OpenJade-compatible)
- **Full DSSSL processing engine** with automatic tree traversal
  - `process-root`: Starts DSSSL processing from document root
  - Rule matching: Automatic element/root matching based on construction rules
  - Construction mode support: Multiple processing passes with different rule sets
  - `next-match`: Explicit continuation to next matching rule
  - OpenJade's ProcessContext architecture ported to Rust

#### Flow Objects
- **`make` special form** for creating flow objects
  - `make entity system-id: "file.txt"`: Write generated code to files
  - `make formatting-instruction data: "text"`: Append text to output buffer
  - Nested flow objects with proper buffer management
  - Multiple file generation from single template run

#### Template Features
- **XML template wrapper support** (`.dsl` format)
  - Entity reference resolution: `<!ENTITY name SYSTEM "file.scm">`
  - DOCTYPE parsing and entity loading
  - Compatible with OpenJade/DSSSL style sheets
  - Auto-detection of XML vs plain Scheme templates

#### Language Features
- **DSSSL keyword syntax**: Trailing colon notation (`system-id:`, `data:`)
- **`define-language` special form**: Defines language identifiers for `declare-default-language`
- **`declare-flow-object-class` special form**: Registers flow object class names as symbols
- **Multi-list `map` and `for-each`**: Full R4RS compliance for operating on multiple lists
- **`node-list->list` primitive**: Convert DSSSL node-lists to Scheme lists

#### Backend Improvements
- **Buffer management API** in `FotBuilder` trait:
  - `current_output()`: Retrieve accumulated text
  - `clear_buffer()`: Reset buffer after file write
  - Proper separation between buffer operations and file I/O

### Fixed
- **`define-language` and `declare-flow-object-class`** are now special forms (not primitives)
  - Variables are properly defined in environment
  - Compatible with OpenJade semantics
- **`map` and `for-each`** now accept multiple lists (R4RS requirement)
  - `(map + '(1 2 3) '(4 5 6))` → `(5 7 9)`
  - All lists must have same length (runtime check)

### Performance
- **4x faster than OpenJade** in release mode (5ms vs 18ms on Scaly parser generation)
- **Optimized release builds** with LTO and aggressive optimization
- Debug builds slower (~7x) but release builds significantly faster
- Successfully generates production code: 170KB Scaly parser (5,532 lines)

### Production Validation
- ✅ **Scaly Language Compiler**: Successfully generates Parser.scaly from XML grammar
- ✅ **Real-world template**: 5 entity files, complex DSSSL rules
- ✅ **Large output**: 170KB generated code (5,532 lines)
- ✅ **Performance**: Faster than 25-year-old OpenJade in production use

### Changed
- Primitive count: **258 primitives + 2 special forms** (total 260 language features)
- `make` is now a special form (was primitive in planning docs)
- Backend trait requires buffer management methods

### Documentation
- Updated FIXES_IMPLEMENTED.md with v0.1.1 compatibility fixes
- Performance benchmarking results documented
- Production validation with Scaly compiler

---

## [Unreleased]

### Planned for v0.3.0
- Full flow object support (`make paragraph`, `make sequence`, etc.)
- External procedure interface
- Additional grove implementations (OpenSP for full SGML)
- Additional backends (RTF, TeX, MIF, HTML)

[0.2.0]: https://github.com/rschleitzer/dazzle/releases/tag/v0.2.0
[0.1.0]: https://github.com/rschleitzer/dazzle/releases/tag/v0.1.0
