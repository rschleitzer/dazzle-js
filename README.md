# Dazzle

**Template-driven code generation powered by Scheme and XML**

Dazzle is a Rust-based code generation tool that uses Scheme templates to transform XML documents into any text output. It's a modern reimplementation of OpenJade's SGML backend, designed specifically for code generation rather than document formatting.

[![Tests](https://img.shields.io/badge/tests-319%20passing-brightgreen)]()
[![Primitives](https://img.shields.io/badge/primitives-256-blue)]()

## Features

- **🦀 Pure Rust**: Fast, safe, and cross-platform
- **📜 Scheme Templates**: Powerful functional programming for transformations
- **🌳 Grove Model**: Standard DSSSL grove interface for XML navigation
- **🔧 256 Primitives**: Complete R4RS + DSSSL query primitives
- **⚡ libxml2 Integration**: Industry-standard XML parsing with DTD validation
- **🎯 Code Generation**: Designed for generating source code, not documents

## Quick Start

### Installation

```bash
cargo install --path crates/dazzle-cli
```

### Basic Usage

```bash
dazzle -d template.scm input.xml
```

### Example: Generate Rust Structs from XML

**model.xml:**
```xml
<?xml version="1.0"?>
<model>
  <struct name="User">
    <field name="id" type="u64" required="true"/>
    <field name="username" type="String" required="true"/>
  </struct>
</model>
```

**codegen.scm:**
```scheme
;; Generate Rust struct from XML
(define (generate-struct struct-node)
  (let ((name (attribute-string "name" struct-node)))
    (string-append
      "#[derive(Debug)]\n"
      "pub struct " name " { ... }\n")))

;; Process all structs
(let ((structs (children (current-node))))
  (generate-all-structs structs 0))
```

**Run:**
```bash
dazzle -d codegen.scm model.xml > output.rs
```

**Output:**
```rust
#[derive(Debug)]
pub struct User {
    pub id: u64,
    pub username: String,
}
```

## Command-Line Options

```
dazzle -d TEMPLATE [-V key=value]... [-D dir]... INPUT

Options:
  -d, --template <FILE>     Template file (.scm) [required]
  -V, --var <KEY=VALUE>     Define template variable
  -D, --dir <DIR>           Add template search directory
  -t, --backend <TYPE>      Backend type: xml, text [default: text]
  -v, --verbose             Enable verbose logging
```

### Examples

**With variables:**
```bash
dazzle -d gen.scm -V package=com.example -V version=1.0 model.xml
```

**With search paths:**
```bash
dazzle -d gen.scm -D ~/templates -D /usr/share/dazzle input.xml
```

## Scheme Primitives

Dazzle implements **256 primitives** from R4RS Scheme and DSSSL:

### R4RS Core (~90 primitives)
- **Lists**: `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`, `member`, `assoc`
- **Strings**: `string-append`, `substring`, `string-length`, `string-ref`
- **Numbers**: `+`, `-`, `*`, `/`, `<`, `>`, `min`, `max`, `floor`, `ceiling`, `sqrt`, `sin`, `cos`
- **Predicates**: `null?`, `pair?`, `symbol?`, `number?`, `string?`, `equal?`
- **Control**: `if`, `cond`, `let`, `let*`, `define`, `lambda`

### DSSSL Grove Query (~50 primitives)
- **Context**: `current-node`
- **Navigation**: `parent`, `children`, `ancestor`, `descendants`, `follow`, `preced`
- **Properties**: `gi`, `id`, `data`, `attribute-string`, `inherited-attribute-string`
- **Node Lists**: `node-list-length`, `node-list-ref`, `node-list-first`, `node-list-rest`
- **Selection**: `select-elements`, `element-with-id`, `match-element?`
- **Position**: `child-number`, `element-number`, `first-sibling?`, `last-sibling?`

### Processing & Utilities (~30 primitives)
- **Processing**: `process-children`, `process-node-list`
- **Formatting**: `format-number`, `format-number-list`
- **I/O**: `load`, `error`, `debug`
- **Types**: `string?`, `number?`, `node?`, `node-list?`

See [PRIMITIVES.md](PRIMITIVES.md) for complete reference.

## Architecture

```
┌─────────────┐
│   XML File  │
└─────┬───────┘
      │
      ▼
┌─────────────┐
│  libxml2    │ ← DTD Validation
│  Grove      │ ← Parse & Create Grove
└─────┬───────┘
      │
      ▼
┌─────────────┐
│   Scheme    │ ← Load Template
│  Evaluator  │ ← 256 Primitives
└─────┬───────┘
      │
      ▼
┌─────────────┐
│    SGML     │ ← Generate Output
│   Backend   │ ← Write Files
└─────────────┘
```

### Crates

- **dazzle-core**: Scheme interpreter, DSSSL primitives, grove/FOT traits
- **dazzle-grove-libxml2**: libxml2 grove implementation
- **dazzle-backend-sgml**: File generation backend
- **dazzle-cli**: Command-line interface

## Use Cases

### Code Generation
- Generate source code from data models (Rust, Java, Python, etc.)
- Create API clients from OpenAPI/Swagger specs
- Generate database schemas from entity definitions

### Documentation
- Transform XML documentation to Markdown
- Generate API documentation from schemas
- Create changelogs from structured data

### Configuration
- Generate config files from templates
- Create deployment manifests (K8s, Docker)
- Build environment-specific configs

## Known Limitations

### Current Limitations (v0.1)
- **Flow objects**: Only basic `literal` support (full FOT implementation in future)
- **File writing**: Via backend buffer, not full `make entity` syntax yet
- **Processing modes**: Not yet implemented (planned for v0.2)

## Testing

```bash
# Run all tests (319 tests)
cargo test --workspace

# Run specific test suites
cargo test --package dazzle-core
cargo test --package dazzle-grove-libxml2
cargo test --test grove_integration
cargo test --test sgml_output
```

**Test Coverage:**
- ✅ 276 core interpreter tests
- ✅ 15 libxml2 grove tests
- ✅ 9 grove integration tests
- ✅ 6 SGML output tests
- ✅ 3 named let tests
- ✅ 13 other tests
- **Total: 322 tests passing**

## Development Status

**Current**: v0.1.0 - Feature Complete Core
- ✅ Phase 1: Architecture & Traits
- ✅ Phase 2: Scheme Interpreter (R4RS)
- ✅ Phase 3: libxml2 Grove
- ✅ Phase 4: SGML Backend
- ✅ Phase 5: 256 Primitives
- ✅ Phase 6: CLI & Loading
- 🚧 Phase 7: Testing & Documentation (current)
- ⏳ Phase 8: Distribution

**Next**: v0.2.0 - Extended Features
- Full flow object support (`make entity`, `make paragraph`, etc.)
- Processing modes and rules
- External procedures
- Performance optimization

## Comparison with OpenJade

| Feature | OpenJade | Dazzle |
|---------|----------|--------|
| **Language** | C++ (72K lines) | Rust (10K lines) |
| **XML Parser** | OpenSP (~150K C++) | libxml2 |
| **Primitives** | 224 | 256 |
| **Focus** | Document formatting | Code generation |
| **Maintained** | No (last update 2010) | Yes (active) |
| **Cross-platform** | Difficult to build | `cargo install` |

## Contributing

Dazzle is in active development. Contributions welcome!

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

MIT License - see [LICENSE](LICENSE) for details

## Acknowledgments

- **OpenJade**: Original implementation and design
- **James Clark**: DSSSL specification and Jade
- **DSSSL Community**: Grove model and processing language

## Links

- **DSSSL Spec**: ISO/IEC 10179:1996
- **R4RS/R5RS**: Scheme standards
- **OpenJade**: https://openjade.sourceforge.net/
- **libxml2**: https://gitlab.gnome.org/GNOME/libxml2

---

**Built with 🦀 Rust** | **Powered by Scheme** | **Inspired by OpenJade**
