# CLAUDE.md - Dazzle Project Context

> **Dazzle**: CLI wrapper for `@scaly/openjade` - a TypeScript port of OpenJade's DSSSL processor

## Project Overview

**Dazzle** is a thin CLI wrapper around `@scaly/openjade`, providing the `dazzle` command for DSSSL document transformation.

- **Core Engine**: `@scaly/openjade` (full TypeScript port of OpenJade)
- **SGML Parser**: `@scaly/opensp` (TypeScript port of OpenSP)
- **Purpose**: Document transformation and code generation using DSSSL stylesheets
- **Backends**: FOT, RTF, TeX, SGML, XML, MIF

## Architecture

```
dazzle/
├── src/
│   ├── index.ts    # Re-exports @scaly/openjade
│   └── cli.ts      # CLI entry point (dazzle command)
├── package.json    # Depends on @scaly/openjade, @scaly/opensp
└── tsconfig.json
```

**Dependencies**:
- `@scaly/openjade` - Full DSSSL processing engine
- `@scaly/opensp` - SGML/XML parser

## CLI Usage

```bash
dazzle [OPTIONS] FILE...

Options:
  -d SPEC         DSSSL specification file
  -t TYPE         Output type: fot, rtf, tex, sgml, xml, mif (default: fot)
  -o FILE         Output filename
  -c FILE         Use catalog file
  -V VAR[=VAL]    Define variable
  -G              Debug mode
  -2              Enable DSSSL2 extensions
  -s              Strict mode
  -v, --version   Show version
  -h, --help      Show this help

Examples:
  dazzle -t sgml -d style.dsl doc.sgml
  dazzle -t xml -o output.xml -d style.dsl doc.xml
  dazzle -t rtf -d format.dsl document.sgml
```

## Related Repositories

| Repository | Package | Description |
|------------|---------|-------------|
| [openjade-js](https://github.com/rschleitzer/openjade-js) | `@scaly/openjade` | TypeScript port of OpenJade DSSSL engine |
| [openjade-js](https://github.com/rschleitzer/openjade-js) | `@scaly/opensp` | TypeScript port of OpenSP SGML parser |
| dazzle (this repo) | `dazzle` | CLI wrapper |

## Development

```bash
# Install dependencies
npm install

# Build
npm run build

# Test CLI
node dist/cli.js --help
```

## DSSSL Background

**DSSSL** (Document Style Semantics and Specification Language) is an ISO standard (ISO/IEC 10179:1996) for transforming SGML/XML documents using a Scheme-based stylesheet language.

**OpenJade** was the reference implementation (C++, ~72K lines), last updated ~2010. The `@scaly/openjade` package is a complete TypeScript port.

**References**:
- DSSSL Standard: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf
- OpenJade: https://openjade.sourceforge.net/

## Development Policies

### Git Commit Messages

**One-line messages only. No multi-line descriptions, no bullet points, no co-author tags, no marketing blurb.**

Format: `Verb + what changed`

Examples:
- `Add FOT output support`
- `Fix catalog path resolution`
- `Update dependencies`

---

**License**: MIT | **Status**: Active Development (Dec 2025)
