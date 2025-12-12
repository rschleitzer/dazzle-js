# Dazzle

CLI wrapper for [`@scaly/openjade`](https://www.npmjs.com/package/@scaly/openjade) - a TypeScript port of the OpenJade DSSSL processor.

## Installation

```bash
npm install -g dazzle
```

## Usage

```bash
dazzle [OPTIONS] FILE...
```

### Options

| Option | Description |
|--------|-------------|
| `-d SPEC` | DSSSL specification file |
| `-t TYPE` | Output type: `fot`, `rtf`, `tex`, `sgml`, `xml`, `mif` (default: `fot`) |
| `-o FILE` | Output filename |
| `-c FILE` | Use catalog file |
| `-V VAR[=VAL]` | Define variable |
| `-G` | Debug mode |
| `-2` | Enable DSSSL2 extensions |
| `-s` | Strict mode |
| `-v, --version` | Show version |
| `-h, --help` | Show help |

### Examples

```bash
# Transform SGML document to XML
dazzle -t xml -d style.dsl document.sgml

# Code generation using SGML backend
dazzle -t sgml -d codegen.dsl model.xml

# Generate RTF document
dazzle -t rtf -o output.rtf -d format.dsl document.sgml

# With variables
dazzle -t sgml -V package=com.example -d gen.dsl input.xml
```

## What is DSSSL?

**DSSSL** (Document Style Semantics and Specification Language) is an ISO standard (ISO/IEC 10179:1996) for transforming SGML/XML documents using a Scheme-based stylesheet language.

Key features:
- **Scheme-based**: Uses R4RS Scheme for stylesheet logic
- **Flow objects**: Declarative document structure
- **Grove queries**: Powerful node selection and navigation
- **Multiple backends**: RTF, TeX, SGML, XML, MIF output

## Related Packages

| Package | Description |
|---------|-------------|
| [`@scaly/openjade`](https://www.npmjs.com/package/@scaly/openjade) | TypeScript port of OpenJade DSSSL engine |
| [`@scaly/opensp`](https://www.npmjs.com/package/@scaly/opensp) | TypeScript port of OpenSP SGML parser |

## References

- [DSSSL Standard (ISO/IEC 10179:1996)](ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf)
- [OpenJade](https://openjade.sourceforge.net/)
- [R4RS Scheme](http://www.schemers.org/Documents/Standards/R5RS/)

## License

MIT
