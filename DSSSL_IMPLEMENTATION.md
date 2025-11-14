# DSSSL Processing Implementation - Complete

## Summary
Full DSSSL processing system ported from OpenJade, enabling template-driven code generation from XML.

## Implemented Components

### Core (packages/dazzle-core/src/)

**Sosofo System** (`scheme/elobj.ts`):
- SosofoObj: empty, literal, formatting-instruction, entity, append
- Full sosofo tree traversal and backend dispatch

**DSSSL Primitives** (`scheme/primitives.ts`):
- `literal` - Text content (0+ strings concatenated)
- `empty-sosofo` - Empty sosofo
- `sosofo-append` - Combine sosofos
- `current-node` - Current processing node
- `process-children`, `process-node-list` - Traversal stubs
- Fixed `gi` to return string (not symbol)

**ProcessContext** (`dsssl/process-context.ts`):
- Sosofo execution engine
- Backend dispatch: literal→characters(), entity→startEntity()/endEntity()

**Compiler** (`scheme/compiler.ts`):
- `make` special form - Flow object construction
- Keyword support (system-id:, data:)

**Parser** (`scheme/parser.ts`):
- DSSSL keyword syntax (trailing colon)

**VM** (`scheme/vm.ts`):
- Added grove, currentNode fields for DSSSL context

### CLI (packages/dazzle-cli/)
- Integrated ProcessContext with SGML backend
- Auto-processes sosofos returned from template evaluation

### Backend (packages/dazzle-backend-sgml/)
- No changes needed - interface already complete

## Test Results
✅ File generation with literal content
✅ Grove queries (gi, current-node)
✅ Multiple files via sosofo-append
✅ String concatenation with literals
✅ Code generation (Java class from XML)

## Example
```scheme
(make entity
  system-id: "MyClass.java"
  (literal "public class ")
  (literal (gi (current-node)))
  (literal " {}\n"))
```

Output: `MyClass.java` containing Java class with XML element name.

## Rule-Based Processing (Complete)

**Construction Rules** (`dsssl/rules.ts`):
- `RuleRegistry`: Stores element and root rules indexed by name+mode
- `element` special form: Registers element construction rules
- `root` special form: Registers document root rule
- Rules compile to closures executed during traversal

**Processing Primitives**:
- `process-root`: Entry point - finds root rule and executes
- `process-children`: Traverses child nodes, matches rules, returns combined sosofo
- Automatic tree traversal with rule matching

**Example**:
```scheme
(root
  (make entity system-id: "output.txt"
    (literal "Header\n")
    (process-children)
    (literal "Footer\n")))

(element chapter
  (sosofo-append
    (literal "Chapter: ")
    (literal (gi (current-node)))
    (literal "\n")))

(process-root)
```

Result: Automatic traversal, rule execution, sosofo collection and backend processing.

## Multi-Pass Processing with Modes (Complete)

**Mode Support** (`compiler.ts:834-913`):
- `mode` special form: `(mode name (element gi body...)...)`
- Registers rules with specific mode names
- Multiple rules for same element in different modes

**Mode Switching** (`primitives.ts:7133-7159`):
- `process-children` with optional mode argument: `(process-children 'mode-name)`
- Saves/restores processing mode during traversal
- Enables multi-pass document processing

**Example**:
```scheme
(root
  (make entity system-id: "output.txt"
    (literal "First pass:\n")
    (process-children)           ;; Default mode
    (literal "\nSecond pass:\n")
    (process-children 'toc)))    ;; TOC mode

(element chapter
  (literal "Chapter content\n"))

(mode toc
  (element chapter
    (literal "TOC entry\n")))

(process-root)
```

Result: Document processed twice with different rules for each pass.

**next-match Primitive** (`primitives.ts:7178-7192`):
- Stub implementation (returns empty-sosofo)
- Placeholder for rule priority chains
- Full implementation requires priority system

## Status
**Phase Complete**: Multi-pass processing with modes ✓
**Next**: XML template wrapper support (.dsl format)
