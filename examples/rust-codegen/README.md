# Rust Code Generator Example

This example demonstrates using Dazzle to generate Rust struct definitions from an XML data model.

## Files

- `model.xml` - XML data model defining structs and fields
- `codegen.scm` - Scheme template for generating Rust code

## Usage

```bash
cd examples/rust-codegen
dazzle -d codegen.scm model.xml > output.rs
```

## Output

Generates Rust struct definitions with:
- `#[derive(Debug, Clone, PartialEq)]` attributes
- Public fields with proper types
- `Option<T>` for optional fields

## Example

**Input (model.xml):**
```xml
<model name="UserAPI">
  <struct name="User">
    <field name="id" type="u64" required="true"/>
    <field name="username" type="String" required="true"/>
    <field name="created_at" type="DateTime" required="false"/>
  </struct>
</model>
```

**Output:**
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct User {
    pub id: u64,
    pub username: String,
    pub created_at: Option<DateTime>,
}
```

## Template Features Demonstrated

- Grove navigation (`current-node`, `children`)
- Attribute extraction (`attribute-string`)
- Conditional logic (`if`, `equal?`)
- Recursion over node-lists
- String building (`string-append`)
- XML-to-code transformation
