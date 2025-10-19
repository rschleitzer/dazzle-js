# Publishing Dazzle v0.1.0 to crates.io

## Prerequisites

1. **Get crates.io API token**:
   - Visit https://crates.io/me
   - Generate a new API token
   - Copy the token

2. **Login to crates.io**:
   ```bash
   cargo login
   # Paste your API token when prompted
   ```

## Publication Order

Publish in this exact order (dependencies first):

### 1. Publish dazzle-core
```bash
cd crates/dazzle-core
cargo publish
cd ../..
```

### 2. Publish dazzle-grove-libxml2
```bash
cd crates/dazzle-grove-libxml2
cargo publish
cd ../..
```

### 3. Publish dazzle-backend-sgml
```bash
cd crates/dazzle-backend-sgml
cargo publish
cd ../..
```

### 4. Publish dazzle-cli
```bash
cd crates/dazzle-cli
cargo publish
cd ../..
```

## After Publication

### Verify Installation
```bash
cargo install dazzle-cli
dazzle --version
```

### Test from crates.io
```bash
cd /tmp
cargo new test-dazzle
cd test-dazzle
cargo add dazzle-core
cargo build
```

## Update Documentation

After successful publication, update:

1. **README.md** - Add crates.io installation instructions:
   ```markdown
   ### From crates.io
   ```bash
   cargo install dazzle-cli
   ```
   ```

2. **Announce** on:
   - GitHub Discussions
   - Reddit r/rust
   - This Week in Rust (submit PR)

## Badges

Add to README.md after publication:

```markdown
[![Crates.io](https://img.shields.io/crates/v/dazzle-cli)](https://crates.io/crates/dazzle-cli)
[![Downloads](https://img.shields.io/crates/d/dazzle-cli)](https://crates.io/crates/dazzle-cli)
[![License](https://img.shields.io/crates/l/dazzle-cli)](LICENSE)
```

## Troubleshooting

### "crate already exists"
- Version already published, bump version in Cargo.toml
- Versions are immutable on crates.io

### "missing dependencies"
- Ensure previous crates in chain are published first
- Wait 1-2 minutes after publishing dependencies

### "verification failed"
- Run `cargo publish --dry-run` to test
- Check for missing files in `Cargo.toml` [package.include]
