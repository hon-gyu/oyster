# Project Structure

## Directory Organization

```
markdown-tools/
├── src/                    # Core library source code
│   ├── lib.rs             # Public API and module exports
│   ├── ast.rs             # Abstract syntax tree types and operations
│   ├── parse.rs           # Markdown parsing and AST construction
│   └── validate.rs        # Validation rules and metadata checking
├── docs/                  # Project documentation and examples
├── target/                # Cargo build artifacts (auto-generated)
├── .spec-workflow/        # Specification workflow documents
│   ├── steering/          # Project-wide guidance documents
│   ├── specs/            # Individual feature specifications
│   └── templates/        # Document templates (auto-generated)
├── Cargo.toml            # Package manifest and dependencies
├── Cargo.lock            # Dependency lock file
├── rustfmt.toml          # Code formatting configuration
└── .gitignore            # Version control exclusions
```

## Development Workflow

### Build Commands
- `cargo build` - Standard compilation
- `cargo test` - Run all tests including snapshot tests
- `cargo fmt` - Format code using rustfmt.toml configuration
- `cargo clippy` - Lint checking with Rust-specific advice
- `cargo doc` - Generate documentation
