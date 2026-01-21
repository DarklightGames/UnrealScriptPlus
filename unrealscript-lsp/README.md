# UnrealScript Language Server

A Language Server Protocol (LSP) implementation for UnrealScript, providing IDE features for `.uc` files.

## Features

- **Diagnostics**: Real-time parse errors and semantic warnings
- **Document Symbols**: Hierarchical outline view showing:
  - Classes with parent class information
  - Functions with full signatures (return type and parameters)
  - Variables with type information
  - Structs with member counts and nested fields
  - Enums with value counts
  - Constants with values
  - States

## Installation

Run the installation script from the project root:

```bash
./install.sh
```

This will:
1. Build the LSP server in release mode
2. Install the binary to `~/.local/bin/unrealscript-lsp`
3. Make it executable
4. Display VS Code configuration instructions

## VS Code Integration

After installation, configure the LSP in your VS Code settings. You have two options:

### Option 1: Using a VS Code Extension

Create or use a VS Code extension that registers the UnrealScript language and LSP.

### Option 2: Manual Configuration

Add to your `.vscode/settings.json`:

```json
{
  "unrealscript.lsp.serverPath": "~/.local/bin/unrealscript-lsp"
}
```

## Development

### Building

```bash
# Build debug version
cargo build --bin unrealscript-lsp

# Build release version
cargo build --release --bin unrealscript-lsp
```

### Testing

Run the parser tests:

```bash
cargo test --package unrealscript_parser
```

### Logging

The LSP server uses verbose logging by default (TRACE level). Logs are written to stderr.

To adjust the log level, set the `RUST_LOG` environment variable:

```bash
RUST_LOG=debug unrealscript-lsp
```

## Architecture

The LSP is built on `tower-lsp` and consists of:

- `main.rs` - Entry point with tokio runtime and stdio transport
- `backend.rs` - LSP protocol implementation (LanguageServer trait)
- `document.rs` - Document state management
- `utils.rs` - Helper functions (span to LSP range conversion)

The LSP re-parses documents on-demand for symbol extraction to avoid thread-safety issues with the AST's `Rc<>` types.

## Workspace Structure

```
unrealscript-lsp/       # LSP server crate
├── src/
│   ├── main.rs         # Entry point
│   ├── backend.rs      # LSP implementation
│   ├── document.rs     # Document tracking
│   └── utils.rs        # Utilities
└── Cargo.toml

unrealscript-parser/    # Parser library
└── ...
```

## License

MIT
