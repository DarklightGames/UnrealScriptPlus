#!/bin/bash
set -e

echo "Bundling UnrealScript LSP server binary..."

# Ensure we're in the right directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
EXTENSION_DIR="$PROJECT_ROOT/vscode-extension"
BIN_DIR="$EXTENSION_DIR/bin"

# Create bin directory if it doesn't exist
mkdir -p "$BIN_DIR"

# Copy the LSP binary
LSP_BINARY="$PROJECT_ROOT/target/release/unrealscript-lsp"

if [ ! -f "$LSP_BINARY" ]; then
    echo "Error: LSP binary not found at $LSP_BINARY"
    echo "Please run 'cargo build --release --bin unrealscript-lsp' first"
    exit 1
fi

echo "Copying $LSP_BINARY to $BIN_DIR/"
cp "$LSP_BINARY" "$BIN_DIR/unrealscript-lsp"

# Make it executable
chmod +x "$BIN_DIR/unrealscript-lsp"

echo "✓ LSP binary bundled successfully"
