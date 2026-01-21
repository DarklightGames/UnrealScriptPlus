#!/bin/bash
set -e

echo "Building UnrealScript LSP server..."
cargo build --release --bin unrealscript-lsp

# Determine installation directory
INSTALL_DIR="${HOME}/.local/bin"

# Create directory if it doesn't exist
mkdir -p "${INSTALL_DIR}"

# Copy binary
echo "Installing to ${INSTALL_DIR}..."
cp target/release/unrealscript-lsp "${INSTALL_DIR}/"

# Make executable
chmod +x "${INSTALL_DIR}/unrealscript-lsp"

echo ""
echo "✓ UnrealScript LSP server installed successfully!"
echo ""
echo "Binary location: ${INSTALL_DIR}/unrealscript-lsp"
echo ""
echo "To use with VS Code, add the following to your settings.json:"
echo ""
echo "{"
echo "  \"unrealscript.lsp.enabled\": true,"
echo "  \"unrealscript.lsp.serverPath\": \"${INSTALL_DIR}/unrealscript-lsp\""
echo "}"
echo ""
echo "Or configure it as a custom language server:"
echo ""
echo "{"
echo "  \"languageServerExample.trace.server\": \"verbose\","
echo "  \"[unrealscript]\": {"
echo "    \"editor.semanticHighlighting.enabled\": true"
echo "  }"
echo "}"
echo ""
echo "Add to your .vscode/settings.json or create a VS Code extension"
echo "that registers the language server for .uc files."
echo ""
echo "For manual testing, you can run:"
echo "  ${INSTALL_DIR}/unrealscript-lsp"
echo ""
