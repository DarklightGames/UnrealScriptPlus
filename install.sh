#!/bin/bash
set -e

echo "========================================="
echo "UnrealScript LSP Installation"
echo "========================================="
echo ""

# Step 1: Build the LSP server
echo "Step 1: Building UnrealScript LSP server..."
cargo build --release --bin unrealscript-lsp

# Determine installation directory
INSTALL_DIR="${HOME}/.local/bin"

# Create directory if it doesn't exist
mkdir -p "${INSTALL_DIR}"

# Copy binary
echo "Installing LSP server to ${INSTALL_DIR}..."
cp target/release/unrealscript-lsp "${INSTALL_DIR}/"

# Make executable
chmod +x "${INSTALL_DIR}/unrealscript-lsp"

echo "✓ LSP server installed to ${INSTALL_DIR}/unrealscript-lsp"
echo ""

# Step 2: Build VS Code extension
echo "Step 2: Building VS Code extension..."

cd vscode-extension

# Check if npm is available
if command -v npm &> /dev/null; then
    echo "Installing npm dependencies..."
    npm install
    
    echo "Compiling TypeScript..."
    npm run compile
    
    echo "Bundling LSP binary with extension..."
    ./scripts/bundle-lsp.sh
    
    echo "Packaging extension..."
    npm run package
    
    echo "✓ Extension packaged successfully"
    echo ""
    
    # Try to install the extension
    VSIX_FILE=$(ls unrealscript-*.vsix 2>/dev/null | head -n 1)
    
    if [ -n "$VSIX_FILE" ]; then
        if command -v code &> /dev/null; then
            echo "Installing extension to VS Code..."
            code --install-extension "$VSIX_FILE"
            echo "✓ Extension installed successfully"
        else
            echo "VS Code 'code' command not found."
            echo "To install the extension manually:"
            echo "  1. Open VS Code"
            echo "  2. Press Ctrl+Shift+P (Cmd+Shift+P on macOS)"
            echo "  3. Type 'Extensions: Install from VSIX'"
            echo "  4. Select: $(pwd)/$VSIX_FILE"
        fi
    fi
else
    echo "npm not found. To build the VS Code extension manually:"
    echo "  cd vscode-extension"
    echo "  npm install"
    echo "  npm run compile"
    echo "  ./scripts/bundle-lsp.sh"
    echo "  npm run package"
    echo "  code --install-extension unrealscript-*.vsix"
fi

cd ..

echo ""
echo "========================================="
echo "Installation Complete!"
echo "========================================="
echo ""
echo "LSP server binary: ${INSTALL_DIR}/unrealscript-lsp"
echo ""
echo "If you installed the VS Code extension, restart VS Code to activate it."
echo "The extension will automatically use the bundled LSP server for .uc files."
echo ""
echo "For manual testing of the LSP server:"
echo "  ${INSTALL_DIR}/unrealscript-lsp"
echo ""

