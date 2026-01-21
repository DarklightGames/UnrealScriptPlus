# VS Code Extension Setup

This document explains how to build and install the UnrealScript VS Code extension.

## Prerequisites

- Node.js and npm installed
- VS Code installed
- The UnrealScript LSP server built (from the root project)

## Quick Start

From the project root, run:

```bash
./install.sh
```

This script will:
1. Build the UnrealScript LSP server
2. Install it to `~/.local/bin/`
3. Build and package the VS Code extension (if npm is available)
4. Install the extension to VS Code (if the `code` command is available)

## Manual Installation (if npm is not detected)

If the automated installation doesn't work, follow these steps:

### 1. Install Dependencies

```bash
cd vscode-extension
npm install
```

### 2. Compile TypeScript

```bash
npm run compile
```

### 3. Bundle the LSP Binary

```bash
./scripts/bundle-lsp.sh
```

This copies the LSP server from `../target/release/unrealscript-lsp` to `bin/unrealscript-lsp`.

### 4. Package the Extension

```bash
npm run package
```

This creates a `.vsix` file (e.g., `unrealscript-0.1.0.vsix`).

### 5. Install in VS Code

Option A: Using VS Code UI
1. Open VS Code
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
3. Type "Extensions: Install from VSIX"
4. Select the generated `.vsix` file

Option B: Using command line
```bash
code --install-extension unrealscript-0.1.0.vsix
```

### 6. Restart VS Code

After installation, restart VS Code to activate the extension.

## Verification

1. Open a `.uc` file in VS Code
2. The extension should activate automatically
3. Check the Output panel (View > Output) and select "UnrealScript Language Server" to see LSP logs
4. Open the outline view (View > Outline or Ctrl+Shift+O) to see document symbols

## Development

For development with live reloading:

```bash
cd vscode-extension
npm run watch
```

Then press `F5` in VS Code to launch the Extension Development Host.

## Troubleshooting

### "LSP server not found" error

The extension looks for the bundled binary at `bin/unrealscript-lsp`. Make sure you ran `./scripts/bundle-lsp.sh` before packaging.

### Extension doesn't activate

Check that:
1. The file has a `.uc` or `.uci` extension
2. The extension is installed (check Extensions view)
3. No errors appear in the Output panel

### No syntax highlighting

The TextMate grammar is registered for the `unrealscript` language ID. Verify the file is recognized as UnrealScript in the bottom-right corner of VS Code.

## File Structure

```
vscode-extension/
├── bin/                      # LSP server binary (generated)
│   └── unrealscript-lsp
├── out/                      # Compiled JavaScript (generated)
│   └── extension.js
├── scripts/
│   └── bundle-lsp.sh         # Script to copy LSP binary
├── src/
│   └── extension.ts          # Extension entry point
├── syntaxes/
│   └── unrealscript.tmLanguage.json  # Syntax highlighting
├── .gitignore
├── .vscodeignore            # Files to exclude from .vsix
├── language-configuration.json  # Language config (brackets, etc.)
├── package.json              # Extension manifest
├── README.md                 # User-facing documentation
└── tsconfig.json             # TypeScript configuration
```

## Next Steps

See the main README.md for planned improvements including:
- Extension icon
- Enhanced syntax highlighting
- Configuration options
- Cross-platform support
