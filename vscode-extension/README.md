# UnrealScript Language Extension

VS Code extension providing language support for UnrealScript (`.uc` files) with LSP-powered features.

## Features

- **Syntax Highlighting**: Basic TextMate grammar for UnrealScript syntax
- **Language Server Protocol (LSP)**: 
  - Real-time diagnostics (parse errors and semantic warnings)
  - Document symbols (outline view with classes, functions, variables, structs, enums)
  - Hierarchical symbol navigation

## Installation

### From VSIX

1. Build the extension using the root `install.sh` script
2. Install the generated `.vsix` file:
   - Open VS Code
   - Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
   - Type "Extensions: Install from VSIX"
   - Select the `unrealscript-*.vsix` file

### Manual Build

```bash
# From the vscode-extension directory
npm install
npm run compile
./scripts/bundle-lsp.sh
npm run package
code --install-extension unrealscript-*.vsix
```

## Requirements

- VS Code 1.75.0 or higher
- The bundled UnrealScript LSP server (included in the extension)

## Extension Settings

Currently, this extension has no configurable settings. The LSP server uses the bundled binary automatically.

## Known Issues

- Syntax highlighting is basic and covers only common keywords
- Cross-platform support is limited to the build platform

## Development

### Building from Source

```bash
# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Watch mode for development
npm run watch

# Bundle the LSP binary
./scripts/bundle-lsp.sh

# Package the extension
npm run package
```

### File Structure

```
vscode-extension/
├── src/
│   └── extension.ts          # Extension activation logic
├── syntaxes/
│   └── unrealscript.tmLanguage.json  # TextMate grammar
├── scripts/
│   └── bundle-lsp.sh         # Script to bundle LSP binary
├── bin/
│   └── unrealscript-lsp      # Bundled LSP server (created during build)
├── language-configuration.json  # Brackets, comments, etc.
├── package.json
└── tsconfig.json
```

## TODO

- [ ] Add extension icon
- [ ] Improve TextMate grammar with more comprehensive syntax coverage
- [ ] Add configuration options for LSP verbosity
- [ ] Cross-platform binary support

## License

MIT

---

**Note**: This extension bundles the UnrealScript LSP server. The server is automatically started when you open a `.uc` file.
