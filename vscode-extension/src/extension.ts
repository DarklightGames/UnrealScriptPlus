import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    console.log('UnrealScript extension is now active');

    // Path to the bundled LSP server binary
    const serverPath = path.join(context.extensionPath, 'bin', 'unrealscript-lsp');

    // Check if the server binary exists
    const fs = require('fs');
    if (!fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(
            `UnrealScript LSP server not found at ${serverPath}. Please reinstall the extension.`
        );
        return;
    }

    // Server options - stdio transport
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'unrealscript' }
        ],
        synchronize: {
            // Notify the server about file changes to '.uc' files in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.uc')
        }
    };

    // Create and start the language client
    client = new LanguageClient(
        'unrealscriptLSP',
        'UnrealScript Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (this will also launch the server)
    client.start();

    console.log('UnrealScript LSP client started');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
