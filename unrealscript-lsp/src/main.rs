mod backend;
mod document;
mod lru_cache;
mod utils;
mod token_collector;

use tower_lsp::{LspService, Server};
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

use backend::Backend;

#[tokio::main]
async fn main() {
    // Initialize tracing with verbose logging
    tracing_subscriber::registry()
        .with(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("trace")),
        )
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    info!("Starting UnrealScript LSP server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    
    info!("LSP server ready, listening on stdio");
    Server::new(stdin, stdout, socket).serve(service).await;
    
    info!("LSP server stopped");
}
