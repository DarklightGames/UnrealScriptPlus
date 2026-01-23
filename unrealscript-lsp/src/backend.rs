use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{debug, info, warn};

use crate::document::DocumentState;
use crate::utils::span_to_range;
use crate::token_collector::{TokenCollector, LEGEND_TYPE};
use unrealscript_parser::ast::*;
use unrealscript_parser::parser::{ProgramError, ProgramErrorSeverity};

pub struct Backend {
    client: Client,
    documents: DashMap<Url, DocumentState>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    /// Convert parser errors to LSP diagnostics
    fn errors_to_diagnostics(&self, text: &str, errors: &[ProgramError]) -> Vec<Diagnostic> {
        errors
            .iter()
            .map(|error| {
                let range = span_to_range(text, &error.span);
                let severity = match error.severity {
                    ProgramErrorSeverity::Error => DiagnosticSeverity::ERROR,
                    ProgramErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                };

                Diagnostic {
                    range,
                    severity: Some(severity),
                    code: None,
                    code_description: None,
                    source: Some("unrealscript".to_string()),
                    message: error.message.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
            .collect()
    }

    /// Publish diagnostics for a document
    async fn publish_diagnostics_for_doc(&self, uri: Url, doc: &DocumentState) {
        let diagnostics = self.errors_to_diagnostics(&doc.text, &doc.errors);
        
        debug!(
            "Publishing {} diagnostics for {}",
            diagnostics.len(),
            uri
        );
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    /// Extract document symbols from the AST
    fn extract_symbols(&self, text: &str, program: &Program) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();

        for statement in &program.statements {
            match &statement.data {
                ProgramStatement::ClassDeclaration(class) => {
                    symbols.push(self.class_to_symbol(text, class));
                }
                ProgramStatement::ConstDeclaration(const_decl) => {
                    symbols.push(self.const_to_symbol(text, const_decl));
                }
                ProgramStatement::VarDeclaration(var) => {
                    symbols.push(self.var_to_symbol(text, var));
                }
                ProgramStatement::EnumDeclaration(enum_decl) => {
                    symbols.push(self.enum_to_symbol(text, enum_decl));
                }
                ProgramStatement::StructDeclaration(struct_decl) => {
                    symbols.push(self.struct_to_symbol(text, struct_decl));
                }
                ProgramStatement::FunctionDeclaration(func) => {
                    symbols.push(self.function_to_symbol(text, func));
                }
                ProgramStatement::StateDeclaration(state) => {
                    symbols.push(self.state_to_symbol(text, state));
                }
                _ => {}
            }
        }

        symbols
    }

    fn class_to_symbol(&self, text: &str, class: &AstNode<ClassDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &class.span);
        let mut detail = String::new();

        if let Some(parent) = &class.data.parent_class {
            detail.push_str(&format!("extends {}", parent.data.string));
        }

        #[allow(deprecated)]
        DocumentSymbol {
            name: class.data.name.data.string.clone(),
            detail: if detail.is_empty() {
                None
            } else {
                Some(detail)
            },
            kind: SymbolKind::CLASS,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn const_to_symbol(&self, text: &str, const_decl: &AstNode<ConstDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &const_decl.span);
        let detail = self.literal_to_string(&const_decl.data.value);

        #[allow(deprecated)]
        DocumentSymbol {
            name: const_decl.data.name.data.string.clone(),
            detail: Some(detail),
            kind: SymbolKind::CONSTANT,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn var_to_symbol(&self, text: &str, var: &AstNode<VarDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &var.span);
        let type_str = self.type_to_string(&var.data.type_);
        let names: Vec<String> = var
            .data
            .names
            .iter()
            .map(|n| n.data.identifier.data.string.clone())
            .collect();

        #[allow(deprecated)]
        DocumentSymbol {
            name: names.join(", "),
            detail: Some(type_str),
            kind: SymbolKind::VARIABLE,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn enum_to_symbol(&self, text: &str, enum_decl: &AstNode<EnumDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &enum_decl.span);
        let detail = format!("{} values", enum_decl.data.values.len());

        #[allow(deprecated)]
        DocumentSymbol {
            name: enum_decl.data.name.data.string.clone(),
            detail: Some(detail),
            kind: SymbolKind::ENUM,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn struct_to_symbol(&self, text: &str, struct_decl: &AstNode<StructDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &struct_decl.span);
        let detail = format!("{} members", struct_decl.data.members.len());

        // Create children for struct members
        let children: Vec<DocumentSymbol> = struct_decl
            .data
            .members
            .iter()
            .map(|member| {
                let member_range = span_to_range(text, &member.span);
                let type_str = self.type_to_string(&member.data.type_);
                let names: Vec<String> = member
                    .data
                    .names
                    .iter()
                    .map(|n| n.data.identifier.data.string.clone())
                    .collect();

                #[allow(deprecated)]
                DocumentSymbol {
                    name: names.join(", "),
                    detail: Some(type_str),
                    kind: SymbolKind::FIELD,
                    tags: None,
                    deprecated: None,
                    range: member_range,
                    selection_range: member_range,
                    children: None,
                }
            })
            .collect();

        #[allow(deprecated)]
        DocumentSymbol {
            name: struct_decl.data.name.data.string.clone(),
            detail: Some(detail),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        }
    }

    fn function_to_symbol(&self, text: &str, func: &AstNode<FunctionDeclaration>) -> DocumentSymbol {
        let range = span_to_range(text, &func.span);

        // Build function signature
        let mut signature = String::new();
        if let Some(return_type) = &func.data.return_type {
            signature.push_str(&self.type_to_string(return_type));
            signature.push(' ');
        }
        
        signature.push('(');
        let params: Vec<String> = func
            .data
            .arguments
            .iter()
            .map(|arg| {
                format!(
                    "{} {}",
                    self.type_to_string(&arg.data.type_),
                    arg.data.name.data.identifier.data.string
                )
            })
            .collect();
        signature.push_str(&params.join(", "));
        signature.push(')');

        #[allow(deprecated)]
        DocumentSymbol {
            name: func.data.name.data.string.clone(),
            detail: Some(signature),
            kind: SymbolKind::FUNCTION,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn state_to_symbol(&self, text: &str, state: &StateDeclaration) -> DocumentSymbol {
        let range = span_to_range(text, &state.name.span);

        #[allow(deprecated)]
        DocumentSymbol {
            name: state.name.data.string.clone(),
            detail: Some("state".to_string()),
            kind: SymbolKind::CLASS,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }

    fn type_to_string(&self, type_: &Type) -> String {
        match type_ {
            Type::Class(id) => id.data.string.clone(),
            Type::Array(inner) => format!("array<{}>", self.type_to_string(inner)),
            Type::Struct(_) => "struct".to_string(),
            Type::Enum(_) => "enum".to_string(),
            Type::Pod(pod) => match pod {
                PodType::Bool => "bool".to_string(),
                PodType::Byte => "byte".to_string(),
                PodType::Int => "int".to_string(),
                PodType::Float => "float".to_string(),
                PodType::String => "string".to_string(),
                PodType::Name => "name".to_string(),
            },
            Type::Identifier(id) => id.string.clone(),
        }
    }

    fn literal_to_string(&self, literal: &AstNode<Literal>) -> String {
        match &literal.data {
            Literal::Boolean(b) => b.to_string(),
            Literal::Numeric(n) => match &n.data {
                NumericLiteral::Integer(i) => i.to_string(),
                NumericLiteral::Float(f) => f.to_string(),
            },
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Name(n) => format!("'{}'", n),
            Literal::None => "None".to_string(),
            _ => "...".to_string(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        info!("Initializing UnrealScript LSP server");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_symbol_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: TextDocumentRegistrationOptions {
                                document_selector: Some(vec![DocumentFilter {
                                    language: Some("unrealscript".to_string()),
                                    scheme: Some("file".to_string()),
                                    pattern: None,
                                }]),
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![SemanticTokenModifier::DECLARATION],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "unrealscript-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("UnrealScript LSP server initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down UnrealScript LSP server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        debug!("Document opened: {}", uri);

        let doc = DocumentState::new(text.clone(), version);
        
        // Publish diagnostics
        self.publish_diagnostics_for_doc(uri.clone(), &doc)
            .await;

        self.documents.insert(uri, doc);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        debug!("Document changed: {} (version {})", uri, version);

        if let Some(change) = params.content_changes.first() {
            let text = change.text.clone();

            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.update(text.clone(), version);

                // Publish diagnostics
                self.publish_diagnostics_for_doc(uri.clone(), &doc)
                    .await;
            } else {
                warn!("Document not found in cache: {}", uri);
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!("Document closed: {}", uri);
        self.documents.remove(&uri);
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        debug!("Document symbols requested for: {}", uri);

        if let Some(doc) = self.documents.get(&uri) {
            // Re-parse to get AST for symbols (parse is fast enough for this)
            match unrealscript_parser::parse_program(&doc.text) {
                Ok(result) => {
                    let symbols = self.extract_symbols(&doc.text, &result.program);
                    debug!("Returning {} symbols for {}", symbols.len(), uri);
                    return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
                }
                Err(e) => {
                    debug!("Failed to parse for symbols: {:?}", e);
                }
            }
        } else {
            warn!("Document not found: {}", uri);
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        debug!("Semantic tokens requested for: {}", uri);

        if let Some(doc) = self.documents.get(&uri) {
            match unrealscript_parser::parse_program(&doc.text) {
                Ok(result) => {
                    let collector = TokenCollector::new(&doc.text);
                    let tokens = collector.collect(&result.program);
                    debug!("Returning {} semantic tokens for {}", tokens.len(), uri);
                    return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                        result_id: None,
                        data: tokens,
                    })));
                }
                Err(e) => {
                    debug!("Failed to parse for semantic tokens: {:?}", e);
                }
            }
        } else {
            warn!("Document not found: {}", uri);
        }

        Ok(None)
    }
}
