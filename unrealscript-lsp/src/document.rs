use unrealscript_parser::parser::{ProgramError, ParsingError, ProgramErrorSeverity};
use unrealscript_parser::ast::AstSpan;

/// Represents the state of a document in the LSP
#[derive(Debug, Clone)]
pub struct DocumentState {
    /// The full text content of the document
    pub text: String,
    
    /// The version number of the document
    pub version: i32,
    
    /// Parse errors from the last parse attempt (including syntax errors)
    pub errors: Vec<ProgramError>,
}

impl DocumentState {
    pub fn new(text: String, version: i32) -> Self {
        let mut state = Self {
            text,
            version,
            errors: Vec::new(),
        };
        state.parse();
        state
    }
    
    /// Update the document with new text and parse it
    pub fn update(&mut self, text: String, version: i32) {
        self.text = text;
        self.version = version;
        self.parse();
    }
    
    /// Parse the current text content and update errors
    pub fn parse(&mut self) {
        match unrealscript_parser::parse_program(&self.text) {
            Ok(result) => {
                self.errors = result.errors;
            }
            Err(err) => {
                // Convert parsing error to a ProgramError
                self.errors = if let Some(program_error) = Self::parsing_error_to_program_error(&err, &self.text) {
                    vec![program_error]
                } else {
                    Vec::new()
                };
            }
        }
    }
    
    /// Convert a ParsingError to a ProgramError for diagnostic purposes
    fn parsing_error_to_program_error(error: &ParsingError, text: &str) -> Option<ProgramError> {
        match error {
            ParsingError::PestError(pest_error) => {
                // Extract error message
                let message = format!("{}", pest_error);
                
                // Extract line and column from pest error
                let span = match pest_error.line_col {
                    pest::error::LineColLocation::Pos((line, col)) => {
                        line_col_to_span(text, line, col)
                    }
                    pest::error::LineColLocation::Span((start_line, start_col), (end_line, end_col)) => {
                        let start = line_col_to_offset(text, start_line, start_col);
                        let end = line_col_to_offset(text, end_line, end_col);
                        AstSpan { start, end, line: None }
                    }
                };
                
                Some(ProgramError {
                    message,
                    span,
                    severity: ProgramErrorSeverity::Error,
                })
            }
            ParsingError::IoError(io_error) => Some(ProgramError {
                message: format!("I/O error: {}", io_error),
                span: AstSpan { start: 0, end: 0, line: None },
                severity: ProgramErrorSeverity::Error,
            }),
            ParsingError::EncodingError(enc_error) => Some(ProgramError {
                message: format!("Encoding error: {}", enc_error),
                span: AstSpan { start: 0, end: 0, line: None },
                severity: ProgramErrorSeverity::Error,
            }),
        }
    }
}

/// Convert 1-indexed line:column to byte offset
fn line_col_to_offset(text: &str, line: usize, col: usize) -> usize {
    let mut current_line = 1;
    let mut current_col = 1;
    
    for (idx, ch) in text.char_indices() {
        if current_line == line && current_col == col {
            return idx;
        }
        
        if ch == '\n' {
            current_line += 1;
            current_col = 1;
        } else {
            current_col += 1;
        }
    }
    
    // If we didn't find the exact position, return end of text
    text.len()
}

/// Convert 1-indexed line:column to byte offset span
fn line_col_to_span(text: &str, line: usize, col: usize) -> AstSpan {
    let start = line_col_to_offset(text, line, col);
    // Span one character
    let end = text[start..].chars().next().map_or(start + 1, |ch| start + ch.len_utf8());
    AstSpan { start, end, line: None }
}
