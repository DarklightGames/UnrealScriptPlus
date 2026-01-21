use unrealscript_parser::parser::ProgramError;

/// Represents the state of a document in the LSP
#[derive(Debug, Clone)]
pub struct DocumentState {
    /// The full text content of the document
    pub text: String,
    
    /// The version number of the document
    pub version: i32,
    
    /// Parse errors from the last parse attempt
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
            Err(_) => {
                // On syntax error, clear semantic errors
                // TODO: convert ParsingError to diagnostic
                self.errors.clear();
            }
        }
    }
}
