pub mod ast;
pub mod fix;
pub mod lint;
pub mod parser;
pub mod source;
pub mod symbols;
pub mod visitor;

// Re-export main parsing functions and types for library consumers
pub use fix::{Applicability, Edit, Fix};
pub use lint::{Category, FixAvailability, Lint, LintSelection};
pub use parser::{
    ParseMode, ParseOptions, ParsingError, ProgramError, ProgramErrorSeverity, ProgramResult, Rule,
    SourceResult, check_file_name, parse_bytes, parse_expression, parse_program,
    parse_program_with, parse_with_rule,
};
pub use source::{Decoded, LineIndex, SourceEncoding, decode, decode_source, encode};
pub use symbols::{Reference, ReferenceKind, Symbol, SymbolKind, SymbolTable};
