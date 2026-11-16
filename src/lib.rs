pub mod ast;
pub mod parser;
pub mod visitor;

// Re-export main parsing functions and types for library consumers
pub use parser::{
    ParsingError, ProgramResult, Rule, parse_expression, parse_program, parse_with_rule,
};
