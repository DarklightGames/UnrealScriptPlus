// Re-export test utilities and parser for all test modules
pub use pest::{parses_to, consumes_to, fails_with};
pub use unrealscriptplus::parser::{UnrealScriptParser, Rule};

// Comment tests
mod comments;

// Identifier tests
mod identifiers;

// Literal tests
mod boolean_literal;
mod float_literal;
mod integer_literal;
mod literal;
mod name_literal;
mod numeric_literal;
mod object_literal;
mod rotator_literal;
mod string_literal;
mod vector_literal;

// Type tests
mod array_type;
mod class_type;

// Declaration tests
mod class_declaration;
mod const_declaration;
mod enum_declaration;
mod function_declaration;
mod struct_declaration;
mod var_declaration;

// Replication tests
mod replication;

// Statement tests
mod statements;

// Compiler directive tests
mod compiler_directive;
