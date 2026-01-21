use super::*;

#[test]
fn string_literal_quick_brown_fox() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "\"The quick brown foxed jumped over the lazy dog.\"",
        rule: Rule::string_literal,
        tokens: [ string_literal(0, 49, [ string_literal_inner(1, 48) ]) ]
    )
}

#[test]
fn string_literal_empty() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "\"\"",
        rule: Rule::string_literal,
        tokens: [ string_literal(0, 2, [ string_literal_inner(1, 1) ]) ]
    )
}
