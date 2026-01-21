use super::*;

#[test]
fn numeric_literal_integer() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "1234",
        rule: Rule::numeric_literal,
        tokens: [ numeric_literal(0, 4, [ integer_literal(0, 4, [ integer_literal_decimal(0, 4) ]) ]) ]
    )
}

#[test]
fn numeric_literal_float() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "1234.5678",
        rule: Rule::numeric_literal,
        tokens: [ numeric_literal(0, 9, [ float_literal(0, 9) ]) ]
    )
}
