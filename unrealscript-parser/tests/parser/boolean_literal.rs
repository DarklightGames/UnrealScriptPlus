use super::*;

#[test]
fn bool_literal_false_as_expression() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "false",
        rule: Rule::expression,
        tokens: [ expression(0, 5, [ literal (0, 5, [ boolean_literal(0, 5) ]) ]) ]
    )
}

#[test]
fn bool_literal_true_as_expression() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "true",
        rule: Rule::expression,
        tokens: [ expression(0, 4, [ literal (0, 4, [ boolean_literal(0, 4) ]) ]) ]
    )
}
