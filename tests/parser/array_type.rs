use super::*;

#[test]
fn array_int() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "array<int>",
        rule: Rule::array_type,
        tokens: [ array_type(0, 10, [ type_(6, 9, [ identifier(6, 9) ]) ]) ]
    )
}

#[test]
fn array_type_empty_fails() {
    fails_with!(
        parser: UnrealScriptParser,
        input: "array<>",
        rule: Rule::array_type,
        positives: [Rule::type_],
        negatives: [],
        pos: 6
    )
}

#[test]
fn array_type_nested() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "array<array<int>>",
        rule: Rule::array_type,
        tokens: [
            array_type(0, 17, [ type_(6, 16, [
                array_type(6, 16, [type_(12, 15, [ identifier(12, 15)])]),
            ])])
        ]
    }
}