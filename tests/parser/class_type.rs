use super::*;

#[test]
fn class_type() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "class<Foo.Bar>",
        rule: Rule::class_type,
        tokens: [ class_type(0, 14, [ identifier(6, 13) ]) ]
    )
}

#[test]
fn class_type_empty_fails() {
    fails_with!(
        parser: UnrealScriptParser,
        input: "class<>",
        rule: Rule::class_type,
        positives: [Rule::identifier],
        negatives: [],
        pos: 6
    )
}
