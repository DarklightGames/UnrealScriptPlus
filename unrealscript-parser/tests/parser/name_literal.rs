use super::*;

#[test]
fn name_literal_all_characters() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "'0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'",
        rule: Rule::name_literal,
        tokens: [ name_literal(0, 65, [ name_inner(1, 64) ])]
    )
}

#[test]
fn name_literal_empty() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "''",
        rule: Rule::name_literal,
        tokens: [ name_literal(0, 2, [ name_inner(1, 1) ]) ]
    )
}

#[test]
fn name_literal_bad_character() {
    fails_with! (
        parser: UnrealScriptParser,
        input: "'abcdef^asd'",
        rule: Rule::name_literal,
        positives: [Rule::name_literal],
        negatives: [],
        pos: 0
    )
}
