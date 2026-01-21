use super::*;

#[test]
fn identifier_unqualified_identifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo",
        rule: Rule::identifier,
        tokens: [ identifier(0, 3) ]
    )
}

#[test]
fn identifier_qualified_identifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo.Bar",
        rule: Rule::identifier,
        tokens: [ identifier(0, 7) ]
    )
}

#[test]
fn unqualified_identifier_starts_with_number_fails() {
    fails_with!(
        parser: UnrealScriptParser,
        input: "0Foo",
        rule: Rule::unqualified_identifier,
        positives: [Rule::unqualified_identifier],
        negatives: [],
        pos: 0
    )
}

#[test]
fn unqualified_identifier_starts_with_underscore() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "_Foo",
        rule: Rule::unqualified_identifier,
        tokens: [ unqualified_identifier(0, 4) ]
    }
}

#[test]
fn unqualified_identifier_list_multiple() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo, Bar, Baz",
        rule: Rule::unqualified_identifier_list,
        tokens: [
            unqualified_identifier(0, 3),
            unqualified_identifier(5, 8),
            unqualified_identifier(10, 13),
        ]
    )
}

#[test]
fn unqualified_identifier_list_single() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo",
        rule: Rule::unqualified_identifier_list,
        tokens: [ unqualified_identifier(0, 3) ]
    )
}
