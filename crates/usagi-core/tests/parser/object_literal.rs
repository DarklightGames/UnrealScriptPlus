use super::*;

#[test]
fn object_literal() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "StaticMesh'Foo.Bar.Baz'",
        rule: Rule::object_literal,
        tokens: [
            object_literal(0, 23, [
                unqualified_identifier(0, 10),
                single_quoted_string(10, 23, [ single_quoted_string_inner(11, 22) ]),
            ])
        ]
    )
}

#[test]
fn object_literal_with_underscore() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "class'Foo_Bar'",
        rule: Rule::object_literal,
        tokens: [ object_literal(0, 14, [
            unqualified_identifier(0, 5),
            single_quoted_string(5, 14, [ single_quoted_string_inner(6, 13) ]),
        ]) ]
    )
}
