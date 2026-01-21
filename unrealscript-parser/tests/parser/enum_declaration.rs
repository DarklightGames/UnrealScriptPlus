use super::*;

#[test]
fn enum_declaration_basic() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "enum Foo { Bar, Baz }",
        rule: Rule::enum_declaration,
        tokens: [
            enum_declaration(0, 21, [
                unqualified_identifier(5, 8),
                unqualified_identifier(11, 14),
                unqualified_identifier(16, 19),
            ])
        ]
    }
}

#[test]
fn enum_declaration_trailing_comma() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "enum Foo { Bar, Baz, }",
        rule: Rule::enum_declaration,
        tokens: [
            enum_declaration(0, 22, [
                unqualified_identifier(5, 8),
                unqualified_identifier(11, 14),
                unqualified_identifier(16, 19),
            ])
        ]
    }
}
