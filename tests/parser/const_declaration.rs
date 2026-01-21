use super::*;

#[test]
fn const_declaration() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "const Foo = 42;",
        rule: Rule::const_declaration,
        tokens: [
            const_declaration(0, 15, [
                unqualified_identifier(6, 9),
                literal(12, 14, [ numeric_literal(12, 14, [ integer_literal(12, 14, [integer_literal_decimal(12, 14)]) ]) ]),
            ])
        ]
    )
}
