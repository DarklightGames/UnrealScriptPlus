use super::*;

#[test]
fn var_declaration_single_int() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "var int Foo;",
        rule: Rule::var_declaration,
        tokens: [
            var_declaration(0, 12, [
                type_(4, 7, [ identifier(4, 7) ]),
                var_name(8, 11, [ unqualified_identifier(8, 11) ]),
            ])
        ]
    )
}

#[test]
fn var_declaration_multiple() {
    parses_to! {
    parser: UnrealScriptParser,
    input: "var int Foo, Bar;",
    rule: Rule::var_declaration,
    tokens: [
        var_declaration(0, 17, [
            type_(4, 7, [ identifier(4, 7) ]),
            var_name(8, 11, [ unqualified_identifier(8, 11) ]),
            var_name(13, 16, [ unqualified_identifier(13, 16) ]),
        ])
    ]}
}

#[test]
fn var_size_integer() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "[42]",
        rule: Rule::var_size,
        tokens: [
            var_size(0, 4, [
                integer_literal(1, 3, [ integer_literal_decimal(1, 3) ])
            ])
        ]
    )
}

#[test]
fn var_size_unqualified_identifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "[FOO]",
        rule: Rule::var_size,
        tokens: [
            var_size(0, 5, [ unqualified_identifier(1, 4) ])
        ]
    )
}

#[test]
fn var_size_empty_fails() {
    fails_with!(
        parser: UnrealScriptParser,
        input: "[]",
        rule: Rule::var_size,
        positives: [Rule::unqualified_identifier, Rule::integer_literal],
        negatives: [],
        pos: 1
    )
}

#[test]
fn var_name() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo",
        rule: Rule::var_name,
        tokens: [
            var_name(0, 3, [ unqualified_identifier(0, 3) ])
        ]
    )
}

#[test]
fn var_name_with_size() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "Foo[3]",
        rule: Rule::var_name,
        tokens: [
            var_name(0, 6, [
                unqualified_identifier(0, 3),
                var_size(3, 6, [
                    integer_literal(4, 5, [ integer_literal_decimal(4, 5) ])
                ])
            ])
        ]
    )
}

#[test]
fn var_category_unqualified_identifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "(Foo)",
        rule: Rule::var_category,
        tokens: [
            var_category(0, 5, [
                unqualified_identifier(1, 4)
            ])
        ]
    )
}

#[test]
fn var_category_empty() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "()",
        rule: Rule::var_category,
        tokens: [ var_category(0, 2) ]
    )
}

#[test]
fn var_declaration_all_features() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "var(Foo) config localized array<Foo.Bar> Foo, Bar, Baz[32];",
        rule: Rule::var_declaration,
        tokens: [
            var_declaration(0, 59, [
                var_category(3, 8, [ unqualified_identifier(4, 7) ]),
                var_modifier(9, 15),
                var_modifier(16, 25),
                type_(26, 40, [ array_type(26, 40, [ type_(32, 39, [ identifier(32, 39) ]) ]) ]),
                var_name(41, 44, [ unqualified_identifier(41, 44) ]),
                var_name(46, 49, [ unqualified_identifier(46, 49) ]),
                var_name(51, 58, [
                    unqualified_identifier(51, 54),
                    var_size(54, 58, [ integer_literal(55, 57, [ integer_literal_decimal(55, 57) ]) ]),
                ])
            ])
        ]
    }
}

#[test]
fn var_declaration_with_inline_struct_type() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "var struct Foo { var int Bar; } Baz;",
        rule: Rule::var_declaration,
        tokens: [
            var_declaration(0, 36, [
                type_(4, 31, [
                    struct_declaration(4, 31, [
                        unqualified_identifier(11, 14),
                        struct_var_declaration(17, 29, [
                            type_(21, 24, [ identifier(21, 24) ]),
                            var_name(25, 28, [ unqualified_identifier(25, 28) ]),
                        ])
                    ])
                ]),
                var_name(32, 35, [ unqualified_identifier(32, 35) ]),
            ])
        ]
    }
}
