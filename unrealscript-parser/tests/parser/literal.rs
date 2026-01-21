use super::*;

#[test]
fn literal_vector_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "vect(0, 1, 2)",
        rule: Rule::literal,
        tokens: [
            literal(0, 13, [
                vector_literal(0, 13, [
                    numeric_literal(5, 6, [ integer_literal(5, 6, [ integer_literal_decimal(5, 6) ]) ]),
                    numeric_literal(8, 9, [ integer_literal(8, 9, [ integer_literal_decimal(8, 9) ]) ]),
                    numeric_literal(11, 12, [ integer_literal(11, 12, [ integer_literal_decimal(11, 12) ]) ]),
                ])
            ])
        ]
    )
}

#[test]
fn literal_rotator_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "rot(0, 1, 2)",
        rule: Rule::literal,
        tokens: [
            literal(0, 12, [
                rotator_literal(0, 12, [
                    numeric_literal(4, 5, [ integer_literal(4, 5, [ integer_literal_decimal(4, 5) ]) ]),
                    numeric_literal(7, 8, [ integer_literal(7, 8, [ integer_literal_decimal(7, 8) ]) ]),
                    numeric_literal(10, 11, [ integer_literal(10, 11, [ integer_literal_decimal(10, 11) ]) ]),
                ])
            ])
        ]
    )
}

#[test]
fn literal_numeric_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "42",
        rule: Rule::literal,
        tokens: [
            literal(0, 2, [ numeric_literal(0, 2, [ integer_literal(0, 2, [ integer_literal_decimal(0, 2) ]) ]) ])
        ]
    )
}

#[test]
fn literal_string_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "\"Foo\"",
        rule: Rule::literal,
        tokens: [ literal(0, 5, [ string_literal(0, 5, [ string_literal_inner(1, 4) ]) ]) ]
    )
}

#[test]
fn literal_name_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "'Foo_Bar'",
        rule: Rule::literal,
        tokens: [ literal(0, 9, [ name_literal(0, 9, [ name_inner(1, 8) ]) ]) ]
    )
}

#[test]
fn literal_object_literal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "StaticMesh'Foo.Bar.Baz'",
        rule: Rule::literal,
        tokens: [
            literal(0, 23, [
                object_literal(0, 23, [
                    unqualified_identifier(0, 10),
                    single_quoted_string(10, 23, [ single_quoted_string_inner(11, 22) ]),
                ])
            ])
        ]
    )
}
