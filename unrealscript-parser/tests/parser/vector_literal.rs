use super::*;

#[test]
fn vector_literal_with_integer_arguments() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "vect(1, 2, 3)",
        rule: Rule::vector_literal,
        tokens: [
            vector_literal(0, 13, [
                numeric_literal(5, 6, [ integer_literal(5, 6, [ integer_literal_decimal(5, 6) ]) ]),
                numeric_literal(8, 9, [ integer_literal(8, 9, [ integer_literal_decimal(8, 9) ]) ]),
                numeric_literal(11, 12, [ integer_literal(11, 12, [ integer_literal_decimal(11, 12) ]) ]),
            ])
        ]
    )
}

#[test]
fn vector_literal_with_float_arguments() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "vect(1.0, 2.0, 3.0)",
        rule: Rule::vector_literal,
        tokens: [
            vector_literal(0, 19, [
                numeric_literal(5, 8, [ float_literal(5, 8) ]),
                numeric_literal(10, 13, [ float_literal(10, 13) ]),
                numeric_literal(15, 18, [ float_literal(15, 18) ]),
            ])
        ]
    )
}
