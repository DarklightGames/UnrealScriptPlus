use super::*;

#[test]
fn rotator_literal_with_int_arguments() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "rot(1, 2, 3)",
        rule: Rule::rotator_literal,
        tokens: [
            rotator_literal(0, 12, [
                numeric_literal(4, 5, [ integer_literal(4, 5, [ integer_literal_decimal(4, 5) ]) ]),
                numeric_literal(7, 8, [ integer_literal(7, 8, [ integer_literal_decimal(7, 8) ]) ]),
                numeric_literal(10, 11, [ integer_literal(10, 11, [ integer_literal_decimal(10, 11) ]) ]),
            ])
        ]
    )
}

#[test]
fn rotator_literal_with_float_arguments() {
    parses_to! (
        parser: UnrealScriptParser,
        input: "rot(1.0, 2.0, 3.0)",
        rule: Rule::rotator_literal,
        tokens: [
            rotator_literal(0, 18, [
                numeric_literal(4, 7, [ float_literal(4, 7) ]),
                numeric_literal(9, 12, [ float_literal(9, 12) ]),
                numeric_literal(14, 17, [ float_literal(14, 17) ]),
            ])
        ]
    )
}

#[test]
fn rotator_literal_with_too_few_arguments() {
    fails_with! (
        parser: UnrealScriptParser,
        input: "rot(0, 1)",
        rule: Rule::rotator_literal,
        positives: [Rule::integer_literal_hexadecimal, Rule::numeric_sign, Rule::float_literal],
        negatives: [],
        pos: 7
    )
}

#[test]
fn rotator_literal_with_too_many_arguments() {
    fails_with! (
        parser: UnrealScriptParser,
        input: "rot(0, 1, 2, 3)",
        rule: Rule::rotator_literal,
        positives: [Rule::integer_literal_hexadecimal, Rule::numeric_sign, Rule::float_literal],
        negatives: [],
        pos: 10
    )
}
