use super::*;

#[test]
fn integer_literal_decimal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "1234567",
        rule: Rule::integer_literal,
        tokens: [ integer_literal(0, 7, [ integer_literal_decimal(0, 7) ]) ]
    )
}

#[test]
fn integer_literal_decimal_negative() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "-1234567",
        rule: Rule::integer_literal,
        tokens: [ integer_literal(0, 8, [ numeric_sign(0, 1), integer_literal_decimal(1, 8) ]) ]
    )
}

#[test]
fn integer_literal_decimal_positive() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "+1234567",
        rule: Rule::integer_literal,
        tokens: [
            integer_literal(0, 8, [ numeric_sign(0, 1), integer_literal_decimal(1, 8) ])
        ]
    )
}

#[test]
fn integer_literal_hexadecimal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "0x0123456789ABCDEF",
        rule: Rule::integer_literal,
        tokens: [
            integer_literal(0, 18, [
                integer_literal_hexadecimal(0, 18, [ hex_digits(2, 18) ])
            ])
        ]
    )
}

#[test]
fn integer_literal_hexadecimal_negative() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "-0x0123456789ABCDEF",
        rule: Rule::integer_literal,
        tokens: [
            integer_literal(0, 19, [
                numeric_sign(0, 1),
                integer_literal_hexadecimal(1, 19, [ hex_digits(3, 19) ]),
            ])
        ]
    )
}
