use super::*;

#[test]
fn float_literal_decimal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "3.14159",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 7) ]
    )
}

#[test]
fn float_literal_decimal_negative() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "-3.14159",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 8) ]
    )
}

#[test]
fn float_literal_decimal_positive() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "+3.14159",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 8) ]
    )
}

#[test]
fn float_literal_scientific() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "123.4567e89",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 11) ]
    )
}

#[test]
fn float_literal_scientific_negative() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "-234.997e8",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 10) ]
    )
}

#[test]
fn float_literal_scientific_positive() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "+234.997e8",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 10) ]
    )
}

#[test]
fn float_literal_scientific_no_decimal() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "123e4",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 5) ]
    )
}

#[test]
fn float_literal_scientific_no_fractional() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "123.e4",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 6) ]
    )
}

#[test]
fn float_literal_decimal_trailing_specifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "123.456f",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 8) ]
    )
}

#[test]
fn float_literal_scientific_trailing_specifier() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "123.456e+4f",
        rule: Rule::float_literal,
        tokens: [ float_literal(0, 11) ]
    )
}
