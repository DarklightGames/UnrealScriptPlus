use super::*;

#[test]
fn code_statement_goto_statement() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "goto 'Foo';",
        rule: Rule::code_statement,
        tokens: [ code_statement(0, 11, [ goto_statement(0, 11, [ unqualified_identifier(6, 9) ]) ]) ]
    }
}

#[test]
fn goto_statement_no_quotes() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "goto Foo;",
        rule: Rule::goto_statement,
        tokens: [ goto_statement(0, 9, [ unqualified_identifier(5, 8) ]) ]
    }
}

#[test]
fn goto_statement_with_quotes() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "goto 'Foo';",
        rule: Rule::goto_statement,
        tokens: [ goto_statement(0, 11, [ unqualified_identifier(6, 9) ]) ]
    }
}

#[test]
fn local_declaration_single() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "local int Foo;",
        rule: Rule::local_declaration,
        tokens: [
            local_declaration(0, 14, [
                type_(6, 9, [ identifier(6, 9) ]),
                var_name(10, 13, [ unqualified_identifier(10, 13) ]),
            ])
        ]
    }
}

#[test]
fn local_declaration_multiple() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "local int Foo, Bar;",
        rule: Rule::local_declaration,
        tokens: [
            local_declaration(0, 19, [
                type_(6, 9, [ identifier(6, 9) ]),
                var_name(10, 13, [ unqualified_identifier(10, 13) ]),
                var_name(15, 18, [ unqualified_identifier(15, 18) ]),
            ])
        ]
    }
}

#[test]
fn code_statement_jump_label() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "Begin:",
        rule: Rule::code_statement,
        tokens: [
            code_statement(0, 6, [ jump_label(0, 6, [ unqualified_identifier(0, 5) ]) ])
        ]
    }
}
