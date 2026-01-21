use super::*;

#[test]
fn struct_declaration_no_members() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "struct Foo { }",
        rule: Rule::struct_declaration,
        tokens: [
            struct_declaration(0, 14, [
                unqualified_identifier(7, 10),
            ])
        ]
    }
}

#[test]
fn struct_var_declaration_simple() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "var int A;",
        rule: Rule::struct_var_declaration,
        tokens: [
            struct_var_declaration(0, 10, [
                type_(4, 7, [ identifier(4, 7) ]),
                var_name(8, 9, [ unqualified_identifier(8, 9) ]),
            ])
        ]
    }
}

#[test]
fn struct_var_declaration_editable() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "var() int A;",
        rule: Rule::struct_var_declaration,
        tokens: [
            struct_var_declaration(0, 12, [
                struct_var_editable(3, 5),
                type_(6, 9, [ identifier(6, 9) ]),
                var_name(10, 11, [ unqualified_identifier(10, 11) ]),
            ])
        ]
    }
}

#[test]
fn struct_declaration_with_modifiers() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "struct native transient Foo { }",
        rule: Rule::struct_declaration,
        tokens: [
            struct_declaration(0, 31, [
                struct_modifier(7, 13),
                struct_modifier(14, 23),
                unqualified_identifier(24, 27),
            ])
        ]
    }
}

#[test]
fn struct_declaration_with_extends() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "struct Foo extends Bar { }",
        rule: Rule::struct_declaration,
        tokens: [
            struct_declaration(0, 26, [
                unqualified_identifier(7, 10),
                extends(11, 22, [ identifier(19, 22) ]),
            ])
        ]
    }
}

#[test]
fn struct_declaration_with_members() {
    parses_to!{
        parser: UnrealScriptParser,
        input: "struct Foo { var int Bar; var string Baz; }",
        rule: Rule::struct_declaration,
        tokens: [
            struct_declaration(0, 43, [
                unqualified_identifier(7, 10),
                struct_var_declaration(13, 25, [
                    type_(17, 20, [ identifier(17, 20) ]),
                    var_name(21, 24, [ unqualified_identifier(21, 24) ]),
                ]),
                struct_var_declaration(26, 41, [
                    type_(30, 36, [ identifier(30, 36) ]),
                    var_name(37, 40, [ unqualified_identifier(37, 40) ]),
                ]),
            ])
        ]
    }
}
