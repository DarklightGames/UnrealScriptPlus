use super::*;

#[test]
fn function_argument_basic() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "int Foo",
        rule: Rule::function_argument,
        tokens: [
            function_argument(0, 7, [
                type_(0, 3, [ identifier(0, 3) ]),
                var_name(4, 7, [ unqualified_identifier(4, 7) ]),
            ])
        ]
    }
}

#[test]
fn function_declaration_with_no_return_type() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "function Foo();",
        rule: Rule::function_declaration,
        tokens: [
            function_declaration(0, 15, [
                function_type(0, 8, [ function_type_no_arguments_type(0, 8) ]),
                function_name(9, 12, [ unqualified_identifier(9, 12) ]),
            ])
        ]
    }
}

#[test]
fn function_declaration_with_return_type() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "function int Foo();",
        rule: Rule::function_declaration,
        tokens: [
            function_declaration(0, 19, [
                function_type(0, 8, [ function_type_no_arguments_type(0, 8) ]),
                type_(9, 12, [ identifier(9, 12) ]),
                function_name(13, 16, [ unqualified_identifier(13, 16) ]),
            ])
        ]
    }
}

#[test]
fn function_declaration_with_one_argument() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "function Foo(string Bar);",
        rule: Rule::function_declaration,
        tokens: [
            function_declaration(0, 25, [
                function_type(0, 8, [ function_type_no_arguments_type(0, 8) ]),
                function_name(9, 12, [ unqualified_identifier(9, 12) ]),
                function_argument(13, 23, [
                    type_(13, 19, [ identifier(13, 19) ]),
                    var_name(20, 23, [ unqualified_identifier(20, 23) ]),
                ])
            ])
        ]
    }
}
