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

#[test]
fn function_type_with_wacky_case() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "FuNcTiOn",
        rule: Rule::function_type_no_arguments_type,
        tokens: [
            function_type_no_arguments_type(0, 8)
        ]
    }
}

#[test]
fn function_declaration_operator() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "native(242) static final operator(24) bool  == ( bool A, bool B );",
        rule: Rule::function_declaration,
        tokens: [
            function_declaration(0, 66, [
            function_modifier(0, 11, [
                // native
                function_modifier_type(0, 6),
                // 242
                integer_literal(7, 10, [ integer_literal_decimal(7, 10) ])
            ]),
            // static
            function_modifier(12, 18, [
                function_modifier_type(12, 18)
            ]),
            // final
            function_modifier(19, 24, [
                function_modifier_type(19, 24)
            ]),
            // operator(24)
            function_type(25, 37, [
                function_type_with_arguments(25, 37, [
                    function_type_with_arguments_type(25, 33),
                    integer_literal(34, 36, [ integer_literal_decimal(34, 36) ])
                ])
            ]),
            // bool
            type_(38, 42, [ identifier(38, 42) ]),
            // ==
            function_name(44, 46, [ symbolic_verb(44, 46) ]),
            function_argument(49, 55, [
                // bool
                type_(49, 53, [ identifier(49, 53) ]),
                // A
                var_name(54, 55, [ unqualified_identifier(54, 55) ])
            ]),
            function_argument(57, 63, [
                // bool
                type_(57, 61, [ identifier(57, 61) ]),
                // B
                var_name(62, 63, [ unqualified_identifier(62, 63) ])
            ])
            ])
        ]
    }
}

#[test]
fn function_declaration_preoperator() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "native(141) static final preoperator  int  ~  ( int A );",
        rule: Rule::function_declaration,
        tokens: [
            function_declaration(0,  56, [
                function_modifier(0, 11, [
                    // native
                    function_modifier_type(0, 6),
                    // 141
                    integer_literal(7, 10, [ integer_literal_decimal(7, 10) ])
                ]),
                // static
                function_modifier(12, 18, [
                    function_modifier_type(12, 18)
                ]),
                // final
                function_modifier(19, 24, [
                    function_modifier_type(19, 24)
                ]),
                // preoperator
                function_type(25, 36, [ function_type_no_arguments_type(25, 36) ]),
                // int
                type_(38, 41, [ identifier(38, 41) ]),
                // ~
                function_name(43, 44, [ symbolic_verb(43, 44) ]),
                function_argument(48, 53, [
                    // int
                    type_(48, 51, [ identifier(48, 51) ]),
                    // A
                    var_name(52, 53, [ unqualified_identifier(52, 53) ])
                ])
            ])
        ]   
    }
}
