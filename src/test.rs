#[cfg(test)]
mod test {
    use pest::{parses_to, consumes_to, fails_with};
    use crate::parser::{UnrealScriptParser, Rule};
    use crate::transform::{ScriptBuilder, ScriptFormattingOptions};

    #[test]
    fn comment_single_line() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "// This is a single-line comment!",
            rule: Rule::comment_single_line,
            tokens: []
        )
    }

    #[test]
    fn comment_multi_line() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "/* This is a multi-line comment! */",
            rule: Rule::comment_multi_line,
            tokens: []
        )
    }

    #[test]
    fn comment_multi_line_nested() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "/* /* This is a multi-line comment! */ Foo */",
            rule: Rule::comment_multi_line,
            tokens: []
        )
    }

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

    #[test]
    fn identifier_unqualified_identifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo",
            rule: Rule::identifier,
            tokens: [ identifier(0, 3) ]
        )
    }

    #[test]
    fn identifier_qualified_identifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo.Bar",
            rule: Rule::identifier,
            tokens: [ identifier(0, 7) ]
        )
    }

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

    #[test]
    fn string_literal_quick_brown_fox() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "\"The quick brown foxed jumped over the lazy dog.\"",
            rule: Rule::string_literal,
            tokens: [ string_literal(0, 49, [ string_literal_inner(1, 48) ]) ]
        )
    }

    #[test]
    fn string_literal_empty() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "\"\"",
            rule: Rule::string_literal,
            tokens: [ string_literal(0, 2, [ string_literal_inner(1, 1) ]) ]
        )
    }

    #[test]
    fn name_literal_all_characters() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "'0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'",
            rule: Rule::name_literal,
            tokens: [ name_literal(0, 65, [ name_inner(1, 64) ])]
        )
    }

    #[test]
    fn name_literal_empty() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "''",
            rule: Rule::name_literal,
            tokens: [ name_literal(0, 2, [ name_inner(1, 1) ]) ]
        )
    }

    #[test]
    fn name_literal_bad_character() {
        fails_with! (
            parser: UnrealScriptParser,
            input: "'abcdef^asd'",
            rule: Rule::name_literal,
            positives: [Rule::name_literal],
            negatives: [],
            pos: 0
        )
    }

    #[test]
    fn object_literal() {
        parses_to! (
            parser: UnrealScriptParser,
            input: "StaticMesh'Foo.Bar.Baz'",
            rule: Rule::object_literal,
            tokens: [
                object_literal(0, 23, [
                    unqualified_identifier(0, 10),
                    single_quoted_string(10, 23, [ single_quoted_string_inner(11, 22) ]),
                ])
            ]
        )
    }

    #[test]
    fn object_literal_with_underscore() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class'Foo_Bar'",
            rule: Rule::object_literal,
            tokens: [ object_literal(0, 14, [
                unqualified_identifier(0, 5),
                single_quoted_string(5, 14, [ single_quoted_string_inner(6, 13) ]),
            ]) ]
        )
    }

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
    fn numeric_literal_integer() {
        parses_to! (
            parser: UnrealScriptParser,
            input: "1234",
            rule: Rule::numeric_literal,
            tokens: [ numeric_literal(0, 4, [ integer_literal(0, 4, [ integer_literal_decimal(0, 4) ]) ]) ]
        )
    }

    #[test]
    fn numeric_literal_float() {
        parses_to! (
            parser: UnrealScriptParser,
            input: "1234.5678",
            rule: Rule::numeric_literal,
            tokens: [ numeric_literal(0, 9, [ float_literal(0, 9) ]) ]
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
    fn class_declaration_no_extends() {
        parses_to! (
            parser: UnrealScriptParser,
            input: "class Foo;",
            rule: Rule::class_declaration,
            tokens: [ class_declaration(0, 10, [ unqualified_identifier(6, 9) ]) ]
        )
    }

    #[test]
    fn class_declaration_no_modifiers() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar;",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 22, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_all_parameterless_modifiers() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar abstract cacheexempt instanced parseconfig perobjectconfig safereplace transient collapsecategories dontcollapsecategories editinlinenew noteditinlinenew hidedropdown placeable notplaceable exportstructs intrinsic native nativereplication noexport;",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 270, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 30, [ class_modifier_type(22, 30) ]), // abstract
                    class_modifier(31, 42, [ class_modifier_type(31, 42) ]), // cacheexempt
                    class_modifier(43, 52, [ class_modifier_type(43, 52) ]), // instanced
                    class_modifier(53, 64, [ class_modifier_type(53, 64) ]), // paseconfig
                    class_modifier(65, 80, [ class_modifier_type(65, 80) ]), // perobjectconfig
                    class_modifier(81, 92, [ class_modifier_type(81, 92) ]), // safereplace
                    class_modifier(93, 102, [ class_modifier_type(93, 102) ]), // transient
                    class_modifier(103, 121, [ class_modifier_type(103, 121) ]), // collapsecategories
                    class_modifier(122, 144, [ class_modifier_type(122, 144) ]), // dontcollapsecategories
                    class_modifier(145, 158, [ class_modifier_type(145, 158) ]), // editinlinenew
                    class_modifier(159, 175, [ class_modifier_type(159, 175) ]), // noteditinlinenew
                    class_modifier(176, 188, [ class_modifier_type(176, 188) ]), // hidedropdown
                    class_modifier(189, 198, [ class_modifier_type(189, 198) ]), // placeable
                    class_modifier(199, 211, [ class_modifier_type(199, 211) ]), // notplaceable
                    class_modifier(212, 225, [ class_modifier_type(212, 225) ]), // exportstructs
                    class_modifier(226, 235, [ class_modifier_type(226, 235) ]), // intrinsic
                    class_modifier(236, 242, [ class_modifier_type(236, 242) ]), // native
                    class_modifier(243, 260, [ class_modifier_type(243, 260) ]), // nativereplication
                    class_modifier(261, 269, [ class_modifier_type(261, 269) ]) // noexport
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_config_modifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar config(Baz);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 34, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 33, [
                        class_modifier_type(22, 28),
                        expression_list(29, 32, [ expression(29, 32, [ unqualified_identifier(29, 32) ]) ]),
                    ]),
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_dependson_modifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar dependson(Baz);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 37, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 36, [
                        class_modifier_type(22, 31),
                        expression_list(32, 35, [ expression(32, 35, [ unqualified_identifier(32, 35) ]) ]),
                    ]),
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_guid_modifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar guid(1, 2, 3, 4);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 39, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 38, [
                        class_modifier_type(22, 26),
                        expression_list(27, 37, [
                            expression(27, 28, [ numeric_literal(27, 28, [ integer_literal(27, 28, [ integer_literal_decimal(27, 28) ]) ]) ]),
                            expression(30, 31, [ numeric_literal(30, 31, [ integer_literal(30, 31, [ integer_literal_decimal(30, 31) ]) ]) ]),
                            expression(33, 34, [ numeric_literal(33, 34, [ integer_literal(33, 34, [ integer_literal_decimal(33, 34) ]) ]) ]),
                            expression(36, 37, [ numeric_literal(36, 37, [ integer_literal(36, 37, [ integer_literal_decimal(36, 37) ]) ]) ]),
                        ])
                    ]),
                ])
            ]
        )
    }

    #[test]
    fn unqualified_identifier_starts_with_number_fails() {
        fails_with!(
            parser: UnrealScriptParser,
            input: "0Foo",
            rule: Rule::unqualified_identifier,
            positives: [Rule::unqualified_identifier],
            negatives: [],
            pos: 0
        )
    }

    #[test]
    fn unqualified_identifier_starts_with_underscore() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "_Foo",
            rule: Rule::unqualified_identifier,
            tokens: [ unqualified_identifier(0, 4) ]
        }
    }

    #[test]
    fn unqualified_identifier_list_multiple() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo, Bar, Baz",
            rule: Rule::unqualified_identifier_list,
            tokens: [
                unqualified_identifier(0, 3),
                unqualified_identifier(5, 8),
                unqualified_identifier(10, 13),
            ]
        )
    }

    #[test]
    fn unqualified_identifier_list_single() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo",
            rule: Rule::unqualified_identifier_list,
            tokens: [ unqualified_identifier(0, 3) ]
        )
    }

    #[test]
    fn class_declaration_hidecategories_modifier() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar hidecategories(Foo, Bar, Baz);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 52, [
                    unqualified_identifier(6, 9),
                    extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 51, [
                        class_modifier_type(22, 36),
                        expression_list(37, 50, [
                            expression(37, 40, [ unqualified_identifier(37, 40) ]),
                            expression(42, 45, [ unqualified_identifier(42, 45) ]),
                            expression(47, 50, [ unqualified_identifier(47, 50) ]),
                        ])
                    ])
                ])
            ]
        )
    }

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
    fn array_int() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "array<int>",
            rule: Rule::array_type,
            tokens: [ array_type(0, 10, [ type_(6, 9, [ identifier(6, 9) ]) ]) ]
        )
    }

    #[test]
    fn array_type_empty_fails() {
        fails_with!(
            parser: UnrealScriptParser,
            input: "array<>",
            rule: Rule::array_type,
            positives: [Rule::type_],
            negatives: [],
            pos: 6
        )
    }

    #[test]
    fn nested_array_types() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "array<array<int>>",
            rule: Rule::array_type,
            tokens: [
                array_type(0, 17, [ type_(6, 16, [
                    array_type(6, 16, [type_(12, 15, [ identifier(12, 15)])]),
                ])])
            ]
        }
    }

    #[test]
    fn class_type() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class<Foo.Bar>",
            rule: Rule::class_type,
            tokens: [ class_type(0, 14, [ identifier(6, 13) ]) ]
        )
    }

    #[test]
    fn class_type_empty_fails() {
        fails_with!(
            parser: UnrealScriptParser,
            input: "class<>",
            rule: Rule::class_type,
            positives: [Rule::identifier],
            negatives: [],
            pos: 6
        )
    }

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

    #[test]
    fn bool_literal_false_as_expression() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "false",
            rule: Rule::expression,
            tokens: [ expression(0, 5, [ literal (0, 5, [ boolean_literal(0, 5) ]) ]) ]
        )
    }

    #[test]
    fn bool_literal_true_as_expression() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "true",
            rule: Rule::expression,
            tokens: [ expression(0, 4, [ literal (0, 4, [ boolean_literal(0, 4) ]) ]) ]
        )
    }

    #[test]
    fn const_declaration() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "const Foo = 42;",
            rule: Rule::const_declaration,
            tokens: [
                const_declaration(0, 15, [
                    unqualified_identifier(6, 9),
                    literal(12, 14, [ numeric_literal(12, 14, [ integer_literal(12, 14, [integer_literal_decimal(12, 14)]) ]) ]),
                ])
            ]
        )
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
    fn enum_declaration_basic() {
        parses_to!{
            parser: UnrealScriptParser,
            input: "enum Foo { Bar, Baz }",
            rule: Rule::enum_declaration,
            tokens: [
                enum_declaration(0, 21, [
                    unqualified_identifier(5, 8),
                    unqualified_identifier(11, 14),
                    unqualified_identifier(16, 19),
                ])
            ]
        }
    }

    #[test]
    fn enum_declaration_trailing_comma() {
        parses_to!{
            parser: UnrealScriptParser,
            input: "enum Foo { Bar, Baz, }",
            rule: Rule::enum_declaration,
            tokens: [
                enum_declaration(0, 22, [
                    unqualified_identifier(5, 8),
                    unqualified_identifier(11, 14),
                    unqualified_identifier(16, 19),
                ])
            ]
        }
    }

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
    fn replication_block_empty() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "replication { }",
            rule: Rule::replication_block,
            tokens: [
                replication_block(0, 15)
            ]
        }
    }

    #[test]
    fn replication_reliability_reliable() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "reliable",
            rule: Rule::replication_reliability,
            tokens: [ replication_reliability(0, 8) ]
        }
    }

    #[test]
    fn replication_reliability_unreliable() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "unreliable",
            rule: Rule::replication_reliability,
            tokens: [ replication_reliability(0, 10) ]
        }
    }
    #[test]
    fn replication_statement_no_variables_fails() {
        fails_with! {
            parser: UnrealScriptParser,
            input: "reliable if (true);",
            rule: Rule::replication_statement,
            positives: [Rule::unqualified_identifier],
            negatives: [],
            pos: 18
        }
    }

    #[test]
    fn replication_statement_single_variable() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "reliable if (Foo) Bar;",
            rule: Rule::replication_statement,
            tokens: [
                replication_statement(0, 22, [
                    replication_reliability(0, 8),
                    expression(13, 16, [ unqualified_identifier(13, 16) ]),
                    unqualified_identifier(18, 21),
                ])
            ]
        }
    }

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
    fn replication_statement_multiple_variables() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "reliable if (Foo) Bar, Baz;",
            rule: Rule::replication_statement,
            tokens: [
                replication_statement(0, 27, [
                    replication_reliability(0, 8),
                    expression(13, 16, [ unqualified_identifier(13, 16) ]),
                    unqualified_identifier(18, 21),
                    unqualified_identifier(23, 26),
                ])
            ]
        }
    }

    #[test]
    fn compiler_directive_start_of_line() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "#exec OBJ LOAD FILE=..\\Foo\\Bar.utx",
            rule: Rule::compiler_directive,
            tokens: [ compiler_directive(0, 34, [ compiler_directive_inner(1, 34), EOI(34, 34) ]) ]
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
}
