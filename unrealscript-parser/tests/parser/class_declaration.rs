use super::*;

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
