#[allow(unused_imports)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "UnrealScript.pest"]
struct UnrealScriptParser;

use std::fs::File;
use std::io::Read;
use crate::ast::{AstNode};
use std::ops::AddAssign;
use pest::iterators::Pair;

mod ast;

use std::convert::Into;

impl Into<Box<AstNode>> for Pair<'_, Rule> {
    fn into(self) -> Box<AstNode> {
        match self.as_rule() {
            Rule::program => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::Program {
                    class_declaration: inner_iter.next().unwrap().into(),
                    statements: inner_iter.map(|pair| { pair.into() }).collect()
                })
            }
            Rule::replication_statement => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::ReplicationStatement {
                    is_reliable: match inner_iter.next().unwrap().as_str().to_lowercase().as_str() {
                        "reliable" => { true }
                        "unreliable" => { false }
                        _ => { panic!("unrecognized reliability type") }
                    },
                    condition: inner_iter.next().unwrap().into(),
                    variables: inner_iter.map(|pair| { pair.as_str().to_string() }).collect()
                })
            }
            Rule::replication_block => {
                Box::new(AstNode::ReplicationBlock {
                    statements: self.into_inner().into_iter().map(|pair| {
                        pair.into()
                    }).collect()
                })
            }
            Rule::class_declaration => {
                // TODO: turn this into a matching iter
                let inner_iter = self.into_inner().into_iter();
                let mut name: String = String::new();
                let mut parent_class: Option<String> = None;
                let mut modifiers: Vec<String> = Vec::new();
                inner_iter.for_each(|pair| {
                    match pair.as_rule() {
                        Rule::unqualified_identifier => {
                            name = pair.as_str().to_string()
                        }
                        Rule::class_extends => {
                            parent_class = Some(pair.into_inner().next().unwrap().as_str().to_string())
                        }
                        Rule::class_modifier => {
                            modifiers.push(pair.as_str().to_string())
                        }
                        _ => { panic!("unexpected pair")}
                    }
                });
                Box::new(AstNode::ClassDeclaration {
                    name,
                    parent_class,
                    modifiers
                })
            }
            Rule::const_declaration => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::ConstDeclaration {
                    name: inner_iter.next().unwrap().as_str().to_string(),
                    value: inner_iter.next().unwrap().into()
                })
            }
            Rule::enum_declaration => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::EnumDeclaration {
                    name: inner_iter.next().unwrap().as_str().to_string(),
                    values: inner_iter.next().unwrap().into_inner().into_iter().map(|pair| {
                        return pair.as_str().to_string();
                    }).collect()
                })
            }
            Rule::literal => {
                self.into_inner().next().unwrap().into()
            }
            Rule::identifier => {
                Box::new(AstNode::Identifier(self.as_str().to_string()))
            }
            Rule::class_type => {
                Box::new(AstNode::ClassType(self.into_inner().next().unwrap().into()))
            }
            Rule::boolean_literal => {
                Box::new(AstNode::BooleanLiteral(self.as_str().to_lowercase().parse::<bool>().unwrap()))
            }
            Rule::integer_literal => {
                let mut integer: i32 = 0;
                let mut sign = 1;
                self.into_inner().into_iter().for_each(|pair| {
                    match pair.as_rule() {
                        Rule::positive_or_negative => {
                            if pair.as_str() == "-" {
                                sign = -1
                            }
                        }
                        Rule::integer_literal_hexadecimal => {
                            integer = i32::from_str_radix(pair.into_inner().next().unwrap().as_str(), 16).unwrap()
                        },
                        Rule::integer_literal_decimal => {
                            integer = pair.as_str().parse::<i32>().unwrap();
                        }
                        _ => panic!("unhandled rule")
                    }
                });
                Box::new(AstNode::IntegerLiteral(integer * sign))
            }
            Rule::float_literal => {
                Box::new(AstNode::FloatLiteral(self.as_str().parse::<f32>().unwrap()))
            }
            Rule::object_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::ObjectLiteral {
                    type_: inner_iter.next().unwrap().as_str().to_string(),
                    reference: inner_iter.next().unwrap().into_inner().next().unwrap().as_str().to_string()
                })
            }
            Rule::name_literal => {
                Box::new(AstNode::NameLiteral(self.into_inner().next().unwrap().as_str().to_string()))
            }
            Rule::string_literal => {
                Box::new(AstNode::StringLiteral(self.into_inner().next().unwrap().as_str().to_string()))
            }
            Rule::vector_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::VectorLiteral {
                    x: inner_iter.next().unwrap().into(),
                    y: inner_iter.next().unwrap().into(),
                    z: inner_iter.next().unwrap().into()
                })
            }
            Rule::array_type => {
                Box::new(AstNode::ArrayType(self.into_inner().next().unwrap().into()))
            }
            Rule::rotator_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Box::new(AstNode::RotatorLiteral {
                    pitch: inner_iter.next().unwrap().into(),
                    yaw: inner_iter.next().unwrap().into(),
                    roll: inner_iter.next().unwrap().into()
                })
            }
            Rule::type_ => {
                self.into_inner().next().unwrap().into()
            }
            Rule::var_name => {
                let mut inner_iter = self.into_inner().into_iter();
                let name = inner_iter.next().unwrap().as_str().to_string();
                let mut size: Option<Box<AstNode>> = None;
                if let Some(pair) = inner_iter.next() {
                    size = Some(pair.into())
                }
                Box::new(AstNode::VarName {
                    name,
                    size
                })
            }
            Rule::var_declaration => {
                let mut category: Option<String> = None;
                let mut modifiers: Vec<String> = Vec::new();
                let mut type_: Option<Box<AstNode>> = None;
                let mut names: Vec<Box<AstNode>> = Vec::new();
                self.into_inner().into_iter().for_each(|pair| {
                    match pair.as_rule() {
                        Rule::var_category => {
                            let inner = pair.into_inner().next();
                            category = match inner {
                                Some(pair) => Some(pair.as_str().to_string()),
                                None => Some(String::new())
                            }
                        }
                        Rule::var_modifier => {
                            modifiers.push(pair.as_str().to_string())
                        },
                        Rule::type_ => {
                            type_ = Some(pair.into())
                        },
                        Rule::var_name => {
                            names.push(pair.into())
                        }
                        _ => { panic!("invalid pair") }
                    }
                });
                Box::new(AstNode::VarDeclaration {
                    category,
                    modifiers,
                    type_: type_.unwrap(),
                    names
                })
            }
            Rule::unqualified_identifier => {
                Box::new(AstNode::UnqualifiedIdentifier(self.as_str().to_string()))
            },
            Rule::function_argument => {
                let mut modifiers: Vec<String> = Vec::new();
                let mut type_: Option<Box<AstNode>> = None;
                let mut name: Option<Box<AstNode>> = None;
                self.into_inner().into_iter().for_each(|pair| {
                    match pair.as_rule() {
                        Rule::function_argument_modifier => {
                            modifiers.push(pair.as_str().to_string())
                        }
                        Rule::type_ => {
                            type_ = Some(pair.into())
                        }
                        Rule::var_name => {
                            name = Some(pair.into())
                        }
                        _ => { panic!("unhandled rule") }
                    }
                });
                Box::new(AstNode::FunctionArgument {
                    modifiers,
                    type_: type_.unwrap(),
                    name: name.unwrap()
                })
            }
            Rule::function_declaration => {
                let mut modifiers: Vec<String> = Vec::new();
                let mut type_: String = String::new();
                let mut return_type: Option<Box<AstNode>> = None;
                let mut arguments: Vec<Box<AstNode>> = Vec::new();
                let mut name: String = String::new();
                let mut body: Option<Box<AstNode>> = None;
                self.into_inner().into_iter().for_each(|pair| {
                    match pair.as_rule() {
                        Rule::function_type => {
                            type_ = pair.as_str().to_string()
                        }
                        Rule::function_modifier => {
                            modifiers.push(pair.as_str().to_string())
                        }
                        Rule::type_ => {
                            return_type = Some(pair.into())
                        }
                        Rule::unqualified_identifier => {
                            name = pair.as_str().to_string()
                        }
                        Rule::function_argument => {
                            arguments.push(pair.into())
                        }
                        Rule::code_body => {
                           body = None;
                        }
                        _ => { panic!("unhandled pair") }
                    }
                });
                Box::new(AstNode::FunctionDelaration {
                    type_,
                    modifiers,
                    return_type,
                    name,
                    arguments,
                    body
                })
            }
            Rule::var_size => {
                self.into_inner().next().unwrap().into()
            }
            Rule::call => {
                let mut arguments: Vec<Option<Box<AstNode>>> = Vec::new();
                self.into_inner().into_iter().for_each(|pair| {
                    match pair.as_rule() {
                        Rule::expression => {
                            arguments.push(Some(pair.into()))
                        },
                        Rule::empty_expression => {
                            arguments.push(None)
                        },
                        _ => { panic!("unhandled rule") }
                    }
                });
                Box::new(AstNode::Call { arguments })
            },
            Rule::expression => {
                Box::new(AstNode::Expression)
            }
            Rule::struct_declaration => {
                let rules_itr = self.into_inner().into_iter();
                let mut modifiers: Vec<String> = Vec::new();
                let mut parent: Option<String> = None;
                let mut name: String = String::new();
                let mut members: Vec<Box<AstNode>> = Vec::new();
                rules_itr.for_each(|pair| {
                    match pair.as_rule() {
                        Rule::struct_modifier => {
                            modifiers.push(pair.as_str().to_string())
                        }
                        Rule::unqualified_identifier => {
                            name.add_assign(pair.as_str())
                        }
                        Rule::struct_extends => {
                            parent = Some(pair.into_inner().next().unwrap().as_str().to_string())
                        }
                        Rule::struct_var_declaration => {
                            let mut editable = false;
                            let mut modifiers: Vec<String> = Vec::new();
                            let mut type_: Option<Box<AstNode>> = None;
                            let mut names: Vec<Box<AstNode>> = Vec::new();
                            pair.into_inner().into_iter().for_each(|pair| {
                                match pair.as_rule() {
                                    Rule::struct_var_editable => {
                                        editable = true
                                    }
                                    Rule::var_modifier => {
                                        modifiers.push(pair.as_str().to_string())
                                    },
                                    Rule::type_ => {
                                        type_ = Some(pair.into())
                                    },
                                    Rule::var_name => {
                                        names.push(pair.into())
                                    }
                                    _ => { panic!("Invalid state") }
                                }
                            });
                            members.push( Box::new(AstNode::StructVarDeclaration {
                                editable,
                                modifiers,
                                type_: type_.unwrap(),
                                names,
                            }));
                        }
                        _ => { }
                    }
                });
                Box::new(AstNode::StructDeclaration {
                    modifiers,
                    name,
                    parent,
                    members
                })
            },
            _ => { Box::new(AstNode::Unhandled) }
        }
    }
}

fn read_file_to_string(path: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    return Ok(contents)
}

fn main() {
    let program = read_file_to_string("C:\\UnrealScriptPlus\\src\\TestClass.uc")
        .unwrap_or_else(|e| panic!("{}", e));
    let root = UnrealScriptParser::parse(Rule::program, &program)
        .expect("failed to parse")
        .next()
        .unwrap();
    let program: Box<AstNode> = root.into();
    println!("{:?}", program);
    // TODO: now we've got the AST, what do we do with it!
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::{parses_to, consumes_to, fails_with};

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
    fn integer_literal_decimal() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "1234567",
            rule: Rule::integer_literal,
            tokens: [
                integer_literal(0, 7, [
                    integer_literal_decimal(0, 7)
                ])
            ]
        )
    }

    #[test]
    fn integer_literal_decimal_negative() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "-1234567",
            rule: Rule::integer_literal,
            tokens: [ integer_literal(0, 8, [
                positive_or_negative(0, 1),
                integer_literal_decimal(1, 8)
                ])
            ]
        )
    }

    #[test]
    fn integer_literal_decimal_positive() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "+1234567",
            rule: Rule::integer_literal,
            tokens: [
                integer_literal(0, 8, [
                    positive_or_negative(0, 1),
                    integer_literal_decimal(1, 8)
                ])
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
                    integer_literal_hexadecimal(0, 18, [
                        hex_digits(2, 18)
                    ])
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
            tokens: [ integer_literal(0, 19, [
                positive_or_negative(0, 1),
                integer_literal_hexadecimal(1, 19, [
                    hex_digits(3, 19)
                ])
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
    fn boolean_literal_true() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "true",
            rule: Rule::boolean_literal,
            tokens: [ boolean_literal(0, 4) ]
        )
    }

    #[test]
    fn boolean_literal_false() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "false",
            rule: Rule::boolean_literal,
            tokens: [ boolean_literal(0, 5) ]
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
            tokens: [ name_literal(0, 65, [
                name_inner(1, 64)
            ])]
        )
    }

    #[test]
    fn name_literal_empty() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "''",
            rule: Rule::name_literal,
            tokens: [
                name_literal(0, 2, [
                    name_inner(1, 1)
                ])
            ]
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
                    single_quoted_string(10, 23, [
                        single_quoted_string_inner(11, 22)
                    ])
                ])
            ]
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
                    integer_literal(5, 6, [ integer_literal_decimal(5, 6) ]),
                    integer_literal(8, 9, [ integer_literal_decimal(8, 9) ]),
                    integer_literal(11, 12, [ integer_literal_decimal(11, 12) ])
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
            tokens: [ integer_literal(0, 4, [ integer_literal_decimal(0, 4) ]) ]
        )
    }

    #[test]
    fn numeric_literal_float() {
        parses_to! (
            parser: UnrealScriptParser,
            input: "1234.5678",
            rule: Rule::numeric_literal,
            tokens: [ float_literal(0, 9) ]
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
                    float_literal(5, 8),
                    float_literal(10, 13),
                    float_literal(15, 18)
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
                    integer_literal(4, 5, [ integer_literal_decimal(4, 5) ]),
                    integer_literal(7, 8, [ integer_literal_decimal(7, 8) ]),
                    integer_literal(10, 11, [ integer_literal_decimal(10, 11) ])
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
                    float_literal(4, 7),
                    float_literal(9, 12),
                    float_literal(14, 17)
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
            tokens: [
                class_declaration(0, 10, [ unqualified_identifier(6, 9) ])
            ]
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
                    class_extends(10, 21, [ identifier(18, 21) ])
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
                    class_extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 30), // abstract
                    class_modifier(31, 42), // cacheexempt
                    class_modifier(43, 52), // instanced
                    class_modifier(53, 64), // paseconfig
                    class_modifier(65, 80), // perobjectconfig
                    class_modifier(81, 92), // safereplace
                    class_modifier(93, 102), // transient
                    class_modifier(103, 121), // collapsecategories
                    class_modifier(122, 144), // dontcollapsecategories
                    class_modifier(145, 158), // editinlinenew
                    class_modifier(159, 175), // noteditinlinenew
                    class_modifier(176, 188), // hidedropdown
                    class_modifier(189, 198), // placeable
                    class_modifier(199, 211), // notplaceable
                    class_modifier(212, 225), // exportstructs
                    class_modifier(226, 235), // intrinsic
                    class_modifier(236, 242), // native
                    class_modifier(243, 260), // nativereplication
                    class_modifier(261, 269) // noexport
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_config_modifer() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar config(Baz);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 34, [
                    unqualified_identifier(6, 9),
                    class_extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 33, [ unqualified_identifier(29, 32) ])
                ])
            ]
        )
    }

    #[test]
    fn class_declaration_dependson_modifer() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class Foo extends Bar dependson(Baz);",
            rule: Rule::class_declaration,
            tokens: [
                class_declaration(0, 37, [
                    unqualified_identifier(6, 9),
                    class_extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 36, [ identifier(32, 35) ])
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
                    class_extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 38, [
                        integer_literal(27, 28, [ integer_literal_decimal(27, 28) ]),
                        integer_literal(30, 31, [ integer_literal_decimal(30, 31) ]),
                        integer_literal(33, 34, [ integer_literal_decimal(33, 34) ]),
                        integer_literal(36, 37, [ integer_literal_decimal(36, 37) ]),
                    ])
                ])
            ]
        )
    }

    #[test]
    fn unqualified_identifier_list_multiple() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo, Bar, Baz",
            rule: Rule::unqualified_identifier_list,
            tokens: [
                unqualified_identifier_list(0, 13, [
                    unqualified_identifier(0, 3),
                    unqualified_identifier(5, 8),
                    unqualified_identifier(10, 13)
                ])
            ]
        )
    }

    #[test]
    fn unqualified_identifier_list_single() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "Foo",
            rule: Rule::unqualified_identifier_list,
            tokens: [
                unqualified_identifier_list(0, 3, [ unqualified_identifier(0, 3) ])
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
                    class_extends(10, 21, [ identifier(18, 21) ]),
                    class_modifier(22, 51, [
                        unqualified_identifier_list(37, 50, [
                            unqualified_identifier(37, 40),
                            unqualified_identifier(42, 45),
                            unqualified_identifier(47, 50)
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
                    var_name(8, 11, [ unqualified_identifier(8, 11) ])
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
            tokens: [
                array_type(0, 10, [ type_(6, 9, [ identifier(6, 9) ]) ])
            ]
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
                    array_type(6, 16, [type_(12, 15, [ identifier(12, 15)])])
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
            tokens: [
                class_type(0, 14, [ identifier(6, 13) ])
            ]
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
                        integer_literal(5, 6, [ integer_literal_decimal(5, 6) ]),
                        integer_literal(8, 9, [ integer_literal_decimal(8, 9) ]),
                        integer_literal(11, 12, [ integer_literal_decimal(11, 12) ]),
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
                        integer_literal(4, 5, [ integer_literal_decimal(4, 5) ]),
                        integer_literal(7, 8, [ integer_literal_decimal(7, 8) ]),
                        integer_literal(10, 11, [ integer_literal_decimal(10, 11) ]),
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
                literal(0, 2, [
                    integer_literal(0, 2, [ integer_literal_decimal(0, 2) ])
                ])
            ]
        )
    }

    #[test]
    fn literal_string_literal() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "\"Foo\"",
            rule: Rule::literal,
            tokens: [
                literal(0, 5, [
                    string_literal(0, 5, [
                        string_literal_inner(1, 4)
                    ])
                ])
            ]
        )
    }

    #[test]
    fn literal_boolean_literal() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "true",
            rule: Rule::literal,
            tokens: [
                literal(0, 4, [
                    boolean_literal(0, 4)
                ])
            ]
        )
    }

    #[test]
    fn literal_none_literal() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "none",
            rule: Rule::literal,
            tokens: [
                literal(0, 4, [
                    none(0, 4)
                ])
            ]
        )
    }

    #[test]
    fn literal_name_literal() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "'Foo_Bar'",
            rule: Rule::literal,
            tokens: [
                literal(0, 9, [
                    name_literal(0, 9, [
                        name_inner(1, 8)
                    ])
                ])
            ]
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
                        single_quoted_string(10, 23, [
                            single_quoted_string_inner(11, 22)
                        ])
                    ])
                ])
            ]
        )
    }

    #[test]
    fn none() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "none",
            rule: Rule::none,
            tokens: [ none(0, 4) ]
        )
    }

    #[test]
    fn self_() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "self",
            rule: Rule::self_,
            tokens: [ self_(0, 4) ]
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
                    literal(12, 14, [ integer_literal(12, 14, [integer_literal_decimal(12, 14)]) ])
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
                var_size(0, 5, [
                    unqualified_identifier(1, 4)
                ])
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
                var_name(0, 3, [
                    unqualified_identifier(0, 3)
                ])
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
                        var_size(54, 58, [ integer_literal(55, 57, [ integer_literal_decimal(55, 57) ]) ])
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
                    unqualified_identifier_list(11, 19, [
                        unqualified_identifier(11, 14),
                        unqualified_identifier(16, 19),
                    ])
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
                    unqualified_identifier_list(11, 19, [
                        unqualified_identifier(11, 14),
                        unqualified_identifier(16, 19),
                    ])
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
                    unqualified_identifier(7, 10)
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
                    var_name(8, 9, [ unqualified_identifier(8, 9) ])
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
                    var_name(10, 11, [ unqualified_identifier(10, 11) ])
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
                    unqualified_identifier(24, 27)
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
                    struct_extends(11, 22, [ identifier(19, 22) ])
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
                        var_name(21, 24, [ unqualified_identifier(21, 24) ])
                    ]),
                    struct_var_declaration(26, 41, [
                        type_(30, 36, [ identifier(30, 36) ]),
                        var_name(37, 40, [ unqualified_identifier(37, 40) ])
                    ]),
                ])
            ]
        }
    }

    #[test]
    fn function_declaration_minimal() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "function Foo();",
            rule: Rule::function_declaration,
            tokens: [
                function_declaration(0, 15, [
                    function_type(0, 8),
                    unqualified_identifier(9, 12)
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
                    var_name(4, 7, [ unqualified_identifier(4, 7) ])
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
                    function_type(0, 8),
                    unqualified_identifier(9, 12)
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
                    function_type(0, 8),
                    type_(9, 12, [ identifier(9, 12) ]),
                    unqualified_identifier(13, 16)
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
                    function_type(0, 8),
                    unqualified_identifier(9, 12),
                    function_argument(13, 23, [
                        type_(13, 19, [ identifier(13, 19) ]),
                        var_name(20, 23, [ unqualified_identifier(20, 23) ])
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
    fn reliability_reliable() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "reliable",
            rule: Rule::reliability,
            tokens: [ reliability(0, 8) ]
        }
    }

    #[test]
    fn reliability_unreliable() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "unreliable",
            rule: Rule::reliability,
            tokens: [ reliability(0, 10) ]
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
            input: "reliable if (true) Foo;",
            rule: Rule::replication_statement,
            tokens: [
                replication_statement(0, 23, [
                    reliability(0, 8),
                    expression(13, 17, [ target(13, 17, [ literal(13, 17, [boolean_literal(13, 17)]) ]) ]),
                    unqualified_identifier(19, 22)
                ])
            ]
        }
    }
    #[test]
    fn replication_statement_multiple_variables() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "reliable if (true) Foo, Bar;",
            rule: Rule::replication_statement,
            tokens: [
                replication_statement(0, 28, [
                    reliability(0, 8),
                    expression(13, 17, [ target(13, 17, [ literal(13, 17, [boolean_literal(13, 17)]) ]) ]),
                    unqualified_identifier(19, 22),
                    unqualified_identifier(24, 27)
                ])
            ]
        }
    }
}
