use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "UnrealScript.pest"]
pub struct UnrealScriptParser;

use pest_consume::{Error, match_nodes};
use std::str::FromStr;

use std::rc::Rc;

use crate::ast::*;
use crate::lint::Lint;
use crate::symbols::SymbolTable;
use crate::visitor::{Visit, Visitor};
pub use t3d::{ParseMode, ParseOptions};

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

pub type ParserRule = Rule;

impl From<&Node<'_>> for AstSpan {
    fn from(node: &Node<'_>) -> Self {
        AstSpan {
            start: node.as_pair().as_span().start(),
            end: node.as_pair().as_span().end(),
            line: None,
        }
    }
}

impl From<&[Node<'_>]> for AstSpan {
    fn from(nodes: &[Node<'_>]) -> Self {
        AstSpan {
            start: nodes
                .iter()
                .map(|n| n.as_pair().as_span().start())
                .min()
                .unwrap(),
            end: nodes
                .iter()
                .map(|n| n.as_pair().as_span().end())
                .max()
                .unwrap(),
            line: None,
        }
    }
}

macro_rules! match_nodes_any {
    ($nodes:expr; $($f:ident($v:ident) => $e:expr),*) => (
        for node in $nodes {
            match node.as_rule() {
                $(
                    Rule::$f => {
                        let $v = Self::$f(node)?;   // TODO: exclude this if the var isn't defined!
                        $e
                    }
                )+
                _ => { return Err(node.error("Unexpected rule")) }
            }
        }
    )
}

/// The precedence of a dyadic operator: an expression is split at its
/// operator with the highest value, so higher values bind more loosely.
// TODO: precedence relies on the TYPE also!
// TODO: all of these should be read from Object.uc before proper parsing, so
// that we don't need to have this table at all.
fn precedence(verb: &DyadicVerb) -> u8 {
    match verb {
        DyadicVerb::Exponentiate => 12,
        DyadicVerb::Multiply | DyadicVerb::Divide | DyadicVerb::Dot | DyadicVerb::Cross => 16,
        DyadicVerb::Modulo => 18,
        DyadicVerb::Add | DyadicVerb::Subtract => 20,
        DyadicVerb::ShiftLeft | DyadicVerb::ShiftRight | DyadicVerb::DoubleShiftRight => 22,
        DyadicVerb::CompareLessThan
        | DyadicVerb::CompareGreaterThan
        | DyadicVerb::CompareLessThanOrEqual
        | DyadicVerb::CompareGreaterThanOrEqual
        | DyadicVerb::CompareEqual
        | DyadicVerb::CompareApproximatelyEqual
        | DyadicVerb::ClockwiseFrom => 24,
        DyadicVerb::CompareNotEqual
        | DyadicVerb::BitwiseAnd
        | DyadicVerb::BitwiseExclusiveOr
        | DyadicVerb::BitwiseOr => 28,
        DyadicVerb::LogicalAnd | DyadicVerb::LogicalExclusiveOr => 30,
        DyadicVerb::LogicalOr => 32,
        DyadicVerb::MultiplyAssign | DyadicVerb::DivideAssign | DyadicVerb::AddAssign => 34,
        DyadicVerb::Concatenate | DyadicVerb::ConcatenateSpace => 40,
        DyadicVerb::ConcatenateAssign | DyadicVerb::ConcatenateSpaceAssign => 44,
        DyadicVerb::SubtractAssign => 45,
        DyadicVerb::Assign => 46, // TODO: just a guess
    }
}

/// The identifier spelled by `node`'s text.
fn identifier_of(node: &Node) -> AstNode<Identifier> {
    AstNode {
        span: AstSpan::from(node),
        data: Identifier {
            string: node.as_str().to_string(),
        },
    }
}

/// The type named by a `class_type` node (`class<Name>`).
fn class_type_of(node: &Node) -> Type {
    Type::Class(identifier_of(&only_child(node)))
}

/// The only child of `node`, for rules the grammar gives exactly one child.
fn only_child<'i>(node: &Node<'i>) -> Node<'i> {
    node.clone()
        .into_children()
        .single()
        .expect("the grammar gives this rule exactly one child")
}

fn parse_target(nodes: &[Node]) -> Result<Box<AstNode<Expression>>> {
    let (node, remaining) = nodes.split_last().unwrap();
    // The whole target, including any operand before the last node (e.g.
    // `Foo` in `Foo(1)`).
    let span = AstSpan::from(nodes);
    let operand = |remaining: &[Node]| match remaining.is_empty() {
        true => Ok(None),
        false => parse_expression_from_nodes(remaining).map(Some),
    };
    match node.as_rule() {
        Rule::unqualified_identifier => Ok(Box::new(AstNode {
            span,
            data: Expression::Identifier(identifier_of(node)),
        })),
        Rule::new => {
            let mut inner_iter = node.clone().into_children().into_iter();
            let first = inner_iter.next().unwrap();
            if let Rule::expression_list = first.as_rule() {
                Ok(Box::new(AstNode {
                    span,
                    data: Expression::New {
                        arguments: Some(UnrealScriptParser::expression_list(first)?),
                        type_: UnrealScriptParser::expression(Node::new(
                            inner_iter.next().unwrap().clone().into_pair(),
                        ))?,
                    },
                }))
            } else {
                Ok(Box::new(AstNode {
                    span,
                    data: Expression::New {
                        arguments: None,
                        type_: UnrealScriptParser::expression(Node::new(
                            first.clone().into_pair(),
                        ))?,
                    },
                }))
            }
        }
        Rule::cast => {
            let mut inner_iter = node.clone().into_children().into_iter();
            Ok(Box::new(AstNode {
                span,
                data: Expression::Cast {
                    type_: class_type_of(&inner_iter.next().unwrap()),
                    operand: UnrealScriptParser::expression(inner_iter.next().unwrap())?,
                },
            }))
        }
        // NOTE: these are required because integer_literal and float_literals can be captured
        // before monadic prefix verbs.
        Rule::numeric_literal => Ok(Box::new(AstNode {
            span,
            data: Expression::Literal(AstNode {
                span,
                data: Literal::Numeric(UnrealScriptParser::numeric_literal(Node::new(
                    node.clone().into_pair(),
                ))?),
            }),
        })),
        Rule::literal => Ok(Box::new(AstNode {
            span,
            data: Expression::Literal(UnrealScriptParser::literal(Node::new(
                node.clone().into_pair(),
            ))?),
        })),
        Rule::default_access => Ok(Box::new(AstNode {
            span,
            data: Expression::DefaultAccess {
                operand: operand(remaining)?,
                target: identifier_of(&only_child(node)),
            },
        })),
        Rule::static_access => Ok(Box::new(AstNode {
            span,
            data: Expression::StaticAccess {
                operand: operand(remaining)?,
                target: identifier_of(&only_child(node)),
            },
        })),
        Rule::global_call => {
            let mut inner_iter = node.clone().into_children().into_iter();
            let name = identifier_of(&inner_iter.next().unwrap());
            let arguments = UnrealScriptParser::expression_list(inner_iter.next().unwrap())?;
            Ok(Box::new(AstNode {
                span,
                data: Expression::GlobalCall { name, arguments },
            }))
        }
        Rule::parenthetical_expression => {
            let children: Vec<Node> = only_child(node).into_children().into_iter().collect();
            Ok(Box::new(AstNode {
                span,
                data: Expression::ParentheticalExpression(parse_expression_from_nodes(
                    &children[..],
                )?),
            }))
        }
        Rule::call => Ok(Box::new(AstNode {
            span,
            data: Expression::Call {
                operand: parse_expression_from_nodes(remaining)?,
                arguments: UnrealScriptParser::expression_list(only_child(node))?,
            },
        })),
        Rule::array_access => Ok(Box::new(AstNode {
            span,
            data: Expression::ArrayAccess {
                operand: parse_expression_from_nodes(remaining)?,
                argument: UnrealScriptParser::expression(only_child(node))?,
            },
        })),
        // The only other rule a target can end with is `member_access`.
        _ => Ok(Box::new(AstNode {
            span,
            data: Expression::MemberAccess {
                operand: parse_expression_from_nodes(remaining)?,
                target: identifier_of(&only_child(node)),
            },
        })),
    }
}

fn parse_expression_from_nodes(nodes: &[Node]) -> Result<Box<AstNode<Expression>>> {
    let mut dyadic_verbs = Vec::new();
    for (index, node) in nodes.iter().enumerate() {
        if let Rule::dyadic_verb = node.as_rule() {
            // The grammar accepts any identifier as an operator, such as a
            // user-defined one; only the known ones can be parsed.
            let verb = DyadicVerb::from_str(node.as_str().to_lowercase().as_str())
                .map_err(|_| node.error("Encountered unregistered dyadic verb"))?;
            dyadic_verbs.push((precedence(&verb), index, verb));
        }
    }
    // Split the expression at the operator with the highest precedence value
    // (the first one, if there's a tie; the sort is stable).
    // TODO: secondarily, sort by order!
    dyadic_verbs.sort_by(|a, b| b.0.cmp(&a.0));
    if let Some((_, index, verb)) = dyadic_verbs.into_iter().next() {
        return Ok(Box::new(AstNode {
            span: AstSpan::from(nodes),
            data: Expression::DyadicExpression {
                lhs: parse_expression_from_nodes(&nodes[..index])?,
                verb,
                rhs: parse_expression_from_nodes(&nodes[index + 1..])?,
            },
        }));
    }
    // Next, search for monadic verbs at the beginning and end of the expression
    if let Rule::monadic_post_verb = nodes.last().unwrap().as_rule() {
        return Ok(Box::new(AstNode {
            span: AstSpan::from(nodes),
            data: Expression::MonadicPostExpression {
                verb: MonadicVerb::from_str(nodes.last().unwrap().as_str()).unwrap(),
                operand: parse_target(&nodes[..nodes.len() - 1])?,
            },
        }));
    }
    if let Rule::monadic_pre_verb = nodes.first().unwrap().as_rule() {
        return Ok(Box::new(AstNode {
            span: AstSpan::from(nodes),
            data: Expression::MonadicPreExpression {
                verb: MonadicVerb::from_str(nodes.first().unwrap().as_str()).unwrap(),
                operand: parse_target(&nodes[1..])?,
            },
        }));
    }
    parse_target(nodes)
}

#[pest_consume::parser]
impl UnrealScriptParser {
    fn cast(input: Node) -> Result<Expression> {
        match_nodes!(input.into_children();
            [class_type(type_), expression(operand)] => Ok(Expression::Cast { type_, operand })
        )
    }

    fn parenthetical_expression(input: Node) -> Result<Expression> {
        match_nodes!(input.into_children();
            [expression(expression)] => Ok(Expression::ParentheticalExpression(expression))
        )
    }

    fn none_literal(input: Node) -> Result<Literal> {
        Ok(Literal::None)
    }

    fn float_literal(input: Node) -> Result<AstNode<NumericLiteral>> {
        let value = malformed_float_prefix(input.as_str())
            .to_lowercase()
            .trim_end_matches("f")
            .parse::<f32>()
            // Out of range floats parse as infinity, not an error.
            .expect("the grammar only matches valid floats");
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: NumericLiteral::Float(value),
        })
    }

    fn single_quoted_string(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn name_literal(input: Node) -> Result<Literal> {
        Ok(Literal::Name(only_child(&input).as_str().to_string()))
    }

    fn string_literal(input: Node) -> Result<Literal> {
        Ok(Literal::String(input.as_str().to_string()))
    }

    fn hex_digits(input: Node) -> Result<i32> {
        u32::from_str_radix(input.as_str(), 16)
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(i32::from_be_bytes(v.to_be_bytes())))
    }

    fn integer_literal_hexadecimal(input: Node) -> Result<i32> {
        match_nodes!(input.into_children();
            [hex_digits(n)] => Ok(n)
        )
    }

    fn integer_literal_decimal(input: Node) -> Result<i32> {
        input.as_str().parse::<i32>().map_err(|e| input.error(e))
    }

    fn numeric_sign(input: Node) -> Result<i32> {
        return if input.as_str() == "-" { Ok(-1) } else { Ok(1) };
    }

    fn integer_literal(input: Node) -> Result<AstNode<NumericLiteral>> {
        let mut sign = 1;
        let mut integer = 0;
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            numeric_sign(s) => sign = s,
            integer_literal_hexadecimal(s) => integer = s,
            integer_literal_decimal(s) => integer = s
        );
        Ok(AstNode {
            span,
            data: NumericLiteral::Integer(sign * integer),
        })
    }

    fn unqualified_identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(identifier_of(&input))
    }

    fn qualified_identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: Identifier {
                string: input.as_str().to_string(),
            },
        })
    }

    fn identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: Identifier {
                string: input.as_str().to_string(),
            },
        })
    }

    fn class_within(input: Node) -> Result<AstNode<Identifier>> {
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(id)
        )
    }

    fn object_literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [unqualified_identifier(type_), single_quoted_string(reference)] => {
                return Ok(Literal::Object { type_, reference })
            }
        );
    }

    fn numeric_literal(input: Node) -> Result<AstNode<NumericLiteral>> {
        match_nodes!(input.into_children();
            [integer_literal(n)] => return Ok(n),
            [float_literal(f)] => return Ok(f)
        )
    }

    fn vector_literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [numeric_literal(x), numeric_literal(y), numeric_literal(z)] => {
                return Ok(Literal::Vector([x, y, z]))
            }
        )
    }

    fn rotator_literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [numeric_literal(pitch), numeric_literal(yaw), numeric_literal(roll)] => {
                return Ok(Literal::Rotator([pitch, yaw, roll]))
            }
        )
    }

    fn compiler_directive_inner(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn compiler_directive(input: Node) -> Result<CompilerDirective> {
        match_nodes!(input.into_children();
            [compiler_directive_inner(command)] => Ok(CompilerDirective { command })
        )
    }

    fn extends(input: Node) -> Result<AstNode<Identifier>> {
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(id)
        )
    }

    fn class_modifier_type(input: Node) -> Result<AstNode<ClassModifierType>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: ClassModifierType::from_str(input.as_str().to_lowercase().as_str()).unwrap(),
        })
    }

    fn class_modifier(input: Node) -> Result<AstNode<ClassModifier>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [class_modifier_type(type_), expression_list(arguments)] => {
                Ok(AstNode { span, data: ClassModifier { type_, arguments: Some(arguments) } })
            },
            [class_modifier_type(type_)] => {
                Ok(AstNode { span, data: ClassModifier { type_, arguments: None } })
            }
        )
    }

    fn var_category(input: Node) -> Result<String> {
        match_nodes!(input.into_children();
            [unqualified_identifier(id)] => Ok(id.data.string),
            // we still want to mark the category as present, even if it's empty
            [] => Ok(String::new())
        )
    }

    // https://stackoverflow.com/questions/69612563/pattern-matching-in-rust-macro
    fn struct_var_editable(_input: Node) -> Result<()> {
        Ok(())
    }

    fn struct_var_modifier(input: Node) -> Result<StructVarModifier> {
        Ok(StructVarModifier::from_str(input.as_str()).unwrap())
    }

    fn struct_var_declaration(input: Node) -> Result<AstNode<StructVarDeclaration>> {
        let mut is_editable = false;
        let mut modifiers = Vec::new();
        let mut type_: Option<Type> = None;
        let mut names = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            struct_var_editable(_e) => is_editable = true,
            struct_var_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => names.push(n)
        );
        Ok(AstNode {
            span,
            data: StructVarDeclaration {
                names,
                type_: type_.unwrap(),
                modifiers,
                is_editable,
            },
        })
    }

    fn struct_modifier(input: Node) -> Result<AstNode<StructModifier>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: StructModifier::from_str(input.as_str()).unwrap(),
        })
    }

    fn struct_declaration(input: Node) -> Result<AstNode<StructDeclaration>> {
        let mut modifiers = Vec::new();
        let mut parent: Option<AstNode<Identifier>> = None;
        let mut name: Option<AstNode<Identifier>> = None;
        let mut members = Vec::new();
        let mut cpp = None;
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            struct_modifier(m) => modifiers.push(m),
            unqualified_identifier(id) => name = Some(id),
            extends(e) => parent = Some(e),
            struct_var_declaration(v) => members.push(v),
            cppstruct(s) => cpp = Some(s)
        );
        Ok(AstNode {
            span,
            data: StructDeclaration {
                modifiers,
                name: name.unwrap(),
                parent,
                members,
                cpp,
            },
        })
    }

    fn enum_declaration(input: Node) -> Result<AstNode<EnumDeclaration>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(name), unqualified_identifier(values)..] => {
                Ok(AstNode {
                    span,
                    data: EnumDeclaration {
                        name,
                        values: values.collect()
                    }
                })
            }
        )
    }

    fn array_type(input: Node) -> Result<Type> {
        match_nodes!(input.into_children();
            [type_(t)] => Ok(Type::Array(Box::new(t)))
        )
    }

    fn class_type(input: Node) -> Result<Type> {
        Ok(class_type_of(&input))
    }

    fn type_(input: Node) -> Result<Type> {
        match_nodes!(input.into_children();
            [struct_declaration(s)] => Ok(Type::Struct(s)),
            [enum_declaration(e)] => Ok(Type::Enum(e)),
            [array_type(a)] => Ok(a),
            [class_type(c)] => Ok(c),
            [identifier(i)] => Ok(Type::from(i.data))
        )
    }

    // fn constant(input: Node) -> Result<Constant> {
    //     match_nodes!(input.into_children();
    //         [literal(l)] => Ok(Constant::Literal(l)),
    //         [identifier(id)] => Ok(Constant::Identifier(id))
    //     )
    // }

    fn var_size(input: Node) -> Result<AstNode<VarSize>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(id)] => Ok(AstNode { span, data: VarSize::Identifier(id) }),
            [integer_literal(n)] => Ok(AstNode { span, data: VarSize::IntegerLiteral(n) })
        )
    }

    fn var_name(input: Node) -> Result<AstNode<VarName>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(identifier)] => {
                Ok(AstNode { span, data: VarName { identifier, size: None } })
            },
            [unqualified_identifier(identifier), var_size(size)] => {
                Ok(AstNode { span, data: VarName { identifier, size: Some(size) } })
            }
        )
    }

    fn var_modifier(input: Node) -> Result<AstNode<VarModifier>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: VarModifier::from_str(input.as_str()).unwrap(),
        })
    }

    fn var_declaration(input: Node) -> Result<AstNode<VarDeclaration>> {
        let mut category: Option<String> = None;
        let mut modifiers = Vec::new();
        let mut type_: Option<Type> = None;
        let mut names = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            var_category(c) => category = Some(c),
            var_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => names.push(n)
        );
        Ok(AstNode {
            span,
            data: VarDeclaration {
                category,
                modifiers,
                type_: type_.unwrap(),
                names,
            },
        })
    }

    fn class_declaration(input: Node) -> Result<AstNode<ClassDeclaration>> {
        let mut parent_class: Option<AstNode<Identifier>> = None;
        let mut modifiers: Vec<AstNode<ClassModifier>> = Vec::new();
        let mut within: Option<AstNode<Identifier>> = None;
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(name), nodes..] => {
                match_nodes_any!(nodes;
                    extends(e) => parent_class = Some(e),
                    class_within(w) => within = Some(w),
                    class_modifier(m) => modifiers.push(m)
                );
                Ok(AstNode {
                    span,
                    data: ClassDeclaration { name, parent_class, modifiers, within }
                })
            }
        )
    }

    fn literal(input: Node) -> Result<AstNode<Literal>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [none_literal(n)] => n,
                [boolean_literal(b)] => b,
                [vector_literal(v)] => v,
                [rotator_literal(r)] => r,
                [numeric_literal(n)] => Literal::Numeric(n),
                [string_literal(s)] => s,
                [name_literal(n)] => n,
                [object_literal(o)] => o,
            ),
        })
    }

    fn boolean_literal(input: Node) -> Result<Literal> {
        // The grammar only matches `true` and `false`, in any case.
        Ok(Literal::Boolean(
            input.as_str().eq_ignore_ascii_case("true"),
        ))
    }

    fn const_declaration(input: Node) -> Result<AstNode<ConstDeclaration>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(name), literal(value)] => {
                Ok(AstNode {
                    span,
                    data: ConstDeclaration {
                        name,
                        value
                    }
                })
            }
        )
    }

    fn function_modifier_type(input: Node) -> Result<FunctionModifierType> {
        Ok(FunctionModifierType::from_str(input.as_str().to_lowercase().as_str()).unwrap())
    }

    fn function_modifier(input: Node) -> Result<AstNode<FunctionModifier>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [function_modifier_type(type_), integer_literal(arguments)..] => {
                Ok(AstNode { span, data: FunctionModifier { type_, arguments: arguments.collect() } })
            }
        )
    }

    fn function_type_no_arguments_type(input: Node) -> Result<FunctionTypeType> {
        Ok(FunctionTypeType::from_str(input.as_str())
            .expect("the grammar only matches function type keywords"))
    }

    fn function_type_with_arguments_type(input: Node) -> Result<FunctionTypeType> {
        Ok(FunctionTypeType::from_str(input.as_str()).unwrap())
    }

    fn function_type_with_arguments(input: Node) -> Result<FunctionType> {
        match_nodes!(input.into_children();
            [function_type_with_arguments_type(type_), integer_literal(arguments)..] => {
                Ok(FunctionType { type_, arguments: Some(arguments.collect()) })
            }
        )
    }

    fn function_type(input: Node) -> Result<FunctionType> {
        match_nodes!(input.into_children();
            [function_type_no_arguments_type(type_)] => Ok(FunctionType { type_, arguments: None }),
            [function_type_with_arguments(t)] => Ok(t)
        )
    }

    fn function_name(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: Identifier::new(input.as_str()),
        })
    }

    fn function_argument_modifier(input: Node) -> Result<FunctionArgumentModifier> {
        Ok(FunctionArgumentModifier::from_str(input.as_str()).unwrap())
    }

    fn function_argument(input: Node) -> Result<AstNode<FunctionArgument>> {
        let mut modifiers = Vec::new();
        let mut type_: Option<Type> = None;
        let mut name = None;
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            function_argument_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => name = Some(n)
        );
        Ok(AstNode {
            span,
            data: FunctionArgument {
                modifiers,
                type_: type_.unwrap(),
                name: name.unwrap(),
            },
        })
    }

    fn local_declaration(input: Node) -> Result<AstNode<LocalDeclaration>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [type_(type_), var_name(names)..] => {
                Ok(AstNode { span, data: LocalDeclaration { type_, names: names.collect() } })
            }
        )
    }

    fn jump_label(input: Node) -> Result<AstNode<Identifier>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(identifier)] => Ok(identifier)
        )
    }

    fn if_statement(input: Node) -> Result<IfStatement> {
        match_nodes!(input.into_children();
            [expression(predicate), code_statement_or_block(body)] => {
                Ok(IfStatement { predicate, body: Some(body) })
            }
        )
    }

    fn elif_statement(input: Node) -> Result<ElifStatement> {
        match_nodes!(input.into_children();
            [expression(predicate), code_statement_or_block(body)] => {
                Ok(ElifStatement { predicate, body })
            }
        )
    }

    fn else_statement(input: Node) -> Result<ElseStatement> {
        match_nodes!(input.into_children();
            [code_statement_or_block(body)] => Ok(ElseStatement { body })
        )
    }

    fn if_statement_empty(input: Node) -> Result<IfStatement> {
        match_nodes!(input.into_children();
            [expression(predicate)] => Ok(IfStatement {
                predicate, body: None
            })
        )
    }

    fn conditional_statement(input: Node) -> Result<ConditionalStatement> {
        let mut if_statement = None;
        let mut elif_statements = Vec::new();
        let mut else_statement = None;
        match_nodes_any!(input.into_children();
            if_statement_empty(i) => if_statement = Some(i),
            if_statement(i) => if_statement = Some(i),
            elif_statement(e) => elif_statements.push(e),
            else_statement(e) => else_statement = Some(e)
        );
        Ok(ConditionalStatement {
            if_statement: if_statement.unwrap(),
            elif_statements,
            else_statement,
        })
    }

    fn foreach_expression(input: Node) -> Result<Box<AstNode<Expression>>> {
        // TODO: we get an error here because the node rules don't match
        let nodes: Vec<Node> = input.into_children().collect();
        parse_expression_from_nodes(&nodes[..])
    }

    fn foreach_statement(input: Node) -> Result<ForEach> {
        match_nodes!(input.into_children();
            [foreach_expression(predicate), code_statement_or_block(body)] => {
                Ok(ForEach { predicate, body })
            }
        )
    }

    fn for_init(input: Node) -> Result<Box<AstNode<Expression>>> {
        match_nodes!(input.into_children(); [expression(e)] => Ok(e) )
    }

    fn for_predicate(input: Node) -> Result<Box<AstNode<Expression>>> {
        match_nodes!(input.into_children(); [expression(e)] => Ok(e) )
    }

    fn for_post(input: Node) -> Result<Box<AstNode<Expression>>> {
        match_nodes!(input.into_children(); [expression(e)] => Ok(e) )
    }

    fn for_statement(input: Node) -> Result<CodeStatement> {
        let mut init = None;
        let mut predicate = None;
        let mut post = None;
        let mut body = None;
        match_nodes_any!(input.into_children();
            for_init(i) => init = Some(i),
            for_predicate(p) => predicate = Some(p),
            for_post(p) => post = Some(p),
            code_statement_or_block(b) => body = Some(b)
        );
        Ok(CodeStatement::For(Box::new(ForStatement {
            init,
            predicate,
            post,
            body: body.unwrap(),
        })))
    }

    fn while_statement(input: Node) -> Result<CodeStatement> {
        match_nodes!(input.into_children();
            [expression(predicate), code_statement_or_block(body)] => {
                Ok(CodeStatement::While(Box::new(WhileStatement { predicate, body })))
            }
        )
    }

    fn do_until_statement(input: Node) -> Result<DoUntil> {
        let mut body = None;
        let mut predicate = None;
        match_nodes_any!(input.into_children();
            code_statement_or_block(b) => body = Some(b),
            expression(e) => predicate = Some(e)
        );
        Ok(DoUntil {
            body: body.unwrap(),
            predicate,
        })
    }

    fn return_statement(input: Node) -> Result<CodeStatement> {
        let mut expression = None;
        match_nodes_any!(input.into_children();
            expression(e) => expression = Some(e)
        );
        Ok(CodeStatement::Return(expression))
    }

    fn break_statement(_input: Node) -> Result<CodeStatement> {
        Ok(CodeStatement::Break)
    }

    fn continue_statement(_input: Node) -> Result<CodeStatement> {
        Ok(CodeStatement::Continue)
    }

    fn goto_statement(input: Node) -> Result<CodeStatement> {
        match_nodes!(input.into_children();
            [unqualified_identifier(label)] => {
                Ok(CodeStatement::Goto(label))
            }
        )
    }

    fn switch_case_label(input: Node) -> Result<SwitchCaseType> {
        match_nodes!(input.into_children();
            [expression(e)] => Ok(SwitchCaseType::Expression(e))
        )
    }

    fn switch_default_case(input: Node) -> Result<SwitchCase> {
        match_nodes!(input.into_children();
            [code_statement(statements)..] => Ok(SwitchCase { type_: SwitchCaseType::Default, statements: statements.collect() })
        )
    }

    fn switch_case(input: Node) -> Result<SwitchCase> {
        match_nodes!(input.into_children();
            [switch_case_label(label), code_statement(statements)..] => {
                Ok(SwitchCase { type_: label, statements: statements.collect() })
            }
        )
    }

    fn switch_statement(input: Node) -> Result<SwitchStatement> {
        match_nodes!(input.into_children();
            [expression(predicate), nodes..] => {
                let mut cases = Vec::new();
                match_nodes_any!(nodes;
                    switch_case(c) => cases.push(c),
                    switch_default_case(c) => cases.push(c)
                );
                Ok(SwitchStatement { predicate, cases })
            }
        )
    }

    fn code_block(input: Node) -> Result<AstNode<CodeBlock>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [code_statement(statements)..] => Ok(AstNode { span, data: CodeBlock { statements: statements.collect() }})
        )
    }

    fn code_statement_or_block(input: Node) -> Result<AstNode<CodeStatementOrBlock>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [code_statement(s)] => Ok(AstNode { span, data: CodeStatementOrBlock::CodeStatement(s) }),
            [code_block(b)] => Ok(AstNode { span, data: CodeStatementOrBlock::CodeBlock(b) })
        )
    }

    fn statement_empty(_input: Node) -> Result<CodeStatement> {
        Ok(CodeStatement::Empty)
    }

    fn code_statement(input: Node) -> Result<AstNode<CodeStatement>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [jump_label(v)] => CodeStatement::JumpLabel(v),
                [conditional_statement(v)] => CodeStatement::Conditional(Box::new(v)),
                [foreach_statement(v)] => CodeStatement::ForEach(Box::new(v)),
                [for_statement(v)] => v,
                [while_statement(v)] => v,
                [do_until_statement(v)] => CodeStatement::DoUntil(Box::new(v)),
                [return_statement(v)] => v,
                [break_statement(v)] => v,
                [continue_statement(v)] => v,
                [goto_statement(v)] => v,
                [compiler_directive(v)] => CodeStatement::CompilerDirective(v),
                [switch_statement(v)] => CodeStatement::Switch(Box::new(v)),
                [const_declaration(v)] => CodeStatement::ConstDeclaration(v),
                [expression(v)] => CodeStatement::Expression(v),
                [statement_empty(_v)] => CodeStatement::Empty
            ),
        })
    }

    fn function_body(input: Node) -> Result<AstNode<FunctionBody>> {
        let mut statements = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            const_declaration(c) => statements.push(FunctionBodyStatement::ConstDeclaration(c)),
            local_declaration(l) => statements.push(FunctionBodyStatement::LocalDeclaration(l)),
            // stray semicolons between local declarations (e.g. `local int A;;`)
            statement_empty(_e) => {},
            code_statement(c) => statements.push(FunctionBodyStatement::CodeStatement(c))
        );
        Ok(AstNode {
            span,
            data: FunctionBody { statements },
        })
    }

    fn function_declaration(input: Node) -> Result<AstNode<FunctionDeclaration>> {
        let mut modifiers = Vec::new();
        let mut types = Vec::new();
        let mut return_type = None;
        let mut arguments = Vec::new();
        let mut name = None;
        let mut body = None;
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            function_modifier(m) => modifiers.push(m),
            function_type(t) => types.push(t),
            type_(t) => return_type = Some(t),
            function_name(n) => name = Some(n),
            function_argument(a) => arguments.push(a),
            function_body(b) => body = Some(b)
        );
        Ok(AstNode {
            span,
            data: FunctionDeclaration {
                types,
                modifiers,
                return_type,
                arguments,
                name: name.unwrap(),
                body,
            },
        })
    }

    fn replication_reliability(input: Node) -> Result<ReplicationReliability> {
        Ok(ReplicationReliability::from_str(input.as_str()).unwrap())
    }

    fn replication_statement(input: Node) -> Result<ReplicationStatement> {
        match_nodes!(input.into_children();
            [replication_reliability(reliability), expression(condition), unqualified_identifier(variables)..] => {
                Ok(ReplicationStatement { reliability, condition, variables: variables.collect() })
            }
        )
    }

    fn replication_block(input: Node) -> Result<AstNode<ReplicationBlock>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [replication_statement(statements)..] => {
                Ok(AstNode { span, data: ReplicationBlock { statements: statements.collect() } })
            }
        )
    }

    fn state_modifier(input: Node) -> Result<StateModifier> {
        Ok(StateModifier::from_str(input.as_str()).unwrap())
    }

    fn state_editable(_input: Node) -> Result<bool> {
        Ok(true)
    }

    fn state_ignores(input: Node) -> Result<Vec<AstNode<Identifier>>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(ids)..] => {
                Ok(ids.collect())
            }
        )
    }

    fn state_statement(input: Node) -> Result<StateStatement> {
        match_nodes!(input.into_children();
            [const_declaration(c)] => Ok(StateStatement::ConstDeclaration(c)),
            [function_declaration(f)] => Ok(StateStatement::FunctionDeclaration(f)),
        )
    }

    fn state_label(input: Node) -> Result<StateLabel> {
        match_nodes!(input.into_children();
            [unqualified_identifier(label), code_statement(statements)..] => {
                Ok(StateLabel { label, statements: statements.collect() })
            }
        )
    }

    fn state_declaration(input: Node) -> Result<StateDeclaration> {
        let mut modifiers = Vec::new();
        let mut is_editable = false;
        let mut name = None;
        let mut parent = None;
        let mut ignores = Vec::new();
        let mut statements = Vec::new();
        let mut labels = Vec::new();
        match_nodes_any!(input.into_children();
            state_modifier(m) => modifiers.push(m),
            state_editable(e) => is_editable = e,
            unqualified_identifier(i) => name = Some(i),
            extends(e) => parent = Some(e),
            state_ignores(i) => ignores = i,
            state_statement(s) => statements.push(s),
            state_label(l) => labels.push(l)
        );
        Ok(StateDeclaration {
            is_editable,
            modifiers,
            name: name.unwrap(),
            parent,
            ignores,
            statements,
            labels,
        })
    }

    fn defaultproperties_closing_brace(_input: Node) -> Result<()> {
        Ok(())
    }

    fn defaultproperties(input: Node) -> Result<AstNode<DefaultProperties>> {
        let span = AstSpan::from(&input);
        let mut children = input.into_children();
        let body = children
            .next()
            .expect("defaultproperties always has a body");
        let closed = children.next().is_some();
        let offset = body.as_span().start();
        // The block's contents are parsed permissively; whether recovered
        // problems are errors is decided by `parse_program_with`.
        let parsed = t3d::parse_properties(body.as_str(), &t3d::ParseOptions::default())
            .expect("permissive parsing never fails");
        let mut statements = parsed.value;
        statements.iter_mut().for_each(|s| s.offset_spans(offset));
        let diagnostics = parsed
            .diagnostics
            .into_iter()
            .map(|mut d| {
                d.span = d.span.offset(offset);
                d
            })
            .collect();
        Ok(AstNode {
            span,
            data: DefaultProperties {
                statements,
                diagnostics,
                closed,
            },
        })
    }

    fn cpp_body_outer(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn cpptext(input: Node) -> Result<String> {
        match_nodes!(input.into_children();
            [cpp_body_outer(b)] => Ok(b)
        )
    }

    fn cppstruct(input: Node) -> Result<String> {
        match_nodes!(input.into_children();
            [cpp_body_outer(b)] => Ok(b)
        )
    }

    fn program_statement(input: Node) -> Result<AstNode<ProgramStatement>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [statement_empty(_e)] => ProgramStatement::Empty,
                [compiler_directive(c)] => ProgramStatement::CompilerDirective(c),
                [const_declaration(c)] => ProgramStatement::ConstDeclaration(c),
                [var_declaration(v)] => ProgramStatement::VarDeclaration(Rc::new(v)),
                [enum_declaration(e)] => ProgramStatement::EnumDeclaration(e),
                [struct_declaration(s)] => ProgramStatement::StructDeclaration(s),
                [function_declaration(f)] => ProgramStatement::FunctionDeclaration(f),
                [replication_block(r)] => ProgramStatement::ReplicationBlock(r),
                [state_declaration(s)] => ProgramStatement::StateDeclaration(s),
                [defaultproperties(d)] => ProgramStatement::DefaultProperties(d),
                [cpptext(c)] => ProgramStatement::CppText(c),
            ),
        })
    }

    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    fn program_statement_pre(input: Node) -> Result<AstNode<ProgramStatement>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [compiler_directive(c)] => ProgramStatement::CompilerDirective(c),
                [const_declaration(c)] => ProgramStatement::ConstDeclaration(c)
            ),
        })
    }

    fn program(input: Node) -> Result<Program> {
        let mut statements = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            program_statement_pre(s) => statements.push(s),
            program_statement(s) => statements.push(s),
            class_declaration(c) => statements.push(AstNode {
                span,
                data: ProgramStatement::ClassDeclaration(c)
            }),
            EOI(_e) => {}
        );
        Ok(Program { statements })
    }

    fn expression_slot(input: Node) -> Result<Option<Box<AstNode<Expression>>>> {
        match_nodes!(input.into_children();
            [expression(e)] => Ok(Some(e)),
            [] => Ok(None),
        )
    }

    fn expression_list(input: Node) -> Result<AstNode<ExpressionList>> {
        let mut expressions = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            expression_slot(e) => expressions.push(e)
        );
        // `F()` has no arguments, not one empty one.
        if let [None] = expressions.as_slice() {
            expressions.clear();
        }
        Ok(AstNode {
            span,
            data: ExpressionList { expressions },
        })
    }

    fn expression(input: Node) -> Result<Box<AstNode<Expression>>> {
        let nodes: Vec<Node> = input.into_children().collect();
        parse_expression_from_nodes(&nodes[..])
    }
}

#[derive(Debug)]
pub enum ParsingError {
    IoError(std::io::Error),
    EncodingError(String),
    PestError(Error<Rule>),
    /// In strict mode: problems that permissive parsing would have recovered
    /// from (e.g. invalid lines in a `defaultproperties` block).
    Strict(Vec<ProgramError>),
}

impl From<std::io::Error> for ParsingError {
    fn from(error: std::io::Error) -> Self {
        ParsingError::IoError(error)
    }
}

impl From<Error<Rule>> for ParsingError {
    fn from(error: Error<Rule>) -> Self {
        ParsingError::PestError(error)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProgramErrorSeverity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct ProgramError {
    pub message: String,
    pub span: AstSpan,
    pub severity: ProgramErrorSeverity,
    /// The lint that reported this problem, or `None` for problems found
    /// while parsing (e.g. recovered `defaultproperties` lines).
    pub lint: Option<Lint>,
    /// A suggested fix, if the lint has one for this problem.
    pub fix: Option<crate::fix::Fix>,
}

pub struct ProgramResult {
    pub program: Program,
    pub errors: Vec<ProgramError>,
    /// The declarations and resolved references of the program.
    pub symbols: SymbolTable,
    /// The byte ranges of the comments, in order.
    pub comments: Vec<std::ops::Range<usize>>,
}

/// For a float literal with more than one decimal point (e.g. `0.0.5`),
/// returns the part before the second point; otherwise returns `text`.
fn malformed_float_prefix(text: &str) -> &str {
    match text.match_indices('.').nth(1) {
        Some((i, _)) => &text[..i],
        None => text,
    }
}

/// Problems that permissive parsing recovered from.
fn recovered_problems(root: &Node, program: &Program) -> Vec<ProgramError> {
    let mut problems = Vec::new();
    for pair in root.as_pair().clone().into_inner().flatten() {
        let text = pair.as_str();
        if pair.as_rule() == Rule::float_literal && malformed_float_prefix(text) != text {
            problems.push(ProgramError {
                message: format!(
                    "Malformed numeric literal {:?} was read as {:?}",
                    text,
                    malformed_float_prefix(text)
                ),
                span: AstSpan {
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    line: None,
                },
                severity: ProgramErrorSeverity::Warning,
                lint: None,
                fix: None,
            });
        }
    }
    for statement in &program.statements {
        if let ProgramStatement::DefaultProperties(d) = &statement.data {
            for diagnostic in &d.diagnostics {
                problems.push(ProgramError {
                    message: format!("defaultproperties: {}", diagnostic.message),
                    span: AstSpan {
                        start: diagnostic.span.start,
                        end: diagnostic.span.end,
                        line: None,
                    },
                    severity: ProgramErrorSeverity::Warning,
                    lint: None,
                    fix: None,
                });
            }
            if !d.closed {
                problems.push(ProgramError {
                    message: "defaultproperties block is not closed before the end of the file"
                        .to_string(),
                    span: d.span,
                    severity: ProgramErrorSeverity::Warning,
                    lint: None,
                    fix: None,
                });
            }
        }
    }
    problems
}

/// Parses an UnrealScript program permissively; see [`parse_program_with`].
pub fn parse_program(contents: &str) -> std::result::Result<ProgramResult, ParsingError> {
    parse_program_with(contents, &ParseOptions::default())
}

/// Parses an UnrealScript program.
///
/// In [`ParseMode::Permissive`] (the default), malformed `defaultproperties`
/// content is recovered from the way Unreal Engine does (invalid lines are
/// discarded, trailing junk after a statement is ignored) and each problem is
/// reported as a warning in [`ProgramResult::errors`]. In [`ParseMode::Strict`]
/// these problems are returned as [`ParsingError::Strict`] instead.
pub fn parse_program_with(
    contents: &str,
    options: &ParseOptions,
) -> std::result::Result<ProgramResult, ParsingError> {
    let root = UnrealScriptParser::parse(Rule::program, contents)?
        .single()
        .expect("a parse has exactly one root");
    let program = UnrealScriptParser::program(root.clone())?;
    let lines = crate::source::LineIndex::new(contents);
    let with_lines = |mut errors: Vec<ProgramError>| {
        for error in &mut errors {
            error.span.line = Some(lines.line(error.span.start));
        }
        errors
    };

    let recovered = with_lines(recovered_problems(&root, &program));
    if options.mode == ParseMode::Strict && !recovered.is_empty() {
        return Err(ParsingError::Strict(recovered));
    }

    let mut symbols = SymbolTable::build(&program);
    symbols.set_lines(|offset| lines.line(offset));
    let comments = crate::source::comment_ranges(contents);
    let mut visitor = Visitor::new(contents, &symbols, &comments);
    program.visit(&mut visitor);
    let mut errors = recovered;
    errors.extend(with_lines(visitor.errors));
    errors.sort_by_key(|e| e.span.start);
    Ok(ProgramResult {
        program,
        errors,
        symbols,
        comments,
    })
}

/// The result of [`parse_bytes`]: the decoded source and the parse.
pub struct SourceResult {
    pub source: crate::source::Decoded,
    pub result: ProgramResult,
}

/// Decodes a source file the way UCC does (see [`crate::source`]) and parses
/// it with [`parse_program_with`]. A UTF-8 file is a recovered problem,
/// because UCC would read it as Latin-1: in [`ParseMode::Strict`] it fails
/// the parse.
pub fn parse_bytes(
    bytes: &[u8],
    options: &ParseOptions,
) -> std::result::Result<SourceResult, ParsingError> {
    let source = crate::source::decode_source(bytes);
    let lines = crate::source::LineIndex::new(&source.text);
    let mut problems: Vec<ProgramError> = crate::source::encoding_problems(&source)
        .into_iter()
        .map(|(range, message)| ProgramError {
            message,
            span: AstSpan {
                start: range.start,
                end: range.end,
                line: Some(lines.line(range.start)),
            },
            severity: ProgramErrorSeverity::Warning,
            lint: None,
            fix: None,
        })
        .collect();
    let strict = options.mode == ParseMode::Strict;
    match parse_program_with(&source.text, options) {
        Ok(_) if strict && !problems.is_empty() => Err(ParsingError::Strict(problems)),
        Ok(mut result) => {
            problems.append(&mut result.errors);
            problems.sort_by_key(|e| e.span.start);
            result.errors = problems;
            Ok(SourceResult { source, result })
        }
        Err(ParsingError::Strict(mut recovered)) => {
            problems.append(&mut recovered);
            problems.sort_by_key(|e| e.span.start);
            Err(ParsingError::Strict(problems))
        }
        Err(other) => Err(other),
    }
}

/// Parses `contents` as one expression. The whole input must be the
/// expression, apart from surrounding whitespace.
pub fn parse_expression(
    contents: &str,
) -> std::result::Result<Box<AstNode<Expression>>, ParsingError> {
    let root = UnrealScriptParser::parse(Rule::expression_input, contents)?
        .single()
        .expect("a parse has exactly one root");
    let expression = root
        .into_children()
        .next()
        .expect("the grammar gives an expression input an expression");
    Ok(UnrealScriptParser::expression(expression)?)
}

/// Parse text starting at a specific grammar rule.
/// Useful for incremental parsing where you want to parse a slice of code.
///
/// # Arguments
/// * `rule` - The grammar rule to start parsing from
/// * `contents` - The text to parse
///
/// # Returns
/// The raw pest_consume Nodes iterator on success, or a ParsingError on failure.
///
/// # Example
/// ```ignore
/// use usagi_core::{parse_with_rule, Rule};
/// let result = parse_with_rule(Rule::expression, "1 + 2");
/// ```
pub fn parse_with_rule(
    rule: Rule,
    contents: &str,
) -> std::result::Result<pest_consume::Nodes<'_, Rule, ()>, ParsingError> {
    UnrealScriptParser::parse(rule, contents).map_err(|e| ParsingError::from(e))
}
