extern crate pest;

use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "UnrealScript.pest"]
pub struct UnrealScriptParser;

use pest_consume::{Error, match_nodes};
use std::str::FromStr;
use std::collections::HashMap;

use encoding::{DecoderTrap, Encoding};
use std::fs::File;
use std::io::Read;

use crate::ast::*;
use crate::visitor::{Visit, Visitor};

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

pub type ParserRule = Rule;

impl From<&Node<'_>> for AstSpan {
    fn from(node: &Node<'_>) -> Self {
        AstSpan {
            start: node.as_pair().as_span().start(),
            end: node.as_pair().as_span().end()
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
                _ => { return Err(node.error("unexpected rule")) }
            }
        }
    )
}

lazy_static! {
    static ref OPERATOR_PRECEDENCES: HashMap<&'static str, u8> = {
        let mut map = HashMap::new();
        map.insert("==", 24);
        map.insert("!=", 26);
        map.insert("&&", 30);
        map.insert("^^", 30);
        map.insert("||", 32);
        map.insert("*=", 34);
        map.insert("/=", 34);
        map.insert("+=", 34);
        map.insert("-=", 34);
        map.insert("*", 16);
        map.insert("/", 16);
        map.insert("%", 18);
        map.insert("+", 20);
        map.insert("-", 20);
        map.insert("<<", 22);
        map.insert(">>", 22);
        map.insert(">>>", 22);
        map.insert("<", 24);
        map.insert(">", 24);
        map.insert("<=", 24);
        map.insert(">=", 24);
        map.insert("==", 24);
        map.insert("~=", 24);
        map.insert("!=", 26);
        map.insert("&", 28);
        map.insert("^", 28);
        map.insert("|", 28);
        map.insert("!=", 28);
        map.insert("**", 12);
        map.insert("^", 28);
        map.insert("|", 28);
        map.insert("!=", 28);
        map.insert("$", 40);
        map.insert("@", 40);
        map.insert("$=", 44);
        map.insert("@=", 44);
        map.insert("-=", 45);
        map.insert("=", 46);    // TODO: just a guess

        // TODO: precedence relies on the TYPE also!
        // TODO: all of these should be read from Object.uc before proper parsing, so that we don't
        // need to have this table at all.
        map.insert("clockwisefrom", 24);
        map.insert("dot", 16);
        map.insert("cross", 16);
        map
    };
}

fn parse_target(nodes: &[Node]) -> Result<Box<Expression>> {
    let (node, remaining) = nodes.split_last().unwrap();
    match node.as_rule() {
        Rule::unqualified_identifier => {
            Ok(Box::new(Expression::Identifier(UnrealScriptParser::unqualified_identifier(Node::new(node.clone().into_pair()))?)))
        }
        Rule::new => {
            let mut inner_iter = node.clone().into_children().into_iter();
            let first = inner_iter.next().unwrap();
            if let Rule::expression_list = first.as_rule() {
                Ok(Box::new(Expression::New {
                    arguments: Some(UnrealScriptParser::expression_list(first)?),
                    type_: UnrealScriptParser::expression(Node::new(inner_iter.next().unwrap().clone().into_pair()))?
                }))
            }  else {
                Ok(Box::new(Expression::New {
                    arguments: None,
                    type_: UnrealScriptParser::expression(Node::new(first.clone().into_pair()))?
                }))
            }
        }
        Rule::cast => {
            let mut inner_iter = node.clone().into_children().into_iter();
            Ok(Box::new(Expression::Cast {
                type_: UnrealScriptParser::class_type(inner_iter.next().unwrap())?,
                operand: UnrealScriptParser::expression(inner_iter.next().unwrap())?
            }))
        }
        // NOTE: these are required because integer_literal and float_literals can be captured
        // before monadic prefix verbs.
        Rule::numeric_literal => {
            Ok(Box::new(Expression::Literal(Literal::Numeric(UnrealScriptParser::numeric_literal(Node::new(node.clone().into_pair()))?))))
        }
        Rule::literal => {
            Ok(Box::new(Expression::Literal(UnrealScriptParser::literal(Node::new(node.clone().into_pair()))?)))
        }
        Rule::default_access => {
            Ok(Box::new(Expression::DefaultAccess {
                operand: match remaining.is_empty() {
                    true => None,
                    false => Some(parse_expression_from_nodes(remaining)?)
                },
                target: UnrealScriptParser::unqualified_identifier(node.clone().into_children().single()?)?
            }))
        }
        Rule::static_access => {
            Ok(Box::new(Expression::StaticAccess {
                operand: match remaining.is_empty() {
                    true => None,
                    false => Some(parse_expression_from_nodes(remaining)?)
                },
                target: UnrealScriptParser::unqualified_identifier(node.clone().into_children().single()?)?
            }))
        }
        Rule::global_call => {
            let mut inner_iter = node.clone().into_children().into_iter();
            let name = UnrealScriptParser::unqualified_identifier(inner_iter.next().unwrap())?;
            let arguments = UnrealScriptParser::expression_list(inner_iter.next().unwrap())?;
            Ok(Box::new(Expression::GlobalCall { name, arguments }))
        }
        Rule::parenthetical_expression => {
            let children: Vec<Node> = node.clone().into_children().single()?.into_children().into_iter().collect();
            Ok(Box::new(Expression::ParentheticalExpression(parse_expression_from_nodes(&children[..])?)))
        }
        Rule::call => {
            Ok(Box::new(Expression::Call {
                operand: parse_expression_from_nodes(remaining)?,
                arguments: UnrealScriptParser::expression_list(node.clone().into_children().single()?)? }))
        }
        Rule::array_access => {
            Ok(Box::new(Expression::ArrayAccess {
                operand: parse_expression_from_nodes(remaining)?,
                argument: UnrealScriptParser::expression(node.clone().into_children().single()?)?
            }))
        }
        Rule::member_access => {
            Ok(Box::new(Expression::MemberAccess {
                operand: parse_expression_from_nodes(remaining)?,
                target: UnrealScriptParser::unqualified_identifier(node.clone().into_children().single()?)?
            }))
        }
        _ => panic!("unhandled rule {:?}", node.as_pair())
    }
}

fn parse_expression_from_nodes(nodes: &[Node]) -> Result<Box<Expression>> {
    let mut dyadic_verbs = Vec::new();
    for (index, node) in nodes.into_iter().enumerate() {
        if let Rule::dyadic_verb = node.as_rule() {
            let operator_precedence = OPERATOR_PRECEDENCES.get(node.as_str().to_lowercase().as_str());
            if let Some(operator_precedence) = operator_precedence {
                dyadic_verbs.push((operator_precedence, index, node.as_str().to_lowercase()))
            } else {
                return Err(node.error("encountered unregistered dyadic verb"));
            }
        }
    }
    if !dyadic_verbs.is_empty() {
        // Sort dyadic verbs by precedence
        // TODO: secondarily, sort by order!
        dyadic_verbs.sort_by(|a, b| b.0.cmp(a.0));
        // Split expression on either side of the verb and return a dyadic expression node
        if let Some((_, index, operator)) = dyadic_verbs.first() {
            return Ok(Box::new(Expression::DyadicExpression {
                lhs: parse_expression_from_nodes(&nodes[..index - 0])?,
                verb: DyadicVerb::from_str(operator.as_str()).unwrap(),
                rhs: parse_expression_from_nodes(&nodes[index + 1..])?
            }))
        }
    }
    // Next, search for monadic verbs at the beginning and end of the expression
    if let Rule::monadic_post_verb = nodes.last().unwrap().as_rule() {
        return Ok(Box::new(Expression::MonadicPostExpression {
            verb: MonadicVerb::from_str(nodes.last().unwrap().as_str()).unwrap(),
            operand: parse_target(&nodes[..nodes.len() - 1])?
        }))
    }
    if let Rule::monadic_pre_verb = nodes.first().unwrap().as_rule() {
        return Ok(Box::new(Expression::MonadicPreExpression {
            verb: MonadicVerb::from_str(nodes.first().unwrap().as_str()).unwrap(),
            operand: parse_target(&nodes[1..])?
        }))
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

    fn float_literal(input: Node) -> Result<NumericLiteral> {
        input.as_str().to_lowercase().trim_end_matches("f").to_string().as_str().parse::<f32>()
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(NumericLiteral::Float(v)))
    }

    fn single_quoted_string(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn name_literal(input: Node) -> Result<Literal> {
        Ok(Literal::Name(input.as_str().to_string()))
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
        input.as_str().parse::<i32>()
            .map_err(|e| input.error(e))
    }

    fn numeric_sign(input: Node) -> Result<i32> {
        return if input.as_str() == "-" { Ok(-1) } else { Ok(1) }
    }

    fn integer_literal(input: Node) -> Result<NumericLiteral> {
        let mut sign = 1;
        let mut integer = 0;
        match_nodes_any!(input.into_children();
            numeric_sign(s) => sign = s,
            integer_literal_hexadecimal(s) => integer = s,
            integer_literal_decimal(s) => integer = s
        );
        Ok(NumericLiteral::Integer(sign * integer))
    }

    fn unqualified_identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode { span: AstSpan::from(&input), data: Identifier { string: input.as_str().to_string() } })
    }

    fn qualified_identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode { span: AstSpan::from(&input), data: Identifier { string: input.as_str().to_string() } })
    }

    fn identifier(input: Node) -> Result<AstNode<Identifier>> {
        Ok(AstNode { span: AstSpan::from(&input), data: Identifier { string: input.as_str().to_string() } })
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

    fn numeric_literal(input: Node) -> Result<NumericLiteral> {
        match_nodes!(input.into_children();
            [integer_literal(n)] => return Ok(n),
            [float_literal(f)] => return Ok(f)
        )
    }

    fn vector_literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [numeric_literal(x), numeric_literal(y), numeric_literal(z)] => {
                return Ok(Literal::Vector { x, y, z })
            }
        )
    }

    fn rotator_literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [numeric_literal(pitch), numeric_literal(yaw), numeric_literal(roll)] => {
                return Ok(Literal::Rotator { pitch, yaw, roll })
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
            data: ClassModifierType::from_str(input.as_str().to_lowercase().as_str()).unwrap()
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
                is_editable
            }
        })
    }

    fn struct_modifier(input: Node) -> Result<AstNode<StructModifier>> {
        Ok(AstNode { span: AstSpan::from(&input), data: StructModifier::from_str(input.as_str()).unwrap() })
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
                cpp
            }
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
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(Type::Class(id))
        )
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
            data: VarModifier::from_str(input.as_str()).unwrap()
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
                names
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

    fn literal(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [vector_literal(v)] => Ok(v),
            [rotator_literal(r)] => Ok(r),
            [numeric_literal(n)] => Ok(Literal::Numeric(n)),
            [string_literal(s)] => Ok(s),
            [name_literal(n)] => Ok(n),
            [object_literal(o)] => Ok(o),
        )
    }

    fn boolean_literal(input: Node) -> Result<Literal> {
        bool::from_str(input.as_str())
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(Literal::Boolean(v)))
    }

    // TODO: once we get everything functioning, revisit why boolean literal is not just included in literal rule
    fn const_value(input: Node) -> Result<Literal> {
        match_nodes!(input.into_children();
            [literal(l)] => Ok(l),
            [boolean_literal(l)] => Ok(l),
        )
    }

    fn const_declaration(input: Node) -> Result<AstNode<ConstDeclaration>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(name), const_value(value)] => {
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

    fn function_modifier(input: Node) -> Result<FunctionModifier> {
        match_nodes!(input.into_children();
            [function_modifier_type(type_), integer_literal(arguments)..] => {
                Ok(FunctionModifier { type_, arguments: arguments.collect() })
            }
        )
    }

    fn function_type_no_arguments_type(input: Node) -> Result<FunctionTypeType> {
        Ok(FunctionTypeType::from_str(input.as_str()).unwrap())
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

    fn function_name(input: Node) -> Result<Identifier> {
        Ok(Identifier::new(input.as_str()))
    }

    fn function_argument_modifier(input: Node) -> Result<FunctionArgumentModifier> {
        Ok(FunctionArgumentModifier::from_str(input.as_str()).unwrap())
    }

    fn function_argument(input: Node) -> Result<FunctionArgument> {
        let mut modifiers = Vec::new();
        let mut type_: Option<Type> = None;
        let mut name = None;
        match_nodes_any!(input.into_children();
            function_argument_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => name = Some(n)
        );
        Ok(FunctionArgument {
            modifiers,
            type_: type_.unwrap(),
            name: name.unwrap()
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
            else_statement
        })
    }

    fn foreach_expression(input: Node) -> Result<Box<Expression>> {
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

    fn for_init(input: Node) -> Result<Box<Expression>> {
        match_nodes!(input.into_children(); [expression(e)] => Ok(e) )
    }

    fn for_predicate(input: Node) -> Result<Box<Expression>> {
        match_nodes!(input.into_children(); [expression(e)] => Ok(e) )
    }

    fn for_post(input: Node) -> Result<Box<Expression>> {
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
        Ok(CodeStatement::For(Box::new(ForStatement { init, predicate, post, body: body.unwrap() })))
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
        Ok(DoUntil { body: body.unwrap(), predicate })
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
            )
        })
    }

    fn function_body(input: Node) -> Result<FunctionBody> {
        let mut statements = Vec::new();
        match_nodes_any!(input.into_children();
            const_declaration(c) => statements.push(FunctionBodyStatement::ConstDeclaration(c)),
            local_declaration(l) => statements.push(FunctionBodyStatement::LocalDeclaration(l)),
            code_statement(c) => statements.push(FunctionBodyStatement::CodeStatement(c))
        );
        Ok(FunctionBody { statements })
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
                body
            }
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
            labels
        })
    }

    fn defaultproperties_object(input: Node) -> Result<AstNode<DefaultPropertiesObject>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [identifier(class), defaultproperties_statement(statements)..] => {
                Ok(AstNode { span, data: DefaultPropertiesObject { class, statements: statements.collect() } })
            }
        )
    }

    fn defaultproperties_statement(input: Node) -> Result<AstNode<DefaultPropertiesStatement>> {
        let span = AstSpan::from(&input);
        Ok(AstNode { span, data: match_nodes!(input.into_children();
            [defaultproperties_assignment(a)] => DefaultPropertiesStatement::Assignment(a),
            [defaultproperties_object(a)] => DefaultPropertiesStatement::Object(a),
        )})
    }

    fn defaultproperties_struct(input: Node) -> Result<AstNode<DefaultPropertiesStruct>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [defaultproperties_assignment(assignments)..] => {
                Ok(AstNode {
                    span,
                    data: DefaultPropertiesStruct {
                        assignments: assignments.collect()
                    }
                })
            }
        )
    }

    fn defaultproperties_array_comma(_input: Node) -> Result<()> {
        Ok(())
    }

    fn defaultproperties_array(input: Node) -> Result<AstNode<DefaultPropertiesArray>> {
        let span = AstSpan::from(&input);
        let mut elements = Vec::new();
        match_nodes_any!(input.into_children();
            defaultproperties_array_comma(_r) => elements.push(None),
            defaultproperties_value(v) => elements.push(Some(v))
        );
        Ok(AstNode { span, data: DefaultPropertiesArray { elements } })
    }

    fn defaultproperties_value(input: Node) -> Result<AstNode<DefaultPropertiesValue>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [literal(l)] => DefaultPropertiesValue::Literal(l),
                [identifier(l)] => DefaultPropertiesValue::Identifier(l),
                [defaultproperties_struct(s)] => DefaultPropertiesValue::Struct(s),
                [defaultproperties_array(a)] => DefaultPropertiesValue::Array(a)
            )
        })
    }

    fn defaultproperties_array_index(input: Node) -> Result<AstNode<DefaultPropertiesArrayIndex>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [integer_literal(l)] => Ok(AstNode { span, data: DefaultPropertiesArrayIndex::IntegerLiteral(l) }),
            [unqualified_identifier(id)] => Ok(AstNode { span, data: DefaultPropertiesArrayIndex::Identifier(id) })
        )
    }

    fn defaultproperties_target(input: Node) -> Result<AstNode<DefaultPropertiesTarget>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [unqualified_identifier(target), defaultproperties_array_index(index)] => {
                Ok(AstNode { span, data: DefaultPropertiesTarget { target, index: Some(index) } })
            },
            [unqualified_identifier(target)] => {
                Ok(AstNode { span, data: DefaultPropertiesTarget { target, index: None } })
            }
        )
    }

    fn defaultproperties_assignment(input: Node) -> Result<AstNode<DefaultPropertiesAssignment>> {
        let span = AstSpan::from(&input);
        Ok(AstNode { span, data: match_nodes!(input.into_children();
            [defaultproperties_target(target)] => DefaultPropertiesAssignment { target, value: None },
            [defaultproperties_target(target), defaultproperties_value(value)] => {
                DefaultPropertiesAssignment { target, value: Some(value) }
            },
        )})
    }

    fn defaultproperties(input: Node) -> Result<AstNode<DefaultProperties>> {
        let span = AstSpan::from(&input);
        match_nodes!(input.into_children();
            [defaultproperties_statement(statements)..] => {
                Ok(AstNode { span, data: DefaultProperties { statements: statements.collect() } })
            }
        )
    }

    fn cpp_body(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn cpptext(input: Node) -> Result<String> {
        match_nodes!(input.into_children();
            [cpp_body(b)] => Ok(b)
        )
    }

    fn cppstruct(input: Node) -> Result<String> {
        match_nodes!(input.into_children();
            [cpp_body(b)] => Ok(b)
        )
    }

    fn program_statement(input: Node) -> Result<AstNode<ProgramStatement>> {
        Ok(AstNode {
            span: AstSpan::from(&input),
            data: match_nodes!(input.into_children();
                [statement_empty(_e)] => ProgramStatement::Empty,
                [compiler_directive(c)] => ProgramStatement::CompilerDirective(c),
                [const_declaration(c)] => ProgramStatement::ConstDeclaration(c),
                [var_declaration(v)] => ProgramStatement::VarDeclaration(v),
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

    fn program(input: Node) -> Result<Program> {
        let mut statements = Vec::new();
        let span = AstSpan::from(&input);
        match_nodes_any!(input.into_children();
            program_statement(s) => statements.push(s),
            class_declaration(c) => statements.push(AstNode {
                span,
                data: ProgramStatement::ClassDeclaration(c)
            }),
            EOI(_e) => {}
        );
        Ok(Program { statements })
    }

    fn expression_empty(_input: Node) -> Result<()> {
        Ok(())
    }

    fn expression_list(input: Node) -> Result<Vec<Option<Box<Expression>>>> {
        let mut expressions = Vec::new();
        match_nodes_any!(input.into_children();
            expression(e) => expressions.push(Some(e)),
            expression_empty(_e) => expressions.push(None)
        );
        Ok(expressions)
    }

    fn expression(input: Node) -> Result<Box<Expression>> {
        let nodes: Vec<Node> = input.into_children().collect();
        parse_expression_from_nodes(&nodes[..])
    }
}

pub enum ParsingError {
    IoError(std::io::Error),
    EncodingError(String),
    PestError(Error<Rule>)
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

fn read_file_to_string(path: &str) -> std::result::Result<String, ParsingError> {
    let mut file = File::open(path)?;
    let mut buffer: Vec<u8> = Vec::new();
    file.read_to_end(&mut buffer)?;
    encoding::all::WINDOWS_1252
        .decode(&mut buffer, DecoderTrap::Strict)
        .map_err(|e| ParsingError::EncodingError(e.to_string()))
}

#[derive(Debug, Clone)]
pub enum ProgramErrorSeverity {
    Warning,
    Error
}

#[derive(Debug, Clone)]
pub struct ProgramError {
    pub message: String,
    pub span: AstSpan,
    pub severity: ProgramErrorSeverity,
}

pub struct ProgramResult {
    pub program: Program,
    pub errors: Vec<ProgramError>
}

pub fn parse_program(contents: &str) -> std::result::Result<ProgramResult, ParsingError> {
    match UnrealScriptParser::program(UnrealScriptParser::parse(Rule::program, contents)?.single()?) {
        Ok(program) => {
            let mut visitor = Visitor::new();
            program.visit(&mut visitor);
            Ok(ProgramResult {
                program,
                errors: visitor.get_errors().to_vec()
            })
        },
        Err(e) => Err(ParsingError::from(e))
    }
}

pub fn parse_expression(contents: &str) -> std::result::Result<Box<Expression>, ParsingError> {
    match UnrealScriptParser::expression(UnrealScriptParser::parse(Rule::expression, contents)?.single()?) {
        Ok(e) => Ok(e),
        Err(e) => Err(ParsingError::from(e))
    }
}

pub fn parse_file(path: &str) -> std::result::Result<ProgramResult, ParsingError> {
    parse_program(read_file_to_string(path)?.as_str())
}
