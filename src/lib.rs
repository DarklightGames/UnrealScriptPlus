use std::str::FromStr;
use std::fs::{File};
use std::io::{Read};
use std::ops::AddAssign;
use std::convert::{TryInto};
use std::collections::HashMap;

extern crate pest;
#[macro_use]
extern crate pest_derive;
use pest::error::{Error, ErrorVariant, LineColLocation, InputLocation};
use pest::iterators::{Pair};
use pest::Parser;

#[derive(Parser)]
#[grammar = "UnrealScript.pest"]
struct UnrealScriptParser;

mod ast;
use crate::ast::*;

extern crate encoding;

#[macro_use]
extern crate lazy_static;

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

fn parse_arguments(pairs: &[Pair<Rule>]) -> ParseResult<Vec<Option<Box<AstNode>>>> {
    let pairs = pairs.clone().into_iter();
    let mut arguments = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::expression => {
                arguments.push(Some(pair.clone().try_into()?))
            }
            Rule::expression_empty => {
                arguments.push(None)
            }
            _ => { panic!("bad rule {:?}", pair); }
        }
    }
    Ok(arguments)
}

// Recursively parse the target.
fn parse_target(pairs: &[Pair<Rule>]) -> ParseResult<Box<AstNode>> {
    let (pair, remaining) = pairs.split_last().unwrap();
    return match pair.as_rule() {
        Rule::parenthetical_expression => {
            pair.clone().try_into()
        }
        Rule::global_call => {
            let mut inner_iter = pair.clone().into_inner().into_iter();
            let name = inner_iter.next().unwrap().as_str().to_string();
            let arguments: Vec<Pair<Rule>> = inner_iter.collect();
            Ok(Box::new(AstNode::GlobalCall {
                name,
                arguments: parse_arguments(&arguments[..])?,
            }))
        }
        Rule::call => {
            let argument_pairs: Vec<Pair<Rule>> = pair.clone().into_inner().into_iter().collect();
            Ok(Box::new(AstNode::Call {
                arguments: parse_arguments(&argument_pairs[..])?,
                operand: parse_target(remaining)?
            }))
        },
        Rule::member_access => {
            Ok(Box::new(AstNode::MemberAccess {
                target: pair.clone().into_inner().next().unwrap().as_str().to_string(),
                operand: parse_target(remaining)?
            }))
        },
        Rule::array_access => {
            Ok(Box::new(AstNode::ArrayAccess {
                argument: pair.clone().into_inner().next().unwrap().try_into()?,
                operand: parse_target(remaining)?
            }))
        },
        Rule::static_access => {
            let mut operand = None;
            if !remaining.is_empty() {
                operand = Some(parse_target(remaining)?);
            }
            Ok(Box::new(AstNode::StaticAccess {
                target: pair.clone().into_inner().next().unwrap().as_str().to_string(),
                operand
            }))
        },
        Rule::default_access => {
            let mut operand = None;
            if !remaining.is_empty() {
                operand = Some(parse_target(remaining)?);
            }
            Ok(Box::new(AstNode::DefaultAccess {
                target: pair.clone().into_inner().next().unwrap().as_str().to_string(),
                operand
            }))
        }
        _ => pair.clone().try_into()
    }
}

// TODO: maybe have this function take an iterator/slice
fn parse_expression(pairs: &[Pair<Rule>]) -> ParseResult<Box<AstNode>> {
    // First, search for dyadic verbs
    let mut dyadic_verbs = vec![];
    for (index, pair) in pairs.into_iter().enumerate() {
        match pair.as_rule() {
            Rule::dyadic_verb => {
                let operator_precedence = OPERATOR_PRECEDENCES.get(pair.as_str().to_lowercase().as_str());
                if let Some(operator_precedence) = operator_precedence {
                    dyadic_verbs.push((operator_precedence, index, pair.as_str()))
                } else {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError { message: format!("unrecognized dyadic verb \"{}\"", pair.as_str()) },
                        pair.as_span())
                    )
                }
            },
            _ => {}
        }
    }
    if !dyadic_verbs.is_empty() {
        // Sort dyadic verbs by precedence
        // TODO: secondarily, sort by order!
        dyadic_verbs.sort_by(|a, b| b.0.cmp(a.0));
        // Split expression on either side of the verb and return a dyadic expression node
        match dyadic_verbs.first() {
            Some((_, index, operator)) => {
                return Ok(Box::new(AstNode::DyadicExpression {
                    lhs: parse_expression(&pairs[..index - 0]).unwrap(),
                    operator: operator.to_string(),
                    rhs: parse_expression(&pairs[index + 1..]).unwrap()
                }));
            },
            _ => {}
        }
    }
    // Next, search for monadic verbs at the beginning and end of the expression
    match pairs.last().unwrap().as_rule() {
        Rule::monadic_post_verb => {
            return Ok(Box::new(AstNode::MonadicPostExpression {
                operator: pairs.last().unwrap().as_str().to_string(),
                target: parse_target(&pairs[..pairs.len() - 1])?
            }))
        }
        _ => {}
    }
    match pairs.first().unwrap().as_rule() {
        Rule::monadic_pre_verb => {
            return Ok(Box::new(AstNode::MonadicPreExpression {
                operator: pairs.first().unwrap().as_str().to_string(),
                target: parse_target(&pairs[1..])?
            }))
        },
        _ => {}
    }
    parse_target(pairs)
}

pub type ParseResult<T> = std::result::Result<T, pest::error::Error<Rule>>;

impl TryInto<Box<AstNode>> for Pair<'_, Rule> {

    type Error = pest::error::Error<Rule>;

    fn try_into(self) -> std::prelude::rust_2015::Result<Box<AstNode>, Self::Error> {
        match self.as_rule() {
            Rule::statement_empty => Ok(Box::new(AstNode::EmptyStatement)),
            Rule::code_scope => {
                let inner_pairs = self.into_inner().into_iter();
                let statements: Result<Vec<Box<AstNode>>, _> = inner_pairs
                    .map(|pair| pair.try_into()).
                    collect();
                Ok(Box::new(AstNode::CodeScope { statements: statements? }))
            }
            Rule::code_statement_outer => {
                self.into_inner().into_iter().next().unwrap().try_into()
            }
            Rule::conditional_statement => {
                let outer_pairs = self.into_inner().into_iter();
                let mut if_statement = None;
                let mut elif_statements: Vec<Box<AstNode>> = Vec::new();
                let mut else_statement = None;
                for pair in outer_pairs {
                    let mut inner_pairs = pair.clone().into_inner().into_iter();
                    match pair.as_rule() {
                        Rule::if_statement_empty => {
                            let predicate = inner_pairs.next().unwrap().try_into()?;
                            if_statement = Some(Box::new(AstNode::IfStatement {
                                predicate,
                                statements: vec![]
                            }))
                        }
                        Rule::if_statement => {
                            let predicate = inner_pairs.next().unwrap().try_into()?;
                            let statements: Result<Vec<Box<AstNode>>, _> = inner_pairs.map(|pair| pair.try_into()).collect();
                            if_statement = Some(Box::new(AstNode::IfStatement {
                                predicate,
                                statements: statements?
                            }))
                        }
                        Rule::elif_statement => {
                            let predicate = inner_pairs.next().unwrap().try_into()?;
                            let statements: Result<Vec<Box<AstNode>>, _> = inner_pairs.map(|pair| { pair.try_into() }).collect();
                            elif_statements.push(Box::new(AstNode::ElifStatement {
                                predicate,
                                statements: statements?
                            }))
                        }
                        Rule::else_statement => {
                            let statements: Result<Vec<Box<AstNode>>, _> = inner_pairs.map(|pair| pair.try_into()).collect();
                            else_statement = Some(Box::new(AstNode::ElseStatement { statements: statements? }))
                        }
                        _ => panic!("unhandled rule")
                    }
                }
                Ok(Box::new(AstNode::ConditionalStatement {
                    if_statement: if_statement.unwrap(),
                    elif_statements,
                    else_statement
                }))
            }
            Rule::for_statement => {
                let mut init = None;
                let mut predicate = None;
                let mut post = None;
                // inner pairs is moved...???
                let mut outer_pairs = self.into_inner().into_iter();
                let inner_pairs = outer_pairs.next().unwrap().into_inner();
                for pair in inner_pairs {
                    match pair.as_rule() {
                        Rule::for_init => {
                            init = Some(pair.into_inner().into_iter().next().unwrap().try_into()?);
                        }
                        Rule::for_predicate => {
                            predicate = Some(pair.into_inner().into_iter().next().unwrap().try_into()?);
                        },
                        Rule::for_post => {
                            post = Some(pair.into_inner().into_iter().next().unwrap().try_into()?);
                        },
                        _ => panic!("unexpected rule")
                    }
                }
                Ok(Box::new(AstNode::ForStatement {
                    init,
                    predicate,
                    post,
                    body: outer_pairs.next().unwrap().try_into()?
                }))
            }
            Rule::while_statement => {
                let mut inner_pairs = self.into_inner().into_iter();
                Ok(Box::new(AstNode::WhileStatement {
                    predicate: inner_pairs.next().unwrap().try_into()?,
                    body: inner_pairs.next().unwrap().try_into()?
                }))
            }
            Rule::do_until_statement => {
                let mut inner_pairs = self.into_inner().into_iter();
                Ok(Box::new(AstNode::DoUntilStatement {
                    body: inner_pairs.next().unwrap().try_into()?,
                    predicate: match inner_pairs.next() {
                        Some(pair) => { Some(pair.try_into()?) },
                        None => { None }
                    }
                }))
            }
            Rule::foreach_statement => {
                let mut inner_pairs = self.into_inner().into_iter();
                Ok(Box::new(AstNode::ForEachStatement {
                    predicate: inner_pairs.next().unwrap().try_into()?,
                    body: inner_pairs.next().unwrap().try_into()?
                }))
            }
            Rule::goto_statement => {
                Ok(Box::new(AstNode::GotoStatement {
                    label: self.into_inner().into_iter().next().unwrap().as_str().to_string()
                }))
            }
            Rule::break_statement => {
                Ok(Box::new(AstNode::BreakStatement))
            }
            Rule::continue_statement => {
                Ok(Box::new(AstNode::ContinueStatement))
            }
            Rule::switch_statement => {
                let mut inner_iter = self.into_inner().into_iter();
                let predicate = inner_iter.next().unwrap().try_into()?;
                let mut cases: Vec<Box<AstNode>> = Vec::new();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::switch_case => {
                            let mut case_inner_iter = pair.into_inner().into_iter();
                            cases.push(Box::new(AstNode::SwitchCase {
                                predicate: case_inner_iter.next().unwrap().try_into()?,
                                statements: case_inner_iter.map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                            }))
                        }
                        Rule::switch_default_case => {
                            cases.push(Box::new(AstNode::SwitchDefaultCase {
                                statements: pair.into_inner().into_iter().map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                            }))
                        }
                        _ => panic!("unhandled rule")
                    }
                }
                Ok(Box::new(AstNode::SwitchStatement {
                    predicate,
                    cases
                }))
            }
            Rule::return_statement => {
                let mut expression: Option<Box<AstNode>> = None;
                match self.into_inner().into_iter().next() {
                    Some(item) => {
                        expression = Some(item.try_into()?)
                    },
                    None => {}
                }
                Ok(Box::new(AstNode::ReturnStatement { expression }))
            }
            Rule::expression | Rule::foreach_expression => {
                let inner_pairs: Vec<Self> = self.into_inner().collect();
                parse_expression(&inner_pairs[..])
            }
            Rule::program => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::Program {
                    class_declaration: inner_iter.next().unwrap().try_into()?,
                    statements: inner_iter.map(|pair| pair.try_into() ).collect::<Result<Vec<Box<AstNode>>, _>>()?
                }))
            }
            Rule::replication_statement => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::ReplicationStatement {
                    reliability: ReplicationReliability::from_str(inner_iter.next().unwrap().as_str()).unwrap(),
                    condition: inner_iter.next().unwrap().try_into()?,
                    variables: inner_iter.next().unwrap().into_inner().into_iter().map(|pair| { pair.as_str().to_string() }).collect()
                }))
            }
            Rule::replication_block => {
                Ok(Box::new(AstNode::ReplicationBlock {
                    statements: self.into_inner().into_iter().map(|pair| { pair.try_into() }).collect::<Result<Vec<Box<AstNode>>, _>>()?
                }))
            }
            Rule::compiler_directive => {
                Ok(Box::new(AstNode::CompilerDirective {
                    contents: self.into_inner().next().unwrap().as_str().to_string()
                }))
            }
            Rule::class_modifier => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::ClassModifier {
                    type_: ClassModifier::from_str(inner_iter.next().unwrap().as_str()).unwrap(),
                    arguments: inner_iter.map(|pair| { pair.try_into() }).collect::<Result<Vec<Box<AstNode>>, _>>()?
                }))
            }
            Rule::class_declaration => {
                // TODO: turn this into a matching iter
                let inner_iter = self.into_inner().into_iter();
                let mut name: String = String::new();
                let mut parent_class: Option<String> = None;
                let mut modifiers: Vec<Box<AstNode>> = Vec::new();
                let mut within: Option<String> = None;
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::unqualified_identifier => {
                            name = pair.as_str().to_string()
                        }
                        Rule::extends => {
                            parent_class = Some(pair.into_inner().next().unwrap().as_str().to_string())
                        }
                        Rule::class_modifier => {
                            modifiers.push(pair.try_into()?)
                        }
                        Rule::class_within => {
                            within = Some(pair.into_inner().into_iter().next().unwrap().as_str().to_string())
                        }
                        _ => { panic!("unexpected pair") }
                    }
                }
                Ok(Box::new(AstNode::ClassDeclaration {
                    name,
                    parent_class,
                    modifiers,
                    within
                }))
            }
            Rule::const_declaration => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::ConstDeclaration {
                    name: inner_iter.next().unwrap().as_str().to_string(),
                    value: inner_iter.next().unwrap().try_into()?
                }))
            }
            Rule::enum_declaration => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::EnumDeclaration {
                    name: inner_iter.next().unwrap().as_str().to_string(),
                    values: inner_iter.next().unwrap().into_inner().into_iter().map(|pair| pair.as_str().to_string() ).collect()
                }))
            }
            Rule::literal => {
                self.into_inner().next().unwrap().try_into()
            }
            Rule::identifier => {
                Ok(Box::new(AstNode::Identifier(self.as_str().to_string())))
            }
            Rule::class_type => {
                Ok(Box::new(AstNode::ClassType(self.into_inner().next().unwrap().try_into()?)))
            }
            Rule::state_declaration => {
                let mut is_editable = false;
                let mut modifiers: Vec<StateModifier> = Vec::new();
                let mut name: Option<String> = None;
                let mut parent: Option<String> = None;
                let mut ignores: Vec<String> = Vec::new();
                let mut statements: Vec<Box<AstNode>> = Vec::new();
                let mut labels: Vec<Box<AstNode>> = Vec::new();
                let inner_iter = self.into_inner().into_iter();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::state_modifier => {
                            modifiers.push(StateModifier::from_str(pair.as_str()).unwrap())
                        },
                        Rule::unqualified_identifier => {
                            name = Some(pair.as_str().to_string())
                        },
                        Rule::state_editable => {
                            is_editable = true
                        }
                        Rule::extends => {
                            parent = Some(pair.into_inner().next().unwrap().as_str().to_string())
                        },
                        Rule::state_ignores => {
                            pair.into_inner().into_iter().for_each(|pair| {
                                ignores.push(pair.as_str().to_string())
                            })
                        },
                        Rule::state_statement => {
                            statements.push(pair.into_inner().next().unwrap().try_into()?)
                        }
                        Rule::state_label => {
                            let mut inner_iter = pair.into_inner().into_iter();
                            labels.push(Box::new(AstNode::StateLabel {
                                label: inner_iter.next().unwrap().as_str().to_string(),
                                statements: inner_iter.map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                            }))
                        },
                        _ => panic!("unhandled rule")
                    }
                }
                Ok(Box::new(AstNode::StateDeclaration {
                    is_editable,
                    modifiers,
                    name: name.unwrap(),
                    parent,
                    ignores,
                    statements,
                    labels
                }))
            }
            Rule::integer_literal => {
                let mut integer: i32 = 0;
                let mut sign = 1;
                let inner_iter = self.into_inner().into_iter();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::positive_or_negative => {
                            if pair.as_str() == "-" {
                                sign = -1
                            }
                        }
                        Rule::integer_literal_hexadecimal => {
                            let span = pair.as_span();
                            match u32::from_str_radix(pair.into_inner().next().unwrap().as_str(), 16) {
                                Ok(u) => {
                                    integer = i32::from_be_bytes(u.to_be_bytes())
                                }
                                Err(e) => return Err(Error::new_from_span(ErrorVariant::CustomError { message: e.to_string() }, span))
                            }
                        },
                        Rule::integer_literal_decimal => {
                            match pair.as_str().parse::<i32>() {
                                Ok(i) => integer = i,
                                Err(e) => return Err(Error::new_from_span(ErrorVariant::CustomError { message: e.to_string() }, pair.as_span()))
                            }
                        }
                        _ => panic!("unhandled rule")
                    }
                }
                Ok(Box::new(AstNode::IntegerLiteral(integer * sign)))
            }
            Rule::float_literal => {
                let string = self.as_str().to_lowercase().trim_end_matches("f").to_string();
                return match string.as_str().parse::<f32>() {
                    Ok(f) => Ok(Box::new(AstNode::FloatLiteral(f))),
                    Err(e) => Err(Error::new_from_span(ErrorVariant::CustomError { message: e.to_string() }, self.as_span()))
                }
            }
            Rule::object_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::ObjectLiteral {
                    type_: inner_iter.next().unwrap().as_str().to_string(),
                    reference: inner_iter.next().unwrap().into_inner().next().unwrap().as_str().to_string()
                }))
            }
            Rule::new => {
                let inner_pairs: Vec<Pair<Rule>> = self.into_inner().into_iter().collect();
                let (type_, arguments) = inner_pairs.split_last().unwrap();
                // TODO: this is wrong, needs to be an EXPRESSION after this!!
                Ok(Box::new(AstNode::New {
                    type_: type_.clone().try_into()?,
                    arguments: parse_arguments(arguments)?
                }))
            }
            Rule::cast => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::Cast {
                    type_: inner_iter.next().unwrap().try_into()?,
                    operand: inner_iter.next().unwrap().try_into()?,
                }))
            }
            Rule::defaultproperties => {
                Ok(Box::new(AstNode::DefaultProperties {
                    lines: self.into_inner().into_iter().map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                }))
            }
            Rule::name_literal => {
                Ok(Box::new(AstNode::NameLiteral(self.into_inner().next().unwrap().as_str().to_string())))
            }
            Rule::string_literal => {
                Ok(Box::new(AstNode::StringLiteral(self.into_inner().next().unwrap().as_str().to_string())))
            }
            Rule::vector_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::VectorLiteral {
                    x: inner_iter.next().unwrap().try_into()?,
                    y: inner_iter.next().unwrap().try_into()?,
                    z: inner_iter.next().unwrap().try_into()?
                }))
            }
            Rule::array_type => {
                Ok(Box::new(AstNode::ArrayType(self.into_inner().next().unwrap().try_into()?)))
            }
            Rule::rotator_literal => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::RotatorLiteral {
                    pitch: inner_iter.next().unwrap().try_into()?,
                    yaw: inner_iter.next().unwrap().try_into()?,
                    roll: inner_iter.next().unwrap().try_into()?
                }))
            }
            Rule::type_ => {
                self.into_inner().next().unwrap().try_into()
            }
            Rule::var_name => {
                let mut inner_iter = self.into_inner().into_iter();
                let name = inner_iter.next().unwrap().as_str().to_string();
                let mut size: Option<Box<AstNode>> = None;
                if let Some(pair) = inner_iter.next() {
                    size = Some(pair.try_into()?)
                }
                Ok(Box::new(AstNode::VarName {
                    name,
                    size
                }))
            }
            Rule::var_declaration => {
                let mut category: Option<String> = None;
                let mut modifiers: Vec<VarModifier> = Vec::new();
                let mut type_: Option<Box<AstNode>> = None;
                let mut names: Vec<Box<AstNode>> = Vec::new();
                let inner_iter = self.into_inner().into_iter();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::var_category => {
                            let inner = pair.into_inner().next();
                            category = match inner {
                                Some(pair) => Some(pair.as_str().to_string()),
                                None => Some(String::new())
                            }
                        }
                        Rule::var_modifier => {
                            modifiers.push(VarModifier::from_str(pair.as_str()).unwrap())
                        },
                        Rule::type_ => {
                            type_ = Some(pair.try_into()?)
                        },
                        Rule::var_name => {
                            names.push(pair.try_into()?)
                        }
                        _ => { panic!("invalid pair") }
                    }
                }
                Ok(Box::new(AstNode::VarDeclaration {
                    category,
                    modifiers,
                    type_: type_.unwrap(),
                    names
                }))
            }
            Rule::unqualified_identifier => {
                Ok(Box::new(AstNode::UnqualifiedIdentifier(self.as_str().to_string())))
            },
            Rule::function_modifier => {
                let mut inner_iter = self.into_inner().into_iter();
                Ok(Box::new(AstNode::FunctionModifier {
                    type_: FunctionModifier::from_str(inner_iter.next().unwrap().as_str()).unwrap(),
                    arguments: inner_iter.map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                }))
            }
            Rule::function_argument => {
                let mut modifiers = Vec::new();
                let mut type_: Option<Box<AstNode>> = None;
                let mut name: Option<Box<AstNode>> = None;
                let inner_iter = self.into_inner().into_iter();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::function_argument_modifier => {
                            modifiers.push(FunctionArgumentModifier::from_str(pair.as_str()).unwrap())
                        }
                        Rule::type_ => {
                            type_ = Some(pair.try_into()?)
                        }
                        Rule::var_name => {
                            name = Some(pair.try_into()?)
                        }
                        _ => { panic!("unhandled rule") }
                    }
                }
                Ok(Box::new(AstNode::FunctionArgument {
                    modifiers,
                    type_: type_.unwrap(),
                    name: name.unwrap()
                }))
            }
            Rule::function_declaration => {
                let mut modifiers: Vec<Box<AstNode>> = Vec::new();
                let mut type_: Option<Box<AstNode>> = None;
                let mut return_type: Option<Box<AstNode>> = None;
                let mut arguments: Vec<Box<AstNode>> = Vec::new();
                let mut name: String = String::new();
                let mut body: Option<Box<AstNode>> = None;
                let inner_iter = self.into_inner().into_iter();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::function_type => {
                            let inner_pair = pair.into_inner().into_iter().next().unwrap();
                            match inner_pair.as_rule() {
                                Rule::function_type_no_arguments => {
                                    type_ = Some(Box::new(AstNode::FunctionType {
                                        type_: inner_pair.as_str().to_string(),
                                        arguments: None
                                    }))
                                }
                                Rule::function_type_operator => {
                                    type_ = Some(Box::new(AstNode::FunctionType {
                                        type_: "operator".to_string(),  // TODO: kinda sloppy
                                        arguments: Some(inner_pair.into_inner().into_iter().map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?)
                                    }))
                                }
                                _ => panic!("unhandled rule")
                            }
                        }
                        Rule::function_modifier => {
                            modifiers.push(pair.try_into()?)
                        }
                        Rule::type_ => {
                            return_type = Some(pair.try_into()?)
                        }
                        Rule::function_name => {
                            name = pair.as_str().to_string()
                        }
                        Rule::function_argument => {
                            arguments.push(pair.try_into()?)
                        }
                        Rule::function_body => {
                            body = Some(pair.try_into()?);
                        }
                        _ => { panic!("unhandled pair") }
                    }
                }
                Ok(Box::new(AstNode::FunctionDelaration {
                    type_: type_.unwrap(),
                    modifiers,
                    return_type,
                    name,
                    arguments,
                    body
                }))
            }
            Rule::function_body => {
                let inner_iter = self.into_inner().into_iter();
                let mut locals: Vec<Box<AstNode>> = Vec::new();
                let mut statements: Vec<Box<AstNode>> = Vec::new();
                for pair in inner_iter {
                    match pair.as_rule() {
                        Rule::local_declaration => {
                            let mut inner_iter = pair.into_inner();
                            locals.push(Box::new(AstNode::LocalDeclaration {
                                type_: inner_iter.next().unwrap().try_into()?,
                                names: inner_iter.into_iter().map(|pair| pair.try_into()).collect::<Result<Vec<Box<AstNode>>, _>>()?
                            }))
                        },
                        _ => { statements.push(pair.try_into()?); }
                    }
                }
                Ok(Box::new(AstNode::FunctionBody {
                    locals,
                    statements
                }))
            }
            Rule::var_size => {
                self.into_inner().next().unwrap().try_into()
            }
            Rule::parenthetical_expression => {
                Ok(Box::new(AstNode::ParentheticalExpression { expression: self.into_inner().next().unwrap().try_into()? }))
            }
            Rule::jump_label => {
                Ok(Box::new(AstNode::JumpLabel(self.into_inner().next().unwrap().as_str().to_string())))
            }
            Rule::struct_declaration => {
                let rules_itr = self.into_inner().into_iter();
                let mut modifiers: Vec<StructModifier> = Vec::new();
                let mut parent: Option<String> = None;
                let mut name: String = String::new();
                let mut members: Vec<Box<AstNode>> = Vec::new();
                for pair in rules_itr {
                    match pair.as_rule() {
                        Rule::struct_modifier => {
                            modifiers.push(StructModifier::from_str(pair.as_str()).unwrap())
                        }
                        Rule::unqualified_identifier => {
                            name.add_assign(pair.as_str())
                        }
                        Rule::extends => {
                            parent = Some(pair.into_inner().next().unwrap().as_str().to_string())
                        }
                        Rule::struct_var_declaration => {
                            let mut editable = false;
                            let mut modifiers: Vec<String> = Vec::new();
                            let mut type_: Option<Box<AstNode>> = None;
                            let mut names: Vec<Box<AstNode>> = Vec::new();
                            let inner_iter = pair.into_inner().into_iter();
                            for pair in inner_iter {
                                match pair.as_rule() {
                                    Rule::struct_var_editable => {
                                        editable = true
                                    }
                                    Rule::var_modifier => {
                                        modifiers.push(pair.as_str().to_string())
                                    },
                                    Rule::type_ => {
                                        type_ = Some(pair.try_into()?)
                                    },
                                    Rule::var_name => {
                                        names.push(pair.try_into()?)
                                    }
                                    _ => { panic!("Invalid state") }
                                }
                            }
                            members.push( Box::new(AstNode::StructVarDeclaration {
                                editable,
                                modifiers,
                                type_: type_.unwrap(),
                                names,
                            }));
                        }
                        _ => { }
                    }
                }
                Ok(Box::new(AstNode::StructDeclaration {
                    modifiers,
                    name,
                    parent,
                    members
                }))
            },
            Rule::cpptext => {
                Ok(Box::new(AstNode::CppText(self.into_inner().into_iter().next().unwrap().as_str().to_string())))
            }
            _ => { Ok(Box::new(AstNode::Unhandled)) }   // TODO: just return an error
        }
    }
}

#[derive(Debug)]
pub enum ParsingError {
    IoError(std::io::Error),
    EncodingError(String),
    PestError(pest::error::Error<Rule>)
}

impl From<std::io::Error> for ParsingError {
    fn from(error: std::io::Error) -> Self {
        ParsingError::IoError(error)
    }
}

impl From<pest::error::Error<Rule>> for ParsingError {
    fn from(error: pest::error::Error<Rule>) -> Self {
        ParsingError::PestError(error)
    }
}

fn read_file_to_string(path: &str) -> Result<String, ParsingError> {
    let mut file = File::open(path)?;
    let mut buffer: Vec<u8> = Vec::new();
    file.read_to_end(&mut buffer)?;
    encoding::all::WINDOWS_1252
        .decode(&mut buffer, DecoderTrap::Strict)
        .map_err(|e| ParsingError::EncodingError(e.to_string()))
}

pub fn parse_program(contents: &str) -> Result<Pair<Rule>, ParsingError> {
    Ok(UnrealScriptParser::parse(Rule::program, contents)?.into_iter().next().unwrap())
}

pub fn parse_file(path: &str) -> Result<Box<AstNode>, ParsingError> {
    Ok(parse_program(read_file_to_string(path)?.as_str())?.try_into()?)
}

// PYTHON STUFF
use pyo3::prelude::{pymodule};
use pyo3::create_exception;
use pyo3::types::{PyDict};
use pyo3::{Python, PyResult, exceptions, PyNativeType, ToPyObject, PyObject};
use pyo3::prelude::PyModule;
use encoding::{DecoderTrap, Encoding};

create_exception!(unrealscriptplus, ScriptParseError, pyo3::exceptions::PyException);

#[pymodule]
fn unrealscriptplus(py: Python, m: &PyModule) -> PyResult<()> {

    fn pest_error_to_py_object(py: Python, e: pest::error::Error<Rule>) -> PyObject {
        let sequence = vec![
            (
                "line_col".to_object(py),
                match e.line_col {
                    LineColLocation::Pos(pos) => pos.to_object(py),
                    LineColLocation::Span(span1, span2) => (span1, span2).to_object(py)
                }
            ).to_object(py),
            (
                "location".to_object(py),
                match e.location {
                    InputLocation::Pos(pos) => pos.to_object(py),
                    InputLocation::Span(span) => span.to_object(py)
                }
            ).to_object(py),
            (
                "variant".to_object(py),
                match e.variant {
                    ErrorVariant::ParsingError { positives, negatives } => {
                        let positives: Vec<String> = positives.into_iter().map(|r| format!("{:?}", r).to_string()).collect();
                        let negatives: Vec<String> = negatives .into_iter().map(|r| format!("{:?}", r).to_string()).collect();
                        PyDict::from_sequence(py, vec![
                            ("type".to_object(py), "parsing".to_object(py)).to_object(py),
                            ("positives".to_object(py), positives.to_object(py)).to_object(py),
                            ("negatives".to_object(py), negatives.to_object(py)).to_object(py),
                        ].to_object(py)).unwrap().to_object(py)
                    },
                    ErrorVariant::CustomError { message } => {
                        PyDict::from_sequence(py, vec![
                            ("type".to_object(py), "custom".to_object(py)).to_object(py),
                            ("message".to_object(py), message.to_object(py)).to_object(py)
                        ].to_object(py)).unwrap().to_object(py)
                    },
                }
            ).to_object(py)
        ].to_object(py);
        PyDict::from_sequence(py, sequence).unwrap().to_object(py)
    }

    #[pyfn(m)]
    #[pyo3(pass_module)]
    fn parse_file(module: &PyModule, path: &str) -> PyResult<bool> {
        match crate::parse_file(path) {
            Ok(_) => Ok(true),
            Err(e) => {
                Err(match e {
                    ParsingError::PestError(e) => ScriptParseError::new_err(pest_error_to_py_object(module.py(), e)),
                    ParsingError::EncodingError(e) => exceptions::PyUnicodeEncodeError::new_err(e.to_string()),
                    ParsingError::IoError(e) => exceptions::PyIOError::new_err(e.to_string())
                })
            }
        }
    }
    m.add("ScriptParseError", py.get_type::<ScriptParseError>())?;
    Ok(())
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

    // TODO: jump_label

    #[test]
    fn comment_multi_line_nested() {
        // TODO:
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
        );
        // TODO: this is good but WAY too verbose, figure out a more compact way to run this test
        match UnrealScriptParser::parse(Rule::float_literal, "123.456f") {
            Ok(mut node) => {
                let bigbox: Box<AstNode> = node.next().unwrap().try_into().unwrap();
                match bigbox.as_ref() {
                    AstNode::FloatLiteral(value) => {
                        assert_eq!(*value, 123.456);
                    },
                    _ => panic!("incorrect parse result")
                }
            },
            Err(error) => panic!("{:?}", error)
        }
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
    fn object_literal_with_underscore() {
        parses_to!(
            parser: UnrealScriptParser,
            input: "class'Foo_Bar'",
            rule: Rule::object_literal,
            tokens: [ object_literal(0, 14, [
                unqualified_identifier(0, 5),
                single_quoted_string(5, 14, [ single_quoted_string_inner(6, 13) ])
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
                    extends(10, 21, [ identifier(18, 21) ])
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
                        expression(29, 32, [ unqualified_identifier(29, 32) ])
                    ])
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
                        expression(32, 35, [ unqualified_identifier(32, 35) ])
                    ])
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
                        expression(27, 28, [ literal(27, 28, [ integer_literal(27, 28, [ integer_literal_decimal(27, 28) ]) ]) ]),
                        expression(30, 31, [ literal(30, 31, [ integer_literal(30, 31, [ integer_literal_decimal(30, 31) ]) ]) ]),
                        expression(33, 34, [ literal(33, 34, [ integer_literal(33, 34, [ integer_literal_decimal(33, 34) ]) ]) ]),
                        expression(36, 37, [ literal(36, 37, [ integer_literal(36, 37, [ integer_literal_decimal(36, 37) ]) ]) ]),
                    ])
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
                unqualified_identifier(10, 13)
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
                unqualified_identifier(0, 3)
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
                        expression(37, 40, [ unqualified_identifier(37, 40) ]),
                        expression(42, 45, [ unqualified_identifier(42, 45) ]),
                        expression(47, 50, [ unqualified_identifier(47, 50) ])
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
                    unqualified_identifier(16, 19)
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
                    extends(11, 22, [ identifier(19, 22) ])
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
                    function_type(0, 8, [ function_type_no_arguments(0, 8) ]),
                    function_name(9, 12, [ unqualified_identifier(9, 12) ])
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
                    function_type(0, 8, [ function_type_no_arguments(0, 8) ]),
                    type_(9, 12, [ identifier(9, 12) ]),
                    function_name(13, 16, [ unqualified_identifier(13, 16) ])
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
                    function_type(0, 8, [ function_type_no_arguments(0, 8) ]),
                    function_name(9, 12, [ unqualified_identifier(9, 12) ]),
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
            input: "reliable if (Foo) Bar;",
            rule: Rule::replication_statement,
            tokens: [
                replication_statement(0, 22, [
                    reliability(0, 8),
                    expression(13, 16, [ unqualified_identifier(13, 16) ]),
                    unqualified_identifier(18, 21)
                ])
            ]
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
                    reliability(0, 8),
                    expression(13, 16, [ unqualified_identifier(13, 16) ]),
                    unqualified_identifier(18, 21),
                    unqualified_identifier(23, 26)
                ])
            ]
        }
    }

    // TODO: operator tests!

    // TODO: compiler directive tests
    #[test]
    fn compiler_directive_start_of_line() {
        parses_to! {
            parser: UnrealScriptParser,
            input: "#exec OBJ LOAD FILE=..\\Foo\\Bar.utx",
            rule: Rule::compiler_directive,
            tokens: [ compiler_directive(0, 34, [ compiler_directive_inner(1, 34), EOI(34, 34) ]) ]
        }
    }
}
