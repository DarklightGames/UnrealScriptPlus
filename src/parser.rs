extern crate pest;

use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "UnrealScript.pest"]
pub struct UnrealScriptParser;

use super::ast::AstNode;

use pest_consume::{Error, match_nodes};
use std::str::FromStr;
use crate::ast::{ReplicationReliability, StateModifier};
use std::convert::TryFrom;
use self::pest::state;
use self::pest::iterators::Pair;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

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

#[pest_consume::parser]
impl UnrealScriptParser {

    fn cast(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [class_type(type_), expression(operand)] => Ok(Box::new(AstNode::Cast { type_, operand }))
        )
    }

    fn parenthetical_expression(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [expression(expression)] => Ok(Box::new(AstNode::ParentheticalExpression { expression }))
        )
    }

    fn float_literal(input: Node) -> Result<Box<AstNode>> {
        input.as_str().to_lowercase().to_string().as_str().parse::<f32>()
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(Box::new(AstNode::FloatLiteral(v))))
    }

    fn single_quoted_string(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn name_literal(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::NameLiteral(input.as_str().to_string())))
    }

    fn string_literal(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::StringLiteral(input.as_str().to_string())))
    }

    fn integer_literal_hexadecimal(input: Node) -> Result<i32> {
        u32::from_str_radix(input.as_str(), 16)
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(i32::from_be_bytes(v.to_be_bytes())))
    }

    fn integer_literal_decimal(input: Node) -> Result<i32> {
        input.as_str().parse::<i32>()
            .map_err(|e| input.error(e))
    }

    fn numeric_sign(input: Node) -> Result<i32> {
        return if input.as_str() == "-" { Ok(-1) } else { Ok(1) }
    }

    fn integer_literal(input: Node) -> Result<Box<AstNode>> {
        let mut sign = 1;
        let mut integer = 0;
        match_nodes_any!(input.into_children();
            numeric_sign(s) => sign = s,
            integer_literal_hexadecimal(s) => integer = s,
            integer_literal_decimal(s) => integer = s
        );
        Ok(Box::new(AstNode::IntegerLiteral(integer)))
    }

    fn unqualified_identifier(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::UnqualifiedIdentifier(input.as_str().to_string())))
    }

    fn qualified_identifier(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::QualifiedIdentifier(input.as_str().to_string())))
    }

    fn identifier(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::Identifier(input.as_str().to_string())))
    }

    fn class_within(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(id)
        )
    }

    fn object_literal(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(type_), single_quoted_string(reference)] => {
                return Ok(Box::new(AstNode::ObjectLiteral { type_, reference }))
            }
        );
    }

    fn numeric_literal(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [integer_literal(n)] => return Ok(n),
            [float_literal(f)] => return Ok(f)
        )
    }

    fn vector_literal(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [numeric_literal(x), numeric_literal(y), numeric_literal(z)] => {
                return Ok(Box::new(AstNode::VectorLiteral { x, y, z }))
            }
        );
    }

    fn rotator_literal(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [numeric_literal(pitch), numeric_literal(yaw), numeric_literal(roll)] => {
                return Ok(Box::new(AstNode::RotatorLiteral { pitch, yaw, roll }))
            }
        )
    }

    fn compiler_directive(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [inner] => {
                return Ok(Box::new(AstNode::CompilerDirective(inner.as_str().to_string())))
            }
        )
    }

    fn extends(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(id)
        )
    }

    fn class_modifier_type(input: Node) -> Result<String> {
        Ok(input.as_str().to_lowercase().to_string())
    }

    fn class_modifier(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [class_modifier_type(type_), expression(arguments)..] => {
                Ok(Box::new(AstNode::ClassModifier {
                    type_,
                    arguments: arguments.collect()
                }))
            }
        )
    }

    fn var_category(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(id)] => Ok(id),
            // we still want to mark the category as present, even if it's empty
            [] => Ok(Box::new(AstNode::UnqualifiedIdentifier(String::new())))
        )
    }

    // https://stackoverflow.com/questions/69612563/pattern-matching-in-rust-macro
    fn struct_var_editable(input: Node) -> Result<String> {
        Ok(String::from("deleteme"))
    }

    fn struct_var_declaration(input: Node) -> Result<Box<AstNode>> {
        let mut editable = false;
        let mut modifiers: Vec<String> = Vec::new();
        let mut type_: Option<Box<AstNode>> = None;
        let mut names: Vec<Box<AstNode>> = Vec::new();
        match_nodes_any!(input.into_children();
            struct_var_editable(e) => editable = true,
            var_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => names.push(n)
        );
        Ok(Box::new(AstNode::StructVarDeclaration {
            names,
            type_: type_.unwrap(),
            modifiers,
            editable
        }))
    }

    fn struct_modifier(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn struct_declaration(input: Node) -> Result<Box<AstNode>> {
        let mut modifiers: Vec<String> = Vec::new();
        let mut parent: Option<Box<AstNode>> = None;
        let mut name: Option<Box<AstNode>> = None;
        let mut members: Vec<Box<AstNode>> = Vec::new();
        let mut cpp = None;
        match_nodes_any!(input.into_children();
            struct_modifier(m) => modifiers.push(m),
            unqualified_identifier(id) => name = Some(id),
            extends(e) => parent = Some(e),
            struct_var_declaration(v) => members.push(v),
            cppstruct(s) => cpp = Some(s)
        );
        Ok(Box::new(AstNode::StructDeclaration {
            modifiers,
            name: name.unwrap(),
            parent,
            members,
            cpp
        }))
    }

    fn enum_declaration(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(name), unqualified_identifier(values)..] => {
                Ok(Box::new(AstNode::EnumDeclaration {
                    name,
                    values: values.collect()
                }))
            }
        )
    }

    fn array_type(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [type_(t)] => Ok(Box::new(AstNode::ArrayType(t)))
        )
    }

    fn class_type(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [identifier(id)] => Ok(Box::new(AstNode::ClassType(id)))
        )
    }

    fn type_(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [struct_declaration(s)] => Ok(s),
            [enum_declaration(e)] => Ok(e),
            [array_type(a)] => Ok(a),
            [class_type(c)] => Ok(c),
            [identifier(i)] => Ok(i)
        )
    }

    fn var_size(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(id)] => Ok(id),
            [integer_literal(n)] => Ok(n)
        )
    }

    fn var_name(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(name)] => Ok(Box::new(AstNode::VarName {
                name,
                size: None
            })),
            [unqualified_identifier(name), var_size(size)] => Ok(Box::new(AstNode::VarName {
                name,
                size: Some(size)
            }))
        )
    }

    fn var_modifier(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn var_declaration(input: Node) -> Result<Box<AstNode>> {
        let mut category: Option<Box<AstNode>> = None;
        let mut modifiers: Vec<String> = Vec::new();
        let mut type_: Option<Box<AstNode>> = None;
        let mut names: Vec<Box<AstNode>> = Vec::new();
        match_nodes_any!(input.into_children();
            var_category(c) => category = Some(c),
            var_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => names.push(n)
        );
        Ok(Box::new(AstNode::VarDeclaration {
            category,
            modifiers,
            type_: type_.unwrap(),
            names
        }))
    }

    fn class_declaration(input: Node) -> Result<Box<AstNode>> {
        let mut parent_class: Option<Box<AstNode>> = None;
        let mut modifiers: Vec<Box<AstNode>> = Vec::new();
        let mut within: Option<Box<AstNode>> = None;
        match_nodes!(input.into_children();
            [unqualified_identifier(name), nodes..] => {
                match_nodes_any!(nodes;
                    extends(e) => parent_class = Some(e),
                    class_within(w) => within = Some(w),
                    class_modifier(m) => modifiers.push(m)
                );
                Ok(Box::new(AstNode::ClassDeclaration {
                    name,
                    parent_class,
                    modifiers,
                    within
                }))
            }
        )
    }

    fn literal(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [vector_literal(v)] => Ok(v),
            [rotator_literal(r)] => Ok(r),
            [numeric_literal(n)] => Ok(n),
            [string_literal(s)] => Ok(s),
            [name_literal(n)] => Ok(n),
            [object_literal(o)] => Ok(o),
        )
    }

    fn boolean_literal(input: Node) -> Result<Box<AstNode>> {
        bool::from_str(input.as_str())
            .map_err(|e| input.error(e))
            .and_then(|v| Ok(Box::new(AstNode::BooleanLiteral(v))))
    }

    fn const_value(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [literal(l)] => Ok(l),
            [boolean_literal(l)] => Ok(l)
        )
    }

    fn const_declaration(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(name), const_value(value)] => {
                Ok(Box::new(AstNode::ConstDeclaration {
                    name,
                    value
                }))
            }
        )
    }

    fn function_modifier_type_with_arguments(input: Node) -> Result<String> {
        Ok(input.as_str().to_lowercase().as_str().to_string())
    }

    fn function_modifier_type_no_arguments(input: Node) -> Result<String> {
        Ok(input.as_str().to_lowercase().as_str().to_string())
    }

    fn function_modifier(input: Node) -> Result<Box<AstNode>> {
        let mut type_: String = String::new();
        let mut arguments: Vec<Box<AstNode>> = Vec::new();
        match_nodes!(input.into_children();
            [function_modifier_type_with_arguments(t), integer_literal(n)] => {
                type_ = t;
                arguments.push(n);
            },
            [function_modifier_type_no_arguments(t)] => {
                type_ = t;
            }
        );
        Ok(Box::new(AstNode::FunctionModifier { type_, arguments }))
    }

    fn function_type_with_arguments_type(input: Node) -> Result<String> {
        Ok(input.as_str().to_lowercase().to_string())
    }

    fn function_type_with_arguments(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [function_type_with_arguments_type(t), integer_literal(i)] => {
                Ok(Box::new(AstNode::FunctionType {
                    type_: t,
                    arguments: Some(vec![i])
                }))
            }
        )
    }

    fn function_type_no_arguments(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::FunctionType {
            type_: input.as_str().to_lowercase().to_string(),
            arguments: None
        }))
    }

    fn function_type(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [function_type_no_arguments(t)] => Ok(t),
            [function_type_with_arguments(t)] => Ok(t)
        )
    }

    fn function_name(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn function_argument(input: Node) -> Result<Box<AstNode>> {
        let mut modifiers = Vec::new();
        let mut type_: Option<Box<AstNode>> = None;
        let mut name: Option<Box<AstNode>> = None;
        match_nodes_any!(input.into_children();
            function_modifier(m) => modifiers.push(m),
            type_(t) => type_ = Some(t),
            var_name(n) => name = Some(n)
        );
        Ok(Box::new(AstNode::FunctionArgument {
            modifiers,
            type_: type_.unwrap(),
            name: name.unwrap()
        }))
    }

    fn local_declaration(input: Node) -> Result<Box<AstNode>> {
        let names: Vec<Box<AstNode>> = Vec::new();
        match_nodes!(input.into_children();
            [type_(t), var_name(names)..] => {
                Ok(Box::new(AstNode::LocalDeclaration {
                    type_: t,
                    names: names.collect()
                }))
            }
        )
    }

    fn jump_label(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::JumpLabel(input.as_str().to_string())))
    }

    fn statement_or_block(input: Node) -> Result<Vec<Box<AstNode>>> {
        match_nodes!(input.into_children();
            [code_statement(s)..] => Ok(s.collect())
        )
    }

    fn if_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [expression(predicate), statement_or_block(statements)] => {
                Ok(Box::new(AstNode::IfStatement { predicate, statements }))
            }
        )
    }

    fn elif_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [expression(predicate), statement_or_block(statements)] => {
                Ok(Box::new(AstNode::ElifStatement { predicate, statements }))
            }
        )
    }

    fn else_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [statement_or_block(statements)] => Ok(Box::new(AstNode::ElseStatement { statements }))
        )
    }

    fn if_statement_empty(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [expression(predicate)] => Ok(Box::new(AstNode::IfStatement {
                predicate, statements: vec![]
            }))
        )
    }

    fn conditional_statement(input: Node) -> Result<Box<AstNode>> {
        let mut if_statement = None;
        let mut elif_statements = Vec::new();
        let mut else_statement = None;
        match_nodes_any!(input.into_children();
            if_statement_empty(i) => if_statement = Some(i),
            if_statement(i) => if_statement = Some(i),
            elif_statement(e) => elif_statements.push(e),
            else_statement(e) => else_statement = Some(e)
        );
        Ok(Box::new(AstNode::ConditionalStatement {
            if_statement: if_statement.unwrap(),
            elif_statements,
            else_statement
        }))
    }

    fn foreach_expression(input: Node) -> Result<Box<AstNode>> {
        Self::expression(input)
    }

    fn foreach_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [foreach_expression(predicate), statement_or_block(statements)] => {
                Ok(Box::new(AstNode::ForEachStatement { predicate, statements }))
            }
        )
    }

    fn for_init(input: Node) -> Result<Box<AstNode>> {
        Self::expression(input.into_children().single()?)
    }

    fn for_predicate(input: Node) -> Result<Box<AstNode>> {
        Self::expression(input.into_children().single()?)
    }

    fn for_post(input: Node) -> Result<Box<AstNode>> {
        Self::expression(input.into_children().single()?)
    }

    fn for_statement(input: Node) -> Result<Box<AstNode>> {
        let mut init = None;
        let mut predicate = None;
        let mut post = None;
        let mut statements = Vec::new();
        match_nodes_any!(input.into_children();
            for_init(i) => init = Some(i),
            for_predicate(p) => predicate = Some(p),
            for_post(p) => post = Some(p),
            statement_or_block(s) => statements = s
        );
        Ok(Box::new(AstNode::ForStatement { init, predicate, post, statements }))
    }

    fn while_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [expression(predicate), statement_or_block(statements)] => {
                Ok(Box::new(AstNode::WhileStatement { predicate, statements }))
            }
        )
    }

    fn do_until_statement(input: Node) -> Result<Box<AstNode>> {
        let mut statements = Vec::new();
        let mut predicate = None;
        match_nodes_any!(input.into_children();
            statement_or_block(s) => statements = s,
            expression(e) => predicate = Some(e)
        );
        Ok(Box::new(AstNode::DoUntilStatement { statements, predicate }))
    }

    fn return_statement(input: Node) -> Result<Box<AstNode>> {
        let mut expression = None;
        match_nodes_any!(input.into_children();
            expression(e) => expression = Some(e)
        );
        Ok(Box::new(AstNode::ReturnStatement { expression }))
    }

    fn break_statement(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::BreakStatement))
    }

    fn continue_statement(input: Node) -> Result<Box<AstNode>> {
        Ok(Box::new(AstNode::ContinueStatement))
    }

    fn goto_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(label)] => {
                Ok(Box::new(AstNode::GotoStatement { label }))
            }
        )
    }

    fn switch_case_label(input: Node) -> Result<Box<AstNode>> {
        Self::expression(input)
    }

    fn switch_case(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [switch_case_label(predicate), code_statement(statements)..] => {
                Ok(Box::new(AstNode::SwitchCase { predicate, statements: statements.collect() }))
            }
        )
    }

    fn switch_default_case(input: Node) -> Result<Box<AstNode>> {
        // TODO: maybe?!
        match_nodes!(input.into_children();
            [code_statement(statements)..] => {
                Ok(Box::new(AstNode::SwitchDefaultCase { statements: statements.collect() }))
            }
        )
    }

    fn switch_statement(input: Node) -> Result<Box<AstNode>> {
        let mut predicate = None;
        let mut cases = Vec::new();
        match_nodes!(input.into_children();
            [expression(e), nodes..] => {
                predicate = Some(e);
                match_nodes_any!(nodes;
                    switch_case(c) => cases.push(c),
                    switch_default_case(c) => cases.push(c)
                )
            }
        );
        Ok(Box::new(AstNode::SwitchStatement { predicate: predicate.unwrap(), cases}))
    }

    fn code_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [jump_label(v)] => Ok(v),
            [conditional_statement(v)] => Ok(v),
            [foreach_statement(v)] => Ok(v),
            [for_statement(v)] => Ok(v),
            [while_statement(v)] => Ok(v),
            [do_until_statement(v)] => Ok(v),
            [return_statement(v)] => Ok(v),
            [break_statement(v)] => Ok(v),
            [continue_statement(v)] => Ok(v),
            [goto_statement(v)] => Ok(v),
            [compiler_directive(v)] => Ok(v),
            [switch_statement(v)] => Ok(v),
            [const_declaration(v)] => Ok(v),
            [expression(v)] => Ok(v),
        )
    }

    fn function_body(input: Node) -> Result<Box<AstNode>> {
        let mut locals = Vec::new();
        let mut statements = Vec::new();
        match_nodes_any!(input.into_children();
            const_declaration(c) => statements.push(c),
            local_declaration(l) => locals.push(l),
            code_statement(c) => statements.push(c)
        );
        Ok(Box::new(AstNode::FunctionBody { locals, statements }))
    }

    fn function_declaration(input: Node) -> Result<Box<AstNode>> {
        let mut modifiers: Vec<Box<AstNode>> = Vec::new();
        let mut type_: Option<Box<AstNode>> = None;
        let mut return_type: Option<Box<AstNode>> = None;
        let mut arguments: Vec<Box<AstNode>> = Vec::new();
        let mut name: String = String::new();
        let mut body: Option<Box<AstNode>> = None;
        match_nodes_any!(input.into_children();
            function_modifier(m) => modifiers.push(m),
            function_type(t) => type_ = Some(t),
            type_(t) => return_type = Some(t),
            function_name(n) => name = n,
            function_argument(a) => arguments.push(a),
            function_body(b) => body = Some(b)
        );
        Ok(Box::new(AstNode::FunctionDeclaration {
            type_: type_.unwrap(),
            modifiers,
            return_type,
            arguments,
            name,
            body
        }))
    }

    fn replication_reliability(input: Node) -> Result<ReplicationReliability> {
        Ok(ReplicationReliability::from(input.as_str()))
    }

    fn replication_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [replication_reliability(reliability), expression(condition), unqualified_identifier(variables)..] => {
                Ok(Box::new(AstNode::ReplicationStatement {
                    reliability,
                    condition,
                    variables: variables.collect()
                }))
            }
        )
    }

    fn replication_block(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [replication_statement(statements)..] => {
                Ok(Box::new(AstNode::ReplicationBlock { statements: statements.collect() }))
            }
        )
    }

    fn state_modifier(input: Node) -> Result<StateModifier> {
        Ok(StateModifier::from(input.as_str()))
    }

    fn state_editable(_input: Node) -> Result<bool> {
        Ok(true)
    }

    fn state_ignores(input: Node) -> Result<Vec<Box<AstNode>>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(ids)..] => {
                Ok(ids.collect())
            }
        )
    }

    fn state_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [const_declaration(c)] => Ok(c),
            [function_declaration(f)] => Ok(f)
        )
    }

    fn state_label(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [unqualified_identifier(label), code_statement(statements)..] => {
                Ok(Box::new(AstNode::StateLabel {
                    label,
                    statements: statements.collect()
                }))
            }
        )
    }

    fn state_declaration(input: Node) -> Result<Box<AstNode>> {
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

    fn defaultproperties(input: Node) -> Result<Box<AstNode>> {
        todo!()
    }

    fn cpp_body(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn cpptext(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [cpp_body(b)] => Ok(Box::new(AstNode::CppText(b)))
        )
    }

    fn cppstruct(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [cpp_body(b)] => Ok(Box::new(AstNode::CppStruct(b)))
        )
    }

    fn class_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [compiler_directive(c)] => Ok(c),
            [const_declaration(c)] => Ok(c),
            [var_declaration(v)] => Ok(v),
            [enum_declaration(e)] => Ok(e),
            [struct_declaration(s)] => Ok(s),
            [function_declaration(f)] => Ok(f),
            [replication_block(r)] => Ok(r),
            [state_declaration(s)] => Ok(s),
            [defaultproperties(d)] => Ok(d),
            [cpptext(c)] => Ok(c)
        )
    }

    fn pre_class_statement(input: Node) -> Result<Box<AstNode>> {
        match_nodes!(input.into_children();
            [compiler_directive(c)] => Ok(c),
            [const_declaration(c)] => Ok(c)
        )
    }

    fn program(input: Node) -> Result<Box<AstNode>> {
        let mut statements = Vec::new();
        match_nodes_any!(input.into_children();
            pre_class_statement(s) => statements.push(s),
            class_declaration(c) => statements.push(c),
            class_statement(s) => statements.push(s)
        );
        Ok(Box::new(AstNode::Program { statements }))
    }

    fn expression(input: Node) -> Result<Box<AstNode>> {
        fn parse_target(nodes: &[Node]) -> Result<Box<AstNode>> {
            let (node, remaining) = nodes.split_last().unwrap();
            return match node.as_rule() {
                Rule::parenthetical_expression => {
                    let children: Vec<Node> = node.into_children().single()?.into_children()[..];
                    Ok(Box::new(AstNode::ParentheticalExpression {
                        expression: parse_expression(&children[..])?
                    }))
                }
                _ => Ok(Box::new(AstNode::Unknown))
            }
        }

        fn parse_expression(nodes: &[Node]) -> Result<Box<AstNode>> {
            let mut dyadic_verbs = Vec::new();
            for (index, node) in nodes.into_iter().enumerate() {
                match node.as_rule() {
                    Rule::dyadic_verb => {
                        let operator_precedence = OPERATOR_PRECEDENCES.get(node.as_str());
                        if let Some(operator_precedence) = operator_precedence {
                            dyadic_verbs.push((operator_precedence, index, node.as_str()))
                        } else {
                            return Err(node.error("encountered unregistered dyadic verb"));
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
                            lhs: parse_expression(&nodes[..index - 0]).unwrap(),
                            operator: operator.to_string(),
                            rhs: parse_expression(&nodes[index + 1..]).unwrap()
                        }));
                    },
                    _ => {}
                }
            }
            // Next, search for monadic verbs at the beginning and end of the expression
            match nodes.last().unwrap().as_rule() {
                Rule::monadic_post_verb => {
                    return Ok(Box::new(AstNode::MonadicPostExpression {
                        operator: nodes.last().unwrap().as_str().to_string(),
                        target: parse_target(&nodes[..nodes.len() - 1])?
                    }))
                }
                _ => {}
            }
            match nodes.first().unwrap().as_rule() {
                Rule::monadic_pre_verb => {
                    return Ok(Box::new(AstNode::MonadicPreExpression {
                        operator: nodes.first().unwrap().as_str().to_string(),
                        target: parse_target(&nodes[1..])?
                    }))
                },
                _ => {}
            }
            parse_target(nodes)
        }
        let nodes: Vec<Node> = input.into_children().collect();
        parse_expression(&nodes[..])
    }
}

pub fn parse_program(contents: &String) -> Result<Box<AstNode>> {
    let inputs = UnrealScriptParser::parse(Rule::expression, contents.as_str())?;
    let input = inputs.single()?;
    UnrealScriptParser::expression(input)
}

