use crate::ast::*;
use crate::parser::{ProgramError, ProgramErrorSeverity};

use convert_case::{Case, Casing};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::rc::Rc;
use if_chain::if_chain;

pub enum VisitorState {
    DefaultPropertiesObject,
    DefaultProperties,
    Function,
    State,
}

pub struct VisitorContext {
    pub state_stack: Vec<VisitorState>,
}

impl VisitorContext {
    pub fn new() -> VisitorContext {
        VisitorContext { state_stack: vec![] }
    }
}

impl VisitorContext {
    pub fn push_state(&mut self, state: VisitorState) {
        self.state_stack.push(state)
    }

    pub fn pop_state(&mut self) -> Option<VisitorState> {
        self.state_stack.pop()
    }

    pub fn top_state(&self) -> Option<&VisitorState> {
        self.state_stack.last()
    }
}

pub struct Visitor<'a> {
    pub errors: Vec<ProgramError>,
    contents: &'a str,
    context: VisitorContext,
    variables: HashMap<Identifier, Rc<AstNode<VarDeclaration>>>,
    types: HashSet<Identifier>,
}

impl Visitor<'_> {
    pub fn new(contents: &str) -> Visitor {
        Visitor {
            contents,
            errors: vec![],
            context: VisitorContext::new(),
            variables: HashMap::new(),
            types: HashSet::new()
        }
    }

    pub fn visit<D>(&mut self, data: &D)
        where D: Visit {
        data.visit(self)
    }

    pub fn warn(&mut self, message: &str, span: AstSpan) {
        self.errors.push(ProgramError {
            message: message.to_string(),
            span,
            severity: ProgramErrorSeverity::Warning,
        })
    }

    pub fn error(&mut self, message: &str, span: AstSpan) {
        self.errors.push(ProgramError {
            message: message.to_string(),
            span,
            severity: ProgramErrorSeverity::Error
        })
    }
}

pub trait Visit {
    fn visit(&self, visitor: &mut Visitor);
}

impl Visit for AstNode<ConstDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        let name = &self.name.string;
        if name.to_case(Case::ScreamingSnake).ne(name) {
            visitor.warn(
                format!("const variable names should be in SCREAMING_SNAKE_CASE (e.g., {:?})",
                        name.to_case(Case::ScreamingSnake)).as_str(),
                self.span)
        }
    }
}

impl Visit for AstNode<DefaultPropertiesStruct> {
    fn visit(&self, visitor: &mut Visitor) {
        self.assignments.iter().for_each(|a| a.visit(visitor))
    }
}

impl Visit for AstNode<DefaultPropertiesArray> {
    fn visit(&self, visitor: &mut Visitor) {
        self.elements.iter().flatten().for_each(|e| e.visit(visitor))
    }
}

impl Visit for AstNode<DefaultPropertiesValue> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            DefaultPropertiesValue::Literal(literal) => {
                literal.visit(visitor);
                if let Literal::Name(name) = &literal.data {
                    visitor.error(
                        format!("values in the defaultproperties block cannot be \
                        initialized with name literals; use string literal instead \
                        (e.g., {:?})", name).as_str(),
                        self.span
                    )
                }
            }
            DefaultPropertiesValue::Identifier(_) => {}
            DefaultPropertiesValue::Struct(s) => s.visit(visitor),
            DefaultPropertiesValue::Array(a) => a.visit(visitor),
        }
    }
}

impl Visit for Rc<AstNode<DefaultPropertiesObject>> {
    fn visit(&self, visitor: &mut Visitor) {
        visitor.context.push_state(VisitorState::DefaultPropertiesObject);
        self.statements.iter().for_each(|statement| statement.visit(visitor));
        visitor.context.pop_state();
    }
}

impl Visit for AstNode<DefaultPropertiesTarget> {
    fn visit(&self, _visitor: &mut Visitor) {
    }
}

impl Visit for AstNode<DefaultPropertiesAssignment> {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(VisitorState::DefaultPropertiesObject) = visitor.context.top_state() {
        } else {
            if_chain! {
                if let Some(value) = &self.value;
                if let Some(variable) = visitor.variables.get(&self.target.target);
                if let Type::Pod(pod_type) = &variable.type_;
                then {
                    match pod_type {
                        PodType::Byte => {
                            if let DefaultPropertiesValue::Literal(literal) = &value.data {
                                if let Literal::Numeric(numeric) = &literal.data {
                                    match numeric.data {
                                        NumericLiteral::Integer(i) => {
                                            if !self.is_array_assignment() && i == 0 {
                                                visitor.warn("redundant specification of default value", self.span)
                                            }
                                            if i < u8::MIN as i32 || i > u8::MAX as i32 {
                                                let bytes: [u8; 1] = [i.to_le_bytes()[0]];
                                                let effective_value = u8::from_le_bytes(bytes);
                                                visitor.warn(
                                                    format!("value {} is outside the range of [{}..{}]; effective value: {}", i, u8::MIN, u8::MAX, effective_value).as_str(),
                                                    self.span
                                                )
                                            }
                                        },
                                        _ => { visitor.warn("incorrect literal type", self.span) }
                                    }
                                }
                            }
                        }
                        PodType::Int | PodType::Float => {
                            if_chain! {
                                if !self.is_array_assignment();
                                if let DefaultPropertiesValue::Literal(literal) = &value.data;
                                if let Literal::Numeric(numeric) = &literal.data;
                                then {
                                    match numeric.data {
                                        NumericLiteral::Integer(i) => {
                                            if i == 0 {
                                                visitor.warn("redundant specification of default value", self.span)
                                            }
                                        }
                                        NumericLiteral::Float(f) => {
                                            if f == 0.0 {
                                                visitor.warn("redundant specification of default value", self.span)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        PodType::String | PodType::Name => {
                            if_chain! {
                                if !self.is_array_assignment();
                                if let DefaultPropertiesValue::Literal(literal) = &value.data;
                                if let Literal::String(s) = &literal.data;
                                if s.is_empty();
                                then { visitor.warn("redundant specification of default value", self.span) }
                            }
                        }
                        PodType::Bool => {
                            if_chain! {
                                if !self.is_array_assignment();
                                if let DefaultPropertiesValue::Literal(literal) = &value.data;
                                if let Literal::Boolean(b) = literal.data;
                                if b == false;
                                then { visitor.warn("redundant specification of default value", self.span) }
                            }
                        }
                    }
                }
            }
        }
        self.target.visit(visitor);
        if let Some(v) = &self.value {
            v.visit(visitor)
        }
    }
}

impl Visit for AstNode<Literal> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            Literal::None => {},
            Literal::Numeric(_) => {},
            Literal::Boolean(_) => {
                let contents = &visitor.contents[self.span.start..self.span.end];
                if contents != contents.to_lowercase().as_str() {
                    visitor.warn(
                        format!("boolean literals should be lowercase (use {:?} instead of {:?})",
                                contents.to_lowercase().as_str(),
                                contents).as_str(),
                        self.span
                    )
                }
            }
            Literal::String(_) => {}
            Literal::Name(_) => {}
            Literal::Rotator(values) => {
                for (index, value) in values.iter().enumerate() {
                    match &value.data {
                        NumericLiteral::Integer(n) => {
                            if *n < i16::MIN as i32 || *n > i16::MAX as i32 {
                                visitor.warn(
                                    format!("rotator literal component {} ({}) should be within the range [{}..{}]", index, n, i16::MIN, i16::MAX).as_str(),
                                    value.span
                                )
                            }
                        }
                        NumericLiteral::Float(f) => {
                            visitor.warn(
                                format!("rotator literal component {} ({}) should be an integer literal (e.g., \"{}\")",
                                        index,
                                        visitor.contents[value.span.start..value.span.end].to_string().as_str(),
                                        *f as i32
                                ).as_str(),
                                value.span
                            )
                        }
                    }
                }
            }
            Literal::Vector(_) => {}
            Literal::Object { type_, .. } => {
                match type_.to_lowercase().as_str() {
                    "class" | "enum" => {
                        if type_.to_lowercase().as_str() != type_.as_str() {
                            visitor.warn(
                                format!("type specifier for class and enum object literals should be lowercase (e.g., {} instead of {})",
                                    type_.to_lowercase(),
                                    type_.as_str()
                                ).as_str(),
                                type_.span
                            )
                        }
                    },
                    _ => {
                        // TODO: the correct thing to do here is check that the capitalization matches the capitalization the type was defined with
                        let first_letter_in_type = type_.chars().nth(0).unwrap();
                        if !first_letter_in_type.is_uppercase() {
                            visitor.warn(
                                format!("non-pod object literal types should begin with a capital letter (e.g., {})",
                                        type_.to_case(Case::Pascal)
                                ).as_str(),
                                type_.span
                            )
                        }
                    }
                }
            }
        }
    }
}

impl Visit for AstNode<DefaultPropertiesStatement> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            DefaultPropertiesStatement::Assignment(assignment) => assignment.visit(visitor),
            DefaultPropertiesStatement::Object(object) => object.visit(visitor)
        }
    }
}

impl Visit for AstNode<DefaultProperties> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.statements.is_empty() {
            visitor.warn("defaultproperties block should not be empty", self.span)
        }
        self.statements.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ReplicationBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.statements.is_empty() {
            visitor.warn("replication block should not be empty", self.span)
        }
        // TODO: check to see that reliability & expression are unique for each statement
        for (i, statement) in self.statements.iter().enumerate() {
            for next_statement in &self.statements[i+1..] {
                if statement.condition == next_statement.condition &&
                    statement.reliability == next_statement.reliability {
                    visitor.warn("identical replication reliability and condition", self.span)
                }
            }
        }
    }
}

impl Visit for AstNode<CodeBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        self.statements.iter().for_each(|s| s.visit(visitor))
    }
}

impl Visit for AstNode<CodeStatementOrBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        match self.deref() {
            CodeStatementOrBlock::CodeBlock(b) => b.visit(visitor),
            CodeStatementOrBlock::CodeStatement(s) => s.visit(visitor)
        }
    }
}

impl<D> Visit for Box<D>
    where D: Visit {
    fn visit(&self, visitor: &mut Visitor) {
        visitor.visit(self.deref())
    }
}

impl Visit for AstNode<FStringElement> {
    fn visit(&self, visitor: &mut Visitor) {
        if let FStringElement::Expression(expression) = &self.data {
            visitor.visit(expression)
        }
    }
}

impl Visit for AstNode<Expression> {
    fn visit(&self, visitor: &mut Visitor) {
        match self.deref() {
            Expression::Identifier(_) => {}
            Expression::Literal(l) => l.visit(visitor),
            Expression::New { type_, arguments } => {
                type_.visit(visitor);
                if let Some(arguments) = arguments {
                    arguments.visit(visitor);
                }
            }
            Expression::MonadicPreExpression { operand, verb: _ } => {
                operand.visit(visitor)
            }
            Expression::MonadicPostExpression { operand, verb: _ } => {
                operand.visit(visitor)
            }
            Expression::DyadicExpression { lhs, verb: _, rhs } => {
                lhs.visit(visitor);
                rhs.visit(visitor);
            }
            Expression::Call { operand, arguments } => {
                operand.visit(visitor);
                if let Some(arguments) = arguments {
                    arguments.visit(visitor);
                }
            }
            Expression::GlobalCall { name, arguments } => {
                name.visit(visitor);
                if let Some(arguments) = arguments {
                    arguments.visit(visitor);
                }
            }
            Expression::ArrayAccess { operand, argument } => {
                operand.visit(visitor);
                argument.visit(visitor);
            }
            Expression::DefaultAccess { operand, target } => {
                if let Some(operand) = operand {
                    operand.visit(visitor);
                }
                target.visit(visitor)
            }
            Expression::StaticAccess { operand, target } => {
                if let Some(operand) = operand {
                    operand.visit(visitor)
                }
                target.visit(visitor)
            }
            Expression::MemberAccess { operand, target } => {
                operand.visit(visitor);
                target.visit(visitor)
            }
            Expression::Cast { type_, operand } => {
                type_.visit(visitor);
                operand.visit(visitor);
            }
            Expression::ParentheticalExpression(expression) => {
                expression.visit(visitor);
                match &expression.data {
                    Expression::New { .. } => {}
                    Expression::DyadicExpression { .. } => {}
                    _ => {
                        visitor.warn("redundant parenthetical expression", expression.span)
                    }
                }
                expression.visit(visitor)
            }
            Expression::FString(fstring) => {
                fstring.elements.iter().for_each(|element| element.visit(visitor))
            }
        }
    }
}


impl Visit for ForEach {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);
        self.body.visit(visitor);
    }
}

impl Visit for AstNode<IfStatement> {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);
        if let Some(body) = &self.body {
            body.visit(visitor)
        }
    }
}

impl Visit for ElifStatement {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);
        self.body.visit(visitor);
    }
}

impl Visit for ElseStatement {
    fn visit(&self, visitor: &mut Visitor) {
        self.body.visit(visitor)
    }
}

impl Visit for ConditionalStatement {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(if_statement) = &self.if_statement.body {
            if_statement.visit(visitor)
        }
        self.elif_statements.iter().for_each(|e| e.visit(visitor));
        if let Some(else_statement) = &self.else_statement {
            else_statement.visit(visitor)
        }
    }
}

impl Visit for ForStatement {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(init) = &self.init {
            init.visit(visitor)
        }
        if let Some(predicate) = &self.predicate {
            predicate.visit(visitor)
        }
        if let Some(post) = &self.post {
            post.visit(visitor)
        }
        self.body.visit(visitor)
    }
}

impl Visit for SwitchCase {
    fn visit(&self, visitor: &mut Visitor) {
        self.statements.iter().for_each(|statement| statement.visit(visitor))
    }
}

impl Visit for SwitchStatement {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);
        self.cases.iter().for_each(|case| case.visit(visitor))
    }
}

impl Visit for WhileStatement {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);
        self.body.visit(visitor);
    }
}

impl Visit for DoUntil {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(predicate) = &self.predicate {
            predicate.visit(visitor)
        }
        self.body.visit(visitor)
    }
}

impl Visit for AstNode<CodeStatement> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            CodeStatement::Empty => {
                visitor.warn("empty statement", self.span)
            }
            CodeStatement::Expression(expression) => {
                if let Expression::DyadicExpression{lhs, verb: _, rhs} = &expression.data {
                    if let Expression::ParentheticalExpression(lhs) = &lhs.data {
                        visitor.warn("redundant parenthetical expression", lhs.span)
                    }
                    if let Expression::ParentheticalExpression(rhs) = &rhs.data {
                        visitor.warn("redundant parenthetical expression", rhs.span)
                    }
                }
                expression.visit(visitor)
            },
            CodeStatement::Return(expression) => {
                if let Some(expression) = expression {
                    expression.visit(visitor)
                }
            }
            CodeStatement::Break => {}
            CodeStatement::Continue => {}
            CodeStatement::Goto(_) => {}
            CodeStatement::JumpLabel(_) => {}
            CodeStatement::ForEach(f) => f.visit(visitor),
            CodeStatement::For(f) => f.visit(visitor),
            CodeStatement::Switch(s) => s.visit(visitor),
            CodeStatement::Conditional(c) => c.visit(visitor),
            CodeStatement::While(w) => w.visit(visitor),
            CodeStatement::DoUntil(d) => d.visit(visitor),
            CodeStatement::CompilerDirective(_) => {}
            CodeStatement::ConstDeclaration(c) => c.visit(visitor)
        }
    }
}

impl Visit for AstNode<Identifier> {
    fn visit(&self, _visitor: &mut Visitor) {
    }
}

impl Visit for AstNode<VarSize> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            VarSize::IntegerLiteral(_) => {}
            VarSize::Identifier(id) => id.visit(visitor)
        }
    }
}

impl Visit for AstNode<VarName> {
    fn visit(&self, visitor: &mut Visitor) {
        self.identifier.visit(visitor);
        if let Some(size) = &self.size {
            size.visit(visitor);
        }
    }
}

impl Visit for AstNode<LocalDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if let Type::Pod(pod_type) = &self.type_ {
            if let PodType::Bool = pod_type {
                visit_bool_var_names(&self.names[..], visitor);
            }
        }
    }
}

impl Visit for FunctionBodyStatement {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            FunctionBodyStatement::ConstDeclaration(c) => c.visit(visitor),
            FunctionBodyStatement::LocalDeclaration(l) => l.visit(visitor),
            FunctionBodyStatement::CodeStatement(c) => c.visit(visitor),
        }
    }
}

impl Visit for AstNode<FunctionBody> {
    fn visit(&self, visitor: &mut Visitor) {
        self.statements.iter().for_each(|s| s.visit(visitor))

    }
}

impl Visit for AstNode<FunctionDeclaration> {
    fn visit<'a>(&'a self, visitor: &'a mut Visitor) {
        // ensure that only one function type is declared
        if self.types.len() > 1 {
            visitor.error("multiple function types declared", self.span)
        }

        // ensure that the name starts with a capital letter
        let first_letter_in_name = self.name.string.chars().nth(0).unwrap();
        if !first_letter_in_name.is_alphabetic() || !first_letter_in_name.is_uppercase() {
            visitor.warn("function names should begin with a capital letter", self.name.span)
        }

        // ensure that each modifier occurs only once
        let mut modifier_occurences: HashMap<FunctionModifierType, usize> = HashMap::new();
        self.modifiers
            .iter()
            .map(|modifier| &modifier.type_)
            .for_each(|type_| {
                let count = modifier_occurences.entry(type_.clone()).or_insert(0);
                *count += 1;
            });
        modifier_occurences
            .iter()
            .filter(|(_, occurences)| **occurences > 1)
            .for_each(|(type_, _)| {
                visitor.warn(format!("function has redundant {:?} modifier", type_).as_str(), self.span)
            });

        // ensure modifiers appear in alphabetical order
        let modifier_types: Vec<FunctionModifierType> = self.modifiers
            .iter()
            .map(|f| f.type_.clone())
            .collect();
        let mut sorted_modifier_types = modifier_types.clone();
        sorted_modifier_types.sort();
        let is_sorted = modifier_types
            .iter()
            .zip(sorted_modifier_types.iter())
            .filter(|&(lhs, rhs)| lhs.ne(rhs))
            .count() == 0;
        if !is_sorted {
            let sorted_modifier_type_names: Vec<String> = sorted_modifier_types
                .iter()
                .map(|t| format!("{:?}", t).to_lowercase())
                .collect();
            visitor.warn(
                format!("function modifiers should appear in alphabetical order (e.g., {:?})", sorted_modifier_type_names.join(" ").as_str()).as_str(),
                self.span
            )
        }

        // ensure that functions with exec modifier does not consecutive float arguments
        for modifier in self.modifiers.iter() {
            if modifier.type_ == FunctionModifierType::Exec {
                let mut last_type: Option<&Type> = None;
                for argument in self.arguments.iter() {
                    if let Type::Pod(pod_type) = &argument.type_ {
                        if let PodType::Float = pod_type {
                            if let Some(last_type) = last_type {
                                if let Type::Pod(PodType::Float) = last_type {
                                    visitor.error(
                                        "functions with the exec modifier cannot correctly parse consecutive float arguments when invoked from the console",
                                        argument.span
                                    );
                                    break
                                }
                            }
                        }
                    }
                    last_type = Some(&argument.type_);
                }
            }
        }

        if let Some(body) = &self.body {
            body.visit(visitor)
        }
    }
}

impl Visit for Type {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            Type::Pod(_) => {},
            Type::Array(t) => t.visit(visitor),
            Type::Class(c) => {
                visitor.types.insert(c.data.clone());
            }
            Type::Struct(e) => e.visit(visitor),
            Type::Enum(e) => e.visit(visitor),
            Type::Identifier(id) => {
                visitor.types.insert(id.clone());
            }
        }
    }
}

impl Visit for Rc<AstNode<VarDeclaration>> {
    fn visit(&self, visitor: &mut Visitor) {
        // add all the variables to the visitor context
        let names = self.names.iter().map(|name| &name.identifier);
        for name in names {
            visitor.variables.insert(name.data.clone(), self.clone());
        }
        self.type_.visit(visitor);
        if let Type::Pod(PodType::Bool) = &self.type_ {
            visit_bool_var_names(&self.names, visitor)
        }
        let modifiers: Vec<VarModifier> = self.modifiers.iter().map(|m| m.data).collect();
        let mut sorted_modifiers = modifiers.clone();
        sorted_modifiers.sort();
        let is_sorted = modifiers
            .iter()
            .zip(sorted_modifiers.iter())
            .all(|(lhs, rhs)| *lhs == *rhs);
        if !is_sorted {
            visitor.warn(format!("var modifiers should be in alphabetical order (e.g., {:?})", sorted_modifiers).as_str(), self.span)
        }
    }
}

impl Visit for AstNode<StructVarDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.type_.visit(visitor);
        if let Type::Pod(PodType::Bool) = self.type_ {
            visit_bool_var_names(&self.names, visitor)
        }
        // // TODO: struct vars should not have localized members if its editable?
        // for modifier in self.modifiers {
        //     match modifier {
        //         StructVarModifier::Localized => {
        //
        //         }
        //         _ => {}
        //     }
        // }
    }
}

impl Visit for AstNode<StructDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.members.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ExpressionList> {
    fn visit(&self, visitor: &mut Visitor) {
        let trailing_empty_arguments_count = self.expressions
            .iter()
            .rev()
            .take_while(|argument| argument.is_none())
            .count();
        if trailing_empty_arguments_count > 0 {
            visitor.warn("superfluous trailing empty arguments in function call", self.span);
        }
        for expression in &self.expressions {
            if let Some(expression) = expression {
                expression.visit(visitor)
            }
        }
    }
}

impl Visit for StateStatement {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            StateStatement::ConstDeclaration(c) => c.visit(visitor),
            StateStatement::FunctionDeclaration(f) => f.visit(visitor),
        }
    }
}

impl Visit for StateDeclaration {
    fn visit(&self, visitor: &mut Visitor) {
        for statement in self.statements.iter() {
            statement.visit(visitor)
        }
        for label in self.labels.iter() {
            label.statements.iter().for_each(|statement| statement.visit(visitor))
        }
    }
}

impl Visit for AstNode<EnumDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.name.string.chars().nth(0).unwrap() != 'E' {
            visitor.warn("enumeration types names should be prefixed with the letter \"E\"", self.span)
        }
    }
}

impl Visit for AstNode<ClassDeclaration> {  // TODO: we can get rid of the nested types by implement Visit on generic type AstNode<T: Visit>!
    fn visit(&self, visitor: &mut Visitor) {
        let mut modifier_occurences: HashMap<ClassModifierType, usize> = HashMap::new();
        self.modifiers
            .iter()
            .map(|modifier| &modifier.type_.data)
            .for_each(|type_| {
                let count = modifier_occurences.entry(type_.clone()).or_insert(0);
                *count += 1;
            });
        modifier_occurences
            .iter()
            .filter(|(type_, occurences)| **occurences > 1 && type_.is_unique())
            .for_each(|(type_, _)| {
                visitor.warn(format!("class has redundant {:?} modifier", type_).as_str(), self.span)
            });
    }
}

impl Visit for Program {
    fn visit(&self, visitor: &mut Visitor) {
        for statement in self.statements.iter() {
            match &statement.data {
                ProgramStatement::Empty => { visitor.warn("empty statement", statement.span) }
                ProgramStatement::ClassDeclaration(c) => c.visit(visitor),
                ProgramStatement::CompilerDirective(_) => {}
                ProgramStatement::ConstDeclaration(c) => c.visit(visitor),
                ProgramStatement::VarDeclaration(v) => v.visit(visitor),
                ProgramStatement::EnumDeclaration(e) => e.visit(visitor),
                ProgramStatement::StructDeclaration(s) => s.visit(visitor),
                ProgramStatement::FunctionDeclaration(f) => f.visit(visitor),
                ProgramStatement::ReplicationBlock(r) => r.visit(visitor),
                ProgramStatement::StateDeclaration(s) => s.visit(visitor),
                ProgramStatement::DefaultProperties(d) => d.visit(visitor),
                ProgramStatement::CppText(_) => {}
            }
        }
    }
}

fn visit_bool_var_names(names: &[AstNode<VarName>], visitor: &mut Visitor) {
    for name in names {
        let variable_name = &name.identifier.string;
        if variable_name.chars().nth(0).unwrap() != 'b' {
            visitor.warn(format!("bool variable name {:?} should begin with a \"b\"", variable_name).as_str(), name.span);
        }
    }
}