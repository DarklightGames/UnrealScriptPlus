use crate::ast::*;
use crate::parser::{ProgramError, ProgramErrorSeverity};

use convert_case::{Case, Casing};
use std::collections::HashMap;

pub struct VisitorContext<'a> {
    pub function: Option<&'a AstNode<FunctionDeclaration>>,
}

impl VisitorContext<'_> {
    pub fn new<'a>() -> VisitorContext<'a> {
        VisitorContext {
            function: None
        }
    }
}

pub struct Visitor<'a> {
    pub errors: Vec<ProgramError>,
    pub context: VisitorContext<'a>,
}

impl Visitor<'_> {
    pub fn new<'a>() -> Visitor<'a> {
        Visitor { errors: vec![], context: VisitorContext::new() }
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
        let name = &self.data.name.data.string;
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
        self.data.assignments.iter().for_each(|a| a.visit(visitor))
    }
}

impl Visit for AstNode<DefaultPropertiesArray> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.elements.iter().flatten().for_each(|e| e.visit(visitor))
    }
}

impl Visit for AstNode<DefaultPropertiesValue> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            DefaultPropertiesValue::Literal(l) => {
                l.visit(visitor);
                if let DefaultPropertiesValue::Literal(literal) = &self.data {
                    if let Literal::Name(name) = literal {
                        visitor.error(
                            format!("values in the defaultproperties block cannot be \
                            initialized with name literals; use string literal instead \
                            (e.g., {:?})", name).as_str(),
                            self.span
                        )
                    }
                }
            }
            DefaultPropertiesValue::Identifier(_) => {}
            DefaultPropertiesValue::Struct(s) => s.visit(visitor),
            DefaultPropertiesValue::Array(a) => a.visit(visitor),
        }
    }
}

impl Visit for AstNode<DefaultPropertiesObject> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.statements.iter().for_each(|statement| statement.visit(visitor))
}
}

impl Visit for AstNode<DefaultPropertiesAssignment> {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(v) = &self.data.value {
            v.visit(visitor)
        }
    }
}

impl Visit for Literal {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            Literal::Numeric(_) => {}
            Literal::Boolean(_) => {}
            Literal::String(_) => {}
            Literal::Name(_) => {}
            Literal::Rotator { .. } => {}
            Literal::Vector { .. } => {}
            Literal::Object { .. } => {}
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
        if self.data.statements.is_empty() {
            visitor.warn("defaultproperties block should not be empty", self.span)
        }
        self.data.statements.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ReplicationBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.data.statements.is_empty() {
            visitor.warn("replication block should not be empty", self.span)
        }
    }
}

impl Visit for AstNode<CodeBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.statements.iter().for_each(|s| s.visit(visitor))
    }
}

impl Visit for AstNode<CodeStatementOrBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            CodeStatementOrBlock::CodeBlock(b) => b.visit(visitor),
            CodeStatementOrBlock::CodeStatement(s) => s.visit(visitor)
        }
    }
}

impl Visit for AstNode<Expression> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            Expression::Identifier(_) => {}
            Expression::Literal(l) => l.visit(visitor),
            Expression::New { .. } => {}
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
                arguments.visit(visitor);
            }
            Expression::GlobalCall { name, arguments } => {
                name.visit(visitor);
                arguments.visit(visitor);
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
        self.data.predicate.visit(visitor);
        if let Some(body) = &self.data.body {
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
                if let Expression::DyadicExpression{lhs, verb, rhs} = &expression.data {
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
        self.data.identifier.visit(visitor);
        if let Some(size) = &self.data.size {
            size.visit(visitor);
        }
    }
}

impl Visit for AstNode<LocalDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if let Type::Bool = &self.data.type_ {
            visit_bool_var_names(&self.data.names[..], visitor);
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
        self.data.statements.iter().for_each(|s| s.visit(visitor))

    }
}

impl Visit for AstNode<FunctionDeclaration> {
    fn visit<'a>(&'a self, visitor: &'a mut Visitor) {
        // ensure that only one function type is declared
        if self.data.types.len() > 1 {
            visitor.error("multiple function types declared", self.span)
        }

        // ensure that the name starts with a capital letter
        let first_letter_in_name = self.data.name.data.string.chars().nth(0).unwrap();
        if !first_letter_in_name.is_alphabetic() || !first_letter_in_name.is_uppercase() {
            visitor.warn("function names should begin with a capital letter", self.data.name.span)
        }

        // ensure that each modifier occurs only once
        let mut modifier_occurences: HashMap<FunctionModifierType, usize> = HashMap::new();
        self.data.modifiers
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
        let modifier_types: Vec<FunctionModifierType> = self.data.modifiers
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
        for modifier in self.data.modifiers.iter() {
            if modifier.type_ == FunctionModifierType::Exec {
                let mut last_type: Option<&Type> = None;
                for argument in self.data.arguments.iter() {
                    if let Type::Float = argument.data.type_ {
                        if let Some(last_type) = last_type {
                            if let Type::Float = last_type {
                                visitor.error(
                                    "functions with the exec modifier cannot correctly parse consecutive float arguments when invoked from the console",
                                    argument.span
                                );
                                break
                            }
                        }
                    }
                    last_type = Some(&argument.data.type_);
                }
            }
        }

        if let Some(body) = &self.data.body {
            body.visit(visitor)
        }
    }
}

impl Visit for Type {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            Type::Int => {}
            Type::Float => {}
            Type::String => {}
            Type::Name => {}
            Type::Bool => {}
            Type::Array(t) => t.visit(visitor),
            Type::Class(c) => {}
            Type::Struct(e) => e.visit(visitor),
            Type::Enum(e) => e.visit(visitor),
            Type::Identifier(_) => {}
        }
    }
}

impl Visit for AstNode<VarDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.type_.visit(visitor);
        if let Type::Bool = &self.data.type_ {
            visit_bool_var_names(&self.data.names, visitor)
        }
    }
}

impl Visit for AstNode<StructVarDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.type_.visit(visitor);
        if let Type::Bool = self.data.type_ {
            visit_bool_var_names(&self.data.names, visitor)
        }
    }
}

impl Visit for AstNode<StructDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.members.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ExpressionList> {
    fn visit(&self, visitor: &mut Visitor) {
        let trailing_empty_arguments_count = self.data.expressions
            .iter()
            .rev()
            .take_while(|argument| argument.is_none())
            .count();
        if trailing_empty_arguments_count > 0 {
            visitor.warn("superfluous trailing empty arguments in function call", self.span);
        }
        for expression in &self.data.expressions {
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
        if self.data.name.data.string.chars().nth(0).unwrap() != 'E' {
            visitor.warn("enumeration types names should be prefixed with the letter \"E\"", self.span)
        }
    }
}

impl Visit for AstNode<ClassDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        let mut modifier_occurences: HashMap<ClassModifierType, usize> = HashMap::new();
        self.data.modifiers
            .iter()
            .map(|modifier| &modifier.data.type_.data)
            .for_each(|type_| {
                let count = modifier_occurences.entry(type_.clone()).or_insert(0);
                *count += 1;
            });
        modifier_occurences
            .iter()
            .filter(|(_, occurences)| **occurences > 1)
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
        let variable_name = &name.data.identifier.data.string;
        if variable_name.chars().nth(0).unwrap() != 'b' {
            visitor.warn(format!("bool variable name {:?} should begin with a \"b\"", variable_name).as_str(), name.span);
        }
    }
}