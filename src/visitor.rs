use crate::ast::*;
use crate::parser::{ProgramError, ProgramErrorSeverity};

use convert_case::{Case, Casing};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::rc::Rc;

pub struct Visitor<'a> {
    pub errors: Vec<ProgramError>,
    contents: &'a str,
    variables: HashMap<Identifier, Rc<AstNode<VarDeclaration>>>,
    types: HashSet<Identifier>,
}

impl Visitor<'_> {
    pub fn new(contents: &str) -> Visitor<'_> {
        Visitor {
            contents,
            errors: vec![],
            variables: HashMap::new(),
            types: HashSet::new(),
        }
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
            severity: ProgramErrorSeverity::Error,
        })
    }
}

pub trait Visit {
    fn visit(&self, visitor: &mut Visitor);
}

impl Visit for AstNode<ConstDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        let name = &self.name.string;
        if name.to_case(Case::Constant).ne(name) {
            visitor.warn(
                format!(
                    "Const variable names should be in SCREAMING_SNAKE_CASE (e.g., {:?}).",
                    name.to_case(Case::Constant)
                )
                .as_str(),
                self.span,
            )
        }
    }
}

impl Visit for AstNode<Literal> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            Literal::None => {}
            Literal::Numeric(_) => {}
            Literal::Boolean(_) => {
                let contents = &visitor.contents[self.span.start..self.span.end];
                if contents != contents.to_lowercase().as_str() {
                    visitor.warn(
                        format!(
                            "Boolean literals should be lowercase (use {:?} instead of {:?})",
                            contents.to_lowercase().as_str(),
                            contents
                        )
                        .as_str(),
                        self.span,
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
                                    format!("Rotator literal component {} ({}) should be within the range [{}..{}]", index, n, i16::MIN, i16::MAX).as_str(),
                                    value.span
                                )
                            }
                        }
                        NumericLiteral::Float(f) => {
                            visitor.warn(
                                format!("Rotator literal component {} ({}) should be an integer literal (e.g., \"{}\")",
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
                                format!("Type specifier for class and enum object literals should be lowercase (e.g., {} instead of {})",
                                    type_.to_lowercase(),
                                    type_.as_str()
                                ).as_str(),
                                type_.span
                            )
                        }
                    }
                    _ => {
                        // TODO: the correct thing to do here is check that the capitalization matches the capitalization the type was defined with
                        let first_letter_in_type = type_.chars().nth(0).unwrap();
                        if !first_letter_in_type.is_uppercase() {
                            visitor.warn(
                                format!("Non-POD object literal types should begin with a capital letter (e.g., {})",
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

impl Visit for AstNode<DefaultProperties> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.statements.is_empty() {
            visitor.warn("DefaultProperties block should not be empty.", self.span)
        }
        for statement in &self.statements {
            // Only top-level assignments set the class's own default values;
            // assignments inside `Begin Object` blocks belong to subobjects.
            if let t3d::syntax::StatementKind::Assignment(assignment) = &statement.kind {
                visit_default_assignment(assignment, visitor);
            }
            visit_default_values(statement, visitor);
        }
    }
}

/// Lints every value in a `defaultproperties` statement, including values in
/// structs, arrays and `Begin Object` blocks.
fn visit_default_values(statement: &t3d::syntax::Statement, visitor: &mut Visitor) {
    use t3d::syntax::{Assignment, StatementKind, Value, ValueKind};

    fn assignment(a: &Assignment, visitor: &mut Visitor) {
        if let Some(v) = &a.value {
            value(v, visitor);
        }
    }
    fn value(v: &Value, visitor: &mut Visitor) {
        match &v.kind {
            ValueKind::Name(name) => visitor.error(
                format!(
                    "Values in the defaultproperties block cannot be initialized with name \
                     literals; use string literal instead (e.g., {:?})",
                    name
                )
                .as_str(),
                ast_span(v.span),
            ),
            ValueKind::Struct(members) => members.iter().for_each(|m| assignment(m, visitor)),
            ValueKind::Array(elements) => elements.iter().flatten().for_each(|e| value(e, visitor)),
            _ => {}
        }
    }

    match &statement.kind {
        StatementKind::Assignment(a) => assignment(a, visitor),
        StatementKind::InlineStruct(s) => s.assignments.iter().for_each(|a| assignment(a, visitor)),
        StatementKind::Object(o) => {
            o.attributes.iter().for_each(|a| assignment(a, visitor));
            o.statements
                .iter()
                .for_each(|s| visit_default_values(s, visitor));
        }
        StatementKind::Vector(_) => {}
    }
}

fn ast_span(span: t3d::Span) -> AstSpan {
    AstSpan {
        start: span.start,
        end: span.end,
        line: None,
    }
}

/// Lints a top-level `defaultproperties` assignment against the type of the
/// variable it assigns, when that variable is declared in this class.
fn visit_default_assignment(assignment: &t3d::syntax::Assignment, visitor: &mut Visitor) {
    use t3d::syntax::ValueKind;

    let Some(value) = &assignment.value else {
        return;
    };
    let name = Identifier {
        string: assignment.name.clone(),
    };
    let Some(variable) = visitor.variables.get(&name) else {
        return;
    };
    let Type::Pod(pod_type) = &variable.type_ else {
        return;
    };
    let is_array_assignment = assignment.index.is_some();
    let span = ast_span(assignment.span);
    let redundant = |visitor: &mut Visitor, default: &dyn std::fmt::Display| {
        visitor.warn(
            format!(
                "Redundant specification of default value ({}) of variable ({}) declared in this class",
                default, assignment.name
            )
            .as_str(),
            span,
        )
    };

    match (pod_type, &value.kind) {
        (PodType::Byte, ValueKind::Int(i)) => {
            if !is_array_assignment && *i == 0 {
                redundant(visitor, &0)
            }
            if *i < u8::MIN as i64 || *i > u8::MAX as i64 {
                let effective_value = (*i & 0xFF) as u8;
                visitor.warn(
                    format!(
                        "Value {} is outside the range of [{}..{}]; effective value: {}",
                        i,
                        u8::MIN,
                        u8::MAX,
                        effective_value
                    )
                    .as_str(),
                    span,
                )
            }
        }
        (PodType::Byte, ValueKind::Float(_)) => visitor.warn("Incorrect literal type", span),
        (PodType::Int | PodType::Float, ValueKind::Int(0)) if !is_array_assignment => {
            redundant(visitor, &0)
        }
        (PodType::Int | PodType::Float, ValueKind::Float(f))
            if !is_array_assignment && *f == 0.0 =>
        {
            redundant(visitor, &0.0)
        }
        (PodType::String | PodType::Name, ValueKind::String(s))
            if !is_array_assignment && s.is_empty() =>
        {
            redundant(visitor, &"")
        }
        (PodType::Bool, ValueKind::Bool(false)) if !is_array_assignment => {
            redundant(visitor, &false)
        }
        _ => {}
    }
}

impl Visit for AstNode<ReplicationBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.statements.is_empty() {
            visitor.warn("Replication block should not be empty", self.span)
        }
        // TODO: check to see that reliability & expression are unique for each statement
        for (i, statement) in self.statements.iter().enumerate() {
            for next_statement in &self.statements[i + 1..] {
                if statement.condition == next_statement.condition
                    && statement.reliability == next_statement.reliability
                {
                    visitor.warn("Duplicate replication reliability and condition", self.span)
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
            CodeStatementOrBlock::CodeStatement(s) => s.visit(visitor),
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
            Expression::MonadicPreExpression { operand, verb: _ } => operand.visit(visitor),
            Expression::MonadicPostExpression { operand, verb: _ } => operand.visit(visitor),
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
                    _ => visitor.warn("Redundant parenthetical expression", expression.span),
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
        self.statements
            .iter()
            .for_each(|statement| statement.visit(visitor))
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
            CodeStatement::Empty => visitor.warn("Empty statement", self.span),
            CodeStatement::Expression(expression) => {
                if let Expression::DyadicExpression { lhs, verb: _, rhs } = &expression.data {
                    if let Expression::ParentheticalExpression(lhs) = &lhs.data {
                        visitor.warn("Redundant parenthetical expression", lhs.span)
                    }
                    if let Expression::ParentheticalExpression(rhs) = &rhs.data {
                        visitor.warn("Redundant parenthetical expression", rhs.span)
                    }
                }
                expression.visit(visitor)
            }
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
            CodeStatement::ConstDeclaration(c) => c.visit(visitor),
        }
    }
}

impl Visit for AstNode<Identifier> {
    fn visit(&self, _visitor: &mut Visitor) {}
}

impl Visit for AstNode<VarSize> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            VarSize::IntegerLiteral(_) => {}
            VarSize::Identifier(id) => id.visit(visitor),
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
            visitor.error("Multiple function types declared", self.span)
        }

        // ensure that the name starts with a capital letter
        let first_letter_in_name = self.name.string.chars().nth(0).unwrap();
        if !first_letter_in_name.is_alphabetic() || !first_letter_in_name.is_uppercase() {
            visitor.warn(
                "Function names should begin with a capital letter",
                self.name.span,
            )
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
                visitor.warn(
                    format!("Function has redundant {:?} modifier", type_).as_str(),
                    self.span,
                )
            });

        // ensure modifiers appear in alphabetical order
        let modifier_types: Vec<FunctionModifierType> =
            self.modifiers.iter().map(|f| f.type_.clone()).collect();
        let mut sorted_modifier_types = modifier_types.clone();
        sorted_modifier_types.sort();
        let is_sorted = modifier_types
            .iter()
            .zip(sorted_modifier_types.iter())
            .filter(|&(lhs, rhs)| lhs.ne(rhs))
            .count()
            == 0;
        if !is_sorted {
            let sorted_modifier_type_names: Vec<String> = sorted_modifier_types
                .iter()
                .map(|t| format!("{:?}", t).to_lowercase())
                .collect();
            visitor.warn(
                format!(
                    "Function modifiers should appear in alphabetical order (e.g., {:?})",
                    sorted_modifier_type_names.join(" ").as_str()
                )
                .as_str(),
                self.span,
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
                                    break;
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
            Type::Pod(_) => {}
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
            visitor.warn(
                format!(
                    "Var modifiers should be in alphabetical order (e.g., {:?})",
                    sorted_modifiers
                )
                .as_str(),
                self.span,
            )
        }
    }
}

impl Visit for AstNode<StructVarDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.type_.visit(visitor);
        if let Type::Pod(PodType::Bool) = self.type_ {
            visit_bool_var_names(&self.names, visitor)
        }
    }
}

impl Visit for AstNode<StructDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        self.members.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ExpressionList> {
    fn visit(&self, visitor: &mut Visitor) {
        let trailing_empty_arguments_count = self
            .expressions
            .iter()
            .rev()
            .take_while(|argument| argument.is_none())
            .count();
        if trailing_empty_arguments_count > 0 {
            visitor.warn(
                "Superfluous trailing empty arguments in function call",
                self.span,
            );
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
            label
                .statements
                .iter()
                .for_each(|statement| statement.visit(visitor))
        }
    }
}

impl Visit for AstNode<EnumDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.name.string.chars().nth(0).unwrap() != 'E' {
            visitor.warn(
                "Enumeration types names should be prefixed with the letter \"E\"",
                self.span,
            )
        }
    }
}

impl Visit for AstNode<ClassDeclaration> {
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
                visitor.warn(
                    format!("Class has redundant {:?} modifier", type_).as_str(),
                    self.span,
                )
            });
    }
}

impl Visit for Program {
    fn visit(&self, visitor: &mut Visitor) {
        for statement in self.statements.iter() {
            match &statement.data {
                ProgramStatement::Empty => visitor.warn("Empty statement", statement.span),
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
            visitor.warn(
                format!(
                    "Boolean variable name {:?} should begin with a \"b\"",
                    variable_name
                )
                .as_str(),
                name.span,
            );
        }
    }
}
