use crate::ast::*;
use crate::lint::Lint;
use crate::parser::ProgramError;

use crate::symbols::{
    Access, Namespace, ReferenceKind, ScopeId, ScopeKind, SymbolId, SymbolKind, SymbolTable,
    TypeRef,
};

use convert_case::{Case, Casing};
use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

/// Latent functions of the engine, which can only be called from state code.
const LATENT_FUNCTIONS: &[&str] = &[
    "Sleep",
    "FinishAnim",
    "FinishInterpolation",
    "MoveTo",
    "MoveToward",
    "StrafeTo",
    "StrafeFacing",
    "TurnTo",
    "TurnToward",
    "FinishRotation",
    "WaitForLanding",
    "WaitToSeeEnemy",
];

/// Functions the engine calls every frame.
const PER_FRAME_FUNCTIONS: &[&str] = &[
    "Tick",
    "PlayerTick",
    "PostRender",
    "DrawHUD",
    "RenderOverlays",
];

/// Iterators that visit every actor (or object) in the level.
const EXPENSIVE_ITERATORS: &[&str] = &["AllActors", "DynamicActors", "AllObjects"];

/// What the visitor knows about a variable, argument or local.
#[derive(Debug, Clone, Copy)]
struct VariableInfo {
    is_byte: bool,
    size: Option<i64>,
}

/// Where the code being visited is.
#[derive(Default)]
struct CodeContext {
    /// The scope names are looked up in.
    scope: ScopeId,
    /// The function being visited, if any.
    function_name: Option<String>,
    /// Whether the code is state code (under a state label).
    in_state_code: bool,
    loop_depth: usize,
    switch_depth: usize,
    /// Whether `X = X` is allowed, as in `for (i = i; ...)`, which continues
    /// a loop from the current value of `i`.
    allow_self_assignment: bool,
}

pub struct Visitor<'a> {
    pub errors: Vec<ProgramError>,
    contents: &'a str,
    symbols: &'a SymbolTable,
    code: CodeContext,
}

impl<'a> Visitor<'a> {
    pub fn new(contents: &'a str, symbols: &'a SymbolTable) -> Visitor<'a> {
        Visitor {
            contents,
            errors: vec![],
            symbols,
            code: CodeContext::default(),
        }
    }

    /// Reports a problem found by `lint`.
    pub fn report(&mut self, lint: Lint, message: impl Into<String>, span: AstSpan) {
        self.errors.push(ProgramError {
            message: message.into(),
            span,
            severity: lint.severity(),
            lint: Some(lint),
        })
    }

    /// Looks up the variable, argument or local `name` declared in this file
    /// and visible from the current scope.
    fn lookup(&self, name: &Identifier) -> Option<VariableInfo> {
        self.lookup_in(self.code.scope, name)
    }

    fn lookup_in(&self, scope: ScopeId, name: &Identifier) -> Option<VariableInfo> {
        let id = self.symbols.lookup(scope, Namespace::Value, name)?;
        let kind = &self.symbols.symbol(id).kind;
        Some(VariableInfo {
            is_byte: kind.type_()? == &TypeRef::Pod(PodType::Byte),
            size: kind.array_size(),
        })
    }

    /// Reports the locals of the function `scope` that are never used, or
    /// only ever written to.
    fn report_unused_locals(&mut self, scope: ScopeId) {
        let symbols = self.symbols;
        for &id in symbols.scope(scope).symbols() {
            let symbol = symbols.symbol(id);
            if !matches!(symbol.kind, SymbolKind::Local { .. }) {
                continue;
            }
            let mut references = symbols.references_to(id).peekable();
            if references.peek().is_none() {
                self.report(
                    Lint::UnusedLocal,
                    format!("Local variable {:?} is never used", symbol.name.string),
                    symbol.span,
                )
            } else if references.all(|r| r.kind == ReferenceKind::Write) {
                self.report(
                    Lint::LocalSetButNeverUsed,
                    format!(
                        "Local variable {:?} is assigned to, but its value is never used",
                        symbol.name.string
                    ),
                    symbol.span,
                )
            }
        }
    }

    /// Visits code in a fresh [`CodeContext`].
    fn with_code_context(&mut self, context: CodeContext, f: impl FnOnce(&mut Self)) {
        let outer = std::mem::replace(&mut self.code, context);
        f(self);
        self.code = outer;
    }
}

pub trait Visit {
    fn visit(&self, visitor: &mut Visitor);
}

// Expression helpers.

/// Strips any parentheses around `expression`.
fn unparenthesize(mut expression: &AstNode<Expression>) -> &AstNode<Expression> {
    while let Expression::ParentheticalExpression(inner) = &expression.data {
        expression = inner;
    }
    expression
}

/// Whether evaluating `expression` can do more than produce a value: calls
/// (which may have side effects, or return different values), `new`,
/// assignments and increments.
fn has_side_effects(expression: &Expression) -> bool {
    match expression {
        Expression::Identifier(_) | Expression::Literal(_) => false,
        Expression::New { .. } | Expression::Call { .. } | Expression::GlobalCall { .. } => true,
        Expression::MonadicPreExpression { operand, verb }
        | Expression::MonadicPostExpression { operand, verb } => {
            matches!(verb, MonadicVerb::Increment | MonadicVerb::Decrement)
                || has_side_effects(operand)
        }
        Expression::DyadicExpression { lhs, verb, rhs } => {
            verb.is_assignment() || has_side_effects(lhs) || has_side_effects(rhs)
        }
        Expression::ArrayAccess { operand, argument } => {
            has_side_effects(operand) || has_side_effects(argument)
        }
        Expression::DefaultAccess { operand, .. } | Expression::StaticAccess { operand, .. } => {
            operand.as_ref().is_some_and(|o| has_side_effects(o))
        }
        Expression::MemberAccess { operand, .. } => has_side_effects(operand),
        Expression::Cast { operand, .. } => has_side_effects(operand),
        Expression::ParentheticalExpression(inner) => has_side_effects(inner),
    }
}

/// The value of an integer literal, including a negated one.
fn integer_literal(expression: &AstNode<Expression>) -> Option<i64> {
    match &unparenthesize(expression).data {
        Expression::Literal(literal) => match &literal.data {
            Literal::Numeric(n) => match n.data {
                NumericLiteral::Integer(i) => Some(i as i64),
                NumericLiteral::Float(_) => None,
            },
            _ => None,
        },
        Expression::MonadicPreExpression {
            operand,
            verb: MonadicVerb::Negate,
        } => integer_literal(operand).map(|i| -i),
        _ => None,
    }
}

fn is_zero_literal(expression: &AstNode<Expression>) -> bool {
    match &unparenthesize(expression).data {
        Expression::Literal(literal) => match &literal.data {
            Literal::Numeric(n) => match n.data {
                NumericLiteral::Integer(i) => i == 0,
                NumericLiteral::Float(f) => f == 0.0,
            },
            _ => false,
        },
        _ => false,
    }
}

/// If `expression` is an unqualified call such as `Foo(...)`, its name.
fn called_name(expression: &AstNode<Expression>) -> Option<&Identifier> {
    match &unparenthesize(expression).data {
        Expression::Call { operand, .. } => match &operand.data {
            Expression::Identifier(name) => Some(&name.data),
            _ => None,
        },
        _ => None,
    }
}

fn is_comparison(verb: &DyadicVerb) -> bool {
    matches!(
        verb,
        DyadicVerb::CompareEqual
            | DyadicVerb::CompareNotEqual
            | DyadicVerb::CompareApproximatelyEqual
            | DyadicVerb::CompareLessThan
            | DyadicVerb::CompareGreaterThan
            | DyadicVerb::CompareLessThanOrEqual
            | DyadicVerb::CompareGreaterThanOrEqual
    )
}

/// Operators for which identical operands are almost certainly a mistake.
/// `X != X` is left out: it is the idiom for checking whether `X` is NaN.
fn is_identical_operand_verb(verb: &DyadicVerb) -> bool {
    (is_comparison(verb) && *verb != DyadicVerb::CompareNotEqual)
        || matches!(
            verb,
            DyadicVerb::LogicalAnd
                | DyadicVerb::LogicalOr
                | DyadicVerb::LogicalExclusiveOr
                | DyadicVerb::Subtract
                | DyadicVerb::Divide
                | DyadicVerb::Modulo
                | DyadicVerb::BitwiseAnd
                | DyadicVerb::BitwiseOr
                | DyadicVerb::BitwiseExclusiveOr
        )
}

fn is_name(identifier: &Identifier, names: &[&str]) -> bool {
    names.iter().any(|n| identifier.eq_ignore_ascii_case(n))
}

// Statement helpers.

/// Whether `statement` is state code's `Stop;`.
fn is_stop(statement: &CodeStatement) -> bool {
    match statement {
        CodeStatement::Expression(e) => match &unparenthesize(e).data {
            Expression::Identifier(id) => id.eq_ignore_ascii_case("stop"),
            _ => false,
        },
        _ => false,
    }
}

/// Whether control never continues past `statement` to the next one.
fn is_jump(statement: &CodeStatement) -> bool {
    matches!(
        statement,
        CodeStatement::Return(_)
            | CodeStatement::Break
            | CodeStatement::Continue
            | CodeStatement::Goto(_)
    ) || is_stop(statement)
}

/// Statements that do nothing at runtime, so don't count as code.
fn is_inert(statement: &CodeStatement) -> bool {
    matches!(
        statement,
        CodeStatement::Empty
            | CodeStatement::CompilerDirective(_)
            | CodeStatement::ConstDeclaration(_)
    )
}

/// Whether every path through `statement` ends in a jump.
fn always_jumps(statement: &CodeStatement) -> bool {
    fn body_always_jumps(body: &CodeStatementOrBlock) -> bool {
        match body {
            CodeStatementOrBlock::CodeStatement(s) => always_jumps(s),
            CodeStatementOrBlock::CodeBlock(b) => b
                .statements
                .iter()
                .rev()
                .find(|s| !is_inert(s))
                .is_some_and(|s| always_jumps(s)),
        }
    }
    match statement {
        CodeStatement::Conditional(c) => {
            c.else_statement
                .as_ref()
                .is_some_and(|e| body_always_jumps(&e.body))
                && c.if_statement
                    .body
                    .as_ref()
                    .is_some_and(|b| body_always_jumps(b))
                && c.elif_statements.iter().all(|e| body_always_jumps(&e.body))
        }
        s => is_jump(s),
    }
}

/// Visits a sequence of statements, reporting code that follows a jump.
fn visit_statements(statements: &[&AstNode<CodeStatement>], visitor: &mut Visitor) {
    let mut unreachable = false;
    for statement in statements {
        if let CodeStatement::JumpLabel(_) = statement.data {
            unreachable = false;
        } else if unreachable && matches!(statement.data, CodeStatement::Break) {
            // `return X; break;` is a common, harmless habit in `switch`es.
        } else if unreachable && !is_inert(statement) {
            visitor.report(Lint::UnreachableCode, "Unreachable code", statement.span);
            // Report only the first unreachable statement.
            unreachable = false;
        }
        statement.visit(visitor);
        if is_jump(statement) {
            unreachable = true;
        }
    }
}

/// Visits the body of an `if`, `else` or loop.
///
/// `may_be_empty` is for loops whose header does all the work, such as
/// `for (S = ""; Len(S) < N; S $= " ");`.
fn visit_body(
    body: &AstNode<CodeStatementOrBlock>,
    keyword: &str,
    may_be_empty: bool,
    visitor: &mut Visitor,
) {
    if let CodeStatementOrBlock::CodeStatement(s) = &body.data {
        if let CodeStatement::Empty = s.data {
            if may_be_empty {
                return;
            }
            visitor.report(
                Lint::EmptyBody,
                format!("Empty `{}` body; remove the `;` or add a block", keyword),
                body.span,
            );
            return;
        }
    }
    body.visit(visitor)
}

/// Visits a loop body, which may `break` and `continue`.
fn visit_loop_body(
    body: &AstNode<CodeStatementOrBlock>,
    keyword: &str,
    may_be_empty: bool,
    visitor: &mut Visitor,
) {
    visitor.code.loop_depth += 1;
    visit_body(body, keyword, may_be_empty, visitor);
    visitor.code.loop_depth -= 1;
}

fn check_condition(predicate: &AstNode<Expression>, keyword: &str, visitor: &mut Visitor) {
    let Expression::Literal(literal) = &unparenthesize(predicate).data else {
        return;
    };
    // `while (true)` is the idiom for a loop that ends with `break`.
    if keyword == "while" && literal.data == Literal::Boolean(true) {
        return;
    }
    visitor.report(
        Lint::ConstantCondition,
        format!("`{}` condition is a constant", keyword),
        predicate.span,
    )
}

impl Visit for AstNode<ConstDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        let name = &self.name.string;
        if name.to_case(Case::Constant).ne(name) {
            visitor.report(
                Lint::ConstNameCase,
                format!(
                    "Const variable names should be in SCREAMING_SNAKE_CASE (e.g., {:?}).",
                    name.to_case(Case::Constant)
                ),
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
                    visitor.report(
                        Lint::BooleanLiteralCase,
                        format!(
                            "Boolean literals should be lowercase (use {:?} instead of {:?})",
                            contents.to_lowercase().as_str(),
                            contents
                        ),
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
                                visitor.report(
                                    Lint::RotatorComponentOutOfRange,
                                    format!(
                                        "Rotator literal component {} ({}) should be within the range [{}..{}]",
                                        index,
                                        n,
                                        i16::MIN,
                                        i16::MAX
                                    ),
                                    value.span,
                                )
                            }
                        }
                        NumericLiteral::Float(f) => visitor.report(
                            Lint::RotatorComponentNotInteger,
                            format!(
                                "Rotator literal component {} ({}) should be an integer literal (e.g., \"{}\")",
                                index,
                                &visitor.contents[value.span.start..value.span.end],
                                *f as i32
                            ),
                            value.span,
                        ),
                    }
                }
            }
            Literal::Vector(_) => {}
            Literal::Object { type_, .. } => match type_.to_lowercase().as_str() {
                "class" | "enum" => {
                    if type_.to_lowercase().as_str() != type_.as_str() {
                        visitor.report(
                            Lint::ObjectLiteralKeywordCase,
                            format!(
                                "Type specifier for class and enum object literals should be lowercase (e.g., {} instead of {})",
                                type_.to_lowercase(),
                                type_.as_str()
                            ),
                            type_.span,
                        )
                    }
                }
                _ => {
                    // TODO: the correct thing to do here is check that the capitalization matches the capitalization the type was defined with
                    if type_.chars().next().is_some_and(|c| !c.is_uppercase()) {
                        visitor.report(
                            Lint::ObjectLiteralTypeCase,
                            format!(
                                "Non-POD object literal types should begin with a capital letter (e.g., {})",
                                type_.to_case(Case::Pascal)
                            ),
                            type_.span,
                        )
                    }
                }
            },
        }
    }
}

impl Visit for AstNode<DefaultProperties> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.statements.is_empty() {
            visitor.report(
                Lint::EmptyDefaultProperties,
                "DefaultProperties block should not be empty.",
                self.span,
            )
        }
        for statement in &self.statements {
            // Only top-level assignments set the class's own default values;
            // assignments inside `Begin Object` blocks belong to subobjects.
            if let t3d::syntax::StatementKind::Assignment(assignment) = &statement.kind {
                visit_default_assignment(assignment, visitor);
            }
            visit_default_values(statement, visitor);
        }
        check_duplicate_default_properties(&self.statements, visitor);
    }
}

/// Reports properties assigned more than once in the same object (the class
/// itself, or one `Begin Object` block).
fn check_duplicate_default_properties(
    statements: &[t3d::syntax::Statement],
    visitor: &mut Visitor,
) {
    use t3d::syntax::StatementKind;

    let mut seen: HashSet<(String, Option<String>)> = HashSet::new();
    for statement in statements {
        match &statement.kind {
            StatementKind::Assignment(a) => {
                let key = (
                    a.name.to_lowercase(),
                    a.index.as_ref().map(|i| i.to_string().to_lowercase()),
                );
                if !seen.insert(key) {
                    let target = match &a.index {
                        Some(index) => format!("{}({})", a.name, index),
                        None => a.name.clone(),
                    };
                    visitor.report(
                        Lint::DuplicateDefaultProperty,
                        format!(
                            "{} is assigned more than once; only the last value is used",
                            target
                        ),
                        ast_span(a.span),
                    )
                }
            }
            StatementKind::Object(o) => check_duplicate_default_properties(&o.statements, visitor),
            StatementKind::InlineStruct(_) | StatementKind::Vector(_) => {}
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
            ValueKind::Name(name) => visitor.report(
                Lint::DefaultPropertiesNameLiteral,
                format!(
                    "Values in the defaultproperties block cannot be initialized with name \
                     literals; use string literal instead (e.g., {:?})",
                    name
                ),
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
    use t3d::syntax::{Index, ValueKind};

    let name = Identifier {
        string: assignment.name.clone(),
    };
    let symbols = visitor.symbols;
    let Some(variable) = symbols
        .lookup(ScopeId::CLASS, Namespace::Value, &name)
        .map(|id| &symbols.symbol(id).kind)
        .filter(|kind| matches!(kind, SymbolKind::Variable { .. }))
    else {
        return;
    };
    let span = ast_span(assignment.span);

    if let (Some(Index::Int(index)), Some(size)) = (&assignment.index, variable.array_size()) {
        if *index < 0 || *index >= size {
            visitor.report(
                Lint::ArrayIndexOutOfBounds,
                format!(
                    "Index {} is out of bounds for {}, which has {} elements",
                    index, assignment.name, size
                ),
                span,
            )
        }
    }

    let Some(value) = &assignment.value else {
        return;
    };
    let Some(TypeRef::Pod(pod_type)) = variable.type_() else {
        return;
    };
    let is_array_assignment = assignment.index.is_some();
    let redundant = |visitor: &mut Visitor, default: &dyn std::fmt::Display| {
        visitor.report(
            Lint::RedundantDefaultValue,
            format!(
                "Redundant specification of default value ({}) of variable ({}) declared in this class",
                default, assignment.name
            ),
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
                visitor.report(
                    Lint::ByteValueOutOfRange,
                    format!(
                        "Value {} is outside the range of [{}..{}]; effective value: {}",
                        i,
                        u8::MIN,
                        u8::MAX,
                        effective_value
                    ),
                    span,
                )
            }
        }
        (PodType::Byte, ValueKind::Float(_)) => visitor.report(
            Lint::DefaultPropertyTypeMismatch,
            "Incorrect literal type",
            span,
        ),
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
            visitor.report(
                Lint::EmptyReplicationBlock,
                "Replication block should not be empty",
                self.span,
            )
        }
        for (i, statement) in self.statements.iter().enumerate() {
            for next_statement in &self.statements[i + 1..] {
                if statement.condition == next_statement.condition
                    && statement.reliability == next_statement.reliability
                {
                    visitor.report(
                        Lint::DuplicateReplicationCondition,
                        "Duplicate replication reliability and condition",
                        self.span,
                    )
                }
            }
        }
        let mut seen = HashSet::new();
        for statement in &self.statements {
            statement.condition.visit(visitor);
            for member in &statement.variables {
                if !seen.insert(member.data.clone()) {
                    visitor.report(
                        Lint::DuplicateReplicatedMember,
                        format!("{} is replicated more than once", member.string),
                        member.span,
                    )
                }
            }
        }
    }
}

impl Visit for AstNode<CodeBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        let statements: Vec<_> = self.statements.iter().collect();
        visit_statements(&statements, visitor)
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
            Expression::DyadicExpression { lhs, verb, rhs } => {
                check_dyadic_expression(self, lhs, verb, rhs, visitor);
                lhs.visit(visitor);
                rhs.visit(visitor);
            }
            Expression::Call { operand, arguments } => {
                if let Some(name) = called_name(self) {
                    if !visitor.code.in_state_code && is_name(name, LATENT_FUNCTIONS) {
                        visitor.report(
                            Lint::LatentCallOutsideStateCode,
                            format!(
                                "{} is a latent function and can only be called from state code",
                                name.string
                            ),
                            self.span,
                        )
                    }
                }
                operand.visit(visitor);
                arguments.visit(visitor);
            }
            Expression::GlobalCall { name, arguments } => {
                name.visit(visitor);
                arguments.visit(visitor);
            }
            Expression::ArrayAccess { operand, argument } => {
                if let (Expression::Identifier(name), Some(index)) =
                    (&operand.data, integer_literal(argument))
                {
                    if let Some(size) = visitor.lookup(name).and_then(|v| v.size) {
                        if index < 0 || index >= size {
                            visitor.report(
                                Lint::ArrayIndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds for {}, which has {} elements",
                                    index, name.string, size
                                ),
                                argument.span,
                            )
                        }
                    }
                }
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
                match &expression.data {
                    Expression::New { .. } => {}
                    Expression::DyadicExpression { .. } => {}
                    _ => visitor.report(
                        Lint::RedundantParentheses,
                        "Redundant parenthetical expression",
                        expression.span,
                    ),
                }
                expression.visit(visitor)
            }
        }
    }
}

fn check_dyadic_expression(
    expression: &AstNode<Expression>,
    lhs: &AstNode<Expression>,
    verb: &DyadicVerb,
    rhs: &AstNode<Expression>,
    visitor: &mut Visitor,
) {
    if matches!(
        verb,
        DyadicVerb::Divide | DyadicVerb::Modulo | DyadicVerb::DivideAssign
    ) && is_zero_literal(rhs)
    {
        visitor.report(
            Lint::DivisionByZero,
            format!("`{}` by zero", verb),
            expression.span,
        )
    }

    if lhs == rhs && !has_side_effects(lhs) {
        if *verb == DyadicVerb::Assign {
            if visitor.code.allow_self_assignment {
                return;
            }
            visitor.report(
                Lint::SelfAssignment,
                "Variable is assigned to itself",
                expression.span,
            )
        } else if is_identical_operand_verb(verb) {
            visitor.report(
                Lint::IdenticalOperands,
                format!("Both operands of `{}` are the same", verb),
                expression.span,
            )
        }
    }

    if is_comparison(verb) {
        if let Expression::MonadicPreExpression {
            verb: MonadicVerb::Not,
            ..
        } = &lhs.data
        {
            visitor.report(
                Lint::NegatedComparisonOperand,
                format!(
                    "`!` applies only to the left operand of `{}`; add parentheses to make the intent clear",
                    verb
                ),
                expression.span,
            )
        }
    }

    if *verb == DyadicVerb::Assign {
        if let (Expression::Identifier(name), Some(value)) = (&lhs.data, integer_literal(rhs)) {
            if visitor.lookup(name).is_some_and(|v| v.is_byte) && !(0..=255).contains(&value) {
                visitor.report(
                    Lint::ByteValueOutOfRange,
                    format!(
                        "Value {} is outside the range of [0..255]; effective value: {}",
                        value,
                        value & 0xFF
                    ),
                    expression.span,
                )
            }
        }
    }
}

impl Visit for ForEach {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(iterator) = called_name(&self.predicate) {
            if is_name(iterator, EXPENSIVE_ITERATORS) {
                if let Some(function) = visitor
                    .code
                    .function_name
                    .clone()
                    .filter(|f| is_name(&Identifier::new(f), PER_FRAME_FUNCTIONS))
                {
                    visitor.report(
                        Lint::ActorIteratorInPerFrameFunction,
                        format!(
                            "`foreach {}` visits every actor, and {} runs every frame",
                            iterator.string, function
                        ),
                        self.predicate.span,
                    )
                }
            }
        }
        self.predicate.visit(visitor);
        visit_loop_body(&self.body, "foreach", false, visitor);
    }
}

impl Visit for ConditionalStatement {
    fn visit(&self, visitor: &mut Visitor) {
        let if_statement = &self.if_statement;
        check_condition(&if_statement.predicate, "if", visitor);
        if_statement.predicate.visit(visitor);
        match &if_statement.body {
            Some(body) => visit_body(body, "if", false, visitor),
            None => visitor.report(
                Lint::EmptyBody,
                "`if` has no body",
                if_statement.predicate.span,
            ),
        }
        for elif in &self.elif_statements {
            check_condition(&elif.predicate, "else if", visitor);
            elif.predicate.visit(visitor);
            visit_body(&elif.body, "else if", false, visitor);
        }
        if let Some(else_statement) = &self.else_statement {
            visit_body(&else_statement.body, "else", false, visitor)
        }
    }
}

impl Visit for ForStatement {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(init) = &self.init {
            visitor.code.allow_self_assignment = true;
            init.visit(visitor);
            visitor.code.allow_self_assignment = false;
        }
        if let Some(predicate) = &self.predicate {
            predicate.visit(visitor)
        }
        if let Some(post) = &self.post {
            post.visit(visitor)
        }
        let may_be_empty = self.post.is_some();
        visit_loop_body(&self.body, "for", may_be_empty, visitor)
    }
}

impl Visit for SwitchStatement {
    fn visit(&self, visitor: &mut Visitor) {
        self.predicate.visit(visitor);

        let labels: Vec<&AstNode<Expression>> = self
            .cases
            .iter()
            .filter_map(|c| match &c.type_ {
                SwitchCaseType::Expression(e) => Some(e.as_ref()),
                SwitchCaseType::Default => None,
            })
            .collect();
        for label in &labels {
            label.visit(visitor);
        }
        for (i, label) in labels.iter().enumerate() {
            if labels[..i].contains(label) {
                visitor.report(
                    Lint::DuplicateSwitchCase,
                    "Duplicate `case` value",
                    label.span,
                )
            }
        }

        visitor.code.switch_depth += 1;
        for case in &self.cases {
            let statements: Vec<_> = case.statements.iter().collect();
            visit_statements(&statements, visitor);
        }
        visitor.code.switch_depth -= 1;

        let Some((_, preceding)) = self.cases.split_last() else {
            return;
        };
        for case in preceding {
            let Some(last) = case.statements.iter().rev().find(|s| !is_inert(s)) else {
                // An empty case shares the next case's code.
                continue;
            };
            if !always_jumps(last) {
                let span = match &case.type_ {
                    SwitchCaseType::Expression(e) => e.span,
                    SwitchCaseType::Default => last.span,
                };
                visitor.report(
                    Lint::SwitchCaseFallthrough,
                    "`case` falls through to the next case; end it with `break`",
                    span,
                )
            }
        }
    }
}

impl Visit for WhileStatement {
    fn visit(&self, visitor: &mut Visitor) {
        check_condition(&self.predicate, "while", visitor);
        self.predicate.visit(visitor);
        let may_be_empty = has_side_effects(&self.predicate);
        visit_loop_body(&self.body, "while", may_be_empty, visitor);
    }
}

impl Visit for DoUntil {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(predicate) = &self.predicate {
            predicate.visit(visitor)
        }
        visitor.code.loop_depth += 1;
        self.body.visit(visitor);
        visitor.code.loop_depth -= 1;
    }
}

/// Whether `expression`, as a statement on its own, does something.
fn has_effect(expression: &AstNode<Expression>) -> bool {
    match &unparenthesize(expression).data {
        Expression::DyadicExpression { verb, .. } => verb.is_assignment(),
        Expression::Call { .. } | Expression::GlobalCall { .. } | Expression::New { .. } => true,
        Expression::MonadicPreExpression { verb, .. }
        | Expression::MonadicPostExpression { verb, .. } => {
            matches!(verb, MonadicVerb::Increment | MonadicVerb::Decrement)
        }
        Expression::Identifier(id) => id.eq_ignore_ascii_case("stop"),
        // e.g. `class<VoicePack>(DynamicLoadObject(...));`, which loads.
        Expression::Cast { operand, .. } => has_effect(operand),
        _ => false,
    }
}

impl Visit for AstNode<CodeStatement> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            CodeStatement::Empty => {
                visitor.report(Lint::EmptyStatement, "Empty statement", self.span)
            }
            CodeStatement::Expression(expression) => {
                if !has_effect(expression) {
                    visitor.report(
                        Lint::UselessExpression,
                        "Expression has no effect",
                        self.span,
                    )
                }
                if let Expression::DyadicExpression { lhs, verb, rhs } = &expression.data {
                    // Parentheses around a whole operand of an assignment are
                    // redundant. (Other redundant parentheses are reported
                    // when visiting the expression.)
                    if verb.is_assignment() {
                        for operand in [lhs, rhs] {
                            if let Expression::ParentheticalExpression(inner) = &operand.data {
                                if matches!(
                                    inner.data,
                                    Expression::DyadicExpression { .. } | Expression::New { .. }
                                ) {
                                    visitor.report(
                                        Lint::RedundantParentheses,
                                        "Redundant parenthetical expression",
                                        inner.span,
                                    )
                                }
                            }
                        }
                    }
                }
                expression.visit(visitor)
            }
            CodeStatement::Return(expression) => {
                if let Some(expression) = expression {
                    expression.visit(visitor)
                }
            }
            CodeStatement::Break => {
                if visitor.code.loop_depth == 0 && visitor.code.switch_depth == 0 {
                    visitor.report(
                        Lint::BreakOutsideLoop,
                        "`break` outside of a loop or `switch`",
                        self.span,
                    )
                }
            }
            CodeStatement::Continue => {
                if visitor.code.loop_depth == 0 {
                    visitor.report(
                        Lint::ContinueOutsideLoop,
                        "`continue` outside of a loop",
                        self.span,
                    )
                }
            }
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
        self.type_.visit(visitor);
        if let Type::Pod(PodType::Bool) = &self.type_ {
            visit_bool_var_names(&self.names[..], visitor);
        }
    }
}

impl Visit for AstNode<FunctionBody> {
    fn visit(&self, visitor: &mut Visitor) {
        let mut code = Vec::new();
        for statement in &self.statements {
            match statement {
                FunctionBodyStatement::ConstDeclaration(c) => c.visit(visitor),
                FunctionBodyStatement::LocalDeclaration(l) => l.visit(visitor),
                FunctionBodyStatement::CodeStatement(c) => code.push(c),
            }
        }
        visit_statements(&code, visitor)
    }
}

/// Reports modifiers that appear more than once.
fn check_duplicate_modifiers<T: std::fmt::Debug>(
    modifiers: impl IntoIterator<Item = T>,
    kind: &str,
    span: AstSpan,
    visitor: &mut Visitor,
) {
    let mut seen = HashSet::new();
    let mut reported = HashSet::new();
    let mut duplicates = Vec::new();
    for modifier in modifiers {
        let name = format!("{:?}", modifier);
        if !seen.insert(name.clone()) && reported.insert(name.clone()) {
            duplicates.push(name);
        }
    }
    for name in duplicates {
        visitor.report(
            Lint::DuplicateModifier,
            format!("{} has redundant {} modifier", kind, name),
            span,
        )
    }
}

/// Reports more than one access modifier among `names`.
fn check_access_modifiers(names: &[String], span: AstSpan, visitor: &mut Visitor) {
    let mut access: Vec<&str> = names
        .iter()
        .map(|n| n.as_str())
        .filter(|n| matches!(*n, "private" | "protected" | "public"))
        .collect();
    access.sort();
    access.dedup();
    if access.len() > 1 {
        visitor.report(
            Lint::ConflictingAccessModifiers,
            format!("Conflicting access modifiers: {}", access.join(", ")),
            span,
        )
    }
}

impl Visit for AstNode<FunctionDeclaration> {
    fn visit<'a>(&'a self, visitor: &'a mut Visitor) {
        // ensure that only one function type is declared
        if self.types.len() > 1 {
            visitor.report(
                Lint::MultipleFunctionTypes,
                "Multiple function types declared",
                self.span,
            )
        }

        // ensure that the name starts with a capital letter (operators are
        // named with symbols)
        let is_operator = self.types.iter().any(|t| {
            matches!(
                t.type_,
                FunctionTypeType::Operator
                    | FunctionTypeType::PreOperator
                    | FunctionTypeType::PostOperator
            )
        });
        if !is_operator
            && self
                .name
                .string
                .chars()
                .next()
                .is_some_and(|c| !c.is_alphabetic() || !c.is_uppercase())
        {
            visitor.report(
                Lint::FunctionNameCase,
                "Function names should begin with a capital letter",
                self.name.span,
            )
        }

        check_duplicate_modifiers(
            self.modifiers.iter().map(|m| &m.type_),
            "Function",
            self.span,
            visitor,
        );
        let modifier_names: Vec<String> =
            self.modifiers.iter().map(|m| m.type_.to_string()).collect();
        check_access_modifiers(&modifier_names, self.span, visitor);

        // ensure modifiers appear in alphabetical order
        let modifier_types: Vec<FunctionModifierType> =
            self.modifiers.iter().map(|f| f.type_.clone()).collect();
        let mut sorted_modifier_types = modifier_types.clone();
        sorted_modifier_types.sort();
        if modifier_types != sorted_modifier_types {
            let sorted_modifier_type_names: Vec<String> = sorted_modifier_types
                .iter()
                .map(|t| t.to_string())
                .collect();
            visitor.report(
                Lint::UnsortedModifiers,
                format!(
                    "Function modifiers should appear in alphabetical order (e.g., {:?})",
                    sorted_modifier_type_names.join(" ").as_str()
                ),
                self.span,
            )
        }

        // ensure that functions with exec modifier does not consecutive float arguments
        if self
            .modifiers
            .iter()
            .any(|m| m.type_ == FunctionModifierType::Exec)
        {
            let mut last_type: Option<&Type> = None;
            for argument in self.arguments.iter() {
                if let (Type::Pod(PodType::Float), Some(Type::Pod(PodType::Float))) =
                    (&argument.type_, last_type)
                {
                    visitor.report(
                        Lint::ExecConsecutiveFloatArguments,
                        "functions with the exec modifier cannot correctly parse consecutive float arguments when invoked from the console",
                        argument.span,
                    );
                    break;
                }
                last_type = Some(&argument.type_);
            }
        }

        let Some(scope) = visitor.symbols.scope_of(self.name.span.start) else {
            return;
        };
        let context = CodeContext {
            scope,
            function_name: Some(self.name.string.clone()),
            ..CodeContext::default()
        };
        visitor.with_code_context(context, |visitor| {
            for argument in &self.arguments {
                argument.type_.visit(visitor);
            }
            if let Some(body) = &self.body {
                body.visit(visitor)
            }
            visitor.report_unused_locals(scope);
        });
    }
}

impl Visit for Type {
    fn visit(&self, visitor: &mut Visitor) {
        match self {
            Type::Pod(_) => {}
            Type::Array(t) => t.visit(visitor),
            Type::Class(_) | Type::Identifier(_) => {}
            Type::Struct(e) => e.visit(visitor),
            Type::Enum(e) => e.visit(visitor),
        }
    }
}

impl Visit for Rc<AstNode<VarDeclaration>> {
    fn visit(&self, visitor: &mut Visitor) {
        self.type_.visit(visitor);
        if let Type::Pod(PodType::Bool) = &self.type_ {
            visit_bool_var_names(&self.names, visitor)
        }
        let modifiers: Vec<VarModifier> = self.modifiers.iter().map(|m| m.data).collect();
        check_duplicate_modifiers(modifiers.iter(), "Variable", self.span, visitor);
        let modifier_names: Vec<String> = modifiers.iter().map(|m| m.to_string()).collect();
        check_access_modifiers(&modifier_names, self.span, visitor);
        let mut sorted_modifiers = modifiers.clone();
        sorted_modifiers.sort();
        if modifiers != sorted_modifiers {
            visitor.report(
                Lint::UnsortedModifiers,
                format!(
                    "Var modifiers should be in alphabetical order (e.g., {:?})",
                    sorted_modifiers
                ),
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
            visitor.report(
                Lint::TrailingEmptyArguments,
                "Superfluous trailing empty arguments in function call",
                self.span,
            );
        }
        for expression in self.expressions.iter().flatten() {
            expression.visit(visitor)
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
        let context = CodeContext {
            scope: visitor
                .symbols
                .scope_of(self.name.span.start)
                .unwrap_or(ScopeId::CLASS),
            in_state_code: true,
            ..CodeContext::default()
        };
        visitor.with_code_context(context, |visitor| {
            for label in self.labels.iter() {
                let statements: Vec<_> = label.statements.iter().collect();
                visit_statements(&statements, visitor)
            }
        });
    }
}

impl Visit for AstNode<EnumDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if !self.name.string.starts_with('E') {
            visitor.report(
                Lint::EnumNamePrefix,
                "Enumeration types names should be prefixed with the letter \"E\"",
                self.span,
            )
        }
    }
}

impl Visit for AstNode<ClassDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        check_duplicate_modifiers(
            self.modifiers
                .iter()
                .map(|modifier| &modifier.type_.data)
                .filter(|type_| type_.is_unique()),
            "Class",
            self.span,
            visitor,
        );
    }
}

impl Visit for Program {
    fn visit(&self, visitor: &mut Visitor) {
        for statement in self.statements.iter() {
            match &statement.data {
                ProgramStatement::Empty => {
                    visitor.report(Lint::EmptyStatement, "Empty statement", statement.span)
                }
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
        check_symbols(visitor);
    }
}

/// Lints that look at the symbol table as a whole.
fn check_symbols(visitor: &mut Visitor) {
    let symbols = visitor.symbols;
    let line = |id: SymbolId| symbols.symbol(id).span.line.unwrap_or(0);

    // Duplicate declarations.
    for (_, scope) in symbols.scopes() {
        for ids in scope.duplicates() {
            for (i, &id) in ids.iter().enumerate().skip(1) {
                let Some(&first) = ids[..i]
                    .iter()
                    .find(|&&other| conflicts(symbols, other, id))
                else {
                    continue;
                };
                let symbol = symbols.symbol(id);
                visitor.report(
                    Lint::DuplicateDeclaration,
                    format!(
                        "{:?} is already declared on line {}",
                        symbol.name.string,
                        line(first)
                    ),
                    symbol.span,
                )
            }
        }
    }

    for (id, symbol) in symbols.iter() {
        // Locals and arguments that hide a class variable or const.
        let what = match symbol.kind {
            SymbolKind::Local { .. } => "Local variable",
            SymbolKind::Argument { .. } => "Argument",
            _ => "",
        };
        if !what.is_empty() {
            let outer = symbols.scope(symbol.scope).parent;
            let shadowed = outer
                .and_then(|outer| symbols.lookup(outer, Namespace::Value, &symbol.name))
                .filter(|&s| {
                    matches!(
                        symbols.symbol(s).kind,
                        SymbolKind::Variable { .. } | SymbolKind::Const { .. }
                    )
                });
            if let Some(shadowed) = shadowed {
                visitor.report(
                    Lint::ShadowedVariable,
                    format!(
                        "{} {:?} hides the {} declared on line {}",
                        what,
                        symbol.name.string,
                        match symbols.symbol(shadowed).kind {
                            SymbolKind::Const { .. } => "const",
                            _ => "variable",
                        },
                        line(shadowed)
                    ),
                    symbol.span,
                )
            }
        }

        if is_unused_private_member(symbols, id) {
            let what = match symbol.kind {
                SymbolKind::Function { .. } => "function",
                _ => "variable",
            };
            visitor.report(
                Lint::UnusedPrivateMember,
                format!("Private {} {:?} is never used", what, symbol.name.string),
                symbol.span,
            )
        }
    }

    // References written in a different case than the declaration.
    for reference in symbols.references() {
        let declared = &symbols.symbol(reference.symbol).name.string;
        if reference.text != *declared {
            visitor.report(
                Lint::IdentifierCaseMismatch,
                format!("{:?} is declared as {:?}", reference.text, declared),
                reference.span,
            )
        }
    }
}

/// Whether two declarations of the same name in the same scope conflict.
fn conflicts(symbols: &SymbolTable, a: SymbolId, b: SymbolId) -> bool {
    let is_operator = |id: SymbolId| match &symbols.symbol(id).kind {
        SymbolKind::Function { types, .. } => types.iter().any(|t| {
            matches!(
                t,
                FunctionTypeType::Operator
                    | FunctionTypeType::PreOperator
                    | FunctionTypeType::PostOperator
            )
        }),
        _ => false,
    };
    match (&symbols.symbol(a).kind, &symbols.symbol(b).kind) {
        // Operators are overloaded by their operand types.
        _ if is_operator(a) && is_operator(b) => false,
        // UCC accepts the same value in different enums of a class.
        (SymbolKind::EnumValue { enum_: x, .. }, SymbolKind::EnumValue { enum_: y, .. }) => x == y,
        _ => true,
    }
}

/// Whether `id` is a `private` variable or function of the class that
/// nothing in the file uses.
fn is_unused_private_member(symbols: &SymbolTable, id: SymbolId) -> bool {
    let symbol = symbols.symbol(id);
    if symbol.access != Access::Private
        || symbol.scope != ScopeId::CLASS
        || symbols.class().is_some_and(|c| c.is_native)
    {
        return false;
    }
    match &symbol.kind {
        // Native code may use native variables.
        SymbolKind::Variable { modifiers, .. } => {
            if modifiers.contains(&VarModifier::Native) {
                return false;
            }
        }
        SymbolKind::Function {
            types, modifiers, ..
        } => {
            // Called by native code, the engine or the console.
            if modifiers.iter().any(|m| {
                matches!(
                    m,
                    FunctionModifierType::Native
                        | FunctionModifierType::Intrinsic
                        | FunctionModifierType::Exec
                )
            }) || types
                .iter()
                .any(|t| matches!(t, FunctionTypeType::Event | FunctionTypeType::Delegate))
            {
                return false;
            }
        }
        _ => return false,
    }
    if symbols.references_to(id).next().is_some() {
        return false;
    }
    // A call from a state that overrides the function resolves to the
    // state's version, which is still a use of the name.
    if let SymbolKind::Function { .. } = symbol.kind {
        let overridden_and_used = symbols.scopes().any(|(_, scope)| {
            matches!(scope.kind, ScopeKind::State(_))
                && scope
                    .declarations(Namespace::Function, &symbol.name)
                    .iter()
                    .any(|&o| symbols.references_to(o).next().is_some())
        });
        if overridden_and_used {
            return false;
        }
    }
    true
}

fn visit_bool_var_names(names: &[AstNode<VarName>], visitor: &mut Visitor) {
    for name in names {
        let variable_name = &name.identifier.string;
        if !variable_name.starts_with('b') {
            visitor.report(
                Lint::BoolNamePrefix,
                format!(
                    "Boolean variable name {:?} should begin with a \"b\"",
                    variable_name
                ),
                name.span,
            );
        }
    }
}
