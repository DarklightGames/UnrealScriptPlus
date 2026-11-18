//! The symbol table of one UnrealScript file: what it declares, in which
//! scopes, and where each declaration is referenced.
//!
//! The table covers a single class. A name it can't resolve, such as a
//! variable inherited from the parent class or a global enum value, is
//! unknown, not an error: no [`Reference`] is recorded for it. The class's
//! parent is recorded in [`ClassInfo`] so that a project-wide table can later
//! chain the tables of a class hierarchy.
//!
//! All spans are byte offsets into the file.

use crate::ast::*;
use std::collections::HashMap;

/// Identifies a [`Symbol`] in its [`SymbolTable`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId(usize);

impl SymbolId {
    pub fn index(self) -> usize {
        self.0
    }
}

/// Identifies a [`Scope`] in its [`SymbolTable`]. The default is the class
/// scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct ScopeId(usize);

impl ScopeId {
    /// The scope of the class's own members.
    pub const CLASS: ScopeId = ScopeId(0);

    pub fn index(self) -> usize {
        self.0
    }
}

/// The kinds of names, which are looked up separately: a variable and a
/// function can have the same name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    /// Consts, variables, arguments, locals and enum values.
    Value,
    /// Functions, events, delegates and operators.
    Function,
    /// Structs and enums.
    Type,
    State,
    /// State code labels.
    Label,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    Public,
    Protected,
    Private,
}

/// An owned summary of a declared [`Type`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Pod(PodType),
    Array(Box<TypeRef>),
    /// `class<Name>`.
    Class(Identifier),
    /// A struct, enum or class.
    Named(Identifier),
}

impl From<&Type> for TypeRef {
    fn from(type_: &Type) -> Self {
        match type_ {
            Type::Pod(pod) => TypeRef::Pod(*pod),
            Type::Array(inner) => TypeRef::Array(Box::new(inner.as_ref().into())),
            Type::Class(name) => TypeRef::Class(name.data.clone()),
            Type::Struct(s) => TypeRef::Named(s.name.data.clone()),
            Type::Enum(e) => TypeRef::Named(e.name.data.clone()),
            Type::Identifier(name) => TypeRef::Named(name.clone()),
        }
    }
}

/// The value of a `const`.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Integer(i64),
    Float(f32),
    Other,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Const {
        value: ConstValue,
    },
    Variable {
        type_: TypeRef,
        /// The size of a static array, if known.
        array_size: Option<i64>,
        modifiers: Vec<VarModifier>,
    },
    Argument {
        type_: TypeRef,
        array_size: Option<i64>,
        modifiers: Vec<FunctionArgumentModifier>,
    },
    Local {
        type_: TypeRef,
        array_size: Option<i64>,
    },
    Enum,
    EnumValue {
        /// The enum the value belongs to.
        enum_: SymbolId,
        index: usize,
    },
    Struct,
    StructMember {
        type_: TypeRef,
        array_size: Option<i64>,
    },
    Function {
        types: Vec<FunctionTypeType>,
        modifiers: Vec<FunctionModifierType>,
        has_body: bool,
    },
    State {
        /// The state it `extends`, if any.
        extends: Option<Identifier>,
    },
    Label,
}

impl SymbolKind {
    pub fn namespace(&self) -> Namespace {
        match self {
            SymbolKind::Const { .. }
            | SymbolKind::Variable { .. }
            | SymbolKind::Argument { .. }
            | SymbolKind::Local { .. }
            | SymbolKind::EnumValue { .. }
            | SymbolKind::StructMember { .. } => Namespace::Value,
            SymbolKind::Function { .. } => Namespace::Function,
            SymbolKind::Enum | SymbolKind::Struct => Namespace::Type,
            SymbolKind::State { .. } => Namespace::State,
            SymbolKind::Label => Namespace::Label,
        }
    }

    /// The declared type of a variable, argument, local or struct member.
    pub fn type_(&self) -> Option<&TypeRef> {
        match self {
            SymbolKind::Variable { type_, .. }
            | SymbolKind::Argument { type_, .. }
            | SymbolKind::Local { type_, .. }
            | SymbolKind::StructMember { type_, .. } => Some(type_),
            _ => None,
        }
    }

    /// The size of a static array variable, argument, local or struct member.
    pub fn array_size(&self) -> Option<i64> {
        match self {
            SymbolKind::Variable { array_size, .. }
            | SymbolKind::Argument { array_size, .. }
            | SymbolKind::Local { array_size, .. }
            | SymbolKind::StructMember { array_size, .. } => *array_size,
            _ => None,
        }
    }
}

/// A declaration.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The name, as written in the declaration.
    pub name: Identifier,
    /// The span of the name in the declaration.
    pub span: AstSpan,
    pub kind: SymbolKind,
    /// The scope the symbol is declared in.
    pub scope: ScopeId,
    pub access: Access,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// The class's members.
    Class,
    /// A state's functions, consts and labels.
    State(SymbolId),
    /// A function's arguments, locals and consts.
    Function(SymbolId),
    /// A struct's members.
    Struct(SymbolId),
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    /// The enclosing scope, whose names are visible in this one.
    pub parent: Option<ScopeId>,
    names: HashMap<(Namespace, Identifier), Vec<SymbolId>>,
    /// The symbols declared in this scope, in order.
    symbols: Vec<SymbolId>,
}

impl Scope {
    /// The symbols declared in this scope, in declaration order.
    pub fn symbols(&self) -> &[SymbolId] {
        &self.symbols
    }

    /// Every declaration of `name` in this scope (more than one is a
    /// duplicate declaration).
    pub fn declarations(&self, namespace: Namespace, name: &Identifier) -> &[SymbolId] {
        self.names
            .get(&(namespace, name.clone()))
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Names declared more than once in this scope.
    pub fn duplicates(&self) -> impl Iterator<Item = &[SymbolId]> {
        self.names
            .values()
            .filter(|ids| ids.len() > 1)
            .map(|ids| ids.as_slice())
    }
}

/// How a [`Reference`] uses its symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    /// Its value is used, or may be: this includes passing it as an argument
    /// (which may be an `out` argument), `X.Y = ...` and `X[0] = ...`.
    Read,
    /// It's assigned (`X = ...`, `X += ...`) or stepped (`X++;`) without its
    /// value being used.
    Write,
    /// It's called.
    Call,
    /// It's assigned a default value in `defaultproperties`.
    DefaultProperty,
    /// It's listed in the replication block.
    Replication,
    /// It's named without being used as a value: a `goto` label, a state's
    /// `extends` or `ignores`.
    Name,
}

/// A use of a declared name.
#[derive(Debug, Clone)]
pub struct Reference {
    pub symbol: SymbolId,
    pub span: AstSpan,
    /// The name as written here (which may differ in case from the
    /// declaration).
    pub text: String,
    pub kind: ReferenceKind,
}

/// About the class a file declares.
#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub name: Identifier,
    /// The span of the class's name.
    pub span: AstSpan,
    pub parent: Option<Identifier>,
    /// Whether the class is `native` or `intrinsic`, so that native code may
    /// use its members.
    pub is_native: bool,
}

/// The declarations and references of one file. See the module docs.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    class: Option<ClassInfo>,
    symbols: Vec<Symbol>,
    scopes: Vec<Scope>,
    /// Sorted by start.
    references: Vec<Reference>,
    /// Indices into `references`, per symbol.
    references_by_symbol: Vec<Vec<usize>>,
    /// The scope of each function and state, by the start of its name.
    scopes_by_declaration: HashMap<usize, ScopeId>,
}

impl SymbolTable {
    /// Builds the symbol table of `program`.
    pub fn build(program: &Program) -> SymbolTable {
        let mut table = SymbolTable {
            class: None,
            symbols: Vec::new(),
            scopes: Vec::new(),
            references: Vec::new(),
            references_by_symbol: Vec::new(),
            scopes_by_declaration: HashMap::new(),
        };
        table.add_scope(ScopeKind::Class, None);
        table.declare_program(program);
        Resolver {
            table: &mut table,
            scope: ScopeId::CLASS,
        }
        .program(program);
        table.finish();
        table
    }

    pub fn class(&self) -> Option<&ClassInfo> {
        self.class.as_ref()
    }

    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0]
    }

    /// Every symbol with its id, in declaration order.
    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, &Symbol)> {
        self.symbols
            .iter()
            .enumerate()
            .map(|(i, s)| (SymbolId(i), s))
    }

    pub fn scopes(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter().enumerate().map(|(i, s)| (ScopeId(i), s))
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0]
    }

    /// Whether `id` is the scope of a `static` function.
    pub fn is_static_function_scope(&self, id: ScopeId) -> bool {
        match self.scope(id).kind {
            ScopeKind::Function(f) => matches!(
                &self.symbol(f).kind,
                SymbolKind::Function { modifiers, .. }
                    if modifiers.contains(&FunctionModifierType::Static)
            ),
            _ => false,
        }
    }

    /// The scope of the function or state whose name starts at `offset`.
    pub fn scope_of(&self, offset: usize) -> Option<ScopeId> {
        self.scopes_by_declaration.get(&offset).copied()
    }

    /// The nearest scope, starting at `scope`, that is a state's or the
    /// class's: where `Self.X` is looked up.
    pub fn object_scope(&self, mut scope: ScopeId) -> ScopeId {
        loop {
            let s = self.scope(scope);
            match (s.kind, s.parent) {
                (ScopeKind::Class | ScopeKind::State(_), _) | (_, None) => return scope,
                (_, Some(parent)) => scope = parent,
            }
        }
    }

    /// Looks `name` up in `scope` and the scopes that enclose it.
    pub fn lookup(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &Identifier,
    ) -> Option<SymbolId> {
        let mut scope = Some(scope);
        while let Some(id) = scope {
            let s = self.scope(id);
            if let Some(&first) = s.declarations(namespace, name).first() {
                return Some(first);
            }
            scope = s.parent;
        }
        None
    }

    /// Every resolved reference, sorted by position.
    pub fn references(&self) -> &[Reference] {
        &self.references
    }

    /// The references to `symbol`, in order.
    pub fn references_to(&self, symbol: SymbolId) -> impl Iterator<Item = &Reference> {
        self.references_by_symbol[symbol.0]
            .iter()
            .map(|&i| &self.references[i])
    }

    /// The symbol whose declared name contains `offset`.
    pub fn symbol_at(&self, offset: usize) -> Option<SymbolId> {
        self.symbols
            .iter()
            .position(|s| s.span.start <= offset && offset < s.span.end)
            .map(SymbolId)
    }

    /// The reference that contains `offset`.
    pub fn reference_at(&self, offset: usize) -> Option<&Reference> {
        let i = self.references.partition_point(|r| r.span.end <= offset);
        self.references
            .get(i)
            .filter(|r| r.span.start <= offset && offset < r.span.end)
    }

    /// Sets the line of every span, given the line of a byte offset.
    pub fn set_lines(&mut self, line_of: impl Fn(usize) -> usize) {
        for symbol in &mut self.symbols {
            symbol.span.line = Some(line_of(symbol.span.start));
        }
        for reference in &mut self.references {
            reference.span.line = Some(line_of(reference.span.start));
        }
        if let Some(class) = &mut self.class {
            class.span.line = Some(line_of(class.span.start));
        }
    }

    // Building.

    fn add_scope(&mut self, kind: ScopeKind, parent: Option<ScopeId>) -> ScopeId {
        self.scopes.push(Scope {
            kind,
            parent,
            names: HashMap::new(),
            symbols: Vec::new(),
        });
        ScopeId(self.scopes.len() - 1)
    }

    fn add(
        &mut self,
        scope: ScopeId,
        name: &AstNode<Identifier>,
        kind: SymbolKind,
        access: Access,
    ) -> SymbolId {
        let id = SymbolId(self.symbols.len());
        let s = &mut self.scopes[scope.0];
        s.names
            .entry((kind.namespace(), name.data.clone()))
            .or_default()
            .push(id);
        s.symbols.push(id);
        self.symbols.push(Symbol {
            name: name.data.clone(),
            span: name.span,
            kind,
            scope,
            access,
        });
        id
    }

    fn resolve_size(&self, scope: ScopeId, size: &Option<AstNode<VarSize>>) -> Option<i64> {
        match &size.as_ref()?.data {
            VarSize::IntegerLiteral(n) => match n.data {
                NumericLiteral::Integer(i) => Some(i as i64),
                NumericLiteral::Float(_) => None,
            },
            VarSize::Identifier(name) => {
                let id = self.lookup(scope, Namespace::Value, name)?;
                match self.symbol(id).kind {
                    SymbolKind::Const {
                        value: ConstValue::Integer(i),
                    } => Some(i),
                    _ => None,
                }
            }
        }
    }

    fn declare_const(&mut self, scope: ScopeId, c: &AstNode<ConstDeclaration>) {
        let value = match &c.value.data {
            Literal::Numeric(n) => match n.data {
                NumericLiteral::Integer(i) => ConstValue::Integer(i as i64),
                NumericLiteral::Float(f) => ConstValue::Float(f),
            },
            _ => ConstValue::Other,
        };
        self.add(scope, &c.name, SymbolKind::Const { value }, Access::Public);
    }

    /// Declares the structs and enums declared inline in `type_`.
    fn declare_inline_types(&mut self, type_: &Type) {
        match type_ {
            Type::Struct(s) => self.declare_struct(s),
            Type::Enum(e) => self.declare_enum(e),
            Type::Array(inner) => self.declare_inline_types(inner),
            Type::Pod(_) | Type::Class(_) | Type::Identifier(_) => {}
        }
    }

    fn declare_enum(&mut self, e: &AstNode<EnumDeclaration>) {
        let enum_ = self.add(ScopeId::CLASS, &e.name, SymbolKind::Enum, Access::Public);
        for (index, value) in e.values.iter().enumerate() {
            self.add(
                ScopeId::CLASS,
                value,
                SymbolKind::EnumValue { enum_, index },
                Access::Public,
            );
        }
    }

    fn declare_struct(&mut self, s: &AstNode<StructDeclaration>) {
        let id = self.add(ScopeId::CLASS, &s.name, SymbolKind::Struct, Access::Public);
        let scope = self.add_scope(ScopeKind::Struct(id), Some(ScopeId::CLASS));
        for member in &s.members {
            self.declare_inline_types(&member.type_);
            let access = struct_member_access(&member.modifiers);
            for name in &member.names {
                let kind = SymbolKind::StructMember {
                    type_: (&member.type_).into(),
                    array_size: self.resolve_size(scope, &name.size),
                };
                self.add(scope, &name.identifier, kind, access);
            }
        }
    }

    fn declare_function(&mut self, scope: ScopeId, f: &AstNode<FunctionDeclaration>) {
        let modifiers: Vec<FunctionModifierType> =
            f.modifiers.iter().map(|m| m.type_.clone()).collect();
        let access = access_of(modifiers.iter().map(|m| match m {
            FunctionModifierType::Private => Some(Access::Private),
            FunctionModifierType::Protected => Some(Access::Protected),
            _ => None,
        }));
        let kind = SymbolKind::Function {
            types: f.types.iter().map(|t| t.type_).collect(),
            modifiers,
            has_body: f.body.is_some(),
        };
        let id = self.add(scope, &f.name, kind, access);
        let function_scope = self.add_scope(ScopeKind::Function(id), Some(scope));
        self.scopes_by_declaration
            .insert(f.name.span.start, function_scope);

        for argument in &f.arguments {
            let kind = SymbolKind::Argument {
                type_: (&argument.type_).into(),
                array_size: self.resolve_size(function_scope, &argument.name.size),
                modifiers: argument.modifiers.iter().copied().collect(),
            };
            self.add(
                function_scope,
                &argument.name.identifier,
                kind,
                Access::Public,
            );
        }
        let Some(body) = &f.body else {
            return;
        };
        for statement in &body.statements {
            match statement {
                FunctionBodyStatement::ConstDeclaration(c) => self.declare_const(function_scope, c),
                FunctionBodyStatement::LocalDeclaration(l) => {
                    for name in &l.names {
                        let kind = SymbolKind::Local {
                            type_: (&l.type_).into(),
                            array_size: self.resolve_size(function_scope, &name.size),
                        };
                        self.add(function_scope, &name.identifier, kind, Access::Public);
                    }
                }
                // Consts among the code are declared when resolving.
                FunctionBodyStatement::CodeStatement(_) => {}
            }
        }
    }

    fn declare_state(&mut self, state: &StateDeclaration) {
        let id = self.add(
            ScopeId::CLASS,
            &state.name,
            SymbolKind::State {
                extends: state.parent.as_ref().map(|p| p.data.clone()),
            },
            Access::Public,
        );
        let scope = self.add_scope(ScopeKind::State(id), Some(ScopeId::CLASS));
        self.scopes_by_declaration
            .insert(state.name.span.start, scope);
        for statement in &state.statements {
            match statement {
                StateStatement::ConstDeclaration(c) => self.declare_const(scope, c),
                StateStatement::FunctionDeclaration(f) => self.declare_function(scope, f),
            }
        }
        for label in &state.labels {
            self.add(scope, &label.label, SymbolKind::Label, Access::Public);
            // Labels can also appear anywhere in the code, even in blocks.
            self.declare_labels(scope, &label.statements);
        }
    }

    fn declare_labels(&mut self, scope: ScopeId, statements: &[AstNode<CodeStatement>]) {
        for statement in statements {
            match &statement.data {
                CodeStatement::JumpLabel(name) => {
                    self.add(scope, name, SymbolKind::Label, Access::Public);
                }
                CodeStatement::Conditional(c) => {
                    let bodies = c
                        .if_statement
                        .body
                        .iter()
                        .chain(c.elif_statements.iter().map(|e| &e.body))
                        .chain(c.else_statement.iter().map(|e| &e.body));
                    for body in bodies {
                        self.declare_labels_in(scope, body);
                    }
                }
                CodeStatement::ForEach(f) => self.declare_labels_in(scope, &f.body),
                CodeStatement::For(f) => self.declare_labels_in(scope, &f.body),
                CodeStatement::While(w) => self.declare_labels_in(scope, &w.body),
                CodeStatement::DoUntil(d) => self.declare_labels_in(scope, &d.body),
                CodeStatement::Switch(s) => {
                    for case in &s.cases {
                        self.declare_labels(scope, &case.statements);
                    }
                }
                _ => {}
            }
        }
    }

    fn declare_labels_in(&mut self, scope: ScopeId, body: &AstNode<CodeStatementOrBlock>) {
        match &body.data {
            CodeStatementOrBlock::CodeStatement(s) => {
                self.declare_labels(scope, std::slice::from_ref(s))
            }
            CodeStatementOrBlock::CodeBlock(b) => self.declare_labels(scope, &b.statements),
        }
    }

    fn declare_program(&mut self, program: &Program) {
        // Consts and types first, as variables' array sizes and types use them.
        for statement in &program.statements {
            match &statement.data {
                ProgramStatement::ClassDeclaration(c) => {
                    let is_native = c.modifiers.iter().any(|m| {
                        matches!(
                            m.type_.data,
                            ClassModifierType::Native | ClassModifierType::Intrinsic
                        )
                    });
                    self.class = Some(ClassInfo {
                        name: c.name.data.clone(),
                        span: c.name.span,
                        parent: c.parent_class.as_ref().map(|p| p.data.clone()),
                        is_native,
                    });
                }
                ProgramStatement::ConstDeclaration(c) => self.declare_const(ScopeId::CLASS, c),
                ProgramStatement::EnumDeclaration(e) => self.declare_enum(e),
                ProgramStatement::StructDeclaration(s) => self.declare_struct(s),
                _ => {}
            }
        }
        for statement in &program.statements {
            match &statement.data {
                ProgramStatement::VarDeclaration(v) => {
                    self.declare_inline_types(&v.type_);
                    let modifiers: Vec<VarModifier> = v.modifiers.iter().map(|m| m.data).collect();
                    let access = access_of(modifiers.iter().map(|m| match m {
                        VarModifier::Private => Some(Access::Private),
                        VarModifier::Protected => Some(Access::Protected),
                        _ => None,
                    }));
                    for name in &v.names {
                        let kind = SymbolKind::Variable {
                            type_: (&v.type_).into(),
                            array_size: self.resolve_size(ScopeId::CLASS, &name.size),
                            modifiers: modifiers.clone(),
                        };
                        self.add(ScopeId::CLASS, &name.identifier, kind, access);
                    }
                }
                ProgramStatement::FunctionDeclaration(f) => {
                    self.declare_function(ScopeId::CLASS, f)
                }
                ProgramStatement::StateDeclaration(s) => self.declare_state(s),
                _ => {}
            }
        }
    }

    fn finish(&mut self) {
        self.references.sort_by_key(|r| r.span.start);
        self.references_by_symbol = vec![Vec::new(); self.symbols.len()];
        for (i, reference) in self.references.iter().enumerate() {
            self.references_by_symbol[reference.symbol.0].push(i);
        }
    }
}

/// The most restrictive access among `modifiers`, or public.
fn access_of(modifiers: impl Iterator<Item = Option<Access>>) -> Access {
    let mut access = Access::Public;
    for m in modifiers.flatten() {
        if m == Access::Private || access == Access::Public {
            access = m;
        }
    }
    access
}

fn struct_member_access(modifiers: &[StructVarModifier]) -> Access {
    access_of(modifiers.iter().map(|m| match m {
        StructVarModifier::Private => Some(Access::Private),
        StructVarModifier::Protected => Some(Access::Protected),
        _ => None,
    }))
}

fn unparenthesize(mut expression: &AstNode<Expression>) -> &AstNode<Expression> {
    while let Expression::ParentheticalExpression(inner) = &expression.data {
        expression = inner;
    }
    expression
}

/// The name in a name literal such as `'Begin'`, spanning the text between
/// the quotes. `''` (no name) is `None`.
pub fn name_literal(expression: &AstNode<Expression>) -> Option<AstNode<Identifier>> {
    let Expression::Literal(literal) = &expression.data else {
        return None;
    };
    let Literal::Name(name) = &literal.data else {
        return None;
    };
    if name.is_empty() {
        return None;
    }
    Some(AstNode {
        data: Identifier::new(name),
        span: AstSpan {
            start: literal.span.start + 1,
            end: literal.span.end - 1,
            line: None,
        },
    })
}

fn is_self(expression: &AstNode<Expression>) -> bool {
    matches!(&expression.data, Expression::Identifier(id) if id.eq_ignore_ascii_case("self"))
}

/// Records the references in the code of a program.
struct Resolver<'t> {
    table: &'t mut SymbolTable,
    /// The scope names are looked up in.
    scope: ScopeId,
}

impl Resolver<'_> {
    /// Looks `name` up in each of `namespaces` in turn, starting at `scope`,
    /// and records a reference if it's found.
    fn refer(
        &mut self,
        scope: ScopeId,
        namespaces: &[Namespace],
        name: &AstNode<Identifier>,
        span: AstSpan,
        kind: ReferenceKind,
    ) {
        let found = namespaces
            .iter()
            .find_map(|&ns| self.table.lookup(scope, ns, name));
        if let Some(symbol) = found {
            self.table.references.push(Reference {
                symbol,
                span,
                text: name.string.clone(),
                kind,
            });
        }
    }

    fn refer_here(
        &mut self,
        namespaces: &[Namespace],
        name: &AstNode<Identifier>,
        kind: ReferenceKind,
    ) {
        self.refer(self.scope, namespaces, name, name.span, kind)
    }

    fn with_scope(&mut self, scope: ScopeId, f: impl FnOnce(&mut Self)) {
        let outer = std::mem::replace(&mut self.scope, scope);
        f(self);
        self.scope = outer;
    }

    fn program(&mut self, program: &Program) {
        for statement in &program.statements {
            match &statement.data {
                ProgramStatement::VarDeclaration(v) => self.var_names(&v.names),
                ProgramStatement::StructDeclaration(s) => self.struct_(s),
                ProgramStatement::FunctionDeclaration(f) => self.function(f),
                ProgramStatement::ReplicationBlock(r) => self.replication(r),
                ProgramStatement::StateDeclaration(s) => self.state(s),
                ProgramStatement::DefaultProperties(d) => self.default_properties(d),
                ProgramStatement::Empty
                | ProgramStatement::ClassDeclaration(_)
                | ProgramStatement::CompilerDirective(_)
                | ProgramStatement::ConstDeclaration(_)
                | ProgramStatement::EnumDeclaration(_)
                | ProgramStatement::CppText(_) => {}
            }
        }
    }

    /// References in static array sizes.
    fn var_names(&mut self, names: &[AstNode<VarName>]) {
        for name in names {
            if let Some(size) = &name.size {
                if let VarSize::Identifier(id) = &size.data {
                    self.refer_here(&[Namespace::Value], id, ReferenceKind::Read);
                }
            }
        }
    }

    fn struct_(&mut self, s: &AstNode<StructDeclaration>) {
        for member in &s.members {
            self.var_names(&member.names);
        }
    }

    fn function(&mut self, f: &AstNode<FunctionDeclaration>) {
        let Some(scope) = self.table.scope_of(f.name.span.start) else {
            return;
        };
        self.with_scope(scope, |r| {
            for argument in &f.arguments {
                r.var_names(std::slice::from_ref(&argument.name));
            }
            let Some(body) = &f.body else {
                return;
            };
            for statement in &body.statements {
                match statement {
                    FunctionBodyStatement::ConstDeclaration(_) => {}
                    FunctionBodyStatement::LocalDeclaration(l) => r.var_names(&l.names),
                    FunctionBodyStatement::CodeStatement(s) => r.statement(s),
                }
            }
        });
    }

    fn replication(&mut self, block: &AstNode<ReplicationBlock>) {
        for statement in &block.statements {
            self.expression(&statement.condition);
            for member in &statement.variables {
                self.refer_here(
                    &[Namespace::Value, Namespace::Function],
                    member,
                    ReferenceKind::Replication,
                );
            }
        }
    }

    fn state(&mut self, state: &StateDeclaration) {
        if let Some(parent) = &state.parent {
            self.refer_here(&[Namespace::State], parent, ReferenceKind::Name);
        }
        for ignored in &state.ignores {
            self.refer_here(&[Namespace::Function], ignored, ReferenceKind::Name);
        }
        for statement in &state.statements {
            if let StateStatement::FunctionDeclaration(f) = statement {
                self.function(f);
            }
        }
        let Some(scope) = self.table.scope_of(state.name.span.start) else {
            return;
        };
        self.with_scope(scope, |r| {
            for label in &state.labels {
                for statement in &label.statements {
                    r.statement(statement);
                }
            }
        });
    }

    fn default_properties(&mut self, d: &AstNode<DefaultProperties>) {
        use t3d::syntax::StatementKind;
        // Only top-level assignments set the class's own properties.
        for statement in &d.statements {
            if let StatementKind::Assignment(a) = &statement.kind {
                let span = AstSpan {
                    start: a.span.start,
                    end: a.span.start + a.name.len(),
                    line: None,
                };
                let name = AstNode {
                    data: Identifier::new(&a.name),
                    span,
                };
                self.refer(
                    ScopeId::CLASS,
                    &[Namespace::Value],
                    &name,
                    span,
                    ReferenceKind::DefaultProperty,
                );
            }
        }
    }

    fn body(&mut self, body: &AstNode<CodeStatementOrBlock>) {
        match &body.data {
            CodeStatementOrBlock::CodeStatement(s) => self.statement(s),
            CodeStatementOrBlock::CodeBlock(b) => {
                for s in &b.statements {
                    self.statement(s)
                }
            }
        }
    }

    fn statement(&mut self, statement: &AstNode<CodeStatement>) {
        match &statement.data {
            CodeStatement::Empty
            | CodeStatement::Break
            | CodeStatement::Continue
            | CodeStatement::JumpLabel(_)
            | CodeStatement::CompilerDirective(_) => {}
            CodeStatement::ConstDeclaration(c) => self.table.declare_const(self.scope, c),
            CodeStatement::Expression(e) => self.discarded(e),
            CodeStatement::Return(e) => {
                if let Some(e) = e {
                    self.expression(e)
                }
            }
            CodeStatement::Goto(label) => {
                self.refer_here(&[Namespace::Label], label, ReferenceKind::Name)
            }
            CodeStatement::ForEach(f) => {
                self.expression(&f.predicate);
                self.body(&f.body);
            }
            CodeStatement::For(f) => {
                if let Some(init) = &f.init {
                    self.discarded(init)
                }
                if let Some(predicate) = &f.predicate {
                    self.expression(predicate)
                }
                if let Some(post) = &f.post {
                    self.discarded(post)
                }
                self.body(&f.body);
            }
            CodeStatement::Switch(s) => {
                self.expression(&s.predicate);
                for case in &s.cases {
                    if let SwitchCaseType::Expression(label) = &case.type_ {
                        self.expression(label);
                    }
                    for statement in &case.statements {
                        self.statement(statement);
                    }
                }
            }
            CodeStatement::Conditional(c) => {
                self.expression(&c.if_statement.predicate);
                if let Some(body) = &c.if_statement.body {
                    self.body(body);
                }
                for elif in &c.elif_statements {
                    self.expression(&elif.predicate);
                    self.body(&elif.body);
                }
                if let Some(e) = &c.else_statement {
                    self.body(&e.body);
                }
            }
            CodeStatement::While(w) => {
                self.expression(&w.predicate);
                self.body(&w.body);
            }
            CodeStatement::DoUntil(d) => {
                self.body(&d.body);
                if let Some(predicate) = &d.predicate {
                    self.expression(predicate);
                }
            }
        }
    }

    /// An expression whose value isn't used: a statement, or a `for` loop's
    /// init or post clause. There, `X++` changes `X` without using it.
    fn discarded(&mut self, expression: &AstNode<Expression>) {
        if let Expression::MonadicPreExpression { operand, verb }
        | Expression::MonadicPostExpression { operand, verb } = &unparenthesize(expression).data
        {
            if matches!(verb, MonadicVerb::Increment | MonadicVerb::Decrement)
                && self.write_target(operand)
            {
                return;
            }
        }
        self.expression(expression)
    }

    /// If `target` names a variable directly (`X`, `Self.X`, `default.X`),
    /// records a write to it and returns `true`.
    fn write_target(&mut self, target: &AstNode<Expression>) -> bool {
        const NAMESPACES: &[Namespace] = &[Namespace::Value, Namespace::Function];
        match &target.data {
            Expression::Identifier(name) => {
                self.refer_here(NAMESPACES, name, ReferenceKind::Write);
                true
            }
            Expression::MemberAccess { operand, target } if is_self(operand) => {
                let scope = self.table.object_scope(self.scope);
                self.refer(scope, NAMESPACES, target, target.span, ReferenceKind::Write);
                true
            }
            Expression::DefaultAccess { operand, target }
                if operand.as_ref().is_none_or(|o| is_self(o)) =>
            {
                self.refer(
                    ScopeId::CLASS,
                    &[Namespace::Value],
                    target,
                    target.span,
                    ReferenceKind::Write,
                );
                true
            }
            _ => false,
        }
    }

    /// The function being called in a call expression.
    fn callee(&mut self, operand: &AstNode<Expression>) {
        const NAMESPACES: &[Namespace] = &[Namespace::Function, Namespace::Value];
        match &operand.data {
            Expression::Identifier(name) => self.refer_here(NAMESPACES, name, ReferenceKind::Call),
            Expression::MemberAccess { operand, target } if is_self(operand) => {
                let scope = self.table.object_scope(self.scope);
                self.refer(scope, NAMESPACES, target, target.span, ReferenceKind::Call)
            }
            Expression::StaticAccess {
                operand: None,
                target,
            } => self.refer(
                ScopeId::CLASS,
                &[Namespace::Function],
                target,
                target.span,
                ReferenceKind::Call,
            ),
            _ => self.expression(operand),
        }
    }

    /// Records the state and label named by `Goto('Label')` and
    /// `GotoState('State', 'Label')`.
    fn state_transition(
        &mut self,
        callee: &AstNode<Expression>,
        arguments: &AstNode<ExpressionList>,
    ) {
        let Expression::Identifier(function) = &callee.data else {
            return;
        };
        let names: Vec<Option<AstNode<Identifier>>> = arguments
            .expressions
            .iter()
            .map(|a| a.as_deref().and_then(name_literal))
            .collect();
        if function.eq_ignore_ascii_case("goto") {
            if let Some(Some(label)) = names.first() {
                self.refer_here(&[Namespace::Label], label, ReferenceKind::Name);
            }
        } else if function.eq_ignore_ascii_case("gotostate") {
            let label = names.get(1).and_then(|l| l.as_ref());
            match (arguments.expressions.first(), names.first()) {
                // `GotoState('State', 'Label')`
                (_, Some(Some(state))) => {
                    self.refer(
                        ScopeId::CLASS,
                        &[Namespace::State],
                        state,
                        state.span,
                        ReferenceKind::Name,
                    );
                    let state_scope = self
                        .table
                        .lookup(ScopeId::CLASS, Namespace::State, state)
                        .and_then(|id| self.table.scope_of(self.table.symbol(id).span.start));
                    if let (Some(label), Some(state_scope)) = (label, state_scope) {
                        self.refer(
                            state_scope,
                            &[Namespace::Label],
                            label,
                            label.span,
                            ReferenceKind::Name,
                        );
                    }
                }
                // `GotoState(SomeExpression, 'Label')`: it may be any state.
                (Some(Some(_)), _) => {
                    if let Some(label) = label {
                        self.refer_in_every_state(label);
                    }
                }
                // `GotoState( , 'Label')`: the current state.
                _ => {
                    if let Some(label) = label {
                        let scope = self.table.object_scope(self.scope);
                        self.refer(
                            scope,
                            &[Namespace::Label],
                            label,
                            label.span,
                            ReferenceKind::Name,
                        );
                    }
                }
            }
        }
    }

    /// Records a reference to `label` in every state that declares it.
    fn refer_in_every_state(&mut self, label: &AstNode<Identifier>) {
        let symbols: Vec<SymbolId> = self
            .table
            .scopes
            .iter()
            .filter(|s| matches!(s.kind, ScopeKind::State(_)))
            .flat_map(|s| s.declarations(Namespace::Label, label).iter().copied())
            .collect();
        for symbol in symbols {
            self.table.references.push(Reference {
                symbol,
                span: label.span,
                text: label.string.clone(),
                kind: ReferenceKind::Name,
            });
        }
    }

    fn arguments(&mut self, arguments: &AstNode<ExpressionList>) {
        for argument in arguments.expressions.iter().flatten() {
            self.expression(argument);
        }
    }

    fn expression(&mut self, expression: &AstNode<Expression>) {
        match &expression.data {
            Expression::Identifier(name) => self.refer_here(
                &[Namespace::Value, Namespace::Function],
                name,
                ReferenceKind::Read,
            ),
            Expression::Literal(_) => {}
            Expression::New { arguments, type_ } => {
                if let Some(arguments) = arguments {
                    self.arguments(arguments);
                }
                self.expression(type_);
            }
            Expression::MonadicPreExpression { operand, .. }
            | Expression::MonadicPostExpression { operand, .. } => self.expression(operand),
            Expression::DyadicExpression { lhs, verb, rhs } => {
                // Compound assignments (`X += 1`) read `X`, but only to
                // update it, so they count as writes.
                if !(verb.is_assignment() && self.write_target(lhs)) {
                    self.expression(lhs);
                }
                self.expression(rhs);
            }
            Expression::Call { operand, arguments } => {
                self.callee(operand);
                self.arguments(arguments);
                self.state_transition(operand, arguments);
            }
            Expression::GlobalCall { name, arguments } => {
                // `global.F()` calls the class's version, not a state's.
                self.refer(
                    ScopeId::CLASS,
                    &[Namespace::Function],
                    name,
                    name.span,
                    ReferenceKind::Call,
                );
                self.arguments(arguments);
            }
            Expression::ArrayAccess { operand, argument } => {
                self.expression(operand);
                self.expression(argument);
            }
            Expression::DefaultAccess { operand, target } => match operand {
                Some(operand) if !is_self(operand) => self.expression(operand),
                _ => self.refer(
                    ScopeId::CLASS,
                    &[Namespace::Value],
                    target,
                    target.span,
                    ReferenceKind::Read,
                ),
            },
            Expression::StaticAccess { operand, target } => match operand {
                Some(operand) => self.expression(operand),
                None => self.refer(
                    ScopeId::CLASS,
                    &[Namespace::Function],
                    target,
                    target.span,
                    ReferenceKind::Read,
                ),
            },
            Expression::MemberAccess { operand, target } => {
                if is_self(operand) {
                    let scope = self.table.object_scope(self.scope);
                    self.refer(
                        scope,
                        &[Namespace::Value, Namespace::Function],
                        target,
                        target.span,
                        ReferenceKind::Read,
                    );
                } else {
                    self.expression(operand);
                }
            }
            Expression::Cast { operand, .. } => self.expression(operand),
            Expression::ParentheticalExpression(inner) => self.expression(inner),
        }
    }
}
