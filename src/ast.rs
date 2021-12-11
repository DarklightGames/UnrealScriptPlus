use strum_macros::{EnumString, Display};
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

#[derive(Debug, Copy, Clone)]
pub struct AstSpan {
    pub start: usize,
    pub end: usize,
    pub line: Option<usize>,
}

#[derive(Debug)]
pub struct AstNode<Data> {
    pub data: Data,
    pub span: AstSpan,
}

impl<T> Deref for AstNode<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: PartialEq> PartialEq for AstNode<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data.eq(&other.data)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum ClassModifierType {
    Abstract,
    CacheExempt,
    CollapseCategories,
    Config,
    DependsOn,
    DontCollapseCategories,
    EditInlineNew,
    ExportStructs,
    Guid,
    HideCategories,
    HideDropDown,
    Instanced,
    Intrinsic,
    Native,
    NativeReplication,
    NoExport,
    NotEditInlineNew,
    NotPlaceable,
    ParseConfig,
    PerObjectConfig,
    Placeable,
    SafeReplace,
    ShowCategories,
    Transient,
}

impl ClassModifierType {
    pub fn is_unique(&self) -> bool {
        return match self {
            ClassModifierType::DependsOn => false,
            _ => true
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd, Display, EnumString, Hash)]
#[strum(serialize_all="lowercase")]
pub enum FunctionModifierType {
    Exec,
    Final,
    Intrinsic,
    Iterator,
    Latent,
    Native,
    Private,
    Protected,
    Public,
    Simulated,
    Singular,
    Static,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum FunctionArgumentModifier {
    Coerce,
    Optional,
    Out,
    Skip,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum StructModifier {
    Export,
    Init,
    Long,
    Native,
    Transient,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum StructVarModifier {
    Config,
    Const,
    EdFindable,
    EditConst,
    EditConstArray,
    EditInline,
    EditInlineNotify,
    EditInlineUse,
    Export,
    GlobalConfig,
    Localized,
    NoExport,
    Private,
    Protected,
    Public,
    Transient,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum StateModifier {
    Auto,
    Simulated,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Copy, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum VarModifier {
    Automated,
    Cache,
    Config,
    Const,
    Deprecated,
    EdFindable,
    EditConst,
    EditConstArray,
    EditInline,
    EditInlineNotify,
    EditInlineUse,
    Export,
    GlobalConfig,
    Input,
    Localized,
    Native,
    NoExport,
    Private,
    Protected,
    Public,
    Transient,
    Travel,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum ReplicationReliability {
    Reliable,
    Unreliable,
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum FunctionTypeType {
    Delegate,
    Event,
    Function,
    Operator,
    PostOperator,
    PreOperator,
}

#[derive(Debug)]
pub struct FunctionType {
    pub type_: FunctionTypeType,
    pub arguments: Option<Vec<AstNode<NumericLiteral>>>,
}

#[derive(Debug, PartialEq, Display, EnumString)]
pub enum MonadicVerb {
    #[strum(serialize="++")]
    Increment,
    #[strum(serialize="--")]
    Decrement,
    #[strum(serialize="~")]
    BitwiseNot,
    #[strum(serialize="-")]
    Negate,
    #[strum(serialize="!")]
    Not,
}

#[derive(Debug, PartialEq, Display, EnumString)]
pub enum DyadicVerb {
    #[strum(serialize="&&")]
    LogicalAnd,
    #[strum(serialize="^^")]
    LogicalExclusiveOr,
    #[strum(serialize="**")]
    Exponentiate,
    #[strum(serialize="+=")]
    AddAssign,
    #[strum(serialize="-=")]
    SubtractAssign,
    #[strum(serialize="*=")]
    MultiplyAssign,
    #[strum(serialize="/=")]
    DivideAssign,
    #[strum(serialize="*")]
    Multiply,
    #[strum(serialize="/")]
    Divide,
    #[strum(serialize="||")]
    LogicalOr,
    #[strum(serialize="%")]
    Modulo,
    #[strum(serialize="<<")]
    ShiftLeft,
    #[strum(serialize=">>>")]
    DoubleShiftRight,
    #[strum(serialize=">>")]
    ShiftRight,
    #[strum(serialize="<=")]
    CompareLessThanOrEqual,
    #[strum(serialize=">=")]
    CompareGreaterThanOrEqual,
    #[strum(serialize="==")]
    CompareEqual,
    #[strum(serialize="~=")]
    CompareApproximatelyEqual,
    #[strum(serialize="<")]
    CompareLessThan,
    #[strum(serialize=">")]
    CompareGreaterThan,
    #[strum(serialize="!=")]
    CompareNotEqual,
    #[strum(serialize="&")]
    BitwiseAnd,
    #[strum(serialize="|")]
    BitwiseOr,
    #[strum(serialize="^")]
    BitwiseExclusiveOr,
    #[strum(serialize="$=")]
    ConcatenateAssign,
    #[strum(serialize="@=")]
    ConcatenateSpaceAssign,
    #[strum(serialize="$")]
    Concatenate,
    #[strum(serialize="@")]
    ConcatenateSpace,
    #[strum(serialize="+")]
    Add,
    #[strum(serialize="-")]
    Subtract,
    #[strum(serialize="=")]
    Assign,
    #[strum(serialize="dot")]
    Dot,
    #[strum(serialize="cross")]
    Cross,
}

impl DyadicVerb {
    pub fn is_assignment(&self) -> bool {
        match self {
            DyadicVerb::AddAssign |
            DyadicVerb::SubtractAssign |
            DyadicVerb::MultiplyAssign |
            DyadicVerb::DivideAssign |
            DyadicVerb::ConcatenateAssign |
            DyadicVerb::ConcatenateSpaceAssign |
            DyadicVerb::Assign => true,
            _ => false,
        }
    }
}

pub struct Program {
    pub statements: Vec<AstNode<ProgramStatement>>,
}

#[derive(Debug)]
pub struct CppText {
    pub text: String,
}

#[derive(Debug)]
pub enum ProgramStatement {
    Empty,
    ClassDeclaration(AstNode<ClassDeclaration>),
    CompilerDirective(CompilerDirective),
    ConstDeclaration(AstNode<ConstDeclaration>),
    VarDeclaration(Rc<AstNode<VarDeclaration>>),
    EnumDeclaration(AstNode<EnumDeclaration>),
    StructDeclaration(AstNode<StructDeclaration>),
    FunctionDeclaration(AstNode<FunctionDeclaration>),
    ReplicationBlock(AstNode<ReplicationBlock>),
    StateDeclaration(AstNode<StateDeclaration>),
    DefaultProperties(AstNode<DefaultProperties>),
    CppText(AstNode<CppText>),
}

#[derive(Debug)]
pub struct CompilerDirective {
    pub command: String,
}

#[derive(Debug, PartialEq)]
pub enum VarSize {
    IntegerLiteral(AstNode<NumericLiteral>),
    Identifier(AstNode<Identifier>),
}

#[derive(Debug, PartialEq)]
pub struct VarName {
    pub identifier: AstNode<Identifier>,
    pub size: Option<AstNode<VarSize>>,
}

#[derive(Debug, PartialEq)]
pub struct StructVarDeclaration {
    pub is_editable: bool,
    pub modifiers: Vec<StructVarModifier>,
    pub type_: Type,
    pub names: Vec<AstNode<VarName>>,
}

#[derive(Debug, PartialEq)]
pub struct StructDeclaration {
    pub name: AstNode<Identifier>,
    pub parent: Option<AstNode<Identifier>>,
    pub modifiers: Vec<AstNode<StructModifier>>,
    pub members: Vec<AstNode<StructVarDeclaration>>,
    pub cpp: Option<String>,
}

#[derive(Debug)]
pub struct VarDeclaration {
    pub category: Option<String>,
    pub modifiers: Vec<AstNode<VarModifier>>,
    pub type_: Type,
    pub names: Vec<AstNode<VarName>>,
}

#[derive(Debug, PartialEq)]
pub enum PodType {
    Byte,
    Int,
    Float,
    String,
    Name,
    Bool,
}

impl ToString for PodType {
    fn to_string(&self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Pod(PodType),
    Array(Box<Type>),
    Class(AstNode<Identifier>),
    Struct(AstNode<StructDeclaration>),
    Enum(AstNode<EnumDeclaration>),
    Identifier(Identifier),
}

impl From<Identifier> for Type {
    fn from(identifier: Identifier) -> Self {
        if identifier.string.eq_ignore_ascii_case("int") {
            Type::Pod(PodType::Int)
        } else if identifier.string.eq_ignore_ascii_case("byte") {
            Type::Pod(PodType::Byte)
        } else if identifier.string.eq_ignore_ascii_case("float") {
            Type::Pod(PodType::Float)
        } else if identifier.string.eq_ignore_ascii_case("string") {
            Type::Pod(PodType::String)
        }else if identifier.string.eq_ignore_ascii_case("name") {
            Type::Pod(PodType::Name)
        } else if identifier.string.eq_ignore_ascii_case("bool") {
            Type::Pod(PodType::Bool)
        } else {
            // TODO: this is technically wrong since it's possible for the identifier string to have invalid characters
            Type::Identifier(identifier)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionList {
    pub expressions: Vec<Option<Box<AstNode<Expression>>>>,
}

#[derive(Debug, PartialEq)]
pub enum FStringElement {
    Literal(String),
    Expression(Box<AstNode<Expression>>),
}

#[derive(Debug, PartialEq)]
pub struct FString {
    pub elements: Vec<AstNode<FStringElement>>
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(AstNode<Identifier>),
    Literal(AstNode<Literal>),
    New { arguments: Option<AstNode<ExpressionList>>, type_: Box<AstNode<Expression>> },
    MonadicPreExpression { operand: Box<AstNode<Expression>>, verb: MonadicVerb },
    MonadicPostExpression { operand: Box<AstNode<Expression>>, verb: MonadicVerb },
    DyadicExpression { lhs: Box<AstNode<Expression>>, verb: DyadicVerb, rhs: Box<AstNode<Expression>> },
    Call { operand: Box<AstNode<Expression>>, arguments: Option<AstNode<ExpressionList>> },
    GlobalCall { name: AstNode<Identifier>, arguments: Option<AstNode<ExpressionList>> },
    ArrayAccess { operand: Box<AstNode<Expression>>, argument: Box<AstNode<Expression>> },
    DefaultAccess { operand: Option<Box<AstNode<Expression>>>, target: AstNode<Identifier> },
    StaticAccess { operand: Option<Box<AstNode<Expression>>>, target: AstNode<Identifier> },
    MemberAccess { operand: Box<AstNode<Expression>>, target: AstNode<Identifier> },
    Cast { type_: Type, operand: Box<AstNode<Expression>> },
    ParentheticalExpression(Box<AstNode<Expression>>),
    FString(AstNode<FString>),
}

#[derive(Debug)]
pub enum CodeStatement {
    Empty,
    Expression(Box<AstNode<Expression>>),
    Return(Option<Box<AstNode<Expression>>>),
    Break,
    Continue,
    Goto(AstNode<Identifier>),
    JumpLabel(AstNode<Identifier>),
    ForEach(Box<ForEach>),
    For(Box<ForStatement>),
    Switch(Box<SwitchStatement>),
    Conditional(Box<ConditionalStatement>),
    While(Box<WhileStatement>),
    DoUntil(Box<DoUntil>),
    CompilerDirective(CompilerDirective),
    ConstDeclaration(AstNode<ConstDeclaration>),
}

#[derive(Debug)]
pub struct ClassDeclaration {
    pub name: AstNode<Identifier>,
    pub parent_class: Option<AstNode<Identifier>>,
    pub modifiers: Vec<AstNode<ClassModifier>>,
    pub within: Option<AstNode<Identifier>>,
}

#[derive(Debug, PartialEq)]
pub enum NumericLiteral {
    Integer(i32),
    Float(f32),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    None,
    Numeric(AstNode<NumericLiteral>),
    Boolean(bool),
    String(String),
    Name(String),
    Rotator([AstNode<NumericLiteral>; 3]),
    Vector([AstNode<NumericLiteral>; 3]),
    Object { type_: AstNode<Identifier>, reference: String },
}

#[derive(Eq, Clone)]
pub struct Identifier {
    pub string: String,
}

impl Deref for Identifier {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.string.to_lowercase().hash(state)
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.string.as_str())
    }
}

impl Identifier {
    pub fn new(s: &str) -> Identifier {
        Identifier { string: s.to_string() }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.string.eq_ignore_ascii_case(other.string.as_str())
    }
}

#[derive(Debug)]
pub struct FunctionArgument {
    pub modifiers: Vec<FunctionArgumentModifier>,
    pub type_: Type,
    pub name: AstNode<VarName>,
}

#[derive(Debug)]
pub struct FunctionModifier {
    pub type_: FunctionModifierType,
    pub arguments: Vec<AstNode<NumericLiteral>>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub types: Vec<FunctionType>,
    pub modifiers: Vec<FunctionModifier>,
    pub return_type: Option<Type>,
    pub arguments: Vec<AstNode<FunctionArgument>>,
    pub name: AstNode<Identifier>,
    pub body: Option<AstNode<FunctionBody>>,
}

#[derive(Debug)]
pub struct ClassModifier {
    pub type_: AstNode<ClassModifierType>,
    pub arguments: Option<AstNode<ExpressionList>>,
}

#[derive(Debug)]
pub struct LocalDeclaration {
    pub type_: Type,
    pub names: Vec<AstNode<VarName>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDeclaration {
    pub name: AstNode<Identifier>,
    pub values: Vec<AstNode<Identifier>>,
}

#[derive(Debug)]
pub enum StateStatement {
    ConstDeclaration(AstNode<ConstDeclaration>),
    FunctionDeclaration(AstNode<FunctionDeclaration>),
}

#[derive(Debug)]
pub struct StateLabel {
    pub label: AstNode<Identifier>,
    pub statements: Vec<AstNode<CodeStatement>>,
}

#[derive(Debug)]
pub struct StateDeclaration {
    pub is_editable: bool,
    pub modifiers: Vec<StateModifier>,
    pub name: AstNode<Identifier>,
    pub parent: Option<AstNode<Identifier>>,
    pub ignores: Vec<AstNode<Identifier>>,
    pub statements: Vec<StateStatement>,
    pub labels: Vec<StateLabel>,
}

#[derive(Debug)]
pub struct ReplicationStatement {
    pub reliability: ReplicationReliability,
    pub condition: Box<AstNode<Expression>>,
    pub variables: Vec<AstNode<Identifier>>,
}

#[derive(Debug)]
pub struct ReplicationBlock {
    pub statements: Vec<ReplicationStatement>,
}

#[derive(Debug)]
pub enum FunctionBodyStatement {
    ConstDeclaration(AstNode<ConstDeclaration>),
    LocalDeclaration(AstNode<LocalDeclaration>),
    CodeStatement(AstNode<CodeStatement>),
}

#[derive(Debug)]
pub struct FunctionBody {
    pub statements: Vec<FunctionBodyStatement>,
}

#[derive(Debug)]
pub struct ConstDeclaration {
    pub name: AstNode<Identifier>,
    pub value: AstNode<Literal>,
}

#[derive(Debug)]
pub struct CodeBlock {
    pub statements: Vec<AstNode<CodeStatement>>,
}

#[derive(Debug)]
pub enum CodeStatementOrBlock {
    CodeBlock(AstNode<CodeBlock>),
    CodeStatement(AstNode<CodeStatement>),
}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Option<Box<AstNode<Expression>>>,
    pub predicate: Option<Box<AstNode<Expression>>>,
    pub post: Option<Box<AstNode<Expression>>>,
    pub body: AstNode<CodeStatementOrBlock>,
}

#[derive(Debug)]
pub struct DoUntil {
    pub body: AstNode<CodeStatementOrBlock>,
    pub predicate: Option<Box<AstNode<Expression>>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub predicate: Box<AstNode<Expression>>,
    pub body: AstNode<CodeStatementOrBlock>,
}

#[derive(Debug)]
pub struct ForEach {
    pub predicate: Box<AstNode<Expression>>,
    pub body: AstNode<CodeStatementOrBlock>,
}

#[derive(Debug)]
pub enum SwitchCaseType {
    Expression(Box<AstNode<Expression>>),
    Default,
}

#[derive(Debug)]
pub struct SwitchCase {
    pub type_: SwitchCaseType,
    pub statements: Vec<AstNode<CodeStatement>>,
}

#[derive(Debug)]
pub struct SwitchStatement {
    pub predicate: Box<AstNode<Expression>>,
    pub cases: Vec<SwitchCase>,
}

#[derive(Debug)]
pub struct ConditionalStatement {
    pub if_statement: IfStatement,
    pub elif_statements: Vec<ElifStatement>,
    pub else_statement: Option<ElseStatement>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub predicate: Box<AstNode<Expression>>,
    pub body: Option<AstNode<CodeStatementOrBlock>>,
}

#[derive(Debug)]
pub struct ElifStatement {
    pub predicate: Box<AstNode<Expression>>,
    pub body: AstNode<CodeStatementOrBlock>,
}

#[derive(Debug)]
pub struct ElseStatement {
    pub body: AstNode<CodeStatementOrBlock>,
}

#[derive(Debug)]
pub struct DefaultProperties {
    pub statements: Vec<AstNode<DefaultPropertiesStatement>>,
}

#[derive(Debug)]
pub enum DefaultPropertiesStatement {
    Assignment(AstNode<DefaultPropertiesAssignment>),
    Object(Rc<AstNode<DefaultPropertiesObject>>),
}

#[derive(Debug)]
pub struct DefaultPropertiesAssignment {
    pub target: AstNode<DefaultPropertiesTarget>,
    pub value: Option<AstNode<DefaultPropertiesValue>>,
}

impl DefaultPropertiesAssignment {
    pub fn is_array_assignment(&self) -> bool {
        self.target.index.is_some()
    }
}

#[derive(Debug)]
pub enum DefaultPropertiesValue {
    Literal(AstNode<Literal>),
    Identifier(AstNode<Identifier>),
    Struct(AstNode<DefaultPropertiesStruct>),
    Array(AstNode<DefaultPropertiesArray>),
}

#[derive(Debug)]
pub struct DefaultPropertiesStruct {
    pub assignments: Vec<AstNode<DefaultPropertiesAssignment>>,
}

#[derive(Debug)]
pub enum DefaultPropertiesArrayIndex {
    Identifier(AstNode<Identifier>),
    IntegerLiteral(AstNode<NumericLiteral>),
}

#[derive(Debug)]
pub struct DefaultPropertiesTarget {
    pub target: AstNode<Identifier>,
    pub index: Option<AstNode<DefaultPropertiesArrayIndex>>,
}

#[derive(Debug)]
pub struct DefaultPropertiesArray {
    pub elements: Vec<Option<AstNode<DefaultPropertiesValue>>>,
}

#[derive(Debug)]
pub struct DefaultPropertiesObject {
    pub class: AstNode<Identifier>,
    pub statements: Vec<AstNode<DefaultPropertiesStatement>>,
}
