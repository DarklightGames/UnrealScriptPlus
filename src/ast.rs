use strum_macros::{EnumString, Display};

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum ClassModifierType {
    Abstract,
    CacheExempt,
    Config,
    DependsOn,
    Guid,
    Instanced,
    ParseConfig,
    PerObjectConfig,
    SafeReplace,
    Transient,
    CollapseCategories,
    DontCollapseCategories,
    EditInlineNew,
    NotEditInlineNew,
    HideCategories,
    ShowCategories,
    HideDropDown,
    Placeable,
    NotPlaceable,
    ExportStructs,
    Intrinsic,
    NativeReplication,
    Native,
    NoExport
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum FunctionModifierType {
    Native,
    Private,
    Protected,
    Public,
    Static,
    Final,
    Exec,
    Simulated,
    Singular,
    Intrinsic,
    Iterator,
    Latent
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum FunctionArgumentModifier {
    Coerce,
    Optional,
    Out,
    Skip
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum StructModifier {
    Long,
    Transient,
    Export,
    Init,
    Native
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum StateModifier {
    Auto,
    Simulated
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum VarModifier {
    Const,
    Deprecated,
    Private,
    Protected,
    Public,
    Automated,
    Config,
    GlobalConfig,
    Input,
    Localized,
    Transient,
    Travel,
    EdFindable,
    EditConstArray,
    EditConst,
    EditInlineNotify,
    EditInlineUse,
    EditInline,
    Cache,
    Export,
    Native,
    NoExport
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum ReplicationReliability {
    Reliable,
    Unreliable
}

#[derive(Debug, PartialEq, Display, EnumString)]
#[strum(serialize_all="lowercase")]
pub enum FunctionTypeType {
    Function,
    Event,
    Delegate,
    PreOperator,
    PostOperator,
    Operator
}

pub struct FunctionType {
    type_: FunctionTypeType,
    arguments: Option<Vec<Literal>>
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
    Not
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
    VectorDot,
    #[strum(serialize="cross")]
    VectorCross,
    #[strum(disabled)]
    Custom(String),
}

pub struct Program {
    class_declaration: ClassDeclaration,
    statements: Vec<ProgramStatement>
}

pub enum ProgramStatement {
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    StateDeclaration(StateDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    CompilerDirective(String),
    CppText(String),
    ReplicationBlock { statements: Vec<ReplicationStatement> }
}

pub struct VarName {
    identifier: Identifier,
    size: Option<Literal>
}

pub struct StructVarDeclaration {
    is_editable: bool,
    modifiers: Vec<StructVarModifier>,
    type_: Type,
    names: Vec<VarName>
}

pub struct StructDeclaration {
    name: Identifier,
    parent: Option<Identifier>,
    modifiers: Vec<StructModifier>,
    members: Vec<StructVarDeclaration>,
    cpp: Option<String>
}

pub struct VarDeclaration {
    category: Option<String>,
    modifiers: Vec<VarModifier>,
    type_: Type,
    names: Vec<VarName>
}

pub enum Type {
    Int,
    Float,
    String,
    Name,
    Bool,
    Array(Type),
    Class(Identifier),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
    Identifier(Identifier)
}

pub enum Expression {
    Identifier(Identifier),
    Conditional,
    Literal(Literal),
    New { arguments: Vec<Option<Expression>>, type_: Expression },
    MonadicPreExpression { operand: Expression, verb: MonadicVerb },
    MonadicPostExpression { operand: Expression, verb: MonadicVerb },
    DyadicExpression { lhs: Expression, verb: DyadicVerb, rhs: Expression },
    Call { operand: Expression, arguments: Vec<Option<Expression>> },
    GlobalCall { name: Identifier, arguments: Vec<Option<Expression>> },
    ArrayAccess { operand: Expression, argument: Expression },
    DefaultAccess { operand: Option<Expression>, target: Identifier },
    StaticAccess { operand: Option<Expression>, target: Identifier },
    MemberAccess { operand: Expression, target: Identifier },
    Cast { type_: Literal, operand: Expression },
    ParentheticalExpression(Expression)
}

pub enum CodeStatement {
    Empty,
    Scope(Vec<CodeStatement>),
    Expression(Expression),
    Return(Option<Expression>),
    Break,
    Continue,
    Goto(String),
    JumpLabel(Identifier),
    ForEach(ForEach),
    For(ForStatement),
    Switch(SwitchStatement),
    Conditional(ConditionalStatement)
}

pub struct ClassDeclaration {
    name: Identifier,
    parent_class: Option<Identifier>,
    modifiers: Vec<ClassModifier>,
    within: Option<Identifier>
}

#[derive(PartialEq)]
pub enum NumericLiteral {
    Integer(i32),
    Float(f32)
}

#[derive(PartialEq)]
pub enum Literal {
    Numeric(NumericLiteral),
    Boolean(bool),
    String(String),
    Name(String),
    Rotator { pitch: NumericLiteral, yaw: NumericLiteral, roll: NumericLiteral },
    Vector { x: NumericLiteral, y: NumericLiteral, z: NumericLiteral },
    Object { type_: Type, reference: String }
}

pub struct Identifier {
    string: String
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.string.eq_ignore_ascii_case(other.string.as_str())
    }
}

pub struct FunctionArgument {
    pub modifiers: Vec<FunctionArgumentModifier>,
    pub type_: Type,
    pub name: Identifier
}

pub struct FunctionModifier {
    type_: FunctionModifierType,
    arguments: Vec<NumericLiteral>
}

pub struct FunctionDeclaration {
    types: Vec<Type>,
    modifiers: Vec<FunctionModifier>,
    return_type: Option<Type>,
    arguments: Vec<FunctionArgument>,
    name: Identifier,
    body: Option<FunctionBody>
}

pub struct ClassModifier {
    type_: ClassModifierType,
    arguments: Vec<Expression>
}

pub struct LocalDeclaration {
    type_: Type,
    names: Vec<VarName>
}

pub struct EnumDeclaration {
    name: Identifier,
    values: Vec<Identifier>
}

pub enum StateStatement {
    FunctionDeclaration(FunctionDeclaration),
    StateLabel(StateLabel)
}

pub struct StateLabel {
    label: String,
    statements: Vec<Statement>
}

pub struct StateDeclaration {
    is_editable: bool,
    modifiers: Vec<StateModifier>,
    name: Identifier,
    parent: Option<Identifier>,
    ignores: Vec<String>,
    statements: Vec<StateStatement>,
    labels: Vec<StateLabel>
}

pub struct ReplicationStatement {
    reliability: ReplicationReliability,
    condition: Expression,
    variables: Vec<Identifier>
}

pub struct FunctionBody {
    locals: Vec<LocalDeclaration>,
    statements: Vec<CodeStatement>
}

pub struct ConstDeclaration {
    name: Identifier,
    value: Literal
}

pub struct CodeBlock {
    statements: Vec<CodeStatement>
}

pub enum CodeBlockOrStatement {
    CodeBlock(CodeBlock),
    CodeStatement(CodeStatement)
}

pub struct ForStatement {
    init: Option<Expression>,
    predicate: Option<Expression>,
    post: Option<Expression>,
    body: CodeBlockOrStatement
}

pub struct DoUntil {
    body: CodeBlockOrStatement,
    predicate: Option<Expression>
}

pub struct WhileStatement {
    predicate: Expression,
    body: CodeBlockOrStatement
}

pub struct ForEach {
    predicate: Expression,
    body: CodeBlockOrStatement
}

pub enum SwitchCaseType {
    Expression(Expression),
    Default
}

pub struct SwitchCase {
    type_: SwitchCaseType,
    statements: Vec<CodeStatement>
}

pub struct SwitchStatement {
    predicate: Expression,
    cases: Vec<SwitchCase>
}

pub struct ConditionalStatement {
    if_statement: IfStatement,
    elif_statements: Vec<ElifStatement>,
    else_statement: Option<ElseStatement>
}

pub struct IfStatement {
    predicate: Expression,
    statements: Vec<CodeStatement>
}

pub struct ElifStatement {
    predicate: Expression,
    statements: Vec<CodeStatement>
}

pub struct ElseStatement {
    statements: Vec<CodeStatement>
}

pub struct DefaultProperties {
    statements: Vec<DefaultPropertiesStatement>
}

pub enum DefaultPropertiesStatement {
    Assignment(DefaultPropertiesAssignment),
    Object(DefaultPropertiesObject)
}

pub struct DefaultPropertiesAssignment {
    target: DefaultPropertiesTarget,
    value: Option<DefaultPropertiesValue>
}

pub struct DefaultPropertiesStruct {
    assignments: Vec<DefaultPropertiesAssignment>
}

pub struct DefaultPropertiesTarget {
    target: Identifier,
    index: Option<Literal>
}

pub struct DefaultPropertiesArray {
    elements: Vec<Option<DefaultPropertiesValue>>
}

pub struct DefaultPropertiesObject {
    class: Identifier,
    statements: Vec<DefaultPropertiesStatement>
}
