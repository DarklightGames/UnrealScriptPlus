use strum_macros::{EnumString, Display};
use std::fmt::{Debug, Formatter};

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
pub enum StructVarModifier {
    Const,
    Private,
    Protected,
    Public,
    Config,
    GlobalConfig,
    Localized,
    Transient,
    EdFindable,
    EditConst,
    EditConstArray,
    EditInline,
    EditInlineUse,
    EditInlineNotify,
    Export,
    NoExport
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

#[derive(Debug)]
pub struct FunctionType {
    pub type_: FunctionTypeType,
    pub arguments: Option<Vec<NumericLiteral>>
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
    VectorCross
}

pub struct Program {
    pub statements: Vec<ProgramStatement>
}

#[derive(Debug)]
pub enum ProgramStatement {
    Empty,
    ClassDeclaration(ClassDeclaration),
    CompilerDirective(CompilerDirective),
    ConstDeclaration(ConstDeclaration),
    VarDeclaration(VarDeclaration),
    EnumDeclaration(EnumDeclaration),
    StructDeclaration(StructDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ReplicationBlock(ReplicationBlock),
    StateDeclaration(StateDeclaration),
    DefaultProperties(DefaultProperties),
    CppText(String),
}

#[derive(Debug)]
pub struct CompilerDirective {
    pub command: String
}

// pub enum Constant {
//     Literal(Literal),
//     Identifier(Identifier)
// }

#[derive(Debug)]
pub enum VarSize {
    IntegerLiteral(NumericLiteral),
    Identifier(Identifier)
}

#[derive(Debug)]
pub struct VarName {
    pub identifier: Identifier,
    pub size: Option<VarSize>
}

#[derive(Debug)]
pub struct StructVarDeclaration {
    pub is_editable: bool,
    pub modifiers: Vec<StructVarModifier>,
    pub type_: Type,
    pub names: Vec<VarName>
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub parent: Option<Identifier>,
    pub modifiers: Vec<StructModifier>,
    pub members: Vec<StructVarDeclaration>,
    pub cpp: Option<String>
}

#[derive(Debug)]
pub struct VarDeclaration {
    pub category: Option<String>,
    pub modifiers: Vec<VarModifier>,
    pub type_: Type,
    pub names: Vec<VarName>
}

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    String,
    Name,
    Bool,
    Array(Box<Type>),
    Class(Identifier),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
    Identifier(Identifier)
}

impl From<Identifier> for Type {
    fn from(identifier: Identifier) -> Self {
        if identifier.string.eq_ignore_ascii_case("int") {
            Type::Int
        } else if identifier.string.eq_ignore_ascii_case("float") {
            Type::Float
        } else if identifier.string.eq_ignore_ascii_case("string") {
            Type::String
        }else if identifier.string.eq_ignore_ascii_case("name") {
            Type::Name
        } else if identifier.string.eq_ignore_ascii_case("bool") {
            Type::Bool
        } else {
            Type::Identifier(identifier)
        }
    }
}

type ExpressionList = Vec<Option<Box<Expression>>>;

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    New { arguments: Option<ExpressionList>, type_: Box<Expression> },
    MonadicPreExpression { operand: Box<Expression>, verb: MonadicVerb },
    MonadicPostExpression { operand: Box<Expression>, verb: MonadicVerb },
    DyadicExpression { lhs: Box<Expression>, verb: DyadicVerb, rhs: Box<Expression> },
    Call { operand: Box<Expression>, arguments: ExpressionList },
    GlobalCall { name: Identifier, arguments: ExpressionList },
    ArrayAccess { operand: Box<Expression>, argument: Box<Expression> },
    DefaultAccess { operand: Option<Box<Expression>>, target: Identifier },
    StaticAccess { operand: Option<Box<Expression>>, target: Identifier },
    MemberAccess { operand: Box<Expression>, target: Identifier },
    Cast { type_: Type, operand: Box<Expression> },
    ParentheticalExpression(Box<Expression>)
}

#[derive(Debug)]
pub enum CodeStatement {
    Empty,
    Expression(Box<Expression>),
    Return(Option<Box<Expression>>),
    Break,
    Continue,
    Goto(Identifier),
    JumpLabel(Identifier),
    ForEach(Box<ForEach>),
    For(Box<ForStatement>),
    Switch(Box<SwitchStatement>),
    Conditional(Box<ConditionalStatement>),
    While(Box<WhileStatement>),
    DoUntil(Box<DoUntil>),
    CompilerDirective(CompilerDirective),
    ConstDeclaration(ConstDeclaration)
}

#[derive(Debug)]
pub struct ClassDeclaration {
    pub name: Identifier,
    pub parent_class: Option<Identifier>,
    pub modifiers: Vec<ClassModifier>,
    pub within: Option<Identifier>
}

#[derive(Debug, PartialEq)]
pub enum NumericLiteral {
    Integer(i32),
    Float(f32)
}

#[derive(Debug)]
pub enum Literal {
    Numeric(NumericLiteral),
    Boolean(bool),
    String(String),
    Name(String),
    Rotator { pitch: NumericLiteral, yaw: NumericLiteral, roll: NumericLiteral },
    Vector { x: NumericLiteral, y: NumericLiteral, z: NumericLiteral },
    Object { type_: Identifier, reference: String }
}

pub struct Identifier {
    pub string: String
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
    pub name: VarName
}

#[derive(Debug)]
pub struct FunctionModifier {
    pub type_: FunctionModifierType,
    pub arguments: Vec<NumericLiteral>
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub types: Vec<FunctionType>,
    pub modifiers: Vec<FunctionModifier>,
    pub return_type: Option<Type>,
    pub arguments: Vec<FunctionArgument>,
    pub name: Identifier,
    pub body: Option<FunctionBody>
}

#[derive(Debug)]
pub struct ClassModifier {
    pub type_: ClassModifierType,
    pub arguments: Option<ExpressionList>
}

#[derive(Debug)]
pub struct LocalDeclaration {
    pub type_: Type,
    pub names: Vec<VarName>
}

#[derive(Debug)]
pub struct EnumDeclaration {
    pub name: Identifier,
    pub values: Vec<Identifier>
}

#[derive(Debug)]
pub enum StateStatement {
    ConstDeclaration(ConstDeclaration),
    FunctionDeclaration(FunctionDeclaration)
}

#[derive(Debug)]
pub struct StateLabel {
    pub label: Identifier,
    pub statements: Vec<CodeStatement>
}

#[derive(Debug)]
pub struct StateDeclaration {
    pub is_editable: bool,
    pub modifiers: Vec<StateModifier>,
    pub name: Identifier,
    pub parent: Option<Identifier>,
    pub ignores: Vec<Identifier>,
    pub statements: Vec<StateStatement>,
    pub labels: Vec<StateLabel>
}

#[derive(Debug)]
pub struct ReplicationStatement {
    pub reliability: ReplicationReliability,
    pub condition: Box<Expression>,
    pub variables: Vec<Identifier>
}

#[derive(Debug)]
pub struct ReplicationBlock {
    pub statements: Vec<ReplicationStatement>
}

#[derive(Debug)]
pub enum FunctionBodyStatement {
    ConstDeclaration(ConstDeclaration),
    LocalDeclaration(LocalDeclaration),
    CodeStatement(CodeStatement)
}

#[derive(Debug)]
pub struct FunctionBody {
    pub statements: Vec<FunctionBodyStatement>
}

#[derive(Debug)]
pub struct ConstDeclaration {
    pub name: Identifier,
    pub value: Literal
}

#[derive(Debug)]
pub struct CodeBlock {
    pub statements: Vec<CodeStatement>
}

#[derive(Debug)]
pub enum CodeStatementOrBlock {
    CodeBlock(CodeBlock),
    CodeStatement(CodeStatement)
}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Option<Box<Expression>>,
    pub  predicate: Option<Box<Expression>>,
    pub post: Option<Box<Expression>>,
    pub body: CodeStatementOrBlock
}

#[derive(Debug)]
pub struct DoUntil {
    pub body: CodeStatementOrBlock,
    pub predicate: Option<Box<Expression>>
}

#[derive(Debug)]
pub struct WhileStatement {
    pub predicate: Box<Expression>,
    pub body: CodeStatementOrBlock
}

#[derive(Debug)]
pub struct ForEach {
    pub predicate: Box<Expression>,
    pub body: CodeStatementOrBlock
}

#[derive(Debug)]
pub enum SwitchCaseType {
    Expression(Box<Expression>),
    Default
}

#[derive(Debug)]
pub struct SwitchCase {
    pub type_: SwitchCaseType,
    pub statements: Vec<CodeStatement>
}

#[derive(Debug)]
pub struct SwitchStatement {
    pub predicate: Box<Expression>,
    pub cases: Vec<SwitchCase>
}

#[derive(Debug)]
pub struct ConditionalStatement {
    pub if_statement: IfStatement,
    pub elif_statements: Vec<ElifStatement>,
    pub else_statement: Option<ElseStatement>
}

#[derive(Debug)]
pub struct IfStatement {
    pub predicate: Box<Expression>,
    pub body: Option<CodeStatementOrBlock>
}

#[derive(Debug)]
pub struct ElifStatement {
    pub predicate: Box<Expression>,
    pub body: CodeStatementOrBlock
}

#[derive(Debug)]
pub struct ElseStatement {
    pub body: CodeStatementOrBlock
}

#[derive(Debug)]
pub struct DefaultProperties {
    pub statements: Vec<DefaultPropertiesStatement>
}

#[derive(Debug)]
pub enum DefaultPropertiesStatement {
    Assignment(DefaultPropertiesAssignment),
    Object(DefaultPropertiesObject)
}

#[derive(Debug)]
pub struct DefaultPropertiesAssignment {
    pub target: DefaultPropertiesTarget,
    pub value: Option<DefaultPropertiesValue>
}

#[derive(Debug)]
pub enum DefaultPropertiesValue {
    Literal(Literal),
    Identifier(Identifier),
    Struct(DefaultPropertiesStruct),
    Array(DefaultPropertiesArray)
}

#[derive(Debug)]
pub struct DefaultPropertiesStruct {
    pub assignments: Vec<DefaultPropertiesAssignment>
}

#[derive(Debug)]
pub enum DefaultPropertiesArrayIndex {
    Identifier(Identifier),
    IntegerLiteral(NumericLiteral)
}

#[derive(Debug)]
pub struct DefaultPropertiesTarget {
    pub target: Identifier,
    pub index: Option<DefaultPropertiesArrayIndex>
}

#[derive(Debug)]
pub struct DefaultPropertiesArray {
    pub elements: Vec<Option<DefaultPropertiesValue>>
}

#[derive(Debug)]
pub struct DefaultPropertiesObject {
    pub class: Identifier,
    pub statements: Vec<DefaultPropertiesStatement>
}
