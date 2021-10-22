pub enum AstNode {
    Program {
        class_declaration: Box<AstNode>,
        statements: Vec<Box<AstNode>>
    },
    IntegerLiteral(i32),
    FloatLiteral(f32),
    BooleanLiteral(bool),
    StringLiteral(String),
    NameLiteral(String),
    ClassModifier {
        type_: String,
        arguments: Vec<Box<AstNode>>
    },
    VectorLiteral {
        x: Box<AstNode>,
        y: Box<AstNode>,
        z: Box<AstNode>
    },
    FunctionModifier {
        type_: String,
        arguments: Vec<Box<AstNode>>
    },
    FunctionArgument {
        modifiers: Vec<String>,
        type_: Box<AstNode>,
        name: Box<AstNode>
    },
    FunctionDelaration {
        type_: Box<AstNode>,
        modifiers: Vec<Box<AstNode>>,
        return_type: Option<Box<AstNode>>,
        arguments: Vec<Box<AstNode>>,
        name: String,
        body: Option<Box<AstNode>>
    },
    OperatorType {
        type_: String,
        arguments: Vec<Box<AstNode>>
    },
    OperatorDeclaration {
        modifiers: Vec<Box<AstNode>>,
        type_: Box<AstNode>,
        return_type: Option<Box<AstNode>>,
        arguments: Vec<Box<AstNode>>,
        name: String,
        body: Option<Box<AstNode>>
    },
    ReplicationStatement {
        is_reliable: bool,
        condition: Box<AstNode>,
        variables: Vec<String>
    },
    ReplicationBlock {
        statements: Vec<Box<AstNode>>
    },
    CompilerDirective {
        contents: String
    },
    RotatorLiteral {
        pitch: Box<AstNode>,
        yaw: Box<AstNode>,
        roll: Box<AstNode>
    },
    None,
    ObjectLiteral {
        type_: String,
        reference: String
    },
    ClassDeclaration {
        name: String,
        parent_class: Option<String>,
        modifiers: Vec<Box<AstNode>>,
        within: Option<String>
    },
    ConstDeclaration {
        name: String,
        value: Box<AstNode>
    },
    EnumDeclaration {
        name: String,
        values: Vec<String>
    },
    VarName {
        name: String,
        size: Option<Box<AstNode>>
    },
    StructVarDeclaration {
        editable: bool,
        modifiers: Vec<String>,
        type_: Box<AstNode>,
        names: Vec<Box<AstNode>>
    },
    StructDeclaration {
        name: String,
        parent: Option<String>,
        modifiers: Vec<String>,
        members: Vec<Box<AstNode>>
    },
    ArrayType(Box<AstNode>),
    VarDeclaration {
        category: Option<String>,
        modifiers: Vec<String>,
        type_: Box<AstNode>,
        names: Vec<Box<AstNode>>
    },
    StateLabel {
        label: String,
        statements: Vec<Box<AstNode>>
    },
    StateDeclaration {
        is_editable: bool,
        modifiers: Vec<String>,
        name: String,
        parent: Option<String>,
        ignores: Vec<String>,
        statements: Vec<Box<AstNode>>,
        labels: Vec<Box<AstNode>>
    },
    UnqualifiedIdentifier(String),
    VarSize(Box<AstNode>),
    ClassType(Box<AstNode>),
    Identifier(String),

    // FLOW
    ForStatement {
        init: Option<Box<AstNode>>,
        predicate: Option<Box<AstNode>>,
        post: Option<Box<AstNode>>,
        body: Box<AstNode>
    },
    DoUntilStatement {
        body: Box<AstNode>,
        predicate: Option<Box<AstNode>>
    },
    New {
        arguments: Vec<Option<Box<AstNode>>>,
        type_: Box<AstNode>
    },
    WhileStatement {
        predicate: Box<AstNode>,
        body: Box<AstNode>
    },
    ForEachStatement {
        predicate: Box<AstNode>,
        body: Box<AstNode>
    },
    SwitchCase {
        predicate: Box<AstNode>,
        statements: Vec<Box<AstNode>>
    },
    SwitchDefaultCase {
        statements: Vec<Box<AstNode>>
    },
    SwitchStatement {
        predicate: Box<AstNode>,
        cases: Vec<Box<AstNode>>
    },
    FunctionType {
        type_: String,
        arguments: Option<Vec<Box<AstNode>>>
    },
    FunctionBody {
        locals: Vec<Box<AstNode>>,
        statements: Vec<Box<AstNode>>
    },
    CodeBlock {
        statements: Vec<Box<AstNode>>
    },
    CodeScope {
        statements: Vec<Box<AstNode>>
    },
    ConditionalStatement {
        if_statement: Box<AstNode>,
        elif_statements: Vec<Box<AstNode>>,
        else_statement: Option<Box<AstNode>>
    },
    IfStatement {
        predicate: Box<AstNode>,
        statements: Vec<Box<AstNode>>
    },
    ElifStatement {
        predicate: Box<AstNode>,
        statements: Vec<Box<AstNode>>
    },
    ElseStatement {
        statements: Vec<Box<AstNode>>
    },

    LocalDeclaration {
        type_: Box<AstNode>,
        names: Vec<Box<AstNode>>
    },

    EmptyStatement,

    // todo: do/while loop

    BreakStatement,
    ContinueStatement,
    GotoStatement {
        label: String
    },
    JumpLabel(String),
    ReturnStatement {
        expression: Option<Box<AstNode>>
    },

    // LOGIC
    ParentheticalExpression {
        expression: Box<AstNode>
    },
    Call {
        operand: Box<AstNode>,
        arguments: Vec<Option<Box<AstNode>>>
    },
    GlobalCall {
        name: String,
        arguments: Vec<Option<Box<AstNode>>>
    },
    ArrayAccess {
        operand: Box<AstNode>,
        argument: Box<AstNode>
    },
    DefaultAccess {
        operand: Option<Box<AstNode>>,
        target: String
    },
    StaticAccess {
        operand: Option<Box<AstNode>>,
        target: String
    },
    MemberAccess {
        operand: Box<AstNode>,
        target: String,
    },
    Expression(),
    MonadicPostExpression {
        operator: String,
        target: Box<AstNode>
    },
    MonadicPreExpression {
        operator: String,
        target: Box<AstNode>
    },
    DyadicExpression {
        lhs: Box<AstNode>,
        operator: String,
        rhs: Box<AstNode>
    },
    DefaultProperties {
        lines: Vec<Box<AstNode>>
    },
    Cast {
        type_: Box<AstNode>,
        operand: Box<AstNode>
    },
    CppText(String),
    Unhandled
}

impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            AstNode::Program { class_declaration, statements } => {
                f.debug_struct("Program")
                    .field("class_declaration", class_declaration)
                    .field("statements", statements)
                    .finish()
            }
            AstNode::ClassDeclaration { name, parent_class, modifiers, within } => {
                let mut d = f.debug_struct("ClassDeclaration");
                d.field("name", name);
                if let Some(parent_class) = parent_class {
                    d.field("parent", parent_class);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                if let Some(within) = within {
                    d.field("within", within);
                }
                d.finish()
            }
            AstNode::ConstDeclaration { name, value } => {
                f.debug_struct("ConstDeclaration")
                    .field("name", name)
                    .field("value", value)
                    .finish()
            }
            AstNode::EnumDeclaration { name, values } => {
                f.debug_struct("EnumDeclaration")
                    .field("name", name)
                    .field("values", values)
                    .finish()
            }
            AstNode::VarName { name, size } => {
                if let Some(size) = size {
                    f.debug_struct("VarName")
                        .field("name", name)
                        .field("size", size)
                        .finish()
                } else {
                    f.write_str(name)
                }
            }
            AstNode::ClassModifier { type_, arguments } => {
                if arguments.is_empty() {
                    f.write_str(type_)
                } else {
                    f.debug_struct("ClassModifier")
                        .field("type", type_)
                        .field("arguments", arguments)
                        .finish()
                }
            }
            AstNode::ArrayType(type_) => {
                f.debug_struct("ArrayType")
                    .field("type", type_)
                    .finish()
            }
            AstNode::ClassType(type_) => {
                f.debug_struct("ClassType")
                    .field("type", type_)
                    .finish()
            }
            AstNode::VarDeclaration { names, type_, modifiers, category } => {
                let mut d = f.debug_struct("VarDeclaration");
                d.field("names", names);
                d.field("type", type_);
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                if let Some(category) = category {
                    d.field("category", category);
                }
                d.finish()
            }
            AstNode::StructVarDeclaration { editable, modifiers, type_, names } => {
                let mut d = f.debug_struct("StructVarDeclaration");
                d.field("type", type_);
                d.field("names", names);
                if *editable {
                    d.field("editable", editable);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                d.finish()
            }
            AstNode::StructDeclaration { name, modifiers, parent, members } => {
                let mut d = f.debug_struct("StructDeclaration");
                d.field("name", name);
                if let Some(parent) = parent {
                    d.field("parent", parent);
                }
                if !members.is_empty() {
                    d.field("members", members);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                d.finish()
            }
            AstNode::ReplicationStatement { is_reliable, condition, variables } => {
                let mut d = f.debug_struct("ReplicationStatement");
                d.field("is_reliable", is_reliable);
                d.field("condition", condition);
                if !variables.is_empty() {
                    d.field("variables", variables);
                }
                d.finish()
            }
            AstNode::ReplicationBlock { statements } => {
                let mut d = f.debug_struct("ReplicationBlock");
                d.field("statements", statements);
                d.finish()
            }
            AstNode::BooleanLiteral(value) => {
                return f.write_str(value.to_string().as_str())
            }
            AstNode::IntegerLiteral(value) => {
                return f.write_str(value.to_string().as_str())
            }
            AstNode::FloatLiteral(value) => {
                return f.write_str(value.to_string().as_str())
            }
            AstNode::ObjectLiteral { type_, reference } => {
                return f.write_fmt(format_args!("{}'{}'", type_, reference));
            }
            AstNode::CompilerDirective { contents } => {
                f.debug_struct("CompilerDirective")
                    .field("contents", contents)
                    .finish()
            }
            AstNode::NameLiteral(name) => {
                return f.write_fmt(format_args!("'{}'", name))
            }
            AstNode::StringLiteral(string) => {
                return f.write_fmt(format_args!("\"{}\"", string))
            }
            AstNode::UnqualifiedIdentifier(value) => {
                return f.write_str(value.as_str())
            }
            AstNode::Identifier(value) => {
                return f.write_str(value.as_str())
            }
            AstNode::StateLabel { label, statements } => {
                let mut d = f.debug_struct("StateLabel");
                d.field("label", label);
                if !statements.is_empty() {
                    d.field("statements", statements);
                }
                d.finish()
            }
            AstNode::StateDeclaration { is_editable, modifiers, name, parent, ignores, statements, labels } => {
                let mut d = f.debug_struct("StateDeclaration");
                if *is_editable {
                    d.field("is_editable", is_editable);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                if let Some(parent) = parent {
                    d.field("parent", parent);
                }
                d.field("name", name);
                if !ignores.is_empty() {
                    d.field("ignores", ignores);
                }
                if !statements.is_empty() {
                    d.field("statements", statements);
                }
                if !labels.is_empty() {
                    d.field("labels", labels);
                }
                d.finish()
            }
            AstNode::FunctionModifier { type_, arguments } => {
                if arguments.is_empty() {
                    return f.write_str(type_)
                } else {
                    let mut d = f.debug_struct("FunctionModifier");
                    d.field("type", type_);
                    d.field("arguments", arguments);
                    d.finish()
                }
            }
            AstNode::FunctionArgument { modifiers, type_, name } => {
                let mut d = f.debug_struct("FunctionArgument");
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                d.field("type", type_);
                d.field("name", name);
                d.finish()
            }
            AstNode::FunctionType { type_, arguments } => {
                let mut d = f.debug_struct("FunctionType");
                d.field("type", type_);
                match arguments {
                    Some(_) => {
                        d.field("arguments", arguments);
                    }
                    None => {}
                }
                d.finish()
            }
            AstNode::FunctionDelaration { type_, modifiers, return_type, name, arguments, body } => {
                let mut d = f.debug_struct("FunctionDeclaration");
                d.field("type", type_);
                d.field("name", name);
                if let Some(return_type) = return_type {
                    d.field("return_type", return_type);
                }
                if !arguments.is_empty() {
                    d.field("arguments", arguments);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                if let Some(body) = body {
                    d.field("body", body);
                }
                d.finish()
            }
            AstNode::OperatorType { type_, arguments } => {
                if arguments.is_empty() {
                    f.write_str(type_)
                } else {
                    f.debug_struct("OperatorType")
                        .field("type", type_)
                        .field("arguments", arguments)
                        .finish()
                }
            }
            AstNode::OperatorDeclaration { type_, modifiers, return_type, name, arguments, body } => {
                let mut d = f.debug_struct("OperatorDeclaration");
                d.field("type", type_);
                d.field("name", name);
                if let Some(return_type) = return_type {
                    d.field("return_type", return_type);
                }
                if !arguments.is_empty() {
                    d.field("arguments", arguments);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                if let Some(body) = body {
                    d.field("body", body);
                }
                d.finish()
            }
            AstNode::Call { operand, arguments } => {
                let mut d = f.debug_struct("Call");
                d.field("operand", operand);
                if !arguments.is_empty() {
                    d.field("arguments", arguments);
                }
                d.finish()
            }
            AstNode::MemberAccess { operand, target } => {
                f.debug_struct("MemberAccess")
                    .field("operand", operand)
                    .field("target", target)
                    .finish()
            }
            AstNode::DefaultAccess { operand, target } => {
                f.debug_struct("DefaultAccess")
                    .field("operand", operand)
                    .field("target", target)
                    .finish()
            }
            AstNode::StaticAccess { operand, target } => {
                f.debug_struct("StaticAccess")
                    .field("operand", operand)
                    .field("target", target)
                    .finish()
            }
            AstNode::GlobalCall { name, arguments } => {
                f.debug_struct("GlobalCall")
                    .field("name", name)
                    .field("arguments", arguments)
                    .finish()
            }
            AstNode::ArrayAccess { operand, argument } => {
                f.debug_struct("ArrayAccess")
                    .field("operand", operand)
                    .field("argument", argument)
                    .finish()
            }
            AstNode::MonadicPreExpression { operator, target } => {
                f.debug_struct("MonadicPreExpression")
                    .field("operator", operator)
                    .field("target", target)
                    .finish()
            }
            AstNode::MonadicPostExpression { operator, target } => {
                f.debug_struct("MonadicPostExpression")
                    .field("operator", operator)
                    .field("target", target)
                    .finish()
            }
            AstNode::DyadicExpression { lhs, operator, rhs } => {
                f.debug_struct("DyadicExpression")
                    .field("lhs", lhs)
                    .field("operator", operator)
                    .field("rhs", rhs)
                    .finish()
            }
            AstNode::ParentheticalExpression { expression } => {
                f.debug_struct("ParentheticalExpression")
                    .field("expression", expression)
                    .finish()
            }
            AstNode::ConditionalStatement { if_statement, elif_statements, else_statement } => {
                let mut d = f.debug_struct("ConditionalStatement");
                d.field("if_statement", if_statement);
                if !elif_statements.is_empty() {
                    d.field("elif_statements", elif_statements);
                }
                if let Some(else_statement) = else_statement {
                    d.field("else_statement", else_statement);
                }
                d.finish()
            }
            AstNode::CodeScope { statements } => {
                let mut d = f.debug_struct("CodeScope");
                if !statements.is_empty() {
                    d.field("statements", statements);
                }
                d.finish()
            }
            AstNode::IfStatement { predicate, statements } => {
                f.debug_struct("IfStatement")
                    .field("predicate", predicate)
                    .field("statements", statements)
                    .finish()
            }
            AstNode::ElifStatement { predicate, statements } => {
                f.debug_struct("ElifStatement")
                    .field("predicate", predicate)
                    .field("statements", statements)
                    .finish()
            }
            AstNode::ForStatement { init, predicate, post, body } => {
                f.debug_struct("ForStatement")
                    .field("init", init)
                    .field("predicate", predicate)
                    .field("post", post)
                    .field("body", body)
                    .finish()
            }
            AstNode::WhileStatement { predicate, body } => {
                f.debug_struct("WhileStatement")
                    .field("predicate", predicate)
                    .field("body", body)
                    .finish()
            }
            AstNode::DoUntilStatement { body, predicate } => {
                f.debug_struct("DoUntilStatement")
                    .field("body", body)
                    .field("predicate", predicate)
                    .finish()
            }
            AstNode::ForEachStatement { predicate, body } => {
                f.debug_struct("ForEachStatement")
                    .field("predicate", predicate)
                    .field("body", body)
                    .finish()
            }
            AstNode::BreakStatement => {
                f.debug_struct("BreakStatement").finish()
            }
            AstNode::ContinueStatement => {
                f.debug_struct("ContinueStatement").finish()
            }
            AstNode::GotoStatement { label } => {
                f.debug_struct("GotoStatement")
                    .field("label", label)
                    .finish()
            }
            AstNode::JumpLabel(label) => {
                f.debug_struct("JumpLabel")
                    .field("label", label)
                    .finish()
            }
            AstNode::ReturnStatement { expression } => {
                let mut d = f.debug_struct("ReturnStatement");
                if let Some(expression) = expression {
                    d.field("expression", expression);
                }
                d.finish()
            }
            AstNode::SwitchDefaultCase { statements } => {
                f.debug_struct("SwitchDefaultCase")
                    .field("statements", statements)
                    .finish()
            }
            AstNode::SwitchCase { predicate, statements } => {
                f.debug_struct("SwitchCase")
                    .field("predicate", predicate)
                    .field("statements", statements)
                    .finish()
            }
            AstNode::SwitchStatement { predicate, cases } => {
                f.debug_struct("SwitchStatement")
                    .field("predicate", predicate)
                    .field("cases", cases)
                    .finish()
            }
            AstNode::LocalDeclaration { type_, names } => {
                f.debug_struct("LocalDeclaration")
                    .field("type", type_)
                    .field("names", names)
                    .finish()
            }
            AstNode::FunctionBody { locals, statements } => {
                let mut d = f.debug_struct("FunctionBody");
                if !locals.is_empty() {
                    d.field("locals", locals);
                }
                if !statements.is_empty() {
                    d.field("statements", statements);
                }
                d.finish()
            }
            AstNode::New { arguments, type_ } => {
                let mut d = f.debug_struct("New");
                d.field("type", type_);
                if !arguments.is_empty() {
                    d.field("arguments", arguments);
                }
                d.finish()
            }
            AstNode::VectorLiteral { x, y, z } => {
                f.debug_struct("VectorLiteral")
                    .field("x", x)
                    .field("y", y)
                    .field("z", z)
                    .finish()
            }
            AstNode::RotatorLiteral { pitch, yaw, roll } => {
                f.debug_struct("RotatorLiteral")
                    .field("pitch", pitch)
                    .field("yaw", yaw)
                    .field("roll", roll)
                    .finish()
            }
            AstNode::DefaultProperties { lines } => {
                f.debug_struct("DefaultProperties")
                    .field("lines", lines)
                    .finish()
            }
            AstNode::Cast { type_, operand } => {
                f.debug_struct("Cast")
                    .field("type", type_)
                    .field("operand", operand)
                    .finish()
            }
            AstNode::CppText(body) => {
                f.debug_struct("CppText")
                    .field("body", body)
                    .finish()
            }
            _ => { f.debug_struct("Unknown").finish() }
        }
    }
}
