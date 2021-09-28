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
        type_: String,  // TODO: enum?
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
    StateLabel, // TODO
    StateDeclaration {
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
    ForLoop {
        body: Box<AstNode>
    },
    SwitchCase {
        predicate: Box<AstNode>,
        body: Option<Box<AstNode>>
    },
    SwitchStatement {
        cases: Vec<Box<AstNode>>
    },
    CodeBlock {
        statements: Vec<Box<AstNode>>
    },
    IfStatement {
        conditional_expression: Box<AstNode>,
        body: Box<AstNode>
    },
    ElifStatement {
        conditional_expression: Box<AstNode>,
        body: Box<AstNode>
    },
    ElseStatement {
        body: Box<AstNode>
    },

    // LOGIC
    Call {
        operand: Box<AstNode>,
        arguments: Vec<Option<Box<AstNode>>>
    },
    ArrayAccess {
        target: Box<AstNode>,
        argument: Box<AstNode>
    },
    DefaultAccess {
        operand: Box<AstNode>,
        target: String
    },
    StaticAccess {
        operand: Box<AstNode>,
        target: String
    },
    MemberAccess {
        operand: Box<AstNode>,
        target: String,
    },
    DyadicExpression {
        lhs: Box<AstNode>,
        operator: String,
        rhs: Box<AstNode>
    },

    Unhandled
}

impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            AstNode::Program { class_declaration, statements} => {
                f.debug_struct("Program")
                    .field("class_declaration", class_declaration)
                    .field("statements", statements)
                    .finish()
            }
            AstNode::ClassDeclaration { name, parent_class, modifiers } => {
                let mut d = f.debug_struct("ClassDeclaration");
                d.field("name", name);
                if let Some(parent_class) = parent_class {
                    d.field("parent", parent_class);
                }
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
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
            AstNode::VarName {name, size} => {
                if let Some(size) = size {
                    f.debug_struct("VarName")
                        .field("name", name)
                        .field("size", size)
                        .finish()
                } else {
                    f.write_str(name)
                }
            }
            AstNode::ClassModifier {type_, arguments} => {
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
            AstNode::VarDeclaration { names, type_,modifiers, category } => {
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
            AstNode::ObjectLiteral {type_, reference} => {
                return f.write_fmt(format_args!("{}'{}'", type_, reference));
            }
            AstNode::CompilerDirective { contents } => {
                f.debug_struct("CompilerDirective")
                    .field("contents", contents)
                    .finish()
            }
            AstNode::NameLiteral(name) => {
                return f.write_str(name)
            }
            AstNode::StringLiteral(string) => {
                return f.write_str(string)
            }
            AstNode::UnqualifiedIdentifier(value) => {
                return f.write_str(value.as_str())
            }
            AstNode::Identifier(value) => {
                return f.write_str(value.as_str())
            }
            AstNode::StateDeclaration { modifiers, name, parent, ignores, statements, labels } => {
                let mut d = f.debug_struct("StateDeclaration");
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
            AstNode::FunctionArgument {modifiers, type_, name } => {
                let mut d = f.debug_struct("FunctionArgument");
                if !modifiers.is_empty() {
                    d.field("modifiers", modifiers);
                }
                d.field("type", type_);
                d.field("name", name);
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
            _ => { f.debug_struct("Unknown").finish() }
        }
    }
}
