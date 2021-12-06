use crate::ast::*;
use std::ops::Deref;

pub struct ScriptFormattingOptions {
    
}

struct ScriptLine {
    indent: usize,
    string: String,
}

pub struct ScriptBuilder {
    options: ScriptFormattingOptions,
    indentation: usize,
    lines: Vec<ScriptLine>,
    buffer: String,
}

impl ToString for ScriptBuilder {
    fn to_string(&self) -> String {
        let mut string = String::new();
        for line in &self.lines {
            if line.indent > 0 {
                string += String::from("    ").repeat(line.indent).as_str();
            }
            string += line.string.as_str();
            string += "\n";
        }
        string
    }
}

impl<T: ToScript> ToScript for AstNode<T> {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        self.data.to_script(builder)
    }
}

impl<T: ToScript> ToScript for Box<T> {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(self.deref());
    }
}

impl ScriptBuilder {
    pub fn new(options: ScriptFormattingOptions) -> ScriptBuilder {
        ScriptBuilder {
            options,
            indentation: 0,
            lines: vec![],
            buffer: String::new(),
        }
    }

    pub fn write_data<D: ToScript>(&mut self, data: &D) -> &mut Self {
        data.to_script(self);
        self
    }

    pub fn write_data_interspersed<D: ToScript>(&mut self, data: &[D], separator: &str) -> &mut Self {
        if !data.is_empty() {
            for (index, datum) in data.iter().enumerate() {
                if index > 0 {
                    self.write(separator);
                }
                datum.to_script(self);
            }
        }
        self
    }

    pub fn space(&mut self) -> &mut Self {
        self.buffer.push(' ');
        self
    }

    pub fn write(&mut self, string: &str) -> &mut Self {
        self.buffer.push_str(string);
        self
    }

    pub fn chain_if<F, G>(&mut self, predicate: F, inner: G) -> &mut Self
        where F: FnOnce() -> bool,
              G: FnOnce(&mut Self) {
        if predicate() {
            inner(self)
        }
        self
    }

    pub fn push_line(&mut self) -> &mut Self {
        self.lines.push(ScriptLine { indent: self.indentation, string: self.buffer.to_string() });
        self.buffer.clear();
        self
    }

    pub fn indent(&mut self) -> &mut Self {
        self.indentation += 1;
        self
    }

    pub fn dedent(&mut self) -> &mut Self {
        if self.indentation > 0 {
            self.indentation -= 1
        }
        self
    }
}

pub trait ToScript {
    fn to_script(&self, builder: &mut ScriptBuilder);
}

impl ToScript for Program {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        for statement in &self.statements {
            builder.write_data(statement).push_line();
        }
    }
}

impl ToScript for ProgramStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            ProgramStatement::Empty => { builder.write(";"); }
            ProgramStatement::ClassDeclaration(class_declaration) => { builder.write_data(&class_declaration.data); }
            ProgramStatement::CompilerDirective(compiler_directive) => { builder.write_data(compiler_directive); }
            ProgramStatement::ConstDeclaration(const_declaration) => { builder.write_data(const_declaration); }
            ProgramStatement::VarDeclaration(var_declaration) => { builder.write_data(&var_declaration.data); }
            ProgramStatement::EnumDeclaration(enum_declaration) => { builder.write_data(&enum_declaration.data).write(";"); }
            ProgramStatement::StructDeclaration(struct_declaration) => { builder.write_data(&struct_declaration.data).write(";"); }
            ProgramStatement::FunctionDeclaration(function_declaration) => { builder.write_data(&function_declaration.data); }
            ProgramStatement::ReplicationBlock(replication_block) => {}
            ProgramStatement::StateDeclaration(state_declaration) => {}
            ProgramStatement::DefaultProperties(defaultproperties) => {}
            ProgramStatement::CppText(cpptext) => {}
        }
    }
}

impl ToScript for FunctionModifierType {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for FunctionModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.type_);
        if !self.arguments.is_empty() {
            builder
                .write("(")
                .write_data_interspersed(&self.arguments, ", ")
                .write(")");
        }
    }
}

impl ToScript for FunctionTypeType {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for FunctionType {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.type_);
        if let Some(arguments) = &self.arguments {
            builder.write("(").write_data_interspersed(arguments, ", ").write(")");
        }
    }
}

impl ToScript for FunctionArgumentModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for FunctionArgument {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write_data_interspersed(&self.modifiers, " ").space()
            .write_data(&self.type_).space()
            .write_data(&self.name);
    }
}

impl ToScript for FunctionDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        if !self.modifiers.is_empty() {
            builder.write_data_interspersed(&self.modifiers, " ").space();
        }
        builder.write_data_interspersed(&self.types, " ").space();
        if let Some(return_type) = &self.return_type {
            builder.write_data(return_type).space();
        }
        builder.write_data(&self.name)
            .write("(")
            .write_data_interspersed(&self.arguments, ", ")
            .write(")").write(";").space();
    }
}

impl ToScript for StructModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for StructDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("struct").space();
        if !self.modifiers.is_empty() {
            builder.write_data_interspersed(&self.modifiers, " ").space();
        }
        builder.write_data(&self.name).space();
        if let Some(parent) = &self.parent {
            builder.write("extends").space().write_data(parent).space();
        }
        builder.write("{").push_line().indent();
        for member in &self.members {
            builder.write_data(member).push_line();
        }
        builder.dedent().write("}");
    }
}

impl ToScript for EnumDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write("enum").space()
            .write_data(&self.name).space().write("{").push_line().indent();
        for value in &self.values {
            builder.write_data(value).write(",").push_line();
        }
        builder.dedent().write("}");
    }
}

impl ToScript for Literal {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            Literal::None => { builder.write("none"); }
            Literal::Numeric(numeric) => { builder.write_data(numeric); }
            Literal::Boolean(boolean) => { builder.write(format!("{}", *boolean).as_str()); }
            Literal::String(string) => { builder.write(format!("\"{}\"", string).as_str()); }
            Literal::Name(name) => { builder.write(format!("'{}'", name).as_str()); }
            Literal::Rotator(rotator) => {
                builder
                    .write("rot")
                    .write("(")
                    .write_data_interspersed(rotator, ", ")
                    .write(")");
            }
            Literal::Vector(vector) => {
                builder
                    .write("vect")
                    .write("(")
                    .write_data_interspersed(vector, ", ")
                    .write(")");
            }
            Literal::Object { type_, reference } => {
                builder.write_data(type_).write(format!("'{}'", reference).as_str());
            }
        };
    }
}

impl ToScript for ConstDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write("const")
            .space()
            .write(&self.name)
            .space()
            .write("=")
            .space()
            .write_data(&self.value)
            .write(";");
    }
}

impl ToScript for CompilerDirective {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("#").write(self.command.as_str());
    }
}

impl ToScript for VarDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write("var")
            .chain_if(|| self.category.is_some(), |builder| {
                builder
                    .write("(")
                    .write(self.category.clone().unwrap().as_str())
                    .write(")");
            })
            .space()
            .write_data_interspersed(&self.modifiers, ", ").space()
            .write_data(&self.type_).space()
            .write_data_interspersed(&self.names, ", ").write(";")
            .push_line();
    }
}

impl ToScript for VarModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for Identifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(self.as_str());
    }
}

impl ToScript for NumericLiteral {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        let string = match self {
            NumericLiteral::Integer(integer) => { format!("{}", *integer) }
            NumericLiteral::Float(float) => { format!("{}", *float) }
        };
        builder.write(string.as_str());
    }
}

impl ToScript for VarSize {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("[");
        match self {
            VarSize::IntegerLiteral(integer_literal) => { builder.write_data(integer_literal); }
            VarSize::Identifier(identifier) => { builder.write_data(identifier); }
        }
        builder.write("]");
    }
}

impl ToScript for VarName {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.identifier);
        if let Some(size) = &self.size {
            builder.write_data(size);
        }
    }
}

impl ToScript for StructVarModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for StructVarDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("var").space();
        if self.is_editable {
            builder.write("()").space();
        }
        builder
            .write_data_interspersed(&self.modifiers, " ")
            .write_data(&self.type_).space()
            .write_data_interspersed(&self.names, ", ")
            .write(";");
    }
}

impl ToScript for Type {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match &self {
            Type::Pod(pod) => {
                builder.write(pod.to_string().as_str());
            }
            Type::Array(type_) => {
                builder
                    .write("array")
                    .write("<")
                    .write_data(type_);
                match type_.deref() {
                    Type::Array(_) | Type::Class(_) => {
                        // UCC cannot handle >> correctly, so we need to add the space to placate it
                        builder.space();
                    }
                    _ => {}
                }
                builder.write(">");
            }
            Type::Class(class) => {
                builder
                    .write("class")
                    .write("<")
                    .write_data(class)
                    .write(">");
            }
            Type::Struct(struct_) => {
                builder
                    .write("struct")
                    .write_data(&struct_.name.data)
                    .write("{")
                    .indent()
                        // .write()
                    .dedent()
                    .write("}");
            }
            Type::Enum(enum_) => {}
            Type::Identifier(identifier) => {}
        }
    }
}

impl ToScript for ClassDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("class").space().write(&self.name.string);
        if let Some(parent_class) = &self.parent_class {
            builder.space().write("extends").space().write(parent_class.string.as_str());
        }
        if let Some(within) = &self.within {
            builder.space().write("within").space().write(within.string.as_str());
        }
        if !self.modifiers.is_empty() {
            builder.push_line().indent();
            builder.write_data_interspersed(&self.modifiers[..], " ");
        }
        builder.write(";").push_line().dedent();
    }
}

impl ToScript for ClassModifierType {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).as_str());
    }
}

impl ToScript for ClassModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.type_);
        if let Some(arguments) = &self.arguments {
            builder
                .write("(")
                .write_data(&arguments.data)
                .write(")");
        }
    }
}

impl ToScript for ExpressionList {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        for (index, expression) in self.expressions.iter().enumerate() {
            if index > 0 {
                builder.write(",");
            }
            if let Some(expression) = expression {
                if index > 0 {
                    builder.space();
                }
                builder.write_data(expression);
            }
        }
    }
}

impl ToScript for Expression {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("<expr>");
    }
}
