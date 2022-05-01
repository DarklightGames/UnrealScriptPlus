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

    pub fn finalize(&mut self) {
        self.push_line();
    }

    pub fn write_data<D: ToScript>(&mut self, data: &D) -> &mut Self {
        data.to_script(self);
        self
    }

    pub fn write_data_line_separated<D: ToScript>(&mut self, data: &[D]) -> &mut Self {
        self.write_data_interspersed_with(data, |builder| { builder.push_line(); })
    }

    pub fn write_data_interspersed_with<D: ToScript, F>(&mut self, data: &[D], separator: F) -> &mut Self
        where F: Fn(&mut Self) {
        if !data.is_empty() {
            for (index, datum) in data.iter().enumerate() {
                if index > 0 {
                    separator(self);
                }
                datum.to_script(self);
            }
        }
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

    pub fn inner_if_some<D, F>(&mut self, data: &Option<D>, inner: F) -> &mut Self
        where F: FnOnce(&mut Self, &D) {
        if let Some(data) = data {
            inner(self, data);
        }
        self
    }

    pub fn write_data_inner_interspersed<D, F>(&mut self, data: &[D], inner: F, separator: &str) -> &mut Self
        where F: Fn(&mut Self, &D) {
        for (index, datum) in data.iter().enumerate() {
            if index > 0 {
                self.write(separator);
            }
            inner(self, datum);
        }
        self
    }

    pub fn write_scope<F>(&mut self, inner: F) -> &mut Self
        where F: FnOnce(&mut Self) {
        self.write("{").push_line().indent();
        inner(self);
        self.push_line().dedent().write("}");
        self
    }

    pub fn inner_if<F, G>(&mut self, predicate: F, inner: G) -> &mut Self
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
        builder.write_data_line_separated(&self.statements);
    }
}

impl ToScript for DefaultPropertiesArrayIndex {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            DefaultPropertiesArrayIndex::Identifier(identifier) => {
                builder.write_data(identifier);
            }
            DefaultPropertiesArrayIndex::IntegerLiteral(integer_literal) => {
                builder.write_data(integer_literal);
            }
        }
    }
}

impl ToScript for DefaultPropertiesTarget {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.target);
        if let Some(index) = &self.index {
            builder.write("(").write_data(index).write(")");
        }
    }
}

impl ToScript for DefaultPropertiesStruct {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write("(")
            .write_data_interspersed(&self.assignments, ",")
            .write(")");
    }
}

impl ToScript for DefaultPropertiesArray {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        // TODO: we need something that can take a fn so this is handled more gracefully
        builder.write("(")
            .write_data_inner_interspersed(&self.elements, |builder, element| {
                if let Some(element) = element {
                    builder.write_data(element);
                }
            }, ",")
            .write(")");
    }
}

impl ToScript for DefaultPropertiesValue {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            DefaultPropertiesValue::Literal(literal) => { builder.write_data(literal); }
            DefaultPropertiesValue::Identifier(identifier) => { builder.write_data(identifier); }
            DefaultPropertiesValue::Struct(struct_) => { builder.write_data(struct_); }
            DefaultPropertiesValue::Array(array) => { builder.write_data(array); }
        }
    }
}

impl ToScript for DefaultPropertiesAssignment {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.target).write("=");
        if let Some(value) = &self.value {
            builder.write_data(value);
        }
    }
}

impl ToScript for DefaultPropertiesObject {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("Begin").space().write("Object").space().write("Class").write("=").write_data(&self.class).push_line().indent();
        for statement in &self.statements {
            builder.write_data(statement).push_line();
        }
        builder.dedent().write("End").space().write("Object");
    }
}

impl ToScript for DefaultPropertiesStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            DefaultPropertiesStatement::Assignment(assignment) => { builder.write_data(assignment); }
            DefaultPropertiesStatement::Object(object) => { builder.write_data(object.as_ref()); }
        }
    }
}

impl ToScript for DefaultProperties {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("defaultproperties").push_line().write_scope(|builder| {
            builder.write_data_line_separated(&self.statements);
        });
    }
}

impl ToScript for CppText {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("cpptext").push_line().write_scope(|builder| {
            builder.write(self.text.as_str());
        });
    }
}

impl ToScript for ProgramStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            ProgramStatement::Empty => { builder.write(";"); }
            ProgramStatement::ClassDeclaration(class_declaration) => { builder.write_data(class_declaration); }
            ProgramStatement::CompilerDirective(compiler_directive) => { builder.write_data(compiler_directive); }
            ProgramStatement::ConstDeclaration(const_declaration) => { builder.write_data(const_declaration); }
            ProgramStatement::VarDeclaration(var_declaration) => { builder.write_data(&var_declaration.data); }
            ProgramStatement::EnumDeclaration(enum_declaration) => { builder.write_data(enum_declaration).write(";"); }
            ProgramStatement::StructDeclaration(struct_declaration) => { builder.write_data(struct_declaration).write(";"); }
            ProgramStatement::FunctionDeclaration(function_declaration) => { builder.write_data(function_declaration); }
            ProgramStatement::ReplicationBlock(replication_block) => { builder.write_data(replication_block); }
            ProgramStatement::StateDeclaration(state_declaration) => { builder.write_data(state_declaration); }
            ProgramStatement::DefaultProperties(defaultproperties) => { builder.write_data(defaultproperties); }
            ProgramStatement::CppText(cpptext) => { builder.write_data(cpptext); }
        }
    }
}

impl ToScript for StateModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).to_lowercase().as_str());
    }
}

impl ToScript for StateStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            StateStatement::ConstDeclaration(const_declaration) => { builder.write_data(const_declaration); }
            StateStatement::FunctionDeclaration(function_declaration) => { builder.write_data(function_declaration); }
        }
    }
}

impl ToScript for ForEach {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("foreach").space().write_data(&self.predicate).push_line().write_data(&self.body);
    }
}

impl ToScript for IfStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("if").space().write("(").write_data(&self.predicate).write(")");
        match &self.body {
            None => { builder.write(";"); }
            Some(body) => {
                builder.push_line().write_data(body);
            }
        }
    }
}

impl ToScript for ElifStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("else").space().write("if").space().write("(").write_data(&self.predicate).write(")").push_line()
            .write_data(&self.body);
    }
}

impl ToScript for ElseStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("else").push_line().write_data(&self.body);
    }
}

impl ToScript for ConditionalStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.if_statement);
        if !self.elif_statements.is_empty() {
            builder.push_line().write_data_line_separated(&self.elif_statements);
        }
        if let Some(else_statement) = &self.else_statement {
            builder.push_line().write_data(else_statement);
        }
    }
}

impl ToScript for WhileStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("while").space().write("(").write_data(&self.predicate).write(")").push_line()
            .write_data(&self.body);
    }
}

impl ToScript for ForStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("for").space().write("(");
        let control_expressions = [&self.init, &self.predicate, &self.post];
        for (index, expression) in control_expressions.iter().enumerate() {
            match expression {
                None => {
                    if index > 0 {
                        builder.write(";");
                    }
                }
                Some(expression) => {
                    if index > 0 {
                        builder.write(";").space();
                    }
                    builder.write_data(expression);
                }
            }
        }
        builder.write(")").push_line()
            .write_data(&self.body);
    }
}

impl ToScript for SwitchCase {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match &self.type_ {
            SwitchCaseType::Expression(expression) => {
                builder.write("case").space().write_data(expression).write(":").push_line();
            }
            SwitchCaseType::Default => {
                builder.write("default").write(":").push_line();
            }
        }
        builder.indent().write_data_line_separated(&self.statements).push_line().dedent();
    }
}

impl ToScript for SwitchStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("switch").space().write("(").write(")").push_line()
            .write_scope(|builder| {
                builder.write_data_line_separated(&self.cases);
            });
    }
}

impl ToScript for DoUntil {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("do").push_line().write_data(&self.body);
        if let Some(predicate) = &self.predicate {
            builder.push_line().write("until").space().write("(").write_data(predicate).write(")");
        }
    }
}

impl ToScript for CodeStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            CodeStatement::Empty => { builder.write(";"); }
            CodeStatement::Expression(expression) => { builder.write_data(expression).write(";"); }
            CodeStatement::Return(value) => {
                builder.write("return");
                if let Some(value) = value {
                    builder.space().write_data(value);
                }
                builder.write(";");
            }
            CodeStatement::Break => { builder.write("break").write(";"); }
            CodeStatement::Continue => { builder.write("continue").write(";"); }
            CodeStatement::Goto(label) => { builder.write("goto").space().write_data(label).write(";"); }
            CodeStatement::JumpLabel(label) => { builder.write_data(label).write(":"); }
            CodeStatement::ForEach(foreach) => { builder.write_data(foreach); }
            CodeStatement::For(for_statement) => { builder.write_data(for_statement); }
            CodeStatement::Switch(switch) => { builder.write_data(switch); }
            CodeStatement::Conditional(conditional) => { builder.write_data(conditional); }
            CodeStatement::While(while_statement) => { builder.write_data(while_statement); }
            CodeStatement::DoUntil(do_until) => { builder.write_data(do_until); }
            CodeStatement::CompilerDirective(compiler_directive) => { builder.write_data(compiler_directive); }
            CodeStatement::ConstDeclaration(const_declaration) => { builder.write_data(const_declaration); }
        }
    }
}

impl ToScript for StateLabel {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data(&self.label).write(":").push_line().write_data_line_separated(&self.statements);
    }
}

impl ToScript for StateDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .inner_if(|| !self.modifiers.is_empty(), |builder| {
                builder.write_data_interspersed(&self.modifiers, " ").space();
            })
            .write("state")
            .inner_if(|| self.is_editable, |builder| {
                builder.write("()");
            }).space().write_data(&self.name).space();
        if let Some(parent) = &self.parent {
            builder.write("extends").space().write_data(parent).space();
        }
        builder.push_line().write_scope(|builder| {
            if !self.ignores.is_empty() {
                builder.write("ignores").space().write_data_interspersed(&self.ignores, ", ").write(";").push_line();
            }
            builder.write_data_line_separated(&self.statements);
            builder.write_data_line_separated(&self.labels);
        });
    }
}

impl ToScript for ReplicationReliability {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).to_lowercase().as_str());
    }
}

impl ToScript for ReplicationStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write_data(&self.reliability).space().write("if").space().write("(").write_data(&self.condition).write(")").push_line()
            .indent().write_data_interspersed(&self.variables, ", ").write(";").push_line().dedent();
    }
}

impl ToScript for ReplicationBlock {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("replication").push_line().write_scope(|builder| {
            builder.write_data_line_separated(&self.statements);
        });
    }
}

impl ToScript for FunctionModifierType {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).to_lowercase().as_str());
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
        builder.write(format!("{:?}", self).to_lowercase().as_str());
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
        builder.write(format!("{:?}", self).to_lowercase().as_str());
    }
}

impl ToScript for FunctionArgument {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        if !self.modifiers.is_empty() {
            builder.write_data_interspersed(&self.modifiers, " ").space();
        }
        builder
            .write_data(&self.type_).space()
            .write_data(&self.name);
    }
}

impl ToScript for CodeBlock {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_scope(|builder| {
            builder.write_data_line_separated(&self.statements);
        });
    }
}

impl ToScript for CodeStatementOrBlock {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            CodeStatementOrBlock::CodeBlock(block) => { builder.write_data(block); }
            CodeStatementOrBlock::CodeStatement(statement) => { builder.write_data(statement); }
        }
    }
}

impl ToScript for LocalDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write("local").space().write_data(&self.type_).space().write_data_interspersed(&self.names, ", ").write(";");
    }
}

impl ToScript for FunctionBodyStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            FunctionBodyStatement::ConstDeclaration(const_declaration) => { builder.write_data(const_declaration); }
            FunctionBodyStatement::LocalDeclaration(local_declaration) => { builder.write_data(local_declaration); }
            FunctionBodyStatement::CodeStatement(code_statement) => {builder.write_data(code_statement); }
        }
    }
}

impl ToScript for FunctionBody {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data_line_separated(&self.statements);
    }
}

impl ToScript for FunctionDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        if !self.modifiers.is_empty() {
            builder.write_data_interspersed(&self.modifiers, " ").space();
        }
        builder.write_data_interspersed(&self.types, " ").space()
            .inner_if_some(&self.return_type, |builder, return_type| {
                builder.write_data(return_type).space();
            })
            .write_data(&self.name)
            .write("(")
            .write_data_interspersed(&self.arguments, ", ")
            .write(")");
        match &self.body {
            None => { builder.write(";"); }
            Some(body) => {
                builder.push_line().write_scope(|builder| {
                    builder.write_data(body);
                });
            }
        }


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
        builder.push_line().write_scope(|builder| {
            builder.write_data_line_separated(&self.members);
        });
    }
}

impl ToScript for EnumDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder
            .write("enum").space().write_data(&self.name).push_line()
            .write_scope(|builder| {
                builder.write_data_line_separated(&self.values);
            });
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
            .inner_if(|| self.category.is_some(), |builder| {
                builder
                    .write("(")
                    .write(self.category.clone().unwrap().as_str())
                    .write(")");
            })
            .space()
            .inner_if(|| !self.modifiers.is_empty(), |builder| {
                builder.write_data_interspersed(&self.modifiers, ", ").space();
            })
            .write_data(&self.type_).space()
            .write_data_interspersed(&self.names, ", ").write(";");
    }
}

impl ToScript for VarModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("{:?}", self).to_lowercase().as_str());
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
        builder.write("var");
        if self.is_editable {
            builder.write("()");
        }
        builder.space()
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
                builder.write("class").write("<").write_data(class).write(">");
            }
            Type::Struct(struct_) => { builder.write_data(struct_); }
            Type::Enum(enum_) => { builder.write_data(enum_); }
            Type::Identifier(identifier) => { builder.write_data(identifier); }
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
            builder.push_line()
                .indent().write_data_interspersed(&self.modifiers[..], " ");
        }
        builder.write(";").dedent();
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

impl ToScript for MonadicVerb {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(match self {
            MonadicVerb::Increment => "++",
            MonadicVerb::Decrement => "--",
            MonadicVerb::BitwiseNot => "~",
            MonadicVerb::Negate => "-",
            MonadicVerb::Not => "!"
        });
    }
}

impl ToScript for DyadicVerb {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(match self {
            DyadicVerb::LogicalAnd => "&&",
            DyadicVerb::LogicalExclusiveOr => "^^",
            DyadicVerb::Exponentiate => "**",
            DyadicVerb::AddAssign => "+=",
            DyadicVerb::SubtractAssign => "-=",
            DyadicVerb::MultiplyAssign => "*=",
            DyadicVerb::DivideAssign => "/=",
            DyadicVerb::Multiply => "*",
            DyadicVerb::Divide => "/",
            DyadicVerb::LogicalOr => "||",
            DyadicVerb::Modulo => "%",
            DyadicVerb::ShiftLeft => "<<",
            DyadicVerb::DoubleShiftRight => ">>>",
            DyadicVerb::ShiftRight => "<<",
            DyadicVerb::CompareLessThanOrEqual => "<=",
            DyadicVerb::CompareGreaterThanOrEqual => ">=",
            DyadicVerb::CompareEqual => "==",
            DyadicVerb::CompareApproximatelyEqual => "~=",
            DyadicVerb::CompareLessThan => "<",
            DyadicVerb::CompareGreaterThan => ">",
            DyadicVerb::CompareNotEqual => "!=",
            DyadicVerb::BitwiseAnd => "&",
            DyadicVerb::BitwiseOr => "|",
            DyadicVerb::BitwiseExclusiveOr => "^",
            DyadicVerb::ConcatenateAssign => "$=",
            DyadicVerb::ConcatenateSpaceAssign => "@=",
            DyadicVerb::Concatenate => "$",
            DyadicVerb::ConcatenateSpace => "@",
            DyadicVerb::Add => "+",
            DyadicVerb::Subtract => "-",
            DyadicVerb::Assign => "=",
            DyadicVerb::Dot => "dot",
            DyadicVerb::Cross => "cross",
        });
    }
}

impl ToScript for FStringElement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            FStringElement::Literal(string) => {
                builder.write("\"").write(string).write("\"");
            }
            FStringElement::Expression(expression) => {
                builder.write("(").write_data(expression).write(")");
            }
        }
    }
}

impl ToScript for FString {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write_data_interspersed_with(&self.elements, |builder| {
            builder.space().write("$").space();
        });
    }
}

impl ToScript for Expression {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            Expression::Identifier(identifier) => { builder.write_data(identifier); }
            Expression::Literal(literal) => { builder.write_data(literal); }
            Expression::New { arguments, type_ } => {
                builder.write("new");
                if let Some(arguments) = arguments {
                    builder.write("(").write_data(arguments).write(")").space();
                }
                builder.write_data(type_);
            }
            Expression::MonadicPreExpression { verb, operand } => {
                builder.write_data(verb).write_data(operand);
            }
            Expression::MonadicPostExpression { operand, verb } => {
                builder.write_data(operand).write_data(verb);
            }
            Expression::DyadicExpression { lhs, verb, rhs } => {
                builder.write_data(lhs).space().write_data(verb).space().write_data(rhs);
            }
            Expression::Call { arguments, operand } => {
                builder.write_data(operand).write("(");
                if let Some(arguments) = arguments {
                    builder.write_data(arguments);
                }
                builder.write(")");
            }
            Expression::GlobalCall { name, arguments } => {
                builder.write("global").write(".").write_data(name).write("(");
                if let Some(arguments) = arguments {
                    builder.write_data(arguments);
                }
                builder.write(")");
            }
            Expression::ArrayAccess { operand, argument } => {
                builder.write_data(operand).write("[").write_data(argument).write("]");
            }
            Expression::DefaultAccess { operand, target } => {
                if let Some(operand) = operand {
                    builder.write_data(operand).write(".");
                }
                builder.write("default").write(".").write_data(target);
            }
            Expression::StaticAccess { operand, target } => {
                if let Some(operand) = operand {
                    builder.write_data(operand).write(".");
                }
                builder.write("static").write(".").write_data(target);
            }
            Expression::MemberAccess { operand, target } => {
                builder.write_data(operand).write(".").write_data(target);
            }
            Expression::Cast { operand, type_ } => {
                builder.write_data(type_).write("(").write_data(operand).write(")");
            }
            Expression::ParentheticalExpression(expression) => {
                builder.write("(").write_data(expression).write(")");
            }
            Expression::FString(fstring) => {
                builder.write_data(fstring);
            }
        }
    }
}
