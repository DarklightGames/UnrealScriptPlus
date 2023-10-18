use crate::ast::*;

pub struct ScriptFormattingOptions {}

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
                string += String::from("\t").repeat(self.indentation).as_str();
            }
            string += line.string.as_str();
            string += "\n";
        }
        string
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

    pub fn write_data_interspersed<D: ToScript>(&mut self, data: &[D], separator: &str) {
        if !data.is_empty() {
            for (index, datum) in data.iter().enumerate() {
                if index > 0 {
                    self.write(separator);
                }
                datum.to_script(self);
            }
        }
    }

    pub fn write(&mut self, string: &str) -> &mut Self {
        self.buffer.push_str(string);
        self
    }

    pub fn push_line(&mut self) -> &mut Self {
        self.lines.push(ScriptLine {
            indent: self.indentation,
            string: self.buffer.to_string(),
        });
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
    fn to_script(&self, _builder: &mut ScriptBuilder) {
        for _statement in &self.statements {}
    }
}

impl ToScript for ProgramStatement {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        match self {
            ProgramStatement::Empty => {}
            ProgramStatement::ClassDeclaration(class_declaration) => {
                builder.write_data(&class_declaration.data);
            }
            ProgramStatement::CompilerDirective(_) => {}
            ProgramStatement::ConstDeclaration(_) => {}
            ProgramStatement::VarDeclaration(_) => {}
            ProgramStatement::EnumDeclaration(_) => {}
            ProgramStatement::StructDeclaration(_) => {}
            ProgramStatement::FunctionDeclaration(_) => {}
            ProgramStatement::ReplicationBlock(_) => {}
            ProgramStatement::StateDeclaration(_) => {}
            ProgramStatement::DefaultProperties(_) => {}
            ProgramStatement::CppText(_) => {}
        }
    }
}

impl ToScript for ClassDeclaration {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        builder.write(format!("class {}", &self.name.string).as_str());
        if let Some(parent_class) = &self.parent_class {
            builder.write(format!("extends {}", parent_class.string).as_str());
        }
        if let Some(within) = &self.within {
            builder.write(format!("within {}", within.string).as_str());
        }
        if !self.modifiers.is_empty() {
            builder.push_line().indent();
            // builder.write_data_interspersed(&self.modifiers.iter().map(|m| &m.data) [..], " ");
        }
        builder.write(";").push_line().dedent();
    }
}

impl ToScript for ClassModifier {
    fn to_script(&self, builder: &mut ScriptBuilder) {
        if let Some(arguments) = &self.arguments {
            builder.write("(").write_data(&arguments.data).write(")");
        }
    }
}

impl ToScript for ExpressionList {
    fn to_script(&self, _builder: &mut ScriptBuilder) {
        todo!()
    }
}
