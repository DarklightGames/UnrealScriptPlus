use crate::ast::*;
use crate::parser::{ProgramError, ProgramErrorSeverity};

pub struct Visitor {
    errors: Vec<ProgramError>,
}

impl Visitor {
    pub fn new() -> Visitor {
        Visitor { errors: vec![] }
    }

    pub fn get_errors(&self) -> &Vec<ProgramError> { &self.errors }

    pub fn warn(&mut self, message: &str, span: AstSpan) {
        self.errors.push(ProgramError {
            message: message.to_string(),
            span,
            severity: ProgramErrorSeverity::Warning,
        })
    }

    pub fn error(&mut self, message: &str, span: AstSpan) {
        self.errors.push(ProgramError {
            message: message.to_string(),
            span,
            severity: ProgramErrorSeverity::Error
        })
    }
}

pub trait Visit {
    fn visit(&self, visitor: &mut Visitor);
}

impl Visit for AstNode<ConstDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.data.name.data.string.ne(&self.data.name.data.string.to_uppercase()) {
            visitor.warn(format!("const variable names should be upper-case ({:?})", self.data.name.data.string).as_str(), self.span)
        }
    }
}

impl Visit for AstNode<DefaultPropertiesValue> {
    fn visit(&self, visitor: &mut Visitor) {
        if let DefaultPropertiesValue::Literal(literal) = &self.data {
            if let Literal::Name(_name) = literal {
                visitor.error("values in defaultproperties block can not be initialized with single-quoted strings", self.span)
            }
        }
    }
}

impl Visit for AstNode<DefaultPropertiesObject> {
    fn visit(&self, visitor: &mut Visitor) {
        self.data.statements.iter().for_each(|statement| statement.visit(visitor))
}
}

impl Visit for AstNode<DefaultPropertiesAssignment> {
    fn visit(&self, visitor: &mut Visitor) {
        if let Some(v) = &self.data.value {
            v.visit(visitor)
        }
    }
}

impl Visit for AstNode<DefaultPropertiesStatement> {
    fn visit(&self, visitor: &mut Visitor) {
        match &self.data {
            DefaultPropertiesStatement::Assignment(assignment) => assignment.visit(visitor),
            DefaultPropertiesStatement::Object(object) => object.visit(visitor)
        }
    }
}

impl Visit for AstNode<DefaultProperties> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.data.statements.is_empty() {
            visitor.warn("defaultproperties block should not be empty", self.span)
        }
        self.data.statements.iter().for_each(|f| f.visit(visitor))
    }
}

impl Visit for AstNode<ReplicationBlock> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.data.statements.is_empty() {
            visitor.warn("replication block should not be empty", self.span)
        }
    }
}

impl Visit for AstNode<FunctionDeclaration> {
    fn visit(&self, visitor: &mut Visitor) {
        if self.data.types.len() > 1 {
            visitor.error("multiple function types declared", self.span)
        }
    }
}

impl Visit for Program {
    fn visit(&self, visitor: &mut Visitor) {
        for statement in self.statements.iter() {
            match &statement.data {
                ProgramStatement::Empty => { visitor.warn("empty program statement", statement.span) }
                ProgramStatement::ClassDeclaration(_) => {},
                ProgramStatement::CompilerDirective(_) => {}
                ProgramStatement::ConstDeclaration(c) => c.visit(visitor),  // TODO: enforce naming convention of ALL CAPS const (warn)
                ProgramStatement::VarDeclaration(_) => {}
                ProgramStatement::EnumDeclaration(_) => {}
                ProgramStatement::StructDeclaration(_) => {}
                ProgramStatement::FunctionDeclaration(f) => f.visit(visitor),
                ProgramStatement::ReplicationBlock(r) => r.visit(visitor),
                ProgramStatement::StateDeclaration(_) => {}
                ProgramStatement::DefaultProperties(d) => { d.visit(visitor) }  // TODO: ensure that only one of these exists
                ProgramStatement::CppText(_) => {}
            }
        }
    }
}
