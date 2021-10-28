use crate::ast::*;

pub trait Transform {
    fn transform(&self) -> String;
}

impl Transform for ClassModifier {
    fn transform(&self) -> String {
        format!("{:?}", self.type_)
    }
}

impl Transform for ClassDeclaration {
    fn transform(&self) -> String {
        format!("class {:?};", self.name)
    }
}

impl Transform for Program {
    fn transform(&self) -> String {
        todo!()
    }
}
