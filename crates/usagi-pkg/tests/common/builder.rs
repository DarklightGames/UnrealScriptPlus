//! Builds packages with classes, properties and objects that refer to each
//! other, for tests.

#![allow(dead_code)]

use std::path::Path;

use usagi_pkg::archive::{NameRef, Saver, Serialize};
use usagi_pkg::{Class, Field, ObjectRef, PropertyDecl, PropertyKind, State, Struct};

use super::{ExportBytes, ImportBytes, PackageBytes, SummaryBytes, index, name};

pub const PUBLIC: u32 = 0x0007_0004;
pub const CPF_TRANSIENT: u32 = 0x2000;

/// Builds a package: names, imports and exports are added as they're
/// needed, and exports' data can be set after they're added, so that they
/// can refer to each other.
pub struct Builder {
    pub names: Vec<String>,
    pub imports: Vec<ImportBytes>,
    pub exports: Vec<ExportBytes>,
    pub data: Vec<Vec<u8>>,
    pub version: u32,
    pub licensee: u32,
}

impl Builder {
    pub fn new() -> Builder {
        let mut builder = Builder {
            names: Vec::new(),
            imports: Vec::new(),
            exports: Vec::new(),
            data: Vec::new(),
            version: 128,
            licensee: 29,
        };
        builder.name("None");
        builder
    }

    pub fn name(&mut self, text: &str) -> i32 {
        match self.names.iter().position(|n| n == text) {
            Some(i) => i as i32,
            None => {
                self.names.push(text.to_string());
                self.names.len() as i32 - 1
            }
        }
    }

    /// An import; its reference.
    pub fn import(&mut self, class_package: &str, class: &str, outer: i32, object: &str) -> i32 {
        let import = ImportBytes {
            class_package: self.name(class_package),
            class_name: self.name(class),
            outer,
            object_name: self.name(object),
        };
        self.imports.push(import);
        -(self.imports.len() as i32)
    }

    /// An import of a package.
    pub fn package(&mut self, package: &str) -> i32 {
        self.import("Core", "Package", 0, package)
    }

    /// An import of class `Core.<class>`, for the classes of the exports.
    pub fn core_class(&mut self, class: &str) -> i32 {
        let core = match self
            .imports
            .iter()
            .position(|i| i.outer == 0 && self.names[i.object_name as usize] == "Core")
        {
            Some(i) => -(i as i32 + 1),
            None => self.package("Core"),
        };
        if let Some(i) = self
            .imports
            .iter()
            .position(|i| i.outer == core && self.names[i.object_name as usize] == class)
        {
            return -(i as i32 + 1);
        }
        self.import("Core", "Class", core, class)
    }

    /// An export with no data yet; its reference.
    pub fn export(&mut self, class: i32, outer: i32, object: &str) -> i32 {
        let object_name = self.name(object);
        self.exports.push(ExportBytes {
            class,
            outer,
            object_name,
            flags: PUBLIC,
            size: 0,
            ..ExportBytes::default()
        });
        self.data.push(Vec::new());
        self.exports.len() as i32
    }

    pub fn set_data(&mut self, export: i32, data: Vec<u8>) {
        let i = export as usize - 1;
        self.exports[i].size = data.len() as i32;
        self.data[i] = data;
    }

    pub fn saved<T: Serialize>(&self, mut value: T) -> Vec<u8> {
        let mut saver = Saver::new(self.version as u16, self.licensee as u16);
        value.serialize(&mut saver).unwrap();
        saver.into_bytes()
    }

    /// A struct's `UStruct` data: its superclass and first field.
    pub fn structure(&mut self, name: &str, super_field: i32, children: i32) -> Struct {
        Struct {
            field: Field {
                super_field: ObjectRef::from_raw(super_field),
                next: ObjectRef::Null,
            },
            children: ObjectRef::from_raw(children),
            friendly_name: NameRef(self.name(name)),
            struct_flags: Some(0),
            ..Struct::default()
        }
    }

    /// A property declaration, in `outer`, followed by `next`.
    pub fn property(
        &mut self,
        outer: i32,
        name: &str,
        kind: PropertyKind,
        array_dim: i32,
        flags: u32,
        next: i32,
    ) -> i32 {
        let class_name = match &kind {
            PropertyKind::Byte { .. } => "ByteProperty",
            PropertyKind::Int => "IntProperty",
            PropertyKind::Bool => "BoolProperty",
            PropertyKind::Float => "FloatProperty",
            PropertyKind::Object { .. } => "ObjectProperty",
            PropertyKind::Class { .. } => "ClassProperty",
            PropertyKind::Name => "NameProperty",
            PropertyKind::Str => "StrProperty",
            PropertyKind::Delegate { .. } => "DelegateProperty",
            PropertyKind::FixedArray { .. } => "FixedArrayProperty",
            PropertyKind::Array { .. } => "ArrayProperty",
            PropertyKind::Map { .. } => "MapProperty",
            PropertyKind::Struct { .. } => "StructProperty",
            PropertyKind::Pointer => "PointerProperty",
        };
        let class = self.core_class(class_name);
        let export = self.export(class, outer, name);
        let decl = PropertyDecl {
            field: Field {
                super_field: ObjectRef::Null,
                next: ObjectRef::from_raw(next),
            },
            array_dim,
            flags,
            kind,
            ..PropertyDecl::default()
        };
        let data = [vec![0], self.saved(decl)].concat();
        self.set_data(export, data);
        export
    }

    pub fn bytes(&self) -> Vec<u8> {
        PackageBytes {
            summary: SummaryBytes {
                file_version: self.version | self.licensee << 16,
                ..SummaryBytes::default()
            },
            names: self.names.iter().map(|text| name(text)).collect(),
            imports: self.imports.clone(),
            exports: self.exports.clone(),
            data: self.data.clone(),
        }
        .bytes()
    }

    pub fn write(&self, dir: &Path, file: &str) {
        std::fs::write(dir.join(file), self.bytes()).unwrap();
    }

    /// A property's tag, with a size byte (or word, for a bigger value),
    /// its struct's name and array index if it has them, then its value.
    /// `size` is the size the tag says, which may be wrong.
    pub fn prop(
        &mut self,
        property: &str,
        id: u8,
        struct_name: Option<&str>,
        array_index: Option<u8>,
        value: &[u8],
        size: usize,
    ) -> Vec<u8> {
        let mut out = index(self.name(property));
        let size_bits = if size <= 255 { 0x50 } else { 0x60 };
        out.push(id | size_bits | if array_index.is_some() { 0x80 } else { 0 });
        if let Some(struct_name) = struct_name {
            out.extend(index(self.name(struct_name)));
        }
        if size <= 255 {
            out.push(size as u8);
        } else {
            out.extend((size as u16).to_le_bytes());
        }
        out.extend(array_index);
        out.extend(value);
        out
    }

    /// A property's tag with the size of its value.
    pub fn simple(&mut self, property: &str, id: u8, value: &[u8]) -> Vec<u8> {
        self.prop(property, id, None, None, value, value.len())
    }

    pub fn set_flags(&mut self, export: i32, flags: u32) {
        self.exports[export as usize - 1].flags = flags;
    }

    /// A class: its superclass, first field and defaults (tagged
    /// properties, without the `None` that ends them).
    pub fn class(&mut self, name: &str, super_class: i32, children: i32, defaults: Vec<u8>) -> i32 {
        let export = self.export(0, 0, name);
        self.set_class(export, super_class, children, defaults);
        export
    }

    pub fn set_class(&mut self, export: i32, super_class: i32, children: i32, defaults: Vec<u8>) {
        let name = self.names[self.exports[export as usize - 1].object_name as usize].clone();
        let structure = self.structure(&name, super_class, children);
        let mut data = self.saved(Class {
            state: State {
                structure,
                ..State::default()
            },
            ..Class::default()
        });
        data.extend(defaults);
        data.push(0);
        self.set_data(export, data);
    }
}

pub fn ob(raw: i32) -> ObjectRef {
    ObjectRef::from_raw(raw)
}

pub fn ints(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

pub fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

// Property types (`Info & 0x0F`).
pub const BYTE: u8 = 1;
pub const INT: u8 = 2;
pub const BOOL: u8 = 3;
pub const FLOAT: u8 = 4;
pub const OBJECT: u8 = 5;
pub const NAME: u8 = 6;
pub const DELEGATE: u8 = 7;
pub const ARRAY: u8 = 9;
pub const STRUCT: u8 = 10;
pub const STR: u8 = 13;
pub const MAP: u8 = 14;
pub const FIXED_ARRAY: u8 = 15;
