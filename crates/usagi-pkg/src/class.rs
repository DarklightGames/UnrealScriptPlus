//! The native data of classes, states, functions and structs: `UField`,
//! `UStruct`, `UState`, `UClass` and `UFunction`'s `Serialize`
//! (`Core/Src/UnClass.cpp`), each after its parent's.

use crate::archive::{Archive, FString, Loader, NameRef, Serialize};
use crate::script::Script;
use crate::{Error, Guid, ObjectRef, Package, Result};

/// `RF_HasStack`: the object's data starts with its state frame.
pub(crate) const RF_HAS_STACK: u32 = 0x0200_0000;

/// `FUNC_Net`: the function is replicated, and has a replication offset.
const FUNC_NET: u32 = 0x0000_0040;

/// `CPF_Net`: the property is replicated, and has a replication offset.
const CPF_NET: u32 = 0x0000_0020;

/// `CPF_CommentString`: the property has a comment.
const CPF_COMMENT_STRING: u32 = 0x0200_0000;

/// The state frame of an object with `RF_HasStack` (such as an actor in a
/// map), at the start of its data (`UObject::Serialize`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct StateFrame {
    pub node: ObjectRef,
    pub state_node: ObjectRef,
    pub probe_mask: u64,
    pub latent_action: i32,
    /// Where the object is in `node`'s code, if `node` isn't `None`:
    /// `INDEX_NONE` (-1) if nowhere.
    pub offset: Option<i32>,
}

impl Serialize for StateFrame {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.object(&mut self.node, "state frame")?;
        ar.object(&mut self.state_node, "state frame")?;
        ar.u64(&mut self.probe_mask, "state frame")?;
        ar.i32(&mut self.latent_action, "state frame")?;
        if self.node != ObjectRef::Null {
            let offset = self.offset.get_or_insert(-1);
            ar.index(offset, "state frame")?;
        }
        Ok(())
    }
}

/// `UField`: a member of a struct.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Field {
    pub super_field: ObjectRef,
    /// The next field in the struct.
    pub next: ObjectRef,
}

impl Serialize for Field {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.object(&mut self.super_field, "field")?;
        ar.object(&mut self.next, "field")
    }
}

/// `UStruct`: a struct, function, state or class.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Struct {
    pub field: Field,
    /// The source code, a `TextBuffer`; `None` in packages built without it.
    pub script_text: ObjectRef,
    /// The first field.
    pub children: ObjectRef,
    pub friendly_name: NameRef,
    /// From licensee version 26 (`0x1A`).
    pub struct_flags: Option<u32>,
    pub line: i32,
    pub text_pos: i32,
    pub script: Script,
}

impl Serialize for Struct {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.field.serialize(ar)?;
        ar.object(&mut self.script_text, "struct")?;
        ar.object(&mut self.children, "struct")?;
        ar.name(&mut self.friendly_name, "struct")?;
        if ar.licensee_version() >= 0x1A {
            ar.u32(self.struct_flags.get_or_insert(0), "struct")?;
        }
        ar.i32(&mut self.line, "struct")?;
        ar.i32(&mut self.text_pos, "struct")?;
        self.script.serialize(ar)
    }
}

/// `UFunction`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Function {
    pub structure: Struct,
    /// Up to version 63.
    pub parms_size: Option<u16>,
    pub native: u16,
    /// Up to version 63.
    pub num_parms: Option<u8>,
    pub operator_precedence: u8,
    /// Up to version 63.
    pub return_value_offset: Option<u16>,
    pub flags: u32,
    /// For a replicated function (`FUNC_Net`).
    pub rep_offset: Option<u16>,
}

impl Serialize for Function {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.structure.serialize(ar)?;
        let old = ar.version() <= 63;
        if old {
            ar.u16(self.parms_size.get_or_insert(0), "function")?;
        }
        ar.u16(&mut self.native, "function")?;
        if old {
            ar.u8(self.num_parms.get_or_insert(0), "function")?;
        }
        ar.u8(&mut self.operator_precedence, "function")?;
        if old {
            ar.u16(self.return_value_offset.get_or_insert(0), "function")?;
        }
        ar.u32(&mut self.flags, "function")?;
        if self.flags & FUNC_NET != 0 {
            ar.u16(self.rep_offset.get_or_insert(0), "function")?;
        }
        Ok(())
    }
}

/// `UState`: a state, or a class.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct State {
    pub structure: Struct,
    pub probe_mask: u64,
    pub ignore_mask: u64,
    pub label_table_offset: u16,
    pub state_flags: u32,
}

impl Serialize for State {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.structure.serialize(ar)?;
        ar.u64(&mut self.probe_mask, "state")?;
        ar.u64(&mut self.ignore_mask, "state")?;
        ar.u16(&mut self.label_table_offset, "state")?;
        ar.u32(&mut self.state_flags, "state")
    }
}

/// A class this class depends on (`FDependency`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Dependency {
    pub class: ObjectRef,
    pub deep: u32,
    pub script_text_crc: u32,
}

impl Serialize for Dependency {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.object(&mut self.class, "dependency")?;
        ar.u32(&mut self.deep, "dependency")?;
        ar.u32(&mut self.script_text_crc, "dependency")
    }
}

/// `UClass`, up to its default properties, which follow it as tagged
/// properties ([`Package::properties`](crate::Package::properties)).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Class {
    pub state: State,
    /// Up to version 61.
    pub old_class_record_size: Option<i32>,
    pub flags: u32,
    pub guid: Guid,
    pub dependencies: Vec<Dependency>,
    pub package_imports: Vec<NameRef>,
    /// From version 62.
    pub within: Option<ObjectRef>,
    /// From version 62.
    pub config_name: Option<NameRef>,
    /// From version 99.
    pub hide_categories: Option<Vec<NameRef>>,
}

impl Serialize for Class {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.state.serialize(ar)?;
        if ar.version() <= 61 {
            ar.i32(self.old_class_record_size.get_or_insert(0), "class")?;
        }
        ar.u32(&mut self.flags, "class")?;
        self.guid.serialize(ar)?;
        ar.array_of(&mut self.dependencies, "class's dependencies")?;
        ar.array_of(&mut self.package_imports, "class's package imports")?;
        if ar.version() >= 62 {
            ar.object(self.within.get_or_insert(ObjectRef::Null), "class")?;
            ar.name(
                self.config_name.get_or_insert_with(NameRef::default),
                "class",
            )?;
        }
        if ar.version() >= 99 {
            ar.array_of(
                self.hide_categories.get_or_insert_with(Vec::new),
                "class's hidden categories",
            )?;
        }
        Ok(())
    }
}

/// `UProperty` and its subclasses: a declared variable, a member of a class
/// or struct, or a function's parameter or local.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct PropertyDecl {
    pub field: Field,
    /// The number of elements of a static array; 1 otherwise.
    pub array_dim: i32,
    /// `CPF_` flags.
    pub flags: u32,
    pub category: NameRef,
    /// For a replicated property (`CPF_Net`).
    pub rep_offset: Option<u16>,
    /// For a property with a comment (`CPF_CommentString`).
    pub comment: Option<FString>,
    pub kind: PropertyKind,
}

/// The type of a [`PropertyDecl`], from its class (`IntProperty`, ...), and
/// what that class's `Serialize` adds.
#[derive(Debug, Clone, Default, PartialEq)]
pub enum PropertyKind {
    Byte {
        /// The enum, if it's one.
        enumeration: ObjectRef,
    },
    #[default]
    Int,
    Bool,
    Float,
    Object {
        class: ObjectRef,
    },
    Class {
        class: ObjectRef,
        meta_class: ObjectRef,
    },
    Name,
    Str,
    Delegate {
        function: ObjectRef,
    },
    FixedArray {
        inner: ObjectRef,
        count: i32,
    },
    Array {
        inner: ObjectRef,
    },
    Map {
        key: ObjectRef,
        value: ObjectRef,
    },
    Struct {
        structure: ObjectRef,
    },
    Pointer,
}

impl PropertyKind {
    /// The kind of property of class `class` (`IntProperty`, ...), with its
    /// references not yet read; `None` if it isn't a property class.
    pub fn of_class(class: &str) -> Option<PropertyKind> {
        let null = ObjectRef::Null;
        Some(match class {
            "ByteProperty" => PropertyKind::Byte { enumeration: null },
            "IntProperty" => PropertyKind::Int,
            "BoolProperty" => PropertyKind::Bool,
            "FloatProperty" => PropertyKind::Float,
            "ObjectProperty" => PropertyKind::Object { class: null },
            "ClassProperty" => PropertyKind::Class {
                class: null,
                meta_class: null,
            },
            "NameProperty" => PropertyKind::Name,
            "StrProperty" => PropertyKind::Str,
            "DelegateProperty" => PropertyKind::Delegate { function: null },
            "FixedArrayProperty" => PropertyKind::FixedArray {
                inner: null,
                count: 0,
            },
            "ArrayProperty" => PropertyKind::Array { inner: null },
            "MapProperty" => PropertyKind::Map {
                key: null,
                value: null,
            },
            "StructProperty" => PropertyKind::Struct { structure: null },
            "PointerProperty" => PropertyKind::Pointer,
            _ => return None,
        })
    }
}

impl Serialize for PropertyDecl {
    /// `UProperty::Serialize`, then its class's. When loading, `kind` must
    /// already be the right kind ([`PropertyKind::of_class`]).
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.field.serialize(ar)?;
        ar.i32(&mut self.array_dim, "property")?;
        ar.u32(&mut self.flags, "property")?;
        ar.name(&mut self.category, "property")?;
        if self.flags & CPF_NET != 0 {
            ar.u16(self.rep_offset.get_or_insert(0), "property")?;
        }
        if self.flags & CPF_COMMENT_STRING != 0 {
            self.comment
                .get_or_insert_with(FString::default)
                .serialize(ar)?;
        }
        match &mut self.kind {
            PropertyKind::Byte { enumeration } => ar.object(enumeration, "property")?,
            PropertyKind::Object { class } => ar.object(class, "property")?,
            PropertyKind::Class { class, meta_class } => {
                ar.object(class, "property")?;
                ar.object(meta_class, "property")?;
            }
            PropertyKind::Delegate { function } => ar.object(function, "property")?,
            PropertyKind::FixedArray { inner, count } => {
                ar.object(inner, "property")?;
                ar.i32(count, "property")?;
            }
            PropertyKind::Array { inner } => ar.object(inner, "property")?,
            PropertyKind::Map { key, value } => {
                ar.object(key, "property")?;
                ar.object(value, "property")?;
            }
            PropertyKind::Struct { structure } => ar.object(structure, "property")?,
            PropertyKind::Int
            | PropertyKind::Bool
            | PropertyKind::Float
            | PropertyKind::Name
            | PropertyKind::Str
            | PropertyKind::Pointer => {}
        }
        Ok(())
    }
}

/// `UEnum`: an enum's values.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Enum {
    pub field: Field,
    pub names: Vec<NameRef>,
}

impl Serialize for Enum {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.field.serialize(ar)?;
        ar.array_of(&mut self.names, "enum")
    }
}

/// `UConst`: a constant, as its source text.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Const {
    pub field: Field,
    pub value: FString,
}

impl Serialize for Const {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        self.field.serialize(ar)?;
        self.value.serialize(ar)
    }
}

/// `UTextBuffer`: a class's source code.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TextBuffer {
    pub pos: i32,
    pub top: i32,
    pub text: FString,
}

impl Serialize for TextBuffer {
    fn serialize(&mut self, ar: &mut dyn Archive) -> Result<()> {
        ar.i32(&mut self.pos, "text buffer")?;
        ar.i32(&mut self.top, "text buffer")?;
        self.text.serialize(ar)
    }
}

impl Package {
    /// Reads export `export`'s data from `offset` as a `T`. Returns it and
    /// where it ends.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if the data ends first or has a value the
    /// engine would fail on.
    ///
    /// # Panics
    ///
    /// If `export` is out of range.
    pub fn read_native<T: Serialize>(&self, export: usize, offset: usize) -> Result<(T, usize)> {
        let file_offset = self.exports()[export].serial_offset.unwrap_or(0) as usize;
        let mut loader = Loader::new(self, self.export_data(export)).at(file_offset);
        loader.seek(offset);
        let mut value = T::default();
        value
            .serialize(&mut loader)
            .map_err(crate::properties::data_error)?;
        Ok((value, loader.pos()))
    }

    /// The state frame at the start of export `export`'s data, if it has
    /// one (`RF_HasStack`), and where what follows it starts.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if the state frame is malformed.
    ///
    /// # Panics
    ///
    /// If `export` is out of range.
    pub fn state_frame(&self, export: usize) -> Result<(Option<StateFrame>, usize)> {
        let has_stack = self.exports()[export].flags.0 & RF_HAS_STACK != 0;
        if has_stack && !self.export_data(export).is_empty() {
            let (frame, end) = self.read_native(export, 0)?;
            Ok((Some(frame), end))
        } else {
            Ok((None, 0))
        }
    }

    /// Export `export`, a class: its data up to its default properties, and
    /// where they start.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if the export isn't a class, or its data is
    /// malformed.
    pub fn class(&self, export: usize) -> Result<(Class, usize)> {
        if self.exports()[export].class != ObjectRef::Null {
            return Err(Error::InvalidData {
                offset: 0,
                reason: format!(
                    "{} isn't a class",
                    self.reference(ObjectRef::Export(export))
                ),
            });
        }
        let (_, start) = self.state_frame(export)?;
        self.read_native(export, start)
    }

    /// Export `export`, a property: its declaration, after its tagged
    /// properties.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if the export isn't a property, or its data is
    /// malformed.
    pub fn property_decl(&self, export: usize) -> Result<PropertyDecl> {
        let class = self.class_name(&self.exports()[export]);
        let kind = PropertyKind::of_class(class).ok_or_else(|| Error::InvalidData {
            offset: 0,
            reason: format!(
                "{} isn't a property",
                self.reference(ObjectRef::Export(export))
            ),
        })?;
        let start = self.native_start(export)?;
        let mut loader = Loader::new(self, self.export_data(export));
        loader.seek(start);
        let mut decl = PropertyDecl {
            kind,
            ..PropertyDecl::default()
        };
        decl.serialize(&mut loader)
            .map_err(crate::properties::data_error)?;
        Ok(decl)
    }

    /// Where export `export`'s native data starts: after its state frame
    /// and tagged properties.
    pub(crate) fn native_start(&self, export: usize) -> Result<usize> {
        let mut properties = self.properties(export)?;
        for property in properties.by_ref() {
            property?;
        }
        Ok(properties.offset())
    }

    /// Export `export`'s `UStruct` part, if it's a class, state, struct or
    /// function; `None` otherwise.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if its data is malformed.
    pub fn structure(&self, export: usize) -> Result<Option<Struct>> {
        if self.exports()[export].class == ObjectRef::Null {
            return Ok(Some(self.class(export)?.0.state.structure));
        }
        let start = || self.native_start(export);
        Ok(Some(match self.class_name(&self.exports()[export]) {
            "State" => self.read_native::<State>(export, start()?)?.0.structure,
            "Struct" => self.read_native::<Struct>(export, start()?)?.0,
            "Function" => self.read_native::<Function>(export, start()?)?.0.structure,
            _ => return Ok(None),
        }))
    }

    /// The next field after export `export`, a field of a struct.
    pub(crate) fn next_field(&self, export: usize) -> Result<ObjectRef> {
        let start = self.native_start(export)?;
        Ok(self.read_native::<Field>(export, start)?.0.next)
    }
}
