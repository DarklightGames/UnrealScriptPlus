//! Tagged properties: an object's script-visible properties, stored at the
//! start of its data as a list of tags, each followed by its value, ended by
//! the name `None`. See `FPropertyTag` and `UStruct::SerializeTaggedProperties`
//! in the engine's `Core/Src/UnClass.cpp`, `UObject::Serialize` in
//! `Core/Src/UnObj.cpp`, and the properties' `SerializeItem` methods in
//! `Core/Src/UnProp.cpp`.
//!
//! A tag records a property's name, type and size, so its value can be read
//! without the class that defines it, except where the type alone doesn't
//! say how: a byte's enum, a dynamic array's element type, and the layout of
//! a struct stored in binary.

use std::rc::Rc;

use crate::packages::{Decl, Lookup, Packages, Scope, Type};
use crate::reader::Reader;
use crate::{Error, ObjectRef, Package, Result};

/// The first licensee version whose struct values are themselves tagged
/// properties, except for `Vector`, `Rotator` and `Color`. Before it, every
/// struct is stored in binary, in the struct's layout
/// (`UStructProperty::SerializeItem`).
pub const TAGGED_STRUCTS_LICENSEE_VERSION: u16 = 0x1C;

/// How deeply structs are read inside structs. A struct nested deeper is
/// kept as raw bytes.
const MAX_DEPTH: usize = 32;

/// A property's type, from the low four bits of its tag's info byte (the
/// engine's `NAME_ByteProperty` ... `NAME_FixedArrayProperty`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropertyType {
    Byte,
    Int,
    Bool,
    Float,
    Object,
    Name,
    Delegate,
    Class,
    Array,
    Struct,
    Vector,
    Rotator,
    Str,
    Map,
    FixedArray,
    /// 0, which isn't a property type.
    Unknown,
}

impl PropertyType {
    /// The type with this ID (`Info & 0x0F`).
    pub fn from_id(id: u8) -> PropertyType {
        use PropertyType::*;
        match id & 0x0F {
            1 => Byte,
            2 => Int,
            3 => Bool,
            4 => Float,
            5 => Object,
            6 => Name,
            7 => Delegate,
            8 => Class,
            9 => Array,
            10 => Struct,
            11 => Vector,
            12 => Rotator,
            13 => Str,
            14 => Map,
            15 => FixedArray,
            _ => Unknown,
        }
    }

    /// The engine's name for the type: `ByteProperty`, `IntProperty`, ...
    pub fn name(self) -> &'static str {
        use PropertyType::*;
        match self {
            Byte => "ByteProperty",
            Int => "IntProperty",
            Bool => "BoolProperty",
            Float => "FloatProperty",
            Object => "ObjectProperty",
            Name => "NameProperty",
            Delegate => "DelegateProperty",
            Class => "ClassProperty",
            Array => "ArrayProperty",
            Struct => "StructProperty",
            Vector => "VectorProperty",
            Rotator => "RotatorProperty",
            Str => "StrProperty",
            Map => "MapProperty",
            FixedArray => "FixedArrayProperty",
            Unknown => "Unknown",
        }
    }
}

/// A tagged property.
#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub name: String,
    pub kind: PropertyType,
    /// For a struct, the struct's name (`ItemName`).
    pub struct_name: Option<String>,
    /// The element of a static array (`var int Foo[4]`); 0 otherwise.
    pub array_index: u32,
    /// Where the tag starts in the export's data.
    pub offset: usize,
    /// Where the value starts in the export's data.
    pub value_offset: usize,
    /// The size of the value in bytes, as read: 0 for a bool, whose value
    /// is in the tag.
    pub size: usize,
    /// The size of the value according to its tag. It differs from
    /// [`size`](Property::size) for some structs and arrays of structs,
    /// which the engine reads by their tags rather than their size (see
    /// [`Value::Struct`]).
    pub tag_size: usize,
    pub value: Value,
}

impl Property {
    /// The property's name as T3D writes it: `Foo`, or `Foo(2)` for an
    /// element of a static array other than the first.
    pub fn t3d_name(&self) -> String {
        if self.array_index == 0 {
            self.name.clone()
        } else {
            format!("{}({})", self.name, self.array_index)
        }
    }
}

/// A property's value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A byte, or an enum's value: which enum isn't stored.
    Byte(u8),
    Int(i32),
    Bool(bool),
    Float(f32),
    /// An object (`ObjectProperty`) or a class (`ClassProperty`).
    Object(ObjectRef),
    Name(String),
    Str(String),
    Delegate {
        object: ObjectRef,
        function: String,
    },
    Vector {
        x: f32,
        y: f32,
        z: f32,
    },
    Rotator {
        pitch: i32,
        yaw: i32,
        roll: i32,
    },
    Color {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    /// A struct stored as tagged properties (from licensee version
    /// [`TAGGED_STRUCTS_LICENSEE_VERSION`]).
    ///
    /// The engine reads it by its tags, not by the size in the property's
    /// tag, which it gets wrong: when it saves a struct, it works out the
    /// sizes of the struct's own properties as if every object and name
    /// in them took one byte, so a property whose object or name takes three
    /// is written with a size byte the struct's size doesn't count.
    Struct(Vec<Property>),
    /// A dynamic array of structs stored as tagged properties.
    ///
    /// An array's tag doesn't say what its elements are. Its elements are
    /// taken to be structs if they read as tagged properties, each ending
    /// in `None`, and either end where the tag's size says, or where a tag
    /// follows them when there's none where the size says (for a size
    /// that's wrong, as above).
    StructArray(Vec<Vec<Property>>),
    /// A dynamic or static array read by its declaration, which gives its
    /// elements' type.
    List(Vec<Value>),
    /// A dynamic array: its length, and its elements' bytes, whose type
    /// isn't stored.
    Array {
        len: usize,
        data: Vec<u8>,
    },
    /// A value that can't be read without its property's definition (a
    /// struct stored in binary, a map), or that doesn't fit its size or
    /// type.
    Raw(Vec<u8>),
}

/// An object's tagged properties, read one at a time. Made by
/// [`Package::properties`].
///
/// Reading stops at the `None` that ends the list, or at the first error.
#[derive(Debug, Clone)]
pub struct Properties<'a> {
    package: &'a Package,
    reader: Reader<'a>,
    start: usize,
    done: bool,
    /// Where the properties' declarations are, if they're read by them.
    schema: Option<(&'a Packages, Scope)>,
}

impl Properties<'_> {
    /// Where the properties start in the export's data: 0, or the size of
    /// the state frame before them.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Where reading has got to in the export's data. Once every property
    /// has been read, this is where they end, after the `None`: where the
    /// object's native data starts, if it has any.
    pub fn offset(&self) -> usize {
        self.reader.pos()
    }
}

impl Iterator for Properties<'_> {
    type Item = Result<Property>;

    fn next(&mut self) -> Option<Result<Property>> {
        if self.done {
            return None;
        }
        let schema = self
            .schema
            .as_ref()
            .map(|(packages, scope)| Schema { packages, scope });
        let property = read_property(self.package, &mut self.reader, 0, schema)
            .map_err(data_error)
            .transpose();
        self.done = !matches!(property, Some(Ok(_)));
        property
    }
}

impl Package {
    /// The tagged properties of export `export`. They're at the start of
    /// its data, after its state frame if it has one (`RF_HasStack`); for a
    /// class, they're its default properties, after its script
    /// ([`Package::class`]). An export with no data has none.
    ///
    /// # Errors
    ///
    /// [`Error::InvalidData`] if its state frame, or a class's data before
    /// its defaults, is malformed.
    ///
    /// # Panics
    ///
    /// If `export` is out of range.
    pub fn properties(&self, export: usize) -> Result<Properties<'_>> {
        let data = self.export_data(export);
        let start = if data.is_empty() {
            0
        } else if self.exports()[export].class == ObjectRef::Null {
            self.class(export)?.1
        } else {
            self.state_frame(export)?.1
        };
        let mut reader = Reader::new(data);
        reader.seek(start);
        Ok(Properties {
            package: self,
            start: reader.pos(),
            reader,
            done: data.is_empty(),
            schema: None,
        })
    }

    /// A value as T3D text, as UnrealEd writes it (`ExportTextItem`): floats
    /// with six decimals, objects as `Class'Package.Group.Name'`, strings in
    /// double quotes, and structs as `(X=1.000000,Y=2.000000)`. A byte is its
    /// number, since which enum it belongs to isn't stored. Values that
    /// can't be read are written in angle brackets, which T3D doesn't have:
    /// `<3 elements, 12 bytes>` for an array and `<4 bytes: 01 02 03 04>` for
    /// raw bytes.
    pub fn t3d_value(&self, value: &Value) -> String {
        match value {
            Value::Byte(v) => v.to_string(),
            Value::Int(v) => v.to_string(),
            Value::Bool(v) => if *v { "True" } else { "False" }.to_string(),
            Value::Float(v) => float(*v),
            Value::Object(object) => self.reference(*object),
            Value::Name(name) => name.clone(),
            Value::Str(text) => format!("\"{}\"", text),
            Value::Delegate { object, function } => match self.object_name(*object) {
                Some(object) => format!("{}.{}", object, function),
                None => "None.None".to_string(),
            },
            Value::Vector { x, y, z } => {
                format!("(X={},Y={},Z={})", float(*x), float(*y), float(*z))
            }
            Value::Rotator { pitch, yaw, roll } => {
                format!("(Pitch={},Yaw={},Roll={})", pitch, yaw, roll)
            }
            Value::Color { r, g, b, a } => format!("(B={},G={},R={},A={})", b, g, r, a),
            Value::Struct(properties) => self.t3d_struct(properties),
            Value::StructArray(elements) => {
                let elements: Vec<String> = elements.iter().map(|e| self.t3d_struct(e)).collect();
                format!("({})", elements.join(","))
            }
            Value::List(elements) => {
                let elements: Vec<String> = elements.iter().map(|e| self.t3d_value(e)).collect();
                format!("({})", elements.join(","))
            }
            Value::Array { len, data } => format!(
                "<{} element{}, {} byte{}>",
                len,
                plural(*len),
                data.len(),
                plural(data.len())
            ),
            Value::Raw(bytes) => {
                const SHOWN: usize = 16;
                let mut hex: Vec<String> = bytes
                    .iter()
                    .take(SHOWN)
                    .map(|b| format!("{:02X}", b))
                    .collect();
                if bytes.len() > SHOWN {
                    hex.push("...".to_string());
                }
                format!(
                    "<{} byte{}{}{}>",
                    bytes.len(),
                    plural(bytes.len()),
                    if bytes.is_empty() { "" } else { ": " },
                    hex.join(" ")
                )
            }
        }
    }

    /// A struct's properties as T3D: `(X=1.000000,Y=2.000000)`.
    fn t3d_struct(&self, properties: &[Property]) -> String {
        let fields: Vec<String> = properties
            .iter()
            .map(|p| format!("{}={}", p.t3d_name(), self.t3d_value(&p.value)))
            .collect();
        format!("({})", fields.join(","))
    }
}

/// A float as the engine writes it (`%f`).
fn float(value: f32) -> String {
    format!("{:.6}", value)
}

fn plural(count: usize) -> &'static str {
    if count == 1 { "" } else { "s" }
}

/// An error in an export's data: the reader's offsets are in the data, not
/// the file.
pub(crate) fn data_error(error: Error) -> Error {
    match error {
        Error::UnexpectedEof { offset, what } => Error::InvalidData {
            offset,
            reason: format!("the data ends in the {}", what),
        },
        other => other,
    }
}

/// Reads a property, or `None` at the `None` that ends the list. `depth` is
/// how many structs it's inside.
fn read_property(
    package: &Package,
    r: &mut Reader,
    depth: usize,
    schema: Option<Schema>,
) -> Result<Option<Property>> {
    let offset = r.pos();
    let property_name = name(package, r, "property tag")?;
    if property_name.eq_ignore_ascii_case("None") {
        return Ok(None);
    }
    let info = r.u8("property tag")?;
    let kind = PropertyType::from_id(info);
    let struct_name = match kind {
        PropertyType::Struct => Some(name(package, r, "property tag")?),
        _ => None,
    };
    let tag_size = match info & 0x70 {
        0x00 => 1,
        0x10 => 2,
        0x20 => 4,
        0x30 => 12,
        0x40 => 16,
        0x50 => usize::from(r.u8("property tag")?),
        0x60 => usize::from(r.u16("property tag")?),
        _ => {
            let at = r.pos();
            let size = r.i32("property tag")?;
            usize::try_from(size).map_err(|_| Error::InvalidData {
                offset: at,
                reason: format!("property {} has a size of {}", property_name, size),
            })?
        }
    };
    let array_index = if info & 0x80 != 0 && kind != PropertyType::Bool {
        array_index(r)?
    } else {
        0
    };
    let value_offset = r.pos();
    let tag = Tag {
        id: info & 0x0F,
        kind,
        struct_name,
        array_index,
        size: tag_size,
        array_bit: info & 0x80 != 0,
    };
    let lookup = schema.map(|schema| {
        (
            schema,
            schema.packages.find_property(schema.scope, property_name),
        )
    });
    let declared = match &lookup {
        Some((schema, Lookup::Found(decl))) => {
            declared_value(package, *schema, decl, &tag, r, depth)?
        }
        _ => None,
    };
    let value = match declared {
        Some(value) => value,
        // The engine skips a property it can't read by its declaration.
        None if matches!(lookup, Some((_, Lookup::Found(_) | Lookup::Missing))) => {
            sized_value(package, &tag, r)?
        }
        // Without its declaration, it's read as well as the tag allows.
        None => match kind {
            // A bool's value is the tag's array bit.
            PropertyType::Bool => {
                r.bytes(tag_size, "property value")?;
                Value::Bool(tag.array_bit)
            }
            _ => match tagged_value(package, kind, struct_name, tag_size, r, depth) {
                Some(value) => value,
                None => sized_value(package, &tag, r)?,
            },
        },
    };
    Ok(Some(Property {
        name: property_name.to_string(),
        kind,
        struct_name: struct_name.map(str::to_string),
        array_index,
        offset,
        value_offset,
        size: r.pos() - value_offset,
        tag_size,
        value,
    }))
}

/// A struct stored as tagged properties, or an array of them, read by its
/// tags as the engine does (see [`Value::Struct`]); `None` if the value
/// isn't one, or can't be read as one. Moves `r` past the value only if it
/// is one.
fn tagged_value(
    package: &Package,
    kind: PropertyType,
    struct_name: Option<&str>,
    tag_size: usize,
    r: &mut Reader,
    depth: usize,
) -> Option<Value> {
    if package.summary.licensee_version() < TAGGED_STRUCTS_LICENSEE_VERSION || depth >= MAX_DEPTH {
        return None;
    }
    let mut t = r.clone();
    let value = match kind {
        PropertyType::Struct
            if !is_binary_struct(struct_name.expect("a struct's tag has its name")) =>
        {
            Value::Struct(struct_properties(package, &mut t, depth)?)
        }
        PropertyType::Array => {
            let len = usize::try_from(t.index("property value").ok()?).ok()?;
            let mut elements = Vec::new();
            for _ in 0..len {
                let element = struct_properties(package, &mut t, depth)?;
                if element.is_empty() || element.iter().any(|p| p.kind == PropertyType::Unknown) {
                    return None;
                }
                elements.push(element);
            }
            // The engine reads the elements and doesn't look at the size, so
            // where they end wins if a tag follows them, unless a property's
            // tag is where the size says: then they're something else that
            // happens to read as structs, and swallow the properties after.
            let mut sized = r.clone();
            sized.seek(r.pos().saturating_add(tag_size));
            if elements.is_empty()
                || (t.pos() != sized.pos()
                    && (tag_at(package, &mut sized) == Some(TagStart::Property)
                        || tag_at(package, &mut t.clone()).is_none()))
            {
                return None;
            }
            Value::StructArray(elements)
        }
        _ => return None,
    };
    *r = t;
    Some(value)
}

/// A tag's fields that decide how its value is read.
struct Tag<'n> {
    /// The type's ID (`Info & 0x0F`).
    id: u8,
    kind: PropertyType,
    struct_name: Option<&'n str>,
    array_index: u32,
    size: usize,
    /// The tag's array bit, which is a bool's value.
    array_bit: bool,
}

/// Where declarations come from, and the struct or class whose properties
/// are being read.
#[derive(Clone, Copy)]
struct Schema<'s> {
    packages: &'s Packages,
    scope: &'s Scope,
}

/// The value as many bytes as the tag says, decoded as well as the tag
/// allows: how the engine skips a property it doesn't read.
fn sized_value(package: &Package, tag: &Tag, r: &mut Reader) -> Result<Value> {
    let bytes = r.bytes(tag.size, "property value")?;
    Ok(match tag.kind {
        PropertyType::Bool => Value::Bool(tag.array_bit),
        _ => decode(package, tag.kind, tag.struct_name, bytes)
            .unwrap_or_else(|| Value::Raw(bytes.to_vec())),
    })
}

/// A value read by its property's declaration, as
/// `UStruct::SerializeTaggedProperties` does; `None` if the engine would
/// skip it (its tag doesn't match the declaration, or it isn't stored), or
/// if a declaration it needs couldn't be read.
fn declared_value(
    package: &Package,
    schema: Schema,
    decl: &Decl,
    tag: &Tag,
    r: &mut Reader,
    depth: usize,
) -> Result<Option<Value>> {
    let start = r.pos();
    let licensee = package.summary.licensee_version();
    let value = match &decl.ty {
        // Old packages: a static array saved before it became dynamic.
        Type::Array(inner) if tag.id != 9 && inner.id() == tag.id => {
            typed_value(package, schema.packages, inner, r, depth)?
        }
        // Old packages: a byte saved before it became a float.
        Type::Float if tag.id == 1 => Some(Value::Float(f32::from(r.u8("property value")?))),
        ty if ty.id() != tag.id => return Ok(None),
        _ if tag.array_index >= decl.array_dim.max(0) as u32 => return Ok(None),
        Type::Struct(name, _)
            if !tag
                .struct_name
                .is_some_and(|tag_name| tag_name.eq_ignore_ascii_case(name)) =>
        {
            return Ok(None);
        }
        _ if !decl.is_stored() => return Ok(None),
        Type::Bool => Some(Value::Bool(tag.array_bit)),
        ty => typed_value(package, schema.packages, ty, r, depth)?,
    };
    if value.is_none() {
        r.seek(start);
    } else if licensee < TAGGED_STRUCTS_LICENSEE_VERSION && r.pos() != start + tag.size {
        // Before tagged structs, the engine trusts the size.
        r.seek(start + tag.size);
    }
    Ok(value)
}

/// A value of type `ty`, as its property's `SerializeItem` reads it; `None`
/// if a declaration it needs couldn't be read.
fn typed_value(
    package: &Package,
    packages: &Packages,
    ty: &Type,
    r: &mut Reader,
    depth: usize,
) -> Result<Option<Value>> {
    let what = "property value";
    let checked_object = |r: &mut Reader| -> Result<ObjectRef> {
        let at = r.pos();
        let raw = r.index(what)?;
        let object = ObjectRef::from_raw(raw);
        let in_range = match object {
            ObjectRef::Null => true,
            ObjectRef::Export(i) => i < package.exports().len(),
            ObjectRef::Import(i) => i < package.imports().len(),
        };
        if in_range {
            Ok(object)
        } else {
            Err(Error::InvalidData {
                offset: at,
                reason: format!("object reference {} is out of range", raw),
            })
        }
    };
    Ok(Some(match ty {
        Type::Byte(_) => Value::Byte(r.u8(what)?),
        Type::Int | Type::Pointer => Value::Int(r.i32(what)?),
        Type::Bool => Value::Bool(r.u8(what)? != 0),
        Type::Float => Value::Float(f32::from_bits(r.u32(what)?)),
        Type::Object => Value::Object(checked_object(r)?),
        Type::Name => Value::Name(name(package, r, what)?.to_string()),
        Type::Str => Value::Str(r.string(what)?),
        Type::Delegate => Value::Delegate {
            object: checked_object(r)?,
            function: name(package, r, what)?.to_string(),
        },
        // `UMapProperty::SerializeItem` reads nothing.
        Type::Map => Value::Raw(Vec::new()),
        Type::Array(inner) => {
            let at = r.pos();
            let count = r.index(what)?;
            let count = usize::try_from(count).map_err(|_| Error::InvalidData {
                offset: at,
                reason: format!("an array has {} elements", count),
            })?;
            let mut elements = Vec::new();
            for _ in 0..count {
                let Some(element) = typed_value(package, packages, inner, r, depth)? else {
                    return Ok(None);
                };
                elements.push(element);
            }
            Value::List(elements)
        }
        Type::FixedArray(inner, count) => {
            let mut elements = Vec::new();
            for _ in 0..(*count).max(0) {
                let Some(element) = typed_value(package, packages, inner, r, depth)? else {
                    return Ok(None);
                };
                elements.push(element);
            }
            Value::List(elements)
        }
        Type::Struct(name, scope) => {
            let tagged = package.summary.licensee_version() >= TAGGED_STRUCTS_LICENSEE_VERSION
                && !is_binary_struct(name);
            if depth >= MAX_DEPTH {
                return Ok(None);
            }
            if tagged {
                let schema = scope.as_ref().map(|scope| Schema { packages, scope });
                let mut properties = Vec::new();
                while let Some(property) = read_property(package, r, depth + 1, schema)? {
                    properties.push(property);
                }
                Value::Struct(properties)
            } else if let Some(value) = binary_struct(name, r) {
                value?
            } else {
                let Some(scope) = scope else {
                    return Ok(None);
                };
                let Some(decls) = packages.struct_properties(scope) else {
                    return Ok(None);
                };
                let mut properties = Vec::new();
                for decl in decls.iter().filter(|decl| decl.is_stored()) {
                    for array_index in 0..decl.array_dim.max(0) as u32 {
                        let offset = r.pos();
                        let Some(value) = typed_value(package, packages, &decl.ty, r, depth + 1)?
                        else {
                            return Ok(None);
                        };
                        let size = r.pos() - offset;
                        properties.push(Property {
                            name: decl.name.clone(),
                            kind: PropertyType::from_id(decl.ty.id()),
                            struct_name: match &decl.ty {
                                Type::Struct(name, _) => Some(name.clone()),
                                _ => None,
                            },
                            array_index,
                            offset,
                            value_offset: offset,
                            size,
                            tag_size: size,
                            value,
                        });
                    }
                }
                Value::Struct(properties)
            }
        }
        Type::Unknown => return Ok(None),
    }))
}

/// `Vector`, `Rotator` and `Color`, which are always stored in binary;
/// `None` for another struct.
fn binary_struct(name: &str, r: &mut Reader) -> Option<Result<Value>> {
    let what = "property value";
    let value = if name.eq_ignore_ascii_case("Vector") {
        (|| {
            Ok(Value::Vector {
                x: f32::from_bits(r.u32(what)?),
                y: f32::from_bits(r.u32(what)?),
                z: f32::from_bits(r.u32(what)?),
            })
        })()
    } else if name.eq_ignore_ascii_case("Rotator") {
        (|| {
            Ok(Value::Rotator {
                pitch: r.i32(what)?,
                yaw: r.i32(what)?,
                roll: r.i32(what)?,
            })
        })()
    } else if name.eq_ignore_ascii_case("Color") {
        // In the struct's layout: `var byte B, G, R, A`.
        r.bytes(4, what).map(|bytes| Value::Color {
            b: bytes[0],
            g: bytes[1],
            r: bytes[2],
            a: bytes[3],
        })
    } else {
        return None;
    };
    Some(value)
}

impl Packages {
    /// Export `export`'s tagged properties, read by their declarations, as
    /// the engine reads them: the declarations are found in its class (or,
    /// for a class, in itself) and its superclasses, in this set's packages.
    /// A property whose declaration can't be found is read as
    /// [`Package::properties`] reads it.
    ///
    /// # Errors
    ///
    /// As [`Package::properties`].
    ///
    /// # Panics
    ///
    /// If `export` is out of range.
    pub fn properties<'a>(
        &'a self,
        package: &'a Rc<Package>,
        export: usize,
    ) -> Result<Properties<'a>> {
        let mut properties = package.properties(export)?;
        properties.schema = self.scope_of(package, export).map(|scope| (self, scope));
        Ok(properties)
    }
}

/// Whether a struct is stored in binary even when others are tagged
/// (`UStructProperty::SerializeItem`).
fn is_binary_struct(name: &str) -> bool {
    ["Vector", "Rotator", "Color"]
        .iter()
        .any(|binary| name.eq_ignore_ascii_case(binary))
}

/// A struct's tagged properties, up to the `None` that ends them.
fn struct_properties(package: &Package, r: &mut Reader, depth: usize) -> Option<Vec<Property>> {
    let mut properties = Vec::new();
    while let Some(property) = read_property(package, r, depth + 1, None).ok()? {
        properties.push(property);
    }
    Some(properties)
}

/// What starts at `r`: a property's tag, or the `None` that ends a list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TagStart {
    Property,
    None,
}

/// The tag that starts at `r`, if it looks like one.
fn tag_at(package: &Package, r: &mut Reader) -> Option<TagStart> {
    let tag_name = name(package, r, "property tag").ok()?;
    if tag_name.eq_ignore_ascii_case("None") {
        return Some(TagStart::None);
    }
    let is_tag = match r.u8("property tag").map(PropertyType::from_id) {
        Ok(PropertyType::Struct) => name(package, r, "property tag").is_ok(),
        Ok(kind) => kind != PropertyType::Unknown,
        Err(_) => false,
    };
    is_tag.then_some(TagStart::Property)
}

/// A tag's array index: one, two or four bytes, by the top bits of the
/// first.
fn array_index(r: &mut Reader) -> Result<u32> {
    let b = u32::from(r.u8("property tag")?);
    if b & 0x80 == 0 {
        return Ok(b);
    }
    let c = u32::from(r.u8("property tag")?);
    if b & 0xC0 == 0x80 {
        return Ok((b & 0x7F) << 8 | c);
    }
    let d = u32::from(r.u8("property tag")?);
    let e = u32::from(r.u8("property tag")?);
    Ok((b & 0x3F) << 24 | c << 16 | d << 8 | e)
}

/// A name reference: a compact index into the name table.
fn name<'p>(package: &'p Package, r: &mut Reader, what: &'static str) -> Result<&'p str> {
    let at = r.pos();
    let index = r.index(what)?;
    usize::try_from(index)
        .ok()
        .filter(|&i| i < package.names().len())
        .map(|i| package.name(i))
        .ok_or_else(|| Error::InvalidData {
            offset: at,
            reason: format!(
                "name {} is out of range: there are {} names",
                index,
                package.names().len()
            ),
        })
}

/// An object reference: a compact index, checked to be in range.
fn object(package: &Package, r: &mut Reader) -> Option<ObjectRef> {
    let object = ObjectRef::from_raw(r.index("property value").ok()?);
    let in_range = match object {
        ObjectRef::Null => true,
        ObjectRef::Export(i) => i < package.exports().len(),
        ObjectRef::Import(i) => i < package.imports().len(),
    };
    in_range.then_some(object)
}

/// Decodes the bytes of a value whose size is the tag's; `None` if they
/// can't be read, or if the value doesn't use exactly all of them.
fn decode(
    package: &Package,
    kind: PropertyType,
    struct_name: Option<&str>,
    bytes: &[u8],
) -> Option<Value> {
    let r = &mut Reader::new(bytes);
    let what = "property value";
    let value = match kind {
        PropertyType::Byte => Value::Byte(r.u8(what).ok()?),
        PropertyType::Int => Value::Int(r.i32(what).ok()?),
        PropertyType::Float => Value::Float(f32::from_bits(r.u32(what).ok()?)),
        PropertyType::Object | PropertyType::Class => Value::Object(object(package, r)?),
        PropertyType::Name => Value::Name(name(package, r, what).ok()?.to_string()),
        PropertyType::Str => Value::Str(r.string(what).ok()?),
        PropertyType::Delegate => Value::Delegate {
            object: object(package, r)?,
            function: name(package, r, what).ok()?.to_string(),
        },
        PropertyType::Vector => vector(r)?,
        PropertyType::Rotator => rotator(r)?,
        PropertyType::Struct => match struct_name.expect("a struct's tag has its name") {
            name if name.eq_ignore_ascii_case("Vector") => vector(r)?,
            name if name.eq_ignore_ascii_case("Rotator") => rotator(r)?,
            name if name.eq_ignore_ascii_case("Color") => {
                // In the struct's layout: `var byte B, G, R, A`.
                let [b, g, red, a] = r.bytes(4, what).ok()?.try_into().expect("4 bytes");
                Value::Color { r: red, g, b, a }
            }
            // Stored in binary, in a layout only its definition gives.
            _ => return None,
        },
        PropertyType::Array => {
            let len = usize::try_from(r.index(what).ok()?).ok()?;
            return Some(Value::Array {
                len,
                data: bytes[r.pos()..].to_vec(),
            });
        }
        PropertyType::Bool
        | PropertyType::Map
        | PropertyType::FixedArray
        | PropertyType::Unknown => {
            return None;
        }
    };
    r.at_end().then_some(value)
}

fn vector(r: &mut Reader) -> Option<Value> {
    let mut float = || r.u32("property value").ok().map(f32::from_bits);
    Some(Value::Vector {
        x: float()?,
        y: float()?,
        z: float()?,
    })
}

fn rotator(r: &mut Reader) -> Option<Value> {
    let mut int = || r.i32("property value").ok();
    Some(Value::Rotator {
        pitch: int()?,
        yaw: int()?,
        roll: int()?,
    })
}
