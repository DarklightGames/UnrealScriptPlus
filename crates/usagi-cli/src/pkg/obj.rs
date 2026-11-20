//! `usagi pkg obj`: print objects' tagged properties.

use std::collections::BTreeSet;
use std::io::{self, Write};

use serde_json::{Map, Value as Json, json};
use usagi_pkg::{Error, ObjectPattern, ObjectRef, Package, Property, Value};

use super::{Align, BOLD, CYAN, Column, DIM, Style, column, separated, write_table};
use crate::cli::{PkgObjArgs, PkgObjFormat};

/// `usagi pkg obj`: prints the properties of the objects asked for. Returns
/// whether every object's properties could be read; problems are written to
/// `err`.
pub fn obj(
    args: &PkgObjArgs,
    color: bool,
    out: &mut impl Write,
    err: &mut impl Write,
) -> Result<bool, String> {
    if args.raw && args.format != PkgObjFormat::Text {
        return Err("--raw only works with --format text".to_string());
    }
    let file = args.path.display().to_string();
    let package = Package::read(&args.path).map_err(|e| format!("{}: {}", file, e))?;
    let exports = select(&package, args, &file)?;
    let objects: Vec<Object> = exports
        .into_iter()
        .map(|export| Object::read(&package, export, &args.property))
        .collect();
    let view = View {
        package: &package,
        objects: &objects,
        properties: &args.property,
        style: Style { color },
    };
    match args.format {
        PkgObjFormat::Text if args.raw => view.raw(out),
        PkgObjFormat::Text if !args.property.is_empty() => view.table(out),
        PkgObjFormat::Text => view.t3d(out),
        PkgObjFormat::Concise => view.concise(out),
        PkgObjFormat::Json => view.json(out),
    }
    .map_err(|e| e.to_string())?;
    let mut ok = true;
    for object in &objects {
        if let Some(error @ Error::InvalidData { .. }) = &object.error {
            writeln!(err, "usagi: {}: {}: {}", file, object.reference, error)
                .map_err(|e| e.to_string())?;
            ok = false;
        }
    }
    Ok(ok)
}

/// The exports to show, in table order: those matching the patterns (or
/// every export), of the classes asked for.
fn select(package: &Package, args: &PkgObjArgs, file: &str) -> Result<Vec<usize>, String> {
    let mut exports = BTreeSet::new();
    if args.objects.is_empty() {
        exports.extend(0..package.exports().len());
    }
    for text in &args.objects {
        let pattern: ObjectPattern = text.parse()?;
        let found = package.find_exports(&pattern);
        if found.is_empty() {
            return Err(not_found(package, &pattern, file));
        }
        exports.extend(found);
    }
    Ok(exports
        .into_iter()
        .filter(|&i| {
            args.class.is_empty() || args.class.iter().any(|class| is_class(package, i, class))
        })
        .collect())
}

/// Why `pattern` matches no export.
pub(super) fn not_found(package: &Package, pattern: &ObjectPattern, file: &str) -> String {
    if pattern.is_glob() {
        return format!("{} matches no objects in {}", pattern, file);
    }
    explain(package, pattern, file).unwrap_or_else(|| format!("{} isn't in {}", pattern, file))
}

/// Why `pattern`, which has no wildcards and matches no export, isn't in
/// the package, if it's there in some form: an export of another class, or
/// an import.
pub(super) fn explain(package: &Package, pattern: &ObjectPattern, file: &str) -> Option<String> {
    let any_class: ObjectPattern = pattern.path().parse().expect("a valid path");
    if let Some(&export) = package.find_exports(&any_class).first() {
        return Some(format!(
            "{} isn't a {}: it's {}",
            pattern,
            pattern.class().expect("a class, since the path matches"),
            package.reference(ObjectRef::Export(export))
        ));
    }
    package.find_imports(&any_class).first().map(|&import| {
        let top = package.top_level(ObjectRef::Import(import));
        format!(
            "{} isn't in {}: it's imported from the package {}",
            pattern,
            file,
            package.object_name(top).expect("an import")
        )
    })
}

/// Whether an export's class is `class`: its name (`Texture`), or its path
/// (`Engine.Texture`), ignoring case.
pub(super) fn is_class(package: &Package, export: usize, class: &str) -> bool {
    let class_ref = package.exports()[export].class;
    let name = package.class_name(&package.exports()[export]);
    let path = match class_ref {
        // A class's class is `Core.Class`.
        ObjectRef::Null => "Core.Class".to_string(),
        _ => package.full_path(class_ref).expect("a class"),
    };
    let wanted = if class.contains('.') {
        path
    } else {
        name.to_string()
    };
    wanted.eq_ignore_ascii_case(class)
}

/// An export and its properties.
pub(super) struct Object {
    pub(super) export: usize,
    pub(super) reference: String,
    /// The properties read, or those asked for.
    pub(super) properties: Vec<Property>,
    /// Where the properties start in the export's data.
    start: usize,
    /// Where they end, after the `None`; `None` if they couldn't all be read.
    pub(super) end: Option<usize>,
    size: usize,
    pub(super) error: Option<Error>,
}

impl Object {
    pub(super) fn read(package: &Package, export: usize, wanted: &[String]) -> Object {
        let mut object = Object {
            export,
            reference: package.reference(ObjectRef::Export(export)),
            properties: Vec::new(),
            start: 0,
            end: None,
            size: package.export_data(export).len(),
            error: None,
        };
        match package.properties(export) {
            Err(e) => object.error = Some(e),
            Ok(mut properties) => {
                object.start = properties.start();
                for property in properties.by_ref() {
                    match property {
                        Ok(property) => object.properties.push(property),
                        Err(e) => object.error = Some(e),
                    }
                }
                if object.error.is_none() {
                    object.end = Some(properties.offset());
                }
            }
        }
        if !wanted.is_empty() {
            object
                .properties
                .retain(|p| wanted.iter().any(|w| w.eq_ignore_ascii_case(&p.name)));
        }
        object
    }

    /// The size of the native data after the properties, if they could be
    /// read.
    fn native_size(&self) -> Option<usize> {
        self.end.map(|end| self.size - end)
    }
}

struct View<'a> {
    package: &'a Package,
    objects: &'a [Object],
    properties: &'a [String],
    style: Style,
}

impl View<'_> {
    /// T3D: a `Begin Object` block for each object, after a comment with its
    /// reference.
    fn t3d(&self, out: &mut impl Write) -> io::Result<()> {
        let style = self.style;
        for (i, object) in self.objects.iter().enumerate() {
            if i > 0 {
                writeln!(out)?;
            }
            write_t3d(self.package, object, style, out)?;
        }
        Ok(())
    }

    /// A table: a row for each object, a column for each property asked for.
    fn table(&self, out: &mut impl Write) -> io::Result<()> {
        let mut columns = vec![column("Object", Align::Left, None)];
        columns.extend(self.properties.iter().map(|name| Column {
            header: name.clone().into(),
            align: Align::Left,
            color: None,
        }));
        let rows: Vec<Vec<String>> = self
            .objects
            .iter()
            .map(|object| {
                let mut row = vec![object.reference.clone()];
                row.extend(self.properties.iter().map(|name| self.cell(object, name)));
                row
            })
            .collect();
        write_table(out, self.style, &columns, &rows)
    }

    /// The value of property `name` of `object`, as a table cell: `-` if it
    /// isn't stored, or `?` if it may be but the properties couldn't all be
    /// read. Elements of a static array are written `(0)=... (2)=...`.
    fn cell(&self, object: &Object, name: &str) -> String {
        let values: Vec<&Property> = object
            .properties
            .iter()
            .filter(|p| p.name.eq_ignore_ascii_case(name))
            .collect();
        match values.as_slice() {
            [] if object.error.is_some() => "?".to_string(),
            [] => "-".to_string(),
            [only] if only.array_index == 0 => self.package.t3d_value(&only.value),
            elements => elements
                .iter()
                .map(|p| format!("({})={}", p.array_index, self.package.t3d_value(&p.value)))
                .collect::<Vec<_>>()
                .join(" "),
        }
    }

    /// Each property's tag: where it is, its type, size and bytes.
    fn raw(&self, out: &mut impl Write) -> io::Result<()> {
        let style = self.style;
        let columns = [
            column("Offset", Align::Right, Some(DIM)),
            column("Name", Align::Left, None),
            column("Type", Align::Left, Some(CYAN)),
            column("Index", Align::Right, None),
            column("Size", Align::Right, None),
            column("Bytes", Align::Left, None),
        ];
        for (i, object) in self.objects.iter().enumerate() {
            if i > 0 {
                writeln!(out)?;
            }
            let export = &self.package.exports()[object.export];
            let header = match export.serial_offset {
                Some(offset) => format!(
                    "{}: {} bytes at {:#X}",
                    object.reference,
                    separated(object.size as u64),
                    offset
                ),
                None => format!("{}: no data", object.reference),
            };
            writeln!(out, "{}", style.paint(BOLD, &header))?;
            if object.start > 0 {
                writeln!(out, "  State frame  {} bytes", object.start)?;
            }
            let data = self.package.export_data(object.export);
            let mut rows: Vec<Vec<String>> = object
                .properties
                .iter()
                .map(|p| {
                    let kind = match &p.struct_name {
                        Some(name) => format!("{} ({})", p.kind.name(), name),
                        None => p.kind.name().to_string(),
                    };
                    vec![
                        format!("{:#X}", p.offset),
                        p.name.clone(),
                        kind,
                        p.array_index.to_string(),
                        if p.size == p.tag_size {
                            p.size.to_string()
                        } else {
                            format!("{} (tag: {})", p.size, p.tag_size)
                        },
                        hex(&data[p.value_offset..p.value_offset + p.size]),
                    ]
                })
                .collect();
            if object.end.is_some() && object.size > 0 {
                // The `None` that ends the list.
                let none = object
                    .properties
                    .last()
                    .map_or(object.start, |p| p.value_offset + p.size);
                rows.push(vec![
                    format!("{:#X}", none),
                    "None".to_string(),
                    String::new(),
                    String::new(),
                    String::new(),
                    String::new(),
                ]);
            }
            write_table(out, style, &columns, &rows)?;
            if let Some(size) = object.native_size().filter(|&size| size > 0) {
                writeln!(
                    out,
                    "  Native data  {} bytes at {:#X}",
                    separated(size as u64),
                    object.end.expect("read")
                )?;
            }
            if let Some(error) = &object.error {
                writeln!(out, "  {}", error)?;
            }
        }
        Ok(())
    }

    /// One line per property: `Texture'MyPkg.Brick' Format=5`.
    fn concise(&self, out: &mut impl Write) -> io::Result<()> {
        for object in self.objects {
            for property in &object.properties {
                writeln!(
                    out,
                    "{} {}={}",
                    object.reference,
                    property.t3d_name(),
                    self.package.t3d_value(&property.value)
                )?;
            }
        }
        Ok(())
    }

    fn json(&self, out: &mut impl Write) -> io::Result<()> {
        let objects: Vec<Json> = self
            .objects
            .iter()
            .map(|object| {
                let export = &self.package.exports()[object.export];
                let unsupported = matches!(object.error, Some(Error::Unsupported(_)));
                json!({
                    "index": ObjectRef::Export(object.export).raw(),
                    "reference": object.reference,
                    "class": self.package.class_name(export),
                    "path": self.package.full_path(ObjectRef::Export(object.export)),
                    "offset": export.serial_offset,
                    "size": object.size,
                    "properties": (!unsupported).then(|| self.json_properties(&object.properties)),
                    "native_size": object.native_size(),
                    "error": object.error.as_ref().map(|e| e.to_string()),
                })
            })
            .collect();
        serde_json::to_writer_pretty(&mut *out, &objects)?;
        writeln!(out)
    }

    fn json_properties(&self, properties: &[Property]) -> Json {
        Json::Array(
            properties
                .iter()
                .map(|p| {
                    let mut fields = Map::new();
                    fields.insert("name".into(), json!(p.name));
                    fields.insert("type".into(), json!(p.kind.name()));
                    if let Some(name) = &p.struct_name {
                        fields.insert("struct".into(), json!(name));
                    }
                    fields.insert("index".into(), json!(p.array_index));
                    fields.insert("offset".into(), json!(p.offset));
                    fields.insert("size".into(), json!(p.size));
                    fields.insert("tag_size".into(), json!(p.tag_size));
                    fields.insert("value".into(), self.json_value(&p.value));
                    Json::Object(fields)
                })
                .collect(),
        )
    }

    /// A value as JSON: numbers, booleans and strings as themselves; an
    /// object as its reference (or `null`); vectors, rotators and colours as
    /// objects of their fields; a tagged struct as an array of properties,
    /// and an array of them as an array of those; other arrays and raw values as `{"length": .., "bytes": "0a0b.."}`.
    fn json_value(&self, value: &Value) -> Json {
        let object = |object: ObjectRef| match object {
            ObjectRef::Null => Json::Null,
            _ => json!(self.package.reference(object)),
        };
        match value {
            Value::Byte(v) => json!(v),
            Value::Int(v) => json!(v),
            Value::Bool(v) => json!(v),
            Value::Float(v) => float(*v),
            Value::Object(o) => object(*o),
            Value::Name(name) => json!(name),
            Value::Str(text) => json!(text),
            Value::Delegate {
                object: o,
                function,
            } => json!({ "object": object(*o), "function": function }),
            Value::Vector { x, y, z } => json!({ "X": float(*x), "Y": float(*y), "Z": float(*z) }),
            Value::Rotator { pitch, yaw, roll } => {
                json!({ "Pitch": pitch, "Yaw": yaw, "Roll": roll })
            }
            Value::Color { r, g, b, a } => json!({ "B": b, "G": g, "R": r, "A": a }),
            Value::Struct(properties) => self.json_properties(properties),
            Value::StructArray(elements) => Json::Array(
                elements
                    .iter()
                    .map(|element| self.json_properties(element))
                    .collect(),
            ),
            Value::Array { len, data } => json!({ "length": len, "bytes": hex_compact(data) }),
            Value::Raw(bytes) => json!({ "bytes": hex_compact(bytes) }),
        }
    }
}

/// An object as T3D: a comment with its reference, then a `Begin Object`
/// block, or a comment saying why there isn't one (a class).
pub(super) fn write_t3d(
    package: &Package,
    object: &Object,
    style: Style,
    out: &mut impl Write,
) -> io::Result<()> {
    let mut comment = format!("// {}", object.reference);
    if let Some(size) = object.native_size().filter(|&size| size > 0) {
        comment.push_str(&format!(
            " ({} bytes of native data)",
            separated(size as u64)
        ));
    }
    writeln!(out, "{}", style.paint(DIM, &comment))?;
    if let Some(error @ Error::Unsupported(_)) = &object.error {
        writeln!(out, "{}", style.paint(DIM, &format!("// {}", error)))?;
        return Ok(());
    }
    let export = &package.exports()[object.export];
    writeln!(
        out,
        "Begin Object Class={} Name={}",
        package.class_name(export),
        package.name(export.object_name)
    )?;
    for property in &object.properties {
        writeln!(
            out,
            "    {}={}",
            property.t3d_name(),
            package.t3d_value(&property.value)
        )?;
    }
    if let Some(error) = &object.error {
        writeln!(out, "    {}", style.paint(DIM, &format!("// {}", error)))?;
    }
    writeln!(out, "End Object")
}

/// A float as a JSON number, in its shortest form (`0.1`, not
/// `0.10000000149011612`); `null` if it isn't finite.
fn float(value: f32) -> Json {
    value
        .to_string()
        .parse::<f64>()
        .ok()
        .and_then(serde_json::Number::from_f64)
        .map_or(Json::Null, Json::Number)
}

/// Bytes in hexadecimal, separated by spaces, up to 32 of them.
fn hex(bytes: &[u8]) -> String {
    const SHOWN: usize = 32;
    let mut text: Vec<String> = bytes
        .iter()
        .take(SHOWN)
        .map(|b| format!("{:02X}", b))
        .collect();
    if bytes.len() > SHOWN {
        text.push("...".to_string());
    }
    text.join(" ")
}

/// Bytes in hexadecimal, all of them, not separated.
fn hex_compact(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}
