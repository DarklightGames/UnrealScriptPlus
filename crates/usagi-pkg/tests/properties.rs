mod common;

use common::{ExportBytes, ImportBytes, PackageBytes, SummaryBytes, index, name};
use usagi_pkg::{Error, ObjectRef, Package, Property, PropertyType, Value};

const NAMES: &[&str] = &[
    "None",
    "Core",
    "Package",
    "Class",
    "Engine",
    "Texture",
    "MyTex",
    "Format",
    "USize",
    "Scale",
    "Vector",
    "Rotator",
    "Color",
    "Plane",
    "Region",
    "Zone",
    "Materials",
    "Material",
    "bMasked",
    "Tag",
    "OnClick",
    "Pos",
    "MyClass",
    "Empty",
    "Actor",
    "W",
    "Stack",
];

// Property types (`Info & 0x0F`).
const BYTE: u8 = 1;
const INT: u8 = 2;
const BOOL: u8 = 3;
const FLOAT: u8 = 4;
const OBJECT: u8 = 5;
const NAME: u8 = 6;
const DELEGATE: u8 = 7;
const CLASS: u8 = 8;
const ARRAY: u8 = 9;
const STRUCT: u8 = 10;
const VECTOR: u8 = 11;
const ROTATOR: u8 = 12;
const STR: u8 = 13;
const MAP: u8 = 14;

/// The index of a name in [`NAMES`].
fn n(text: &str) -> i32 {
    NAMES.iter().position(|&n| n == text).expect("a name") as i32
}

/// A property tag as the engine writes it: the size in the info byte if
/// it's 1, 2, 4, 12 or 16, otherwise after it, in a byte, a word or an
/// int; then the array index if it isn't 0 (or, for a bool, `array_bit`).
fn tag(name: &str, kind: u8, struct_name: Option<&str>, size: usize, array_index: u32) -> Vec<u8> {
    tag_with(name, kind, struct_name, size, array_index, false)
}

fn tag_with(
    name: &str,
    kind: u8,
    struct_name: Option<&str>,
    size: usize,
    array_index: u32,
    array_bit: bool,
) -> Vec<u8> {
    let mut out = index(n(name));
    let size_bits = match size {
        1 => 0x00,
        2 => 0x10,
        4 => 0x20,
        12 => 0x30,
        16 => 0x40,
        s if s <= 255 => 0x50,
        s if s <= 65535 => 0x60,
        _ => 0x70,
    };
    let array = array_index != 0 || array_bit;
    out.push(kind | size_bits | if array { 0x80 } else { 0 });
    if let Some(struct_name) = struct_name {
        out.extend(index(n(struct_name)));
    }
    match size_bits {
        0x50 => out.push(size as u8),
        0x60 => out.extend((size as u16).to_le_bytes()),
        0x70 => out.extend((size as i32).to_le_bytes()),
        _ => {}
    }
    if array_index != 0 {
        let i = array_index;
        if i <= 127 {
            out.push(i as u8);
        } else if i <= 16383 {
            out.extend([(i >> 8) as u8 | 0x80, i as u8]);
        } else {
            out.extend([
                (i >> 24) as u8 | 0xC0,
                (i >> 16) as u8,
                (i >> 8) as u8,
                i as u8,
            ]);
        }
    }
    out
}

/// A property: its tag, then its value.
fn prop(name: &str, kind: u8, value: &[u8]) -> Vec<u8> {
    let mut out = tag(name, kind, None, value.len(), 0);
    out.extend(value);
    out
}

/// A struct property whose value is `body` (its properties and `None`),
/// with the size `size` in its tag.
fn struct_prop(name: &str, struct_name: &str, body: &[u8], size: usize) -> Vec<u8> {
    let mut out = tag(name, STRUCT, Some(struct_name), size, 0);
    out.extend(body);
    out
}

fn none() -> Vec<u8> {
    index(0)
}

fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

fn ints(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

const PUBLIC: u32 = 0x00070004;
const HAS_STACK: u32 = 0x02000000;

/// A package whose first export, `MyPkg.MyTex`, a texture, has `data`. Its
/// second export is a class, and its third has no data.
fn package_with(data: Vec<u8>, flags: u32, licensee: u32) -> Package {
    let bytes = PackageBytes {
        summary: SummaryBytes {
            file_version: 128 | licensee << 16,
            ..SummaryBytes::default()
        },
        names: NAMES.iter().map(|text| name(text)).collect(),
        imports: vec![
            ImportBytes {
                class_package: n("Core"),
                class_name: n("Package"),
                outer: 0,
                object_name: n("Core"),
            },
            ImportBytes {
                class_package: n("Core"),
                class_name: n("Package"),
                outer: 0,
                object_name: n("Engine"),
            },
            ImportBytes {
                class_package: n("Core"),
                class_name: n("Class"),
                outer: -2,
                object_name: n("Texture"),
            },
        ],
        exports: vec![
            ExportBytes {
                class: -3,
                object_name: n("MyTex"),
                flags,
                size: data.len() as i32,
                ..ExportBytes::default()
            },
            ExportBytes {
                super_struct: -3,
                object_name: n("MyClass"),
                size: 4,
                ..ExportBytes::default()
            },
            ExportBytes {
                class: -3,
                object_name: n("Empty"),
                size: 0,
                ..ExportBytes::default()
            },
        ],
        data: vec![data],
    }
    .bytes();
    let mut package = Package::from_bytes(&bytes).unwrap();
    package.name = "MyPkg".to_string();
    package
}

fn package(data: Vec<u8>) -> Package {
    package_with(data, PUBLIC, 29)
}

/// The first export's properties, which must all read.
fn read(package: &Package) -> Vec<Property> {
    package.properties(0).unwrap().map(Result::unwrap).collect()
}

/// The value of the only property in `data`, followed by `None`.
fn value_of(mut data: Vec<u8>) -> Value {
    data.extend(none());
    let properties = read(&package(data));
    assert_eq!(properties.len(), 1, "{:?}", properties);
    properties.into_iter().next().unwrap().value
}

/// An error reading the first export's properties.
fn error(data: Vec<u8>) -> String {
    let package = package(data);
    let mut properties = package.properties(0).unwrap();
    let error = properties.find_map(Result::err).expect("an error");
    assert!(properties.next().is_none(), "reading stops at an error");
    error.to_string()
}

#[test]
fn simple_values() {
    assert_eq!(value_of(prop("Format", BYTE, &[5])), Value::Byte(5));
    assert_eq!(
        value_of(prop("USize", INT, &ints(&[-256]))),
        Value::Int(-256)
    );
    assert_eq!(
        value_of(prop("Scale", FLOAT, &floats(&[0.5]))),
        Value::Float(0.5)
    );
    assert_eq!(
        value_of(prop("Tag", NAME, &index(n("Actor")))),
        Value::Name("Actor".to_string())
    );
    let mut text = index(4);
    text.extend(b"abc\0");
    assert_eq!(
        value_of(prop("Tag", STR, &text)),
        Value::Str("abc".to_string())
    );
    assert_eq!(
        value_of(prop("Pos", VECTOR, &floats(&[1.0, 2.0, 3.0]))),
        Value::Vector {
            x: 1.0,
            y: 2.0,
            z: 3.0
        }
    );
    assert_eq!(
        value_of(prop("Pos", ROTATOR, &ints(&[1, -2, 3]))),
        Value::Rotator {
            pitch: 1,
            yaw: -2,
            roll: 3
        }
    );
}

#[test]
fn bools_are_in_the_tag() {
    let mut data = tag_with("bMasked", BOOL, None, 0, 0, true);
    data.extend(tag_with("bMasked", BOOL, None, 0, 0, false));
    data.extend(none());
    let properties = read(&package(data));
    let values: Vec<&Value> = properties.iter().map(|p| &p.value).collect();
    assert_eq!(values, [&Value::Bool(true), &Value::Bool(false)]);
    assert_eq!((properties[0].size, properties[0].tag_size), (0, 0));
    // A bool's array bit is its value, so it has no array index.
    assert_eq!(properties[0].array_index, 0);
}

#[test]
fn object_references() {
    assert_eq!(
        value_of(prop("Material", OBJECT, &index(-3))),
        Value::Object(ObjectRef::Import(2))
    );
    assert_eq!(
        value_of(prop("Material", CLASS, &index(2))),
        Value::Object(ObjectRef::Export(1))
    );
    assert_eq!(
        value_of(prop("Material", OBJECT, &index(0))),
        Value::Object(ObjectRef::Null)
    );
    // Out of range: kept as bytes.
    assert_eq!(
        value_of(prop("Material", OBJECT, &index(9))),
        Value::Raw(index(9))
    );
    assert_eq!(
        value_of(prop("Material", OBJECT, &index(-9))),
        Value::Raw(index(-9))
    );
    let mut delegate = index(1);
    delegate.extend(index(n("OnClick")));
    assert_eq!(
        value_of(prop("OnClick", DELEGATE, &delegate)),
        Value::Delegate {
            object: ObjectRef::Export(0),
            function: "OnClick".to_string()
        }
    );
}

#[test]
fn sizes_and_array_indices() {
    // A size in a byte, a word and an int, and array indices of one, two
    // and four bytes.
    let mut text = index(300);
    text.extend(vec![b'a'; 299]);
    text.push(0);
    let mut long = index(70000);
    long.extend(vec![b'b'; 69999]);
    long.push(0);
    let mut data = tag("Tag", STR, None, 5, 7);
    data.extend(index(4));
    data.extend(b"abc\0");
    data.extend(tag("Tag", STR, None, text.len(), 300));
    data.extend(&text);
    data.extend(tag("Tag", STR, None, long.len(), 20000));
    data.extend(&long);
    data.extend(prop("USize", INT, &ints(&[1])));
    data.extend(none());
    let properties = read(&package(data.clone()));
    let indices: Vec<u32> = properties.iter().map(|p| p.array_index).collect();
    assert_eq!(indices, [7, 300, 20000, 0]);
    let sizes: Vec<usize> = properties.iter().map(|p| p.size).collect();
    assert_eq!(sizes, [5, text.len(), long.len(), 4]);
    assert_eq!(properties[0].t3d_name(), "Tag(7)");
    assert_eq!(properties[3].t3d_name(), "USize");
    // Offsets are in the export's data.
    assert_eq!(properties[0].offset, 0);
    assert_eq!(properties[0].value_offset, 4);
    assert_eq!(properties[3].offset, data.len() - 1 - 6);
    // 12 and 16 bytes in the info byte.
    assert_eq!(
        value_of(prop("Tag", MAP, &[0; 16])),
        Value::Raw(vec![0; 16])
    );
}

#[test]
fn values_that_cant_be_read_are_kept_as_bytes() {
    // A size that doesn't fit the type, and types whose layout isn't stored.
    assert_eq!(
        value_of(prop("USize", INT, &[1, 2])),
        Value::Raw(vec![1, 2])
    );
    assert_eq!(value_of(prop("USize", INT, &[])), Value::Raw(vec![]));
    assert_eq!(value_of(prop("Tag", MAP, &[1])), Value::Raw(vec![1]));
    assert_eq!(value_of(prop("Tag", 15, &[1])), Value::Raw(vec![1]));
    assert_eq!(value_of(prop("Tag", 0, &[1])), Value::Raw(vec![1]));
    assert_eq!(
        value_of(prop("Tag", NAME, &index(99))),
        Value::Raw(index(99))
    );
    assert_eq!(value_of(prop("Tag", STR, &index(9))), Value::Raw(index(9)));
    assert_eq!(
        value_of(prop("Pos", VECTOR, &[0; 8])),
        Value::Raw(vec![0; 8])
    );
    assert_eq!(
        value_of(prop("Pos", ROTATOR, &[0; 8])),
        Value::Raw(vec![0; 8])
    );
    // Nothing at all.
    for kind in [BYTE, FLOAT, OBJECT, VECTOR, ROTATOR] {
        assert_eq!(value_of(prop("Pos", kind, &[])), Value::Raw(vec![]));
    }
    let mut delegate = index(9);
    delegate.extend(index(n("OnClick")));
    assert_eq!(
        value_of(prop("OnClick", DELEGATE, &delegate)),
        Value::Raw(delegate)
    );
    let mut delegate = index(1);
    delegate.extend(index(99));
    assert_eq!(
        value_of(prop("OnClick", DELEGATE, &delegate)),
        Value::Raw(delegate)
    );
}

#[test]
fn binary_structs() {
    let vector = floats(&[1.0, 2.0, 3.0]);
    assert_eq!(
        value_of(struct_prop("Pos", "Vector", &vector, 12)),
        Value::Vector {
            x: 1.0,
            y: 2.0,
            z: 3.0
        }
    );
    assert_eq!(
        value_of(struct_prop("Pos", "Rotator", &ints(&[4, 5, 6]), 12)),
        Value::Rotator {
            pitch: 4,
            yaw: 5,
            roll: 6
        }
    );
    // `var byte B, G, R, A`.
    assert_eq!(
        value_of(struct_prop("Pos", "Color", &[1, 2, 3, 4], 4)),
        Value::Color {
            b: 1,
            g: 2,
            r: 3,
            a: 4
        }
    );
    assert_eq!(
        value_of(struct_prop("Pos", "Color", &[1, 2], 2)),
        Value::Raw(vec![1, 2])
    );
    assert_eq!(
        value_of(struct_prop("Pos", "Rotator", &[0; 4], 4)),
        Value::Raw(vec![0; 4])
    );
    assert_eq!(
        value_of(struct_prop("Pos", "Vector", &[0; 4], 4)),
        Value::Raw(vec![0; 4])
    );
}

/// `Region=(Zone=Texture'MyPkg.MyTex',Format=3)`, as tagged properties.
fn region_body() -> Vec<u8> {
    let mut body = prop("Zone", OBJECT, &index(1));
    body.extend(prop("Format", BYTE, &[3]));
    body.extend(none());
    body
}

#[test]
fn tagged_structs() {
    let body = region_body();
    let value = value_of(struct_prop("Region", "Plane", &body, body.len()));
    let Value::Struct(fields) = &value else {
        panic!("a struct: {:?}", value);
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].value, Value::Object(ObjectRef::Export(0)));
    // Offsets of a struct's properties are in the export's data too.
    let header = tag("Region", STRUCT, Some("Plane"), body.len(), 0).len();
    assert_eq!(fields[0].offset, header);
    let package = package(vec![]);
    assert_eq!(
        package.t3d_value(&value),
        "(Zone=Texture'MyPkg.MyTex',Format=3)"
    );
}

#[test]
fn tagged_structs_are_read_by_their_tags() {
    // The engine can save a struct with a size that's too small; it's read
    // by its tags, and the properties after it are still read.
    let body = region_body();
    let mut data = struct_prop("Region", "Plane", &body, body.len() - 1);
    data.extend(prop("USize", INT, &ints(&[64])));
    data.extend(none());
    let properties = read(&package(data));
    assert!(matches!(properties[0].value, Value::Struct(_)));
    assert_eq!(properties[0].size, body.len());
    assert_eq!(properties[0].tag_size, body.len() - 1);
    assert_eq!(properties[1].value, Value::Int(64));
}

#[test]
fn structs_before_licensee_version_28_are_binary() {
    let body = region_body();
    let mut data = struct_prop("Region", "Plane", &body, body.len());
    data.extend(none());
    let properties = read(&package_with(data, PUBLIC, 27));
    assert_eq!(properties[0].value, Value::Raw(body));
}

#[test]
fn a_struct_that_doesnt_read_as_tags_is_kept_as_bytes() {
    // Its first tag's name is out of range.
    let body = [50, 0];
    assert_eq!(
        value_of(struct_prop("Region", "Plane", &body, 2)),
        Value::Raw(body.to_vec())
    );
}

#[test]
fn structs_are_read_32_deep() {
    // Each struct holds the next; the innermost holds an int.
    let mut body = prop("USize", INT, &ints(&[1]));
    body.extend(none());
    for _ in 0..33 {
        let mut outer = struct_prop("Region", "Plane", &body, body.len());
        outer.extend(none());
        body = outer;
    }
    let mut value = value_of(body);
    let mut depth = 0;
    while let Value::Struct(fields) = value {
        value = fields.into_iter().next().unwrap().value;
        depth += 1;
    }
    assert_eq!(depth, 32);
    assert!(matches!(value, Value::Raw(_)));
}

/// An array of structs, with `size` in its tag, then `after`.
fn struct_array(elements: &[Vec<u8>], size: Option<usize>, after: &[u8]) -> Vec<Property> {
    let mut value = index(elements.len() as i32);
    for element in elements {
        value.extend(element);
    }
    let mut data = tag("Materials", ARRAY, None, size.unwrap_or(value.len()), 0);
    data.extend(&value);
    data.extend(after);
    data.extend(none());
    read(&package(data))
}

#[test]
fn arrays() {
    let mut ints_array = index(2);
    ints_array.extend(ints(&[1, 2]));
    assert_eq!(
        value_of(prop("Materials", ARRAY, &ints_array)),
        Value::Array {
            len: 2,
            data: ints(&[1, 2])
        }
    );
    assert_eq!(
        value_of(prop("Materials", ARRAY, &index(0))),
        Value::Array {
            len: 0,
            data: vec![]
        }
    );
    assert_eq!(
        value_of(prop("Materials", ARRAY, &index(-1))),
        Value::Raw(index(-1))
    );
    assert_eq!(value_of(prop("Materials", ARRAY, &[])), Value::Raw(vec![]));
}

#[test]
fn arrays_of_structs() {
    let element = region_body();
    let properties = struct_array(&[element.clone(), element.clone()], None, &[]);
    let Value::StructArray(elements) = &properties[0].value else {
        panic!("an array of structs: {:?}", properties[0].value);
    };
    assert_eq!(elements.len(), 2);
    assert_eq!(elements[1][1].value, Value::Byte(3));
    let package = package(vec![]);
    assert_eq!(
        package.t3d_value(&properties[0].value),
        "((Zone=Texture'MyPkg.MyTex',Format=3),(Zone=Texture'MyPkg.MyTex',Format=3))"
    );
}

#[test]
fn arrays_of_structs_with_a_wrong_size() {
    // The size is too small, and no tag starts where it says: the elements
    // are read by their tags, and so is the property after them.
    let element = region_body();
    let after = prop("USize", INT, &ints(&[64]));
    let size = 1 + 2 * element.len() - 2;
    let properties = struct_array(&[element.clone(), element.clone()], Some(size), &after);
    assert!(matches!(properties[0].value, Value::StructArray(_)));
    assert_eq!(
        (properties[0].size, properties[0].tag_size),
        (size + 2, size)
    );
    assert_eq!(properties[1].value, Value::Int(64));
}

#[test]
fn arrays_that_arent_structs() {
    let element = region_body();
    // A `None` where the size says isn't trusted: it may be the end of the
    // last element (Darkest Hour has such sizes, which the engine ignores).
    let properties = struct_array(std::slice::from_ref(&element), Some(element.len()), &[]);
    assert!(matches!(properties[0].value, Value::StructArray(_)));
    // An empty element, or a property of type 0: not structs.
    let properties = struct_array(&[none()], None, &[]);
    assert!(matches!(properties[0].value, Value::Array { len: 1, .. }));
    let properties = struct_array(&[[prop("Tag", 0, &[1]), none()].concat()], None, &[]);
    assert!(matches!(properties[0].value, Value::Array { len: 1, .. }));
    // An element that doesn't read as tags.
    let properties = struct_array(&[vec![50]], None, &[]);
    assert!(matches!(properties[0].value, Value::Array { len: 1, .. }));
}

#[test]
fn what_counts_as_a_tag_after_an_array() {
    // The size says the array is 3 bytes longer than its elements. They're
    // structs only if a tag starts after them, and no property's tag 3 bytes
    // later.
    let element = region_body();
    let tag_ = n("Tag") as u8;
    let plane = n("Plane") as u8;
    for (after, is_struct) in [
        // A name out of range 3 bytes later; `None` after the elements. A
        // `None` 3 bytes later doesn't count.
        (vec![0, 0, 0, 50], true),
        (vec![0, 0, 0, 0], true),
        (vec![0, 0, 0, tag_, INT], false),
        // Type 0.
        (vec![0, 0, 0, tag_, 0], true),
        (vec![0, 0, 0, tag_, STRUCT, plane], false),
        (vec![0, 0, 0, tag_, STRUCT, 50], true),
        // Cut short.
        (vec![0, 0, 0, tag_], true),
        // No tag after the elements either.
        (vec![50, 0, 0, 50], false),
    ] {
        let mut data = tag("Materials", ARRAY, None, 1 + element.len() + 3, 0);
        data.extend(index(1));
        data.extend(&element);
        data.extend(&after);
        let package = package(data);
        let property = package.properties(0).unwrap().next().unwrap().unwrap();
        assert_eq!(
            matches!(property.value, Value::StructArray(_)),
            is_struct,
            "{:?}: {:?}",
            after,
            property.value
        );
    }
}

#[test]
fn state_frames_come_first() {
    // Node, StateNode, ProbeMask, LatentAction, and the code offset if
    // there's a Node.
    for (node, offset) in [(0, vec![]), (1, index(5))] {
        let mut data = index(node);
        data.extend(index(0));
        data.extend([0; 8]);
        data.extend(ints(&[0]));
        data.extend(&offset);
        let start = data.len();
        data.extend(prop("USize", INT, &ints(&[8])));
        data.extend(none());
        let package = package_with(data, PUBLIC | HAS_STACK, 29);
        let properties = package.properties(0).unwrap();
        assert_eq!(properties.start(), start);
        let properties: Vec<Property> = properties.map(Result::unwrap).collect();
        assert_eq!(properties[0].offset, start);
    }
    let package = package_with(vec![0, 0, 1], PUBLIC | HAS_STACK, 29);
    assert_eq!(
        package.properties(0).unwrap_err().to_string(),
        "invalid data at offset 2: the data ends in the state frame"
    );
    // Cut short in each of its fields.
    for data in [
        vec![0x40],
        vec![0, 0x40],
        [vec![0, 0], vec![0; 8], vec![0; 2]].concat(),
        [vec![1, 0], vec![0; 8], vec![0; 4], vec![0x40]].concat(),
    ] {
        let package = package_with(data, PUBLIC | HAS_STACK, 29);
        assert!(matches!(
            package.properties(0),
            Err(Error::InvalidData { .. })
        ));
    }
}

#[test]
fn where_properties_end() {
    let mut data = prop("USize", INT, &ints(&[8]));
    data.extend(none());
    let end = data.len();
    data.extend([1, 2, 3]);
    let package = package(data);
    let mut properties = package.properties(0).unwrap();
    assert_eq!(properties.start(), 0);
    assert!(properties.by_ref().all(|p| p.is_ok()));
    assert_eq!(properties.offset(), end);
}

#[test]
fn exports_without_properties() {
    let package = package(vec![]);
    // A class: its defaults come after its script, and this one's data is
    // too short to be a class (see tests/class.rs).
    assert!(matches!(
        package.properties(1),
        Err(Error::InvalidData { .. })
    ));
    // No data.
    let mut properties = package.properties(2).unwrap();
    assert!(properties.next().is_none());
    assert_eq!(properties.offset(), 0);
}

#[test]
fn errors() {
    assert_eq!(
        error(vec![50]),
        "invalid data at offset 0: name 50 is out of range: there are 27 names"
    );
    assert_eq!(
        error(prop("USize", INT, &ints(&[1]))),
        "invalid data at offset 6: the data ends in the property tag"
    );
    let mut data = index(n("USize"));
    data.extend([INT | 0x70]);
    data.extend(ints(&[-1]));
    assert_eq!(
        error(data),
        "invalid data at offset 2: property USize has a size of -1"
    );
    let mut data = tag("USize", INT, None, 4, 0);
    data.extend([1, 2]);
    assert_eq!(
        error(data),
        "invalid data at offset 2: the data ends in the property value"
    );
    let mut data = tag_with("bMasked", BOOL, None, 3, 0, false);
    data.push(0);
    assert_eq!(
        error(data),
        "invalid data at offset 3: the data ends in the property value"
    );
    // A struct cut short reads as bytes, which run out too.
    let mut data = tag("Region", STRUCT, Some("Plane"), 9, 0);
    data.extend(prop("USize", INT, &[]));
    assert_eq!(
        error(data),
        "invalid data at offset 4: the data ends in the property value"
    );
    for data in [
        vec![0x40],
        tag("Tag", NAME, None, 0, 200)[..3].to_vec(),
        tag("Tag", NAME, None, 0, 20000)[..4].to_vec(),
        tag("Tag", NAME, None, 0, 20000)[..5].to_vec(),
        tag("Tag", NAME, None, 0, 20000)[..6].to_vec(),
        tag("Region", STRUCT, Some("Plane"), 0, 0)[..2].to_vec(),
        vec![n("Tag") as u8, 0x60, 1],
        vec![n("Tag") as u8, 0x50],
        vec![n("Tag") as u8, 0x70, 1],
        vec![n("Tag") as u8],
        // An array with no bytes left for its length.
        tag("Materials", ARRAY, None, 0, 0),
    ] {
        assert!(error(data).contains("ends in the property tag"));
    }
}

#[test]
fn t3d_values() {
    let package = package(vec![]);
    let t3d = |value: Value| package.t3d_value(&value);
    assert_eq!(t3d(Value::Byte(3)), "3");
    assert_eq!(t3d(Value::Int(-3)), "-3");
    assert_eq!(t3d(Value::Bool(true)), "True");
    assert_eq!(t3d(Value::Bool(false)), "False");
    assert_eq!(t3d(Value::Float(0.25)), "0.250000");
    assert_eq!(
        t3d(Value::Object(ObjectRef::Import(2))),
        "Class'Engine.Texture'"
    );
    assert_eq!(t3d(Value::Object(ObjectRef::Null)), "None");
    assert_eq!(t3d(Value::Name("Actor".to_string())), "Actor");
    assert_eq!(t3d(Value::Str("a b".to_string())), "\"a b\"");
    assert_eq!(
        t3d(Value::Delegate {
            object: ObjectRef::Export(0),
            function: "OnClick".to_string()
        }),
        "MyTex.OnClick"
    );
    assert_eq!(
        t3d(Value::Delegate {
            object: ObjectRef::Null,
            function: "None".to_string()
        }),
        "None.None"
    );
    assert_eq!(
        t3d(Value::Vector {
            x: 1.0,
            y: -2.5,
            z: 0.0
        }),
        "(X=1.000000,Y=-2.500000,Z=0.000000)"
    );
    assert_eq!(
        t3d(Value::Rotator {
            pitch: 1,
            yaw: 2,
            roll: 3
        }),
        "(Pitch=1,Yaw=2,Roll=3)"
    );
    assert_eq!(
        t3d(Value::Color {
            r: 1,
            g: 2,
            b: 3,
            a: 4
        }),
        "(B=3,G=2,R=1,A=4)"
    );
    assert_eq!(t3d(Value::Struct(vec![])), "()");
    assert_eq!(t3d(Value::StructArray(vec![])), "()");
    assert_eq!(
        t3d(Value::Array {
            len: 1,
            data: vec![1]
        }),
        "<1 element, 1 byte>"
    );
    assert_eq!(
        t3d(Value::Array {
            len: 2,
            data: vec![1, 2]
        }),
        "<2 elements, 2 bytes>"
    );
    assert_eq!(t3d(Value::Raw(vec![])), "<0 bytes>");
    assert_eq!(t3d(Value::Raw(vec![0xAB])), "<1 byte: AB>");
    assert_eq!(
        t3d(Value::Raw((0..17).collect())),
        "<17 bytes: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F ...>"
    );
}

#[test]
fn property_types() {
    let names: Vec<&str> = (0..16).map(|id| PropertyType::from_id(id).name()).collect();
    assert_eq!(
        names,
        [
            "Unknown",
            "ByteProperty",
            "IntProperty",
            "BoolProperty",
            "FloatProperty",
            "ObjectProperty",
            "NameProperty",
            "DelegateProperty",
            "ClassProperty",
            "ArrayProperty",
            "StructProperty",
            "VectorProperty",
            "RotatorProperty",
            "StrProperty",
            "MapProperty",
            "FixedArrayProperty",
        ]
    );
    // Only the low four bits are the type.
    assert_eq!(PropertyType::from_id(0x52), PropertyType::Int);
}

#[test]
fn the_package_keeps_its_data() {
    let package = package(vec![1, 2, 3]);
    assert_eq!(package.export_data(0), [1, 2, 3]);
    assert_eq!(package.export_data(2), [] as [u8; 0]);
    // Printed as its length.
    assert!(format!("{:?}", package).contains("bytes: <"));
}

#[test]
fn references() {
    let package = package(vec![]);
    assert_eq!(
        package.full_path(ObjectRef::Export(0)).unwrap(),
        "MyPkg.MyTex"
    );
    assert_eq!(
        package.full_path(ObjectRef::Import(2)).unwrap(),
        "Engine.Texture"
    );
    assert_eq!(package.full_path(ObjectRef::Null), None);
    assert_eq!(
        package.reference(ObjectRef::Export(0)),
        "Texture'MyPkg.MyTex'"
    );
    assert_eq!(
        package.reference(ObjectRef::Export(1)),
        "Class'MyPkg.MyClass'"
    );
    assert_eq!(package.reference(ObjectRef::Import(0)), "Package'Core'");
    assert_eq!(package.reference(ObjectRef::Null), "None");
    assert_eq!(package.object_class_name(ObjectRef::Null), None);
}
