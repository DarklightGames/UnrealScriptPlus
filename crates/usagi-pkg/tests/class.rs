mod common;

use common::{ExportBytes, ImportBytes, PackageBytes, SummaryBytes, index, name};
use usagi_pkg::archive::{Archive, FString, Loader, NameRef, Saver, Serialize};
use usagi_pkg::script::{Expr, Script};
use usagi_pkg::{
    Class, Const, Dependency, Enum, Field, Function, Guid, ObjectRef, Package, PropertyDecl,
    PropertyKind, State, StateFrame, Struct, TextBuffer,
};

const NAMES: &[&str] = &[
    "None",
    "Core",
    "Package",
    "Class",
    "Function",
    "State",
    "Struct",
    "IntProperty",
    "MyClass",
    "Tick",
    "Idle",
    "Pair",
    "Count",
    "System",
    "Movement",
    "Enum",
    "Const",
    "TextBuffer",
    "Colors",
    "Max",
    "ScriptText",
    "Red",
];

/// The index of a name in [`NAMES`].
fn n(text: &str) -> i32 {
    NAMES.iter().position(|&n| n == text).expect("a name") as i32
}

fn le(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

/// A package at `version` and `licensee`, with imports of `Core.Class`,
/// `Core.Function` and so on (`-2` is `Class`, `-3` `Function`, ... in the
/// order of `classes`), and an export of each `(class, name, data)`.
fn package_at(version: u32, licensee: u32, exports: &[(i32, &str, Vec<u8>)]) -> Package {
    let classes = [
        "Class",
        "Function",
        "State",
        "Struct",
        "IntProperty",
        "Enum",
        "Const",
        "TextBuffer",
    ];
    let mut imports = vec![ImportBytes {
        class_package: n("Core"),
        class_name: n("Package"),
        outer: 0,
        object_name: n("Core"),
    }];
    imports.extend(classes.iter().map(|class| ImportBytes {
        class_package: n("Core"),
        class_name: n("Class"),
        outer: -1,
        object_name: n(class),
    }));
    let bytes = PackageBytes {
        summary: SummaryBytes {
            file_version: version | licensee << 16,
            ..SummaryBytes::default()
        },
        names: NAMES.iter().map(|text| name(text)).collect(),
        imports,
        exports: exports
            .iter()
            .map(|(class, object_name, data)| ExportBytes {
                class: *class,
                object_name: n(object_name),
                size: data.len() as i32,
                ..ExportBytes::default()
            })
            .collect(),
        data: exports.iter().map(|(_, _, data)| data.clone()).collect(),
    }
    .bytes();
    let mut package = Package::from_bytes(&bytes).unwrap();
    package.name = "MyPkg".to_string();
    package
}

fn package(exports: &[(i32, &str, Vec<u8>)]) -> Package {
    package_at(128, 29, exports)
}

/// Reads a `T` from `bytes`, checks that it's all of them, that writing it
/// back gives them again, and that it can't be read from fewer.
fn round_trip<T: Serialize + std::fmt::Debug>(package: &Package, bytes: &[u8]) -> T {
    for len in 0..bytes.len() {
        let mut loader = Loader::new(package, &bytes[..len]);
        assert!(
            T::default().serialize(&mut loader).is_err(),
            "{} of {} bytes",
            len,
            bytes.len()
        );
    }
    let mut loader = Loader::new(package, bytes);
    let mut value = T::default();
    value.serialize(&mut loader).unwrap();
    assert_eq!(loader.pos(), bytes.len(), "{:?}", value);
    let mut saver = Saver::new(
        package.summary.version(),
        package.summary.licensee_version(),
    );
    value.serialize(&mut saver).unwrap();
    assert_eq!(saver.pos(), bytes.len());
    assert_eq!(saver.into_bytes(), bytes, "{:?}", value);
    value
}

/// Reads a declaration of `kind` from `bytes`, as [`round_trip`] does.
fn decl_round_trip(package: &Package, kind: PropertyKind, bytes: &[u8]) -> PropertyDecl {
    let empty = || PropertyDecl {
        kind: kind.clone(),
        ..PropertyDecl::default()
    };
    for len in 0..bytes.len() {
        let mut loader = Loader::new(package, &bytes[..len]);
        assert!(empty().serialize(&mut loader).is_err(), "{} bytes", len);
    }
    let mut decl = empty();
    let mut loader = Loader::new(package, bytes);
    decl.serialize(&mut loader).unwrap();
    assert_eq!(loader.pos(), bytes.len());
    let mut saver = Saver::new(128, 29);
    decl.serialize(&mut saver).unwrap();
    assert_eq!(saver.into_bytes(), bytes);
    decl
}

/// A `UStruct` with no fields and the script `return;` (2 bytes), at
/// licensee version 29: its `StructFlags` are there.
fn struct_bytes(friendly_name: &str) -> Vec<u8> {
    let mut out = vec![0, 0]; // SuperField, Next
    out.extend([0, 0]); // ScriptText, Children
    out.extend(index(n(friendly_name)));
    out.extend(le(&[0x10])); // StructFlags
    out.extend(le(&[12, 34])); // Line, TextPos
    out.extend(le(&[2]));
    out.extend([0x04, 0x0B]);
    out
}

fn expected_struct(friendly_name: &str) -> Struct {
    Struct {
        field: Field::default(),
        script_text: ObjectRef::Null,
        children: ObjectRef::Null,
        friendly_name: NameRef(n(friendly_name)),
        struct_flags: Some(0x10),
        line: 12,
        text_pos: 34,
        script: Script {
            size: 2,
            exprs: vec![Expr::Return(Box::new(Expr::Nothing))],
        },
    }
}

/// `UState`'s fields after its struct.
fn state_tail() -> Vec<u8> {
    let mut out = 1u64.to_le_bytes().to_vec(); // ProbeMask
    out.extend(2u64.to_le_bytes()); // IgnoreMask
    out.extend(3u16.to_le_bytes()); // LabelTableOffset
    out.extend(le(&[4])); // StateFlags
    out
}

/// A class, up to its defaults, at version 128.
fn class_bytes() -> Vec<u8> {
    let mut out = struct_bytes("MyClass");
    out.extend(state_tail());
    out.extend(le(&[0x21])); // ClassFlags
    out.extend(le(&[1, 2, 3, 4])); // ClassGuid
    out.extend(index(1)); // Dependencies
    out.extend(index(-2)); // Class
    out.extend(le(&[1, 0xABCD]));
    out.extend(index(1)); // PackageImports
    out.extend(index(n("Core")));
    out.extend(index(-2)); // ClassWithin
    out.extend(index(n("System"))); // ClassConfigName
    out.extend(index(1)); // HideCategories
    out.extend(index(n("Movement")));
    out
}

#[test]
fn classes() {
    let mut data = class_bytes();
    let defaults = data.len();
    // The defaults: `Count=5`.
    data.extend(index(n("Count")));
    data.push(0x22);
    data.extend(le(&[5]));
    data.push(0);
    let package = package(&[(0, "MyClass", data.clone())]);
    let (class, end) = package.class(0).unwrap();
    assert_eq!(end, defaults);
    assert_eq!(
        class,
        Class {
            state: State {
                structure: expected_struct("MyClass"),
                probe_mask: 1,
                ignore_mask: 2,
                label_table_offset: 3,
                state_flags: 4,
            },
            old_class_record_size: None,
            flags: 0x21,
            guid: Guid {
                a: 1,
                b: 2,
                c: 3,
                d: 4,
            },
            dependencies: vec![Dependency {
                class: ObjectRef::Import(1),
                deep: 1,
                script_text_crc: 0xABCD,
            }],
            package_imports: vec![NameRef(n("Core"))],
            within: Some(ObjectRef::Import(1)),
            config_name: Some(NameRef(n("System"))),
            hide_categories: Some(vec![NameRef(n("Movement"))]),
        }
    );
    round_trip::<Class>(&package, &data[..defaults]);
    // Its tagged properties are its defaults.
    let properties: Vec<_> = package.properties(0).unwrap().map(Result::unwrap).collect();
    assert_eq!(properties.len(), 1);
    assert_eq!(properties[0].name, "Count");
    assert_eq!(package.structure(0).unwrap(), Some(class.state.structure));
}

#[test]
fn classes_in_old_packages() {
    // Version 61: the old class record size, and no `ClassWithin`,
    // `ClassConfigName` or `HideCategories`. Licensee version 0: no
    // `StructFlags`.
    let mut data = vec![0, 0, 0, 0];
    data.extend(index(n("MyClass")));
    data.extend(le(&[0, 0, 0]));
    data.extend(state_tail());
    data.extend(le(&[99])); // OldClassRecordSize
    data.extend(le(&[0, 0, 0, 0, 0]));
    data.extend([0, 0]);
    let package = package_at(61, 0, &[(0, "MyClass", Vec::new())]);
    let class: Class = round_trip(&package, &data);
    assert_eq!(class.old_class_record_size, Some(99));
    assert_eq!(class.state.structure.struct_flags, None);
    assert_eq!(class.within, None);
    assert_eq!(class.hide_categories, None);
    // Version 98: `ClassWithin` and `ClassConfigName`, but no
    // `HideCategories`.
    let mut data = struct_bytes("MyClass");
    data.extend(state_tail());
    data.extend(le(&[0, 0, 0, 0, 0]));
    data.extend([0, 0, 0, 0]);
    let package = package_at(98, 29, &[(0, "MyClass", Vec::new())]);
    let class: Class = round_trip(&package, &data);
    assert_eq!(class.within, Some(ObjectRef::Null));
    assert_eq!(class.config_name, Some(NameRef(0)));
    assert_eq!(class.hide_categories, None);
}

#[test]
fn functions_states_and_structs() {
    // A function, replicated: its replication offset follows its flags.
    let mut function = vec![0]; // its tagged properties: none
    function.extend(struct_bytes("Tick"));
    function.extend(7u16.to_le_bytes()); // iNative
    function.push(3); // OperPrecedence
    function.extend(le(&[0x40])); // FunctionFlags: FUNC_Net
    function.extend(9u16.to_le_bytes()); // RepOffset
    let mut state = vec![0];
    state.extend(struct_bytes("Idle"));
    state.extend(state_tail());
    let mut structure = vec![0];
    structure.extend(struct_bytes("Pair"));
    let package = package(&[
        (-3, "Tick", function.clone()),
        (-4, "Idle", state.clone()),
        (-5, "Pair", structure.clone()),
        (-6, "Count", vec![0]),
    ]);
    let function: Function = round_trip(&package, &function[1..]);
    assert_eq!(function.native, 7);
    assert_eq!(function.operator_precedence, 3);
    assert_eq!(function.rep_offset, Some(9));
    assert_eq!(function.parms_size, None);
    let state: State = round_trip(&package, &state[1..]);
    assert_eq!(state.structure, expected_struct("Idle"));
    round_trip::<Struct>(&package, &structure[1..]);
    for (export, name) in [(0, "Tick"), (1, "Idle"), (2, "Pair")] {
        assert_eq!(
            package.structure(export).unwrap(),
            Some(expected_struct(name))
        );
    }
    // A property isn't a struct.
    assert_eq!(package.structure(3).unwrap(), None);

    // Before version 64, a function also stores its parameters' size and
    // number, and its return value's offset. Not replicated: no offset.
    let mut data = vec![0, 0, 0, 0];
    data.extend(index(n("Tick")));
    data.extend(le(&[0, 0, 0]));
    data.extend(1u16.to_le_bytes()); // ParmsSize
    data.extend(2u16.to_le_bytes()); // iNative
    data.push(3); // NumParms
    data.push(4); // OperPrecedence
    data.extend(5u16.to_le_bytes()); // ReturnValueOffset
    data.extend(le(&[0])); // FunctionFlags
    let package = package_at(63, 0, &[]);
    let function: Function = round_trip(&package, &data);
    assert_eq!(
        (
            function.parms_size,
            function.num_parms,
            function.return_value_offset,
            function.rep_offset
        ),
        (Some(1), Some(3), Some(5), None)
    );
}

#[test]
fn properties_enums_constants_and_text() {
    let package = package(&[]);
    // An int property: replicated, with a comment.
    let mut data = vec![0, 0];
    data.extend(le(&[1, 0x0200_0020])); // ArrayDim, PropertyFlags
    data.extend(index(n("Movement"))); // Category
    data.extend(5u16.to_le_bytes()); // RepOffset
    data.extend([3, b'h', b'i', 0]); // CommentString
    let decl = decl_round_trip(&package, PropertyKind::Int, &data);
    assert_eq!(decl.rep_offset, Some(5));
    assert_eq!(
        decl.comment.as_ref().map(FString::text),
        Some("hi".to_string())
    );

    // Each kind of property, and what its class adds: object references,
    // and a static array's count.
    let base = [vec![0, 0], le(&[1, 0]), vec![0]].concat();
    for (class, extra, kind) in [
        (
            "ByteProperty",
            vec![0x82],
            PropertyKind::Byte {
                enumeration: ObjectRef::Import(1),
            },
        ),
        ("IntProperty", vec![], PropertyKind::Int),
        ("BoolProperty", vec![], PropertyKind::Bool),
        ("FloatProperty", vec![], PropertyKind::Float),
        (
            "ObjectProperty",
            vec![0x82],
            PropertyKind::Object {
                class: ObjectRef::Import(1),
            },
        ),
        (
            "ClassProperty",
            vec![0x82, 0x83],
            PropertyKind::Class {
                class: ObjectRef::Import(1),
                meta_class: ObjectRef::Import(2),
            },
        ),
        ("NameProperty", vec![], PropertyKind::Name),
        ("StrProperty", vec![], PropertyKind::Str),
        (
            "DelegateProperty",
            vec![0x82],
            PropertyKind::Delegate {
                function: ObjectRef::Import(1),
            },
        ),
        (
            "FixedArrayProperty",
            [vec![0x82], le(&[4])].concat(),
            PropertyKind::FixedArray {
                inner: ObjectRef::Import(1),
                count: 4,
            },
        ),
        (
            "ArrayProperty",
            vec![0x82],
            PropertyKind::Array {
                inner: ObjectRef::Import(1),
            },
        ),
        (
            "MapProperty",
            vec![0x82, 0x83],
            PropertyKind::Map {
                key: ObjectRef::Import(1),
                value: ObjectRef::Import(2),
            },
        ),
        (
            "StructProperty",
            vec![0x82],
            PropertyKind::Struct {
                structure: ObjectRef::Import(1),
            },
        ),
        ("PointerProperty", vec![], PropertyKind::Pointer),
    ] {
        let data = [base.clone(), extra].concat();
        let decl = decl_round_trip(&package, PropertyKind::of_class(class).unwrap(), &data);
        assert_eq!(decl.kind, kind, "{}", class);
    }
    assert_eq!(PropertyKind::of_class("Texture"), None);

    let mut data = vec![0, 0];
    data.extend(index(2));
    data.extend(index(n("Red")));
    data.extend(index(n("Max")));
    let colors: Enum = round_trip(&package, &data);
    assert_eq!(colors.names, [NameRef(n("Red")), NameRef(n("Max"))]);
    let constant: Const = round_trip(&package, &[0, 0, 2, b'7', 0]);
    assert_eq!(constant.value.text(), "7");
    let text: TextBuffer = round_trip(&package, &[le(&[1, 2]), vec![1, 0]].concat());
    assert_eq!(
        (text.pos, text.top, text.text.text()),
        (1, 2, String::new())
    );
}

#[test]
fn property_declarations_of_exports() {
    // After its tagged properties (none).
    let data = [vec![0, 0, 0], le(&[2, 0]), vec![0]].concat();
    let package = package(&[(-6, "Count", data), (-2, "MyClass", vec![0])]);
    let decl = package.property_decl(0).unwrap();
    assert_eq!((decl.kind, decl.array_dim), (PropertyKind::Int, 2));
    assert_eq!(
        package.property_decl(1).unwrap_err().to_string(),
        "invalid data at offset 0: Class'MyPkg.MyClass' isn't a property"
    );
    // Cut short.
    let package = package_with_short_property();
    assert_eq!(
        package.property_decl(0).unwrap_err().to_string(),
        "invalid data at offset 3: the data ends in the property"
    );
}

fn package_with_short_property() -> Package {
    package(&[(-6, "Count", vec![0, 0, 0])])
}

#[test]
fn classes_cut_short() {
    // In the config name, and in the hidden categories.
    let data = class_bytes();
    let hide = data.len() - 2;
    let config = hide - 1;
    for (end, what) in [(config, "class"), (hide, "class's hidden categories")] {
        let package = package(&[(0, "MyClass", data[..end].to_vec())]);
        assert_eq!(
            package.class(0).unwrap_err().to_string(),
            format!(
                "invalid data at offset {}: the data ends in the {}",
                end, what
            )
        );
    }
    // A property whose tagged properties are malformed.
    let package = package(&[(-6, "Count", vec![50])]);
    assert_eq!(
        package.property_decl(0).unwrap_err().to_string(),
        "invalid data at offset 0: name 50 is out of range: there are 22 names"
    );
}

#[test]
fn malformed_state_frames_and_tagged_properties() {
    // A class and a property with `RF_HasStack`, and a state frame cut
    // short.
    let bytes = PackageBytes {
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
                class_name: n("Class"),
                outer: -1,
                object_name: n("IntProperty"),
            },
        ],
        exports: vec![
            ExportBytes {
                object_name: n("MyClass"),
                flags: 0x0207_0004,
                size: 1,
                ..ExportBytes::default()
            },
            ExportBytes {
                class: -2,
                object_name: n("Count"),
                flags: 0x0207_0004,
                size: 1,
                ..ExportBytes::default()
            },
        ],
        data: vec![vec![0], vec![0]],
        ..PackageBytes::default()
    }
    .bytes();
    let stacked = Package::from_bytes(&bytes).unwrap();
    let message = "invalid data at offset 1: the data ends in the state frame";
    assert_eq!(stacked.class(0).unwrap_err().to_string(), message);
    assert_eq!(stacked.property_decl(1).unwrap_err().to_string(), message);

    // A state, struct and function whose tagged properties are malformed,
    // or whose data is cut short.
    for (class, name) in [(-4, "Idle"), (-5, "Pair"), (-3, "Tick")] {
        let package = package(&[(class, name, vec![50]), (class, name, vec![0, 0])]);
        assert_eq!(
            package.structure(0).unwrap_err().to_string(),
            "invalid data at offset 0: name 50 is out of range: there are 22 names"
        );
        assert_eq!(
            package.structure(1).unwrap_err().to_string(),
            "invalid data at offset 2: the data ends in the field"
        );
    }
}

#[test]
fn class_errors() {
    let package = package(&[(-2, "MyClass", vec![0]), (0, "Idle", vec![0, 0])]);
    assert_eq!(
        package.class(0).unwrap_err().to_string(),
        "invalid data at offset 0: Class'MyPkg.MyClass' isn't a class"
    );
    // Cut short: reported as invalid data, with the offset in the export's
    // data.
    assert_eq!(
        package.class(1).unwrap_err().to_string(),
        "invalid data at offset 2: the data ends in the struct"
    );
}

#[test]
fn state_frames() {
    // `RF_HasStack`: a state frame first. With a node, the offset in its
    // code follows.
    let mut data = vec![0x81, 0x81];
    data.extend(5u64.to_le_bytes());
    data.extend(le(&[6]));
    data.extend(index(-1));
    let frame: StateFrame = round_trip(&package(&[]), &data);
    assert_eq!(
        frame,
        StateFrame {
            node: ObjectRef::Import(0),
            state_node: ObjectRef::Import(0),
            probe_mask: 5,
            latent_action: 6,
            offset: Some(-1),
        }
    );
    let mut data = vec![0, 0];
    data.extend(0u64.to_le_bytes());
    data.extend(le(&[0]));
    let frame: StateFrame = round_trip(&package(&[]), &data);
    assert_eq!(frame.offset, None);

    let bytes = PackageBytes {
        names: NAMES.iter().map(|text| name(text)).collect(),
        exports: vec![
            ExportBytes {
                object_name: n("MyClass"),
                flags: 0x0207_0004,
                size: data.len() as i32 + 1,
                ..ExportBytes::default()
            },
            ExportBytes {
                object_name: n("Idle"),
                flags: 0x0207_0004,
                size: 0,
                ..ExportBytes::default()
            },
        ],
        data: vec![[data.clone(), vec![0]].concat()],
        ..PackageBytes::default()
    }
    .bytes();
    let package = Package::from_bytes(&bytes).unwrap();
    assert_eq!(package.state_frame(0).unwrap(), (Some(frame), data.len()));
    // No data: no state frame.
    assert_eq!(package.state_frame(1).unwrap(), (None, 0));
}

#[test]
fn strings() {
    let package = package(&[]);
    let latin1: FString = round_trip(&package, &[4, b'a', 0, b'b', 0]);
    assert_eq!(latin1, FString::Latin1(vec![b'a', 0, b'b', 0]));
    // The text ends at the first null, as in the engine.
    assert_eq!(latin1.text(), "a");
    let utf16: FString = round_trip(&package, &[0x83, 0xAC, 0x20, b'!', 0, 0, 0]);
    assert_eq!(utf16.text(), "\u{20AC}!");
    let empty: FString = round_trip(&package, &[0]);
    assert_eq!(empty, FString::default());
    assert_eq!(empty.text(), "");
}

#[test]
fn archives() {
    let package = package(&[]);
    // Compact indices, written as the engine writes them.
    for value in [
        0,
        1,
        -1,
        0x3f,
        0x40,
        -0x40,
        0x1fff,
        0x2000,
        0xfffff,
        0x100000,
        0x7ff_ffff,
        0x800_0000,
        i32::MAX,
        -i32::MAX,
    ] {
        let mut saver = Saver::new(128, 29);
        let mut v = value;
        saver.index(&mut v, "index").unwrap();
        let bytes = saver.into_bytes();
        assert_eq!(bytes, index(value), "{:#x}", value);
        let mut loader = Loader::new(&package, &bytes);
        let mut read = 0;
        loader.index(&mut read, "index").unwrap();
        assert_eq!(read, value);
    }
    // A negative count.
    let mut loader = Loader::new(&package, &[0x81]);
    let mut names: Vec<NameRef> = Vec::new();
    assert_eq!(
        (&mut loader as &mut dyn Archive)
            .array_of(&mut names, "list")
            .unwrap_err()
            .to_string(),
        "invalid data at offset 0: the list has -1 items"
    );
    // Object references, in an array.
    let mut loader = Loader::new(&package, &[2, 0, 0x81]);
    let mut objects: Vec<ObjectRef> = Vec::new();
    (&mut loader as &mut dyn Archive)
        .array_of(&mut objects, "list")
        .unwrap();
    assert_eq!(objects, [ObjectRef::Null, ObjectRef::Import(0)]);
    let mut saver = Saver::new(128, 29);
    (&mut saver as &mut dyn Archive)
        .array_of(&mut objects, "list")
        .unwrap();
    assert_eq!(saver.into_bytes(), [2, 0, 0x81]);
    // What a saver says about itself.
    let saver = Saver::new(61, 3);
    assert!(!saver.is_loading());
    assert_eq!((saver.version(), saver.licensee_version()), (61, 3));
    assert_eq!(saver.peek(1), None);
    assert!(!saver.is_none_name(0));
    let loader = Loader::new(&package, &[1, 2]);
    assert!(loader.is_loading());
    assert_eq!(loader.peek(2), Some(&[1u8, 2][..]));
    assert_eq!(loader.peek(3), None);
    // A length that overflows.
    let mut loader = Loader::new(&package, &[1, 2]);
    loader.u8(&mut 0, "byte").unwrap();
    assert_eq!(loader.peek(usize::MAX), None);
    // Floats.
    let mut saver = Saver::new(128, 29);
    let mut value = 2.5f32;
    saver.f32(&mut value, "float").unwrap();
    assert_eq!(saver.into_bytes(), 2.5f32.to_le_bytes());
}

#[test]
fn reading_native_data_reports_invalid_data() {
    let package = package_with_short_property();
    // The data ends in the struct: reported with its offset in the data.
    assert_eq!(
        package.read_native::<Field>(0, 2).unwrap_err().to_string(),
        "invalid data at offset 3: the data ends in the field"
    );
    assert_eq!(
        package.read_native::<Field>(0, 0).unwrap(),
        (Field::default(), 2)
    );
}
