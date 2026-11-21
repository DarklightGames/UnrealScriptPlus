mod common;

use common::{ExportBytes, ImportBytes, PackageBytes, SummaryBytes, index, name};
use usagi_pkg::{Error, ObjectFlags, ObjectRef, Package};

/// Names, imports and exports like a small real package's:
///
/// ```text
/// -1  Core.Package  Core
/// -2  Core.Package  Engine
/// -3  Core.Class    Engine.Texture
/// -4  Core.Class    Core.Package
/// -5  Core.Class    Engine.Actor
///  1  Class         MyClass        (extends Engine.Actor)
///  2  Package       MyGroup
///  3  Texture       MyGroup.MyTex
///  4  Class         None           (a placeholder with no data)
/// ```
fn sample() -> PackageBytes {
    let names = [
        "None", "Core", "Package", "Class", "Engine", "Texture", "Object", "MyGroup", "MyTex",
        "MyClass", "Actor",
    ];
    let import = |class_name, outer, object_name| ImportBytes {
        class_package: 1,
        class_name,
        outer,
        object_name,
    };
    PackageBytes {
        summary: SummaryBytes::default(),
        names: names.iter().map(|n| name(n)).collect(),
        imports: vec![
            import(2, 0, 1),
            import(2, 0, 4),
            import(3, -2, 5),
            import(3, -1, 2),
            import(3, -2, 10),
        ],
        exports: vec![
            ExportBytes {
                super_struct: -5,
                object_name: 9,
                flags: 0x040F0004,
                size: 40,
                ..ExportBytes::default()
            },
            ExportBytes {
                class: -4,
                object_name: 7,
                ..ExportBytes::default()
            },
            ExportBytes {
                class: -3,
                outer: 2,
                object_name: 8,
                size: 300,
                ..ExportBytes::default()
            },
            ExportBytes {
                flags: 0,
                size: 0,
                ..ExportBytes::default()
            },
        ],
        data: Vec::new(),
    }
}

fn read(package: &PackageBytes) -> Result<Package, Error> {
    Package::from_bytes(&package.bytes())
}

fn invalid(package: &PackageBytes) -> String {
    match read(package) {
        Err(Error::InvalidTable(message)) => message,
        other => panic!("expected an invalid table, got {:?}", other),
    }
}

#[test]
fn reads_the_tables() {
    let bytes = sample().bytes();
    let package = Package::from_bytes(&bytes).unwrap();

    assert_eq!(package.names().len(), 11);
    assert_eq!(package.names()[8].name, "MyTex");
    assert_eq!(package.names()[8].flags, 0x00070010);
    assert_eq!(package.name(9), "MyClass");

    let imports = package.imports();
    assert_eq!(imports.len(), 5);
    let texture = &imports[2];
    assert_eq!(package.name(texture.class_package), "Core");
    assert_eq!(package.name(texture.class_name), "Class");
    assert_eq!(texture.outer, ObjectRef::Import(1));
    assert_eq!(package.name(texture.object_name), "Texture");
    assert_eq!(imports[0].outer, ObjectRef::Null);

    let exports = package.exports();
    assert_eq!(exports.len(), 4);
    let class = &exports[0];
    assert_eq!(class.class, ObjectRef::Null);
    assert_eq!(class.super_struct, ObjectRef::Import(4));
    assert_eq!(class.outer, ObjectRef::Null);
    assert_eq!(class.flags, ObjectFlags(0x040F0004));
    assert_eq!(class.serial_size, 40);
    // The data comes right after the 64-byte summary and the names.
    let names_len: usize = sample().names.iter().map(Vec::len).sum();
    assert_eq!(class.serial_offset, Some(64 + names_len as u32));
    assert_eq!(exports[2].serial_offset, Some(64 + names_len as u32 + 41));
    assert_eq!(exports[3].serial_size, 0);
    assert_eq!(exports[3].serial_offset, None);
}

#[test]
fn resolves_names_and_paths() {
    let package = read(&sample()).unwrap();
    let exports = package.exports();
    assert_eq!(package.path(ObjectRef::Import(0)).unwrap(), "Core");
    assert_eq!(
        package.path(ObjectRef::Import(2)).unwrap(),
        "Engine.Texture"
    );
    assert_eq!(package.path(ObjectRef::Export(0)).unwrap(), "MyClass");
    assert_eq!(package.path(ObjectRef::Export(2)).unwrap(), "MyGroup.MyTex");
    assert_eq!(package.path(ObjectRef::Export(3)).unwrap(), "None");
    assert_eq!(package.path(ObjectRef::Null), None);
    assert_eq!(package.object_name(ObjectRef::Export(1)), Some("MyGroup"));
    assert_eq!(package.object_name(ObjectRef::Null), None);
    assert_eq!(package.class_name(&exports[0]), "Class");
    assert_eq!(package.class_name(&exports[1]), "Package");
    assert_eq!(package.class_name(&exports[2]), "Texture");
}

#[test]
fn an_export_can_be_inside_an_import() {
    // The writer never does this, but the engine's loader accepts it.
    let mut package = sample();
    package.exports[2].outer = -2;
    let package = read(&package).unwrap();
    assert_eq!(package.path(ObjectRef::Export(2)).unwrap(), "Engine.MyTex");
}

#[test]
fn object_refs_round_trip() {
    for raw in [0, 1, 2, 15530, i32::MAX, -1, -2, -103, i32::MIN] {
        assert_eq!(ObjectRef::from_raw(raw).raw(), raw);
    }
    assert_eq!(ObjectRef::from_raw(0), ObjectRef::Null);
    assert_eq!(ObjectRef::from_raw(1), ObjectRef::Export(0));
    assert_eq!(ObjectRef::from_raw(-1), ObjectRef::Import(0));
    assert_eq!(
        ObjectRef::from_raw(i32::MIN),
        ObjectRef::Import(i32::MAX as usize)
    );
}

#[test]
fn object_flag_names() {
    let flags = ObjectFlags(0x040F0004 | 0x8);
    assert_eq!(
        flags.names(),
        vec![
            "Public",
            "LoadForClient",
            "LoadForServer",
            "LoadForEdit",
            "Standalone",
            "Native"
        ]
    );
    assert_eq!(flags.unknown(), 0x8);
}

#[cfg(feature = "serde")]
#[test]
fn object_flags_serialize_to_json() {
    assert_eq!(
        serde_json::to_value(ObjectFlags(0x10005)).unwrap(),
        serde_json::json!({ "bits": 0x10005, "names": ["Transactional", "Public", "LoadForClient"], "unknown": 0 })
    );
}

/// A package with these names and nothing else.
fn names_package(names: Vec<Vec<u8>>, version: u32) -> Package {
    read(&PackageBytes {
        summary: SummaryBytes {
            file_version: version,
            ..SummaryBytes::default()
        },
        names,
        ..PackageBytes::default()
    })
    .unwrap()
}

fn flags(mut entry: Vec<u8>) -> Vec<u8> {
    entry.extend_from_slice(&7u32.to_le_bytes());
    entry
}

#[test]
fn name_strings() {
    // Latin-1, UTF-16 (a negative count), the empty string, a string that
    // stops at an embedded null, and a name longer than the engine keeps.
    let mut latin1 = index(5);
    latin1.extend(b"Caf\xE9\0");
    let mut utf16 = index(-3);
    utf16.extend("Ж€\0".encode_utf16().flat_map(|u| u.to_le_bytes()));
    let mut embedded_null = index(6);
    embedded_null.extend(b"ab\0cd\0");
    let long = "N".repeat(70);
    let package = names_package(
        vec![
            flags(latin1),
            flags(utf16),
            flags(index(0)),
            flags(index(1).into_iter().chain([0]).collect()),
            flags(embedded_null),
            name(&long),
        ],
        128,
    );
    let names: Vec<&str> = package.names().iter().map(|n| n.name.as_str()).collect();
    assert_eq!(names, ["Café", "Ж€", "", "", "ab", &long[..63]]);
    assert_eq!(package.names()[0].flags, 7);
}

#[test]
fn names_before_version_64_are_null_terminated() {
    let package = names_package(
        vec![flags(b"Engine\0".to_vec()), flags(b"\xC0\0".to_vec())],
        63,
    );
    let names: Vec<&str> = package.names().iter().map(|n| n.name.as_str()).collect();
    assert_eq!(names, ["Engine", "À"]);
}

#[test]
fn compact_indices_of_every_length() {
    // Name indices are checked against the name table, so a package with
    // enough names for indices of 1 to 4 bytes. (5 bytes are tested with
    // sizes, below.)
    let values = [0, 1, 63, 64, 8191, 8192, 1_048_575, 1_048_576];
    let names: Vec<Vec<u8>> = (0..1_048_577).map(|_| name("")).collect();
    let package = read(&PackageBytes {
        names,
        exports: values
            .iter()
            .map(|&v| ExportBytes {
                object_name: v,
                size: 0,
                ..ExportBytes::default()
            })
            .collect(),
        ..PackageBytes::default()
    })
    .unwrap();
    let got: Vec<usize> = package.exports().iter().map(|e| e.object_name).collect();
    assert_eq!(got, values.map(|v| v as usize));
    assert_eq!(index(1_048_576).len(), 4);
}

#[test]
fn large_and_negative_compact_indices() {
    // A size of i32::MAX takes 5 bytes; it doesn't fit, so it's reported
    // with its value.
    let mut package = sample();
    package.exports[3].size = 0;
    let mut bytes = package.bytes();
    let export_offset = u32::from_le_bytes(bytes[24..28].try_into().unwrap()) as usize;
    // Rewrite the last export's size (the last byte of the file, since its
    // size is 0 and it has no offset) as i32::MAX, then as -5, then as 0x80
    // (negative zero, which reads as 0).
    assert!(export_offset < bytes.len());
    bytes.pop();
    let base = bytes.clone();
    bytes.extend(index(i32::MAX));
    bytes.extend(index(1));
    let error = Package::from_bytes(&bytes).unwrap_err().to_string();
    assert_eq!(
        error,
        format!(
            "invalid export 4: its data (2147483647 bytes at offset 1) isn't in the file ({} bytes)",
            bytes.len()
        )
    );
    let mut bytes = base.clone();
    bytes.extend(index(-5));
    bytes.extend(index(1));
    assert_eq!(
        Package::from_bytes(&bytes).unwrap_err().to_string(),
        "invalid export 4: its size is -5"
    );
    let mut bytes = base.clone();
    bytes.push(0x80);
    let package = Package::from_bytes(&bytes).unwrap();
    assert_eq!(package.exports()[3].serial_size, 0);
    // A 5-byte index whose last byte has high bits set wraps, as it does in
    // the engine: the high bits of the last byte are shifted out, leaving -1.
    let mut bytes = base;
    bytes.extend([0x40 | 0x3f, 0xff, 0xff, 0xff, 0xff]);
    bytes.extend(index(1));
    assert_eq!(
        Package::from_bytes(&bytes).unwrap_err().to_string(),
        "invalid export 4: its size is -1"
    );
}

#[test]
fn name_indices_must_be_in_range() {
    let base = sample();
    let cases: [(fn(&mut PackageBytes), &str); 5] = [
        (
            |p| p.imports[2].class_package = 11,
            "import -3: the class package is name 11, but there are 11 names",
        ),
        (
            |p| p.imports[0].class_name = -1,
            "import -1: the class name is name -1, but there are 11 names",
        ),
        (
            |p| p.imports[4].object_name = 99,
            "import -5: the object name is name 99, but there are 11 names",
        ),
        (
            |p| p.exports[1].object_name = 11,
            "export 2: the object name is name 11, but there are 11 names",
        ),
        (
            |p| p.exports[0].object_name = -3,
            "export 1: the object name is name -3, but there are 11 names",
        ),
    ];
    for (change, expected) in cases {
        let mut package = base.clone();
        change(&mut package);
        assert_eq!(invalid(&package), expected);
    }
}

#[test]
fn object_references_must_be_in_range() {
    let base = sample();
    let cases: [(fn(&mut PackageBytes), &str); 6] = [
        (
            |p| p.imports[1].outer = -6,
            "import -2: its outer is object -6, but there are 5 imports and 4 exports",
        ),
        (
            |p| p.imports[1].outer = 1,
            "import -2: its outer is export 1; an import must be in an import",
        ),
        (
            |p| p.exports[2].class = -6,
            "export 3: its class is object -6, but there are 5 imports and 4 exports",
        ),
        (
            |p| p.exports[0].super_struct = 5,
            "export 1: its super is object 5, but there are 5 imports and 4 exports",
        ),
        (
            |p| p.exports[2].outer = 5,
            "export 3: its outer is object 5, but there are 5 imports and 4 exports",
        ),
        (
            |p| p.exports[2].class = 5,
            "export 3: its class is object 5, but there are 5 imports and 4 exports",
        ),
    ];
    for (change, expected) in cases {
        let mut package = base.clone();
        change(&mut package);
        assert_eq!(invalid(&package), expected);
    }
}

#[test]
fn outers_must_not_loop() {
    let mut package = sample();
    package.exports[1].outer = 3;
    assert_eq!(invalid(&package), "export 2: its outers form a loop");
    let mut package = sample();
    package.exports[0].outer = 1;
    assert_eq!(invalid(&package), "export 1: its outers form a loop");
    let mut package = sample();
    package.imports[0].outer = -4;
    package.imports[3].outer = -1;
    assert_eq!(invalid(&package), "import -1: its outers form a loop");
}

#[test]
fn export_data_must_be_in_the_file() {
    // The file is 624 bytes, and every offset below takes 2 bytes, like the
    // offsets it replaces, so the file stays 624 bytes.
    let len = sample().bytes().len();
    assert_eq!(len, 624);
    let check = |export: usize, offset: i32| {
        let mut package = sample();
        package.exports[export].offset = Some(offset);
        assert_eq!(package.bytes().len(), len);
        read(&package)
    };
    let message = |e: Error| e.to_string();
    assert_eq!(
        message(check(0, -64).unwrap_err()),
        "invalid export 1: its data (40 bytes at offset -64) isn't in the file (624 bytes)"
    );
    assert_eq!(
        message(check(2, 325).unwrap_err()),
        "invalid export 3: its data (300 bytes at offset 325) isn't in the file (624 bytes)"
    );
    // Data that ends exactly at the end of the file is fine.
    check(2, 324).unwrap();
}

#[test]
fn truncated_tables_are_errors() {
    // Each case is a table that ends the file, with an entry cut short but
    // within the minimum sizes the summary check allows.
    let eof = |bytes: &[u8]| match Package::from_bytes(bytes) {
        Err(Error::UnexpectedEof { what, .. }) => what,
        other => panic!("expected the file to end early, got {:?}", other),
    };

    // A name that says it has 100 bytes, in a 5-byte entry.
    let package = PackageBytes {
        names: vec![index(100).into_iter().chain(*b"abcd").collect()],
        ..PackageBytes::default()
    };
    assert_eq!(eof(&package.bytes()), "name table");

    // An import whose object name is cut off after its first byte.
    let mut package = sample();
    package.exports.clear();
    package.imports.truncate(1);
    let mut bytes = package.bytes();
    *bytes.last_mut().unwrap() = 0x41;
    assert_eq!(eof(&bytes), "import or export table");

    // An export whose data offset (2 bytes) is cut off.
    let mut package = sample();
    package.exports.truncate(1);
    let mut bytes = package.bytes();
    bytes.truncate(bytes.len() - 2);
    assert_eq!(eof(&bytes), "export table");
}

#[test]
fn invalid_table_errors_are_displayed() {
    assert_eq!(
        Error::InvalidTable("export 1: its size is -5".to_string()).to_string(),
        "invalid export 1: its size is -5"
    );
}

#[test]
fn every_truncation_of_the_tables_is_an_error() {
    // The tables come after the summary and run to the end of the file, so
    // cutting the file anywhere from the names on ends a table early.
    let bytes = sample().bytes();
    for len in 64..bytes.len() {
        match Package::from_bytes(&bytes[..len]) {
            Err(Error::UnexpectedEof { .. }) => {}
            other => panic!("{} bytes: {:?}", len, other),
        }
    }
    // The offset is where the field that was cut short starts.
    assert_eq!(
        Package::from_bytes(&bytes[..600]).unwrap_err().to_string(),
        "the file ends in the export table at offset 599"
    );
}

#[test]
fn a_table_past_the_end_is_an_error() {
    let mut bytes = sample().bytes();
    // The name offset is at byte 16.
    bytes[16..20].copy_from_slice(&5000i32.to_le_bytes());
    assert_eq!(
        Package::from_bytes(&bytes).unwrap_err().to_string(),
        "the file ends in the name table at offset 5000"
    );
}

#[test]
fn names_that_end_early_are_errors() {
    // UTF-16 with only one of its two code units, and a null-terminated
    // name (before version 64) with no null.
    let mut utf16 = index(-2);
    utf16.extend([b'A', 0]);
    for (entry, version) in [(utf16, 128), (b"Engine".to_vec(), 63)] {
        let package = PackageBytes {
            summary: SummaryBytes {
                file_version: version,
                ..SummaryBytes::default()
            },
            names: vec![entry],
            ..PackageBytes::default()
        };
        let error = read(&package).unwrap_err();
        assert!(
            matches!(
                error,
                Error::UnexpectedEof {
                    what: "name table",
                    ..
                }
            ),
            "{:?}",
            error
        );
    }
}

#[test]
fn outers() {
    let package = read(&sample()).unwrap();
    assert_eq!(package.outer(ObjectRef::Export(2)), ObjectRef::Export(1));
    assert_eq!(package.outer(ObjectRef::Export(1)), ObjectRef::Null);
    assert_eq!(package.outer(ObjectRef::Import(2)), ObjectRef::Import(1));
    assert_eq!(package.outer(ObjectRef::Import(1)), ObjectRef::Null);
    assert_eq!(package.outer(ObjectRef::Null), ObjectRef::Null);
}

#[test]
fn top_level() {
    let package = read(&sample()).unwrap();
    // Engine.Texture is in Engine, a top-level package.
    assert_eq!(
        package.top_level(ObjectRef::Import(2)),
        ObjectRef::Import(1)
    );
    assert_eq!(
        package.top_level(ObjectRef::Import(1)),
        ObjectRef::Import(1)
    );
    // MyGroup.MyTex is in MyGroup.
    assert_eq!(
        package.top_level(ObjectRef::Export(2)),
        ObjectRef::Export(1)
    );
    assert_eq!(
        package.top_level(ObjectRef::Export(0)),
        ObjectRef::Export(0)
    );
    assert_eq!(package.top_level(ObjectRef::Null), ObjectRef::Null);
}
