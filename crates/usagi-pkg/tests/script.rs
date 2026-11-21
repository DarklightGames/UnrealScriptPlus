mod common;

use common::{ExportBytes, ImportBytes, PackageBytes, name};
use usagi_pkg::archive::{Archive, Loader, NameRef, Saver, Serialize};
use usagi_pkg::script::{Call, Context, Expr, Label, Script};
use usagi_pkg::{ObjectRef, Package};

const NAMES: &[&str] = &["None", "Core", "Package", "Foo", "Label"];

/// A package with the names above, an import (`Core`) and an export, for
/// the script's names and object references to be checked against.
fn package() -> Package {
    let bytes = PackageBytes {
        names: NAMES.iter().map(|text| name(text)).collect(),
        imports: vec![ImportBytes {
            class_package: 1,
            class_name: 2,
            outer: 0,
            object_name: 1,
        }],
        exports: vec![ExportBytes {
            object_name: 3,
            ..ExportBytes::default()
        }],
        ..PackageBytes::default()
    }
    .bytes();
    Package::from_bytes(&bytes).unwrap()
}

/// Export 0, as an object reference in the script: one byte in the file,
/// four in memory.
const OBJ: u8 = 0x01;
/// The name `Foo`.
const FOO: u8 = 3;

fn obj() -> ObjectRef {
    ObjectRef::Export(0)
}

fn foo() -> NameRef {
    NameRef(3)
}

fn b(expr: Expr) -> Box<Expr> {
    Box::new(expr)
}

fn le(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

/// Reads a script: its size in memory, then its code.
fn read(package: &Package, bytes: &[u8]) -> usagi_pkg::Result<(Script, usize)> {
    let mut loader = Loader::new(package, bytes);
    let mut script = Script::default();
    script.serialize(&mut loader)?;
    Ok((script, loader.pos()))
}

fn save(script: &mut Script) -> Vec<u8> {
    let mut saver = Saver::new(128, 29);
    script.serialize(&mut saver).unwrap();
    saver.into_bytes()
}

/// Every expression the engine accepts: its bytes in the file, its size in
/// memory, and what it reads as.
fn every_expr() -> Vec<(Vec<u8>, usize, Expr)> {
    let debug = |version: i32| {
        let mut bytes = vec![0x42];
        bytes.extend(le(&[version, 1, 2]));
        bytes.push(3);
        (
            bytes,
            14,
            Expr::DebugInfo {
                version,
                line: 1,
                pos: 2,
                opcode: 3,
            },
        )
    };
    let (debug_100, _, debug_100_expr) = debug(100);
    let context = |token: u8| {
        (
            vec![token, 0x20, OBJ, 2, 0, 1, 0x01, OBJ],
            14,
            Context {
                object: b(Expr::ObjectConst(obj())),
                skip: 2,
                size: 1,
                expr: b(Expr::InstanceVariable(obj())),
            },
        )
    };
    let (class_context, n, class_context_expr) = context(0x12);
    let (object_context, _, object_context_expr) = context(0x19);
    vec![
        (vec![0x00, OBJ], 5, Expr::LocalVariable(obj())),
        (vec![0x01, OBJ], 5, Expr::InstanceVariable(obj())),
        (vec![0x02, OBJ], 5, Expr::DefaultVariable(obj())),
        (vec![0x04, 0x0B], 2, Expr::Return(b(Expr::Nothing))),
        (
            vec![0x05, 4, 0x25],
            3,
            Expr::Switch {
                size: 4,
                expr: b(Expr::IntZero),
            },
        ),
        (vec![0x06, 0x34, 0x12], 3, Expr::Jump(0x1234)),
        (
            vec![0x07, 0x10, 0, 0x27],
            4,
            Expr::JumpIfNot {
                offset: 0x10,
                condition: b(Expr::True),
            },
        ),
        (vec![0x08], 1, Expr::Stop),
        (
            vec![0x09, 7, 0, 0x28],
            4,
            Expr::Assert {
                line: 7,
                condition: b(Expr::False),
            },
        ),
        (
            vec![0x0A, 0x20, 0, 0x26],
            4,
            Expr::Case {
                offset: 0x20,
                value: Some(b(Expr::IntOne)),
            },
        ),
        // `default:`
        (
            vec![0x0A, 0xFF, 0xFF],
            3,
            Expr::Case {
                offset: 0xFFFF,
                value: None,
            },
        ),
        (vec![0x0B], 1, Expr::Nothing),
        (
            vec![0x0D, 0x21, FOO],
            6,
            Expr::GotoLabel(b(Expr::NameConst(foo()))),
        ),
        (
            vec![0x0E, 0x1F, b'h', b'i', 0],
            5,
            Expr::EatString(b(Expr::StringConst(b"hi".to_vec()))),
        ),
        (
            [vec![0x0F, 0x00, OBJ, 0x1D], le(&[5])].concat(),
            11,
            Expr::Let {
                variable: b(Expr::LocalVariable(obj())),
                value: b(Expr::IntConst(5)),
            },
        ),
        (
            vec![0x10, 0x25, 0x00, OBJ],
            7,
            Expr::DynArrayElement {
                index: b(Expr::IntZero),
                array: b(Expr::LocalVariable(obj())),
            },
        ),
        (
            vec![0x11, 0x2A, 0x2A, 0x2A, 0x2A],
            5,
            Expr::New {
                outer: b(Expr::NoObject),
                name: b(Expr::NoObject),
                flags: b(Expr::NoObject),
                class: b(Expr::NoObject),
            },
        ),
        (class_context, n, Expr::ClassContext(class_context_expr)),
        (
            vec![0x13, OBJ, 0x17],
            6,
            Expr::MetaCast {
                class: obj(),
                expr: b(Expr::SelfObject),
            },
        ),
        (
            vec![0x14, 0x27, 0x28],
            3,
            Expr::LetBool {
                variable: b(Expr::True),
                value: b(Expr::False),
            },
        ),
        (vec![0x16], 1, Expr::EndFunctionParms),
        (vec![0x17], 1, Expr::SelfObject),
        (
            vec![0x18, 3, 0, 0x26],
            4,
            Expr::Skip {
                size: 3,
                expr: b(Expr::IntOne),
            },
        ),
        (object_context, n, Expr::Context(object_context_expr)),
        (
            vec![0x1A, 0x26, 0x00, OBJ],
            7,
            Expr::ArrayElement {
                index: b(Expr::IntOne),
                array: b(Expr::LocalVariable(obj())),
            },
        ),
        (
            vec![0x1B, FOO, 0x25, 0x16],
            7,
            Expr::VirtualFunction {
                name: foo(),
                call: Call {
                    args: vec![Expr::IntZero],
                    debug_info: None,
                },
            },
        ),
        // Debug information of version 100 after a call is part of it.
        (
            [vec![0x1C, OBJ, 0x16], debug_100].concat(),
            20,
            Expr::FinalFunction {
                function: obj(),
                call: Call {
                    args: Vec::new(),
                    debug_info: Some(b(debug_100_expr)),
                },
            },
        ),
        (
            [vec![0x1E], 1.5f32.to_le_bytes().to_vec()].concat(),
            5,
            Expr::FloatConst(1.5),
        ),
        (
            [vec![0x22], le(&[1, 2, 3])].concat(),
            13,
            Expr::RotationConst {
                pitch: 1,
                yaw: 2,
                roll: 3,
            },
        ),
        (
            [
                vec![0x23],
                [1.0f32, 2.0, 3.0]
                    .iter()
                    .flat_map(|v| v.to_le_bytes())
                    .collect(),
            ]
            .concat(),
            13,
            Expr::VectorConst {
                x: 1.0,
                y: 2.0,
                z: 3.0,
            },
        ),
        (vec![0x24, 7], 2, Expr::ByteConst(7)),
        (vec![0x29, OBJ], 5, Expr::NativeParm(obj())),
        (vec![0x2C, 9], 2, Expr::IntConstByte(9)),
        (vec![0x2D], 1, Expr::BoolVariable),
        (
            vec![0x2E, OBJ, 0x17],
            6,
            Expr::DynamicCast {
                class: obj(),
                expr: b(Expr::SelfObject),
            },
        ),
        (
            vec![0x2F, 0x00, OBJ, 0x40, 0],
            8,
            Expr::Iterator {
                expr: b(Expr::LocalVariable(obj())),
                offset: 0x40,
            },
        ),
        (vec![0x30], 1, Expr::IteratorPop),
        (vec![0x31], 1, Expr::IteratorNext),
        (
            vec![0x32, OBJ, 0x25, 0x26],
            7,
            Expr::StructCmpEq {
                structure: obj(),
                left: b(Expr::IntZero),
                right: b(Expr::IntOne),
            },
        ),
        (
            vec![0x33, OBJ, 0x25, 0x26],
            7,
            Expr::StructCmpNe {
                structure: obj(),
                left: b(Expr::IntZero),
                right: b(Expr::IntOne),
            },
        ),
        (
            vec![0x34, b'h', 0, b'i', 0, 0, 0],
            7,
            Expr::UnicodeStringConst(vec![u16::from(b'h'), u16::from(b'i')]),
        ),
        (
            vec![0x36, OBJ, 0x17],
            6,
            Expr::StructMember {
                property: obj(),
                expr: b(Expr::SelfObject),
            },
        ),
        (
            vec![0x37, 0x00, OBJ],
            6,
            Expr::DynArrayLength(b(Expr::LocalVariable(obj()))),
        ),
        (
            vec![0x38, FOO, 0x16],
            6,
            Expr::GlobalFunction {
                name: foo(),
                call: Call::default(),
            },
        ),
        (
            vec![0x39, 0x3A, 0x26],
            3,
            Expr::PrimitiveCast {
                cast: 0x3A,
                expr: b(Expr::IntOne),
            },
        ),
        (
            vec![0x40, 0x00, OBJ, 0x25, 0x26],
            8,
            Expr::DynArrayInsert {
                array: b(Expr::LocalVariable(obj())),
                index: b(Expr::IntZero),
                count: b(Expr::IntOne),
            },
        ),
        (
            vec![0x41, 0x00, OBJ, 0x25, 0x26],
            8,
            Expr::DynArrayRemove {
                array: b(Expr::LocalVariable(obj())),
                index: b(Expr::IntZero),
                count: b(Expr::IntOne),
            },
        ),
        // A native call: the debug information after it isn't version 100,
        // so it's an expression of its own.
        (
            vec![0x70, 0x25, 0x16],
            3,
            Expr::Native {
                index: 0x70,
                call: Call {
                    args: vec![Expr::IntZero],
                    debug_info: None,
                },
            },
        ),
        debug(99),
        (
            vec![0x43, OBJ, FOO],
            9,
            Expr::DelegateFunction {
                property: obj(),
                name: foo(),
            },
        ),
        (vec![0x44, FOO], 5, Expr::DelegateProperty(foo())),
        (
            vec![0x45, 0x00, OBJ, 0x2A],
            7,
            Expr::LetDelegate {
                variable: b(Expr::LocalVariable(obj())),
                value: b(Expr::NoObject),
            },
        ),
        // EX_ExtendedNative: the high bits in the token, then a byte.
        (
            vec![0x61, 0x23, 0x16],
            3,
            Expr::Native {
                index: 0x123,
                call: Call::default(),
            },
        ),
        (vec![0x47], 1, Expr::EndOfScript),
    ]
}

#[test]
fn every_expression_reads_and_writes_back() {
    let package = package();
    let mut code = Vec::new();
    let mut memory = 0;
    let mut expected = Vec::new();
    for (bytes, size, expr) in every_expr() {
        code.extend(bytes);
        memory += size;
        expected.push(expr);
    }
    // A label table, which must start 4-byte aligned in memory: `Nothing`s
    // to align it.
    while (memory + 1) % 4 != 0 {
        code.push(0x0B);
        memory += 1;
        expected.push(Expr::Nothing);
    }
    code.push(0x0C);
    code.push(4); // `Label`
    code.extend(le(&[0x30]));
    code.push(0); // `None`
    code.extend(le(&[-1]));
    memory += 17;
    expected.push(Expr::LabelTable(vec![
        Label {
            name: NameRef(4),
            offset: 0x30,
        },
        Label {
            name: NameRef(0),
            offset: -1,
        },
    ]));
    // A call at the end: nothing after it to be its debug information.
    code.extend([0x71, 0x16]);
    memory += 2;
    expected.push(Expr::Native {
        index: 0x71,
        call: Call::default(),
    });

    let bytes = [le(&[memory as i32]), code].concat();
    // Cut short anywhere: an error.
    for len in 0..bytes.len() {
        assert!(read(&package, &bytes[..len]).is_err(), "{} bytes", len);
    }
    let (mut script, end) = read(&package, &bytes).unwrap();
    assert_eq!(end, bytes.len());
    assert_eq!(script.size, memory as i32);
    assert_eq!(script.exprs, expected);
    // Written back: the same bytes, and the size worked out again.
    script.size = 0;
    assert_eq!(save(&mut script), bytes);
}

#[test]
fn a_script_that_isnt_its_size() {
    let package = package();
    // An int constant is 5 bytes in memory, not 3.
    let bytes = [le(&[3]), vec![0x1D], le(&[7])].concat();
    assert_eq!(
        read(&package, &bytes).unwrap_err().to_string(),
        "invalid data at offset 9: the script is 5 bytes in memory, not 3"
    );
    assert_eq!(
        read(&package, &le(&[-1])).unwrap_err().to_string(),
        "invalid data at offset 0: the script's size is -1"
    );
}

#[test]
fn bad_scripts() {
    let package = package();
    for (code, message) in [
        // Tokens the engine rejects.
        (
            vec![0x03],
            "invalid data at offset 4: bad token 0x03 in the script",
        ),
        (
            vec![0x15],
            "invalid data at offset 4: bad token 0x15 in the script",
        ),
        (
            vec![0x5F],
            "invalid data at offset 4: bad token 0x5F in the script",
        ),
        // A label table that isn't aligned in memory.
        (
            vec![0x0B, 0x0C, 0, 0, 0, 0, 0],
            "invalid data at offset 6: a label table that isn't aligned to 4 bytes in memory",
        ),
        // References out of range.
        (
            vec![0x20, 0x02],
            "invalid data at offset 5: object reference 2 in the script is out of range",
        ),
        (
            vec![0x20, 0x82],
            "invalid data at offset 5: object reference -2 in the script is out of range",
        ),
        (
            vec![0x21, 0x05],
            "invalid data at offset 5: name 5 in the script is out of range",
        ),
        (
            vec![0x21, 0x81],
            "invalid data at offset 5: name -1 in the script is out of range",
        ),
    ] {
        let bytes = [le(&[100]), code].concat();
        assert_eq!(read(&package, &bytes).unwrap_err().to_string(), message);
    }
    // Cut short: an `UnexpectedEof`, which `Package::read_native` reports
    // as invalid data.
    let bytes = [le(&[100]), vec![0x1D, 1]].concat();
    assert!(matches!(
        read(&package, &bytes),
        Err(usagi_pkg::Error::UnexpectedEof { what: "script", .. })
    ));
}
