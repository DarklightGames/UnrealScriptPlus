mod common;

use std::path::Path;
use std::rc::Rc;

use common::builder::*;
use common::index;
use tempfile::TempDir;
use usagi_pkg::archive::NameRef;
use usagi_pkg::{
    Decl, Enum, Field, Lookup, ObjectRef, Package, Packages, Property, PropertyDecl, PropertyKind,
    Scope, SearchPath, Type, Value,
};

/// How many levels of `struct Deep { var Deep Inner; }` the test nests.
const DEEP: usize = 34;

/// `Base.u`: class `Base`, whose fields are an enum `Colors`, structs
/// `Point { X, Y }` and `Deep { Inner }`, and a property of nearly every
/// type; and its defaults. Then things that can't be read: a class that is
/// its own superclass, one cut short, one with a field cut short, one with
/// a field that isn't a property cut short; a private struct; and a group.
fn base(dir: &Path, licensee: u32) {
    let mut b = Builder::new();
    b.licensee = licensee;
    let base_class = b.export(0, 0, "Base");
    let enum_class = b.core_class("Enum");
    let colors = b.export(enum_class, base_class, "Colors");
    let struct_class = b.core_class("Struct");
    let point = b.export(struct_class, base_class, "Point");
    let deep = b.export(struct_class, base_class, "Deep");
    // A struct whose superclass can't be found; one with a field whose type
    // can't be read; one with a struct in it.
    let shaky = b.export(struct_class, base_class, "Shaky");
    let holey = b.export(struct_class, base_class, "Holey");
    let pair = b.export(struct_class, base_class, "Pair");
    let object = b.package("Object");
    let vector = b.import("Core", "Struct", object, "Vector");
    let rotator = b.import("Core", "Struct", object, "Rotator");
    let color = b.import("Core", "Struct", object, "Color");
    let lost = b.import("Core", "Struct", object, "Lost");

    // struct Point { var int X, Y; };
    let y = b.property(point, "Y", PropertyKind::Int, 1, 0, 0);
    let x = b.property(point, "X", PropertyKind::Int, 1, 0, y);
    // struct Deep { var Deep Inner; };
    let inner = b.property(
        deep,
        "Inner",
        PropertyKind::Struct {
            structure: ob(deep),
        },
        1,
        0,
        0,
    );
    let hole = b.property(
        holey,
        "Hole",
        PropertyKind::Array {
            inner: ObjectRef::Null,
        },
        1,
        0,
        0,
    );
    let a = b.property(
        pair,
        "A",
        PropertyKind::Struct {
            structure: ob(point),
        },
        1,
        0,
        0,
    );
    // A property cut short, the element of an array.
    let bad = b.property(base_class, "Bad", PropertyKind::Int, 1, 0, 0);
    b.set_data(bad, vec![0, 0]);

    // Arrays' elements.
    let element = |b: &mut Builder, name: &str, kind: PropertyKind| {
        b.property(base_class, name, kind, 1, 0, 0)
    };
    let fixed_inner = element(&mut b, "FixedInner", PropertyKind::Int);
    let point_inner = element(
        &mut b,
        "PointsInner",
        PropertyKind::Struct {
            structure: ob(point),
        },
    );
    let int_inner = element(&mut b, "IntsInner", PropertyKind::Int);
    let pointer_inner = element(&mut b, "PtrsInner", PropertyKind::Pointer);
    let bool_inner = element(&mut b, "BoolsInner", PropertyKind::Bool);
    // An array whose element is itself.
    let array_class = b.core_class("ArrayProperty");
    let selfish = b.export(array_class, base_class, "Selfish");

    // The properties, last first, each followed by the one after it.
    let mut next = selfish;
    for (name, kind, dim, flags) in [
        ("Hidden", PropertyKind::Int, 1, CPF_TRANSIENT),
        ("BadArray", PropertyKind::Array { inner: ob(bad) }, 1, 0),
        (
            "BadFixed",
            PropertyKind::FixedArray {
                inner: ObjectRef::Null,
                count: 1,
            },
            1,
            0,
        ),
        (
            "ShakyOne",
            PropertyKind::Struct {
                structure: ob(shaky),
            },
            1,
            0,
        ),
        (
            "HoleyOne",
            PropertyKind::Struct {
                structure: ob(holey),
            },
            1,
            0,
        ),
        (
            "PairOne",
            PropertyKind::Struct {
                structure: ob(pair),
            },
            1,
            0,
        ),
        ("Ptr", PropertyKind::Pointer, 1, 0),
        (
            "Ptrs",
            PropertyKind::Array {
                inner: ob(pointer_inner),
            },
            1,
            0,
        ),
        (
            "Bools",
            PropertyKind::Array {
                inner: ob(bool_inner),
            },
            1,
            0,
        ),
        (
            "Table",
            PropertyKind::Map {
                key: ObjectRef::Null,
                value: ObjectRef::Null,
            },
            1,
            0,
        ),
        (
            "Where",
            PropertyKind::Struct {
                structure: ob(vector),
            },
            1,
            0,
        ),
        (
            "Rot",
            PropertyKind::Struct {
                structure: ob(rotator),
            },
            1,
            0,
        ),
        (
            "Tint",
            PropertyKind::Struct {
                structure: ob(color),
            },
            1,
            0,
        ),
        (
            "Lost",
            PropertyKind::Struct {
                structure: ob(lost),
            },
            1,
            0,
        ),
        (
            "DeepOne",
            PropertyKind::Struct {
                structure: ob(deep),
            },
            1,
            0,
        ),
        (
            "Pos",
            PropertyKind::Struct {
                structure: ob(point),
            },
            1,
            0,
        ),
        (
            "Fixed",
            PropertyKind::FixedArray {
                inner: ob(fixed_inner),
                count: 2,
            },
            1,
            0,
        ),
        (
            "Points",
            PropertyKind::Array {
                inner: ob(point_inner),
            },
            1,
            0,
        ),
        (
            "Ints",
            PropertyKind::Array {
                inner: ob(int_inner),
            },
            1,
            0,
        ),
        // An array whose elements' type can't be read.
        (
            "Broken",
            PropertyKind::Array {
                inner: ObjectRef::Null,
            },
            1,
            0,
        ),
        (
            "OnHit",
            PropertyKind::Delegate {
                function: ObjectRef::Null,
            },
            1,
            0,
        ),
        ("Text", PropertyKind::Str, 1, 0),
        ("Label", PropertyKind::Name, 1, 0),
        (
            "Kind",
            PropertyKind::Class {
                class: ob(base_class),
                meta_class: ob(base_class),
            },
            1,
            0,
        ),
        (
            "Ref",
            PropertyKind::Object {
                class: ob(base_class),
            },
            1,
            0,
        ),
        ("Speed", PropertyKind::Float, 1, 0),
        ("Flag", PropertyKind::Bool, 1, 0),
        (
            "Mode",
            PropertyKind::Byte {
                enumeration: ob(colors),
            },
            1,
            0,
        ),
        ("Count", PropertyKind::Int, 2, 0),
    ] {
        next = b.property(base_class, name, kind, dim, flags, next);
    }
    let count = next;
    let data = b.saved(PropertyDecl {
        array_dim: 1,
        kind: PropertyKind::Array { inner: ob(selfish) },
        ..PropertyDecl::default()
    });
    b.set_data(selfish, [vec![0], data].concat());

    let names = vec![NameRef(b.name("Red")), NameRef(b.name("Blue"))];
    let data = b.saved(Enum {
        field: Field {
            super_field: ObjectRef::Null,
            next: ob(point),
        },
        names,
    });
    b.set_data(colors, [vec![0], data].concat());
    let mut structure = b.structure("Point", 0, x);
    structure.field.next = ob(deep);
    let data = b.saved(structure);
    b.set_data(point, [vec![0], data].concat());
    for (export, name, super_struct, children, next) in [
        (deep, "Deep", 0, inner, shaky),
        (shaky, "Shaky", lost, 0, holey),
        (holey, "Holey", 0, hole, pair),
        (pair, "Pair", 0, a, count),
    ] {
        let mut structure = b.structure(name, super_struct, children);
        structure.field.next = ob(next);
        let data = b.saved(structure);
        b.set_data(export, [vec![0], data].concat());
    }

    let mut defaults = b.simple("Count", INT, &ints(&[5]));
    defaults.extend(b.simple("Mode", BYTE, &[1]));
    b.set_class(base_class, 0, colors, defaults);

    // A class that is its own superclass.
    let looped = b.export(0, 0, "Loop");
    b.set_class(looped, looped, 0, Vec::new());
    // A class cut short.
    let short = b.export(0, 0, "Short");
    b.set_data(short, vec![0, 0]);
    // A class whose field is a property cut short.
    b.class("BadField", 0, bad, Vec::new());
    // A class whose field is an enum cut short, and one whose field is an
    // enum with malformed tagged properties.
    let odd = b.export(enum_class, base_class, "Odd");
    b.set_data(odd, vec![0]);
    b.class("BadChild", 0, odd, Vec::new());
    let odder = b.export(enum_class, base_class, "Odder");
    b.set_data(odder, vec![120]);
    b.class("BadChild2", 0, odder, Vec::new());
    // A struct whose superclass can't be read.
    let weak = b.export(struct_class, base_class, "Weak");
    let structure = b.structure("Weak", short, 0);
    let data = b.saved(structure);
    b.set_data(weak, [vec![0], data].concat());
    // An object whose state frame is cut short.
    let stuck = b.export(base_class, 0, "Stuck");
    b.set_data(stuck, vec![0]);
    b.set_flags(stuck, 0x0207_0004);
    // A struct other packages can't import.
    let private = b.export(struct_class, base_class, "Private");
    let structure = b.structure("Private", 0, 0);
    let data = b.saved(structure);
    b.set_data(private, [vec![0], data].concat());
    b.set_flags(private, 0x0007_0000);
    // A group with a struct in it.
    let package_class = b.core_class("Package");
    let group = b.export(package_class, 0, "Grp");
    b.set_data(group, vec![0]);
    let inside = b.export(struct_class, group, "Inside");
    let structure = b.structure("Inside", 0, 0);
    let data = b.saved(structure);
    b.set_data(inside, [vec![0], data].concat());
    b.write(dir, "Base.u");
}

/// What `Derived.u` imports from `Base.u` (and from itself and a missing
/// package), by name.
struct Imports {
    inside: i32,
    group_base: i32,
    private: i32,
    no_group: i32,
    point: i32,
    thing: i32,
    base_package: i32,
    nope: i32,
}

/// `Derived.u`: class `Derived`, which extends `Base.Base`, with defaults;
/// an object `Thing` of class `Derived` with `thing`'s properties; class
/// `Lost`, whose superclass isn't in `Base.u`; and an object `Orphan` of a
/// class in a missing package.
fn derived(dir: &Path, licensee: u32, thing: impl FnOnce(&mut Builder) -> Vec<u8>) -> Imports {
    derived_cut(dir, licensee, thing, None)
}

/// [`derived`], with `Thing`'s data cut to `cut` bytes.
fn derived_cut(
    dir: &Path,
    licensee: u32,
    thing: impl FnOnce(&mut Builder) -> Vec<u8>,
    cut: Option<usize>,
) -> Imports {
    let mut b = Builder::new();
    b.licensee = licensee;
    let base_package = b.package("Base");
    let base_class = b.import("Core", "Class", base_package, "Base");
    let defaults = b.prop("Count", INT, None, Some(1), &ints(&[7]), 4);
    let derived = b.class("Derived", base_class, 0, defaults);
    let thing_export = b.export(derived, 0, "Thing");
    let mut data = thing(&mut b);
    data.push(0);
    data.truncate(cut.unwrap_or(data.len()));
    b.set_data(thing_export, data);
    let nope = b.import("Core", "Class", base_package, "Nope");
    let lost_defaults = b.simple("Count", INT, &ints(&[3]));
    b.class("Lost", nope, 0, lost_defaults);
    let missing = b.package("Missing");
    let gone = b.import("Core", "Class", missing, "Gone");
    let orphan = b.export(gone, 0, "Orphan");
    let orphan_data = [b.simple("Count", INT, &ints(&[4])), vec![0]].concat();
    b.set_data(orphan, orphan_data);

    let group = b.import("Core", "Package", base_package, "Grp");
    let imports = Imports {
        inside: b.import("Core", "Struct", group, "Inside"),
        // The quirk: `Base.Base` is found as `Base.Grp.Base`.
        group_base: b.import("Core", "Class", group, "Base"),
        private: b.import("Core", "Struct", base_package, "Private"),
        no_group: {
            let group = b.import("Core", "Package", base_package, "NoGroup");
            b.import("Core", "Struct", group, "Inside")
        },
        point: {
            let base = b.import("Core", "Class", base_package, "Base");
            b.import("Core", "Struct", base, "Point")
        },
        thing: {
            let me = b.package("Derived");
            b.import("Derived", "Derived", me, "Thing")
        },
        base_package,
        nope,
    };
    b.write(dir, "Derived.u");
    imports
}

/// A set of packages in `dir`, `.u` files.
fn packages(dir: &Path) -> Packages {
    Packages::new(vec![SearchPath::from_pattern(dir, "*.u")])
}

/// Export `name` of `package`.
fn export(package: &Package, name: &str) -> usize {
    (0..package.exports().len())
        .find(|&i| package.name(package.exports()[i].object_name) == name)
        .unwrap()
}

fn scope(package: &Rc<Package>, name: &str) -> Scope {
    Scope {
        package: Rc::clone(package),
        export: export(package, name),
    }
}

fn props(packages: &Packages, package: &Rc<Package>, name: &str) -> Vec<Property> {
    packages
        .properties(package, export(package, name))
        .unwrap()
        .map(Result::unwrap)
        .collect()
}

#[test]
fn search_paths() {
    let base = Path::new("/game/System");
    assert_eq!(
        SearchPath::from_pattern(base, "..\\Textures\\*.utx"),
        SearchPath {
            dir: base.join("../Textures/"),
            extension: ".utx".to_string(),
        }
    );
    // No `*`: a folder, and no extension.
    assert_eq!(
        SearchPath::from_pattern(base, "../Maps/"),
        SearchPath {
            dir: base.join("../Maps/"),
            extension: String::new(),
        }
    );
}

#[test]
fn finding_packages() {
    let dir = TempDir::new().unwrap();
    base(dir.path(), 29);
    std::fs::write(dir.path().join("Broken.u"), b"not a package").unwrap();
    std::fs::create_dir(dir.path().join("Sub")).unwrap();
    std::fs::write(dir.path().join("Sub").join("Base.U"), b"shadowed").unwrap();
    let packages = Packages::new(vec![
        SearchPath::from_pattern(dir.path(), "*.utx"),
        SearchPath::from_pattern(dir.path(), "*.u"),
        SearchPath::from_pattern(&dir.path().join("Sub"), "*.u"),
        SearchPath::from_pattern(&dir.path().join("Missing"), "*.u"),
    ]);
    // By name in any case; the first path that has it.
    let found = packages.get("BASE").unwrap();
    assert_eq!(found.name, "Base");
    // Read once.
    assert!(Rc::ptr_eq(&found, &packages.get("base").unwrap()));
    // Not there, or not a package: remembered too.
    assert!(packages.get("Nope").is_none());
    assert!(packages.get("Broken").is_none());
    assert!(packages.get("broken").is_none());
    // One added takes the place of one of the same name.
    let added = packages.add(Package::read(&dir.path().join("Base.u")).unwrap());
    assert!(Rc::ptr_eq(&added, &packages.get("Base").unwrap()));
}

#[test]
fn resolving_imports() {
    let dir = TempDir::new().unwrap();
    base(dir.path(), 29);
    let imports = derived(dir.path(), 29, |_| Vec::new());
    let packages = packages(dir.path());
    let derived = packages.get("Derived").unwrap();
    let base = packages.get("Base").unwrap();
    let resolve = |raw: i32| packages.resolve(&derived, ob(raw));
    // In a group; the quirk; through an outer that is an export.
    assert_eq!(resolve(imports.inside), Some(scope(&base, "Inside")));
    assert_eq!(resolve(imports.group_base), Some(scope(&base, "Base")));
    assert_eq!(resolve(imports.point), Some(scope(&base, "Point")));
    // Again, from the cache.
    assert_eq!(resolve(imports.point), Some(scope(&base, "Point")));
    // An object whose class is in its own package.
    assert_eq!(resolve(imports.thing), Some(scope(&derived, "Thing")));
    // Not public; not there; a package; nothing.
    assert_eq!(resolve(imports.private), None);
    assert_eq!(resolve(imports.no_group), None);
    assert_eq!(resolve(imports.nope), None);
    assert_eq!(resolve(imports.base_package), None);
    assert_eq!(resolve(0), None);
    // An export is itself.
    assert_eq!(resolve(1), Some(scope(&derived, "Derived")));
}

#[test]
fn finding_declarations() {
    let dir = TempDir::new().unwrap();
    base(dir.path(), 29);
    derived(dir.path(), 29, |_| Vec::new());
    let packages = packages(dir.path());
    let derived = packages.get("Derived").unwrap();
    let base = packages.get("Base").unwrap();
    // A class's properties are its own; another object's, its class's.
    let derived_class = packages
        .scope_of(&derived, export(&derived, "Derived"))
        .unwrap();
    assert_eq!(format!("{:?}", derived_class), "Class'Derived.Derived'");
    assert_eq!(
        packages.scope_of(&derived, export(&derived, "Thing")),
        Some(derived_class.clone())
    );
    assert_eq!(
        packages.scope_of(&derived, export(&derived, "Orphan")),
        None
    );
    // Found in the superclass, in another package; in any case.
    let Lookup::Found(count) = packages.find_property(&derived_class, "count") else {
        panic!("Count isn't found");
    };
    assert_eq!(
        *count,
        Decl {
            name: "Count".to_string(),
            array_dim: 2,
            flags: 0,
            ty: Type::Int,
        }
    );
    assert!(count.is_stored());
    assert_eq!(
        packages.find_property(&derived_class, "Nope"),
        Lookup::Missing
    );
    // A superclass that can't be found, or loops; classes cut short; a
    // field cut short; not a struct.
    for (package, name) in [
        (&derived, "Lost"),
        (&base, "Loop"),
        (&base, "Short"),
        (&base, "BadField"),
        (&base, "BadChild"),
        (&base, "BadChild2"),
        (&base, "Colors"),
    ] {
        assert_eq!(
            packages.find_property(&scope(package, name), "Count"),
            Lookup::Unknown,
            "{}",
            name
        );
    }
    // The types of the declarations.
    let base_class = scope(&base, "Base");
    let ty = |name: &str| match packages.find_property(&base_class, name) {
        Lookup::Found(decl) => decl.ty.clone(),
        other => panic!("{}: {:?}", name, other),
    };
    assert_eq!(ty("Mode"), Type::Byte(Some(scope(&base, "Colors"))));
    assert_eq!(ty("Kind"), Type::Object);
    assert_eq!(ty("Ints"), Type::Array(Box::new(Type::Int)));
    assert_eq!(ty("Fixed"), Type::FixedArray(Box::new(Type::Int), 2));
    assert_eq!(
        ty("Pos"),
        Type::Struct("Point".to_string(), Some(scope(&base, "Point")))
    );
    assert_eq!(ty("Where"), Type::Struct("Vector".to_string(), None));
    assert_eq!(ty("Broken"), Type::Array(Box::new(Type::Unknown)));
    assert_eq!(ty("BadArray"), Type::Array(Box::new(Type::Unknown)));
    assert_eq!(ty("Table"), Type::Map);
    assert_eq!(ty("Ptr"), Type::Pointer);
    assert!(!match packages.find_property(&base_class, "Hidden") {
        Lookup::Found(decl) => decl.is_stored(),
        _ => true,
    });
    // An array of itself: given up on, eventually.
    let mut selfish = ty("Selfish");
    let mut depth = 0;
    while let Type::Array(inner) = selfish {
        selfish = *inner;
        depth += 1;
    }
    assert_eq!((selfish, depth > 32), (Type::Unknown, true));

    // A struct's properties: its own, then its superclass's.
    let names = |scope: &Scope| -> Option<Vec<String>> {
        packages
            .struct_properties(scope)
            .map(|decls| decls.iter().map(|d| d.name.clone()).collect())
    };
    assert_eq!(
        names(&scope(&base, "Point")),
        Some(vec!["X".into(), "Y".into()])
    );
    assert_eq!(names(&derived_class).map(|n| n.len()), Some(30));
    assert_eq!(names(&scope(&base, "Loop")), None);
    assert_eq!(names(&scope(&derived, "Lost")), None);
    assert_eq!(names(&scope(&base, "Weak")), None);
    // An object whose properties can't be found for its state frame.
    assert_eq!(
        packages
            .properties(&base, export(&base, "Stuck"))
            .unwrap_err()
            .to_string(),
        "invalid data at offset 1: the data ends in the state frame"
    );
}

#[test]
fn every_type() {
    for (id, ty) in [
        (1, Type::Byte(None)),
        (2, Type::Int),
        (3, Type::Bool),
        (4, Type::Float),
        (5, Type::Object),
        (6, Type::Name),
        (7, Type::Delegate),
        (9, Type::Array(Box::new(Type::Int))),
        (10, Type::Struct(String::new(), None)),
        (13, Type::Str),
        (14, Type::Map),
        (15, Type::FixedArray(Box::new(Type::Int), 1)),
        (16, Type::Pointer),
        (0, Type::Unknown),
    ] {
        assert_eq!(ty.id(), id, "{:?}", ty);
    }
}

#[test]
fn defaults_through_the_superclass() {
    let dir = TempDir::new().unwrap();
    base(dir.path(), 29);
    derived(dir.path(), 29, |_| Vec::new());
    let packages = packages(dir.path());
    let derived = packages.get("Derived").unwrap();
    let base = packages.get("Base").unwrap();
    let defaults = props(&packages, &base, "Base");
    assert_eq!(defaults[0].value, Value::Int(5));
    assert_eq!(defaults[1].value, Value::Byte(1));
    let defaults = props(&packages, &derived, "Derived");
    assert_eq!(
        (defaults[0].array_index, &defaults[0].value),
        (1, &Value::Int(7))
    );
    // Without the declarations: as `Package::properties` reads them.
    assert_eq!(props(&packages, &derived, "Lost")[0].value, Value::Int(3));
    assert_eq!(props(&packages, &derived, "Orphan")[0].value, Value::Int(4));
}

/// `Thing`'s properties as T3D, `Name=Value`.
/// Checks that it can't be read from any less of the data (but some: with
/// none, there are no properties).
fn thing_t3d(licensee: u32, data: impl Fn(&mut Builder) -> Vec<u8>) -> Vec<String> {
    let dir = TempDir::new().unwrap();
    base(dir.path(), licensee);
    derived(dir.path(), licensee, &data);
    let set = packages(dir.path());
    let derived = set.get("Derived").unwrap();
    let thing = export(&derived, "Thing");
    for cut in 1..derived.export_data(thing).len() {
        let dir = TempDir::new().unwrap();
        base(dir.path(), licensee);
        derived_cut(dir.path(), licensee, &data, Some(cut));
        let cut_packages = packages(dir.path());
        let derived = cut_packages.get("Derived").unwrap();
        let ok = cut_packages
            .properties(&derived, thing)
            .map(|mut properties| properties.all(|p| p.is_ok()));
        assert!(!matches!(ok, Ok(true)), "cut to {} bytes", cut);
    }
    props(&set, &derived, "Thing")
        .iter()
        .map(|p| format!("{}={}", p.t3d_name(), derived.t3d_value(&p.value)))
        .collect()
}

#[test]
fn values_read_by_their_declarations() {
    assert_eq!(
        thing_t3d(29, |b| {
            let mut out = b.simple("Count", INT, &ints(&[1]));
            out.extend(b.prop("Count", INT, None, Some(1), &ints(&[2]), 4));
            out.extend(b.simple("Mode", BYTE, &[2]));
            // A bool's value is in its tag.
            out.extend(index(b.name("Flag")));
            out.extend([BOOL | 0x50 | 0x80, 0]);
            out.extend(b.simple("Speed", FLOAT, &floats(&[1.5])));
            out.extend(b.simple("Ref", OBJECT, &[1]));
            out.extend(b.simple("Ref", OBJECT, &[0x81]));
            // A class property's tag is an object property's.
            out.extend(b.simple("Kind", OBJECT, &[0]));
            let label = b.name("Label") as u8;
            out.extend(b.simple("Label", NAME, &[label]));
            out.extend(b.simple("Text", STR, &[3, b'h', b'i', 0]));
            let on_hit = b.name("OnHit") as u8;
            out.extend(b.simple("OnHit", DELEGATE, &[1, on_hit]));
            out
        }),
        [
            "Count=1",
            "Count(1)=2",
            "Mode=2",
            "Flag=True",
            "Speed=1.500000",
            "Ref=Class'Derived.Derived'",
            "Ref=Package'Base'",
            "Kind=None",
            "Label=Label",
            "Text=\"hi\"",
            "OnHit=Derived.OnHit",
        ]
    );
    // Arrays and structs: by their declarations, whatever size the tag says
    // (here, a byte too few).
    assert_eq!(
        thing_t3d(29, |b| {
            let mut out = b.prop(
                "Ints",
                ARRAY,
                None,
                None,
                &[vec![2], ints(&[1, 2])].concat(),
                8,
            );
            let x = b.simple("X", INT, &ints(&[3]));
            let points = [vec![1], x.clone(), vec![0]].concat();
            out.extend(b.prop("Points", ARRAY, None, None, &points, points.len() - 1));
            let pos = [x, vec![0]].concat();
            out.extend(b.prop("Pos", STRUCT, Some("Point"), None, &pos, pos.len() - 1));
            out.extend(b.simple("Fixed", FIXED_ARRAY, &ints(&[4, 5])));
            out.extend(b.simple("Ptrs", ARRAY, &[vec![1], ints(&[6])].concat()));
            out.extend(b.simple("Bools", ARRAY, &[2, 1, 0]));
            out.extend(b.prop(
                "Where",
                STRUCT,
                Some("Vector"),
                None,
                &floats(&[1.0, 2.0, 3.0]),
                12,
            ));
            out.extend(b.prop("Rot", STRUCT, Some("Rotator"), None, &ints(&[1, 2, 3]), 12));
            out.extend(b.prop("Tint", STRUCT, Some("Color"), None, &[1, 2, 3, 4], 4));
            // A struct whose declaration can't be found: by its tags.
            let lost = [b.simple("Count", INT, &ints(&[9])), vec![0]].concat();
            out.extend(b.prop("Lost", STRUCT, Some("Lost"), None, &lost, lost.len()));
            // A map: `UMapProperty::SerializeItem` reads nothing.
            out.extend(b.prop("Table", MAP, None, None, &[], 0));
            out
        }),
        [
            "Ints=(1,2)",
            "Points=((X=3))",
            "Pos=(X=3)",
            "Fixed=(4,5)",
            "Ptrs=(6)",
            "Bools=(True,False)",
            "Where=(X=1.000000,Y=2.000000,Z=3.000000)",
            "Rot=(Pitch=1,Yaw=2,Roll=3)",
            "Tint=(B=1,G=2,R=3,A=4)",
            "Lost=(Count=9)",
            "Table=<0 bytes>",
        ]
    );
}

#[test]
fn properties_the_engine_skips() {
    // Each is skipped by the size in its tag, and read as well as the tag
    // allows, as `Package::properties` reads them.
    assert_eq!(
        thing_t3d(29, |b| {
            // Out of the static array's bounds.
            let mut out = b.prop("Count", INT, None, Some(2), &ints(&[1]), 4);
            // Not the declared type.
            out.extend(b.simple("Count", FLOAT, &floats(&[2.0])));
            // Not the declared struct.
            out.extend(b.prop("Pos", STRUCT, Some("Vector"), None, &floats(&[0.0; 3]), 12));
            // Transient.
            out.extend(b.simple("Hidden", INT, &ints(&[3])));
            // Not declared.
            out.extend(b.simple("Nope", INT, &ints(&[4])));
            // An array whose elements' type can't be read.
            out.extend(b.simple("Broken", ARRAY, &[1, 5]));
            out.extend(b.simple("BadFixed", FIXED_ARRAY, &[6]));
            // A bool that isn't declared: its value is still in its tag.
            out.extend(index(b.name("NoBool")));
            out.extend([BOOL | 0x50 | 0x80, 0]);
            out
        }),
        [
            "Count(2)=1",
            "Count=2.000000",
            "Pos=(X=0.000000,Y=0.000000,Z=0.000000)",
            "Hidden=3",
            "Nope=4",
            "Broken=<1 element, 1 byte>",
            "BadFixed=<1 byte: 06>",
            "NoBool=True",
        ]
    );
}

#[test]
fn old_packages_conversions() {
    assert_eq!(
        thing_t3d(29, |b| {
            // A byte saved before the property became a float.
            let mut out = b.simple("Speed", BYTE, &[3]);
            // An element saved before the property became a dynamic array.
            out.extend(b.simple("Ints", INT, &ints(&[7])));
            out
        }),
        ["Speed=3.000000", "Ints=7"]
    );
}

#[test]
fn binary_structs_before_licensee_version_28() {
    // Structs are stored in binary, in the order `SerializeBin` writes their
    // properties; and the size in the tag is trusted.
    assert_eq!(
        thing_t3d(27, |b| {
            let mut out = b.prop("Pos", STRUCT, Some("Point"), None, &ints(&[1, 2]), 8);
            // One byte more than the struct: skipped.
            let value = [ints(&[3, 4]), vec![0xFF]].concat();
            out.extend(b.prop("Pos", STRUCT, Some("Point"), None, &value, 9));
            out.extend(b.simple("Count", INT, &ints(&[5])));
            out
        }),
        ["Pos=(X=1,Y=2)", "Pos=(X=3,Y=4)", "Count=5"]
    );
    // A struct in a struct.
    assert_eq!(
        thing_t3d(27, |b| b.prop(
            "PairOne",
            STRUCT,
            Some("Pair"),
            None,
            &ints(&[1, 2]),
            8
        )),
        ["PairOne=(A=(X=1,Y=2))"]
    );
    // A struct whose declaration, its superclass's, or a field's type can't
    // be found can't be read in binary.
    assert_eq!(
        thing_t3d(27, |b| {
            let mut out = b.prop("Lost", STRUCT, Some("Lost"), None, &[1, 2], 2);
            out.extend(b.prop("ShakyOne", STRUCT, Some("Shaky"), None, &[3], 1));
            out.extend(b.prop("HoleyOne", STRUCT, Some("Holey"), None, &[1, 4], 2));
            out
        }),
        [
            "Lost=<2 bytes: 01 02>",
            "ShakyOne=<1 byte: 03>",
            "HoleyOne=<2 bytes: 01 04>",
        ]
    );
}

#[test]
fn structs_too_deep() {
    // `Deep` structs inside each other, too deep to read: the innermost are
    // skipped by their size.
    let lines = thing_t3d(29, |b| {
        let mut value = vec![0];
        for _ in 0..DEEP {
            let tag = b.prop("Inner", STRUCT, Some("Deep"), None, &value, value.len());
            value = [tag, vec![0]].concat();
        }
        let size = value.len();
        b.prop("DeepOne", STRUCT, Some("Deep"), None, &value, size)
    });
    assert_eq!(lines.len(), 1);
    assert!(
        lines[0].starts_with("DeepOne=(Inner=(Inner=("),
        "{}",
        lines[0]
    );
    assert!(lines[0].contains("<"), "{}", lines[0]);
}

#[test]
fn errors_in_values_read_by_their_declarations() {
    let error = |data: fn(&mut Builder) -> Vec<u8>| {
        let dir = TempDir::new().unwrap();
        base(dir.path(), 29);
        derived(dir.path(), 29, data);
        let packages = packages(dir.path());
        let derived = packages.get("Derived").unwrap();
        packages
            .properties(&derived, export(&derived, "Thing"))
            .unwrap()
            .find_map(Result::err)
            .unwrap()
            .to_string()
    };
    assert_eq!(
        error(|b| b.simple("Ref", OBJECT, &[50])),
        "invalid data at offset 3: object reference 50 is out of range"
    );
    assert_eq!(
        error(|b| b.simple("Ints", ARRAY, &[0x81])),
        "invalid data at offset 3: an array has -1 elements"
    );
}
