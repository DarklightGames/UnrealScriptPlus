mod common;

use std::path::Path;

use common::builder::*;
use common::{PackageBytes, SummaryBytes, index, name, uz2};
use tempfile::TempDir;
use usagi_pkg::archive::{NameRef, Saver, Serialize};
use usagi_pkg::diff::{Change, ExportDiff, PackageDiff, PropertyDiff, Side, diff};
use usagi_pkg::{Mipmap, Package, PackageFlags, Packages, SearchPath, Sound, Texture};

/// Reads a package named `name`.
fn read(bytes: &[u8], name: &str) -> Package {
    let mut package = Package::from_bytes(bytes).unwrap();
    package.name = name.to_string();
    package
}

/// `MyPkg`, as `build` makes it.
fn made(build: impl FnOnce(&mut Builder)) -> Package {
    let mut b = Builder::new();
    build(&mut b);
    read(&b.bytes(), "MyPkg")
}

/// Compares two packages, each read by the packages in `dir` (if any), and
/// only the exports given (if any).
fn compare_with(
    dir: Option<&Path>,
    old: Package,
    new: Package,
    exports: Option<(&[usize], &[usize])>,
) -> PackageDiff {
    let paths = || dir.map_or(Vec::new(), |dir| vec![SearchPath::from_pattern(dir, "*.u")]);
    let (old_packages, new_packages) = (Packages::new(paths()), Packages::new(paths()));
    let old = old_packages.add(old);
    let new = new_packages.add(new);
    diff(
        &Side {
            packages: &old_packages,
            package: &old,
            exports: exports.map(|e| e.0),
        },
        &Side {
            packages: &new_packages,
            package: &new,
            exports: exports.map(|e| e.1),
        },
    )
}

fn compare(old: Package, new: Package) -> PackageDiff {
    compare_with(None, old, new, None)
}

/// An import of class `package.class`, made once.
fn class(b: &mut Builder, package: &str, class: &str) -> i32 {
    let outer = find_or_import(b, "Package", 0, package);
    find_or_import(b, "Class", outer, class)
}

fn find_or_import(b: &mut Builder, class: &str, outer: i32, object: &str) -> i32 {
    let (class_name, object_name) = (b.name(class), b.name(object));
    let found = b.imports.iter().position(|i| {
        i.class_name == class_name && i.outer == outer && i.object_name == object_name
    });
    match found {
        Some(i) => -(i as i32 + 1),
        None => b.import("Core", class, outer, object),
    }
}

/// An object of class `package.class`, with tagged properties (without the
/// `None` that ends them) and native data.
fn object(
    b: &mut Builder,
    (package, class_name): (&str, &str),
    outer: i32,
    name: &str,
    properties: Vec<u8>,
    native: &[u8],
) -> i32 {
    let class = class(b, package, class_name);
    let export = b.export(class, outer, name);
    let mut data = properties;
    data.extend(index(b.name("None")));
    data.extend(native);
    b.set_data(export, data);
    export
}

const THING: (&str, &str) = ("Engine", "Thing");

fn changes(diff: &PackageDiff) -> Vec<(&str, Option<usize>, Option<usize>)> {
    diff.exports
        .iter()
        .map(|e| (e.reference.as_str(), e.old, e.new))
        .collect()
}

/// The exports with differences other than volatile ones.
fn stable(diff: &PackageDiff) -> Vec<&str> {
    diff.exports
        .iter()
        .filter(|e| e.has_changes(false))
        .map(|e| e.reference.as_str())
        .collect()
}

fn export<'a>(diff: &'a PackageDiff, reference: &str) -> &'a ExportDiff {
    diff.exports
        .iter()
        .find(|e| e.reference == reference)
        .unwrap_or_else(|| panic!("{} isn't in {:?}", reference, changes(diff)))
}

#[test]
fn identical_packages_have_no_differences() {
    let build = |b: &mut Builder| {
        let group = object(b, ("Core", "Package"), 0, "Group", Vec::new(), &[]);
        let one = b.simple("One", INT, &ints(&[1]));
        object(b, THING, group, "A", one, &[1, 2, 3]);
    };
    let diff = compare(made(build), made(build));
    assert!(!diff.has_changes(true));
    assert!(diff.exports.is_empty());
    assert!(diff.dependencies.is_empty() && diff.imports.is_empty());
}

fn summary_package(summary: SummaryBytes) -> Package {
    let bytes = PackageBytes {
        summary,
        names: vec![name("None")],
        ..PackageBytes::default()
    };
    read(&bytes.bytes(), "MyPkg")
}

#[test]
fn summaries() {
    let old = SummaryBytes::default();
    // Everything that changes when a package is saved again.
    let resaved = SummaryBytes {
        guid: [1, 2, 3, 4],
        generations: vec![(0, 0), (0, 1)],
        ..old.clone()
    };
    let diff = compare(summary_package(old.clone()), summary_package(resaved));
    let summary = &diff.summary;
    assert_eq!(
        summary.guid.map(|c| c.new.to_string()),
        Some("00000001000000020000000300000004".to_string())
    );
    assert_eq!(summary.generations.as_ref().map(|c| c.new.len()), Some(2));
    // A longer summary moves the tables, and makes the file bigger.
    assert!(summary.names.is_some() && summary.imports.is_some() && summary.exports.is_some());
    assert_eq!(summary.size.map(|c| c.new - c.old), Some(8));
    assert!(summary.tag.is_none() && summary.package_flags.is_none());
    assert!(diff.has_changes(false));
    let resaved = SummaryBytes {
        guid: [1, 2, 3, 4],
        ..old.clone()
    };
    let diff = compare(summary_package(old.clone()), summary_package(resaved));
    assert!(!diff.has_changes(false));
    assert!(diff.has_changes(true));

    // What's in the summary itself.
    let changed = SummaryBytes {
        tag: 0x9E2A83C2,
        flags: 0x1,
        file_version: 128 | 30 << 16,
        ..old.clone()
    };
    let summary = compare(summary_package(old.clone()), summary_package(changed)).summary;
    assert_eq!(
        summary.tag,
        Some(Change {
            old: 0x9E2A83C1,
            new: 0x9E2A83C2
        })
    );
    assert_eq!(summary.version, None);
    assert_eq!(summary.licensee_version, Some(Change { old: 29, new: 30 }));
    assert_eq!(
        summary.package_flags,
        Some(Change {
            old: PackageFlags(0x21),
            new: PackageFlags(0x1)
        })
    );
    assert!(summary.has_changes(false));

    // An old package has a heritage table.
    let ancient = SummaryBytes {
        file_version: 61,
        heritage: (0, 0),
        ..old.clone()
    };
    let summary = compare(summary_package(old.clone()), summary_package(ancient)).summary;
    assert_eq!(summary.version, Some(Change { old: 128, new: 61 }));
    assert_eq!(summary.heritage.map(|c| c.old), Some(None));

    // A compressed package.
    let bytes = PackageBytes {
        summary: old,
        names: vec![name("None")],
        ..PackageBytes::default()
    }
    .bytes();
    let mut compressed = Package::from_uz2_bytes(&uz2(&bytes)).unwrap();
    compressed.name = "MyPkg".to_string();
    let summary = compare(read(&bytes, "MyPkg"), compressed).summary;
    assert_eq!(summary.size, None);
    assert_eq!(summary.compressed_size.map(|c| c.old), Some(None));
}

#[test]
fn dependencies_and_imports() {
    let old = made(|b| {
        class(b, "Engine", "Texture");
        class(b, "Gone", "Thing");
    });
    let new = made(|b| {
        class(b, "Engine", "Texture");
        class(b, "Fire", "Wave");
        // The same package again, in another case, is listed once.
        b.import("Core", "Package", 0, "fire");
    });
    let diff = compare(old, new);
    assert_eq!(diff.dependencies.added, ["Fire"]);
    assert_eq!(diff.dependencies.removed, ["Gone"]);
    assert_eq!(diff.imports.added, ["Package'Fire'", "Class'Fire.Wave'"]);
    assert_eq!(diff.imports.removed, ["Package'Gone'", "Class'Gone.Thing'"]);
    assert!(diff.has_changes(false));
}

#[test]
fn exports_are_matched_by_class_and_path() {
    let old = made(|b| {
        let group = object(b, ("Core", "Package"), 0, "Group", Vec::new(), &[]);
        object(b, THING, group, "Same", Vec::new(), &[]);
        object(b, THING, 0, "Gone", Vec::new(), &[1]);
        object(b, THING, 0, "Swap", Vec::new(), &[]);
        object(b, THING, 0, "Twin", Vec::new(), &[1]);
        object(b, THING, 0, "Twin", Vec::new(), &[2]);
        let flagged = object(b, THING, 0, "Flagged", Vec::new(), &[]);
        b.set_flags(flagged, 0x0007_0000);
    });
    // Reordered, so that every index changes.
    let new = made(|b| {
        object(b, THING, 0, "Added", Vec::new(), &[1, 2]);
        object(b, ("Engine", "Other"), 0, "Swap", Vec::new(), &[]);
        object(b, THING, 0, "twin", Vec::new(), &[1]);
        let group = object(b, ("Core", "Package"), 0, "Group", Vec::new(), &[]);
        object(b, THING, group, "Same", Vec::new(), &[]);
        object(b, THING, 0, "Flagged", Vec::new(), &[]);
    });
    let diff = compare(old, new);
    // By path, then class; removed after the rest.
    assert_eq!(
        changes(&diff),
        [
            ("Thing'MyPkg.Added'", None, Some(0)),
            ("Thing'MyPkg.Flagged'", Some(6), Some(5)),
            ("Thing'MyPkg.Gone'", Some(2), None),
            ("Package'MyPkg.Group'", Some(0), Some(3)),
            ("Thing'MyPkg.Group.Same'", Some(1), Some(4)),
            ("Other'MyPkg.Swap'", None, Some(1)),
            ("Thing'MyPkg.Swap'", Some(3), None),
            // Paired in table order; the second is removed.
            ("Thing'MyPkg.twin'", Some(4), Some(2)),
            ("Thing'MyPkg.Twin'", Some(5), None),
        ]
    );
    let added = export(&diff, "Thing'MyPkg.Added'");
    assert!(added.is_added() && !added.is_removed() && added.has_changes(false));
    assert!(export(&diff, "Thing'MyPkg.Gone'").is_removed());
    let flagged = export(&diff, "Thing'MyPkg.Flagged'");
    assert_eq!(flagged.flags.map(|c| c.old.0), Some(0x0007_0000));
    // Only its index and offset changed.
    let same = export(&diff, "Thing'MyPkg.Group.Same'");
    assert!(same.offset.is_some());
    assert!(!same.has_changes(false) && same.has_changes(true));
    assert!(!export(&diff, "Thing'MyPkg.twin'").has_changes(false));
}

#[test]
fn only_the_exports_given() {
    let build = |native: u8| {
        made(move |b| {
            object(b, THING, 0, "A", Vec::new(), &[native]);
            object(b, THING, 0, "B", Vec::new(), &[native]);
        })
    };
    let diff = compare_with(None, build(1), build(2), Some((&[1], &[1])));
    assert_eq!(changes(&diff), [("Thing'MyPkg.B'", Some(1), Some(1))]);
    // Only an object differs.
    assert!(!diff.summary.has_changes(true) && diff.has_changes(false));
}

#[test]
fn properties_and_native_data() {
    let old = made(|b| {
        let target = object(b, THING, 0, "Target", Vec::new(), &[]);
        let mut properties = b.simple("A", INT, &ints(&[1]));
        properties.extend(b.simple("B", INT, &ints(&[2])));
        properties.extend(b.simple("Ref", OBJECT, &index(target)));
        object(b, THING, 0, "Changed", properties, &[1, 2]);
    });
    let new = made(|b| {
        object(b, THING, 0, "Padding", Vec::new(), &[]);
        let target = object(b, THING, 0, "Target", Vec::new(), &[]);
        let mut properties = b.simple("a", INT, &ints(&[5]));
        // A reference to the same object, whose index changed.
        properties.extend(b.simple("Ref", OBJECT, &index(target)));
        properties.extend(b.prop("C", INT, None, Some(2), &ints(&[3]), 4));
        object(b, THING, 0, "Changed", properties, &[1, 3, 4]);
    });
    let diff = compare(old, new);
    let changed = export(&diff, "Thing'MyPkg.Changed'");
    let property = |name: &str, old: Option<&str>, new: Option<&str>| PropertyDiff {
        name: name.to_string(),
        old: old.map(str::to_string),
        new: new.map(str::to_string),
        volatile: None,
    };
    assert_eq!(
        changed.properties,
        [
            property("a", Some("1"), Some("5")),
            property("C(2)", None, Some("3")),
            property("B", Some("2"), None),
        ]
    );
    assert_eq!(changed.native, Some(Change { old: 2, new: 3 }));
    assert!(changed.size.is_some());
}

#[test]
fn script_objects_are_only_added_or_removed() {
    let build = |extra: bool, data: u8| {
        made(move |b| {
            let function = b.core_class("Function");
            let tick = b.export(function, 0, "Tick");
            b.set_data(tick, vec![data]);
            let int_property = b.core_class("IntProperty");
            let count = b.export(int_property, 0, "Count");
            b.set_data(count, vec![data]);
            if extra {
                let text = b.core_class("TextBuffer");
                b.export(text, 0, "ScriptText");
            }
        })
    };
    let diff = compare(build(false, 1), build(true, 2));
    assert_eq!(stable(&diff), ["TextBuffer'MyPkg.ScriptText'"]);
    let tick = export(&diff, "Function'MyPkg.Tick'");
    assert!(tick.native.is_none() && tick.size.is_none());
    let package = build(true, 1);
    let is_script = |i| usagi_pkg::diff::is_script_object(&package, i);
    assert!(is_script(0) && is_script(1) && is_script(2));
    let other = made(|b| {
        object(b, ("Core", "Package"), 0, "Group", Vec::new(), &[]);
        object(b, THING, 0, "Thing", Vec::new(), &[]);
        b.class("MyClass", 0, 0, Vec::new());
    });
    assert!((0..3).all(|i| !usagi_pkg::diff::is_script_object(&other, i)));
}

#[test]
fn classes_are_their_defaults_and_superclass() {
    let build = |super_class: &str, value: i32| {
        let super_class = super_class.to_string();
        made(move |b| {
            let parent = class(b, "Engine", &super_class);
            let defaults = b.simple("Speed", INT, &ints(&[value]));
            let class = b.class("MyClass", parent, 0, defaults);
            b.exports[class as usize - 1].super_struct = parent;
        })
    };
    let diff = compare(build("Actor", 1), build("Info", 2));
    let class = export(&diff, "Class'MyPkg.MyClass'");
    assert_eq!(
        class.super_struct,
        Some(Change {
            old: "Class'Engine.Actor'".to_string(),
            new: "Class'Engine.Info'".to_string()
        })
    );
    assert_eq!(class.properties.len(), 1);
}

#[test]
fn objects_that_cant_be_read() {
    let old = made(|b| {
        // A tag that runs past the end of the data.
        let cut = b.simple("A", INT, &ints(&[1]));
        let export = object(b, THING, 0, "Cut", Vec::new(), &[]);
        b.set_data(export, cut[..cut.len() - 1].to_vec());
        // A class whose data is cut short.
        let class = b.export(0, 0, "Broken");
        b.set_data(class, vec![0]);
    });
    let new = made(|b| {
        object(b, THING, 0, "Cut", Vec::new(), &[]);
        let class = b.export(0, 0, "Broken");
        b.set_data(class, vec![0, 0]);
    });
    let diff = compare(old, new);
    let cut = export(&diff, "Thing'MyPkg.Cut'");
    assert!(cut.old_error.is_some() && cut.new_error.is_none());
    assert_eq!(cut.native, None);
    let broken = export(&diff, "Class'MyPkg.Broken'");
    assert!(broken.old_error.is_some() && broken.new_error.is_some());
    assert!(broken.has_changes(false));
}

/// Writes `value` as an object's native data, after an empty list of
/// properties, for where the object's data is in the package `b` makes.
fn set_media<T: Serialize + Clone>(b: &mut Builder, export: i32, value: &T) {
    let data = |offset: usize| {
        let mut saver = Saver::new(128, 29).at(offset + 1);
        value.clone().serialize(&mut saver).unwrap();
        [vec![0], saver.into_bytes()].concat()
    };
    // Its size doesn't depend on where it is.
    let placeholder = data(0);
    b.set_data(export, placeholder);
    let offset = read(&b.bytes(), "MyPkg").exports()[export as usize - 1]
        .serial_offset
        .unwrap() as usize;
    let data = data(offset);
    b.set_data(export, data);
}

fn texture(pixels: &[u8]) -> Texture {
    Texture {
        mips: vec![Mipmap {
            data: usagi_pkg::archive::LazyArray {
                seek_pos: None,
                items: pixels.to_vec(),
            },
            u_size: 1,
            v_size: 1,
            u_bits: 0,
            v_bits: 0,
        }],
    }
}

/// A package with padding of `padding` bytes, then a texture of class
/// `Engine.<class>` with `pixels`, then `after` after it.
fn texture_package(padding: usize, class_name: &str, pixels: &[u8], after: &[u8]) -> Package {
    let mut b = Builder::new();
    object(&mut b, THING, 0, "Padding", Vec::new(), &vec![0; padding]);
    let tex = object(&mut b, ("Engine", class_name), 0, "Tex", Vec::new(), &[]);
    set_media(&mut b, tex, &texture(pixels));
    let mut data = b.data[tex as usize - 1].clone();
    data.extend(after);
    b.set_data(tex, data);
    read(&b.bytes(), "MyPkg")
}

#[test]
fn textures_are_compared_by_their_mipmaps() {
    let tex = "Texture'MyPkg.Tex'";
    // Moved in the file, so its mipmaps' SeekPos changed.
    let diff = compare(
        texture_package(1, "Texture", &[1, 2], &[]),
        texture_package(5, "Texture", &[1, 2], &[]),
    );
    assert_eq!(stable(&diff), ["Thing'MyPkg.Padding'"]);
    // Different pixels.
    let diff = compare(
        texture_package(1, "Texture", &[1, 2], &[]),
        texture_package(1, "Texture", &[1, 3], &[]),
    );
    // A count, then SeekPos, the pixels and the size: 18 bytes.
    assert_eq!(export(&diff, tex).native, Some(Change { old: 18, new: 18 }));
    // Different data after the mipmaps.
    let diff = compare(
        texture_package(1, "Texture", &[1, 2], &[7]),
        texture_package(1, "Texture", &[1, 2], &[8]),
    );
    assert!(export(&diff, tex).native.is_some());
    // Malformed.
    let mut bad = Builder::new();
    object(&mut bad, ("Engine", "Texture"), 0, "Tex", Vec::new(), &[5]);
    let diff = compare(
        texture_package(0, "Texture", &[1], &[]),
        read(&bad.bytes(), "MyPkg"),
    );
    assert!(export(&diff, tex).new_error.is_some());
}

#[test]
fn texture_subclasses_are_found_through_the_game() {
    let dir = TempDir::new().unwrap();
    let mut engine = Builder::new();
    let texture_class = engine.class("Texture", 0, 0, Vec::new());
    engine.class("Cubemap", texture_class, 0, Vec::new());
    engine.class("Thing", 0, 0, Vec::new());
    engine.write(dir.path(), "Engine.u");
    let in_game = |old: Package, new: Package| compare_with(Some(dir.path()), old, new, None);
    // A subclass's texture moved: no change.
    let diff = in_game(
        texture_package(1, "Cubemap", &[1, 2], &[]),
        texture_package(5, "Cubemap", &[1, 2], &[]),
    );
    assert_eq!(stable(&diff), ["Thing'MyPkg.Padding'"]);
    // A class that isn't a texture: its data is compared as bytes, so its
    // moved SeekPos is a change.
    let diff = in_game(
        texture_package(1, "Thing", &[1, 2], &[]),
        texture_package(5, "Thing", &[1, 2], &[]),
    );
    assert!(export(&diff, "Thing'MyPkg.Tex'").native.is_some());
}

/// A package with padding of `padding` bytes, then a sound of class
/// `Engine.<class>`.
fn sound_package(padding: usize, class_name: &str, file_type: &str, data: &[u8]) -> Package {
    let mut b = Builder::new();
    if padding > 1 {
        // Another name, so that the file type's index differs.
        b.name("Extra");
    }
    object(&mut b, THING, 0, "Padding", Vec::new(), &vec![0; padding]);
    let sound = object(&mut b, ("Engine", class_name), 0, "Boom", Vec::new(), &[]);
    let value = Sound {
        file_type: NameRef(b.name(file_type)),
        likelihood: Some(1.0),
        data: usagi_pkg::archive::LazyArray {
            seek_pos: None,
            items: data.to_vec(),
        },
    };
    set_media(&mut b, sound, &value);
    read(&b.bytes(), "MyPkg")
}

#[test]
fn sounds_are_compared_by_their_contents() {
    // Moved in the file, with its file type's name at another index.
    let diff = compare(
        sound_package(1, "Sound", "WAV", &[1, 2]),
        sound_package(5, "Sound", "wav", &[1, 2]),
    );
    assert_eq!(stable(&diff), ["Thing'MyPkg.Padding'"]);
    let diff = compare(
        sound_package(1, "Sound", "WAV", &[1, 2]),
        sound_package(1, "Sound", "OGG", &[1, 2]),
    );
    assert!(export(&diff, "Sound'MyPkg.Boom'").native.is_some());
    // A subclass of `Sound` has data of its own: compared as bytes.
    let diff = compare(
        sound_package(1, "ProceduralSound", "WAV", &[1, 2]),
        sound_package(5, "ProceduralSound", "WAV", &[1, 2]),
    );
    assert!(
        export(&diff, "ProceduralSound'MyPkg.Boom'")
            .native
            .is_some()
    );
    // Malformed.
    let mut bad = Builder::new();
    object(
        &mut bad,
        ("Engine", "Sound"),
        0,
        "Boom",
        Vec::new(),
        &[0x7F],
    );
    let diff = compare(
        sound_package(1, "Sound", "WAV", &[1]),
        read(&bad.bytes(), "MyPkg"),
    );
    assert!(export(&diff, "Sound'MyPkg.Boom'").new_error.is_some());
}

/// A package with `padding` objects, then a sound `Boom`, and a sound group
/// whose sounds are `sounds` (by name).
fn sound_group_package(padding: usize, sounds: &[&str]) -> Package {
    made(|b| {
        for i in 0..padding {
            object(b, THING, 0, &format!("Pad{}", i), Vec::new(), &[]);
        }
        object(b, THING, 0, "Boom", Vec::new(), &[]);
        let mut native = index(sounds.len() as i32);
        for sound in sounds {
            let name = b.name(sound);
            let export = b
                .exports
                .iter()
                .position(|e| e.object_name == name)
                .unwrap();
            native.extend(index(export as i32 + 1));
        }
        object(b, ("Engine", "SoundGroup"), 0, "Group", Vec::new(), &native);
    })
}

#[test]
fn sound_groups_are_compared_by_their_sounds() {
    let group = "SoundGroup'MyPkg.Group'";
    // Its sound moved in the export table.
    let diff = compare(
        sound_group_package(0, &["Boom"]),
        sound_group_package(2, &["Boom"]),
    );
    assert_eq!(stable(&diff), ["Thing'MyPkg.Pad0'", "Thing'MyPkg.Pad1'"]);
    assert!(export(&diff, group).native.is_none());
    // Another sound.
    let diff = compare(
        sound_group_package(1, &["Boom"]),
        sound_group_package(1, &["Pad0"]),
    );
    assert_eq!(export(&diff, group).native, Some(Change { old: 2, new: 2 }));
    // Malformed.
    let mut bad = Builder::new();
    object(
        &mut bad,
        ("Engine", "SoundGroup"),
        0,
        "Group",
        Vec::new(),
        &[1, 9],
    );
    let diff = compare(sound_group_package(0, &[]), read(&bad.bytes(), "MyPkg"));
    assert!(export(&diff, group).new_error.is_some());
    // Before licensee version 27, the package its sounds are in.
    let old_group = |package: &str| {
        let package = package.to_string();
        made(move |b| {
            b.licensee = 26;
            let mut native = index(package.len() as i32 + 1);
            native.extend(package.as_bytes());
            native.push(0);
            object(b, ("Engine", "SoundGroup"), 0, "Group", Vec::new(), &native);
        })
    };
    assert!(
        compare(old_group("Fire"), old_group("Fire"))
            .exports
            .is_empty()
    );
    let diff = compare(old_group("Fire"), old_group("Wave"));
    assert!(export(&diff, group).native.is_some());
}

#[test]
fn a_size_that_changed_only_in_how_references_are_stored() {
    // A reference to `Target`, whose index (and the name `Ref`'s) needs a
    // byte more in the new package: an object's, and a class's defaults.
    let build = |padding: usize| {
        made(move |b| {
            for i in 0..padding {
                object(b, THING, 0, &format!("Pad{}", i), Vec::new(), &[]);
            }
            let target = object(b, THING, 0, "Target", Vec::new(), &[]);
            let reference = b.simple("Ref", OBJECT, &index(target));
            object(b, THING, 0, "Holder", reference.clone(), &[]);
            b.class("MyClass", 0, 0, reference);
        })
    };
    let diff = compare(build(0), build(70));
    let holder = export(&diff, "Thing'MyPkg.Holder'");
    assert_eq!(holder.size, None);
    assert_eq!(holder.encoded_size, Some(Change { old: 5, new: 7 }));
    assert!(!holder.has_changes(false) && holder.has_changes(true));
    // A class's own data isn't compared, so its size may be all there is.
    let class = export(&diff, "Class'MyPkg.MyClass'");
    assert!(class.size.is_some() && class.encoded_size.is_none());
}

#[test]
fn properties_the_engine_never_uses_are_volatile() {
    // `M`, and an element of it as if it were a static array, and another
    // property; of a texture panner, of a subclass of one, and of a class.
    let build = |m: i32, other: i32| {
        made(move |b| {
            let mut properties = b.simple("M", INT, &ints(&[m]));
            properties.extend(b.prop("M", INT, None, Some(1), &ints(&[m]), 4));
            properties.extend(b.simple("Other", INT, &ints(&[other])));
            object(
                b,
                ("Engine", "TexPanner"),
                0,
                "Pan",
                properties.clone(),
                &[],
            );
            object(
                b,
                ("Engine", "TexPannerTriggered"),
                0,
                "Triggered",
                properties.clone(),
                &[],
            );
            b.class("MyClass", 0, 0, properties);
        })
    };
    let dir = TempDir::new().unwrap();
    let mut engine = Builder::new();
    let panner = engine.class("TexPanner", 0, 0, Vec::new());
    engine.class("TexPannerTriggered", panner, 0, Vec::new());
    engine.write(dir.path(), "Engine.u");
    let diff = compare_with(Some(dir.path()), build(1, 1), build(2, 1), None);
    for reference in [
        "TexPanner'MyPkg.Pan'",
        "TexPannerTriggered'MyPkg.Triggered'",
    ] {
        let object = export(&diff, reference);
        let volatile: Vec<_> = object.properties.iter().map(|p| p.is_volatile()).collect();
        assert_eq!(volatile, [true, true], "{}", reference);
        assert_eq!(
            object.properties[0].volatile,
            Some("set from the time by GetMatrix whenever it's used")
        );
        assert!(!object.has_changes(false) && object.has_changes(true));
    }
    // A class's own `M` isn't a texture panner's.
    let class = export(&diff, "Class'MyPkg.MyClass'");
    assert!(class.properties.iter().all(|p| !p.is_volatile()));
    assert_eq!(stable(&diff), ["Class'MyPkg.MyClass'"]);
    // Other properties are still changes.
    let diff = compare_with(Some(dir.path()), build(1, 1), build(2, 2), None);
    let pan = export(&diff, "TexPanner'MyPkg.Pan'");
    assert!(pan.has_changes(false));
    assert!(!pan.properties[2].is_volatile());
    // The table names classes as `Package.Class`.
    for property in usagi_pkg::volatile::VOLATILE_PROPERTIES {
        assert_eq!(property.class.split('.').count(), 2, "{:?}", property);
    }
}
