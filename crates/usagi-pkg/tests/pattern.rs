mod common;

use std::path::Path;

use common::{ExportBytes, ImportBytes, PackageBytes, name};
use usagi_pkg::{ObjectPattern, Package, package_name};

fn pattern(text: &str) -> ObjectPattern {
    text.parse().unwrap()
}

fn invalid(text: &str) -> String {
    text.parse::<ObjectPattern>().unwrap_err()
}

#[test]
fn references_with_and_without_a_class() {
    let with = pattern("Texture'MyPkg.Walls.Brick'");
    assert_eq!(with.class(), Some("Texture"));
    assert_eq!(with.path(), "MyPkg.Walls.Brick");
    assert_eq!(with.to_string(), "Texture'MyPkg.Walls.Brick'");
    let without = pattern("MyPkg.Walls.Brick");
    assert_eq!(without.class(), None);
    assert_eq!(without.to_string(), "MyPkg.Walls.Brick");
    assert!(!with.is_glob());
}

#[test]
fn invalid_references() {
    assert_eq!(
        invalid("Texture'MyPkg.Brick"),
        "invalid object reference \"Texture'MyPkg.Brick\": it doesn't end with a '"
    );
    assert!(invalid("'MyPkg.Brick'").ends_with("the class is missing before the '"));
    assert!(
        invalid("Engine.Texture'MyPkg.Brick'")
            .ends_with("the class is a name, without its package")
    );
    assert!(invalid("Texture'My'Pkg'").ends_with("there's a ' in the path"));
    assert!(invalid("MyPkg..Brick").ends_with("a name in the path is empty"));
    assert!(invalid("").ends_with("a name in the path is empty"));
    assert!(invalid("MyPkg.").ends_with("a name in the path is empty"));
}

#[test]
fn names_match_ignoring_case() {
    let p = pattern("Texture'MyPkg.Walls.Brick'");
    assert!(p.matches_path("mypkg.WALLS.brick"));
    assert!(!p.matches_path("MyPkg.Walls.Bricks"));
    assert!(!p.matches_path("MyPkg.Walls"));
    assert!(p.matches_class("TEXTURE"));
    assert!(!p.matches_class("Shader"));
    assert!(pattern("MyPkg.Brick").matches_class("Shader"));
}

#[test]
fn globs() {
    let one_group = pattern("MyPkg.UI.*");
    assert!(one_group.is_glob());
    assert!(one_group.matches_path("MyPkg.UI.Button"));
    assert!(!one_group.matches_path("MyPkg.UI"));
    assert!(!one_group.matches_path("MyPkg.UI.Sub.Button"));
    assert!(!one_group.matches_path("MyPkg.HUD.Button"));
    let any = pattern("MyPkg.UI.**");
    assert!(any.matches_path("MyPkg.UI.Sub.Button"));
    assert!(!any.matches_path("MyPkg.UI"));
    // More than two `*` are `**`.
    assert!(pattern("MyPkg.***").matches_path("MyPkg.UI.Button"));
    let one = pattern("MyPkg.Brick?");
    assert!(one.is_glob());
    assert!(one.matches_path("MyPkg.Brick2"));
    assert!(!one.matches_path("MyPkg.Brick"));
    assert!(!pattern("MyPkg?Brick").matches_path("MyPkg.Brick"));
    assert!(pattern("*.Brick*").matches_path("MyPkg.BrickWall"));
    assert!(pattern("*").matches_path(""));
}

#[test]
fn finding_objects() {
    // -1 Core.Package Core, -2 Core.Package Engine, -3 Core.Class
    // Engine.Texture; 1 Texture Walls.Brick, 2 Package Walls.
    let names = [
        "None", "Core", "Package", "Class", "Engine", "Texture", "Walls", "Brick",
    ];
    let import = |class_name, outer, object_name| ImportBytes {
        class_package: 1,
        class_name,
        outer,
        object_name,
    };
    let bytes = PackageBytes {
        names: names.iter().map(|n| name(n)).collect(),
        imports: vec![import(2, 0, 1), import(2, 0, 4), import(3, -2, 5)],
        exports: vec![
            ExportBytes {
                class: -3,
                outer: 2,
                object_name: 7,
                ..ExportBytes::default()
            },
            ExportBytes {
                class: -1,
                object_name: 6,
                ..ExportBytes::default()
            },
        ],
        ..PackageBytes::default()
    }
    .bytes();
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("MyPkg.utx");
    std::fs::write(&path, bytes).unwrap();
    let package = Package::read(&path).unwrap();
    assert_eq!(package.name, "MyPkg");
    let not_a_package = dir.path().join("Foo.utx");
    std::fs::write(&not_a_package, b"not a package").unwrap();
    assert!(Package::read(&not_a_package).is_err());
    assert_eq!(package.find_exports(&pattern("MyPkg.Walls.Brick")), [0]);
    assert_eq!(
        package.find_exports(&pattern("Texture'MyPkg.Walls.Brick'")),
        [0]
    );
    assert_eq!(
        package.find_exports(&pattern("Shader'MyPkg.Walls.Brick'")),
        [] as [usize; 0]
    );
    assert_eq!(package.find_exports(&pattern("MyPkg.**")), [0, 1]);
    assert_eq!(package.find_imports(&pattern("Class'Engine.Texture'")), [2]);
    assert_eq!(package.find_imports(&pattern("*")), [0, 1]);
}

#[test]
fn package_names() {
    assert_eq!(package_name(Path::new("System/Engine.u")), "Engine");
    assert_eq!(package_name(Path::new("Engine.u.uz2")), "Engine");
    assert_eq!(package_name(Path::new("/")), "");
}
